"""'New mesh...' dialog: fixed boundary points, free interior points and a Delaunay triangle mesh for MUT."""

from __future__ import annotations

from pathlib import Path

from qgis.core import (
    QgsApplication,
    QgsCoordinateTransformContext,
    QgsMapLayerProxyModel,
    QgsProject,
    QgsSettings,
    QgsVectorFileWriter,
)
from qgis.gui import QgsFileWidget, QgsMapLayerComboBox
from qgis.PyQt.QtCore import QUrl
from qgis.PyQt.QtGui import QDesktopServices
from qgis.PyQt.QtWidgets import (
    QCheckBox,
    QComboBox,
    QDialog,
    QFormLayout,
    QGroupBox,
    QHBoxLayout,
    QLabel,
    QLineEdit,
    QMessageBox,
    QPlainTextEdit,
    QProgressBar,
    QPushButton,
    QSpinBox,
    QVBoxLayout,
)

from ..core.mesh import mesh_outputs, preview
from ..core.project import STREAM_SOURCES, MeshSpec, Project, StreamOptions
from ..core.qgis_project import populate_mesh, refresh_qgz, streams_layer
from ..core.streams import SOURCE_LABELS
from .focus_dialog import FocusAreaDialog
from .task import MeshTask, StreamsTask
from .wizard import SETTINGS, _spin


def _int_spin(lo: int, hi: int, value: int, step: int = 1, suffix: str = "") -> QSpinBox:
    box = QSpinBox()
    box.setRange(lo, hi)
    box.setSingleStep(step)
    box.setValue(value)
    if suffix:
        box.setSuffix(suffix)
    return box


class MeshDialog(QDialog):
    def __init__(self, iface) -> None:
        super().__init__(iface.mainWindow())
        self.iface = iface
        self.setWindowTitle("MUT Model Builder: new mesh")
        self.resize(720, 860)
        self.project: Project | None = None
        self.task: MeshTask | None = None
        self.stream_task: StreamsTask | None = None

        layout = QVBoxLayout(self)
        intro = QLabel(
            "Builds a 2D Delaunay triangle mesh of the regional model area (2_mesh/&lt;name&gt;/). Points on the "
            "model boundary and optional user points are <b>fixed</b>; interior points are evenly spaced and "
            "<b>free</b> to move during smoothing. <i>Add streams</i> also fixes nodes along ranked rivers and "
            "streams, with mesh edges along them. MUT reads the mesh with <tt>2d mesh from gb</tt>; with "
            "<tt>nodal control volumes</tt> each point becomes a MODFLOW cell."
        )
        intro.setWordWrap(True)
        layout.addWidget(intro)

        form = QFormLayout()
        self.yaml = QgsFileWidget()
        self.yaml.setFilter("Project files (*.yaml *.yml)")
        self.yaml.fileChanged.connect(self.load_project)
        form.addRow("Project file", self.yaml)
        self.name = QComboBox()
        self.name.setEditable(True)
        self.name.setInsertPolicy(QComboBox.InsertPolicy.NoInsert)
        self.name.lineEdit().setPlaceholderText("Mesh name, e.g. Regional")
        self.name.currentTextChanged.connect(self.name_changed)
        form.addRow("Mesh name", self.name)
        self.spacing = _spin(1, 100000, 300, 1, " m", 50)
        self.spacing.setToolTip("Distance between fixed points along the model boundary (and around any holes)")
        form.addRow("Boundary point spacing", self.spacing)
        self.tolerance = _spin(0, 10000, 0, 1, " m", 10)
        self.tolerance.setSpecialValueText("0 (follow the boundary exactly)")
        self.tolerance.setToolTip(
            "0 keeps every model-boundary vertex, so mesh edges lie on the boundary. A positive value removes "
            "boundary detail smaller than this (the mesh outline then stays within this distance of the boundary)."
        )
        form.addRow("Boundary simplification", self.tolerance)
        self.points = _int_spin(1, 5_000_000, 5000, 500)
        self.points.setToolTip("Approximate number of free points on a hexagonal lattice inside the boundary")
        form.addRow("Interior points", self.points)
        self.layer = QgsMapLayerComboBox()
        self.layer.setFilters(QgsMapLayerProxyModel.Filter.PointLayer)
        self.layer.setAllowEmptyLayer(True, "(none)")
        self.layer.setLayer(None)
        self.layer.setToolTip("Point layer of fixed nodes, e.g. wells, shafts or observation points")
        form.addRow("Fixed points (map layer)", self.layer)
        self.file = QgsFileWidget()
        self.file.setFilter("Vector files (*.shp *.gpkg *.geojson *.json *.csv *.kml *.gml);;All files (*)")
        self.file.setToolTip("Used when no map layer is chosen")
        form.addRow("... or fixed points file", self.file)
        self.smooth = _int_spin(0, 200, 0)
        self.smooth.setToolTip("Laplacian smoothing passes; only free nodes move, the mesh is re-triangulated each pass")
        form.addRow("Smoothing passes", self.smooth)
        layout.addLayout(form)
        for box in (self.spacing, self.points, self.tolerance):
            box.valueChanged.connect(self.show_preview)
        layout.addWidget(self._streams_box())

        self.summary = QLabel("Open a project file.")
        self.summary.setWordWrap(True)
        layout.addWidget(self.summary)

        row = QHBoxLayout()
        self.build_button = QPushButton("Build mesh")
        self.build_button.clicked.connect(self.build)
        self.cancel_button = QPushButton("Cancel")
        self.cancel_button.clicked.connect(self.cancel)
        self.folder_button = QPushButton("Open mesh folder")
        self.folder_button.clicked.connect(self.open_folder)
        for w in (self.build_button, self.cancel_button, self.folder_button):
            row.addWidget(w)
        row.addStretch()
        layout.addLayout(row)
        self.progress = QProgressBar()
        self.log = QPlainTextEdit()
        self.log.setReadOnly(True)
        layout.addWidget(self.progress)
        layout.addWidget(self.log, 1)
        self.finished.connect(lambda _result: self.cleanup())

        start = FocusAreaDialog.open_project_yaml() or QgsSettings().value(f"{SETTINGS}/mesh_project", "", type=str)
        if start and Path(start).is_file():
            self.yaml.setFilePath(str(start))
        self.update_buttons()

    def _streams_box(self) -> QGroupBox:
        box = QGroupBox("Add streams (fixed nodes and mesh edges along rivers and streams)")
        box.setCheckable(True)
        box.setChecked(False)
        self.streams_box = box
        form = QFormLayout(box)
        self.stream_source = QComboBox()
        for key in STREAM_SOURCES:
            self.stream_source.addItem(SOURCE_LABELS[key], key)
        self.stream_source.setToolTip(
            "Automatic tries the Ontario Hydro Network (Ontario), USGS NHDPlus HR (United States), then OpenStreetMap. "
            "'From the regional DEM' extracts streams from ground_top.tif with GRASS r.stream.extract."
        )
        self.stream_source.currentIndexChanged.connect(self.stream_source_changed)
        form.addRow("Source", self.stream_source)
        self.stream_levels = _int_spin(1, 6, 2)
        self.stream_levels.setToolTip(
            "1 = the main river only (e.g. the Grand River); 2 = also the streams flowing into it; 3 = also their "
            "tributaries; ..."
        )
        form.addRow("Levels", self.stream_levels)
        self.stream_min_length = _spin(0, 1000, 5, 1, " km", 1)
        self.stream_min_length.setToolTip("Streams shorter than this inside the boundary are left out")
        form.addRow("Minimum stream length", self.stream_min_length)
        self.stream_spacing = _spin(0, 100000, 0, 0, " m", 50)
        self.stream_spacing.setSpecialValueText("same as the boundary spacing")
        self.stream_spacing.setToolTip("Distance between fixed nodes along the streams")
        form.addRow("Stream node spacing", self.stream_spacing)
        self.main_river = QLineEdit()
        self.main_river.setPlaceholderText("optional, e.g. Grand River")
        self.main_river.setToolTip(
            "Name of the main river; at confluences the branch with this name is followed for level 1. "
            "Otherwise the branch with the most stream length upstream is followed."
        )
        form.addRow("Main river name", self.main_river)
        self.include_canals = QCheckBox("Include canals (OpenStreetMap)")
        form.addRow("", self.include_canals)
        self.dem_threshold = _spin(0.01, 100000, 10, 2, " km2", 1)
        self.dem_threshold.setToolTip("Smallest drainage area that starts a stream when extracting from the DEM")
        form.addRow("DEM stream threshold", self.dem_threshold)
        row = QHBoxLayout()
        self.find_button = QPushButton("Find streams")
        self.find_button.setToolTip("Download or extract the streams, rank them into levels and show them on the map")
        self.find_button.clicked.connect(self.find_streams)
        row.addWidget(self.find_button)
        row.addStretch()
        form.addRow(row)
        self.stream_summary = QLabel("")
        self.stream_summary.setWordWrap(True)
        form.addRow(self.stream_summary)
        box.toggled.connect(lambda _on: self.update_buttons())
        self.stream_source_changed()
        return box

    def stream_source_changed(self) -> None:
        source = self.stream_source.currentData()
        self.dem_threshold.setEnabled(source == "dem")
        self.include_canals.setEnabled(source in ("auto", "osm"))

    def stream_options(self) -> StreamOptions | None:
        if not self.streams_box.isChecked():
            return None
        return StreamOptions(
            source=self.stream_source.currentData(),
            levels=self.stream_levels.value(),
            min_length_km=self.stream_min_length.value(),
            spacing_m=self.stream_spacing.value(),
            main_river=self.main_river.text().strip(),
            include_canals=self.include_canals.isChecked(),
            dem_threshold_km2=self.dem_threshold.value(),
        )

    def set_stream_options(self, opts: StreamOptions | None) -> None:
        self.streams_box.setChecked(opts is not None)
        if opts is None:
            return
        self.stream_source.setCurrentIndex(max(0, self.stream_source.findData(opts.source)))
        self.stream_levels.setValue(opts.levels)
        self.stream_min_length.setValue(opts.min_length_km)
        self.stream_spacing.setValue(opts.spacing_m)
        self.main_river.setText(opts.main_river)
        self.include_canals.setChecked(opts.include_canals)
        self.dem_threshold.setValue(opts.dem_threshold_km2)

    def find_streams(self) -> None:
        if self.project is None or self.busy():
            return
        spec = self.trial_spec()
        if spec.streams is None:
            return
        self.stream_task = StreamsTask(self.project, spec)
        self.stream_task.message.connect(self.append)
        self.stream_task.completed.connect(self.on_streams)
        self.stream_summary.setText("Searching for streams ...")
        self.update_buttons()
        QgsApplication.taskManager().addTask(self.stream_task)

    def on_streams(self, ok: bool) -> None:
        task, self.stream_task = self.stream_task, None
        self.update_buttons()
        if task is None:
            return
        if not ok or task.network is None:
            self.stream_summary.setText("No streams found; see the log.")
            self.append(task.error or "Stream search failed.")
            return
        self.stream_summary.setText(task.network.describe().replace("\n", "<br>"))
        title = f"{task.spec.name} streams (preview)"
        qproject = QgsProject.instance()
        for old in qproject.mapLayersByName(title):
            qproject.removeMapLayer(old.id())
        layer = streams_layer(task.path, title)
        if layer.isValid():
            qproject.addMapLayer(layer)

    def busy(self) -> bool:
        return self.task is not None or self.stream_task is not None

    def append(self, text: str) -> None:
        self.log.appendPlainText(text)

    def mesh_name(self) -> str:
        return self.name.currentText().strip()

    def load_project(self, path: str) -> None:
        self.project = None
        self.name.blockSignals(True)
        self.name.clear()
        self.name.blockSignals(False)
        if not path or not Path(path).is_file():
            self.summary.setText("Open a project file.")
            self.update_buttons()
            return
        try:
            project = Project.load(Path(path))
        except (OSError, ValueError, TypeError) as exc:
            self.summary.setText(f"Could not read {path}: {exc}")
            self.update_buttons()
            return
        QgsSettings().setValue(f"{SETTINGS}/mesh_project", path)
        self.project = project
        self.name.blockSignals(True)
        self.name.addItems([m.name for m in project.meshes])
        self.name.setEditText("")
        self.name.blockSignals(False)
        self.show_preview()

    def name_changed(self, name: str) -> None:
        spec = self.project.get_mesh(name.strip()) if self.project and name.strip() else None
        if spec is not None:
            for box, value in (
                (self.spacing, spec.boundary_spacing_m),
                (self.tolerance, spec.boundary_tolerance_m),
                (self.points, spec.interior_points),
                (self.smooth, spec.smoothing_iterations),
            ):
                box.blockSignals(True)
                box.setValue(value)
                box.blockSignals(False)
            self.file.setFilePath(spec.fixed_points_file)
            self.set_stream_options(spec.streams)
        self.show_preview()

    def trial_spec(self) -> MeshSpec:
        return MeshSpec(
            name=self.mesh_name() or "mesh",
            boundary_spacing_m=self.spacing.value(),
            boundary_tolerance_m=self.tolerance.value(),
            interior_points=self.points.value(),
            smoothing_iterations=self.smooth.value(),
            streams=self.stream_options(),
        )

    def show_preview(self) -> None:
        self.update_buttons()
        if self.project is None:
            return
        try:
            p = preview(self.project, self.trial_spec())
        except (RuntimeError, ValueError) as exc:
            self.summary.setText(f"Boundary problem: {exc}")
            return
        existing = ", ".join(f"{m.name} ({m.status})" for m in self.project.meshes) or "none yet"
        boundary = self.project.boundary_name or ("model boundary" if self.project.has_boundary else "regional square")
        self.summary.setText(
            f"<b>{self.project.name}</b>: {boundary}, {p['area_km2']:,.2f} km2.<br>"
            f"About {p['boundary_points']:,} fixed boundary points and {self.points.value():,} free interior points "
            f"(spacing about {p['interior_spacing_m']:,.0f} m), {p['total_points']:,} nodes in total; "
            f"shortest boundary side {p['shortest_boundary_side_m']:,.0f} m.<br>"
            f"Meshes: {existing}."
            + ("" if self.mesh_name() else "<br><b>Type a mesh name above to enable Build mesh.</b>")
        )

    def update_buttons(self) -> None:
        if not hasattr(self, "build_button"):
            return
        busy = self.busy()
        self.build_button.setEnabled(self.project is not None and not busy and bool(self.mesh_name()))
        self.find_button.setEnabled(self.project is not None and not busy and self.streams_box.isChecked())
        self.cancel_button.setEnabled(busy)
        spec = self.project.get_mesh(self.mesh_name()) if self.project and self.mesh_name() else None
        self.folder_button.setEnabled(spec is not None and self.project.mesh_dir(spec).exists())

    def fixed_points_file(self, spec: MeshSpec) -> str:
        """The chosen point layer copied to the mesh folder (so memory layers work), else the chosen file."""
        layer = self.layer.currentLayer()
        if layer is None:
            return self.file.filePath().strip()
        folder = self.project.mesh_dir(spec)
        folder.mkdir(parents=True, exist_ok=True)
        path = folder / "fixed_points.gpkg"
        options = QgsVectorFileWriter.SaveVectorOptions()
        options.driverName = "GPKG"
        options.layerName = "fixed_points"
        error = QgsVectorFileWriter.writeAsVectorFormatV3(layer, str(path), QgsCoordinateTransformContext(), options)
        if error[0] != QgsVectorFileWriter.WriterError.NoError:
            raise RuntimeError(f"Could not copy {layer.name()} to {path}: {error[1]}")
        return str(path)

    def build(self) -> None:
        if self.project is None or not self.mesh_name() or self.busy():
            return
        existing = self.project.get_mesh(self.mesh_name())
        if existing is not None and existing.status == "built":
            answer = QMessageBox.question(self, "Mesh", f"'{existing.name}' is already built. Replace it?")
            if answer != QMessageBox.StandardButton.Yes:
                return
        spec = self.trial_spec()
        try:
            spec.fixed_points_file = self.fixed_points_file(spec)
        except RuntimeError as exc:
            QMessageBox.warning(self, "Mesh", str(exc))
            return
        self.task = MeshTask(self.project, spec)
        self.task.message.connect(self.append)
        self.task.progressChanged.connect(lambda pct: self.progress.setValue(int(pct)))
        self.task.completed.connect(self.on_built)
        self.progress.setValue(0)
        self.update_buttons()
        QgsApplication.taskManager().addTask(self.task)

    def on_built(self, ok: bool) -> None:
        task, self.task = self.task, None
        self.update_buttons()
        if task is None:
            return
        if not ok or task.result is None:
            self.append(task.error or "Mesh build failed.")
            return
        populate_mesh(QgsProject.instance(), task.project, task.spec)
        try:
            self.append(refresh_qgz(task.project))
        except (RuntimeError, OSError, ValueError) as exc:
            self.append(str(exc))
        if self.name.findText(task.spec.name) < 0:
            self.name.blockSignals(True)
            self.name.addItem(task.spec.name)
            self.name.setEditText(task.spec.name)
            self.name.blockSignals(False)
        snippet = mesh_outputs(task.project, task.spec)["snippet"]
        self.append(f"MUT instructions: {snippet}")
        if snippet.exists():
            self.append(snippet.read_text(encoding="utf-8"))
        self.show_preview()

    def cancel(self) -> None:
        for task in (self.task, self.stream_task):
            if task is not None:
                task.cancel()

    def open_folder(self) -> None:
        spec = self.project.get_mesh(self.mesh_name()) if self.project else None
        if spec is not None:
            QDesktopServices.openUrl(QUrl.fromLocalFile(str(self.project.mesh_dir(spec))))

    def cleanup(self) -> None:
        self.cancel()
