"""'Add streams...' dialog: find rivers; attach as CLN, map onto mesh for SWF, or rebuild with fixed stream nodes."""

from __future__ import annotations

from pathlib import Path

from qgis.core import QgsApplication, QgsProject, QgsSettings
from qgis.gui import QgsFileWidget
from qgis.PyQt.QtCore import QUrl
from qgis.PyQt.QtGui import QDesktopServices
from qgis.PyQt.QtWidgets import (
    QButtonGroup,
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
    QRadioButton,
    QSpinBox,
    QVBoxLayout,
)

from ..core.mesh import mesh_outputs
from ..core.project import STREAM_SOURCES, MeshSpec, Project, StreamOptions
from ..core.qgis_project import (
    bind_open_qgz,
    populate_mesh,
    refresh_qgz,
    release_mesh_layers,
    release_streams_preview,
    streams_layer,
)
from ..core.streams import SOURCE_LABELS
from .focus_dialog import FocusAreaDialog
from .task import ClnExportTask, MeshTask, StreamMapTask, StreamsTask
from .wizard import SETTINGS, _spin


def _int_spin(lo: int, hi: int, value: int, step: int = 1, suffix: str = "") -> QSpinBox:
    box = QSpinBox()
    box.setRange(lo, hi)
    box.setSingleStep(step)
    box.setValue(value)
    if suffix:
        box.setSuffix(suffix)
    return box


class StreamsDialog(QDialog):
    def __init__(self, iface) -> None:
        super().__init__(iface.mainWindow())
        self.iface = iface
        self.setWindowTitle("MUT Model Builder: add streams")
        self.resize(720, 720)
        self.project: Project | None = None
        self.task: MeshTask | None = None
        self.stream_task: StreamsTask | None = None
        self.cln_task: ClnExportTask | None = None
        self.map_task: StreamMapTask | None = None
        self._last_network = None

        layout = QVBoxLayout(self)
        intro = QLabel(
            "Add rivers and streams to a <b>built</b> mesh under <tt>2_mesh/&lt;name&gt;/</tt>. "
            "Find streams, then either attach them as a <b>CLN</b> 1-D domain, "
            "<b>map</b> them onto the existing mesh for <b>SWF</b> (.nchos, no remesh), "
            "or <b>rebuild</b> the mesh with fixed stream nodes for <b>SWF</b>. "
            "CLN is optional. Build the mesh first with <b>New mesh...</b> if none exist yet."
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
        self.name.lineEdit().setPlaceholderText("Built mesh name")
        self.name.setToolTip("Select a built mesh from the project (needs 2_mesh/<slug>/ with Grid Builder files)")
        self.name.currentTextChanged.connect(self.name_changed)
        form.addRow("Mesh name", self.name)
        layout.addLayout(form)

        layout.addWidget(self._streams_box())

        self.summary = QLabel("Open a project file.")
        self.summary.setWordWrap(True)
        layout.addWidget(self.summary)

        row = QHBoxLayout()
        self.build_button = QPushButton("Attach streams as CLN")
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

        start = FocusAreaDialog.open_project_yaml() or QgsSettings().value(f"{SETTINGS}/streams_project", "", type=str)
        if not start:
            start = QgsSettings().value(f"{SETTINGS}/mesh_project", "", type=str)
        if start and Path(start).is_file():
            self.yaml.setFilePath(str(start))
        self.update_buttons()

    def _streams_box(self) -> QGroupBox:
        box = QGroupBox("Stream options")
        form = QFormLayout(box)

        self.mode_cln = QRadioButton("Keep current mesh — attach as CLN (1-D domain)")
        self.mode_cln.setToolTip(
            "Does not remesh. Writes Streams_CLN.xyzList beside the mesh; Build MUT project emits "
            "cln mesh intersection so MUT splits segments at GWF cells."
        )
        self.mode_mapped = QRadioButton("Keep current mesh — map onto mesh for SWF (.nchos)")
        self.mode_mapped.setToolTip(
            "Does not remesh and does not create CLN. Snaps ranked streams onto existing mesh nodes "
            "(shortest paths along triangle edges) and writes .nchos for SWF choose cells from gb nodes."
        )
        self.mode_embedded = QRadioButton("Rebuild mesh with fixed stream nodes for SWF")
        self.mode_embedded.setToolTip(
            "Rebuilds the Delaunay mesh with fixed nodes along rivers and mesh edges along them "
            "(for SWF choose cells from gb nodes). Does not create CLN."
        )
        self.mode_cln.setChecked(True)
        self.mode_group = QButtonGroup(box)
        self.mode_group.addButton(self.mode_cln)
        self.mode_group.addButton(self.mode_mapped)
        self.mode_group.addButton(self.mode_embedded)
        form.addRow(self.mode_cln)
        form.addRow(self.mode_mapped)
        form.addRow(self.mode_embedded)
        self.mode_group.buttonClicked.connect(lambda _b: self.update_buttons())

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
        self.stream_spacing.setToolTip(
            "Distance between CLN vertices, mapped samples, or fixed mesh nodes along the streams "
            "(default: boundary spacing)"
        )
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
        self.stream_source_changed()
        return box

    def stream_source_changed(self) -> None:
        source = self.stream_source.currentData()
        self.dem_threshold.setEnabled(source == "dem")
        self.include_canals.setEnabled(source in ("auto", "osm"))

    def stream_options(self) -> StreamOptions:
        return StreamOptions(
            source=self.stream_source.currentData(),
            levels=self.stream_levels.value(),
            min_length_km=self.stream_min_length.value(),
            spacing_m=self.stream_spacing.value(),
            main_river=self.main_river.text().strip(),
            include_canals=self.include_canals.isChecked(),
            dem_threshold_km2=self.dem_threshold.value(),
        )

    def set_stream_options(self, opts: StreamOptions | None, stream_mode: str = "cln") -> None:
        if stream_mode == "embedded":
            self.mode_embedded.setChecked(True)
        elif stream_mode == "mapped":
            self.mode_mapped.setChecked(True)
        else:
            self.mode_cln.setChecked(True)
        if opts is None:
            return
        self.stream_source.setCurrentIndex(max(0, self.stream_source.findData(opts.source)))
        self.stream_levels.setValue(opts.levels)
        self.stream_min_length.setValue(opts.min_length_km)
        self.stream_spacing.setValue(opts.spacing_m)
        self.main_river.setText(opts.main_river)
        self.include_canals.setChecked(opts.include_canals)
        self.dem_threshold.setValue(opts.dem_threshold_km2)

    def stream_mode(self) -> str:
        if self.mode_embedded.isChecked():
            return "embedded"
        if self.mode_mapped.isChecked():
            return "mapped"
        return "cln"

    def has_ranked_streams(self) -> bool:
        if self._last_network is not None:
            return True
        if self.project is None or not self.mesh_name():
            return False
        from ..core.cln_export import has_stream_geometry

        return has_stream_geometry(self.project, self.trial_spec())

    def find_streams(self) -> None:
        if self.project is None or self.busy():
            return
        if not self.mesh_name():
            QMessageBox.warning(self, "Streams", "Choose a built mesh name first.")
            return
        spec = self.trial_spec()
        self._cancel_logged = False
        release_streams_preview(QgsProject.instance(), self.project, spec)
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
            self._last_network = None
            self.stream_summary.setText("No streams found; see the log.")
            self.append(task.error or "Stream search failed.")
            return
        self._last_network = task.network
        self.stream_summary.setText(task.network.describe().replace("\n", "<br>"))
        title = f"{task.spec.name} streams (preview)"
        qproject = QgsProject.instance()
        for old in qproject.mapLayersByName(title):
            qproject.removeMapLayer(old.id())
        layer = streams_layer(task.path, title)
        if layer.isValid():
            qproject.addMapLayer(layer)
        self.update_buttons()

    def busy(self) -> bool:
        return any(t is not None for t in (self.task, self.stream_task, self.cln_task, self.map_task))

    def append(self, text: str) -> None:
        self.log.appendPlainText(text)

    def mesh_name(self) -> str:
        return self.name.currentText().strip()

    def _built_meshes(self, project: Project) -> list:
        out = []
        for m in project.meshes:
            if m.status == "built" and (project.mesh_dir(m) / f"{m.slug}.grd").exists():
                out.append(m)
        return out

    def load_project(self, path: str) -> None:
        self.project = None
        self._last_network = None
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
        QgsSettings().setValue(f"{SETTINGS}/streams_project", path)
        self.project = project
        built = self._built_meshes(project)
        self.name.blockSignals(True)
        self.name.addItems([m.name for m in built])
        if built:
            self.name.setCurrentIndex(0)
        else:
            self.name.setEditText("")
        self.name.blockSignals(False)
        if built:
            self.name_changed(built[0].name)
        else:
            self.refresh_summary()

    def name_changed(self, name: str) -> None:
        self._last_network = None
        spec = self.project.get_mesh(name.strip()) if self.project and name.strip() else None
        if spec is not None:
            self.set_stream_options(spec.streams, spec.stream_mode if spec.stream_mode != "none" else "cln")
        self.refresh_summary()

    def trial_spec(self) -> MeshSpec:
        existing = self.project.get_mesh(self.mesh_name()) if self.project and self.mesh_name() else None
        mode = self.stream_mode()
        if existing is None:
            return MeshSpec(
                name=self.mesh_name() or "mesh",
                streams=self.stream_options(),
                stream_mode=mode,
            )
        return MeshSpec(
            name=existing.name,
            boundary_spacing_m=existing.boundary_spacing_m,
            boundary_tolerance_m=existing.boundary_tolerance_m,
            interior_points=existing.interior_points,
            smoothing_iterations=existing.smoothing_iterations,
            smoothing_tolerance_m=existing.smoothing_tolerance_m,
            fixed_points_file=existing.fixed_points_file,
            streams=self.stream_options(),
            stream_mode=mode,
            cln_xyz_file=existing.cln_xyz_file,
            status=existing.status,
            created=existing.created,
        )

    def refresh_summary(self) -> None:
        self.update_buttons()
        if self.project is None:
            return
        built = self._built_meshes(self.project)
        existing = self.project.get_mesh(self.mesh_name()) if self.mesh_name() else None
        names = ", ".join(m.name for m in built) or "none"
        bits = [
            f"<b>{self.project.name}</b>",
            f"Built meshes: {names}",
        ]
        if existing is None and self.mesh_name():
            bits.append("<b>Mesh name is not a built mesh in this project.</b>")
        elif not built:
            bits.append("<b>No built meshes yet — use New mesh... first.</b>")
        elif existing is not None:
            bits.append(
                f"Selected: {existing.name} (stream_mode={existing.stream_mode or 'none'}); "
                f"folder {self.project.mesh_dir(existing)}"
            )
        self.summary.setText("<br>".join(bits))

    def update_buttons(self) -> None:
        if not hasattr(self, "build_button"):
            return
        busy = self.busy()
        mode = self.stream_mode()
        existing = self.project.get_mesh(self.mesh_name()) if self.project and self.mesh_name() else None
        mesh_built = (
            existing is not None
            and existing.status == "built"
            and self.project is not None
            and (self.project.mesh_dir(existing) / f"{existing.slug}.grd").exists()
        )
        if mode == "cln":
            self.build_button.setText("Attach streams as CLN")
            can = (
                self.project is not None
                and not busy
                and bool(self.mesh_name())
                and mesh_built
                and self.has_ranked_streams()
            )
            self.build_button.setEnabled(can)
        elif mode == "mapped":
            self.build_button.setText("Map streams to mesh")
            self.build_button.setEnabled(
                self.project is not None
                and not busy
                and bool(self.mesh_name())
                and mesh_built
                and self.has_ranked_streams()
            )
        else:
            self.build_button.setText("Rebuild mesh with streams")
            self.build_button.setEnabled(
                self.project is not None and not busy and bool(self.mesh_name()) and mesh_built
            )
        self.find_button.setEnabled(self.project is not None and not busy and mesh_built)
        self.cancel_button.setEnabled(busy)
        self.folder_button.setEnabled(
            existing is not None and self.project is not None and self.project.mesh_dir(existing).exists()
        )

    def build(self) -> None:
        if self.project is None or not self.mesh_name() or self.busy():
            return
        mode = self.stream_mode()
        if mode == "cln":
            self.attach_cln()
            return
        if mode == "mapped":
            self.map_streams()
            return
        existing = self.project.get_mesh(self.mesh_name())
        if existing is None or existing.status != "built":
            QMessageBox.warning(self, "Streams", "Choose a built mesh first.")
            return
        answer = QMessageBox.question(
            self,
            "Mesh",
            f"Rebuild '{existing.name}' with fixed stream nodes? This replaces the triangulation.",
        )
        if answer != QMessageBox.StandardButton.Yes:
            return
        spec = self.trial_spec()
        spec.stream_mode = "embedded"
        if self._last_network is None and not self.has_ranked_streams():
            QMessageBox.warning(self, "Streams", "Find streams first, then rebuild.")
            return
        self._cancel_logged = False
        release_mesh_layers(QgsProject.instance(), self.project, spec)
        self.task = MeshTask(self.project, spec)
        self.task.message.connect(self.append)
        self.task.progressChanged.connect(lambda pct: self.progress.setValue(int(pct)))
        self.task.completed.connect(self.on_built)
        self.progress.setValue(0)
        self.update_buttons()
        QgsApplication.taskManager().addTask(self.task)

    def map_streams(self) -> None:
        if self.project is None or not self.mesh_name() or self.busy():
            return
        existing = self.project.get_mesh(self.mesh_name())
        if existing is None or existing.status != "built":
            QMessageBox.warning(
                self,
                "Map streams",
                "Build the regional mesh first (New mesh...), then map streams onto it for SWF.",
            )
            return
        if not self.has_ranked_streams():
            QMessageBox.warning(self, "Map streams", "Find streams first, then map onto the mesh.")
            return
        spec = self.trial_spec()
        spec.stream_mode = "mapped"
        self._cancel_logged = False
        release_mesh_layers(QgsProject.instance(), self.project, spec)
        self.map_task = StreamMapTask(self.project, spec, network=self._last_network)
        self.map_task.message.connect(self.append)
        self.map_task.progressChanged.connect(lambda pct: self.progress.setValue(int(pct)))
        self.map_task.completed.connect(self.on_mapped)
        self.progress.setValue(0)
        self.update_buttons()
        QgsApplication.taskManager().addTask(self.map_task)

    def attach_cln(self) -> None:
        if self.project is None or not self.mesh_name() or self.busy():
            return
        existing = self.project.get_mesh(self.mesh_name())
        if existing is None or existing.status != "built":
            QMessageBox.warning(
                self,
                "CLN",
                "Build the regional mesh first (New mesh...), then attach streams as CLN.",
            )
            return
        spec = self.trial_spec()
        spec.stream_mode = "cln"
        if not self.has_ranked_streams():
            QMessageBox.warning(self, "CLN", "Find streams first, then attach as CLN.")
            return
        self.cln_task = ClnExportTask(self.project, spec, network=self._last_network)
        self.cln_task.message.connect(self.append)
        self.cln_task.progressChanged.connect(lambda pct: self.progress.setValue(int(pct)))
        self.cln_task.completed.connect(self.on_cln)
        self.progress.setValue(0)
        self.update_buttons()
        QgsApplication.taskManager().addTask(self.cln_task)

    def on_cln(self, ok: bool) -> None:
        task, self.cln_task = self.cln_task, None
        self.update_buttons()
        if task is None:
            return
        if not ok or task.result is None:
            self.append(task.error or "CLN export failed.")
            return
        self.project = task.project
        snippet = task.result.outputs.get("snippet")
        if snippet is not None and snippet.exists():
            self.append(f"CLN MUT instructions: {snippet}")
            self.append(snippet.read_text(encoding="utf-8"))
        self.refresh_summary()

    def on_mapped(self, ok: bool) -> None:
        task, self.map_task = self.map_task, None
        self.update_buttons()
        if task is None:
            return
        if not ok or task.result is None:
            self.append(task.error or "Stream mapping failed.")
            return
        self.project = task.project
        populate_mesh(QgsProject.instance(), task.project, task.spec)
        try:
            self.append(refresh_qgz(task.project))
        except (RuntimeError, OSError, ValueError) as exc:
            self.append(str(exc))
        bind_open_qgz(task.project)
        snippet = task.result.outputs.get("snippet")
        if snippet is not None and Path(snippet).exists():
            self.append(f"SWF .nchos written; MUT instructions: {snippet}")
            self.append(Path(snippet).read_text(encoding="utf-8"))
        self.append(f"Mapped {task.result.n_stream_nodes:,} stream nodes on {task.result.n_paths} path(s)")
        self.refresh_summary()

    def on_built(self, ok: bool) -> None:
        task, self.task = self.task, None
        self.update_buttons()
        if task is None:
            return
        if not ok or task.result is None:
            self.append(task.error or "Mesh rebuild failed.")
            return
        populate_mesh(QgsProject.instance(), task.project, task.spec)
        try:
            self.append(refresh_qgz(task.project))
        except (RuntimeError, OSError, ValueError) as exc:
            self.append(str(exc))
        bind_open_qgz(task.project)
        snippet = mesh_outputs(task.project, task.spec)["snippet"]
        self.append(f"MUT instructions: {snippet}")
        if snippet.exists():
            self.append(snippet.read_text(encoding="utf-8"))
        self.project = task.project
        self.refresh_summary()

    def cancel(self) -> None:
        tasks = [t for t in (self.task, self.stream_task, self.cln_task, self.map_task) if t is not None]
        if not tasks:
            return
        if not getattr(self, "_cancel_logged", False):
            self.append("Cancel requested — stopping after the current step…")
            self._cancel_logged = True
        for task in tasks:
            task.cancel()

    def open_folder(self) -> None:
        spec = self.project.get_mesh(self.mesh_name()) if self.project else None
        if spec is not None:
            QDesktopServices.openUrl(QUrl.fromLocalFile(str(self.project.mesh_dir(spec))))

    def cleanup(self) -> None:
        self.cancel()
