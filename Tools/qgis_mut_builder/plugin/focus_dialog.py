"""'New focus area...' dialog: outline a subregion of a built regional model, review it, then build its ~1 m DEM."""

from __future__ import annotations

import json
from pathlib import Path

from osgeo import ogr
from qgis.core import (
    Qgis,
    QgsApplication,
    QgsCoordinateReferenceSystem,
    QgsCoordinateTransform,
    QgsGeometry,
    QgsPointXY,
    QgsProject,
    QgsRectangle,
    QgsSettings,
    QgsTask,
)
from qgis.gui import QgsFileWidget, QgsMapTool, QgsMapToolExtent, QgsRubberBand, QgsVertexMarker
from qgis.PyQt.QtCore import Qt, QUrl, pyqtSignal
from qgis.PyQt.QtGui import QColor, QDesktopServices
from qgis.PyQt.QtWidgets import (
    QButtonGroup,
    QCheckBox,
    QComboBox,
    QDialog,
    QFormLayout,
    QHBoxLayout,
    QLabel,
    QLineEdit,
    QListWidget,
    QMessageBox,
    QPlainTextEdit,
    QProgressBar,
    QPushButton,
    QRadioButton,
    QSizePolicy,
    QStackedWidget,
    QVBoxLayout,
    QWidget,
)

from ..core import boundary as bnd
from ..core import local_watershed as lw
from ..core.focus import define_focus, describe, focus_outputs, preview_summary
from ..core.geo import MAX_FOCUS_RES_M
from ..core.project import FocusArea, Project
from ..core.qgis_project import bind_open_qgz, populate_focus, refresh_qgz, release_focus_layers
from .task import FocusTask
from .wizard import SETTINGS, _spin, confirm_grid_size

WGS84 = QgsCoordinateReferenceSystem("EPSG:4326")
DELINEATION_LABELS = {
    "regional_dem": "Local watershed from the regional ground surface (GRASS)",
    "global_watersheds": "Local watershed from Global Watersheds (MERIT-Hydro, online)",
    "none": "Feature outline only (no watershed)",
}


def qgs_geometry(geojson: dict) -> QgsGeometry:
    return QgsGeometry.fromWkt(ogr.CreateGeometryFromJson(json.dumps(geojson)).ExportToWkt())


class DigitizeTool(QgsMapTool):
    """Left-click adds a vertex, Backspace removes the last one, right-click or double-click closes the polygon."""

    digitized = pyqtSignal(QgsGeometry)

    def __init__(self, canvas) -> None:
        super().__init__(canvas)
        self.band = QgsRubberBand(canvas, Qgis.GeometryType.Polygon)
        self.band.setStrokeColor(QColor(230, 120, 0))
        self.band.setFillColor(QColor(230, 120, 0, 40))
        self.band.setWidth(2)
        self.points: list[QgsPointXY] = []
        self.after_double_click = False

    def _redraw(self, cursor: QgsPointXY | None = None) -> None:
        points = self.points + ([cursor] if cursor is not None else [])
        self.band.reset(Qgis.GeometryType.Polygon)
        for p in points:
            self.band.addPoint(p, False)
        self.band.show()
        self.band.updatePosition()
        self.band.update()

    def canvasMoveEvent(self, event) -> None:  # noqa: N802 (Qt API)
        if self.points:
            self._redraw(self.toMapCoordinates(event.pos()))

    def canvasReleaseEvent(self, event) -> None:  # noqa: N802
        if self.after_double_click:
            self.after_double_click = False
            return
        if event.button() == Qt.MouseButton.RightButton:
            self.finish()
        elif event.button() == Qt.MouseButton.LeftButton:
            self.points.append(self.toMapCoordinates(event.pos()))
            self._redraw()

    def canvasDoubleClickEvent(self, _event) -> None:  # noqa: N802
        self.after_double_click = True
        self.finish()

    def keyPressEvent(self, event) -> None:  # noqa: N802
        if event.key() == Qt.Key.Key_Backspace and self.points:
            self.points.pop()
            self._redraw()
        elif event.key() == Qt.Key.Key_Escape:
            self.reset()

    def finish(self) -> None:
        points = list(dict.fromkeys((p.x(), p.y()) for p in self.points))
        self.reset()
        if len(points) >= 3:
            self.digitized.emit(QgsGeometry.fromPolygonXY([[QgsPointXY(x, y) for x, y in points]]))

    def reset(self) -> None:
        self.points = []
        self.band.reset(Qgis.GeometryType.Polygon)

    def deactivate(self) -> None:
        self.reset()
        super().deactivate()


class FocusAreaDialog(QDialog):
    METHODS = {
        "rectangle": "Rectangle drawn on the map",
        "digitized": "Outline digitized on the map",
        "file": "Polygon(s) from a vector file",
        "feature": "Feature in the regional area (e.g. a wetland or lake) and its local watershed",
    }

    def __init__(self, iface) -> None:
        super().__init__(iface.mainWindow())
        self.iface = iface
        self.canvas = iface.mapCanvas()
        self.setWindowTitle("MUT Model Builder: new focus area")
        self.resize(760, 760)
        self.project: Project | None = None
        self.pending: dict | None = None
        self.saved = False
        self.candidates: list[lw.FeatureCandidate] = []
        self.task: QgsTask | None = None
        self.build_task: FocusTask | None = None
        self.messages: list[str] = []
        self.report_path: Path | None = None
        self.extent_tool: QgsMapToolExtent | None = None
        self.digitize_tool: DigitizeTool | None = None
        self.previous_tool = None

        layout = QVBoxLayout(self)
        intro = QLabel(
            "A focus area is a named subregion of a built regional model with its own ~1 m ground surface "
            "(1_data/focus/&lt;name&gt;/). Outline it, check the orange outline and grid size, click "
            "<b>Accept extent</b>, then <b>Download 1 m DEM</b>. The regional surfaces are not changed."
        )
        intro.setWordWrap(True)
        layout.addWidget(intro)

        form = QFormLayout()
        self.yaml = QgsFileWidget()
        self.yaml.setFilter("Project files (*.yaml *.yml)")
        self.yaml.fileChanged.connect(self.load_project)
        form.addRow("Regional project file", self.yaml)
        self.name = QComboBox()
        self.name.setEditable(True)
        self.name.setInsertPolicy(QComboBox.InsertPolicy.NoInsert)
        self.name.lineEdit().setPlaceholderText("Focus area name, e.g. Luther Marsh")
        self.name.currentTextChanged.connect(self.name_changed)
        form.addRow("Focus area name", self.name)
        layout.addLayout(form)

        self.group = QButtonGroup(self)
        for i, (method, text) in enumerate(self.METHODS.items()):
            radio = QRadioButton(text)
            radio.setProperty("method", method)
            self.group.addButton(radio, i)
            layout.addWidget(radio)
        self.group.button(0).setChecked(True)
        self.group.idToggled.connect(lambda i, checked: checked and self.stack.setCurrentIndex(i))

        self.stack = QStackedWidget()
        self.stack.addWidget(self._tool_panel("Draw rectangle on map", "Click and drag on the QGIS map.", self.start_extent))
        self.stack.addWidget(
            self._tool_panel(
                "Digitize outline on map",
                "Left-click vertices; Backspace undoes; right-click or double-click closes the outline.",
                self.start_digitize,
            )
        )
        file_panel = QWidget()
        row = QHBoxLayout(file_panel)
        self.file = QgsFileWidget()
        self.file.setFilter("Vector files (*.shp *.gpkg *.geojson *.json *.kml *.gml);;All files (*)")
        self.file.fileChanged.connect(self.use_file)
        row.addWidget(self.file)
        self.stack.addWidget(file_panel)
        self.stack.addWidget(self._feature_panel())
        self.stack.currentChanged.connect(self.fit_stack)
        layout.addWidget(self.stack)

        form = QFormLayout()
        self.margin = _spin(0, 5000, 200, 0, " m", 50)
        self.res = _spin(0.25, MAX_FOCUS_RES_M, 1.0, 2, " m", 0.5)
        for box in (self.margin, self.res):
            box.valueChanged.connect(self.settings_changed)
        form.addRow("Margin around the outline", self.margin)
        form.addRow("Cell size", self.res)
        layout.addLayout(form)
        self.summary = QLabel("Open a regional project file.")
        self.summary.setWordWrap(True)
        layout.addWidget(self.summary)

        row = QHBoxLayout()
        self.accept_button = QPushButton("Accept extent")
        self.accept_button.clicked.connect(self.accept_extent)
        self.download_button = QPushButton("Download 1 m DEM")
        self.download_button.clicked.connect(self.download)
        self.cancel_button = QPushButton("Cancel")
        self.cancel_button.clicked.connect(self.cancel)
        self.report_button = QPushButton("Open report")
        self.report_button.clicked.connect(self.open_report)
        self.refresh = QCheckBox("Ignore cached downloads")
        for w in (self.accept_button, self.download_button, self.cancel_button, self.report_button, self.refresh):
            row.addWidget(w)
        row.addStretch()
        self.lakes = QCheckBox("Correct lakes with bathymetry (measured surveys where available, otherwise estimated)")
        self.lakes.setChecked(True)
        self.lakes.setToolTip(
            "Lidar DEMs show lakes as a flat water surface. When checked, lakes are lowered to a lake bottom and the "
            "report lists every lake with how its depth was found. Saved as the project's lake_bathymetry option."
        )
        layout.addWidget(self.lakes)
        layout.addLayout(row)
        self.progress = QProgressBar()
        self.log = QPlainTextEdit()
        self.log.setReadOnly(True)
        layout.addWidget(self.progress)
        layout.addWidget(self.log, 1)
        self.fit_stack(self.stack.currentIndex())

        self.region_band = self._band(QColor(26, 150, 65), 2)
        self.feature_band = self._band(QColor(120, 120, 120), 2)
        self.outline_band = self._band(QColor(230, 120, 0), 3)
        self.grid_band = self._band(QColor(230, 120, 0), 1)
        self.grid_band.setLineStyle(Qt.PenStyle.DashLine)
        self.outlet_marker = QgsVertexMarker(self.canvas)
        self.outlet_marker.setColor(QColor(0, 90, 220))
        self.outlet_marker.setIconType(QgsVertexMarker.IconType.ICON_CIRCLE)
        self.outlet_marker.setIconSize(12)
        self.outlet_marker.setPenWidth(3)
        self.outlet_marker.hide()
        self.finished.connect(lambda _result: self.cleanup())

        start = self.open_project_yaml() or QgsSettings().value(f"{SETTINGS}/focus_project", "", type=str)
        if start and Path(start).is_file():
            self.yaml.setFilePath(str(start))
        self.update_buttons()

    @staticmethod
    def open_project_yaml() -> Path | None:
        """The project.yaml beside the open QGIS project (a .qgz written by the plugin), if any."""
        qgz = QgsProject.instance().absoluteFilePath()
        if not qgz:
            return None
        folder = Path(qgz).parent
        for name in ("project.yaml", "project.yml"):
            if (folder / name).is_file():
                return folder / name
        found = sorted(folder.glob("*.yaml")) + sorted(folder.glob("*.yml"))
        return found[0] if len(found) == 1 else None

    def fit_stack(self, index: int) -> None:
        """Size the method panel to the current page so short panels do not leave a gap."""
        for i in range(self.stack.count()):
            policy = QSizePolicy.Policy.Preferred if i == index else QSizePolicy.Policy.Ignored
            self.stack.widget(i).setSizePolicy(QSizePolicy.Policy.Preferred, policy)
        self.stack.setMaximumHeight(self.stack.widget(index).sizeHint().height())
        self.stack.updateGeometry()

    def _tool_panel(self, button_text: str, hint: str, slot) -> QWidget:
        panel = QWidget()
        row = QHBoxLayout(panel)
        button = QPushButton(button_text)
        button.clicked.connect(slot)
        label = QLabel(hint)
        label.setWordWrap(True)
        row.addWidget(button)
        row.addWidget(label, 1)
        return panel

    def _feature_panel(self) -> QWidget:
        panel = QWidget()
        box = QVBoxLayout(panel)
        row = QHBoxLayout()
        self.feature_name = QLineEdit()
        self.feature_name.setPlaceholderText("Feature name inside the regional area, e.g. Luther Marsh")
        self.feature_name.returnPressed.connect(self.search)
        self.search_button = QPushButton("Search")
        self.search_button.clicked.connect(self.search)
        row.addWidget(self.feature_name)
        row.addWidget(self.search_button)
        box.addLayout(row)
        self.results = QListWidget()
        self.results.currentRowChanged.connect(self.show_candidate)
        self.results.itemDoubleClicked.connect(lambda _item: self.use_feature())
        box.addWidget(self.results)
        row = QHBoxLayout()
        self.delineation = QComboBox()
        for key, text in DELINEATION_LABELS.items():
            self.delineation.addItem(text, key)
        self.use_button = QPushButton("Outline selected feature")
        self.use_button.clicked.connect(self.use_feature)
        row.addWidget(self.delineation, 1)
        row.addWidget(self.use_button)
        box.addLayout(row)
        note = QLabel(
            "Features come from OpenStreetMap. The outlet is the cell of largest flow accumulation in the feature; "
            "the focus outline is the watershed draining to it plus the feature itself (blue marker: outlet)."
        )
        note.setWordWrap(True)
        box.addWidget(note)
        return panel

    def _band(self, colour: QColor, width: int) -> QgsRubberBand:
        band = QgsRubberBand(self.canvas, Qgis.GeometryType.Polygon)
        band.setStrokeColor(colour)
        band.setFillColor(QColor(0, 0, 0, 0))
        band.setWidth(width)
        return band

    def append(self, text: str) -> None:
        self.log.appendPlainText(text)

    def method(self) -> str:
        return self.group.checkedButton().property("method")

    def focus_name(self) -> str:
        return self.name.currentText().strip()

    def load_project(self, path: str) -> None:
        self.project = None
        self.pending = None
        self.name.blockSignals(True)
        self.name.clear()
        self.name.blockSignals(False)
        self.region_band.reset(Qgis.GeometryType.Polygon)
        if not path or not Path(path).is_file():
            self.summary.setText("Open a regional project file.")
            self.update_buttons()
            return
        try:
            project = Project.load(Path(path))
        except (OSError, ValueError, TypeError) as exc:
            self.summary.setText(f"Could not read {path}: {exc}")
            self.update_buttons()
            return
        if not (project.surfaces_dir / "ground_top.tif").exists():
            self.summary.setText(
                f"{project.name}: the regional ground surface has not been built yet. Run the regional model first "
                "('New hydrogeological model...')."
            )
            self.update_buttons()
            return
        QgsSettings().setValue(f"{SETTINGS}/focus_project", path)
        self.project = project
        self.lakes.setChecked(project.options.lake_bathymetry)
        self.res.blockSignals(True)
        self.res.setValue(project.options.fine_resolution_m)
        self.res.blockSignals(False)
        self.name.blockSignals(True)
        self.name.addItems([f.name for f in project.focus_areas])
        self.name.setEditText("")
        self.name.blockSignals(False)
        crs = QgsCoordinateReferenceSystem(f"EPSG:{project.epsg}")
        self.region_band.setToGeometry(QgsGeometry.fromRect(QgsRectangle(*project.regional_grid().bounds)), crs)
        existing = ", ".join(f"{f.name} ({f.status})" for f in project.focus_areas) or "none yet"
        self.summary.setText(f"<b>{project.name}</b> (green: regional grid). Focus areas: {existing}.")
        self.show_preview()

    def name_changed(self, name: str) -> None:
        focus = self.project.get_focus(name.strip()) if self.project and name.strip() else None
        if focus is None:
            self.saved = False
            self.update_buttons()
            return
        try:
            saved = json.loads(Path(focus.boundary_file).read_text(encoding="utf-8"))["features"][0]
        except (OSError, ValueError, KeyError, IndexError) as exc:
            self.summary.setText(f"Focus area '{focus.name}': cannot read {focus.boundary_file} ({exc})")
            return
        for box, value in ((self.margin, focus.margin_m), (self.res, focus.resolution_m)):
            box.blockSignals(True)
            box.setValue(value)
            box.blockSignals(False)
        self.pending = {
            "geometry": saved["geometry"],
            "mode": focus.mode,
            "source": focus.source,
            "feature_name": focus.feature_name,
            "delineation": focus.delineation,
            "outlet": (focus.outlet_lat, focus.outlet_lon) if focus.outlet_lat is not None else None,
            "citation": (saved.get("properties") or {}).get("citation", ""),
            "feature_geometry": None,
        }
        self.saved = True
        self.report_path = focus_outputs(self.project, focus)["report"]
        self.show_preview(zoom=True)

    def settings_changed(self) -> None:
        self.saved = False
        self.show_preview()

    def set_pending(self, geometry: dict, mode: str, source: str, **extra) -> None:
        self.pending = {
            "geometry": geometry,
            "mode": mode,
            "source": source,
            "feature_name": extra.get("feature_name", ""),
            "delineation": extra.get("delineation", "none"),
            "outlet": extra.get("outlet"),
            "citation": extra.get("citation", ""),
            "feature_geometry": extra.get("feature_geometry"),
        }
        self.saved = False
        self.show_preview(zoom=True)

    def trial_focus(self) -> FocusArea:
        p = self.pending or {}
        outlet = p.get("outlet") or (None, None)
        return FocusArea(
            name=self.focus_name() or "focus",
            mode=p.get("mode", "rectangle"),
            source=p.get("source", ""),
            feature_name=p.get("feature_name", ""),
            outlet_lat=outlet[0],
            outlet_lon=outlet[1],
            delineation=p.get("delineation", "none"),
            margin_m=self.margin.value(),
            resolution_m=self.res.value(),
        )

    def show_preview(self, zoom: bool = False) -> None:
        for band in (self.outline_band, self.grid_band, self.feature_band):
            band.reset(Qgis.GeometryType.Polygon)
        self.outlet_marker.hide()
        if self.project is None or self.pending is None:
            self.update_buttons()
            return
        p = self.pending
        self.outline_band.setToGeometry(qgs_geometry(p["geometry"]), WGS84)
        if p.get("feature_geometry") and lw.is_areal(p["feature_geometry"]):
            self.feature_band.setToGeometry(qgs_geometry(p["feature_geometry"]), WGS84)
        if p.get("outlet"):
            self.place_outlet(*p["outlet"])
        try:
            summary = preview_summary(self.project, self.trial_focus(), p["geometry"])
        except (RuntimeError, ValueError) as exc:
            self.summary.setText(f"Outline problem: {exc}")
            self.update_buttons()
            return
        crs = QgsCoordinateReferenceSystem(f"EPSG:{self.project.epsg}")
        grid_rect = QgsRectangle(*summary["bounds"])
        self.grid_band.setToGeometry(QgsGeometry.fromRect(grid_rect), crs)
        lines = [f"<b>Outline:</b> {describe(summary, self.res.value())}", f"Source: {p['source'] or p['mode']}"]
        if p.get("feature_name"):
            lines.append(f"Feature: {p['feature_name']}; {DELINEATION_LABELS.get(p['delineation'], p['delineation'])}")
        if not QgsRectangle(*self.project.regional_grid().bounds).contains(grid_rect):
            lines.append("<span style='color:#b00'>The focus grid extends outside the regional grid.</span>")
        if summary["warning"]:
            lines.append(f"<span style='color:#b00'>{summary['warning']}</span>")
        focus = self.project.get_focus(self.focus_name())
        if self.saved and focus is not None:
            lines.append(f"Saved focus area, status: <b>{focus.status}</b>.")
        else:
            lines.append("Orange: outline; dashed: focus grid. Click <b>Accept extent</b> when it looks right.")
        self.summary.setText("<br>".join(lines))
        if zoom:
            self.zoom_to(grid_rect, crs)
        self.update_buttons()

    def place_outlet(self, lat: float, lon: float) -> None:
        to_canvas = QgsCoordinateTransform(WGS84, self.canvas.mapSettings().destinationCrs(), QgsProject.instance())
        self.outlet_marker.setCenter(to_canvas.transform(QgsPointXY(lon, lat)))
        self.outlet_marker.show()

    def zoom_to(self, rect: QgsRectangle, crs: QgsCoordinateReferenceSystem) -> None:
        transform = QgsCoordinateTransform(crs, self.canvas.mapSettings().destinationCrs(), QgsProject.instance())
        extent = transform.transformBoundingBox(rect)
        extent.scale(1.3)
        self.canvas.setExtent(extent)
        self.canvas.refresh()

    def update_buttons(self) -> None:
        busy = self.task is not None or self.build_task is not None
        ready = self.project is not None
        self.accept_button.setEnabled(ready and not busy and self.pending is not None and bool(self.focus_name()))
        self.download_button.setEnabled(ready and not busy and self.saved)
        self.cancel_button.setEnabled(busy)
        self.search_button.setEnabled(ready and not busy)
        self.use_button.setEnabled(ready and not busy and bool(self.candidates))
        self.report_button.setEnabled(self.report_path is not None and self.report_path.exists())
        self.progress.setRange(0, 0 if self.task is not None else 100)

    def to_wgs84(self, geom: QgsGeometry) -> dict:
        geom = QgsGeometry(geom)
        geom.transform(QgsCoordinateTransform(self.canvas.mapSettings().destinationCrs(), WGS84, QgsProject.instance()))
        return json.loads(geom.asJson(7))

    def _set_tool(self, tool, message: str) -> None:
        if self.project is None:
            QMessageBox.information(
                self, "Focus area", "Choose the regional project.yaml first (Regional project file, '...' button)."
            )
            return
        current = self.canvas.mapTool()
        if current not in (self.extent_tool, self.digitize_tool):
            self.previous_tool = current
        self.canvas.setMapTool(tool)
        self.iface.messageBar().pushInfo("MUT Model Builder", message)

    def _restore_tool(self) -> None:
        current = self.canvas.mapTool()
        if current is None or current not in (self.extent_tool, self.digitize_tool):
            return
        if self.previous_tool is not None:
            self.canvas.setMapTool(self.previous_tool)
        else:
            self.canvas.unsetMapTool(current)

    def start_extent(self) -> None:
        if self.extent_tool is None:
            self.extent_tool = QgsMapToolExtent(self.canvas)
            self.extent_tool.extentChanged.connect(self.extent_drawn)
        self._set_tool(self.extent_tool, "Click and drag the focus-area rectangle on the map.")

    def extent_drawn(self, rect: QgsRectangle) -> None:
        if rect.isEmpty():
            return
        self._restore_tool()
        self.extent_tool.clearRubberBand()
        self.set_pending(self.to_wgs84(QgsGeometry.fromRect(rect).densifyByCount(20)), "rectangle", "QGIS map")
        self.raise_()

    def start_digitize(self) -> None:
        if self.digitize_tool is None:
            self.digitize_tool = DigitizeTool(self.canvas)
            self.digitize_tool.digitized.connect(self.digitized)
        self._set_tool(
            self.digitize_tool, "Click the outline vertices; Backspace undoes; right-click or double-click to finish."
        )

    def digitized(self, geom: QgsGeometry) -> None:
        self._restore_tool()
        if not geom.isGeosValid():
            geom = geom.makeValid()
        self.set_pending(self.to_wgs84(geom), "digitized", "digitized on the QGIS map")
        self.raise_()

    def use_file(self, path: str) -> None:
        if not path or not Path(path).is_file() or self.project is None:
            return
        try:
            geometry = bnd.file_geometry(Path(path))
        except RuntimeError as exc:
            QMessageBox.warning(self, "Focus area file", str(exc))
            return
        self.set_pending(geometry, "file", path)

    def run_task(self, title: str, function, done) -> None:
        """Run ``function(task)`` in the background; ``done(result)`` runs on the main thread."""
        self.messages = []

        def finished(exception, result=None) -> None:
            self.task = None
            self.update_buttons()
            for line in self.messages:
                self.append(line)
            if exception is not None:
                self.append(f"{title} failed: {exception}")
                return
            if result is not None:
                done(result)

        self._cancel_logged = False
        self.task = QgsTask.fromFunction(title, function, on_finished=finished, flags=QgsTask.Flag.CanCancel)
        self.append(f"{title} ...")
        self.update_buttons()
        QgsApplication.taskManager().addTask(self.task)

    def search(self) -> None:
        name = self.feature_name.text().strip()
        if not name or self.project is None or self.task is not None:
            return
        region = self.project.regional_grid().geo_bbox()

        def found(candidates: list) -> None:
            self.candidates = candidates
            self.results.clear()
            for c in candidates:
                area = lw.feature_area_km2(c.geometry, self.project.epsg)
                self.results.addItem(c.display + (f" {area:,.2f} km2" if area else ""))
            if candidates:
                self.results.setCurrentRow(0)
            self.append(f"{len(candidates)} feature(s) named like '{name}' in the regional area.")
            self.update_buttons()

        self.run_task(f"Searching for '{name}'", lambda _task: lw.search_features(name, region), found)

    def show_candidate(self, row: int) -> None:
        self.feature_band.reset(Qgis.GeometryType.Polygon)
        if 0 <= row < len(self.candidates):
            c = self.candidates[row]
            if lw.is_areal(c.geometry):
                self.feature_band.setToGeometry(qgs_geometry(c.geometry), WGS84)
            else:
                self.place_outlet(c.lat, c.lon)

    def use_feature(self) -> None:
        row = self.results.currentRow()
        if not 0 <= row < len(self.candidates) or self.task is not None or self.project is None:
            return
        feature = self.candidates[row]
        delineation = self.delineation.currentData()
        if delineation == "regional_dem" and not lw.grass_available():
            QMessageBox.warning(
                self, "Focus area", "GRASS (qgis_process) was not found; choose Global Watersheds or the feature outline."
            )
            return
        project = self.project
        if not self.focus_name():
            self.name.setEditText(feature.label)

        def done(outline: lw.FocusOutline) -> None:
            self.set_pending(
                outline.geometry,
                "feature" if delineation == "none" else "feature_watershed",
                outline.source,
                feature_name=feature.label,
                delineation=delineation,
                outlet=(outline.outlet_lat, outline.outlet_lon) if outline.outlet_lat is not None else None,
                citation=outline.citation,
                feature_geometry=feature.geometry,
            )

        self.run_task(
            f"Outlining {feature.label}",
            lambda _task: lw.outline_from_feature(project, feature, delineation, self.messages.append),
            done,
        )

    def accept_extent(self) -> None:
        if self.project is None or self.pending is None or not self.focus_name():
            return
        existing = self.project.get_focus(self.focus_name())
        if existing is not None and existing.status == "built":
            answer = QMessageBox.question(
                self, "Focus area", f"'{existing.name}' is already built. Replace its outline (the DEM must be rebuilt)?"
            )
            if answer != QMessageBox.StandardButton.Yes:
                return
        try:
            summary = preview_summary(self.project, self.trial_focus(), self.pending["geometry"])
        except (RuntimeError, ValueError) as exc:
            QMessageBox.warning(self, "Focus area", f"Outline problem: {exc}")
            return
        if not confirm_grid_size(self, "Focus area", summary["level"], summary["warning"]):
            return
        focus = define_focus(self.project, self.trial_focus(), self.pending["geometry"], self.pending["citation"])
        if self.name.findText(focus.name) < 0:
            self.name.blockSignals(True)
            self.name.addItem(focus.name)
            self.name.setEditText(focus.name)
            self.name.blockSignals(False)
        self.saved = True
        self.append(f"Focus area '{focus.name}' saved: {focus.boundary_file}")
        self.show_preview()

    def download(self) -> None:
        focus = self.project.get_focus(self.focus_name()) if self.project else None
        if focus is None or not self.saved:
            return
        summary = preview_summary(self.project, focus, self.pending["geometry"]) if self.pending else None
        if summary and not confirm_grid_size(self, "Focus area", summary["level"], summary["warning"]):
            return
        self.project.options.lake_bathymetry = self.lakes.isChecked()
        api_keys = {"opentopography": QgsSettings().value(f"{SETTINGS}/opentopography_api_key", "", type=str)}
        self._cancel_logged = False
        release_focus_layers(QgsProject.instance(), self.project, focus)
        self.build_task = FocusTask(self.project, focus, api_keys, self.refresh.isChecked())
        self.build_task.message.connect(self.append)
        self.build_task.progressChanged.connect(lambda pct: self.progress.setValue(int(pct)))
        self.build_task.completed.connect(self.on_built)
        self.progress.setValue(0)
        self.update_buttons()
        QgsApplication.taskManager().addTask(self.build_task)

    def on_built(self, ok: bool) -> None:
        task, self.build_task = self.build_task, None
        self.update_buttons()
        if task is None:
            return
        if not ok or task.result is None:
            self.append(task.error or "Focus-area build failed.")
            return
        populate_focus(QgsProject.instance(), task.project, task.focus)
        try:
            self.append(refresh_qgz(task.project))
        except RuntimeError as exc:
            self.append(str(exc))
        bind_open_qgz(task.project)
        self.report_path = task.result.report_path
        self.append(f"Report: {self.report_path}")
        self.show_preview()

    def cancel(self) -> None:
        tasks = [t for t in (self.task, self.build_task) if t is not None]
        if not tasks:
            return
        if not getattr(self, "_cancel_logged", False):
            self.append("Cancel requested — stopping after the current GDAL/download step…")
            self._cancel_logged = True
        for task in tasks:
            task.cancel()

    def open_report(self) -> None:
        if self.report_path is not None and self.report_path.exists():
            QDesktopServices.openUrl(QUrl.fromLocalFile(str(self.report_path)))

    def cleanup(self) -> None:
        self._restore_tool()
        for band in (self.region_band, self.feature_band, self.outline_band, self.grid_band):
            band.reset(Qgis.GeometryType.Polygon)
        self.outlet_marker.hide()
        if self.extent_tool is not None:
            self.extent_tool.clearRubberBand()
        if self.task is not None:
            self.task.cancel()
