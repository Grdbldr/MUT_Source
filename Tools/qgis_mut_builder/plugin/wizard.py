"""Six-page wizard: project, location, model boundary, existing data, sources and options, run."""

from __future__ import annotations

import json
import os
from pathlib import Path

from qgis.core import (
    Qgis,
    QgsApplication,
    QgsCoordinateReferenceSystem,
    QgsCoordinateTransform,
    QgsGeometry,
    QgsProject,
    QgsRasterLayer,
    QgsRectangle,
    QgsSettings,
    QgsTask,
)
from qgis.gui import (
    QgsFileWidget,
    QgsMapToolEmitPoint,
    QgsMapToolExtent,
    QgsProjectionSelectionWidget,
    QgsRubberBand,
)
from qgis.PyQt.QtCore import Qt, QUrl
from qgis.PyQt.QtGui import QColor, QDesktopServices
from qgis.PyQt.QtWidgets import (
    QButtonGroup,
    QCheckBox,
    QComboBox,
    QDoubleSpinBox,
    QFileDialog,
    QFormLayout,
    QGroupBox,
    QHBoxLayout,
    QHeaderView,
    QLabel,
    QLineEdit,
    QListWidget,
    QMessageBox,
    QPlainTextEdit,
    QProgressBar,
    QPushButton,
    QRadioButton,
    QStackedWidget,
    QTableWidget,
    QTableWidgetItem,
    QVBoxLayout,
    QWidget,
    QWizard,
    QWizardPage,
)

from ..core import boundary as bnd
from ..core.geo import LARGE_GRID_CELLS, MAX_REGIONAL_RES_M, grid_advice, utm_epsg
from ..core.inventory import scan_folder
from ..core.project import ROLES, DataFile, Options, Project
from ..core.providers import KINDS, PROVIDERS
from ..core.qgis_project import OSM_URI, bind_open_qgz, populate, refresh_qgz, release_surfaces_layers
from .task import PipelineTask

SETTINGS = "mut_model_builder"
KIND_TITLES = {
    "coarse": "Regional ground surface (~30 m)",
    "fine": "Focus-area ground surface (~1 m lidar, used by 'New focus area...')",
    "bedrock": "Bedrock surface",
}


def _spin(lo: float, hi: float, value: float, decimals: int = 1, suffix: str = "", step: float = 1.0) -> QDoubleSpinBox:
    box = QDoubleSpinBox()
    box.setRange(lo, hi)
    box.setDecimals(decimals)
    box.setSingleStep(step)
    box.setValue(value)
    if suffix:
        box.setSuffix(suffix)
    return box


def regional_size(project: Project) -> tuple[str, str]:
    """``grid_advice`` level and message for the project's regional grid."""
    return grid_advice(project.regional_grid(), LARGE_GRID_CELLS, 1.0, MAX_REGIONAL_RES_M, "regional")


def confirm_grid_size(parent, title: str, level: str, text: str) -> bool:
    """Refuse (with a message) above the hard cell limit; above the soft limit ask the user to confirm."""
    if level == "too_large":
        QMessageBox.warning(parent, title, f"{text}\n\nChoose a coarser cell size or a smaller area; nothing was downloaded.")
        return False
    if level == "large":
        answer = QMessageBox.question(parent, title, f"{text}\n\nContinue with this grid?")
        return answer == QMessageBox.StandardButton.Yes
    return True


class ProjectPage(QWizardPage):
    def __init__(self, wizard: "ModelBuilderWizard") -> None:
        super().__init__()
        self.wiz = wizard
        self.setTitle("Project")
        self.setSubTitle("Name the model, describe the hydrogeological system, and choose where outputs are written.")
        self.name = QLineEdit()
        self.description = QPlainTextEdit()
        self.description.setPlaceholderText(
            "e.g. Unconfined sand and gravel aquifer over fractured granite; assess drawdown from a proposed well field."
        )
        self.output = QgsFileWidget()
        self.output.setStorageMode(QgsFileWidget.StorageMode.GetDirectory)
        load = QPushButton("Open existing project.yaml...")
        load.clicked.connect(self.wiz.load_yaml)

        form = QFormLayout(self)
        form.addRow("Project name", self.name)
        form.addRow("Description", self.description)
        form.addRow("Output folder", self.output)
        form.addRow("", load)
        self.registerField("name*", self.name)

    def validatePage(self) -> bool:
        folder = self.output.filePath().strip()
        if not folder:
            QMessageBox.warning(self, "Output folder", "Choose an output folder.")
            return False
        Path(folder).mkdir(parents=True, exist_ok=True)
        return True


class SitePage(QWizardPage):
    def __init__(self, wizard: "ModelBuilderWizard") -> None:
        super().__init__()
        self.wiz = wizard
        self.setTitle("Location")
        self.setSubTitle(
            "Click 'Pick on map' and then the project location on the QGIS map, or type the coordinates. "
            "Blue outline: regional model area (~30 m DEM). Detailed ~1 m surfaces are added later with "
            "'New focus area...'."
        )
        self.lat = _spin(-90, 90, 0.0, 6, " deg", 0.01)
        self.lon = _spin(-180, 180, 0.0, 6, " deg", 0.01)
        pick = QPushButton("Pick on map")
        pick.clicked.connect(self.wiz.start_pick)
        osm = QPushButton("Add OpenStreetMap basemap")
        osm.clicked.connect(self.add_basemap)
        zoom = QPushButton("Zoom to regional area")
        zoom.clicked.connect(self.wiz.zoom_to_region)
        self.regional = _spin(1, 500, 20.0, 1, " km")
        self.auto_crs = QCheckBox("Use the UTM zone of the project location")
        self.auto_crs.setChecked(True)
        self.crs = QgsProjectionSelectionWidget()
        self.crs.setCrs(QgsCoordinateReferenceSystem("EPSG:32631"))

        buttons = QHBoxLayout()
        for button in (pick, osm, zoom):
            buttons.addWidget(button)
        form = QFormLayout(self)
        form.addRow("Latitude", self.lat)
        form.addRow("Longitude", self.lon)
        form.addRow("", buttons)
        form.addRow("Regional half-width", self.regional)
        form.addRow("", self.auto_crs)
        form.addRow("Model projection", self.crs)

        for box in (self.lat, self.lon, self.regional):
            box.valueChanged.connect(self.changed)
        self.auto_crs.toggled.connect(self.changed)
        self.crs.crsChanged.connect(self.wiz.update_rubber_bands)

    def changed(self) -> None:
        self.crs.setEnabled(not self.auto_crs.isChecked())
        if self.auto_crs.isChecked():
            self.crs.setCrs(QgsCoordinateReferenceSystem(f"EPSG:{utm_epsg(self.lat.value(), self.lon.value())}"))
        self.wiz.update_rubber_bands()

    def epsg(self) -> int:
        authid = self.crs.crs().authid()
        if not authid.upper().startswith("EPSG:"):
            raise ValueError(f"Choose an EPSG projected CRS (got '{authid}')")
        return int(authid.split(":", 1)[1])

    def add_basemap(self) -> None:
        layer = QgsRasterLayer(OSM_URI, "OpenStreetMap", "wms")
        if layer.isValid():
            QgsProject.instance().addMapLayer(layer)

    def initializePage(self) -> None:
        self.changed()

    def validatePage(self) -> bool:
        if self.lat.value() == 0.0 and self.lon.value() == 0.0:
            QMessageBox.warning(self, "Location", "Pick the project location on the map or enter its latitude and longitude.")
            return False
        crs = self.crs.crs()
        if crs.isGeographic() or crs.mapUnits() != Qgis.DistanceUnit.Meters:
            QMessageBox.warning(self, "Projection", "Choose a projected CRS in metres (e.g. a UTM zone).")
            return False
        try:
            self.epsg()
        except ValueError as exc:
            QMessageBox.warning(self, "Projection", str(exc))
            return False
        return True


class BoundaryPage(QWizardPage):
    MODE_LABELS = {
        "square": "Square around the project location (regional half-width on the Location page)",
        "rectangle": "Rectangle drawn on the QGIS map",
        "watershed": "Watershed found online by name, or the watershed draining to the project location",
        "file": "Polygon(s) from a vector file",
    }

    def __init__(self, wizard: "ModelBuilderWizard") -> None:
        super().__init__()
        self.wiz = wizard
        self.setTitle("Model boundary")
        self.setSubTitle(
            "The regional grid covers the boundary extent plus the margin; rasters stay rectangular and the boundary "
            "is saved as 1_data/boundary/model_boundary.geojson. Green outline: model boundary."
        )
        self.group = QButtonGroup(self)
        layout = QVBoxLayout(self)
        for i, (mode, text) in enumerate(self.MODE_LABELS.items()):
            radio = QRadioButton(text)
            radio.setProperty("mode", mode)
            self.group.addButton(radio, i)
            layout.addWidget(radio)
        self.group.button(0).setChecked(True)

        self.stack = QStackedWidget()
        self.stack.addWidget(QWidget())

        rect_panel = QWidget()
        rect_row = QHBoxLayout(rect_panel)
        draw = QPushButton("Draw rectangle on map")
        draw.clicked.connect(self.wiz.start_extent)
        rect_row.addWidget(draw)
        rect_row.addWidget(QLabel("Click and drag on the QGIS map."))
        rect_row.addStretch()
        self.stack.addWidget(rect_panel)

        ws_panel = QWidget()
        ws = QVBoxLayout(ws_panel)
        row = QHBoxLayout()
        self.ws_name = QLineEdit()
        self.ws_name.setPlaceholderText("Watershed, basin or river name, e.g. Boulder Creek")
        self.ws_name.returnPressed.connect(self.search)
        self.search_button = QPushButton("Search")
        self.search_button.clicked.connect(self.search)
        self.at_site_button = QPushButton("Watershed draining to the location")
        self.at_site_button.clicked.connect(self.at_site)
        for w in (self.ws_name, self.search_button, self.at_site_button):
            row.addWidget(w)
        ws.addLayout(row)
        self.results = QListWidget()
        self.results.itemDoubleClicked.connect(lambda _item: self.use_selected())
        ws.addWidget(self.results)
        row = QHBoxLayout()
        self.use_button = QPushButton("Use selected")
        self.use_button.clicked.connect(self.use_selected)
        self.cancel_button = QPushButton("Cancel")
        self.cancel_button.clicked.connect(self.cancel_task)
        self.busy = QProgressBar()
        self.busy.setRange(0, 0)
        self.busy.setVisible(False)
        for w in (self.use_button, self.cancel_button, self.busy):
            row.addWidget(w)
        ws.addLayout(row)
        ws.addWidget(
            QLabel(
                "Sources: USGS Watershed Boundary Dataset (US), OpenStreetMap rivers and basins, and Global Watersheds "
                "(MERIT-Hydro) delineation. A river is delineated at several outlets; pick the outline that fits."
            )
        )
        self.stack.addWidget(ws_panel)

        file_panel = QWidget()
        file_row = QHBoxLayout(file_panel)
        self.file = QgsFileWidget()
        self.file.setFilter("Vector files (*.shp *.gpkg *.geojson *.json *.kml *.gml);;All files (*)")
        self.file.fileChanged.connect(self.use_file)
        file_row.addWidget(self.file)
        self.stack.addWidget(file_panel)
        layout.addWidget(self.stack)

        self.margin = _spin(0, 100, 1.0, 2, " km", 0.5)
        self.margin.valueChanged.connect(self.wiz.update_rubber_bands)
        form = QFormLayout()
        form.addRow("Margin around boundary", self.margin)
        layout.addLayout(form)
        self.status = QLabel("")
        self.status.setWordWrap(True)
        layout.addWidget(self.status)

        self.group.idToggled.connect(self.mode_changed)
        self.candidates: list[bnd.BoundaryCandidate] = []
        self.task: QgsTask | None = None
        self.messages: list[str] = []
        self.applied_mode = ""
        self.boundary_file = ""
        self.boundary_name = ""
        self.boundary_source = ""
        self.set_busy(False)

    def mode(self) -> str:
        return self.group.checkedButton().property("mode")

    def set_mode(self, mode: str) -> None:
        self.group.button(list(self.MODE_LABELS).index(mode)).setChecked(True)

    def mode_changed(self, _id: int, checked: bool) -> None:
        if not checked:
            return
        self.stack.setCurrentIndex(self.group.checkedId())
        self.margin.setEnabled(self.mode() != "square")
        self.show_status()
        self.wiz.update_rubber_bands()

    def boundary_fields(self) -> dict:
        mode = self.mode()
        if mode == "square" or mode != self.applied_mode:
            return {"boundary_mode": mode, "boundary_margin_km": self.margin.value()}
        return {
            "boundary_mode": mode,
            "boundary_file": self.boundary_file,
            "boundary_name": self.boundary_name,
            "boundary_source": self.boundary_source,
            "boundary_margin_km": self.margin.value(),
        }

    def has_geometry(self) -> bool:
        return self.mode() == self.applied_mode and bool(self.boundary_file) and Path(self.boundary_file).exists()

    def show_status(self) -> None:
        if self.mode() == "square":
            self.status.setText("")
        elif not self.has_geometry():
            self.status.setText("No boundary chosen yet.")
        else:
            try:
                area = bnd.boundary_area_km2(Path(self.boundary_file), self.wiz.site_page.epsg())
                area_text = f"{area:,.1f} km2"
            except (RuntimeError, ValueError):
                area_text = "area unknown"
            self.status.setText(f"<b>Boundary:</b> {self.boundary_name} ({area_text})<br>Source: {self.boundary_source}")

    def apply(self, geometry: dict, mode: str, name: str, source: str, citation: str = "") -> None:
        project = self.wiz.build_project(include_data=False, boundary=False)
        bnd.apply_geometry(project, geometry, mode, name, source, citation)
        self.record(project)

    def apply_candidate(self, candidate: bnd.BoundaryCandidate) -> None:
        project = self.wiz.build_project(include_data=False, boundary=False)
        bnd.apply_candidate(project, candidate)
        self.record(project)

    def record(self, project: Project) -> None:
        self.applied_mode = project.boundary_mode
        self.boundary_file = project.boundary_file
        self.boundary_name = project.boundary_name
        self.boundary_source = project.boundary_source
        self.show_status()
        self.wiz.update_rubber_bands()
        self.wiz.zoom_to_region()

    def use_rectangle(self, rect: QgsRectangle, crs: QgsCoordinateReferenceSystem) -> None:
        geom = QgsGeometry.fromRect(rect).densifyByCount(20)
        geom.transform(QgsCoordinateTransform(crs, QgsCoordinateReferenceSystem("EPSG:4326"), QgsProject.instance()))
        self.apply(json.loads(geom.asJson(7)), "rectangle", "Rectangle drawn on map", "QGIS map")

    def use_file(self, path: str) -> None:
        if not path or not Path(path).is_file():
            return
        try:
            geometry = bnd.file_geometry(Path(path))
        except RuntimeError as exc:
            QMessageBox.warning(self, "Boundary file", str(exc))
            return
        self.apply(geometry, "file", Path(path).stem, path)

    def set_busy(self, busy: bool) -> None:
        for w in (self.search_button, self.at_site_button, self.use_button, self.ws_name):
            w.setEnabled(not busy)
        self.cancel_button.setEnabled(busy)
        self.busy.setVisible(busy)

    def run_task(self, title: str, function, done) -> None:
        """Run ``function(task)`` in the background; ``done(result)`` runs on the main thread."""
        self.messages = []

        def finished(exception, result=None) -> None:
            self.task = None
            self.set_busy(False)
            if exception is not None:
                self.status.setText(f"{title} failed: {exception}<br><small>{'<br>'.join(self.messages)}</small>")
                return
            if result is not None:
                done(result)

        self.task = QgsTask.fromFunction(title, function, on_finished=finished, flags=QgsTask.Flag.CanCancel)
        self.set_busy(True)
        self.status.setText(f"{title} ...")
        QgsApplication.taskManager().addTask(self.task)

    def cancel_task(self) -> None:
        if self.task is not None:
            self.task.cancel()

    def near(self) -> tuple[float, float] | None:
        lat, lon = self.wiz.site_page.lat.value(), self.wiz.site_page.lon.value()
        return None if lat == 0.0 and lon == 0.0 else (lat, lon)

    def list_candidates(self, candidates: list[bnd.BoundaryCandidate], note: str) -> None:
        self.candidates = candidates
        self.results.clear()
        for c in candidates:
            self.results.addItem(("[river] " if c.kind == "river" else "") + c.display)
        if candidates:
            self.results.setCurrentRow(0)
        log = "<br>".join(self.messages)
        self.status.setText(note + (f"<br><small>{log}</small>" if log else ""))

    def search(self) -> None:
        name = self.ws_name.text().strip()
        if not name or self.task is not None:
            return
        near = self.near()
        self.run_task(
            f"Searching for '{name}'",
            lambda _task: bnd.search_watersheds(name, near, log=self.messages.append),
            lambda found: self.list_candidates(
                found, f"{len(found)} candidate(s). Select one and click 'Use selected'; rivers are delineated at several outlets."
            ),
        )

    def at_site(self) -> None:
        near = self.near()
        if near is None or self.task is not None:
            return
        self.run_task(
            "Delineating the watershed draining to the project location",
            lambda _task: [bnd.delineate(*near)],
            lambda found: self.list_candidates(found, "Watershed draining to the location; click 'Use selected' to apply."),
        )

    def use_selected(self) -> None:
        row = self.results.currentRow()
        if row < 0 or row >= len(self.candidates) or self.task is not None:
            return
        candidate = self.candidates[row]
        if candidate.kind == "river":
            near = self.near()
            self.run_task(
                f"Delineating outlets along {candidate.ref.get('name', candidate.label)}",
                lambda task: bnd.river_outlets(candidate, near, log=self.messages.append, is_canceled=task.isCanceled),
                lambda found: self.list_candidates(
                    found, f"{len(found)} watershed(s) along the river, largest first. Select one and click 'Use selected'."
                ),
            )
            return
        self.run_task(
            f"Fetching {candidate.label}",
            lambda _task: bnd.resolve_geometry(candidate),
            lambda _geometry: self.apply_candidate(candidate),
        )

    def validatePage(self) -> bool:
        if self.task is not None:
            QMessageBox.warning(self, "Model boundary", "Wait for the search to finish or cancel it.")
            return False
        if self.mode() == "square":
            return True
        if not self.has_geometry():
            QMessageBox.warning(self, "Model boundary", "Choose a boundary for the selected option, or use the square.")
            return False
        lat, lon = self.wiz.site_page.lat.value(), self.wiz.site_page.lon.value()
        if not bnd.contains_point(Path(self.boundary_file), lat, lon):
            answer = QMessageBox.question(
                self, "Model boundary", "The project location lies outside the model boundary. Continue anyway?"
            )
            return answer == QMessageBox.StandardButton.Yes
        return True


class DataPage(QWizardPage):
    def __init__(self, wizard: "ModelBuilderWizard") -> None:
        super().__init__()
        self.wiz = wizard
        self.setTitle("Existing data")
        self.setSubTitle(
            "Optional. Choose the folder holding existing project data. Rasters and vector files are listed with a "
            "suggested role; local files take priority over downloads. Set unwanted files to 'ignore'."
        )
        self.folder = QgsFileWidget()
        self.folder.setStorageMode(QgsFileWidget.StorageMode.GetDirectory)
        scan = QPushButton("Scan folder")
        scan.clicked.connect(self.scan)
        self.table = QTableWidget(0, 3)
        self.table.setHorizontalHeaderLabels(["Role", "File", "Details"])
        header = self.table.horizontalHeader()
        header.setSectionResizeMode(0, QHeaderView.ResizeMode.ResizeToContents)
        header.setSectionResizeMode(1, QHeaderView.ResizeMode.Interactive)
        header.setSectionResizeMode(2, QHeaderView.ResizeMode.Stretch)
        self.status = QLabel("")
        self.scanned_folder = ""

        row = QHBoxLayout()
        row.addWidget(self.folder)
        row.addWidget(scan)
        layout = QVBoxLayout(self)
        layout.addLayout(row)
        layout.addWidget(self.table)
        layout.addWidget(self.status)

    def initializePage(self) -> None:
        folder = self.folder.filePath().strip()
        if folder and folder != self.scanned_folder and self.table.rowCount() == 0:
            self.scan()

    def scan(self) -> None:
        folder = self.folder.filePath().strip()
        if not folder or not Path(folder).is_dir():
            self.status.setText("Choose an existing folder first.")
            return
        QgsApplication.setOverrideCursor(Qt.CursorShape.WaitCursor)
        try:
            project = self.wiz.build_project(include_data=False)
            items = scan_folder(Path(folder), centre_lat=project.lat, region=project.regional_grid().geo_bbox())
        finally:
            QgsApplication.restoreOverrideCursor()
        self.set_rows([(str(i.path), i.suggested_role, i.summary) for i in items])
        self.scanned_folder = folder
        used = sum(1 for i in items if i.suggested_role != "ignore")
        self.status.setText(f"{len(items)} file(s) found; {used} assigned a role.")

    def set_rows(self, rows: list[tuple[str, str, str]]) -> None:
        self.table.setRowCount(0)
        for path, role, details in rows:
            r = self.table.rowCount()
            self.table.insertRow(r)
            combo = QComboBox()
            combo.addItems(ROLES)
            combo.setCurrentText(role)
            self.table.setCellWidget(r, 0, combo)
            item = QTableWidgetItem(path)
            item.setFlags(item.flags() & ~Qt.ItemFlag.ItemIsEditable)
            self.table.setItem(r, 1, item)
            info = QTableWidgetItem(details)
            info.setFlags(info.flags() & ~Qt.ItemFlag.ItemIsEditable)
            self.table.setItem(r, 2, info)
        self.table.resizeColumnToContents(1)

    def data_files(self) -> list[DataFile]:
        files = []
        for r in range(self.table.rowCount()):
            role = self.table.cellWidget(r, 0).currentText()
            files.append(DataFile(self.table.item(r, 1).text(), role))
        return files


class SourcesPage(QWizardPage):
    def __init__(self, wizard: "ModelBuilderWizard") -> None:
        super().__init__()
        self.wiz = wizard
        self.setTitle("Sources and options")
        self.setSubTitle(
            "Download sources are tried in the order listed, only where local files leave gaps. "
            "Sources that do not cover the regional area are disabled."
        )
        self.checks: dict[str, QCheckBox] = {}
        layout = QVBoxLayout(self)
        for kind in KINDS:
            group = QGroupBox(KIND_TITLES[kind])
            box = QVBoxLayout(group)
            for provider in [p for p in PROVIDERS if p.kind == kind]:
                check = QCheckBox(provider.name)
                tip = [f"Vertical accuracy: {provider.vertical_accuracy or 'n/a'}", f"Datum: {provider.vertical_datum or 'n/a'}"]
                if provider.licence:
                    tip.append(f"Licence: {provider.licence}")
                check.setToolTip("\n".join(tip))
                self.checks[provider.id] = check
                box.addWidget(check)
            layout.addWidget(group)

        self.api_key = QLineEdit(QgsSettings().value(f"{SETTINGS}/opentopography_api_key", "", type=str))
        self.api_key.setEchoMode(QLineEdit.EchoMode.Password)
        self.api_key.setPlaceholderText("free key from portal.opentopography.org (optional)")
        self.api_key.textChanged.connect(self.initializePage)

        opts = Options()
        self.coarse_res = _spin(5, MAX_REGIONAL_RES_M, opts.coarse_resolution_m, 1, " m")
        self.fine_res = _spin(0.25, 30, opts.fine_resolution_m, 2, " m", 0.5)
        self.min_thick = _spin(0, 100, opts.min_thickness_m, 2, " m", 0.5)
        self.lakes = QCheckBox("Correct lakes with bathymetry (measured surveys where available, otherwise estimated)")
        self.lakes.setChecked(opts.lake_bathymetry)
        self.lakes.setToolTip(
            "Lidar DEMs show lakes as a flat water surface. When checked, lakes, ponds and reservoirs from OpenStreetMap "
            "are lowered to a lake bottom: local bathymetry files first, then measured surveys (Ontario MNRF), then an "
            "estimate from lake shape and surrounding slope. The report lists every lake and how its depth was found."
        )
        self.lake_area = _spin(0.1, 10000, opts.lake_min_area_ha, 1, " ha", 1.0)
        self.lakes.toggled.connect(self.lake_area.setEnabled)
        self.climate = QCheckBox("Retrieve precipitation, temperature and outlet river flows from the internet")
        self.climate.setChecked(opts.climate_retrieval)
        self.climate.setToolTip(
            "Downloads domain-mean daily precipitation and temperature (Daymet in North America, otherwise "
            "Open-Meteo ERA5) and PET, plus discharge near level-1 outlets (ECCC then USGS). Writes "
            "1_data/climate and 1_data/flows for the report and for Build MUT project climate forcing."
        )
        self.refresh = QCheckBox("Ignore cached downloads (fetch again)")

        form = QFormLayout()
        form.addRow("OpenTopography API key", self.api_key)
        form.addRow("Regional cell size", self.coarse_res)
        form.addRow("Focus-area cell size (default)", self.fine_res)
        form.addRow("Minimum surficial thickness", self.min_thick)
        form.addRow("", self.lakes)
        form.addRow("Smallest lake corrected", self.lake_area)
        form.addRow("", self.climate)
        form.addRow("", self.refresh)
        layout.addLayout(form)
        self._selected: set[str] | None = None

    def initializePage(self) -> None:
        try:
            project = self.wiz.build_project(include_data=False)
        except ValueError:
            return
        bbox = project.regional_grid().geo_bbox()
        keys = self.api_keys()
        for provider in PROVIDERS:
            check = self.checks[provider.id]
            covers = provider.covers(bbox)
            needs_key = provider.missing_key(keys)
            check.setEnabled(covers and not needs_key)
            label = provider.name
            if not covers:
                label += "  (does not cover this area)"
            elif needs_key:
                label += "  (needs an API key)"
            check.setText(label)
            wanted = self._selected is None or provider.id in self._selected
            check.setChecked(covers and not needs_key and wanted)

    def set_selected(self, ids: list[str]) -> None:
        self._selected = set(ids) if ids else None

    def api_keys(self) -> dict:
        return {"opentopography": self.api_key.text().strip()}

    def selected(self) -> list[str]:
        return [pid for pid, check in self.checks.items() if check.isChecked() and check.isEnabled()]

    def options(self) -> Options:
        return Options(
            coarse_resolution_m=self.coarse_res.value(),
            fine_resolution_m=self.fine_res.value(),
            min_thickness_m=self.min_thick.value(),
            lake_bathymetry=self.lakes.isChecked(),
            lake_min_area_ha=self.lake_area.value(),
            climate_retrieval=self.climate.isChecked(),
        )

    def validatePage(self) -> bool:
        QgsSettings().setValue(f"{SETTINGS}/opentopography_api_key", self.api_key.text().strip())
        self._selected = set(self.selected())
        try:
            level, text = regional_size(self.wiz.build_project(include_data=False))
        except ValueError:
            return True
        return level != "too_large" or confirm_grid_size(self, "Regional grid", level, text)


class RunPage(QWizardPage):
    def __init__(self, wizard: "ModelBuilderWizard") -> None:
        super().__init__()
        self.wiz = wizard
        self.setTitle("Run")
        self.setSubTitle("Save the project file and build the surfaces. Downloads run in the background.")
        self.summary = QLabel()
        self.summary.setWordWrap(True)
        self.run_button = QPushButton("Run")
        self.cancel_button = QPushButton("Cancel run")
        self.report_button = QPushButton("Open report")
        self.cancel_button.setEnabled(False)
        self.report_button.setEnabled(False)
        self.run_button.clicked.connect(self.start)
        self.cancel_button.clicked.connect(self.cancel)
        self.report_button.clicked.connect(self.open_report)
        self.progress = QProgressBar()
        self.log = QPlainTextEdit()
        self.log.setReadOnly(True)
        self.task: PipelineTask | None = None
        self.done_ok = False

        row = QHBoxLayout()
        for button in (self.run_button, self.cancel_button, self.report_button):
            row.addWidget(button)
        layout = QVBoxLayout(self)
        layout.addWidget(self.summary)
        layout.addLayout(row)
        layout.addWidget(self.progress)
        layout.addWidget(self.log)

    def initializePage(self) -> None:
        project = self.wiz.build_project()
        reg = project.regional_grid()
        used = [f for f in project.data_files if f.role != "ignore"]
        if project.has_boundary:
            boundary = f"Boundary: {project.boundary_name} ({project.boundary_mode}, {project.boundary_margin_km:g} km margin).<br>"
        else:
            boundary = f"Boundary: square, {project.regional_buffer_km:g} km half-width.<br>"
        level, size_text = regional_size(project)
        if level == "too_large":
            size_text = f"<span style='color:#b00'><b>Too large to run:</b> {size_text}</span><br>"
        elif level:
            size_text = f"<span style='color:#b00'>{size_text} You will be asked to confirm.</span><br>"
        self.summary.setText(
            f"<b>{project.name}</b> at {project.lat:.5f}, {project.lon:.5f} in EPSG:{project.epsg}.<br>"
            + boundary
            + f"Regional grid {reg.width} x {reg.height} at {reg.res:g} m ({reg.cells / 1e6:,.1f} million cells). "
            "Add ~1 m focus areas afterwards with 'New focus area...'.<br>"
            + size_text
            + f"{len(used)} local file(s) in use; {len(project.providers)} download source(s) enabled.<br>"
            + (
                f"Lake bathymetry: on (lakes of {project.options.lake_min_area_ha:g} ha or more).<br>"
                if project.options.lake_bathymetry
                else "Lake bathymetry: off (lakes keep the DEM water surface).<br>"
            )
            + (
                "Climate and outlet flows: retrieve from the internet.<br>"
                if project.options.climate_retrieval
                else "Climate and outlet flows: off.<br>"
            )
            + f"Outputs: {project.surfaces_dir}"
        )

    def isComplete(self) -> bool:
        return self.done_ok

    def append(self, text: str) -> None:
        self.log.appendPlainText(text)

    def start(self) -> None:
        try:
            project = self.wiz.build_project()
        except ValueError as exc:
            QMessageBox.warning(self, "Project", str(exc))
            return
        if not confirm_grid_size(self, "Regional grid", *regional_size(project)):
            return
        path = project.save()
        self.log.clear()
        self.append(f"Saved {path}")
        self._cancel_logged = False
        release_surfaces_layers(QgsProject.instance(), project)
        self.task = PipelineTask(project, self.wiz.sources_page.api_keys(), self.wiz.sources_page.refresh.isChecked())
        self.task.message.connect(self.append)
        self.task.progressChanged.connect(lambda pct: self.progress.setValue(int(pct)))
        self.task.completed.connect(self.on_completed)
        self.run_button.setEnabled(False)
        self.cancel_button.setEnabled(True)
        self.report_button.setEnabled(False)
        QgsApplication.taskManager().addTask(self.task)

    def cancel(self) -> None:
        if self.task is None:
            return
        if not getattr(self, "_cancel_logged", False):
            self.append("Cancel requested — stopping after the current GDAL/download step…")
            self._cancel_logged = True
        self.task.cancel()

    def on_completed(self, ok: bool) -> None:
        task, self.task = self.task, None
        self.run_button.setEnabled(True)
        self.cancel_button.setEnabled(False)
        if task is None:
            return
        if not ok or task.result is None:
            self.append(task.error or "Run failed.")
            return
        result = task.result
        project = task.project
        qproject = QgsProject.instance()
        if not qproject.mapLayers():
            qproject.setCrs(QgsCoordinateReferenceSystem(f"EPSG:{project.epsg}"))
        populate(qproject, project, result, add_basemap=False)
        try:
            self.append(refresh_qgz(project, result))
        except RuntimeError as exc:
            self.append(str(exc))
        bind_open_qgz(project)
        self.wiz.zoom_to_region()
        self.report_path = result.report_path
        self.report_button.setEnabled(result.report_path is not None)
        self.append(f"Report: {result.report_path}")
        self.done_ok = True
        self.completeChanged.emit()

    def open_report(self) -> None:
        if getattr(self, "report_path", None):
            QDesktopServices.openUrl(QUrl.fromLocalFile(str(self.report_path)))


class ModelBuilderWizard(QWizard):
    def __init__(self, iface) -> None:
        super().__init__(iface.mainWindow())
        self.iface = iface
        self.canvas = iface.mapCanvas()
        self.setWindowTitle("MUT Model Builder")
        self.setWizardStyle(QWizard.WizardStyle.ModernStyle)
        self.setOption(QWizard.WizardOption.NoCancelButtonOnLastPage, False)
        self.resize(820, 640)

        self.project_page = ProjectPage(self)
        self.site_page = SitePage(self)
        self.boundary_page = BoundaryPage(self)
        self.data_page = DataPage(self)
        self.sources_page = SourcesPage(self)
        self.run_page = RunPage(self)
        pages = (self.project_page, self.site_page, self.boundary_page, self.data_page, self.sources_page, self.run_page)
        for page in pages:
            self.addPage(page)

        self.region_band = self._band(QColor(30, 90, 200))
        self.boundary_band = self._band(QColor(26, 150, 65))
        self.boundary_band.setWidth(3)
        self.pick_tool: QgsMapToolEmitPoint | None = None
        self.extent_tool: QgsMapToolExtent | None = None
        self.previous_tool = None
        self.loaded: Project | None = None
        self.finished.connect(lambda _result: self.cleanup())

    def _band(self, colour: QColor) -> QgsRubberBand:
        band = QgsRubberBand(self.canvas, Qgis.GeometryType.Polygon)
        band.setStrokeColor(colour)
        band.setFillColor(QColor(0, 0, 0, 0))
        band.setWidth(2)
        return band

    def build_project(self, include_data: bool = True, boundary: bool = True) -> Project:
        site = self.site_page
        out = self.project_page.output.filePath().strip()
        extra = {"created": self.loaded.created, "focus_areas": list(self.loaded.focus_areas)} if self.loaded else {}
        if boundary:
            extra.update(self.boundary_page.boundary_fields())
        return Project(
            **extra,
            name=self.project_page.name.text().strip() or "project",
            description=self.project_page.description.toPlainText().strip(),
            lat=site.lat.value(),
            lon=site.lon.value(),
            output_folder=out or os.getcwd(),
            data_folder=self.data_page.folder.filePath().strip(),
            regional_buffer_km=site.regional.value(),
            epsg=site.epsg(),
            providers=self.sources_page.selected() if include_data else [],
            data_files=self.data_page.data_files() if include_data else [],
            options=self.sources_page.options(),
        )

    def load_yaml(self) -> None:
        path, _ = QFileDialog.getOpenFileName(self, "Open project.yaml", "", "Project files (*.yaml *.yml)")
        if not path:
            return
        try:
            project = Project.load(Path(path))
        except (OSError, ValueError, TypeError) as exc:
            QMessageBox.warning(self, "Open project", f"Could not read {path}:\n{exc}")
            return
        self.loaded = project
        self.project_page.name.setText(project.name)
        self.project_page.description.setPlainText(project.description)
        self.project_page.output.setFilePath(project.output_folder)
        site = self.site_page
        auto = project.epsg == utm_epsg(project.lat, project.lon)
        site.auto_crs.setChecked(auto)
        site.lat.setValue(project.lat)
        site.lon.setValue(project.lon)
        site.regional.setValue(project.regional_buffer_km)
        site.crs.setCrs(QgsCoordinateReferenceSystem(f"EPSG:{project.epsg}"))
        page = self.boundary_page
        page.margin.setValue(project.boundary_margin_km)
        page.applied_mode = project.boundary_mode
        page.boundary_file = project.boundary_file
        page.boundary_name = project.boundary_name
        page.boundary_source = project.boundary_source
        if project.boundary_mode == "file":
            page.file.blockSignals(True)
            page.file.setFilePath(project.boundary_source)
            page.file.blockSignals(False)
        page.set_mode(project.boundary_mode)
        page.show_status()
        self.data_page.folder.setFilePath(project.data_folder)
        self.data_page.set_rows([(f.path, f.role, "") for f in project.data_files])
        self.data_page.scanned_folder = project.data_folder
        src = self.sources_page
        src.coarse_res.setValue(project.options.coarse_resolution_m)
        src.fine_res.setValue(project.options.fine_resolution_m)
        src.min_thick.setValue(project.options.min_thickness_m)
        src.lakes.setChecked(project.options.lake_bathymetry)
        src.lake_area.setValue(project.options.lake_min_area_ha)
        src.climate.setChecked(project.options.climate_retrieval)
        src.set_selected(project.providers)
        self.update_rubber_bands()

    def _rects(self) -> tuple[QgsRectangle, QgsCoordinateReferenceSystem] | None:
        try:
            project = self.build_project(include_data=False)
            region = project.regional_grid().bounds
        except ValueError:
            return None
        return QgsRectangle(*region), QgsCoordinateReferenceSystem(f"EPSG:{project.epsg}")

    def update_rubber_bands(self) -> None:
        rects = self._rects()
        if rects is None or (self.site_page.lat.value() == 0.0 and self.site_page.lon.value() == 0.0):
            self.region_band.reset(Qgis.GeometryType.Polygon)
            return
        region, crs = rects
        self.region_band.setToGeometry(QgsGeometry.fromRect(region), crs)
        self.boundary_band.reset(Qgis.GeometryType.Polygon)
        page = self.boundary_page
        if page.mode() != "square" and page.has_geometry():
            wkt = bnd.boundary_wkt(Path(page.boundary_file), 4326)
            self.boundary_band.setToGeometry(QgsGeometry.fromWkt(wkt), QgsCoordinateReferenceSystem("EPSG:4326"))

    def zoom_to_region(self) -> None:
        rects = self._rects()
        if rects is None:
            return
        region, crs = rects
        transform = QgsCoordinateTransform(crs, self.canvas.mapSettings().destinationCrs(), QgsProject.instance())
        extent = transform.transformBoundingBox(region)
        extent.scale(1.1)
        self.canvas.setExtent(extent)
        self.canvas.refresh()

    def start_pick(self) -> None:
        if self.pick_tool is None:
            self.pick_tool = QgsMapToolEmitPoint(self.canvas)
            self.pick_tool.canvasClicked.connect(self.picked)
        self.previous_tool = self.canvas.mapTool()
        self.canvas.setMapTool(self.pick_tool)
        self.iface.messageBar().pushInfo("MUT Model Builder", "Click the project location on the map.")

    def start_extent(self) -> None:
        if self.extent_tool is None:
            self.extent_tool = QgsMapToolExtent(self.canvas)
            self.extent_tool.extentChanged.connect(self.extent_drawn)
        self.previous_tool = self.canvas.mapTool()
        self.canvas.setMapTool(self.extent_tool)
        self.iface.messageBar().pushInfo("MUT Model Builder", "Click and drag a rectangle on the map.")

    def extent_drawn(self, rect: QgsRectangle) -> None:
        if rect.isEmpty():
            return
        self._restore_tool()
        self.boundary_page.use_rectangle(rect, self.canvas.mapSettings().destinationCrs())
        self.extent_tool.clearRubberBand()
        self.raise_()
        self.activateWindow()

    def picked(self, point, _button) -> None:
        transform = QgsCoordinateTransform(
            self.canvas.mapSettings().destinationCrs(), QgsCoordinateReferenceSystem("EPSG:4326"), QgsProject.instance()
        )
        geo = transform.transform(point)
        self.site_page.lat.setValue(geo.y())
        self.site_page.lon.setValue(geo.x())
        self._restore_tool()
        self.raise_()
        self.activateWindow()

    def _restore_tool(self) -> None:
        current = self.canvas.mapTool()
        if current is None or current not in (self.pick_tool, self.extent_tool):
            return
        if self.previous_tool is not None and self.previous_tool not in (self.pick_tool, self.extent_tool):
            self.canvas.setMapTool(self.previous_tool)
        else:
            self.canvas.unsetMapTool(current)

    def cleanup(self) -> None:
        self._restore_tool()
        for band in (self.region_band, self.boundary_band):
            band.reset(Qgis.GeometryType.Polygon)
        if self.extent_tool is not None:
            self.extent_tool.clearRubberBand()
        self.boundary_page.cancel_task()
        if self.run_page.task is not None:
            self.run_page.task.cancel()
