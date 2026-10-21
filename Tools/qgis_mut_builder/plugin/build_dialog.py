"""'Build MUT project...' dialog: write ``2_models/<name>/_build.mut`` from a built mesh and surfaces."""

from __future__ import annotations

from pathlib import Path

from qgis.core import QgsApplication, QgsSettings
from qgis.gui import QgsFileWidget
from qgis.PyQt.QtCore import QUrl
from qgis.PyQt.QtGui import QDesktopServices
from qgis.PyQt.QtWidgets import (
    QCheckBox,
    QComboBox,
    QDialog,
    QFormLayout,
    QHBoxLayout,
    QLabel,
    QLineEdit,
    QMessageBox,
    QPlainTextEdit,
    QProgressBar,
    QPushButton,
    QVBoxLayout,
)

from ..core.cln_export import cln_files, has_stream_geometry, stream_preview_candidates
from ..core.mut_build import nchos_paths
from ..core.project import ModelSpec, Project, slugify
from .focus_dialog import FocusAreaDialog
from .task import BuildMutTask
from .wizard import SETTINGS


class BuildMutDialog(QDialog):
    def __init__(self, iface) -> None:
        super().__init__(iface.mainWindow())
        self.iface = iface
        self.setWindowTitle("MUT Model Builder: build MUT project")
        self.resize(640, 560)
        self.project: Project | None = None
        self.task: BuildMutTask | None = None

        layout = QVBoxLayout(self)
        intro = QLabel(
            "Creates a starter MODFLOW-USG model folder under <tt>2_models/&lt;name&gt;/</tt> with "
            "<tt>_build.mut</tt> and elevation list files sampled from the regional surfaces onto the mesh nodes. "
            "Streams follow the mesh: <b>CLN</b> if attached without remeshing, or <b>SWF</b> "
            "<tt>choose cells from gb nodes</tt> if the mesh was rebuilt with fixed stream nodes. "
            "Does not run <tt>mut.exe</tt>."
        )
        intro.setWordWrap(True)
        layout.addWidget(intro)

        form = QFormLayout()
        self.yaml = QgsFileWidget()
        self.yaml.setFilter("Project files (*.yaml *.yml)")
        self.yaml.fileChanged.connect(self.load_project)
        form.addRow("Project file", self.yaml)
        self.mesh = QComboBox()
        self.mesh.currentTextChanged.connect(self.mesh_changed)
        form.addRow("Mesh", self.mesh)
        self.model_name = QLineEdit()
        self.model_name.setPlaceholderText("Model name, e.g. Regional")
        self.model_name.textChanged.connect(lambda _t: self.update_buttons())
        form.addRow("Model name", self.model_name)
        self.include_swf = QCheckBox("Include SWF domain")
        self.include_swf.setChecked(True)
        form.addRow("", self.include_swf)
        self.include_cln = QCheckBox("Include CLN from stream XYZ (when present)")
        self.include_cln.setChecked(True)
        form.addRow("", self.include_cln)
        self.include_nchos = QCheckBox("Include stream node selections (.nchos) under SWF")
        self.include_nchos.setChecked(True)
        form.addRow("", self.include_nchos)
        self.climate = QComboBox()
        self.climate.addItem("Long-term average P−ET (TR spin-up)", "average")
        self.climate.addItem("Transient monthly (P → SWF RCH, PET → GWF EVT)", "transient")
        self.climate.addItem("None (steady SS, no climate BCs)", "none")
        form.addRow("Climate forcing", self.climate)
        layout.addLayout(form)

        self.summary = QLabel("Open a project file.")
        self.summary.setWordWrap(True)
        layout.addWidget(self.summary)

        row = QHBoxLayout()
        self.build_button = QPushButton("Build MUT project")
        self.build_button.clicked.connect(self.build)
        self.cancel_button = QPushButton("Cancel")
        self.cancel_button.clicked.connect(self.cancel)
        self.folder_button = QPushButton("Open model folder")
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

        start = FocusAreaDialog.open_project_yaml() or QgsSettings().value(f"{SETTINGS}/build_project", "", type=str)
        if start and Path(start).is_file():
            self.yaml.setFilePath(str(start))
        self.update_buttons()

    def append(self, text: str) -> None:
        self.log.appendPlainText(text)

    def load_project(self, path: str) -> None:
        self.project = None
        self.mesh.blockSignals(True)
        self.mesh.clear()
        self.mesh.blockSignals(False)
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
        QgsSettings().setValue(f"{SETTINGS}/build_project", path)
        self.project = project
        built = [m for m in project.meshes if m.status == "built" and (project.mesh_dir(m) / f"{m.slug}.grd").exists()]
        self.mesh.blockSignals(True)
        self.mesh.addItems([m.name for m in built])
        self.mesh.blockSignals(False)
        if not self.model_name.text().strip():
            self.model_name.setText(project.name)
        self.mesh_changed(self.mesh.currentText())
        self.update_buttons()

    def current_mesh(self):
        if self.project is None or not self.mesh.currentText():
            return None
        return self.project.get_mesh(self.mesh.currentText())

    def mesh_changed(self, _name: str = "") -> None:
        spec = self.current_mesh()
        if self.project is None or spec is None:
            self.summary.setText("Choose a built mesh." if self.project else "Open a project file.")
            self.update_buttons()
            return
        ground = self.project.surfaces_dir / "ground_top.tif"
        bedrock = self.project.surfaces_dir / "bedrock_bottom.tif"
        nchos = nchos_paths(self.project, spec)
        cln = cln_files(self.project, spec)
        can_export_cln = has_stream_geometry(self.project, spec)
        previews = stream_preview_candidates(self.project, spec)
        ground_label = "yes" if ground.exists() else "<b>missing</b>"
        bedrock_label = "yes" if bedrock.exists() else "no (offset base)"
        bits = [
            f"<b>{self.project.name}</b> / mesh <b>{spec.name}</b> (stream_mode={spec.stream_mode})",
            f"Ground: {ground_label}; bedrock: {bedrock_label}",
        ]
        if cln:
            bits.append(f"CLN XYZ: {len(cln)} file(s)")
        elif can_export_cln:
            src = previews[0].name if previews else "mesh streams layer"
            bits.append(
                f"Ranked streams available ({src}) but not yet attached as CLN to this mesh. "
                "Check <b>Include CLN</b> to export Streams_CLN.xyzList during this build."
            )
        if nchos:
            bits.append(f"Stream .nchos: {len(nchos)} file(s)")
        if not cln and not nchos and not can_export_cln and spec.stream_mode == "none":
            bits.append(
                "No stream attachments on this mesh. Use <b>Add streams...</b> → Find streams, "
                "then Attach streams as CLN (or rebuild with fixed stream nodes)."
            )
        models = ", ".join(f"{m.name} ({m.status})" for m in self.project.models) or "none yet"
        bits.append(f"Models: {models}")
        climate = self.project.output_path / "1_data" / "climate" / "climate.json"
        flows = self.project.output_path / "1_data" / "flows" / "flows.json"
        climate_label = "yes" if climate.exists() else "<b>missing</b> (run regional model)"
        flows_label = "yes" if flows.exists() else "none"
        bits.append(f"Climate data: {climate_label}; outlet flows: {flows_label}")
        self.summary.setText("<br>".join(bits))
        self.include_cln.setEnabled(bool(cln) or spec.stream_mode == "cln" or can_export_cln)
        self.include_nchos.setEnabled(bool(nchos))
        if not self.include_cln.isEnabled():
            self.include_cln.setChecked(False)
        elif cln or spec.stream_mode == "cln" or can_export_cln:
            self.include_cln.setChecked(True)
        if not self.include_nchos.isEnabled():
            self.include_nchos.setChecked(False)
        elif nchos:
            self.include_nchos.setChecked(True)
        if climate.exists():
            idx = self.climate.findData("average")
            if idx >= 0:
                self.climate.setCurrentIndex(idx)
        else:
            idx = self.climate.findData("none")
            if idx >= 0:
                self.climate.setCurrentIndex(idx)
        self.update_buttons()

    def update_buttons(self) -> None:
        busy = self.task is not None
        ready = (
            self.project is not None
            and self.current_mesh() is not None
            and bool(self.model_name.text().strip())
            and (self.project.surfaces_dir / "ground_top.tif").exists()
        )
        self.build_button.setEnabled(ready and not busy)
        self.cancel_button.setEnabled(busy)
        model = self.project.get_model(self.model_name.text().strip()) if self.project and self.model_name.text().strip() else None
        self.folder_button.setEnabled(model is not None and self.project.model_dir(model).exists())

    def build(self) -> None:
        if self.project is None or self.task is not None:
            return
        spec = self.current_mesh()
        name = self.model_name.text().strip()
        if spec is None or not name:
            return
        existing = self.project.get_model(name)
        if existing is not None and existing.status == "built":
            answer = QMessageBox.question(self, "Build MUT project", f"'{existing.name}' already exists. Replace it?")
            if answer != QMessageBox.StandardButton.Yes:
                return
        model = ModelSpec(
            name=name,
            mesh=spec.name,
            include_swf=self.include_swf.isChecked(),
            include_cln=self.include_cln.isChecked(),
            include_stream_nchos=self.include_nchos.isChecked(),
            climate=self.climate.currentData() or "average",
        )
        self.task = BuildMutTask(self.project, spec, model)
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
            self.append(task.error or "Build MUT project failed.")
            return
        self.project = task.project
        mut = task.result.outputs.get("mut")
        if mut is not None:
            self.append(f"Wrote {mut}")
            self.append(mut.read_text(encoding="utf-8")[:2000])
        self.mesh_changed()
        self.update_buttons()

    def cancel(self) -> None:
        if self.task is not None:
            self.task.cancel()

    def open_folder(self) -> None:
        name = self.model_name.text().strip()
        model = self.project.get_model(name) if self.project and name else None
        if model is None and self.project and name:
            # open the target folder even before a successful build
            folder = self.project.model_root / slugify(name, "model")
        elif model is not None:
            folder = self.project.model_dir(model)
        else:
            return
        if folder.exists():
            QDesktopServices.openUrl(QUrl.fromLocalFile(str(folder)))

    def cleanup(self) -> None:
        self.cancel()
