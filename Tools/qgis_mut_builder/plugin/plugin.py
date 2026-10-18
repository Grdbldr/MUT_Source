"""QGIS plugin entry: "New hydrogeological model..." (wizard), "New focus area..." and "New mesh..." actions."""

from __future__ import annotations

from qgis.core import QgsApplication
from qgis.PyQt.QtWidgets import QAction

MENU = "&MUT Model Builder"


class MutModelBuilderPlugin:
    def __init__(self, iface) -> None:
        self.iface = iface
        self.action: QAction | None = None
        self.focus_action: QAction | None = None
        self.mesh_action: QAction | None = None
        self.wizard = None
        self.focus_dialog = None
        self.mesh_dialog = None

    def initGui(self) -> None:  # noqa: N802 (QGIS API)
        icon = QgsApplication.getThemeIcon("/mActionNewMap.svg")
        self.action = QAction(icon, "New hydrogeological model...", self.iface.mainWindow())
        self.action.setToolTip("Define a regional model area and build ground and bedrock surfaces for MUT")
        self.action.triggered.connect(self.open_wizard)
        self.iface.addPluginToMenu(MENU, self.action)
        self.iface.addToolBarIcon(self.action)

        icon = QgsApplication.getThemeIcon("/mActionZoomToArea.svg")
        self.focus_action = QAction(icon, "New focus area...", self.iface.mainWindow())
        self.focus_action.setToolTip("Outline a subregion of a regional model and build its ~1 m ground surface")
        self.focus_action.triggered.connect(self.open_focus_dialog)
        self.iface.addPluginToMenu(MENU, self.focus_action)
        self.iface.addToolBarIcon(self.focus_action)

        icon = QgsApplication.getThemeIcon("/mAlgorithmDelaunay.svg")
        self.mesh_action = QAction(icon, "New mesh...", self.iface.mainWindow())
        self.mesh_action.setToolTip("Build a 2D Delaunay triangle mesh of the model area for MUT ('2d mesh from gb')")
        self.mesh_action.triggered.connect(self.open_mesh_dialog)
        self.iface.addPluginToMenu(MENU, self.mesh_action)
        self.iface.addToolBarIcon(self.mesh_action)

    def unload(self) -> None:
        for dialog in (self.wizard, self.focus_dialog, self.mesh_dialog):
            if dialog is not None:
                dialog.cleanup()
                dialog.close()
        self.wizard = self.focus_dialog = self.mesh_dialog = None
        for action in (self.action, self.focus_action, self.mesh_action):
            if action is not None:
                self.iface.removePluginMenu(MENU, action)
                self.iface.removeToolBarIcon(action)
        self.action = self.focus_action = self.mesh_action = None

    def open_wizard(self) -> None:
        from .wizard import ModelBuilderWizard

        if self.wizard is None or not self.wizard.isVisible():
            self.wizard = ModelBuilderWizard(self.iface)
        self.wizard.show()
        self.wizard.raise_()
        self.wizard.activateWindow()

    def open_focus_dialog(self) -> None:
        from .focus_dialog import FocusAreaDialog

        if self.focus_dialog is None or not self.focus_dialog.isVisible():
            self.focus_dialog = FocusAreaDialog(self.iface)
        self.focus_dialog.show()
        self.focus_dialog.raise_()
        self.focus_dialog.activateWindow()

    def open_mesh_dialog(self) -> None:
        from .mesh_dialog import MeshDialog

        if self.mesh_dialog is None or not self.mesh_dialog.isVisible():
            self.mesh_dialog = MeshDialog(self.iface)
        self.mesh_dialog.show()
        self.mesh_dialog.raise_()
        self.mesh_dialog.activateWindow()
