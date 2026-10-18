"""Background QgsTasks for the regional pipeline, focus-area, stream and mesh builds; log lines go through a signal."""

from __future__ import annotations

import traceback

from qgis.core import QgsTask
from qgis.PyQt.QtCore import pyqtSignal

from ..core.focus import build_focus
from ..core.geo import GridTooLargeError
from ..core.mesh import build_mesh, preview_streams
from ..core.pipeline import run_pipeline


class PipelineTask(QgsTask):
    message = pyqtSignal(str)
    completed = pyqtSignal(bool)

    def __init__(self, project, api_keys: dict, refresh: bool = False) -> None:
        super().__init__(f"MUT Model Builder: {project.name}", QgsTask.CanCancel)
        self.project = project
        self.api_keys = api_keys
        self.refresh = refresh
        self.result = None
        self.error = ""

    def run(self) -> bool:  # runs in a worker thread; no GUI or layer access here
        try:
            self.result = run_pipeline(
                self.project,
                api_keys=self.api_keys,
                log=self.message.emit,
                progress=self.setProgress,
                is_canceled=self.isCanceled,
                refresh=self.refresh,
            )
            return True
        except InterruptedError:
            self.error = "Run canceled."
        except GridTooLargeError as exc:
            self.error = str(exc)
        except Exception:  # noqa: BLE001 (reported to the user in the run log)
            self.error = traceback.format_exc()
        return False

    def finished(self, ok: bool) -> None:  # main thread
        self.completed.emit(ok)


class FocusTask(QgsTask):
    message = pyqtSignal(str)
    completed = pyqtSignal(bool)

    def __init__(self, project, focus, api_keys: dict, refresh: bool = False) -> None:
        super().__init__(f"MUT focus area: {focus.name}", QgsTask.CanCancel)
        self.project = project
        self.focus = focus
        self.api_keys = api_keys
        self.refresh = refresh
        self.result = None
        self.error = ""

    def run(self) -> bool:  # worker thread
        try:
            self.result = build_focus(
                self.project,
                self.focus,
                api_keys=self.api_keys,
                log=self.message.emit,
                progress=self.setProgress,
                is_canceled=self.isCanceled,
                refresh=self.refresh,
            )
            return True
        except InterruptedError:
            self.error = "Download canceled."
        except GridTooLargeError as exc:
            self.error = str(exc)
        except Exception:  # noqa: BLE001 (reported to the user in the log)
            self.error = traceback.format_exc()
        return False

    def finished(self, ok: bool) -> None:  # main thread
        self.completed.emit(ok)


class StreamsTask(QgsTask):
    message = pyqtSignal(str)
    completed = pyqtSignal(bool)

    def __init__(self, project, spec, refresh: bool = False) -> None:
        super().__init__(f"MUT streams: {spec.name}", QgsTask.CanCancel)
        self.project = project
        self.spec = spec
        self.refresh = refresh
        self.network = None
        self.path = None
        self.error = ""

    def run(self) -> bool:  # worker thread
        try:
            self.network, self.path = preview_streams(
                self.project, self.spec, log=self.message.emit, is_canceled=self.isCanceled, refresh=self.refresh
            )
            return True
        except InterruptedError:
            self.error = "Stream search canceled."
        except Exception:  # noqa: BLE001 (reported to the user in the log)
            self.error = traceback.format_exc()
        return False

    def finished(self, ok: bool) -> None:  # main thread
        self.completed.emit(ok)


class MeshTask(QgsTask):
    message = pyqtSignal(str)
    completed = pyqtSignal(bool)

    def __init__(self, project, spec) -> None:
        super().__init__(f"MUT mesh: {spec.name}", QgsTask.CanCancel)
        self.project = project
        self.spec = spec
        self.result = None
        self.error = ""

    def run(self) -> bool:  # worker thread
        try:
            self.result = build_mesh(
                self.project,
                self.spec,
                log=self.message.emit,
                progress=self.setProgress,
                is_canceled=self.isCanceled,
            )
            return True
        except InterruptedError:
            self.error = "Mesh build canceled."
        except Exception:  # noqa: BLE001 (reported to the user in the log)
            self.error = traceback.format_exc()
        return False

    def finished(self, ok: bool) -> None:  # main thread
        self.completed.emit(ok)
