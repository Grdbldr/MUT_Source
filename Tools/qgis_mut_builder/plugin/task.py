"""Background QgsTasks for the regional pipeline, focus-area, stream, mesh, CLN and MUT builds; log lines go through a signal."""

from __future__ import annotations

import threading
import traceback

from qgis.core import QgsTask
from qgis.PyQt.QtCore import pyqtSignal

from ..core.cln_export import export_cln
from ..core.focus import build_focus
from ..core.geo import GridTooLargeError
from ..core.mesh import build_mesh, preview_streams
from ..core.mut_build import build_mut_project
from ..core.pipeline import run_pipeline


class _CancelableTask(QgsTask):
    """QgsTask that exposes a thread-safe cancel flag for GDAL/network callbacks.

    ``QgsTask.cancel()`` is a no-op unless ``CanCancel`` is set; we also keep a
    ``threading.Event`` so long GDAL work can see the request even if the SIP
    ``isCanceled()`` wrapper is slow to observe it from a progress callback.
    """

    def __init__(self, description: str) -> None:
        super().__init__(description, QgsTask.Flag.CanCancel)
        self._cancel_event = threading.Event()

    def cancel(self) -> None:
        self._cancel_event.set()
        super().cancel()

    def stop_requested(self) -> bool:
        return self._cancel_event.is_set() or self.isCanceled()


class PipelineTask(_CancelableTask):
    message = pyqtSignal(str)
    completed = pyqtSignal(bool)

    def __init__(self, project, api_keys: dict, refresh: bool = False) -> None:
        super().__init__(f"MUT Model Builder: {project.name}")
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
                is_canceled=self.stop_requested,
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


class FocusTask(_CancelableTask):
    message = pyqtSignal(str)
    completed = pyqtSignal(bool)

    def __init__(self, project, focus, api_keys: dict, refresh: bool = False) -> None:
        super().__init__(f"MUT focus area: {focus.name}")
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
                is_canceled=self.stop_requested,
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


class StreamsTask(_CancelableTask):
    message = pyqtSignal(str)
    completed = pyqtSignal(bool)

    def __init__(self, project, spec, refresh: bool = False) -> None:
        super().__init__(f"MUT streams: {spec.name}")
        self.project = project
        self.spec = spec
        self.refresh = refresh
        self.network = None
        self.path = None
        self.error = ""

    def run(self) -> bool:  # worker thread
        try:
            self.network, self.path = preview_streams(
                self.project, self.spec, log=self.message.emit, is_canceled=self.stop_requested, refresh=self.refresh
            )
            return True
        except InterruptedError:
            self.error = "Stream search canceled."
        except Exception:  # noqa: BLE001 (reported to the user in the log)
            self.error = traceback.format_exc()
        return False

    def finished(self, ok: bool) -> None:  # main thread
        self.completed.emit(ok)


class MeshTask(_CancelableTask):
    message = pyqtSignal(str)
    completed = pyqtSignal(bool)

    def __init__(self, project, spec) -> None:
        super().__init__(f"MUT mesh: {spec.name}")
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
                is_canceled=self.stop_requested,
            )
            return True
        except InterruptedError:
            self.error = "Mesh build canceled."
        except Exception:  # noqa: BLE001 (reported to the user in the log)
            self.error = traceback.format_exc()
        return False

    def finished(self, ok: bool) -> None:  # main thread
        self.completed.emit(ok)


class ClnExportTask(_CancelableTask):
    message = pyqtSignal(str)
    completed = pyqtSignal(bool)

    def __init__(self, project, spec, network=None) -> None:
        super().__init__(f"MUT CLN streams: {spec.name}")
        self.project = project
        self.spec = spec
        self.network = network
        self.result = None
        self.error = ""

    def run(self) -> bool:  # worker thread
        try:
            self.result = export_cln(
                self.project,
                self.spec,
                network=self.network,
                log=self.message.emit,
                progress=self.setProgress,
            )
            return True
        except InterruptedError:
            self.error = "CLN export canceled."
        except Exception:  # noqa: BLE001 (reported to the user in the log)
            self.error = traceback.format_exc()
        return False

    def finished(self, ok: bool) -> None:  # main thread
        self.completed.emit(ok)


class BuildMutTask(_CancelableTask):
    message = pyqtSignal(str)
    completed = pyqtSignal(bool)

    def __init__(self, project, mesh_spec, model_spec) -> None:
        super().__init__(f"MUT build: {model_spec.name}")
        self.project = project
        self.mesh_spec = mesh_spec
        self.model_spec = model_spec
        self.result = None
        self.error = ""

    def run(self) -> bool:  # worker thread
        try:
            self.result = build_mut_project(
                self.project,
                self.mesh_spec,
                self.model_spec,
                log=self.message.emit,
                progress=self.setProgress,
            )
            return True
        except InterruptedError:
            self.error = "Build MUT project canceled."
        except Exception:  # noqa: BLE001 (reported to the user in the log)
            self.error = traceback.format_exc()
        return False

    def finished(self, ok: bool) -> None:  # main thread
        self.completed.emit(ok)
