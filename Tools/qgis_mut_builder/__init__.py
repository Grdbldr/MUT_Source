"""MUT Model Builder: QGIS plugin and headless pipeline for surficial-layer surfaces.

QGIS loads this package as a plugin through ``classFactory``. The processing
logic lives in ``core`` and does not import QGIS GUI modules, so it can also
run headless through ``run_headless.py``.
"""


def classFactory(iface):  # noqa: N802 (QGIS plugin entry-point name)
    from .plugin.plugin import MutModelBuilderPlugin

    return MutModelBuilderPlugin(iface)
