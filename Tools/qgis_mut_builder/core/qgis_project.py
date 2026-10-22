"""Styled QGIS layers for the outputs, written to a .qgz or added to the open project.

Requires an initialised ``QgsApplication``. Call from the main (GUI) thread.
"""

from __future__ import annotations

from pathlib import Path

from qgis.core import (
    Qgis,
    QgsApplication,
    QgsContrastEnhancement,
    QgsCoordinateReferenceSystem,
    QgsFillSymbol,
    QgsLineSymbol,
    QgsMarkerSymbol,
    QgsProject,
    QgsRasterBandStats,
    QgsRasterDataProvider,
    QgsRasterLayer,
    QgsRuleBasedRenderer,
    QgsSingleBandGrayRenderer,
    QgsSingleBandPseudoColorRenderer,
    QgsStyle,
    QgsVectorLayer,
)

OSM_URI = "type=xyz&url=https://tile.openstreetmap.org/{z}/{x}/{y}.png&zmax=19&zmin=0"

LAYERS = (
    ("surficial_thickness", "Surficial layer thickness (m)", "YlOrBr", False),
    ("bedrock_bottom", "Bedrock surface (m)", "Viridis", False),
    ("ground_top", "Ground surface (m)", "Spectral", True),
)
FOCUS_COLOUR = "230,120,0"


def _min_max(path: Path) -> tuple[float, float]:
    layer = QgsRasterLayer(str(path), "stats")
    stats = layer.dataProvider().bandStatistics(1, QgsRasterBandStats.Min | QgsRasterBandStats.Max)
    return stats.minimumValue, stats.maximumValue


def _pseudocolor(layer: QgsRasterLayer, ramp_name: str, invert: bool, value_range: tuple[float, float] | None = None) -> None:
    provider = layer.dataProvider()
    if value_range is None:
        stats = provider.bandStatistics(1, QgsRasterBandStats.Min | QgsRasterBandStats.Max)
        value_range = (stats.minimumValue, stats.maximumValue)
    ramp = QgsStyle.defaultStyle().colorRamp(ramp_name)
    if ramp is None:
        return
    if invert:
        ramp.invert()
    renderer = QgsSingleBandPseudoColorRenderer(provider, 1)
    renderer.setClassificationMin(value_range[0])
    renderer.setClassificationMax(value_range[1])
    renderer.createShader(
        ramp,
        Qgis.ShaderInterpolationMethod.Linear,
        Qgis.ShaderClassificationMethod.Continuous,
        10,
    )
    layer.setRenderer(renderer)


def _smooth(layer: QgsRasterLayer) -> None:
    provider = layer.dataProvider()
    provider.enableProviderResampling(True)
    provider.setZoomedInResamplingMethod(QgsRasterDataProvider.ResamplingMethod.Cubic)
    provider.setZoomedOutResamplingMethod(QgsRasterDataProvider.ResamplingMethod.Bilinear)


def _hillshade(path: Path, name: str) -> QgsRasterLayer:
    """Grey layer from a precomputed GDAL hillshade (QGIS's live hillshade renderer stripes when zoomed in)."""
    layer = QgsRasterLayer(str(path), name)
    _smooth(layer)
    renderer = QgsSingleBandGrayRenderer(layer.dataProvider(), 1)
    enhancement = QgsContrastEnhancement(layer.dataProvider().dataType(1))
    enhancement.setContrastEnhancementAlgorithm(QgsContrastEnhancement.ContrastEnhancementAlgorithm.StretchToMinimumMaximum)
    enhancement.setMinimumValue(0)
    enhancement.setMaximumValue(255)
    renderer.setContrastEnhancement(enhancement)
    layer.setRenderer(renderer)
    layer.setOpacity(0.45)
    return layer


LAKE_RULES = (
    ("Lake bottom ESTIMATED (not measured)", "\"method\" LIKE '%estimated%'", "200,0,160", "dash"),
    ("Lake bottom from measured survey", "\"method\" LIKE '%measured%' AND \"method\" NOT LIKE '%estimated%'", "0,90,220", "solid"),
    ("Lake bottom from local file", "\"method\" = 'local'", "0,150,90", "solid"),
    ("Lake not corrected", "\"method\" = 'skipped'", "120,120,120", "dot"),
)


def _lakes_layer(path: Path, title: str) -> QgsVectorLayer:
    layer = QgsVectorLayer(str(path), title, "ogr")
    if not layer.isValid():
        return layer
    root = QgsRuleBasedRenderer.Rule(None)
    for label, expression, colour, style in LAKE_RULES:
        symbol = QgsFillSymbol.createSimple(
            {"color": "0,0,0,0", "outline_color": colour, "outline_width": "0.6", "outline_style": style}
        )
        root.appendChild(QgsRuleBasedRenderer.Rule(symbol, 0, 0, expression, label))
    layer.setRenderer(QgsRuleBasedRenderer(root))
    return layer


def _add_lake_layers(qproject: QgsProject, group, outputs: dict, lakes_file, water_key: str, ground_range, prefix: str) -> list:
    """Lake outlines (by depth source), lake depth and the uncorrected DEM (hidden) when bathymetry was applied."""
    added = []
    if lakes_file is not None and Path(lakes_file).exists():
        layer = _lakes_layer(Path(lakes_file), f"{prefix}Lakes: bathymetry applied")
        if layer.isValid():
            qproject.addMapLayer(layer, False)
            group.insertLayer(0, layer)
            added.append(layer)
    depth = outputs.get("lake_depth")
    if depth is not None and Path(depth).exists():
        layer = QgsRasterLayer(str(depth), f"{prefix}Lake depth below water surface (m)")
        if layer.isValid():
            _pseudocolor(layer, "Blues", False)
            qproject.addMapLayer(layer, False)
            group.addLayer(layer).setItemVisibilityChecked(False)
            added.append(layer)
    water = outputs.get(water_key)
    if water is not None and Path(water).exists():
        layer = QgsRasterLayer(str(water), f"{prefix}Ground surface before bathymetry (lake water surface) (m)")
        if layer.isValid():
            _smooth(layer)
            _pseudocolor(layer, "Spectral", True, ground_range)
            qproject.addMapLayer(layer, False)
            group.addLayer(layer).setItemVisibilityChecked(False)
            added.append(layer)
    return added


def _remove_group(qproject: QgsProject, title: str) -> None:
    """Remove a layer-tree group and every map layer it contains."""
    root = qproject.layerTreeRoot()
    old = root.findGroup(title)
    if old is None:
        return
    for layer_id in old.findLayerIds():
        qproject.removeMapLayer(layer_id)
    root.removeChildNode(old)


def _path_key(path: Path) -> str:
    return str(Path(path).resolve()).replace("\\", "/").casefold()


def release_paths(qproject: QgsProject, paths) -> None:
    """Remove any map layer whose data source refers to one of ``paths`` (Windows file locks)."""
    keys = {_path_key(p) for p in paths if p is not None}
    if not keys:
        return
    remove = []
    for layer in qproject.mapLayers().values():
        source = layer.source().replace("\\", "/").casefold()
        if any(key in source for key in keys):
            remove.append(layer.id())
    for layer_id in remove:
        qproject.removeMapLayer(layer_id)


def _flush_layer_handles() -> None:
    """Let QGIS finish closing OGR/GDAL handles before a worker overwrites the files."""
    QgsApplication.processEvents()


def release_mesh_layers(qproject: QgsProject, project, spec) -> None:
    """Unload mesh and streams-preview layers so ``write_gpkg`` can replace the GeoPackage."""
    from .mesh import mesh_outputs, streams_preview_path

    _remove_group(qproject, f"{project.name} mesh: {spec.name}")
    preview_title = f"{spec.name} streams (preview)"
    for old in qproject.mapLayersByName(preview_title):
        qproject.removeMapLayer(old.id())
    release_paths(qproject, (mesh_outputs(project, spec)["gpkg"], streams_preview_path(project, spec)))
    _flush_layer_handles()


def release_streams_preview(qproject: QgsProject, project, spec) -> None:
    """Unload only the streams preview layer before rewriting the preview GeoPackage."""
    from .mesh import streams_preview_path

    preview_title = f"{spec.name} streams (preview)"
    for old in qproject.mapLayersByName(preview_title):
        qproject.removeMapLayer(old.id())
    release_paths(qproject, (streams_preview_path(project, spec),))
    _flush_layer_handles()


def release_focus_layers(qproject: QgsProject, project, focus) -> None:
    """Unload focus-area layers so DEM / lake outputs can be overwritten."""
    from .focus import focus_outputs

    outputs = focus_outputs(project, focus)
    _remove_group(qproject, f"{project.name} focus: {focus.name}")
    release_paths(
        qproject,
        (
            outputs.get("ground_top"),
            outputs.get("hillshade"),
            outputs.get("water_surface"),
            outputs.get("lake_depth"),
            outputs.get("lakes"),
            Path(focus.boundary_file) if focus.boundary_file else None,
        ),
    )
    _flush_layer_handles()


def release_surfaces_layers(qproject: QgsProject, project) -> None:
    """Unload regional surface layers so a wizard re-run can rewrite GeoTIFFs and lake files."""
    _remove_group(qproject, f"{project.name} surfaces")
    paths = [project.surfaces_dir]
    if project.boundary_file:
        paths.append(Path(project.boundary_file))
    release_paths(qproject, paths)
    _flush_layer_handles()


def populate(qproject: QgsProject, project, result, add_basemap: bool = True) -> list:
    """Add styled output layers (and boundary files) to ``qproject`` under a "<name> surfaces" group."""
    release_surfaces_layers(qproject, project)
    root = qproject.layerTreeRoot()
    group = root.insertGroup(0, f"{project.name} surfaces")
    added = []

    for i, vec in enumerate(result.boundary_files):
        title = "Model boundary" if i == 0 and project.has_boundary else Path(vec).stem
        layer = QgsVectorLayer(str(vec), title, "ogr")
        if layer.isValid():
            if layer.geometryType() == Qgis.GeometryType.Polygon:
                colour = "26,150,65" if title == "Model boundary" else "90,90,90"
                symbol = QgsFillSymbol.createSimple({"color": "0,0,0,0", "outline_color": colour, "outline_width": "0.8"})
                layer.renderer().setSymbol(symbol)
            qproject.addMapLayer(layer, False)
            group.addLayer(layer)
            added.append(layer)

    ground_range = _min_max(result.outputs["ground_top"]) if "ground_top" in result.outputs else None

    for key, title, ramp, invert in LAYERS:
        path = result.outputs.get(key)
        if path is None:
            continue
        shade_path = result.hillshades.get(key)
        if shade_path is not None:
            shade = _hillshade(shade_path, f"{title} hillshade")
            if shade.isValid():
                qproject.addMapLayer(shade, False)
                group.addLayer(shade)
                added.append(shade)
        if key == "ground_top" and "ground_top_water_surface" in result.outputs:
            title = "Ground surface with lake bottoms (bathymetry) (m)"
        layer = QgsRasterLayer(str(path), title)
        if not layer.isValid():
            continue
        _smooth(layer)
        _pseudocolor(layer, ramp, invert, ground_range if key == "ground_top" else None)
        qproject.addMapLayer(layer, False)
        node = group.addLayer(layer)
        if key in ("bedrock_bottom", "surficial_thickness"):
            node.setItemVisibilityChecked(False)
        added.append(layer)
    added += _add_lake_layers(
        qproject, group, result.outputs, result.lakes_file, "ground_top_water_surface", ground_range, ""
    )

    if add_basemap and not any(isinstance(l, QgsRasterLayer) and l.providerType() == "wms" and "openstreetmap" in l.source() for l in qproject.mapLayers().values()):
        osm = QgsRasterLayer(OSM_URI, "OpenStreetMap", "wms")
        if osm.isValid():
            qproject.addMapLayer(osm, False)
            root.addLayer(osm)
    return added


def _outline(path: Path, title: str, colour: str, width: str = "0.8") -> QgsVectorLayer:
    layer = QgsVectorLayer(str(path), title, "ogr")
    if layer.isValid():
        symbol = QgsFillSymbol.createSimple({"color": "0,0,0,0", "outline_color": colour, "outline_width": width})
        layer.renderer().setSymbol(symbol)
    return layer


def populate_focus(qproject: QgsProject, project, focus) -> list:
    """Add a built focus area (outline, ground surface, hillshade) as a "<name> focus: <focus>" group."""
    from .focus import focus_outputs

    outputs = focus_outputs(project, focus)
    title = f"{project.name} focus: {focus.name}"
    release_focus_layers(qproject, project, focus)
    root = qproject.layerTreeRoot()
    group = root.insertGroup(0, title)
    added = []
    outline = _outline(Path(focus.boundary_file), f"{focus.name} outline", FOCUS_COLOUR, "1.0")
    if outline.isValid():
        qproject.addMapLayer(outline, False)
        group.addLayer(outline)
        added.append(outline)
    regional = project.surfaces_dir / "ground_top.tif"
    ground_range = _min_max(regional) if regional.exists() else None
    shade_path = outputs.get("hillshade")
    if shade_path is not None and shade_path.exists():
        shade = _hillshade(shade_path, f"{focus.name} hillshade")
        if shade.isValid():
            qproject.addMapLayer(shade, False)
            group.addLayer(shade)
            added.append(shade)
    dem_path = outputs.get("ground_top")
    if dem_path is not None and dem_path.exists():
        lakes = " with lake bottoms (bathymetry)" if outputs["water_surface"].exists() else ""
        layer = QgsRasterLayer(str(dem_path), f"{focus.name} ground surface{lakes} {focus.resolution_m:g} m")
        if layer.isValid():
            _smooth(layer)
            _pseudocolor(layer, "Spectral", True, ground_range)
            qproject.addMapLayer(layer, False)
            group.addLayer(layer)
            added.append(layer)
    lake_outputs = {"lake_depth": outputs["lake_depth"], "water_surface": outputs["water_surface"]}
    added += _add_lake_layers(
        qproject, group, lake_outputs, outputs["lakes"], "water_surface", ground_range, f"{focus.name}: "
    )
    return added


MESH_COLOUR = "30,60,200"
NODE_RULES = (
    ("Fixed node (boundary, user point)", '"fixed" = 1', "220,30,30", "1.6"),
    ("Free node (may move when smoothed)", '"fixed" = 0', "30,60,200", "1.0"),
)
STREAM_NODE_RULES = (
    ("Fixed node on a stream", '"stream_level" > 0', "0,170,220", "1.8"),
    ("Fixed node (boundary, user point)", '"fixed" = 1 AND "stream_level" = 0', "220,30,30", "1.6"),
    ("Free node (may move when smoothed)", '"fixed" = 0', "30,60,200", "1.0"),
)
STREAM_STYLES = ((1, "0,40,140", "1.0"), (2, "20,90,220", "0.7"), (3, "0,160,230", "0.5"))


def streams_layer(path: Path, title: str, layer_name: str = "streams") -> QgsVectorLayer:
    """Ranked streams styled by level (darker and wider for the main river)."""
    layer = QgsVectorLayer(f"{path}|layername={layer_name}", title, "ogr")
    if not layer.isValid():
        return layer
    root = QgsRuleBasedRenderer.Rule(None)
    for level, colour, width in STREAM_STYLES:
        symbol = QgsLineSymbol.createSimple({"color": colour, "width": width, "capstyle": "round"})
        root.appendChild(QgsRuleBasedRenderer.Rule(symbol, 0, 0, f'"level" = {level}', f"Level {level}"))
    symbol = QgsLineSymbol.createSimple({"color": "90,190,240", "width": "0.35"})
    root.appendChild(QgsRuleBasedRenderer.Rule(symbol, 0, 0, '"level" > 3', "Level 4 and above"))
    layer.setRenderer(QgsRuleBasedRenderer(root))
    return layer


def populate_mesh(qproject: QgsProject, project, spec) -> list:
    """Add a built mesh (elements, fixed/free nodes) as a "<name> mesh: <mesh>" group."""
    from .mesh import mesh_outputs

    gpkg = mesh_outputs(project, spec)["gpkg"]
    title = f"{project.name} mesh: {spec.name}"
    release_mesh_layers(qproject, project, spec)
    if not gpkg.exists():
        return []
    root = qproject.layerTreeRoot()
    group = root.insertGroup(0, title)
    added = []
    nodes = QgsVectorLayer(f"{gpkg}|layername=nodes", f"{spec.name} nodes", "ogr")
    if nodes.isValid():
        rules = QgsRuleBasedRenderer.Rule(None)
        has_streams = nodes.fields().indexOf("stream_level") >= 0
        for label, expression, colour, size in STREAM_NODE_RULES if has_streams else NODE_RULES:
            symbol = QgsMarkerSymbol.createSimple({"name": "circle", "color": colour, "size": size, "outline_style": "no"})
            rules.appendChild(QgsRuleBasedRenderer.Rule(symbol, 0, 0, expression, label))
        nodes.setRenderer(QgsRuleBasedRenderer(rules))
        qproject.addMapLayer(nodes, False)
        group.addLayer(nodes)
        added.append(nodes)
    streams = streams_layer(gpkg, f"{spec.name} streams")
    if streams.isValid():
        qproject.addMapLayer(streams, False)
        group.addLayer(streams)
        added.append(streams)
    elements = _outline(Path(f"{gpkg}|layername=elements"), f"{spec.name} triangles", MESH_COLOUR, "0.2")
    if elements.isValid():
        qproject.addMapLayer(elements, False)
        group.addLayer(elements)
        added.append(elements)
    return added


def write_qgz(project, result=None) -> Path:
    """Write ``<output>/<slug>.qgz`` with the styled regional outputs, every built focus area and mesh."""
    if result is None and (project.surfaces_dir / "sources.json").exists():
        from .pipeline import load_result

        result = load_result(project)
    qproject = QgsProject()
    qproject.setCrs(QgsCoordinateReferenceSystem(f"EPSG:{project.epsg}"))
    qproject.setTitle(f"{project.name} surfaces")
    if result is not None:
        populate(qproject, project, result)
    for focus in project.focus_areas:
        if focus.status == "built":
            populate_focus(qproject, project, focus)
    for spec in project.meshes:
        if spec.status == "built":
            populate_mesh(qproject, project, spec)
    path = project.qgz_path
    if not qproject.write(str(path)):
        raise RuntimeError(f"Could not write {path}: {qproject.error()}")
    return path


def refresh_qgz(project, result=None) -> str:
    """Rewrite the project's .qgz unless it is the project open in QGIS (whose layers the caller has already
    updated); rewriting the open file makes QGIS warn that it changed on disk."""
    open_file = QgsProject.instance().fileName()
    if open_file and Path(open_file).resolve() == project.qgz_path.resolve():
        return f"Layers added to the open QGIS project; save it (Ctrl+S) to keep them in {project.qgz_path.name}"
    return f"QGIS project written: {write_qgz(project, result)}"


def _set_home_path(qproject: QgsProject, home: str) -> None:
    """Set project home path across QGIS API variants without raising AttributeError.

    Older/newer builds expose ``setHomePath`` or ``setPresetHomePath`` (the latter
    backs the ``presetHomePath`` property). If neither setter exists, skip safely.
    """
    for name in ("setHomePath", "setPresetHomePath"):
        setter = getattr(qproject, name, None)
        if callable(setter):
            setter(home)
            return


def bind_open_qgz(project) -> None:
    """Point the open QGIS project at ``project.qgz_path`` so Save/Save As use the model folder."""
    qproject = QgsProject.instance()
    path = str(project.qgz_path.resolve())
    if Path(qproject.fileName() or "").resolve() != project.qgz_path.resolve():
        qproject.setFileName(path)
    _set_home_path(qproject, str(project.output_path.resolve()))
    if not qproject.title():
        qproject.setTitle(project.name)
