<#
.SYNOPSIS
    Install Tools\qgis_mut_builder into the QGIS plugins folder as "mut_model_builder".

.DESCRIPTION
    By default the plugin is copied, so QGIS's Plugin Manager can uninstall or reinstall it safely.
    Re-run this script after editing the plugin, then restart QGIS.

    -Link creates a directory junction instead, so QGIS loads the plugin straight from this repo.
    Never uninstall or reinstall a linked plugin from QGIS's Plugin Manager: it deletes through the
    junction and removes the source files in this repo. Use -Remove instead.

    Enable it afterwards in QGIS: Plugins > Manage and Install Plugins > Installed > MUT Model Builder.

.EXAMPLE
    .\Tools\qgis_mut_builder\install_plugin.ps1
    .\Tools\qgis_mut_builder\install_plugin.ps1 -QgisProfile work
    .\Tools\qgis_mut_builder\install_plugin.ps1 -Link
    .\Tools\qgis_mut_builder\install_plugin.ps1 -Remove
#>
param(
    [string]$QgisProfile = "default",
    [switch]$Link,
    [switch]$Remove
)

$ErrorActionPreference = "Stop"
$source = $PSScriptRoot
$plugins = Join-Path $env:APPDATA "QGIS\QGIS3\profiles\$QgisProfile\python\plugins"
$target = Join-Path $plugins "mut_model_builder"

if (Test-Path $target) {
    $item = Get-Item $target -Force
    if ($item.LinkType -eq "Junction" -or $item.LinkType -eq "SymbolicLink") {
        # Delete the link only, never the repo files it points to.
        [System.IO.Directory]::Delete($target)
        Write-Host "Removed existing link $target"
    }
    elseif (Test-Path (Join-Path $target "metadata.txt")) {
        Remove-Item $target -Recurse -Force
        Write-Host "Removed existing copy $target"
    }
    else {
        throw "$target exists but is not a MUT Model Builder install; remove it manually."
    }
}

if ($Remove) {
    return
}

New-Item -ItemType Directory -Path $plugins -Force | Out-Null
if ($Link) {
    New-Item -ItemType Junction -Path $target -Target $source | Out-Null
    Write-Host "Linked $target -> $source"
    Write-Warning "Do not uninstall or reinstall this plugin from QGIS's Plugin Manager; it would delete the repo files. Use -Remove."
}
else {
    robocopy $source $target /E /XD __pycache__ /XF *.pyc install_plugin.ps1 /NFL /NDL /NJH /NJS /NP | Out-Null
    if ($LASTEXITCODE -ge 8) {
        throw "Copy to $target failed (robocopy exit $LASTEXITCODE)."
    }
    Write-Host "Copied $source -> $target"
}
Write-Host "Restart QGIS, then enable it: Plugins > Manage and Install Plugins > Installed > MUT Model Builder"
exit 0
