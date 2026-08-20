<#
.SYNOPSIS
    Run verification models, write the User's Guide comparison appendix, publish selected inputs to MUT_Examples.

.DESCRIPTION
    Working tree is C:\Work\Examples-Release (never copies FROM MUT_Examples).
    Uses VerificationFolder.List only. Extra problem-set folders in Examples-Release are ignored.
    After a successful run, calls ToRepos.bat so Robocopy publishes selected inputs
    into C:\_repo\GrdBldr\MUT_Examples. Does not commit or push.

.PARAMETER SkipBatch
    Skip mut _build / usgs_1 / mut _post.

.PARAMETER SkipExport
    Skip tec360 PNG export.

.PARAMETER SkipPdf
    Write TeX only; do not rebuild the User's Guide PDF.

.PARAMETER SkipToRepos
    Do not run ToRepos.bat after verification.

.PARAMETER ReleaseDir
    Working/run tree. Default: C:\Work\Examples-Release

.PARAMETER BaseDir
    Previous-version results. Default: C:\Work\Examples-Base

.PARAMETER ExamplesRepo
    MUT_Examples git repo. Default: C:\_repo\GrdBldr\MUT_Examples

.EXAMPLE
    .\Tools\verify_release.ps1
    .\Tools\verify_release.ps1 -SkipBatch
#>
[CmdletBinding()]
param(
    [switch]$SkipBatch,
    [switch]$SkipExport,
    [switch]$SkipPdf,
    [switch]$SkipToRepos,
    [string]$ReleaseDir = 'C:\Work\Examples-Release',
    [string]$BaseDir = 'C:\Work\Examples-Base',
    [string]$ExamplesRepo = 'C:\_repo\GrdBldr\MUT_Examples'
)

$ErrorActionPreference = 'Stop'

$repoRoot = Resolve-Path (Join-Path $PSScriptRoot '..')
Set-Location $repoRoot

function Get-MutVersion {
    $gr = Join-Path $repoRoot 'GeneralRoutines.f90'
    if (-not (Test-Path -LiteralPath $gr)) {
        throw "GeneralRoutines.f90 not found at $gr"
    }
    $text = Get-Content -LiteralPath $gr -Raw
    $m = [regex]::Match($text, "MUTVersion\s*=\s*'([^']+)'")
    if (-not $m.Success) {
        throw "Could not parse MUTVersion from GeneralRoutines.f90"
    }
    $raw = $m.Groups[1].Value.Trim()
    $ver = ($raw -replace '\s+(DEBUG|RELEASE)\s*$', '').Trim()
    if ($ver -notmatch '^\d{4}\.\d+') {
        throw "Unexpected MUTVersion format: '$raw'"
    }
    return $ver
}

function Get-Python {
    foreach ($name in @('python', 'py')) {
        $cmd = Get-Command $name -ErrorAction SilentlyContinue
        if ($cmd) {
            return $cmd.Source
        }
    }
    throw 'python not found on PATH'
}

if (-not (Test-Path -LiteralPath $ReleaseDir)) {
    throw "Release dir not found: $ReleaseDir"
}

$version = Get-MutVersion
Write-Host "MUTVersion: $version"
Write-Host "Release dir: $ReleaseDir"
Write-Host "Baseline:    $BaseDir"
Write-Host "Never copying MUT_Examples -> Examples-Release"

$python = Get-Python
$script = Join-Path $PSScriptRoot 'mut_verify\mut_verify.py'
$pyArgs = @(
    $script,
    '--release-dir', $ReleaseDir,
    '--base-dir', $BaseDir
)
if ($SkipBatch) { $pyArgs += '--skip-batch' }
if ($SkipExport) { $pyArgs += '--skip-export' }
if ($SkipPdf) { $pyArgs += '--skip-pdf' }

& $python @pyArgs
if ($LASTEXITCODE -ne 0) {
    Write-Error "mut_verify.py failed (exit $LASTEXITCODE)."
    exit $LASTEXITCODE
}

if (-not $SkipToRepos) {
    $toRepos = Join-Path $ReleaseDir 'ToRepos.bat'
    if (-not (Test-Path -LiteralPath $toRepos)) {
        throw "ToRepos.bat not found: $toRepos"
    }
    Write-Host ''
    Write-Host "Publishing selected inputs with ToRepos.bat (Robocopy -> MUT_Examples)"
    Push-Location $ReleaseDir
    try {
        cmd /c ToRepos.bat
        # Robocopy: 0-7 are success
        if ($LASTEXITCODE -ge 8) {
            throw "ToRepos.bat / Robocopy failed (exit $LASTEXITCODE)."
        }
    }
    finally {
        Pop-Location
        $global:LASTEXITCODE = 0
    }
    if (Test-Path -LiteralPath $ExamplesRepo) {
        $versionLst = Join-Path $ExamplesRepo 'Version.lst'
        $utf8NoBom = New-Object System.Text.UTF8Encoding $false
        [System.IO.File]::WriteAllText(
            $versionLst,
            "Mut Examples Version $version`r`n",
            $utf8NoBom
        )
        Write-Host "Wrote $versionLst"
    }
    else {
        Write-Warning "MUT_Examples repo not found: $ExamplesRepo (Version.lst not updated)"
    }
}
else {
    Write-Host 'Skipping ToRepos.bat'
}

Write-Host ''
Write-Host 'Verification appendix updated (local only; not committed).'
Write-Host "Commit MUT_Source (appendix TeX/PNGs) and MUT_Examples (if ToRepos ran) with version $version."
Write-Host '  .\Tools\commit_local.ps1 -Message "Add release-verification appendix"'
exit 0
