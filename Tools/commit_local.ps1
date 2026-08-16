<#
.SYNOPSIS
    Stage all changes and create a local git commit with a versioned message.

.DESCRIPTION
    Reads MUTVersion from GeneralRoutines.f90, drafts a brief summary from
    changed paths (or uses -Message), appends " - Version YYYY.nnn", then
    git add -A and git commit. Never pushes or amends.

.PARAMETER Message
    Optional one-line summary. If omitted, a short default is built from paths.

.EXAMPLE
    .\Tools\commit_local.ps1
    .\Tools\commit_local.ps1 -Message "Add GSTR and default VEL package wiring"
#>
[CmdletBinding()]
param(
    [Parameter(Mandatory = $false)]
    [string]$Message
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
    # Strip DEBUG / RELEASE suffix; keep YYYY.nnn
    $ver = ($raw -replace '\s+(DEBUG|RELEASE)\s*$', '').Trim()
    if ($ver -notmatch '^\d{4}\.\d+') {
        throw "Unexpected MUTVersion format: '$raw'"
    }
    return $ver
}

function Get-AutoSummary {
    $all = @(
        git diff --name-only
        git diff --name-only --cached
        git ls-files --others --exclude-standard
    ) | Where-Object { $_ } | Sort-Object -Unique

    if (-not $all -or $all.Count -eq 0) {
        return 'Update repository'
    }

    $parts = [System.Collections.Generic.List[string]]::new()
    $hasF90 = @($all | Where-Object { $_ -like '*.f90' }).Count -gt 0
    $hasGuide = @($all | Where-Object { $_ -like "*User's Guide*" -or $_ -like '*User?s Guide*' }).Count -gt 0
    $hasTools = @($all | Where-Object { $_ -like 'Tools/*' -or $_ -like 'Tools\*' }).Count -gt 0
    $hasAi = @($all | Where-Object { $_ -like 'Docs/ai/*' -or $_ -like 'Docs\ai\*' -or $_ -eq 'AGENTS.md' }).Count -gt 0

    if ($hasF90) { [void]$parts.Add('Fortran sources') }
    if ($hasGuide) { [void]$parts.Add("User's Guide") }
    if ($hasTools) { [void]$parts.Add('Tools') }
    if ($hasAi) { [void]$parts.Add('agent docs') }

    if ($parts.Count -eq 0) {
        $n = $all.Count
        return "Update $n file$(if ($n -ne 1) { 's' })"
    }
    if ($parts.Count -eq 1) {
        return "Update $($parts[0])"
    }
    if ($parts.Count -eq 2) {
        return "Update $($parts[0]) and $($parts[1])"
    }
    $last = $parts[$parts.Count - 1]
    $head = ($parts[0..($parts.Count - 2)] -join ', ')
    return "Update $head, and $last"
}

# --- main ---
$version = Get-MutVersion
Write-Host "MUTVersion: $version"

$status = git status --porcelain
if (-not $status) {
    Write-Error 'Nothing to commit (working tree clean).'
    exit 1
}

$summary = if ($Message -and $Message.Trim().Length -gt 0) {
    $Message.Trim()
} else {
    Get-AutoSummary
}

$suffix = " - Version $version"
if ($summary -match [regex]::Escape($version)) {
    $commitMessage = $summary
} else {
    $commitMessage = "$summary$suffix"
}

Write-Host "Commit message:"
Write-Host "  $commitMessage"

git add -A
if ($LASTEXITCODE -ne 0) {
    Write-Error 'git add -A failed.'
    exit $LASTEXITCODE
}

# Use a temp file so multiline / special characters are safe; message is one line.
$msgFile = Join-Path $env:TEMP ("mut_commit_msg_{0}.txt" -f [guid]::NewGuid().ToString('N'))
try {
    # UTF-8 without BOM for git
    $utf8NoBom = New-Object System.Text.UTF8Encoding $false
    [System.IO.File]::WriteAllText($msgFile, $commitMessage, $utf8NoBom)
    git commit -F $msgFile
    if ($LASTEXITCODE -ne 0) {
        Write-Error 'git commit failed.'
        exit $LASTEXITCODE
    }
}
finally {
    if (Test-Path -LiteralPath $msgFile) {
        Remove-Item -LiteralPath $msgFile -Force -ErrorAction SilentlyContinue
    }
}

Write-Host ''
Write-Host 'Commit complete (local only; not pushed).'
git status -sb
exit 0
