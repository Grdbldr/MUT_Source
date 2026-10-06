<#
.SYNOPSIS
    Write a framed, multi-column TeX listing of a folder's contents.

.DESCRIPTION
    Lists the top-level entries of -Folder the way Windows File Explorer does
    when sorted by name: folders first, then files, each group ordered with
    StrCmpLogicalW (Explorer's case-insensitive, numeric-aware comparison).
    Folders get a trailing backslash. The listing is written to -OutTex as a
    \fbox'ed tabular filled column by column, suitable for \input in the
    User's Guide. The generated file header records the regenerate command.

.PARAMETER Folder
    Folder to list (e.g. C:\Work\Examples-Release\1_VSF_Column).

.PARAMETER OutTex
    Output .tex snippet path. Relative paths are resolved from the current
    directory.

.PARAMETER Exclude
    Wildcard patterns (case-insensitive, matched against entry names) to omit,
    e.g. outputs from later workflow steps.

.PARAMETER Columns
    Number of columns (default 2).

.PARAMETER FontSize
    LaTeX size command applied to the listing (default \small).

.EXAMPLE
    .\Tools\write_folder_listing.ps1 -Folder C:\Work\Examples-Release\1_VSF_Column `
        -OutTex "Docs\User's Guide\listings\1_VSF_Column_build.tex" `
        -Exclude '_posto.*','Modflow.lst'
#>
[CmdletBinding()]
param(
    [Parameter(Mandatory = $true)]
    [string]$Folder,

    [Parameter(Mandatory = $true)]
    [string]$OutTex,

    [string[]]$Exclude = @(),

    [ValidateRange(1, 6)]
    [int]$Columns = 2,

    [string]$FontSize = '\small'
)

$ErrorActionPreference = 'Stop'

if (-not (Test-Path -LiteralPath $Folder -PathType Container)) {
    throw "Folder not found: $Folder"
}
$Folder = (Resolve-Path -LiteralPath $Folder).ProviderPath

if (-not ('MutListing.NaturalSort' -as [type])) {
    Add-Type -Namespace MutListing -Name NaturalSort -MemberDefinition @'
[System.Runtime.InteropServices.DllImport("shlwapi.dll", CharSet = System.Runtime.InteropServices.CharSet.Unicode)]
public static extern int StrCmpLogicalW(string a, string b);
'@
}

function Sort-Explorer([string[]]$Names) {
    if (-not $Names) { return @() }
    $list = [System.Collections.Generic.List[string]]::new([string[]]$Names)
    $list.Sort([System.Comparison[string]] { param($a, $b) [MutListing.NaturalSort]::StrCmpLogicalW($a, $b) })
    return $list.ToArray()
}

# Output is always inside \texttt; \char codes select the typewriter font's
# ASCII glyphs (same slots in OT1 and T1) instead of math-font substitutes.
function ConvertTo-TexText([string]$s) {
    $sb = [System.Text.StringBuilder]::new()
    foreach ($ch in $s.ToCharArray()) {
        switch -CaseSensitive ($ch) {
            '\' { [void]$sb.Append('{\char92}') }
            '_' { [void]$sb.Append('{\char95}') }
            '{' { [void]$sb.Append('{\char123}') }
            '}' { [void]$sb.Append('{\char125}') }
            '~' { [void]$sb.Append('{\char126}') }
            '^' { [void]$sb.Append('{\char94}') }
            '&' { [void]$sb.Append('\&') }
            '%' { [void]$sb.Append('\%') }
            '#' { [void]$sb.Append('\#') }
            '$' { [void]$sb.Append('\$') }
            default { [void]$sb.Append($ch) }
        }
    }
    return $sb.ToString()
}

function Test-Excluded([string]$Name) {
    foreach ($p in $Exclude) {
        if ($Name -like $p) { return $true }
    }
    return $false
}

$entries = Get-ChildItem -LiteralPath $Folder -Force |
    Where-Object { -not ($_.Attributes -band [IO.FileAttributes]::Hidden) } |
    Where-Object { -not (Test-Excluded $_.Name) }

$dirs  = Sort-Explorer @($entries | Where-Object { $_.PSIsContainer } | ForEach-Object Name)
$files = Sort-Explorer @($entries | Where-Object { -not $_.PSIsContainer } | ForEach-Object Name)

$cells = [System.Collections.Generic.List[string]]::new()
foreach ($d in $dirs)  { $cells.Add('\texttt{' + (ConvertTo-TexText $d) + '{\char92}}') }
foreach ($f in $files) { $cells.Add('\texttt{' + (ConvertTo-TexText $f) + '}') }

$nonAscii = @($dirs + $files | Where-Object { $_ -match '[^\x20-\x7E]' })
if ($nonAscii.Count -gt 0) {
    Write-Warning "Non-ASCII names will not typeset reliably: $($nonAscii -join ', ')"
}

$rows = [math]::Max(1, [math]::Ceiling($cells.Count / $Columns))

function Format-PsArg([string]$s) { "'" + $s.Replace("'", "''") + "'" }
$cmd = '.\Tools\write_folder_listing.ps1 -Folder ' + (Format-PsArg $Folder) + ' -OutTex ' + (Format-PsArg $OutTex)
if ($Exclude.Count -gt 0) { $cmd += ' -Exclude ' + (($Exclude | ForEach-Object { Format-PsArg $_ }) -join ',') }
if ($Columns -ne 2) { $cmd += " -Columns $Columns" }
if ($FontSize -ne '\small') { $cmd += ' -FontSize ' + (Format-PsArg $FontSize) }

$colSpec = '@{}' + ((1..$Columns | ForEach-Object { 'l' }) -join '@{\hspace{2.5em}}') + '@{}'

$out = [System.Text.StringBuilder]::new()
[void]$out.AppendLine('% GENERATED FILE - do not edit by hand.')
[void]$out.AppendLine("% Folder listing of $Folder")
[void]$out.AppendLine('% Regenerate from the MUT_Source root with:')
[void]$out.AppendLine("%   $cmd")
[void]$out.AppendLine('{' + $FontSize + '\setlength{\fboxsep}{8pt}%')
[void]$out.AppendLine('\fbox{\begin{tabular}{' + $colSpec + '}')
for ($r = 0; $r -lt $rows; $r++) {
    $line = @()
    for ($c = 0; $c -lt $Columns; $c++) {
        $i = $c * $rows + $r
        $line += $(if ($i -lt $cells.Count) { $cells[$i] } else { '' })
    }
    [void]$out.AppendLine(($line -join ' & ') + ' \\')
}
[void]$out.AppendLine('\end{tabular}}}')

$outPath = $ExecutionContext.SessionState.Path.GetUnresolvedProviderPathFromPSPath($OutTex)
$outDir = Split-Path -Parent $outPath
if ($outDir -and -not (Test-Path -LiteralPath $outDir)) {
    New-Item -ItemType Directory -Path $outDir | Out-Null
}
[System.IO.File]::WriteAllText($outPath, $out.ToString(), [System.Text.Encoding]::ASCII)
Write-Host "Wrote $outPath ($($dirs.Count) folders, $($files.Count) files)"
