@echo off
setlocal
rem Runs the MUT Model Builder pipeline headless with QGIS's Python (override with QGIS_BIN).
if not defined QGIS_BIN set "QGIS_BIN=C:\Program Files\QGIS 3.44.15\bin"
set "SCRIPT=%~dp0qgis_mut_builder\run_headless.py"
call "%QGIS_BIN%\python-qgis-ltr.bat" "%SCRIPT%" %*
exit /b %ERRORLEVEL%
