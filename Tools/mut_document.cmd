@echo off
setlocal
set "SCRIPT=%~dp0mut_document\mut_document.py"
python "%SCRIPT%" %*
if %ERRORLEVEL%==9009 py "%SCRIPT%" %*
exit /b %ERRORLEVEL%
