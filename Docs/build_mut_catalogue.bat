@echo off
setlocal
cd /d "%~dp0.."
python Tools\mut_catalogue\mut_catalogue.py
exit /b %ERRORLEVEL%
