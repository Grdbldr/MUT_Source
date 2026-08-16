@echo off
cd /d "%~dp0User's Guide"
set TEX=MUT User's Guide.tex
set BASE=MUT User's Guide
pdflatex -interaction=nonstopmode "%TEX%"
if errorlevel 1 exit /b 1
makeindex "%BASE%"
if errorlevel 1 exit /b 1
pdflatex -interaction=nonstopmode "%TEX%"
if errorlevel 1 exit /b 1
pdflatex -interaction=nonstopmode "%TEX%"
if errorlevel 1 exit /b 1
echo Build complete.
