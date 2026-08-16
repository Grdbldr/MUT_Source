copy "x64\Release\Modflow-User-Tools.exe" "%USERBIN%\mut.exe"
copy "x64\Release\Modflow-User-Tools.exe" C:\_repo\Grdbldr\MUT_Examples\_MUT_USERBIN\mut.exe"
robocopy "Tools\mut_document" "%USERBIN%\mut_document" /E /XD __pycache__ /XF *.pyc /NFL /NDL /NJH /NJS
robocopy "Tools\mut_document" "C:\_repo\Grdbldr\MUT_Examples\_MUT_USERBIN\mut_document" /E /XD __pycache__ /XF *.pyc /NFL /NDL /NJH /NJS
exit /b 0
