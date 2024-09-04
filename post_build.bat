robocopy Database\  %USERBIN% *.txt *.accdb
robocopy Database\  C:\Repos\Grdbldr\MUT_Examples\_MUT_USERBIN *.txt *.accdb
copy "x64\Debug\Modflow-User-Tools.exe" "%USERBIN%\mut.exe"
