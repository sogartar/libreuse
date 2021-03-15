@echo off

SET SCRIPT_DIR=%~dp0
SET WORKING_DIR=%cd%
cd "%SCRIPT_DIR%"
7z a "%WORKING_DIR%\mod_libreuse.zip" scripts || goto error
exit 0

:error
echo Failed with error #%errorlevel%.
exit /b %errorlevel%
