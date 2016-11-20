@echo off
setlocal

if defined KNNENV (
  goto :eof
)

call :SetLAPACKPATHFILE "%~dp0../.stack-work/lapack-path.txt"

pushd "%~dp0"
if not exist "%LAPACKPATHFILE%" (
  stack exec -- sh get-lapack-path.sh > "%LAPACKPATHFILE%"
)

set /p LAPACKPATH=<"%LAPACKPATHFILE%"

rem Export PATH to global scope
for /f "delims=" %%i in ('echo "%LAPACKPATH%;%PATH%"') do endlocal & set KNNENV=1 & set PATH=%%~i

goto :eof

:SetLAPACKPATHFILE
set LAPACKPATHFILE=%~f1
goto :eof
