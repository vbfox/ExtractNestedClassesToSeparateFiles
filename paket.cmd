@echo off

setlocal
set BOOTSTRAPER=%~dp0.paket\paket.bootstrapper.exe
set PAKET=%~dp0.paket\paket.exe

"%BOOTSTRAPER%" -s
if errorlevel 1 (
  set ERROR_LEVEL=%errorlevel%
  exit /b %ERROR_LEVEL%
)

"%PAKET%" %*
