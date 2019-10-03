@echo off

setlocal
pushd %~dp0%

if errorlevel 1 (
  endlocal
  exit /b %errorlevel%
)

powershell -noprofile -executionPolicy RemoteSigned -file "%~dp0\download-paket.ps1"
.paket\paket.exe restore
if errorlevel 1 (
  endlocal
  exit /b %errorlevel%
)

:: don't care if this fails
dotnet build-server shutdown >NUL 2>&1

dotnet tool install fake-cli --tool-path .\tools
.\tools\fake.exe build.fsx %*

if errorlevel 1 (
  endlocal
  exit /b %errorlevel%
)
endlocal
exit /b 0
