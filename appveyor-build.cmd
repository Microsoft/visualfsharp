@echo on

set APPVEYOR_CI=1

:: Check prerequisites
if not '%VisualStudioVersion%' == '' goto vsversionset
if exist "%ProgramFiles(x86)%\Microsoft Visual Studio 14.0\common7\ide\devenv.exe" set VisualStudioVersion=14.0
if not '%VisualStudioVersion%' == '' goto vsversionset
if exist "%ProgramFiles(x86)%\Microsoft Visual Studio 12.0\common7\ide\devenv.exe" set VisualStudioVersion=12.0
:vsversionset
if '%VisualStudioVersion%' == '' echo Error: Could not find a Visual Studio Installed.MSBuild.exe.  Please see http://www.visualstudio.com/en-us/news/vs2015-vs.aspx. && goto :eof

if '%VisualStudioVersion%'=='14.0' set _msbuildexe="%ProgramFiles(x86)%\MSBuild\14.0\Bin\MSBuild.exe"
if '%VisualStudioVersion%'=='12.0' set _msbuildexe="%ProgramFiles(x86)%\MSBuild\12.0\Bin\MSBuild.exe"
if not exist %_msbuildexe% echo Error: Could not find MSBuild.exe.  Please see http://www.microsoft.com/en-us/download/details.aspx?id=40760. && goto :eof

set _ngenexe="%SystemRoot%\Microsoft.NET\Framework\v4.0.30319\ngen.exe"
if not exist %_ngenexe% echo Error: Could not find ngen.exe. && goto :eof

.\.nuget\NuGet.exe restore packages.config -PackagesDirectory packages
@if ERRORLEVEL 1 echo Error: Nuget restore failed  && goto :eof

:: Build
%_msbuildexe% src\fsharp-proto-build.proj
@if ERRORLEVEL 1 echo Error: compiler proto build failed && goto :eof

%_ngenexe% install proto\net40\bin\FSharp.Compiler-proto.dll
%_ngenexe% install proto\net40\bin\fsharp.core.dll
%_ngenexe% install proto\net40\bin\FSharp.Build-proto.dll
%_ngenexe% install Proto\net40\bin\fsc-proto.exe
@if ERRORLEVEL 1 echo Error: NGen of proto failed && goto :eof

%_msbuildexe% src/fsharp-library-build.proj /p:Configuration=Release
@if ERRORLEVEL 1 echo Error: library release build failed && goto :eof

%_msbuildexe% src/fsharp-compiler-build.proj /p:Configuration=Release
@if ERRORLEVEL 1 echo Error: compile Release build failed && goto :eof

%_msbuildexe% src/fsharp-library-build.proj /p:TargetFramework=portable47 /p:Configuration=Release
@if ERRORLEVEL 1 echo Error: library portable47 build failed && goto :eof

%_msbuildexe% src/fsharp-library-build.proj /p:TargetFramework=portable7 /p:Configuration=Release
@if ERRORLEVEL 1 echo Error: library portable7 build failed && goto :eof

%_msbuildexe% src/fsharp-library-build.proj /p:TargetFramework=portable78 /p:Configuration=Release
@if ERRORLEVEL 1 echo Error: library portable78 build failed && goto :eof

%_msbuildexe% src/fsharp-library-build.proj /p:TargetFramework=portable259 /p:Configuration=Release
@if ERRORLEVEL 1 echo Error: library portable259 build failed && goto :eof
%_msbuildexe% src/fsharp-compiler-unittests-build.proj /p:Configuration=Release
@if ERRORLEVEL 1 echo Error: compiler unittests build failed && goto :eof

%_msbuildexe% src/fsharp-library-unittests-build.proj /p:Configuration=Release
@if ERRORLEVEL 1 echo Error: library unittests build failed && goto :eof

%_msbuildexe% src/fsharp-library-unittests-build.proj /p:TargetFramework=portable47 /p:Configuration=Release
@if ERRORLEVEL 1 echo Error: library unittests build failed portable47 && goto :eof

%_msbuildexe% src/fsharp-library-unittests-build.proj /p:TargetFramework=portable7 /p:Configuration=Release
@if ERRORLEVEL 1 echo Error: library unittests build failed portable7 && goto :eof

%_msbuildexe% src/fsharp-library-unittests-build.proj /p:TargetFramework=portable78 /p:Configuration=Release
@if ERRORLEVEL 1 echo Error: library unittests build failed portable78 && goto :eof

%_msbuildexe% src/fsharp-library-unittests-build.proj /p:TargetFramework=portable259 /p:Configuration=Release
@if ERRORLEVEL 1 echo Error: library unittests build failed portable259 && goto :eof

%_msbuildexe% vsintegration\fsharp-vsintegration-build.proj /p:TargetFramework=net40 /p:Configuration=Release
@if ERRORLEVEL 1 echo Error: library unittests build failed vsintegration-unittests && goto :eof

%_msbuildexe% vsintegration\fsharp-vsintegration-unittests-build.proj /p:TargetFramework=net40 /p:Configuration=Release
@if ERRORLEVEL 1 echo Error: library unittests build failed vsintegration-unittests && goto :eof

@echo on
call src\update.cmd release -ngen

@echo on
call tests\BuildTestTools.cmd release 
@if ERRORLEVEL 1 echo Error: 'tests\BuildTestTools.cmd release' failed && goto :eof

@echo on
pushd tests

call RunTests.cmd release fsharp Smoke
@if ERRORLEVEL 1 echo Error: 'RunTests.cmd release fsharp Smoke' failed && goto :eof

call RunTests.cmd release fsharpqa Smoke
@if ERRORLEVEL 1 echo Error: 'RunTests.cmd release fsharpqa Smoke' failed && goto :eof

call RunTests.cmd release compilerunit
@if ERRORLEVEL 1 echo Error: 'RunTests.cmd release compilerunit' failed && goto :eof
call RunTests.cmd release coreunit
@if ERRORLEVEL 1 echo Error: 'RunTests.cmd release coreunit' failed && goto :eof

popd
