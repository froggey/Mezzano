@echo off
:: Usage: ./make-asdf.bat [keyword=argument ...] <command>
:: See the Makefile for the list of acceptable keyword arguments

set here=%~dp0
set header_lisp=header.lisp
set driver_lisp=uiop\package.lisp + uiop\common-lisp.lisp + uiop\utility.lisp + uiop\version.lisp + uiop\os.lisp + uiop\pathname.lisp + uiop\filesystem.lisp + uiop\stream.lisp + uiop\image.lisp + uiop\lisp-build.lisp + uiop\launch-program.lisp + uiop\run-program.lisp + uiop\configuration.lisp + uiop\backward-driver.lisp + uiop\driver.lisp
set defsystem_lisp=upgrade.lisp + session.lisp + component.lisp + operation.lisp + system.lisp + system-registry.lisp + action.lisp + lisp-action.lisp + find-component.lisp + forcing.lisp + plan.lisp + operate.lisp + find-system.lisp + parse-defsystem.lisp + bundle.lisp + concatenate-source.lisp + package-inferred-system.lisp + output-translations.lisp + source-registry.lisp + backward-internals.lisp + backward-interface.lisp + interface.lisp + user.lisp + footer.lisp

%~d0
cd "%~p0"

if "%~1"=="" goto all
if "%~1"=="all" goto all
if "%~1"=="build_asdf" goto build_asdf
if "%~1"=="build_asdf_tools" goto build_asdf_tools
if "%~1"=="ext" goto ext
if "%~1"=="noext" goto noext
if "%~1"=="driver_files" goto driver_files
if "%~1"=="defsystem_files" goto defsystem_files

call "%0" build_asdf_tools
"%here%\build\asdf-tools.exe" env %*
goto end


:all
:: Default action: bootstrap asdf.lisp

:build_asdf
:: That's the only thing that we really need before we may invoke the asdf-tools
 if not exist build mkdir build
 set a=build\asdf.lisp
 copy /y /b %header_lisp% + %driver_lisp% + %defsystem_lisp% %a%.tmp > nul
 if not exist %a% goto clobber
 fc /b /0 %a%.tmp %a% > nul
 if errorlevel 1 goto clobber
 del /f /q %a%.tmp
 goto end

:build_asdf_tools
:: Building a binary for asdf-tools
 if exist build\asdf-tools.exe goto end
 call "%0" build_asdf
 "%here%\tools\asdf-tools.bat" build-asdf-tools
 goto end

:clobber
 if exist %a% del /f /q %a%
 rename %a%.tmp asdf.lisp
 goto end

:ext
:: Download all the development-time dependencies of ASDF:
 git submodule update --init
 goto end

:noext
:: Remove all the development-time dependencies of ASDF:
 git submodule deinit .
 goto end

:driver_files
:: These targets are used during tests to ensure the Makefile is in synch with the .asd files.
 echo %driver_lisp%
 goto end

:defsystem_files
:: These targets are used during tests to ensure the Makefile is in synch with the .asd files.
 echo %defsystem_lisp%
 goto end

:end
