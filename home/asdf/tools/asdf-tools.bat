::@echo off

::: By default. We use CCL, because SBCL doesn't have a good enough run-program on Windows.
if "%LISP%" == "" goto ccl
if "%LISP%" == "allegro" goto allegro
if "%LISP%" == "ccl" goto ccl
if "%LISP%" == "sbcl" goto sbcl


:ccl
if "%CCL%" == "" set CCL=ccl
"%CCL%" --no-init --load "%~dp0asdf-tools" -- %*
goto end


:sbcl
::: As of SBCL 1.2.13, SBCL's run-program fails to call CMD.EXE directly, so can't fully run asdf-tools
if "%SBCL%" == "" set SBCL=sbcl
"%SBCL%" --noinform --no-userinit --no-sysinit --script "%~dp0asdf-tools" %*
goto end


:allegro
if "%ALLEGRO%" == "" set ALLEGRO=alisp.exe
if "%~1" == "get_allegro_dir" goto get_allegro_dir
call %0 get_allegro_dir "%ALLEGRO%" "%ALLEGRO%.exe"
"%ALLEGRODIR%buildi.exe" -I "%ALLEGRODIR%alisp.dxl" -qq -e "(setf *load-verbose* nil)" -L "%~dp0asdf-tools." -- %*
goto end
:get_allegro_dir
if not "%~dp$PATH:2" == "" ( set ALLEGRODIR=%~dp$PATH:2& goto end )
if not "%~dp$PATH:3" == "" ( set ALLEGRODIR=%~dp$PATH:3& goto end )
if not "%~dp2" == "" ( set ALLEGRODIR=%~dp2& goto end )
goto end


::: Make sure this remains at the end
:end
