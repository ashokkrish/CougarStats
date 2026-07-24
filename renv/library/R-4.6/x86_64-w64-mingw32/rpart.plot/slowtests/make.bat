@rem rpart.plot/inst/slowtests/make.bat

call test.rpart.plot.bat
                        @if %errorlevel% NEQ 0 goto err
call test.rpart.rules.bat
                        @if %errorlevel% NEQ 0 goto err
call vignette.bat
                        @if %errorlevel% NEQ 0 goto err
call rpart.report.bat
                        @if %errorlevel% NEQ 0 goto err
call usersplits.bat
                        @if %errorlevel% NEQ 0 goto err
call webpage.figs.bat
                        @if %errorlevel% NEQ 0 goto err
call test.describe.col.bat
                        @if %errorlevel% NEQ 0 goto err
call test.palette.bat
                        @if %errorlevel% NEQ 0 goto err
call test.na.bat
                        @if %errorlevel% NEQ 0 goto err

@rem TODO R 4.2.0: Removed following because "Warning: package 'DStree' is not available for this version of R"
@rem call test.imports.bat
@rem                         @if %errorlevel% NEQ 0 goto err

call test.type5.bat
                        @if %errorlevel% NEQ 0 goto err
@goto done
:err
@echo ==== ERROR ====
:done
@exit /B  0
