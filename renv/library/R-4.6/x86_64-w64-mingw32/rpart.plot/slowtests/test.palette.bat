@rem test.palette.bat:

@"C:\PROGRA~1\R\R-4.5.2\bin\x64\R.exe" CMD BATCH --quiet --vanilla test.palette.R
@if %errorlevel% equ 0 goto good1:
@echo R returned errorlevel %errorlevel%, see test.palette.Rout:
@echo.
@tail test.palette.Rout
@echo test.palette.R
@exit /B 1
:good1
diff test.palette.Rout test.palette.Rout.save
@if %errorlevel% equ 0 goto good2:
@echo === Files are different ===
@diffps -s Rplots.ps ..\..\.#\test-reference\test.palette.save.ps
@exit /B 1
:good2
@rem test.palette.save.ps is too big to be included in the release
@rem so it is stored elsewhere
diffps Rplots.ps ..\..\.#\test-reference\test.palette.save.ps
@if %errorlevel% equ 0 goto good3:
@echo === Files are different ===
@exit /B 1
:good3
@rm -f test.palette.Rout
@rm -f Rplots.ps
@exit /B 0
