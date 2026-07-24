@rem test.na.bat:

@"C:\PROGRA~1\R\R-4.5.2\bin\x64\R.exe" CMD BATCH --quiet --vanilla test.na.R
@if %errorlevel% equ 0 goto good1:
@echo R returned errorlevel %errorlevel%, see test.na.Rout:
@echo.
@tail test.na.Rout
@echo test.na.R
@exit /B 1
:good1
diff test.na.Rout test.na.Rout.save
@if %errorlevel% equ 0 goto good2:
@echo === Files are different ===
@diffps -s Rplots.ps ..\..\.#\test-reference\test.na.save.ps
@exit /B 1
:good2
@rem test.na.save.ps is too big to be included in the release
@rem so it is stored elsewhere
diffps Rplots.ps ..\..\.#\test-reference\test.na.save.ps
@if %errorlevel% equ 0 goto good3:
@echo === Files are different ===
@exit /B 1
:good3
@rm -f test.na.Rout
@rm -f Rplots.ps
@exit /B 0
