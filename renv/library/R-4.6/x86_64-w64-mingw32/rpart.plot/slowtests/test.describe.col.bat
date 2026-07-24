@rem test.describe.col.bat:

@"C:\PROGRA~1\R\R-4.5.2\bin\x64\R.exe" CMD BATCH --quiet --vanilla test.describe.col.R
@if %errorlevel% equ 0 goto good1:
@echo R returned errorlevel %errorlevel%, see test.describe.col.Rout:
@echo.
@tail test.describe.col.Rout
@echo test.describe.col.R
@exit /B 1
:good1
diff test.describe.col.Rout test.describe.col.Rout.save
@if %errorlevel% equ 0 goto good2:
@echo === Files are different ===
@diffps -s Rplots.ps ..\..\.#\test-reference\test.describe.col.save.ps
@exit /B 1
:good2
@rem test.describe.col.save.ps is too big to be included in the release
@rem so it is stored elsewhere
diffps Rplots.ps ..\..\.#\test-reference\test.describe.col.save.ps
@if %errorlevel% equ 0 goto good3:
@echo === Files are different ===
@exit /B 1
:good3
@rm -f test.describe.col.Rout
@rm -f Rplots.ps
@exit /B 0
