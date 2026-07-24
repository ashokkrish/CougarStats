@rem rpart.report.bat:

@"C:\PROGRA~1\R\R-4.5.2\bin\x64\R.exe" CMD BATCH --quiet --vanilla rpart.report.R
@if %errorlevel% equ 0 goto good1:
@echo R returned errorlevel %errorlevel%, see rpart.report.Rout:
@echo.
@tail rpart.report.Rout
@echo rpart.report.R
@exit /B 1
:good1
diff rpart.report.Rout rpart.report.Rout.save
@if %errorlevel% equ 0 goto good2:
@echo === Files are different ===
@diffps -s Rplots.ps ..\..\.#\test-reference\rpart.report.save.ps
@exit /B 1
:good2
@rem rpart.report.save.ps is too big to be included in the release
@rem so it is stored elsewhere
diffps Rplots.ps ..\..\.#\test-reference\rpart.report.save.ps
@if %errorlevel% equ 0 goto good3:
@echo === Files are different ===
@exit /B 1
:good3
@rm -f rpart.report.Rout
@rm -f Rplots.ps
@exit /B 0
