@rem test.rpart.rules.bat:

@"C:\PROGRA~1\R\R-4.5.2\bin\x64\R.exe" CMD BATCH --quiet --vanilla test.rpart.rules.R
@if %errorlevel% equ 0 goto good1:
@echo R returned errorlevel %errorlevel%, see test.rpart.rules.Rout:
@echo.
@tail test.rpart.rules.Rout
@echo test.rpart.rules.R
@exit /B 1
:good1
diff test.rpart.rules.Rout test.rpart.rules.Rout.save
@if %errorlevel% equ 0 goto good2:
@echo === Files are different ===
@diffps -s Rplots.ps ..\..\.#\test-reference\test.rpart.rules.save.ps
@exit /B 1
:good2
@rem test.rpart.rules.save.ps is too big to be included in the release
@rem so it is stored elsewhere
diffps Rplots.ps ..\..\.#\test-reference\test.rpart.rules.save.ps
@if %errorlevel% equ 0 goto good3:
@echo === Files are different ===
@exit /B 1
:good3
@rm -f test.rpart.rules.Rout
@rm -f Rplots.ps
@exit /B 0
