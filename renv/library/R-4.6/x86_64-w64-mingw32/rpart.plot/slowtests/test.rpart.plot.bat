@rem test.rpart.plot.bat: this does a regression test of rpart.plot and rpart.plot
@rem Stephen Milborrow Nov 2010 Gardens, Cape Town

@"C:\PROGRA~1\R\R-4.5.2\bin\x64\R.exe" CMD BATCH --quiet --vanilla test.rpart.plot.R
@if %errorlevel% equ 0 goto good1:
@echo R returned errorlevel %errorlevel%, see test.rpart.plot.Rout:
@echo.
@tail test.rpart.plot.Rout
@echo test.rpart.plot.R
@exit /B 1
:good1
diff test.rpart.plot.Rout test.rpart.plot.Rout.save
@if %errorlevel% equ 0 goto good2:
@echo === Files are different ===
@diffps -s Rplots.ps ..\..\.#\test-reference\test.rpart.plot.save.ps
@exit /B 1
:good2
@rem test.rpart.plot.save.ps is too big to be included in the release
@rem so it is stored elsewhere
diffps Rplots.ps ..\..\.#\test-reference\test.rpart.plot.save.ps
@if %errorlevel% equ 0 goto good3:
@echo === Files are different ===
@exit /B 1
:good3
@rm -f test.rpart.plot.Rout
@rm -f Rplots.ps
@exit /B 0
