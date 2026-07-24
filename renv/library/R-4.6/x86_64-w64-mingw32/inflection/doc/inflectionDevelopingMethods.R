## ----setup, include=FALSE-----------------------------------------------------
library(inflection)
knitr::opts_chunk$set(echo = TRUE)
options(max.width = 1000)
options(max.print = 100000)

## ----tanhESE, echo=TRUE,out.width='75%', fig.align='center',fig.width=10, fig.height=6----
library(inflection)
data("table_01")
x=table_01$x
y=table_01$y
plot(x,y,cex=0.3,pch=19)
grid()
bb=ese(x,y,0);bb
pese=bb[,3];pese
abline(v=pese)
cc=bese(x,y,0)
cc$iplast
abline(v=cc$iplast,col='blue')
knitr::kable(cc$iters, caption = 'BESE')

## ----tanhESEnoisy, echo=TRUE,out.width='75%', fig.align='center',fig.width=10, fig.height=6----
library(inflection)
data("table_02")
x=table_01$x
y=table_01$y
plot(x,y,cex=0.3,pch=19)
grid()
bb=ese(x,y,0);bb
pese=bb[,3];pese
abline(v=pese)
cc=bese(x,y,0)
cc$iplast
abline(v=cc$iplast,col='blue')
knitr::kable(cc$iters, caption = 'BESE')

## ----tanhESEasym, echo=TRUE---------------------------------------------------
data("table_03_04")
x=table_03_04$x
y=table_03_04$y
tese=ese(x,y,0);tese
pese=tese[,3]
tede=ede(x,y,0);tede
pede=tede[,3]
cc=bese(x,y,0)
cc$iplast
dd=bede(x,y,0)
dd$iplast

## ----tanhESEasymPLOT, echo=FALSE,out.width='75%', fig.align='center',fig.width=10, fig.height=6----
plot(x,y,cex=0.3,pch=19)
grid()
abline(v=pese)
abline(v=cc$iplast,col='blue')
abline(v=dd$iplast,col='red')
knitr::kable(cc$iters, caption = 'BESE')
knitr::kable(dd$iters, caption = 'BEDE')

## ----tanhESEasymNOISE, echo=TRUE,out.width='75%', fig.align='center',fig.width=10, fig.height=6----
data("table_05_06")
x=table_05_06$x
y=table_05_06$y
tese=ese(x,y,0);tese
pese=tese[,3]
tede=ede(x,y,0);tede
pede=tede[,3]
cc=bese(x,y,0)
cc$iplast
dd=bede(x,y,0)
dd$iplast
plot(x,y,cex=0.3,pch=19)
grid()
abline(v=pese)
abline(v=cc$iplast,col='blue')
abline(v=dd$iplast,col='red')
knitr::kable(cc$iters, caption = 'BESE')
knitr::kable(dd$iters, caption = 'BEDE')

## ----Gomb, echo=TRUE----------------------------------------------------------
data("table_08_09")
x=table_08_09$x
y=table_08_09$y
tese=ese(x,y,0);tese
pese=tese[,3]
tede=ede(x,y,0);tede
pede=tede[,3]
cc=bese(x,y,0)
cc$iplast
dd=bede(x,y,0)
dd$iplast

## ----GombPLOT, echo=FALSE,out.width='75%', fig.align='center',fig.width=10, fig.height=6----
plot(x,y,cex=0.3,pch=19)
grid()
abline(v=pese)
abline(v=cc$iplast,col='blue')
abline(v=dd$iplast,col='red')
knitr::kable(cc$iters, caption = 'BESE')
knitr::kable(dd$iters, caption = 'BEDE')

## ----GombNOISE, echo=TRUE-----------------------------------------------------
data("table_10_11")
x=table_08_09$x
y=table_08_09$y
tese=ese(x,y,0);tese
pese=tese[,3]
tede=ede(x,y,0);tede
pede=tede[,3]
cc=bese(x,y,0)
cc$iplast
dd=bede(x,y,0)
dd$iplast

## ----GombPLOTNOISE, echo=FALSE,out.width='75%', fig.align='center',fig.width=10, fig.height=6----
plot(x,y,cex=0.3,pch=19)
grid()
abline(v=pese)
abline(v=cc$iplast,col='blue')
abline(v=dd$iplast,col='red')
knitr::kable(cc$iters, caption = 'BESE')
knitr::kable(dd$iters, caption = 'BEDE')

## ----poly3sym, echo=TRUE,out.width='75%', fig.align='center',fig.width=10, fig.height=6----
data("table_13")
x=table_13$x
y=table_13$y
plot(x,y,cex=0.3,pch=19)
grid()
bb=ese(x,y,0);bb
pese=bb[,3];pese
abline(v=pese)

## ----poly3symNOISE, echo=TRUE,out.width='75%', fig.align='center',fig.width=10, fig.height=6----
data("table_14_15")
x=table_14_15$x
y=table_14_15$y
plot(x,y,cex=0.3,pch=19)
grid()
bb=ese(x,y,0);bb
pese=bb[,3];pese
abline(v=pese)
cc=bese(x,y,0)
cc$iplast
abline(v=cc$iplast,col='blue')
knitr::kable(cc$iters, caption = 'BESE')

## ----poly3asym, echo=TRUE,out.width='75%', fig.align='center',fig.width=10, fig.height=6----
data("table_17_18")
x=table_17_18$x
y=table_17_18$y
bb=ese(x,y,0);bb
pese=bb[,3];pese
plot(x,y,cex=0.3,pch=19)
grid()
cc=bese(x,y,0)
cc$iplast
dd=bede(x,y,0)
dd$iplast
abline(v=pese)
abline(v=cc$iplast,col='blue')
abline(v=dd$iplast,col='red')
knitr::kable(cc$iters, caption = 'BESE')
knitr::kable(dd$iters, caption = 'BEDE')

## ----poly3asymNOISE, echo=TRUE,out.width='75%', fig.align='center',fig.width=10, fig.height=6----
data("table_19_20")
x=table_19_20$x
y=table_19_20$y
bb=ese(x,y,0);bb
pese=bb[,3];pese
plot(x,y,cex=0.3,pch=19)
grid()
cc=bese(x,y,0)
cc$iplast
dd=bede(x,y,0)
dd$iplast
abline(v=pese)
abline(v=cc$iplast,col='blue')
abline(v=dd$iplast,col='red')
knitr::kable(cc$iters, caption = 'BESE')
knitr::kable(dd$iters, caption = 'BEDE')

