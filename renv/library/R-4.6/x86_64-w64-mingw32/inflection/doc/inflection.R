## ----setup, include=FALSE-----------------------------------------------------
library(inflection)
knitr::opts_chunk$set(echo = TRUE)
options(max.width = 1000)
options(max.print = 100000)

## ----tanhESE, echo=TRUE,out.width='75%', fig.align='center',fig.width=10, fig.height=6----
f=function(x){5+5*tanh(x-5)}
x=seq(0,10,by=0.05)
y=f(x)
plot(x,y,cex=0.3,pch=19)
grid()
bb=ese(x,y,0);bb
pese=bb[,3];pese
abline(v=pese)
cc=bese(x,y,0)
cc$iplast
abline(v=cc$iplast,col='blue')
knitr::kable(cc$iters, caption = 'BESE')

## ----tanhEDE, echo=TRUE,out.width='75%', fig.align='center',fig.width=10, fig.height=6----
x=seq(0,10,by=0.05)
y=f(x)
plot(x,y,cex=0.3,pch=19)
grid()
dd=ede(x,y,0);dd
pede=dd[,3];pede
abline(v=pede)
ee=bede(x,y,0)
ee$iplast
abline(v=cc$iplast,col='red')
knitr::kable(ee$iters, caption = 'BEDE')

## ----tanhESEright, echo=TRUE,out.width='75%', fig.align='center',fig.width=10, fig.height=6----
x=seq(3,10,by=0.05)
y=f(x)
plot(x,y,cex=0.5,pch=19)
abline(v=5,lty=2)
grid()
bb=ese(x,y,0);bb
pese=bb[,3];pese
abline(v=pese)
cc=bese(x,y,0)
cc$iplast
abline(v=cc$iplast,col='blue')
knitr::kable(cc$iters, caption = 'BESE')

## ----tanhEDEright, echo=TRUE,out.width='75%', fig.align='center',fig.width=10, fig.height=6----
x=seq(3,10,by=0.05)
y=f(x)
plot(x,y,cex=0.5,pch=19)
abline(v=5,lty=2)
grid()
dd=ede(x,y,0);dd
pede=dd[,3];pede
abline(v=pede)
ee=bede(x,y,0)
ee$iplast
abline(v=cc$iplast,col='red')
knitr::kable(ee$iters, caption = 'BEDE')

