## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
options(max.width = 1000)
options(max.print = 100000)

## ----positive, echo=TRUE,out.width='75%', fig.align='center',fig.width=10, fig.height=6----
library(inflection)
f=function(x){(1/8*x+1/2*x^2)/(1+1/8*x+1/2*x^2)}
x=seq(0,5,0.1)
y=f(x)
# First approximation
cc=check_curve(x,y);cc
ese(x,y,cc$index)
# New interval with equal distances from first approximation
x=seq(0,1.3,0.001)
y=f(x)
# Second approximation 
cc=check_curve(x,y);cc
ipbese=bese(x,y,cc$index)
ipbese$iplast
plot(x,y,pch=19,cex=0.1)
abline(v=ipbese$iplast,col='blue')

## ----positers,echo=FALSE------------------------------------------------------
knitr::kable(ipbese$iters, caption = 'BESE')

## ----pos6digits, echo=TRUE----------------------------------------------------
# x=seq(0.68,0.69,0.0001)
# y=f(x)
# cc=check_curve(x,y);cc
# ipbese=bese(x,y,cc$index,doparallel = TRUE)
# ipbese$iplast
# # [1] 0.6883
# ipbese$iters
#     n      a      b     ESE
## 1 101 0.6800 0.6900 0.68870
## 2  25 0.6876 0.6887 0.68815
## 3  12 0.6881 0.6886 0.68835
## 4   6 0.6882 0.6884 0.68830

## ----positiveNOISE, echo=TRUE,out.width='75%', fig.align='center',fig.width=10, fig.height=6----
x=seq(0.0,3.,0.001)
set.seed(20190628)
y=f(x)+runif(length(x),-0.01,0.01)
cc=check_curve(x,y)
cc
ipbese=bese(x,y,cc$index)
ipbese$iplast
plot(x,y,pch=19,cex=0.1)
abline(v=ipbese$iplast,col='blue')

## ----positersNOISE,echo=FALSE-------------------------------------------------
knitr::kable(ipbese$iters, caption = 'BESE')

## ----bigdata, echo=TRUE,out.width='75%', fig.align='center',fig.width=10, fig.height=6----
f=function(x){500+500*tanh(x-500)}
x=seq(0,1000,0.001)
y=f(x)
length(x)
t1=Sys.time();ede(x,y,0);t2=Sys.time();t2-t1
ipbede=bede(x,y,cc$index)
ipbede$iplast

## ----bigdataiters,echo=FALSE--------------------------------------------------
knitr::kable(ipbede$iters, caption = 'BEDE')

## ----bigdataNOISE, echo=TRUE,out.width='75%', fig.align='center',fig.width=10, fig.height=6----
f=function(x){500+500*tanh(x-500)}
x=seq(0,1000,0.001)
set.seed(20190628)
y=f(x)+runif(length(x),-50,50)
length(x)
t1=Sys.time();ede(x,y,0);t2=Sys.time();t2-t1
ipbede=bede(x,y,0)
ipbede$iplast
plot(x[495000:505000],y[495000:505000],xlab="x",ylab="y",pch='.')
abline(v=ipbede$iplast)

## ----bigdataitersNOISE,echo=FALSE---------------------------------------------
knitr::kable(ipbede$iters, caption = 'BEDE')

## ----bigdataNOISEasym, echo=TRUE,out.width='75%', fig.align='center',fig.width=10, fig.height=6----
f=function(x){500+500*tanh(x-500)}
x=seq(0,700,0.001)
set.seed(20190628)
y=f(x)+runif(length(x),-50,50)
length(x)
t1=Sys.time();ede(x,y,0);t2=Sys.time();t2-t1
ipbede=bede(x,y,0)
ipbede$iplast
plot(x[495000:505000],y[495000:505000],xlab="x",ylab="y",pch='.')
abline(v=ipbede$iplast)

## ----bigdataitersNOISEasym,echo=FALSE-----------------------------------------
knitr::kable(ipbede$iters, caption = 'BEDE')

## ----positiveNLS, echo=TRUE---------------------------------------------------
# library(nlme)
# x=seq(0,5,0.1)
# f=function(x){(1/8*x+1/2*x^2)/(1+1/8*x+1/2*x^2)}
# y=f(x)
# df=data.frame("x"=x,"y"=y)
# Asym=1;xmid=1;scal=1;
# fmla=as.formula("y~SSlogis(x,Asym,xmid,scal)");fmla
# try(nls(fmla,df))
# est=try(nls(fmla,df))
# coef(est)

## ----nls1---------------------------------------------------------------------
# y ~ SSlogis(x, Asym, xmid, scal)
# Nonlinear regression model
#   model: y ~ SSlogis(x, Asym, xmid, scal)
#    data: df
#   Asym   xmid   scal 
# 0.8909 1.2516 0.5478 
#  residual sum-of-squares: 0.05242
# 
# Number of iterations to convergence: 0 
# Achieved convergence tolerance: 1.401e-06
###
# est=try(nls(fmla,df))
# coef(est)
#      Asym      xmid      scal 
# 0.8908913 1.2516410 0.5478263 
# summary(est)
# 
# Formula: y ~ SSlogis(x, Asym, xmid, scal)
# 
# Parameters:
#      Estimate Std. Error t value Pr(>|t|)    
# Asym 0.890891   0.007872  113.18   <2e-16 ***
# xmid 1.251641   0.025638   48.82   <2e-16 ***
# scal 0.547826   0.023718   23.10   <2e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.03305 on 48 degrees of freedom
# 
# Number of iterations to convergence: 0 
# Achieved convergence tolerance: 1.401e-06

## ----positiveNOISEnls, echo=TRUE----------------------------------------------
# x=seq(0.0,3.,0.001)
# f=function(x){(1/8*x+1/2*x^2)/(1+1/8*x+1/2*x^2)}
# set.seed(20190628)
# y=f(x)+runif(length(x),-0.01,0.01)
# df=data.frame("x"=x,"y"=y)
# Asym=1;xmid=1;scal=1;
# fmla=as.formula("y~SSlogis(x,Asym,xmid,scal)");fmla
# try(nls(fmla,df))

## ----nls2---------------------------------------------------------------------
# Nonlinear regression model
# model: y ~ SSlogis(x, Asym, xmid, scal)
# data: df
# Asym   xmid   scal 
# 0.8018 1.0938 0.4318 
# residual sum-of-squares: 1.884
# 
# Number of iterations to convergence: 0 
# Achieved convergence tolerance: 1.768e-07
###
# est=try(nls(fmla,df))
# coef(est)
#      Asym      xmid      scal 
# 0.8018405 1.0937758 0.4318175 
# summary(est)
# 
# Formula: y ~ SSlogis(x, Asym, xmid, scal)
# 
# Parameters:
#      Estimate Std. Error t value Pr(>|t|)    
# Asym 0.801840   0.001175   682.7   <2e-16 ***
# xmid 1.093776   0.002417   452.5   <2e-16 ***
# scal 0.431817   0.002063   209.3   <2e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.02507 on 2998 degrees of freedom
# 
# Number of iterations to convergence: 0 
# Achieved convergence tolerance: 1.768e-07

## ----bigdatanls, echo=TRUE----------------------------------------------------
# f=function(x){500+500*tanh(x-500)}
# x=seq(0,1000,0.001)
# y=f(x)
# length(x)
# df=data.frame("x"=x,"y"=y)
# Asym=1000;xmid=500;scal=1;
# fmla=as.formula("y~SSlogis(x,Asym,xmid,scal)");fmla
#
# t1=Sys.time();
# try(nls(fmla,df))
# Error in nls(y ~ 1/(1 + exp((xmid - x)/scal)), data = xy, start = list(xmid = aux[[1L]],  :
#   step factor 0.000488281 reduced below 'minFactor' of 0.000976562
# t2=Sys.time();t2-t1
# Time difference of 19.29784 secs

