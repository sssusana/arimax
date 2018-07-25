#Known example to train intervention models


#libraries
rm(list=ls())
library(foreign)
library(TSA)
library(forecast)

#load data & view series
bush <- read.table("bushjob.txt",header=TRUE)
plot.ts(bush$approve)

plot(y=bush$approve, x=bush$t, type='l')


#Identifying arima
tsdisplay(bush$approve)
ar1<- arima(bush$approve, order = c(1,0,0))
ar1

tsdisplay(ar1$residuals)

plot.ts(bush$approve)
lines(bush$approve-ar1$residuals, col="red")
legend("topright",c("Bush approve","Fitted values"),col=1:2,lty=1,ncol=1,cex=0.8)


head(bush, n=20)

##Arima with transfer from bush$s11

mod.2=TSA::arimax(bush$approve, order=c(1,0,0), xtransf=bush$s11, transfer=list(c(1,0))
)
summary(mod.2)


##Let's plot the intervention model

#Coefficients:
#      ar1    intercept T1-AR1   T1-MA0
#     0.8562   56.0327  0.8984  27.6660
#s.e. 0.2028    5.7056  0.0197   4.6373

#delta0*Pt; pulse function is 0; before t0 (MA part)
bush$z1<-27.6660*bush$s11

# t0 and past t0 periods (AR part)
for (t in 10:50){bush$z1[t]<-0.8984*bush$z1[t-1]}

#adds the intercept
bush$z1<-bush$z1+56.0327


##Now plotting the data and the intervention model (only)
plot.ts(bush$approve)
lines(bush$z1, lty=2,col="red")
legend("topright",c("Bush approve","Intervention model"),col=1:2,lty=1,ncol=1,cex= 0.8)


