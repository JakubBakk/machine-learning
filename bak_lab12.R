library("astsa")
flu_dane <- flu

#sam wykres
par(mar=c(4,4,1,1)+0.1, mgp=c(3,0.6,0),las=1)
plot(flu_dane,type="l",col="SteelBlue")

#trendy1
par(mfcol=c(1,2),mar=c(4,4,1,1)+0.1, mgp=c(3,0.6,0),las=1)
plot(log(flu_dane),col="SteelBlue"); title("addytywny")
plot(flu_dane,col="SteelBlue"); title("multiplikatywwny")

#jednokrotne ró¿nicowanie szeregów czasowych
par(mfcol=c(1,2),mar=c(4,4,1,1)+0.1, mgp=c(3,0.6,0),las=1)
plot(diff(log(flu_dane)),col="SteelBlue"); title("addytywny")
plot(diff(flu_dane),col="SteelBlue"); title("multiplikatywwny")

#dwukrotne ró¿nicowanie szeregów czasowych
par(mfcol=c(1,2),mar=c(4,4,1,1)+0.1, mgp=c(3,0.6,0),las=1)
plot(diff(log(flu_dane), lag=1, differences = 2),col="SteelBlue"); title("addytywny")
plot(diff(flu_dane, lag=1, differences = 2),col="SteelBlue"); title("multiplikatywwny")

#filtrowanie filtrem Hodricka-Prescotta oraz Cyclical component
library(FRAPO)
f <- FRAPO::trdhp(flu_dane, lambda=14400)

par(mfcol=c(2,1),mar=c(4,4,1,1)+0.1, mgp=c(3,0.6,0),las=1)
plot(flu_dane,col="SteelBlue",
     main="Hodrick-Prescott filter")

lines(f,col="YellowGreen")
plot(flu_dane-f,col="SteelBlue",
     main="Cyclical component (deviations from trend)")


#Funkcja autokorelacji - ACF
library(forecast)
forecast::tsdisplay(flu_dane,col=2,lwd=2,las=1)
forecast::tsdisplay(diff(flu_dane),col=2,lwd=2,las=1)
plot(stl(flu_dane,s.window="periodic"),col=2,lwd=2)

#Modele autoregresyjne ARIMA
m <- forecast::auto.arima(flu_dane,d=1)
summary(m)
r <- resid(m)
p <- sapply(1:10,function(i) Box.test(r, i, type = "Ljung-Box")$p.value)
p
par(mar=c(4,4,1,1)+0.1, mgp=c(3,0.6,0),las=1)
tsdiag(m)
forecast::forecast(m,h=12)
par(mar=c(4,4,1,1)+0.1, mgp=c(3,0.6,0),las=1)
#Prognozowanie
plot(forecast::forecast(m,h=12))
p <- predict(m,n.ahead=12)
ts( cbind(pred=p$pred, se=p$se, error=p$se/p$pred), start=c(1979,1),freq=12)