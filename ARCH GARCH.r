#install beberapa packages yang dibutuhkan
install.packages("prettydoc")
install.packages("rmdformats")
install.packages("fGarch")
install.packages("aTSA")
install.packages("FinTS") 
install.packages("lmtest")
install.packages("forecast")
install.packages("TSA")
install.packages("tseries")
install.packages("xts")
install.packages("openxlsx")
install.packages("tidyverse")
install.packages("dygraphs")
#test

#memakai library yang telah diinstall
library("prettydoc")
library("rmdformats")
library("fGarch")
library("aTSA")
library("FinTS") 
library("lmtest")
library("forecast")
library("TSA")
library("tseries")
library("xts")
library("openxlsx")
library("tidyverse")
library("dygraphs")

#mengambil data dari file
url = "https://github.com/MoDHer/SP500_2019-2022/blob/main/SP500_2019-2022.xlsx?raw=true"
sp500 = read.xlsx(url)
#membuat variable time series
close = sp500$Close
tsclose = ts(close)
view(tsclose)

#melihat plot dan deteksi stasioneritas
ts.plot(tsclose)
adf.test(tsclose) #hasil tidak stasioner
acf(tsclose)
pacf(tsclose)
#transformasi differencing 1, karena Box-cox (lambda=1) yang telah diuji pada Minitab 18
diff1_close = diff(tsclose)
ts.plot(diff1_close)
adf.test(diff1_close)
acf(diff1_close)
pacf(diff1_close)
diff2_close = diff(diff1_close)
acf(diff2_close)
pacf(diff2_close)
eacf(diff2_close)

#Minitab 18 menunjukan model ARIMA(1, 2, 2) adalah model terbaik
#dengan parameter yang signikan, white noise, dan MSE terkecil
model_021 = arima(diff2_close, order = c(0,0,1), include.mean = FALSE)
model_022 = arima(diff2_close, order = c(0,0,2), include.mean = FALSE)
model_120 = arima(diff2_close, order = c(1,0,0), include.mean = FALSE)
model_121 = arima(diff2_close, order = c(1,0,1), include.mean = FALSE)
model_122 = arima(diff2_close, order = c(1,0,2), include.mean = FALSE)

#Diagnosa
coeftest(model_021)
coeftest(model_022)
coeftest(model_120)
coeftest(model_121)
coeftest(model_122)
#parameter sinifikan adalah model ARI(1,2) dan IMA(2,1)

#White noise
Box.test(model_120$residuals,type = "Ljung") #hasil tidak white noise
Box.test(model_021$residuals,type = "Ljung") #hasil white noise

#Uji Box-Ljung
AutocorTest(diff2_close) #data tidak normal

#Menguji kenormalan residual
jarque.bera.test(residuals(model_021)) #residual tidak normal

#menguji efek arch
arch.test(model_021)
ArchTest(diff2_close) #ada efek heteroskedastisitas
for(i in 1:12)
{
    AT = ArchTest(diff2_close, lags = i, demean = TRUE)
    cat("P Value LM Test lag ke", i , "adalah", AT$p.value, "\n")
}
#P-Value < 0,05 menunjukkan ada efek ARCH

#Melanjutkan ke GARCH karena residual tidak normal
model_garch_10 = garchFit(~arma(0,1)+garch(1,0), data = diff2_close, trace = F)
summary(model_garch_10)

model_garch_11 = garchFit(~arma(0,1)+garch(1,1), data = diff2_close, trace = F)
summary(model_garch_11)

model_garch_12 = garchFit(~arma(0,1)+garch(1,2), data = diff2_close, trace = F)
summary(model_garch_12)

model_garch_20 = garchFit(~arma(0,1)+garch(2,0), data = diff2_close, trace = F)
summary(model_garch_20)

model_garch_21 = garchFit(~arma(0,1)+garch(2,1), data = diff2_close, trace = F)
summary(model_garch_21)

model_garch_22 = garchFit(~arma(0,1)+garch(2,2), data = diff2_close, trace = F)
summary(model_garch_22)

#model terpilih adalah ARIMA(0, 2, 1) + GARCH(1, 1)