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
adf.test(tsclose)
acf(tsclose)
pacf(tsclose)

#transformasi differencing 1, karena Box-cox (lambda=1) yang telah diuji pada Minitab 18
diff_close = diff(tsclose)
ts.plot(diff_close)
adf.test(diff_close)
acf(diff_close)
pacf(diff_close)
eacf(diff_close)

#Plot menunjukan model IMA(0, 1, 1)
model011 = arima(diff_close, order = c(0,0,1), include.mean = FALSE)

#Diagnosa
coeftest(model011)
Box.test(model011$residuals,type = "Ljung")
ks.test(model011$residuals, ecdf(model011$residuals))
AutocorTest(diff_close)

#Menguji kenormalan residual
jarque.bera.test(residuals(model011))

#menguji efek arch
arch.test(model011)
ArchTest(diff_close)
for(i in 1:12)
{
    AT = ArchTest(diff_close, lags = i, demean = TRUE)
    cat("P Value LM Test lag ke", i , "adalah", AT$p.value, "\n")
}
#P-Value < 0,05 menunjukkan ada efek ARCH

#Melanjutkan ke GARCH karena residual tidak normal
model_garch_10 = garchFit(~arma(0,1)+garch(1,0), data = diff_close, trace = F)
summary(model_garch_10)

model_garch_11 = garchFit(~arma(0,1)+garch(1,1), data = diff_close, trace = F)
summary(model_garch_11)

model_garch_12 = garchFit(~arma(0,1)+garch(1,2), data = diff_close, trace = F)
summary(model_garch_12)

model_garch_20 = garchFit(~arma(0,1)+garch(2,0), data = diff_close, trace = F)
summary(model_garch_20)

model_garch_21 = garchFit(~arma(0,1)+garch(2,1), data = diff_close, trace = F)
summary(model_garch_21)

model_garch_22 = garchFit(~arma(0,1)+garch(2,2), data = diff_close, trace = F)
summary(model_garch_22)
