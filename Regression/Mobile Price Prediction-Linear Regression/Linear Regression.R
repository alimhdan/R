#memanggil data
data=read.csv("E:/My Works/My WORKS/DSA/R/Soal Latihan/Cellphone.csv",header=T,sep=",")
str(data)
head(data)
sum(is.na(data))
View(data)

#membuat model regresi
library(tidyverse)
data =data %>% select(-Product_id)
model= lm(Price~.,data=data)

#uji model
#uji simultan,parsail,R-sq
summary(model)

#uji asumsi
#normalitas resid
resid=model$residuals
shapiro.test(resid)

#non multiko
library(car)
vif(model)

#Linieritas
library(randtests)
library(lmtest)
resettest(model)

#uji homoskedastisitas
absresid=abs(resid)
gl=data %>% select(-Price)
gljr=lm(absresid~.,data=gl)
summary(gljr)

#uji non autokorelasi
dwtest(model,alternative = "two")
runs.test(resid)

#plot prediksi vs aktual
prediksi=predict(model)
prediksi
win.graph()
plot(data$Price,type="l",col="blue")
par(new=T)
plot(prediksi,type="l",col="red")