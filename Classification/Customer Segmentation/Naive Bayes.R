library(tidyverse)

library(readxl)
data=read_excel("E:/My Works/DSA/R/Soal Latihan/Train.xlsx")
head(data)
sum(is.na(data))
data=drop_na(data)
data %>% count(Segmentation)
View(data)
training=data

str(training)
training$Gender=as.factor(training$Gender)
training$Ever_Married=as.factor(training$Ever_Married)
training$Graduated=as.factor(training$Graduated)
training$Profession=as.factor(training$Profession)
training$Spending_Score=as.factor(training$Spending_Score)
training$Segmentation=as.factor(training$Segmentation)


#klasifikasi
library(caret)
library(e1071)
training = training %>% select(-ID)
model=naiveBayes(Segmentation~.,data=training)
model



data1=read_excel("E:/My Works/DSA/R/Soal Latihan/Test.xlsx")
head(data1)
sum(is.na(data1))
data1=drop_na(data1)
data1 %>% count(Segmentation)
View(data1)
testing=data1

str(testing)
testing$Gender=as.factor(testing$Gender)
testing$Ever_Married=as.factor(testing$Ever_Married)
testing$Graduated=as.factor(testing$Graduated)
testing$Profession=as.factor(testing$Profession)
testing$Spending_Score=as.factor(testing$Spending_Score)
testing$Segmentation=as.factor(testing$Segmentation)


prediksi=predict(model,testing)
confusionMatrix(prediksi,testing$Segmentation)


nrow(training)
nrow(testing)
