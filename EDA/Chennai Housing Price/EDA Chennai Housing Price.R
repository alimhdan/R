data=read.csv("E:/My Works/DSA/R/Data/Chennai housing sale.csv",header=T,sep=",")
str(data)
View(data)


library(tidyverse)
dup<-distinct(data)
nrow(dup)
nrow(data)


dup %>% summarise_all(funs(sum(is.na(.))))

dup[dup=="" | dup==" "]<-NA
view(dup)


edit<- dup %>% select(-c(UTILITY_AVAIL,AREA,MZZONE,QS_ROOMS,QS_BATHROOM,QS_BEDROOM,QS_OVERALL))
edit %>% summarise_all(funs(sum(is.na(.))))
write.csv(data,"E:/My Works/DSA/R/Data/Chennai Housing Sale Dataset.csv")


str(edit)
edit1<-edit

#variabel area
edit1 %>% count(AREA)


data=read.csv("E:/My Works/DSA/R/Data/Chennai Housing Sale Dataset.csv",header=T,sep=",")
head(data)
str(data)
data=data%>%select(-X)
head(data)
data %>% summarise_all(funs(sum(is.na(.))))

#variabel area
data %>% count(AREA)
data = data%>% mutate(AREA=tolower(AREA))
data = data %>% mutate(AREA=case_when(str_detect(AREA,"adyr")~"adyar",
                                 str_detect(AREA,"ana nagar|ann nagar")~"anna nagar",
                                 str_detect(AREA,"karapakam")~"karapakkam",
                                 str_detect(AREA,"chrmpet|chrompt|chormpet")~"chrompet",
                                 str_detect(AREA,"tnagar")~"t nagar",
                                 str_detect(AREA,"kknagar")~"kk nagar",
                                 str_detect(AREA,"velchery")~"velachery",
                                 TRUE~AREA))
data$AREA=as.factor(data$AREA)

#date
data$DATE_BUILD=as.Date(data$DATE_BUILD,"%d-%m-%Y")
data$DATE_SALE=as.Date(data$DATE_SALE,"%d-%m-%Y")

#Menangani missing value
data=data%>% mutate(N_BEDROOM=ifelse(is.na(N_BEDROOM)==T,0,N_BEDROOM))
data=data%>% mutate(N_BATHROOM=ifelse(is.na(N_BATHROOM)==T,0,N_BATHROOM))
data$N_BEDROOM=as.integer(data$N_BEDROOM)
data$N_BATHROOM=as.integer(data$N_BATHROOM)

#park_facil
data %>% count(PARK_FACIL)
data=data%>%mutate(PARK_FACIL=case_when(str_detect(PARK_FACIL,"Noo")~"No",
                                        TRUE~PARK_FACIL))
data=data%>%mutate(PARK_FACIL=tolower(PARK_FACIL))
data$PARK_FACIL=as.factor(data$PARK_FACIL)

#buildtype
data%>% count(BUILDTYPE)
data=data%>% mutate(BUILDTYPE=tolower(BUILDTYPE))
data=data%>%mutate(BUILDTYPE=case_when(str_detect(BUILDTYPE,"others")~"other",
                                       str_detect(BUILDTYPE,"comercial")~"commercial",
                                        TRUE~BUILDTYPE))
data$BUILDTYPE=as.factor(data$BUILDTYPE)

#street
data%>%count(STREET)
data=data%>% mutate(STREET=tolower(STREET))
data=data%>%mutate(STREET=case_when(str_detect(STREET,"noaccess")~"no access",
                                       str_detect(STREET,"pavd")~"paved",
                                       TRUE~STREET))
data$STREET=as.factor(data$STREET)

#TAMBAH VARIABEL TOTAL PRICE
data=data %>% mutate(TOTAL_PRICE=REG_FEE+COMMIS+SALES_PRICE)
head(data)
str(data)

win.graph()
par(mfrow=c(3,3))
boxplot(data$INT_SQFT)
boxplot(data$DIST_MAINROAD)
boxplot(data$N_BATHROOM)
boxplot(data$N_BEDROOM)
boxplot(data$N_ROOM)
boxplot(data$REG_FEE)
boxplot(data$COMMIS)
boxplot(data$SALES_PRICE)
boxplot(data$TOTAL_PRICE)

#check yang sales price aja
win.graph()
boxplot(data$SALES_PRICE)

library(rtweet)
write_as_csv(data,"E:/My Works/DSA/R/Data/chennai housing kurang outlier.csv")

data=read.csv("E:/My Works/DSA/R/Data/chennai housing kurang outlier.csv",header = T,sep=",")
head(data)
str(data)
data$DATE_BUILD=as.Date(data$DATE_BUILD,"%Y-%m-%d")
data$DATE_SALE=as.Date(data$DATE_SALE,"%Y-%m-%d")

#KERJAKAN YANG OUTLIER
boxplot(data$SALES_PRICE)
library(psych)
winsorizing=winsor(data$SALES_PRICE,trim = 0.1)
boxplot(winsorizing)

library(tidyverse)
databaru=data %>% mutate(SALES_PRICE_WINSOR=winsorizing)
head(databaru)

library(rtweet)
write_as_csv(databaru,"E:/My Works/DSA/R/Data/chennai housing bersih.csv")

#VISUALISASI DATA
library(ggplot2)

win.graph()
ggplot(data=databaru)+
  geom_bar(
    aes(x=BUILDTYPE,fill=BUILDTYPE)
  )+
  geom_label(
    aes(x=BUILDTYPE,label=..count..),
    stat='count'
  )+
  labs(title = 'Barchart of Build Type')

#side by side barchart
win.graph()
ggplot(data=databaru)+
  geom_bar(
    aes(x=BUILDTYPE,fill=PARK_FACIL),
    position = 'dodge'
  )+
  labs(title='Barchart of Build Type')

#ggplot
win.graph()
ggplot(data=databaru)+
  geom_point(
    aes(x=DATE_SALE,y=TOTAL_PRICE,color=BUILDTYPE)
  )+
  labs(title="Plot Total Price")

