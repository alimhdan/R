#memanggil data
data=read.csv("E:/My Works/My WORKS/DSA/R/Data/data cleaning-R Community Survey.csv",header=T,sep = ",")
head(data)
View(data)

names(data)

#ubah nama variabel
library(tidyverse)
data1<-data
data1<-data1 %>% rename(time=Timestamp,
                        experience=How.would.you.rate.your.level.of.experience.using.R.,
                        highest_degree=What.is.the.highest.degree.or.level.of.school.you.have.completed..If.currently.enrolled..please.use.the.highest.degree.received.,
                        country=In.what.country.do.you.currently.reside.,
                        industry=What.industry.do.you.work.or.participate.in.,
                        people_help=How.many.people.in.your.organization.or.work.group.do.you.feel.that.you.can.ask.for.help.or.support.when.working.with.R.)
names(data1)

#data duplikat
data1<- distinct(data1)
nrow(data1)
nrow(data)

#missing value dan structure data
sum(is.na(data1))

data1[data1=="" | data1==" "]<-NA

data1 %>% summarise_all(funs(sum(is.na(.))))

str(data1)

#variabel time
data1$time<- as.Date(data1$time,"%m/%d/%Y")
str(data1$time)

#variabel experience
data1 %>% count(experience)
data1$experience<-as.factor(data1$experience)
str(data1$experience)

#variabel highest_degree
data1 %>% count(highest_degree)
data1<- data1 %>% mutate(highest_degree = case_when(str_detect(highest_degree,"Bachelor's degree")~"Bachelor's",
                                                    str_detect(highest_degree,"Master's degree")~"Master's",
                                                    TRUE~highest_degree))
data1$highest_degree<-as.factor(data1$highest_degree)
str(data1$highest_degree)

#variabel country
sum(is.na(data1$country))
#ditangani dengan mengganti NA menjadi Unkown
data1<- data1 %>% mutate(country = ifelse(is.na(country)==T,"Unkown",country))

#variabel industry
sum(is.na(data1$industry))
str(data1$industry)

data1 %>% count(industry)
data1<- data1 %>% mutate(industry = str_squish(tolower(industry)))
data1<- data1 %>% mutate(industry = case_when(str_detect(industry,"consulting|consultancy")~"consulting",
                                              str_detect(industry,"biotechnology|biotech|agriculture and animal science|agrifood|agriculture")~"agriculture",
                                              TRUE~"other"))
data1$industry<-as.factor(data1$industry)
str(data1$industry)
sum(is.na(data1$industry))

names(data1)

#variabel people
#Numerik
#missing value diganti dengan nilai 0

data1<- data1 %>% mutate(people_help = ifelse(is.na(people_help)==T,0,people_help))

#mengidentifikasi outlier
#visual
#plot atau scatterplot
plot(data1$people)

#histogram
hist(data1$people)

#boxplot
boxplot(data1$people)


#cara mengatasi outlier
#winsorization atau winsorizing
library(psych)
winsor_data=winsor(data1$people,trim=0.1)
head(winsor_data)

data_winsor= data1 %>% mutate(people_winsor=winsor_data)

boxplot(data_winsor$people_winsor)


#data yang dipakai adalah data hasil winsorizing 80%
datafiks=data_winsor %>% select(-people_help)
head(datafiks)


#RECHECKING HASIL CLEANING DATA
#missing value
sum(is.na(datafiks))

#struktur data
str(datafiks)
datafiks$people_winsor=as.integer(datafiks$people_winsor)

#checking ulang outlier
boxplot(datafiks$people_winsor)
plot(datafiks$people_winsor)
hist(datafiks$people_winsor)


#Visualisasi
data=datafiks
library(ggplot2)

people=data$people_winsor
experience=data$experience
degree=data$highest_degree
country=data$country
industry=data$industry
time=data$time

#barchart
win.graph()
ggplot(data=data)+
  geom_bar(
    aes(x=experience,fill=experience)
  )+
  geom_label(
    aes(x=experience,label=..count..),
    stat = "count"
  )+
  labs(title = "Experience Using R")

#100% Stacked
win.graph()
ggplot(data=data)+
  geom_bar(
    aes(x=highest_degree,fill=experience),
    position = "fill"
  )

#flipbar
win.graph()
ggplot(data=data)+
  coord_flip()+
  geom_bar(data=data %>% filter(experience=="Expert"),
           aes(x=country,fill=experience))

#side by side bar chart
win.graph()
ggplot(data=data)+
  geom_bar(
    aes(x=highest_degree,fill=experience),
    position = "dodge")
