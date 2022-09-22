data=read.delim("clipboard",header=T)
head(data)
nrow(data)

library(tidyverse)

x= data %>% select(-terhadap.ketertiban.umum,-wilayah,-pencurian.Biasa,-pencurian.dengan.kekerasan,-pencurian.dengan.pemberatan,-pencurian.kendaraan.bermotor,-pengrusakan.penghancuran.barang)

library(psych)
KMO(x)

model=lm(pembunuhan*2~pembunuhan+.,data=x)
library(car)
vif(model)

View(x)
rownames(x)=data$wilayah

#clustering
#cluster optimal dengan silhouette
library(factoextra)
fviz_nbclust(x,kmeans,method = "silhouette")

#kmeans clustering dengan k=2
clustering=kmeans(x,2,nstart = 25)
clustering

#visualisasi
win.graph()
fviz_cluster(clustering,x)

indikator = x %>% mutate(cluster=clustering$cluster)

#profiling cluster
clustering

