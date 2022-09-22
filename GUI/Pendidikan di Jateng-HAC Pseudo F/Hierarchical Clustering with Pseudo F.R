library(shiny)
library(shinythemes)
library(psych) #KMO dan PCA
library(clusterSim) #Cluster Optimal

#Statistika Deskriptif
gambaran_umum=function(data){
  m=ncol(data)
  rata=NULL
  minimum=NULL
  maksimum=NULL
  stdeviasi=NULL
  for (i in 1:m){
    rata[i]=mean(data[,i])
    maksimum[i]=max(data[,i])
    minimum[i]=min(data[,i])
    stdeviasi[i]=sd(data[,i])
  }
  baris=cbind(minimum,maksimum,rata,stdeviasi)
  colnames(baris)=c("Minimum","Maksimum","Rata-Rata","Std.Deviasi")
  rownames(baris)=c("X1","X2","X3","X4","X5","X6","X7","X8","X9","X10","X11","X12","X13","X14","X15","X16","X17","X18","X19")
  print(baris)
}

#rata-rata tiap cluster
rata2=function(data,objk,jml,metode){
  d=dist(data,method = "euclidean")
  h=hclust(d,method =metode)
  Cluster=cutree(h,k=jml)
  rata=aggregate(x=data,by=list(Cluster),FUN="mean") 
  anggota<-data.frame(id=objk, Cluster)
  print(anggota)
  cat("\nBerikut rata-rata tiap cluster-nya:","\n")
  print(rata)
}

#rata-rata tiap cluster dengan perbaikan non multiko
rata22=function(data,dataasli,objk,jml,metode){
  d=dist(data, method = "euclidean")
  h=hclust(d,method =metode)
  Cluster=cutree(h,k=jml)
  rata=aggregate(x=dataasli,by=list(Cluster),FUN="mean") 
  anggota<-data.frame(id=objk, Cluster)
  print(anggota)
  cat("\nBerikut rata-rata tiap cluster-nya:","\n")
  print(rata)
}

#nilai pesudo F
psF=function(data,jmlh,metode){
  nilai=seq(2,jmlh,by=1)
  d=dist(data, method = "euclidean")
  h=hclust(d,method=metode)
  kl=(jmlh-1)
  pseudoF=NULL
  for(i in 1:kl){
      pseudoF[i]=index.G1(data,cutree(h,k=nilai[i]))
      }
  R=matrix(c(nilai,pseudoF),nrow=kl)
  sR<-R[order(-R[,2]),]
  cat('============================\n')
  cat('No   k-cluster     Pseudo-F \n')
  print(R)
  cat('============================\n')
  cat('Nilai Pseudo-F tertinggi=',sR[1,2],'
jumlah cluster optimal yang terbentuk sebanyak',sR[1,1],'cluster \n')
}


ui <- fluidPage(theme=shinytheme("lumen"),
                # Judul GUI R
                titlePanel("Optimalisasi Metode Hierarchical Agglomerative Clustering dengan Pseudo F-Statistics"),
                h4(tags$a(href = "https://www.instagram.com/alimhdan2/","oleh Ali Mahmudan-Statistika 2018")),
                navbarPage("Tugas Akhir",
                           tabPanel("Tentang GUI",
                                    mainPanel(
                                      tabsetPanel(type = "pills",id = "navbar",
                                                  tabPanel("Topik",
                                                           tags$img(src="E:/UNDIP/Skripsi/Ahdan/Skripsi/cover1.png"),
                                                           imageOutput("image"),value="Topik"),
                                                  tabPanel("Petunjuk Penggunaan",
                                                           h4("Petunjuk Penggunaan GUI"),
                                                           h4("1. Aplikasi GUI ini digunakan untuk melakukan clustering dengan metode Hierarchical Agglomerative Clustering dan optimasi Pseudo F-Statistics"),
                                                           h4("2. Input data variabel indikator dan objek di menu input data format .txt"),
                                                           h4("3. Baris pertama pada data inputan tersebut harus beruapa nama variabelnya"),
                                                           h4("4. Setelah data terinput maka output statistika deskriptif, matriks jarak, uji asumsi, dan penanganan asumsi akan otomatis keluar pada tampilan GUI"),
                                                           h4("5. Lakukan pemilihan jumlah cluster optimal dengan cara klik panel 'Cluster Optimal', lalu pilih metode penentuan jarak terdekat yang diinginkan pada menu 'Metode' (Metode yang dapat dipilih adalah Single Linkage, Complete Linkage, Average Linkage, Centroid, dan Ward), isikan jumlah cluster maksimal yang ingin dibentuk pada bagian 'Jumlah Cluster Maksimal', isikan jumlah komponen utama pada bagian 'Jumlah Komponen Utama' (isikan nilai 0 atau bilangan sembarang apabila tidak menggunakan komponen utama), lalu klik 'Run'"),
                                                           h4("6. Pilih output bagian atas jika tidak menggunakan variabel komponen utama dan pilih output bagian bawah 'Pseudo F Komponen Utama' jika menggunakan variabel komponen utama"),
                                                           h4("7. Lakukan clustering dengan cara klik panel 'Hasil Clustering', lalu pilih metode penentuan jarak terdekat yang diinginkan pada menu 'Metode' (Metode yang dapat dipilih adalah Single Linkage, Complete Linkage, Average Linkage, Centroid, dan Ward), isikan jumlah cluster optimal pada bagian 'Jumlah Cluster Optimal', isikan jumlah komponen utama pada bagian 'Jumlah Komponen Utama' (isikan nilai 0 atau bilangan sembarang apabila tidak menggunakan komponen utama), lalu klik 'Clustering'"),
                                                           h4("8. Pilih tab panel 'Hasil Clustering' apabila tidak menggunakan variabel komponen utama dan pilih tab panel 'Hasil Clustering Komponen Utama' apabila menggunakan variabel komponen utama"),
                                                           h4("9. Lakukan interpretasi berdasarkan hasil clustering tersebut"))))),
                           tabPanel("Input Data",
                                    sidebarLayout(
                                      sidebarPanel(
                                        fileInput("dataku","Input Variabel Indikator",accept = c("text",".txt")),
                                        fileInput("objek","Input Data Objek",accept = c("text",".txt"))),
                                      mainPanel(
                                        tabsetPanel(type ="pills",id = "navbar",
                                                    tabPanel("Statistika Deskriptif",verbatimTextOutput("statdes")),
                                                    tabPanel("Matriks Jarak",verbatimTextOutput("matrix")))))),
                           tabPanel("Uji Asumsi",
                                    mainPanel(
                                      tabsetPanel(type ="pills",id = "navbar",
                                                  tabPanel("Representativeness of the Sample",verbatimTextOutput("kmo")),
                                                  tabPanel("Non Multikolinieritas",verbatimTextOutput("multiko"))))),
                           tabPanel("Penanganan Asumsi",
                                    mainPanel(
                                      tabsetPanel(type ="pills",id = "navbar",
                                                  tabPanel("Representativeness of the Sample",verbatimTextOutput("pkmo")),
                                                  tabPanel("Non Multikolinieritas",plotOutput("screeplot"),verbatimTextOutput("pmultiko"))))),
                           tabPanel("Cluster Optimal",
                                    sidebarLayout(
                                      sidebarPanel(
                                        h4("Pilih Metode dan Jumlah Cluster Maksimal"),
                                        selectInput("mtd","Metode",choices = c('Single Linkage','Complete Linkage','Average Linkage','Centroid','Ward')),
                                        textInput("jml","Jumlah Maksimal Cluster"),
                                        textInput("jmlkomponen","Jumlah Komponen Utama"),
                                        actionButton("run",'Olah',class="btn-success")),
                                      mainPanel(
                                        tabsetPanel(type ="pills",id = "navbar",
                                                    tabPanel("Nilai Pseudo-F",verbatimTextOutput("pseudoF"),verbatimTextOutput("ppseudoF")))))),
                           tabPanel("Hasil Clustering",
                                    sidebarLayout(
                                      sidebarPanel(
                                        h4("Clustering sejumlah-k Cluster Optimal"),
                                        selectInput("method","Metode",choices = c('Single Linkage','Complete Linkage','Average Linkage','Centroid','Ward')),
                                        textInput("jumlah","Jumlah Cluster Optimal"),
                                        textInput("jmlkomponen","Jumlah Komponen Utama"),
                                        actionButton("running",'Clustering',class="btn-success")),
                                      mainPanel(
                                        tabsetPanel(type ="pills",id = "navbar",
                                                    tabPanel("Hasil Clustering",plotOutput("dendogram"),verbatimTextOutput("anggotacluster")),
                                                    tabPanel("Hasil Clustering Komponen Utama",plotOutput("dendogramm"),verbatimTextOutput("anggotaclus"))
                                        ))))))


server <- function(input, output) {
  output$image<-renderImage({list(src="E:/UNDIP/Skripsi/Ahdan/Skripsi/cover1.png")})
  output$statdes<-renderPrint({
    data<-input$dataku
    if(is.null(data)){return()}
    file<-read.csv(data$datapath, sep = '\t', header = T )
    cat("=====Berikut Hasil Statistika Deskriptif=====","\n")
    gambaran_umum(file)
    cat("=============================================")})
  output$matrix<-renderPrint({
    data<-input$dataku
    if(is.null(data)){return()}
    file<-read.csv(data$datapath, sep = '\t', header = T )
    cat("===============Berikut Merupakan Matriks Jarak yang Terbentuk==============","\n")
    print(dist(file,method = "euclidean"))
    cat("===========================================================================")})
  output$kmo<-renderPrint({
    data<-input$dataku
    if(is.null(data)){return()}
    file<-read.csv(data$datapath, sep = '\t', header = T )
    cat("=======Berikut Merupakan Hasil dari Uji KMO====================================","\n")
    cat("Asumsi Terpenuhi Bila Nilai KMO (Overall MSA) > 0,5","\n")
    cat("\n")
    print(KMO(file))
    cat("===============================================================================")})
  output$multiko<-renderPrint({
    data<-input$dataku
    if(is.null(data)){return()}
    indikator<-read.csv(data$datapath, sep = '\t', header = T )
    cat("=============Berikut Merupakan Hasil dari Variance Inflation Factor==============","\n")
    cat("Asumsi Terpenuhi Bila Nilai Variance Inflation Factor (VIF) < 10","\n")
    VIF=function(indikator,i) 
    { 
      n = nrow(indikator)
      p = ncol(indikator) 
      
      y = indikator[,i] # Definisikan prediktor ke-i sebagai respon 
      X = indikator[,-i] # prediktor yang lain  
      
      bo= rep(1,n) 
      X = cbind(bo,X) 
      X = as.matrix(X) 
      
      beta=solve(t(X)%*%X)%*%t(X)%*%y 
      H = X%*%solve(t(X)%*%X)%*%t(X) 
      I = diag(rep(1,n)) 
      SSE = t(y)%*%(I-H)%*%y  
      
      J = matrix(1,n,n) 
      SST = t(y)%*%(I-(1/n)*J)%*%y 
      Rsquare=1-(SSE/SST) 
      vif=1/(1-Rsquare)
      
      return(vif) 
    }
    
    p = ncol(indikator)
    vif=rep(0,p)
    for (i in 1:p){
      vif[i]=VIF(indikator,i)
    }
    cat('\n********************************************************************************\n')
    cat('Nilai Variance Inflation Factor (VIF):\n')
    print(vif)
    cat('\n********************************************************************************\n')})
  output$pkmo<-renderPrint({
    data<-input$dataku
    if(is.null(data)){return()}
    file<-read.csv(data$datapath, sep = '\t', header = T )
    cat("=================================================================================","\n")
    cat("Penanganan Asumsi Sampel Representatif Dapat Dilakukan dengan Manambah Variabel","\n")
    cat("=================================================================================","\n")})
  output$pmultiko<-renderPrint({
    data<-input$dataku
    if(is.null(data)){return()}
    file<-read.csv(data$datapath, sep = '\t', header = T )
    pca <- princomp(file, cor = FALSE, scores = TRUE, covmat = NULL)
    ringkasan=summary(pca)
    cat("=================================================================================================","\n")
    cat("Penanganan Asumsi Non Multikolinieritas dapat Dilakukan dengan Principal Component Analysis (PCA)","\n")
    cat("Berikut Merupakan Hasil Pengujian Menggunakan PCA:","\n")
    cat("\nKomponen Utama yang Dipilih Sampai pada Nilai Cumulative Propotion > 0,8","\n")
    cat("\n")
    print(ringkasan)
    cat("==================================================================================================","\n")})
  output$screeplot<-renderPlot({
    data<-input$dataku
    if(is.null(data)){return()}
    file<-read.csv(data$datapath, sep = '\t', header = T )
    Scree_Plot <- princomp(file, cor = FALSE, scores = TRUE, covmat = NULL)
    plot(Scree_Plot,type="lines")})
  observeEvent(input$run,{
    data<-input$dataku
    if(is.null(data)){return()}
    file<-read.csv(data$datapath, sep = '\t', header = T )
    jml=as.numeric(input$jml)
    jmlk=as.numeric(input$jmlkomponen)
    pca <- princomp(file, cor = FALSE, scores = TRUE, covmat = NULL)
    file1=pca$score
    file2=file1[,1:jmlk]
    if(input$mtd=='Single Linkage')
    {
      output$pseudoF<-renderPrint({
        psF(file,jml,"single")
      })
      output$ppseudoF<-renderPrint({
        cat("Nilai Psudo F Komponen Utama\n")
        psF(file2,jml,"single")
      })
    }
    if(input$mtd=='Complete Linkage')
    {
      output$pseudoF<-renderPrint({
        psF(file,jml,"complete")
      })
      output$ppseudoF<-renderPrint({
        cat("Nilai Psudo F Komponen Utama\n")
        psF(file2,jml,"complete")
      })
    }
    if(input$mtd=='Average Linkage')
    {
      output$pseudoF<-renderPrint({
        psF(file,jml,"average")
      })
      output$ppseudoF<-renderPrint({
        cat("Nilai Psudo F Komponen Utama\n")
        psF(file2,jml,"average")
      })
    }
    if(input$mtd=='Centroid')
    {
      output$pseudoF<-renderPrint({
        psF(file,jml,"centroid")
      })
      output$ppseudoF<-renderPrint({
        cat("Nilai Psudo F Komponen Utama\n")
        psF(file2,jml,"centroid")
      })
    }
    if(input$mtd=='Ward')
    {
      output$pseudoF<-renderPrint({
        psF(file,jml,"ward.D2")
      })
      output$ppseudoF<-renderPrint({
        cat("Nilai Psudo F Komponen Utama\n")
        psF(file2,jml,"ward.D2")
      })
    }})
  observeEvent(input$running,{
    data<-input$dataku
    if(is.null(data)){return()}
    file<-read.csv(data$datapath, sep = '\t', header = T )
    objek<-input$objek
    if(is.null(objek)){return()}
    objek<-read.csv(objek$datapath, sep = '\t', header = T )
    d=dist(file, method = "euclidean")
    jumlah=as.numeric(input$jumlah)
    jmlk=as.numeric(input$jmlkomponen)
    pca <- princomp(file, cor = FALSE, scores = TRUE, covmat = NULL)
    file1=pca$score
    file2=file1[,1:jmlk]
    if(input$method=='Single Linkage')
    {
      output$anggotacluster<-renderPrint({
        cat("Hasil Clustering\n")
        rata2(file,objek,jumlah,"single")
      })
      output$dendogram<-renderPlot({
        h=hclust(d,method = "single")
        plot(h,cex=0.86,hang=-1)
        })
      output$dendogramm<-renderPlot({
        d1=dist(file2,method = "euclidean")
        h1=hclust(d1,method = "single")
        plot(h1,cex=0.86,hang=-1)
      })
      output$anggotaclus<-renderPrint({
        cat("Hasil Clustering (Komponen Utama)\n")
        rata22(file2,file,objek,jumlah,"single")
      })
    }
    if(input$method=='Complete Linkage')
    {
      output$anggotacluster<-renderPrint({
        cat("Hasil Clustering\n")
        rata2(file,objek,jumlah,"complete")
      })
      output$dendogram<-renderPlot({
        h=hclust(d,method = "complete")
        plot(h,cex=0.86,hang=-1)
        })
      output$dendogramm<-renderPlot({
        d1=dist(file2,method = "euclidean")
        h1=hclust(d1,method = "complete")
        plot(h1,cex=0.86,hang=-1)
      })
      output$anggotaclus<-renderPrint({
        cat("Hasil Clustering (Komponen Utama)\n")
        rata22(file2,file,objek,jumlah,"complete")
      })
    }
    if(input$method=='Average Linkage')
    {
      output$anggotacluster<-renderPrint({
        cat("Hasil Clustering\n")
        rata2(file,objek,jumlah,"average")
      })
      output$dendogram<-renderPlot({
        h=hclust(d,method = "average")
        plot(h,cex=0.86,hang=-1)
        })
      output$dendogramm<-renderPlot({
        d1=dist(file2,method = "euclidean")
        h1=hclust(d1,method = "average")
        plot(h1,cex=0.86,hang=-1)
      })
      output$anggotaclus<-renderPrint({
        cat("Hasil Clustering (Komponen Utama)\n")
        rata22(file2,file,objek,jumlah,"average")
      })
    }
    if(input$method=='Centroid')
    {
      output$anggotacluster<-renderPrint({
        cat("Hasil Clustering\n")
        rata2(file,objek,jumlah,"centroid")
      })
      output$dendogram<-renderPlot({
        h=hclust(d,method = "centroid")
        plot(h,cex=0.86,hang=-1)
      })
      output$dendogramm<-renderPlot({
        d1=dist(file2,method = "euclidean")
        h1=hclust(d1,method = "centroid")
        plot(h1,cex=0.86,hang=-1)
      })
      output$anggotaclus<-renderPrint({
        cat("Hasil Clustering (Komponen Utama)\n")
        rata22(file2,file,objek,jumlah,"centroid")
      })
    }
    if(input$method=='Ward')
    {
      output$anggotacluster<-renderPrint({
        cat("Hasil Clustering\n")
        rata2(file,objek,jumlah,"ward.D2")
      })
      output$dendogram<-renderPlot({
        h=hclust(d,method = "ward.D2")
        plot(h,cex=0.86,hang=-1)
        })
      output$dendogramm<-renderPlot({
        d1=dist(file2,method = "euclidean")
        h1=hclust(d1,method = "ward.D2")
        plot(h1,cex=0.86,hang=-1)
      })
      output$anggotaclus<-renderPrint({
        cat("Hasil Clustering (Komponen Utama)\n")
        rata22(file2,file,objek,jumlah,"ward.D2")
      })
    }})
}


# Menjalankan GUI R
shinyApp(ui = ui, server = server)
