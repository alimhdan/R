library(shiny)
library(shinythemes)
library(psych) #KMO dan PCA
library(clusterSim) #Cluster Optimal

#rata-rata tiap cluster
rata2=function(data,objk,jml,metode){
  data1=scale(data)
  d=dist(data1,method = "euclidean")
  h=hclust(d,method =metode)
  Cluster=cutree(h,k=jml)
  rata=aggregate(x=data,by=list(Cluster),FUN="mean") 
  anggota<-data.frame(id=objk, Cluster)
  print(anggota)
  cat("The following is the average of each cluster:","\n")
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
  cat("The following is the average of each cluster:","\n")
  print(rata)
}

#nilai pesudo F
psF=function(data,jmlh,metode){
  data=scale(data)
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
  cat('Higest Pseudo F Score=',sR[1,2],'
The optimal number of cluster is',sR[1,1],'clusters \n')
}

#nilai pesudo F
psF1=function(data,jmlh,metode){
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
  cat('Higest Pseudo F Score=',sR[1,2],'
The optimal number of cluster is',sR[1,1],'clusters \n')
}

ui <- fluidPage(theme=shinytheme("darkly"),
                # Application title
                titlePanel("Hierarchical Agglomerative Clustering with Pseudo F-Statistic"),
                h4("by Departement of Statistics UNDIP Research Team"),
                navbarPage("Hierarchical Agglomerative Clustering",
                           tabPanel("Input Data",
                                    sidebarLayout(
                                      sidebarPanel(
                                        fileInput("dataku","Indicators",accept = c("text",".txt")),
                                        fileInput("objek","Objects",accept = c("text",".txt"))),
                                      mainPanel(
                                        tabsetPanel(type ="pills",id = "navbar",
                                                    tabPanel("Descriptive Statistics",verbatimTextOutput("statdes")),
                                                    tabPanel("Distance Matrix",verbatimTextOutput("jarak")))))),
                           tabPanel("Assumption Tests",
                                    mainPanel(
                                      tabsetPanel(type ="pills",id = "navbar",
                                                  tabPanel("Representativeness of the sample",verbatimTextOutput("kmo")),
                                                  tabPanel("Non Multicollinearity",verbatimTextOutput("multiko"))))),
                           tabPanel("Assumption Handling",
                                    mainPanel(
                                      tabsetPanel(type ="pills",id = "navbar",
                                                  tabPanel("Representativeness of the sample",verbatimTextOutput("pkmo")),
                                                  tabPanel("Non Multicollinearity",verbatimTextOutput("pmultiko"),plotOutput("screeplot"))))),
                           tabPanel("Optimal Cluster",
                                    sidebarLayout(
                                      sidebarPanel(
                                        h4("Choose Method and Maximum Number of Clusters"),
                                        selectInput("mtd","Method",choices = c('Single Linkage','Complete Linkage','Average Linkage','Centroid','Ward')),
                                        textInput("jml","Maximum Number of Clusters"),
                                        textInput("jmlkomponen","Number of Principal Components"),
                                        actionButton("run",'Run',class="btn-success")),
                                      mainPanel(
                                        tabsetPanel(type ="pills",id = "navbar",
                                                    tabPanel("Pseudo-F Scores",verbatimTextOutput("pseudoF"),verbatimTextOutput("ppseudoF")))))),
                           tabPanel("Clustering Results",
                                    sidebarLayout(
                                      sidebarPanel(
                                        h4("Optimal k-cluster for clustering"),
                                        selectInput("method","Method",choices = c('Single Linkage','Complete Linkage','Average Linkage','Centroid','Ward')),
                                        textInput("jumlah","Optimal number of clusters"),
                                        textInput("jmlkomponen","Number of Principal Components"),
                                        actionButton("running",'Clustering',class="btn-success")),
                                      mainPanel(
                                        tabsetPanel(type ="pills",id = "navbar",
                                                    tabPanel("Clustering Results",verbatimTextOutput("anggotacluster"),plotOutput("dendogram")),
                                                    tabPanel("Clustering Results for Principal Components",verbatimTextOutput("anggotaclus"),plotOutput("dendogramm"))
                                        ))))))


server <- function(input, output) {
  output$statdes<-renderPrint({
    data<-input$dataku
    if(is.null(data)){return()}
    file<-read.csv(data$datapath, sep = '\t', header = T )
    cat("====================Result of Descriptive Statistics===================","\n")
    print(summary(file))
    cat("======================================================================")})
  output$jarak<-renderPrint({
    data<-input$dataku
    if(is.null(data)){return()}
    file<-read.csv(data$datapath, sep = '\t', header = T )
    cat("=======================Result of Distance Matrix======================","\n")
    print(dist(file, method = "euclidean"))
    cat("======================================================================")})
  output$kmo<-renderPrint({
    data<-input$dataku
    if(is.null(data)){return()}
    file<-read.csv(data$datapath, sep = '\t', header = T )
    cat("================Result of KMO Test================","\n")
    cat("Assumptions are met if the Overall MSA > 0.5","\n")
    print(KMO(file))
    cat("==================================================")})
  output$multiko<-renderPrint({
    data<-input$dataku
    if(is.null(data)){return()}
    indikator<-read.csv(data$datapath, sep = '\t', header = T )
    cat("=============Result of Variance Inflation Factor==============","\n")
    cat("Assumptions are met if Variance Inflation Factor (VIF) < 10","\n")
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
    cat('\n***************************************\n')
    cat('Variance Inflation Factor (VIF):\n')
    print(vif)
    cat('\n***************************************\n')})
  output$pkmo<-renderPrint({
    data<-input$dataku
    if(is.null(data)){return()}
    file<-read.csv(data$datapath, sep = '\t', header = T )
    cat("\n","===============================================================================================","\n")
    cat("Handling Representative Sample Assumptions can be Done by Adding or Subtracting Variables","\n")
    cat("=================================================================================================","\n")})
  output$pmultiko<-renderPrint({
    data<-input$dataku
    if(is.null(data)){return()}
    file<-read.csv(data$datapath, sep = '\t', header = T )
    pca <- princomp(file, cor = FALSE, scores = TRUE, covmat = NULL)
    ringkasan=summary(pca)
    cat("\n","=================================================================================================","\n")
    cat("Handling Non-Multicollinearity Assumptions can be Done with Principal Component Analysis (PCA)","\n")
    cat("Here are the test results using PCA","\n")
    cat("Selected Main Components Up to Cumulative Proportion Value >= 0.7","\n")
    print(ringkasan)
    cat("===================================================================================================","\n")})
  output$screeplot<-renderPlot({
    data<-input$dataku
    if(is.null(data)){return()}
    file<-read.csv(data$datapath, sep = '\t', header = T )
    file=scale(file)
    pca <- princomp(file, cor = TRUE, scores = TRUE, covmat = NULL)
    plot(pca,type="lines")})
  observeEvent(input$run,{
    data<-input$dataku
    if(is.null(data)){return()}
    file<-read.csv(data$datapath, sep = '\t', header = T )
    file=scale(file)
    jml=as.numeric(input$jml)
    jmlk=as.numeric(input$jmlkomponen)
    pca <- princomp(file, cor = TRUE, scores = TRUE, covmat = NULL)
    file1=pca$score
    file2=file1[,1:jmlk]
    if(input$mtd=='Single Linkage')
    {
      output$pseudoF<-renderPrint({
        psF(file,jml,"single")
      })
      output$ppseudoF<-renderPrint({
        cat("\nPseudo F of Principal Components\n")
        psF1(file2,jml,"single")
      })
    }
    if(input$mtd=='Complete Linkage')
    {
      output$pseudoF<-renderPrint({
        psF(file,jml,"complete")
      })
      output$ppseudoF<-renderPrint({
        cat("\nPseudo F of Principal Components\n")
        psF1(file2,jml,"complete")
      })
    }
    if(input$mtd=='Average Linkage')
    {
      output$pseudoF<-renderPrint({
        psF(file,jml,"average")
      })
      output$ppseudoF<-renderPrint({
        cat("\nPseudo F of Principal Components\n")
        psF1(file2,jml,"average")
      })
    }
    if(input$mtd=='Centroid')
    {
      output$pseudoF<-renderPrint({
        psF(file,jml,"centroid")
      })
      output$ppseudoF<-renderPrint({
        cat("\nPseudo F of Principal Components\n")
        psF1(file2,jml,"centroid")
      })
    }
    if(input$mtd=='Ward')
    {
      output$pseudoF<-renderPrint({
        psF(file,jml,"ward.D2")
      })
      output$ppseudoF<-renderPrint({
        cat("\nPseudo F of Principal Components\n")
        psF1(file2,jml,"ward.D2")
      })
    }})
  observeEvent(input$running,{
    data<-input$dataku
    if(is.null(data)){return()}
    file<-read.csv(data$datapath, sep = '\t', header = T )
    fille=scale(file)
    objek<-input$objek
    if(is.null(objek)){return()}
    objek<-read.csv(objek$datapath, sep = '\t', header = T )
    d=dist(fille, method = "euclidean")
    jumlah=as.numeric(input$jumlah)
    jmlk=as.numeric(input$jmlkomponen)
    pca <- princomp(fille, cor = TRUE, scores = TRUE, covmat = NULL)
    file1=pca$score
    file2=file1[,1:jmlk]
    if(input$method=='Single Linkage')
    {
      output$anggotacluster<-renderPrint({
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
        rata22(file2,file,objek,jumlah,"single")
      })
    }
    if(input$method=='Complete Linkage')
    {
      output$anggotacluster<-renderPrint({
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
        rata22(file2,file,objek,jumlah,"complete")
      })
    }
    if(input$method=='Average Linkage')
    {
      output$anggotacluster<-renderPrint({
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
        rata22(file2,file,objek,jumlah,"average")
      })
    }
    if(input$method=='Centroid')
    {
      output$anggotacluster<-renderPrint({
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
        rata22(file2,file,objek,jumlah,"centroid")
      })
    }
    if(input$method=='Ward')
    {
      output$anggotacluster<-renderPrint({
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
        rata22(file2,file,objek,jumlah,"ward.D2")
      })
    }})
}


# Run the application 
shinyApp(ui = ui, server = server)
