#model ARIMA kasus aktif covid-19 jateng

library(shiny)
library(shinythemes)
library(timeSeries) #convert to time series
library(tseries) #ADF
library(MASS) #visualisasi boxcox
library(car) #boxcox transform
library(FitAR) #Ljung Box test
library(lmtest)

#membuat UI
tampilan_ARIMA<-fluidPage(theme = shinytheme("cerulean"),
                          titlePanel(tags$b("ARIMA Modeling for Active Cases of COVID-19 in Indonesia")),
                          h4("by: Dr.Budi Warsito, S.Si., M.Si.,  Ali Mahmudan, & Statistics Department Team"),
                          navbarPage("ARIMA Model",
                                     tabPanel("Input the Data",
                                              sidebarLayout(
                                                sidebarPanel(
                                                  fileInput("input","Input Data Here:",accept = ".txt"),
                                                  selectInput("prov","Choose Region:",choices = c("National","DKI Jakarta","West Java","Central Java","East Java")),
                                                  radioButtons("inout","Choose Division of Training and Testing:",choices = c("70:30"=0.7,"75:25"=0.75,"80:20"=0.8,"85:15"=0.85,"90:10"=0.9))
                                                ),
                                                mainPanel(
                                                  tabsetPanel(type = "pills",
                                                              tabPanel("Data Display",tableOutput("tabel")),
                                                              tabPanel("Descriptive Statistics",verbatimTextOutput("statdes"),plotOutput("plot"),plotOutput("plot1")))
                                                ))),
                                     tabPanel("Assumptions Test",
                                              sidebarLayout(
                                                sidebarPanel(
                                                  selectInput("prov1","Choose Region:",choices = c("National","DKI Jakarta","West Java","Central Java","East Java")),
                                                  radioButtons("inout1","Choose Division of Training and Testing:",choices = c("70:30"=0.7,"75:25"=0.75,"80:20"=0.8,"85:15"=0.85,"90:10"=0.9)),
                                                  br(),
                                                  h5(tags$b("Variance Stationarity Test")),
                                                  textInput("trans","Input Box-Cox Transformation Value:"),
                                                  actionButton("asumsi","Checking Assumption",class="btn-success"),
                                                  br(),
                                                  br(),
                                                  h5(tags$b("Mean Stationarity Test")),
                                                  selectInput("diff","Choose Difference Orde:",choices=c("Level","First","Second"))
                                                ),
                                                mainPanel(tabsetPanel(type="pills",
                                                                      tabPanel("Variance Stationarity Test",plotOutput("plotboxcox"),verbatimTextOutput("boxcox")),
                                                                      tabPanel("Mean Stationarity Test",plotOutput("plotdiff"),verbatimTextOutput("adf"))))
                                              )),
                                     tabPanel("ACF and PACF",
                                              sidebarLayout(
                                                sidebarPanel(
                                                  selectInput("plotpilih","Choose Plot (ACF or PACF):",choices = c("ACF","PACF"))
                                                ),
                                                mainPanel(plotOutput("plot2"))
                                              )),
                                     tabPanel("Model Estimation",
                                              sidebarLayout(
                                                sidebarPanel(
                                                  h4("Input ARIMA(p,d,q) Model:"),
                                                  textInput("ar","AR(p):"),
                                                  textInput("i","I(d):"),
                                                  textInput("ma","MA(q):"),
                                                  textInput("balik","Rounded Value (Box-Cox Transformation):"),
                                                  actionButton("model","Model",class="btn-success")
                                                ),
                                                mainPanel(verbatimTextOutput("estimasi"),plotOutput("plotin"))
                                              )),
                                     tabPanel("Model Verification",
                                              sidebarLayout(
                                                sidebarPanel(
                                                  h4("Input ARIMA(p,d,q) Model:"),
                                                  textInput("ar1","AR(p):"),
                                                  textInput("i1","I(d):"),
                                                  textInput("ma1","MA(q):"),
                                                  textInput("balik1","Rounded Value (Box-Cox Transformation):"),
                                                  actionButton("model2","Model Test",class="btn-success")
                                                ),
                                                mainPanel(verbatimTextOutput("uji"))
                                              )),
                                     tabPanel("Model Evaluation",
                                              sidebarLayout(
                                                sidebarPanel(
                                                  h4("Input ARIMA(p,d,q) Model:"),
                                                  textInput("ar2","AR(p):"),
                                                  textInput("i2","I(d):"),
                                                  textInput("ma2","MA(q):"),
                                                  textInput("balik2","Rounded Value (Box-Cox Transformation):"),
                                                  actionButton("model3","Evaluation",class="btn-success")
                                                ),
                                                mainPanel(verbatimTextOutput("evaluasi"),plotOutput("plotgabung"),plotOutput("plotramal"))
                                              ))
                                     )
                          )

#membuat server
program_ARIMA<-function(input,output){
  output$tabel<-renderTable({
    datainput=input$input
    if (is.null(datainput)){return()}
    datapakai=read.delim(datainput$datapath,header = T)
    datapakai
  })
  output$statdes<-renderPrint({
    datainput=input$input
    if (is.null(datainput)){return()}
    datapakai=read.delim(datainput$datapath,header = T)
    summary(datapakai)
  })
  output$plot<-renderPlot({
    datainput=input$input
    if (is.null(datainput)){return()}
    datapakai=read.delim(datainput$datapath,header = T)
    if (input$prov=="National"){
      covid=datapakai[,2]
      plot(covid,type = "l",col="black",main="Active Cases of COVID-19 in Indonesia")
    }
    if (input$prov=="DKI Jakarta"){
      covid=datapakai[,3]
      n=length(covid)-sum(is.na(covid))
      covid=covid[1:n]
      plot(covid,type = "l",col="black",main="Active Cases of COVID-19 in DKI Jakarta")
    }
    if (input$prov=="West Java"){
      covid=datapakai[,4]
      n=length(covid)-sum(is.na(covid))
      covid=covid[1:n]
      plot(covid,type = "l",col="black",main="Active Cases of COVID-19 in West Java")
    }
    if (input$prov=="Central Java"){
      covid=datapakai[,5]
      n=length(covid)-sum(is.na(covid))
      covid=covid[1:n]
      plot(covid,type = "l",col="black",main="Active Cases of COVID-19 in Central Java")
    }
    if (input$prov=="East Java"){
      covid=datapakai[,6]
      n=length(covid)-sum(is.na(covid))
      covid=covid[1:n]
      plot(covid,type = "l",col="black",main="Active Cases of COVID-19 in East Java")
    }
  })
  output$plot1<-renderPlot({
    datainput=input$input
    if (is.null(datainput)){return()}
    datapakai=read.delim(datainput$datapath,header = T)
    if (input$prov=="National"){
      covid=datapakai[,2]
      n=length(covid)
      ins=as.numeric(input$inout)
      insample=n*ins
      covid1=covid[1:insample]
      plot(covid1,type = "l",col="black",main="Active Cases of COVID-19 in Indonesia (Training)")
    }
    if (input$prov=="DKI Jakarta"){
      covid=datapakai[,3]
      n=length(covid)-sum(is.na(covid))
      ins=as.numeric(input$inout)
      insample=n*ins
      covid1=covid[1:insample]
      plot(covid1,type = "l",col="black",main="Active Cases of COVID-19 in DKI Jakarta (Training)")
    }
    if (input$prov=="West Java"){
      covid=datapakai[,4]
      n=length(covid)-sum(is.na(covid))
      ins=as.numeric(input$inout)
      insample=n*ins
      covid1=covid[1:insample]
      plot(covid1,type = "l",col="black",main="Active Cases of COVID-19 in West Java (Training)")
    }
    if (input$prov=="Central Java"){
      covid=datapakai[,5]
      n=length(covid)-sum(is.na(covid))
      ins=as.numeric(input$inout)
      insample=n*ins
      covid1=covid[1:insample]
      plot(covid1,type = "l",col="black",main="Active Cases of COVID-19 in Central Java (Training)")
    }
    if (input$prov=="East Java"){
      covid=datapakai[,6]
      n=length(covid)-sum(is.na(covid))
      ins=as.numeric(input$inout)
      insample=n*ins
      covid1=covid[1:insample]
      plot(covid1,type = "l",col="black",main="Active Cases of COVID-19 in East Java (Training)")
    }
  })
  observeEvent(input$asumsi,{
    datainput=input$input
    if (is.null(datainput)){return()}
    datapakai=read.delim(datainput$datapath,header = T)
    if (input$prov1=="National"){
      covid=datapakai[,2]
      n=length(covid)
      ins=as.numeric(input$inout1)
      insample=n*ins
      covid1=covid[1:insample]
      data1=as.ts(covid1)
      rounded=as.numeric(input$trans)
      data1=data1^rounded
      output$boxcox<-renderPrint({
        boxcox=powerTransform(data1) #nilai estimasi akan dibulatkan menuju rounded value
        lamda=boxcox$lambda #estimated transformation parameter (lihat di fitur help)
        rounded=boxcox$roundlam #rounded value
        cat("******************************************\n")
        cat("Here are the Results of Box-Cox Transformation Test:\n")
        cat("Lambda Value = ",lamda,"\n")
        cat("Rounded Value = ",rounded,"\n")
        cat("******************************************\n")
      })
      output$plotboxcox<-renderPlot({
        boxcox(data1~1)
      })
      output$plotdiff<-renderPlot({
        if (input$diff=="Level"){
          plot(data1)
        }
        if (input$diff=="First"){
          d=diff(data1,differences = 1)
          plot(d)
        }
        if (input$diff=="Second"){
          d=diff(data1,differences = 2)
          plot(d)
        }
      })
      output$adf<-renderPrint({
        if (input$diff=="Level"){
          print(adf.test(data1))
        }
        if (input$diff=="First"){
          d=diff(data1,differences = 1)
          print(adf.test(d))
        }
        if (input$diff=="Second"){
          d=diff(data1,differences = 2)
          print(adf.test(d))
        }
      })
    }
    if (input$prov1=="DKI Jakarta"){
      covid=datapakai[,3]
      n=length(covid)-sum(is.na(covid))
      ins=as.numeric(input$inout1)
      insample=n*ins
      covid1=covid[1:insample]
      data1=as.ts(covid1)
      rounded=as.numeric(input$trans)
      data1=data1^rounded
      output$boxcox<-renderPrint({
        boxcox=powerTransform(data1) #nilai estimasi akan dibulatkan menuju rounded value
        lamda=boxcox$lambda #estimated transformation parameter (lihat di fitur help)
        rounded=boxcox$roundlam #rounded value
        cat("******************************************\n")
        cat("Here are the Results of Box-Cox Transformation Test:\n")
        cat("Lambda Value = ",lamda,"\n")
        cat("Rounded Value = ",rounded,"\n")
        cat("******************************************\n")
      })
      output$plotboxcox<-renderPlot({
        boxcox(data1~1)
      })
      output$plotdiff<-renderPlot({
        if (input$diff=="Level"){
          plot(data1)
        }
        if (input$diff=="First"){
          d=diff(data1,difference=1)
          plot(d)
        }
        if (input$diff=="Second"){
          d=diff(data1,difference=2)
          plot(d)
        }
      })
      output$adf<-renderPrint({
        if (input$diff=="Level"){
          print(adf.test(data1))
        }
        if (input$diff=="First"){
          d=diff(data1,difference=1)
          print(adf.test(d))
        }
        if (input$diff=="Second"){
          d=diff(data1,difference=2)
          print(adf.test(d))
        }
      })
    }
    if (input$prov1=="West Java"){
      covid=datapakai[,4]
      n=length(covid)-sum(is.na(covid))
      ins=as.numeric(input$inout1)
      insample=n*ins
      covid1=covid[1:insample]
      data1=as.ts(covid1)
      rounded=as.numeric(input$trans)
      data1=data1^rounded
      output$boxcox<-renderPrint({
        boxcox=powerTransform(data1) #nilai estimasi akan dibulatkan menuju rounded value
        lamda=boxcox$lambda #estimated transformation parameter (lihat di fitur help)
        rounded=boxcox$roundlam #rounded value
        cat("******************************************\n")
        cat("Here are the Results of Box-Cox Transformation Test:\n")
        cat("Lambda Value = ",lamda,"\n")
        cat("Rounded Value = ",rounded,"\n")
        cat("******************************************\n")
      })
      output$plotboxcox<-renderPlot({
        boxcox(data1~1)
      })
      output$plotdiff<-renderPlot({
        if (input$diff=="Level"){
          plot(data1)
        }
        if (input$diff=="First"){
          d=diff(data1,difference=1)
          plot(d)
        }
        if (input$diff=="Second"){
          d=diff(data1,difference=2)
          plot(d)
        }
      })
      output$adf<-renderPrint({
        if (input$diff=="Level"){
          print(adf.test(data1))
        }
        if (input$diff=="First"){
          d=diff(data1,difference=1)
          print(adf.test(d))
        }
        if (input$diff=="Second"){
          d=diff(data1,difference=2)
          print(adf.test(d))
        }
      })
    }
    if (input$prov1=="Central Java"){
      covid=datapakai[,5]
      n=length(covid)-sum(is.na(covid))
      ins=as.numeric(input$inout1)
      insample=n*ins
      covid1=covid[1:insample]
      data1=as.ts(covid1)
      rounded=as.numeric(input$trans)
      data1=data1^rounded
      output$boxcox<-renderPrint({
        boxcox=powerTransform(data1) #nilai estimasi akan dibulatkan menuju rounded value
        lamda=boxcox$lambda #estimated transformation parameter (lihat di fitur help)
        rounded=boxcox$roundlam #rounded value
        cat("******************************************\n")
        cat("Here are the Results of Box-Cox Transformation Test:\n")
        cat("Lambda Value = ",lamda,"\n")
        cat("Rounded Value = ",rounded,"\n")
        cat("******************************************\n")
      })
      output$plotboxcox<-renderPlot({
        boxcox(data1~1)
      })
      output$plotdiff<-renderPlot({
        if (input$diff=="Level"){
          plot(data1)
        }
        if (input$diff=="First"){
          d=diff(data1,difference=1)
          plot(d)
        }
        if (input$diff=="Second"){
          d=diff(data1,difference=2)
          plot(d)
        }
      })
      output$adf<-renderPrint({
        if (input$diff=="Level"){
          print(adf.test(data1))
        }
        if (input$diff=="First"){
          d=diff(data1,difference=1)
          print(adf.test(d))
        }
        if (input$diff=="Second"){
          d=diff(data1,difference=2)
          print(adf.test(d))
        }
      })
    }
    if (input$prov1=="East Java"){
      covid=datapakai[,6]
      n=length(covid)-sum(is.na(covid))
      ins=as.numeric(input$inout1)
      insample=n*ins
      covid1=covid[1:insample]
      data1=as.ts(covid1)
      rounded=as.numeric(input$trans)
      data1=data1^rounded
      output$boxcox<-renderPrint({
        boxcox=powerTransform(data1) #nilai estimasi akan dibulatkan menuju rounded value
        lamda=boxcox$lambda #estimated transformation parameter (lihat di fitur help)
        rounded=boxcox$roundlam #rounded value
        cat("******************************************\n")
        cat("Here are the Results of Box-Cox Transformation Test:\n")
        cat("Lambda Value = ",lamda,"\n")
        cat("Rounded Value = ",rounded,"\n")
        cat("******************************************\n")
      })
      output$plotboxcox<-renderPlot({
        boxcox(data1~1)
      })
      output$plotdiff<-renderPlot({
        if (input$diff=="Level"){
          plot(data1)
        }
        if (input$diff=="First"){
          d=diff(data1,difference=1)
          plot(d)
        }
        if (input$diff=="Second"){
          d=diff(data1,difference=2)
          plot(d)
        }
      })
      output$adf<-renderPrint({
        if (input$diff=="Level"){
          print(adf.test(data1))
        }
        if (input$diff=="First"){
          d=diff(data1,difference=1)
          print(adf.test(d))
        }
        if (input$diff=="Second"){
          d=diff(data1,difference=2)
          print(adf.test(d))
        }
      })
    }
  })
    
  output$plot2<-renderPlot({
    datainput=input$input
    if (is.null(datainput)){return()}
    datapakai=read.delim(datainput$datapath,header = T)
    if (input$prov1=="National"){
      covid=datapakai[,2]
      n=length(covid)
      ins=as.numeric(input$inout1)
      insample=n*ins
      covid1=covid[1:insample]
      data1=as.ts(covid1)
      rounded=as.numeric(input$trans)
      data1=data1^rounded
      if(input$plotpilih=="ACF"){
        if (input$diff=="Level"){
          ACF=data1
          plot(acf(ACF))
        }
        if (input$diff=="First"){
          ACF=diff(data1,difference=1)
          plot(acf(ACF))
        }
        if (input$diff=="Second"){
          ACF=diff(data1,difference=2)
          plot(acf(ACF))
        }
      }
      if(input$plotpilih=="PACF"){
        if (input$diff=="Level"){
          PACF=data1
          plot(pacf(PACF))
        }
        if (input$diff=="First"){
          PACF=diff(data1,difference=1)
          plot(pacf(PACF))
        }
        if (input$diff=="Second"){
          PACF=diff(data1,difference=2)
          plot(pacf(PACF))
        }
      }
    }
    if (input$prov1=="DKI Jakarta"){
      covid=datapakai[,3]
      n=length(covid)-sum(is.na(covid))
      ins=as.numeric(input$inout1)
      insample=n*ins
      covid1=covid[1:insample]
      data1=as.ts(covid1)
      rounded=as.numeric(input$trans)
      data1=data1^rounded
      if(input$plotpilih=="ACF"){
        if (input$diff=="Level"){
          ACF=data1
          plot(acf(ACF))
        }
        if (input$diff=="First"){
          ACF=diff(data1,difference=1)
          plot(acf(ACF))
        }
        if (input$diff=="Second"){
          ACF=diff(data1,difference=2)
          plot(acf(ACF))
        }
      }
      if(input$plotpilih=="PACF"){
        if (input$diff=="Level"){
          PACF=data1
          plot(pacf(PACF))
        }
        if (input$diff=="First"){
          PACF=diff(data1,difference=1)
          plot(pacf(PACF))
        }
        if (input$diff=="Second"){
          PACF=diff(data1,difference=2)
          plot(pacf(PACF))
        }
      }
    }
    if (input$prov1=="West Java"){
      covid=datapakai[,4]
      n=length(covid)-sum(is.na(covid))
      ins=as.numeric(input$inout1)
      insample=n*ins
      covid1=covid[1:insample]
      data1=as.ts(covid1)
      rounded=as.numeric(input$trans)
      data1=data1^rounded
      if(input$plotpilih=="ACF"){
        if (input$diff=="Level"){
          ACF=data1
          plot(acf(ACF))
        }
        if (input$diff=="First"){
          ACF=diff(data1,difference=1)
          plot(acf(ACF))
        }
        if (input$diff=="Second"){
          ACF=diff(data1,difference=2)
          plot(acf(ACF))
        }
      }
      if(input$plotpilih=="PACF"){
        if (input$diff=="Level"){
          PACF=data1
          plot(pacf(PACF))
        }
        if (input$diff=="First"){
          PACF=diff(data1,difference=1)
          plot(pacf(PACF))
        }
        if (input$diff=="Second"){
          PACF=diff(data1,difference=2)
          plot(pacf(PACF))
        }
      }
    }
    if (input$prov1=="Central Java"){
      covid=datapakai[,5]
      n=length(covid)-sum(is.na(covid))
      ins=as.numeric(input$inout1)
      insample=n*ins
      covid1=covid[1:insample]
      data1=as.ts(covid1)
      rounded=as.numeric(input$trans)
      data1=data1^rounded
      if(input$plotpilih=="ACF"){
        if (input$diff=="Level"){
          ACF=data1
          plot(acf(ACF))
        }
        if (input$diff=="First"){
          ACF=diff(data1,difference=1)
          plot(acf(ACF))
        }
        if (input$diff=="Second"){
          ACF=diff(data1,difference=2)
          plot(acf(ACF))
        }
      }
      if(input$plotpilih=="PACF"){
        if (input$diff=="Level"){
          PACF=data1
          plot(pacf(PACF))
        }
        if (input$diff=="First"){
          PACF=diff(data1,difference=1)
          plot(pacf(PACF))
        }
        if (input$diff=="Second"){
          PACF=diff(data1,difference=2)
          plot(pacf(PACF))
        }
      }
    }
    if (input$prov1=="East Java"){
      covid=datapakai[,6]
      n=length(covid)-sum(is.na(covid))
      ins=as.numeric(input$inout1)
      insample=n*ins
      covid1=covid[1:insample]
      data1=as.ts(covid1)
      rounded=as.numeric(input$trans)
      data1=data1^rounded
      if(input$plotpilih=="ACF"){
        if (input$diff=="Level"){
          ACF=data1
          plot(acf(ACF))
        }
        if (input$diff=="First"){
          ACF=diff(data1,difference=1)
          plot(acf(ACF))
        }
        if (input$diff=="Second"){
          ACF=diff(data1,difference=2)
          plot(acf(ACF))
        }
      }
      if(input$plotpilih=="PACF"){
        if (input$diff=="Level"){
          PACF=data1
          plot(pacf(PACF))
        }
        if (input$diff=="First"){
          PACF=diff(data1,difference=1)
          plot(pacf(PACF))
        }
        if (input$diff=="Second"){
          PACF=diff(data1,difference=2)
          plot(pacf(PACF))
        }
      }
    }
  })
  observeEvent(input$model,{
    datainput=input$input
    if (is.null(datainput)){return()}
    datapakai=read.delim(datainput$datapath,header = T)
    if (input$prov1=="National"){
      covid=datapakai[,2]
      n=length(covid)
      ins=as.numeric(input$inout1)
      insample=n*ins
      covid1=covid[1:insample]
      balik=as.numeric(input$balik)
      data1=covid1^balik
      ar=as.numeric(input$ar)
      i<-as.numeric(input$i)
      ma<-as.numeric(input$ma)
      fit1=arima(data1,order=c(ar,i,ma),include.mean = F)
      myresid1=fit1$residuals
      predin=data1+myresid1
      MSE=mean(myresid1^2)
      MAPEin=mean(abs(myresid1/data1))*100
      output$estimasi<-renderPrint({        
        cat("*********************************************************************\n")
        cat("Here are the Results of Parameter Estimation for ARIMA(",ar,",",i,",",ma,")")
        print(coeftest(fit1))
        cat("*********************************************************************\n")
      })
      output$plotin<-renderPlot({
        plot(data1,type="p",col='black',xlim=c(1,length(data1)),ylim=c(min(data1,predin),max(data1,predin)),xlab="time(t)",ylab="data")
        title("Training Data")
        par(new=TRUE)
        plot(predin,type="l",col="red",xlim=c(1,length(data1)),ylim=c(min(data1,predin),max(data1,predin)),sub=paste("\n  MSE=",round(MSE,digit=4)," and MAPE of Training Data=",round(MAPEin,digit=4),"%"),xlab=" ",ylab=" ")
        legend("topleft",legend=c("actual","estimation"),col=c("black","red"),pch=10)
      })
    }
    if (input$prov1=="DKI Jakarta"){
      covid=datapakai[,3]
      n=length(covid)-sum(is.na(covid))
      ins=as.numeric(input$inout1)
      insample=n*ins
      covid1=covid[1:insample]
      balik=as.numeric(input$balik)
      data1=covid1^balik
      ar=as.numeric(input$ar)
      i<-as.numeric(input$i)
      ma<-as.numeric(input$ma)
      fit1=arima(data1,order=c(ar,i,ma),include.mean = F)
      myresid1=fit1$residuals
      predin=data1+myresid1
      MSE=mean(myresid1^2)
      MAPEin=mean(abs(myresid1/data1))*100
      output$estimasi<-renderPrint({        
        cat("*********************************************************************\n")
        cat("Here are the Results of Parameter Estimation for ARIMA(",ar,",",i,",",ma,")")
        print(coeftest(fit1))
        cat("*********************************************************************\n")
      })
      output$plotin<-renderPlot({
        plot(data1,type="p",col='black',xlim=c(1,length(data1)),ylim=c(min(data1,predin),max(data1,predin)),xlab="time(t)",ylab="data")
        title("Training Data")
        par(new=TRUE)
        plot(predin,type="l",col="red",xlim=c(1,length(data1)),ylim=c(min(data1,predin),max(data1,predin)),sub=paste("\n MSE=",round(MSE,digit=4)," and MAPE of Training Data=",round(MAPEin,digit=4),"%"),xlab=" ",ylab=" ")
        legend("topleft",legend=c("actual","estimation"),col=c("black","red"),pch=10)
      })
    }
    if (input$prov1=="West Java"){
      covid=datapakai[,4]
      n=length(covid)-sum(is.na(covid))
      ins=as.numeric(input$inout1)
      insample=n*ins
      covid1=covid[1:insample]
      balik=as.numeric(input$balik)
      data1=covid1^balik
      ar=as.numeric(input$ar)
      i<-as.numeric(input$i)
      ma<-as.numeric(input$ma)
      fit1=arima(data1,order=c(ar,i,ma),include.mean = F)
      myresid1=fit1$residuals
      predin=data1+myresid1
      MSE=mean(myresid1^2)
      MAPEin=mean(abs(myresid1/data1))*100
      output$estimasi<-renderPrint({        
        cat("*********************************************************************\n")
        cat("Here are the Results of Parameter Estimation for ARIMA(",ar,",",i,",",ma,")")
        print(coeftest(fit1))
        cat("*********************************************************************\n")
      })
      output$plotin<-renderPlot({
        plot(data1,type="p",col='black',xlim=c(1,length(data1)),ylim=c(min(data1,predin),max(data1,predin)),xlab="time(t)",ylab="data")
        title("Training Data")
        par(new=TRUE)
        plot(predin,type="l",col="red",xlim=c(1,length(data1)),ylim=c(min(data1,predin),max(data1,predin)),sub=paste("\n MSE=",round(MSE,digit=4)," and MAPE for Training Data=",round(MAPEin,digit=4),"%"),xlab=" ",ylab=" ")
        legend("topleft",legend=c("actual","estimation"),col=c("black","red"),pch=10)
      })
    }
    if (input$prov1=="Central Java"){
      covid=datapakai[,5]
      n=length(covid)-sum(is.na(covid))
      ins=as.numeric(input$inout1)
      insample=n*ins
      covid1=covid[1:insample]
      balik=as.numeric(input$balik)
      data1=covid1^balik
      ar=as.numeric(input$ar)
      i<-as.numeric(input$i)
      ma<-as.numeric(input$ma)
      fit1=arima(data1,order=c(ar,i,ma),include.mean = F)
      myresid1=fit1$residuals
      predin=data1+myresid1
      MSE=mean(myresid1^2)
      MAPEin=mean(abs(myresid1/data1))*100
      output$estimasi<-renderPrint({        
        cat("*********************************************************************\n")
        cat("Here are the Results of Parameter Estimation for ARIMA(",ar,",",i,",",ma,")")
        print(coeftest(fit1))
        cat("*********************************************************************\n")
      })
      output$plotin<-renderPlot({
        plot(data1,type="p",col='black',xlim=c(1,length(data1)),ylim=c(min(data1,predin),max(data1,predin)),xlab="time(t)",ylab="data")
        title("Training Data")
        par(new=TRUE)
        plot(predin,type="l",col="red",xlim=c(1,length(data1)),ylim=c(min(data1,predin),max(data1,predin)),sub=paste("\n MSE=",round(MSE,digit=4)," and MAPE for Training Data=",round(MAPEin,digit=4),"%"),xlab=" ",ylab=" ")
        legend("topleft",legend=c("actual","estimation"),col=c("black","red"),pch=10)
      })
    }
    if (input$prov1=="East Java"){
      covid=datapakai[,6]
      n=length(covid)-sum(is.na(covid))
      ins=as.numeric(input$inout1)
      insample=n*ins
      covid1=covid[1:insample]
      balik=as.numeric(input$balik)
      data1=covid1^balik
      ar=as.numeric(input$ar)
      i<-as.numeric(input$i)
      ma<-as.numeric(input$ma)
      fit1=arima(data1,order=c(ar,i,ma),include.mean = F)
      myresid1=fit1$residuals
      predin=data1+myresid1
      MSE=mean(myresid1^2)
      MAPEin=mean(abs(myresid1/data1))*100
      output$estimasi<-renderPrint({        
        cat("*********************************************************************\n")
        cat("Here are the Results of Parameter Estimation for ARIMA(",ar,",",i,",",ma,")")
        print(coeftest(fit1))
        cat("*********************************************************************\n")
      })
      output$plotin<-renderPlot({
        plot(data1,type="p",col='black',xlim=c(1,length(data1)),ylim=c(min(data1,predin),max(data1,predin)),xlab="time(t)",ylab="data")
        title("Training Data")
        par(new=TRUE)
        plot(predin,type="l",col="red",xlim=c(1,length(data1)),ylim=c(min(data1,predin),max(data1,predin)),sub=paste("\n MSE=",round(MSE,digit=4)," and MAPE for Training Data=",round(MAPEin,digit=4),"%"),xlab=" ",ylab=" ")
        legend("topleft",legend=c("actual","estimation"),col=c("black","red"),pch=10)
      })
    }
  })
  observeEvent(input$model2,{
    datainput=input$input
    if (is.null(datainput)){return()}
    datapakai=read.delim(datainput$datapath,header = T)
    if (input$prov1=="National"){
      covid=datapakai[,2]
      n=length(covid)
      ins=as.numeric(input$inout1)
      insample=n*ins
      covid1=covid[1:insample]
      balik=as.numeric(input$balik1)
      data1=covid1^balik
      ar=as.numeric(input$ar1)
      i<-as.numeric(input$i1)
      ma<-as.numeric(input$ma1)
      fit1=arima(data1,order=c(ar,i,ma),include.mean = F)
      myresid1=fit1$residuals
      jb=jarque.bera.test(myresid1)
      ljb=LjungBoxTest(myresid1,lag.max = 50)
      Aic=fit1$aic
      output$uji<-renderPrint({        
        cat("***********************************************\n")
        cat("Results of Verification Model:\n")
        cat("================================================\n")
        cat("Normality of Residuals Test\n")
        print(jb)
        cat("================================================\n")
        cat("Independent of Residuals Test\n")
        print(ljb)
        cat("================================================\n")
        cat("AIC = ",Aic,"\n")
        cat("***********************************************")
      })
    }
    if (input$prov1=="DKI Jakarta"){
      covid=datapakai[,3]
      n=length(covid)-sum(is.na(covid))
      ins=as.numeric(input$inout1)
      insample=n*ins
      covid1=covid[1:insample]
      balik=as.numeric(input$balik1)
      data1=covid1^balik
      ar=as.numeric(input$ar1)
      i<-as.numeric(input$i1)
      ma<-as.numeric(input$ma1)
      fit1=arima(data1,order=c(ar,i,ma),include.mean = F)
      myresid1=fit1$residuals
      jb=jarque.bera.test(myresid1)
      ljb=LjungBoxTest(myresid1,lag.max = 50)
      Aic=fit1$aic
      output$uji<-renderPrint({        
        cat("***********************************************\n")
        cat("Results of Verification Model:\n")
        cat("================================================\n")
        cat("Normality of Residuals Test\n")
        print(jb)
        cat("================================================\n")
        cat("Independent of Residuals Test\n")
        print(ljb)
        cat("================================================\n")
        cat("AIC = ",Aic,"\n")
        cat("***********************************************")
      })
    }
    if (input$prov1=="West Java"){
      covid=datapakai[,4]
      n=length(covid)-sum(is.na(covid))
      ins=as.numeric(input$inout1)
      insample=n*ins
      covid1=covid[1:insample]
      balik=as.numeric(input$balik1)
      data1=covid1^balik
      ar=as.numeric(input$ar1)
      i<-as.numeric(input$i1)
      ma<-as.numeric(input$ma1)
      fit1=arima(data1,order=c(ar,i,ma),include.mean = F)
      myresid1=fit1$residuals
      jb=jarque.bera.test(myresid1)
      ljb=LjungBoxTest(myresid1,lag.max = 50)
      Aic=fit1$aic
      output$uji<-renderPrint({        
        cat("***********************************************\n")
        cat("Results of Verification Model:\n")
        cat("================================================\n")
        cat("Normality of Residuals Test\n")
        print(jb)
        cat("================================================\n")
        cat("Independent of Residuals Test\n")
        print(ljb)
        cat("================================================\n")
        cat("AIC = ",Aic,"\n")
        cat("***********************************************")
      })
    }
    if (input$prov1=="Central Java"){
      covid=datapakai[,5]
      n=length(covid)-sum(is.na(covid))
      ins=as.numeric(input$inout1)
      insample=n*ins
      covid1=covid[1:insample]
      balik=as.numeric(input$balik1)
      data1=covid1^balik
      ar=as.numeric(input$ar1)
      i<-as.numeric(input$i1)
      ma<-as.numeric(input$ma1)
      fit1=arima(data1,order=c(ar,i,ma),include.mean = F)
      myresid1=fit1$residuals
      jb=jarque.bera.test(myresid1)
      ljb=LjungBoxTest(myresid1,lag.max = 50)
      Aic=fit1$aic
      output$uji<-renderPrint({        
        cat("***********************************************\n")
        cat("Results of Verification Model:\n")
        cat("================================================\n")
        cat("Normality of Residuals Test\n")
        print(jb)
        cat("================================================\n")
        cat("Independent of Residuals Test\n")
        print(ljb)
        cat("================================================\n")
        cat("AIC = ",Aic,"\n")
        cat("***********************************************")
      })
    }
    if (input$prov1=="East Java"){
      covid=datapakai[,6]
      n=length(covid)-sum(is.na(covid))
      ins=as.numeric(input$inout1)
      insample=n*ins
      covid1=covid[1:insample]
      balik=as.numeric(input$balik1)
      data1=covid1^balik
      ar=as.numeric(input$ar1)
      i<-as.numeric(input$i1)
      ma<-as.numeric(input$ma1)
      fit1=arima(data1,order=c(ar,i,ma),include.mean = F)
      myresid1=fit1$residuals
      jb=jarque.bera.test(myresid1)
      ljb=LjungBoxTest(myresid1,lag.max = 50)
      Aic=fit1$aic
      output$uji<-renderPrint({        
        cat("***********************************************\n")
        cat("Results of Verification Model:\n")
        cat("================================================\n")
        cat("Normality of Residuals Test\n")
        print(jb)
        cat("================================================\n")
        cat("Independent of Residuals Test\n")
        print(ljb)
        cat("================================================\n")
        cat("AIC = ",Aic,"\n")
        cat("***********************************************")
      })
    }
  }) 
  observeEvent(input$model3,{
    datainput=input$input
    if (is.null(datainput)){return()}
    datapakai=read.delim(datainput$datapath,header = T)
    if (input$prov1=="National"){
      covid=datapakai[,2]
      n=length(covid)
      ins=as.numeric(input$inout1)
      insample=n*ins
      covid1=covid[1:insample]
      balik=as.numeric(input$balik2)
      data1=covid1^balik
      ar=as.numeric(input$ar2)
      i<-as.numeric(input$i2)
      ma<-as.numeric(input$ma2)
      fit1=arima(data1,order=c(ar,i,ma),include.mean = F)
      myresid1=fit1$residuals
      outsample=1+insample
      dataout=covid[outsample:n]
      n1=length(dataout)
      predik1<-predict(fit1,n.ahead=n1)
      predik1<-(predik1$pred)^(1/balik)
      pr=data1+myresid1
      pr1=pr^(1/balik)
      ress=pr1-covid1
      prediksi=covid1+ress
      pred=c(predik1,dataout)
      matriks=matrix(pred,nrow=n1,ncol=2)
      outsamp=matriks[,2]
      prediks=matriks[,1]
      MAPE=mean(abs(outsamp-prediks)/outsamp)*100
      databaru=c(prediksi,prediks)
      output$evaluasi<-renderPrint({        
        cat("***********************************************\n")
        cat("Here are Results of Model Evaluation:\n")
        cat("================================================\n")
        cat("   Forecasting    Testing\n")
        print(matriks)
        cat("================================================\n")
        cat("MAPE = ",MAPE,"%\n")
        cat("***********************************************")
      })
      output$plotramal<-renderPlot({
        plot(outsamp,type="p",col='black',xlim=c(1,length(outsamp)),ylim=c(min(outsamp,prediks),max(outsamp,prediks)),xlab="time(t)",ylab="data")
        title("Testing Data")
        par(new=TRUE)
        plot(prediks,type="l",col="blue",xlim=c(1,length(outsamp)),ylim=c(min(outsamp,prediks),max(outsamp,prediks)),sub=paste("\n MAPE=",round(MAPE,digit=4),"%"),xlab=" ",ylab=" ")
        legend("topleft",legend=c("actual","forecasting"),col=c("black","blue"),pch=10)
      })
      output$plotgabung<-renderPlot({
        plot(covid1,type="p",col='black',xlim=c(1,length(databaru)),ylim=c(min(covid1,databaru),max(covid1,databaru)),xlab="time(t)",ylab="data")
        title("Forecasting Result")
        par(new=TRUE)
        plot(databaru,type="l",col="blue",xlim=c(1,length(databaru)),ylim=c(min(covid1,databaru),max(covid1,databaru)),xlab=" ",ylab=" ")
        par(new=TRUE)
        plot(prediksi,type="l",col="red",xlim=c(1,length(databaru)),ylim=c(min(covid1,databaru),max(covid1,databaru)),xlab=" ",ylab=" ")
        legend("topleft",legend=c("actual","estimation","forecasting"),col=c("black","red","blue"),pch=10)
      })
    }
    if (input$prov1=="DKI Jakarta"){
      covid=datapakai[,3]
      n=length(covid)-sum(is.na(covid))
      ins=as.numeric(input$inout1)
      insample=n*ins
      covid1=covid[1:insample]
      balik=as.numeric(input$balik2)
      data1=covid1^balik
      ar=as.numeric(input$ar2)
      i<-as.numeric(input$i2)
      ma<-as.numeric(input$ma2)
      fit1=arima(data1,order=c(ar,i,ma),include.mean = F)
      myresid1=fit1$residuals
      outsample=1+insample
      dataout=covid[outsample:n]
      n1=length(dataout)
      predik1<-predict(fit1,n.ahead=n1)
      predik1<-(predik1$pred)^(1/balik)
      pr=data1+myresid1
      pr1=pr^(1/balik)
      ress=pr1-covid1
      prediksi=covid1+ress
      pred=c(predik1,dataout)
      matriks=matrix(pred,nrow=n1,ncol=2)
      outsamp=matriks[,2]
      prediks=matriks[,1]
      MAPE=mean(abs(outsamp-prediks)/outsamp)*100
      databaru=c(prediksi,prediks)
      output$evaluasi<-renderPrint({        
        cat("***********************************************\n")
        cat("Here are Results of Model Evaluation:\n")
        cat("================================================\n")
        cat("   Forecasting    Testing\n")
        print(matriks)
        cat("================================================\n")
        cat("MAPE = ",MAPE,"%\n")
        cat("***********************************************")
      })
      output$plotramal<-renderPlot({
        plot(outsamp,type="p",col='black',xlim=c(1,length(outsamp)),ylim=c(min(outsamp,prediks),max(outsamp,prediks)),xlab="time(t)",ylab="data")
        title("Testing Data")
        par(new=TRUE)
        plot(prediks,type="l",col="blue",xlim=c(1,length(outsamp)),ylim=c(min(outsamp,prediks),max(outsamp,prediks)),sub=paste("\n MAPE=",round(MAPE,digit=4),"%"),xlab=" ",ylab=" ")
        legend("topleft",legend=c("actual","forecasting"),col=c("black","blue"),pch=10)
      })
      output$plotgabung<-renderPlot({
        plot(covid1,type="p",col='black',xlim=c(1,length(databaru)),ylim=c(min(covid1,databaru),max(covid1,databaru)),xlab="time(t)",ylab="data")
        title("Forecasting Result")
        par(new=TRUE)
        plot(databaru,type="l",col="blue",xlim=c(1,length(databaru)),ylim=c(min(covid1,databaru),max(covid1,databaru)),xlab=" ",ylab=" ")
        par(new=TRUE)
        plot(prediksi,type="l",col="red",xlim=c(1,length(databaru)),ylim=c(min(covid1,databaru),max(covid1,databaru)),xlab=" ",ylab=" ")
        legend("topleft",legend=c("actual","estimation","forecasting"),col=c("black","red","blue"),pch=10)
      })
    }
    if (input$prov1=="West Java"){
      covid=datapakai[,4]
      n=length(covid)-sum(is.na(covid))
      ins=as.numeric(input$inout1)
      insample=n*ins
      covid1=covid[1:insample]
      balik=as.numeric(input$balik2)
      data1=covid1^balik
      ar=as.numeric(input$ar2)
      i<-as.numeric(input$i2)
      ma<-as.numeric(input$ma2)
      fit1=arima(data1,order=c(ar,i,ma),include.mean = F)
      myresid1=fit1$residuals
      outsample=1+insample
      dataout=covid[outsample:n]
      n1=length(dataout)
      predik1<-predict(fit1,n.ahead=n1)
      predik1<-(predik1$pred)^(1/balik)
      pr=data1+myresid1
      pr1=pr^(1/balik)
      ress=pr1-covid1
      prediksi=covid1+ress
      pred=c(predik1,dataout)
      matriks=matrix(pred,nrow=n1,ncol=2)
      outsamp=matriks[,2]
      prediks=matriks[,1]
      MAPE=mean(abs(outsamp-prediks)/outsamp)*100
      databaru=c(prediksi,prediks)
      output$evaluasi<-renderPrint({        
        cat("***********************************************\n")
        cat("Here are Results of Model Evaluation:\n")
        cat("================================================\n")
        cat("   Forecasting    Testing\n")
        print(matriks)
        cat("================================================\n")
        cat("MAPE = ",MAPE,"%\n")
        cat("***********************************************")
      })
      output$plotramal<-renderPlot({
        plot(outsamp,type="p",col='black',xlim=c(1,length(outsamp)),ylim=c(min(outsamp,prediks),max(outsamp,prediks)),xlab="time(t)",ylab="data")
        title("Testing Data")
        par(new=TRUE)
        plot(prediks,type="l",col="blue",xlim=c(1,length(outsamp)),ylim=c(min(outsamp,prediks),max(outsamp,prediks)),sub=paste("\n MAPE=",round(MAPE,digit=4),"%"),xlab=" ",ylab=" ")
        legend("topleft",legend=c("actual","forecasting"),col=c("black","blue"),pch=10)
      })
      output$plotgabung<-renderPlot({
        plot(covid1,type="p",col='black',xlim=c(1,length(databaru)),ylim=c(min(covid1,databaru),max(covid1,databaru)),xlab="time(t)",ylab="data")
        title("Forecasting Result")
        par(new=TRUE)
        plot(databaru,type="l",col="blue",xlim=c(1,length(databaru)),ylim=c(min(covid1,databaru),max(covid1,databaru)),xlab=" ",ylab=" ")
        par(new=TRUE)
        plot(prediksi,type="l",col="red",xlim=c(1,length(databaru)),ylim=c(min(covid1,databaru),max(covid1,databaru)),xlab=" ",ylab=" ")
        legend("topleft",legend=c("actual","estimation","forecasting"),col=c("black","red","blue"),pch=10)
      })
    }
    if (input$prov1=="Central Java"){
      covid=datapakai[,5]
      n=length(covid)-sum(is.na(covid))
      ins=as.numeric(input$inout1)
      insample=n*ins
      covid1=covid[1:insample]
      balik=as.numeric(input$balik2)
      data1=covid1^balik
      ar=as.numeric(input$ar2)
      i<-as.numeric(input$i2)
      ma<-as.numeric(input$ma2)
      fit1=arima(data1,order=c(ar,i,ma),include.mean = F)
      myresid1=fit1$residuals
      outsample=1+insample
      dataout=covid[outsample:n]
      n1=length(dataout)
      predik1<-predict(fit1,n.ahead=n1)
      predik1<-(predik1$pred)^(1/balik)
      pr=data1+myresid1
      pr1=pr^(1/balik)
      ress=pr1-covid1
      prediksi=covid1+ress
      pred=c(predik1,dataout)
      matriks=matrix(pred,nrow=n1,ncol=2)
      outsamp=matriks[,2]
      prediks=matriks[,1]
      MAPE=mean(abs(outsamp-prediks)/outsamp)*100
      databaru=c(prediksi,prediks)
      output$evaluasi<-renderPrint({        
        cat("***********************************************\n")
        cat("Here are Results of Model Evaluation:\n")
        cat("================================================\n")
        cat("   Forecasting    Testing\n")
        print(matriks)
        cat("================================================\n")
        cat("MAPE = ",MAPE,"%\n")
        cat("***********************************************")
      })
      output$plotramal<-renderPlot({
        plot(outsamp,type="p",col='black',xlim=c(1,length(outsamp)),ylim=c(min(outsamp,prediks),max(outsamp,prediks)),xlab="time(t)",ylab="data")
        title("Testing Data")
        par(new=TRUE)
        plot(prediks,type="l",col="blue",xlim=c(1,length(outsamp)),ylim=c(min(outsamp,prediks),max(outsamp,prediks)),sub=paste("\n MAPE=",round(MAPE,digit=4),"%"),xlab=" ",ylab=" ")
        legend("topleft",legend=c("actual","forecasting"),col=c("black","blue"),pch=10)
      })
      output$plotgabung<-renderPlot({
        plot(covid1,type="p",col='black',xlim=c(1,length(databaru)),ylim=c(min(covid1,databaru),max(covid1,databaru)),xlab="time(t)",ylab="data")
        title("Forecasting Result")
        par(new=TRUE)
        plot(databaru,type="l",col="blue",xlim=c(1,length(databaru)),ylim=c(min(covid1,databaru),max(covid1,databaru)),xlab=" ",ylab=" ")
        par(new=TRUE)
        plot(prediksi,type="l",col="red",xlim=c(1,length(databaru)),ylim=c(min(covid1,databaru),max(covid1,databaru)),xlab=" ",ylab=" ")
        legend("topleft",legend=c("actual","estimation","forecasting"),col=c("black","red","blue"),pch=10)
      })
    }
    if (input$prov1=="East Java"){
      covid=datapakai[,6]
      n=length(covid)-sum(is.na(covid))
      ins=as.numeric(input$inout1)
      insample=n*ins
      covid1=covid[1:insample]
      balik=as.numeric(input$balik2)
      data1=covid1^balik
      ar=as.numeric(input$ar2)
      i<-as.numeric(input$i2)
      ma<-as.numeric(input$ma2)
      fit1=arima(data1,order=c(ar,i,ma),include.mean = F)
      myresid1=fit1$residuals
      outsample=1+insample
      dataout=covid[outsample:n]
      n1=length(dataout)
      predik1<-predict(fit1,n.ahead=n1)
      predik1<-(predik1$pred)^(1/balik)
      pr=data1+myresid1
      pr1=pr^(1/balik)
      ress=pr1-covid1
      prediksi=covid1+ress
      pred=c(predik1,dataout)
      matriks=matrix(pred,nrow=n1,ncol=2)
      outsamp=matriks[,2]
      prediks=matriks[,1]
      MAPE=mean(abs(outsamp-prediks)/outsamp)*100
      databaru=c(prediksi,prediks)
      output$evaluasi<-renderPrint({        
        cat("***********************************************\n")
        cat("Here are Results of Model Evaluation:\n")
        cat("================================================\n")
        cat("   Forecasting    Testing\n")
        print(matriks)
        cat("================================================\n")
        cat("MAPE = ",MAPE,"%\n")
        cat("***********************************************")
      })
      output$plotramal<-renderPlot({
        plot(outsamp,type="p",col='black',xlim=c(1,length(outsamp)),ylim=c(min(outsamp,prediks),max(outsamp,prediks)),xlab="time(t)",ylab="data")
        title("Testing Data")
        par(new=TRUE)
        plot(prediks,type="l",col="blue",xlim=c(1,length(outsamp)),ylim=c(min(outsamp,prediks),max(outsamp,prediks)),sub=paste("\n MAPE=",round(MAPE,digit=4),"%"),xlab=" ",ylab=" ")
        legend("topleft",legend=c("actual","forecasting"),col=c("black","blue"),pch=10)
      })
      output$plotgabung<-renderPlot({
        plot(covid1,type="p",col='black',xlim=c(1,length(databaru)),ylim=c(min(covid1,databaru),max(covid1,databaru)),xlab="time(t)",ylab="data")
        title("Forecasting Result")
        par(new=TRUE)
        plot(databaru,type="l",col="blue",xlim=c(1,length(databaru)),ylim=c(min(covid1,databaru),max(covid1,databaru)),xlab=" ",ylab=" ")
        par(new=TRUE)
        plot(prediksi,type="l",col="red",xlim=c(1,length(databaru)),ylim=c(min(covid1,databaru),max(covid1,databaru)),xlab=" ",ylab=" ")
        legend("topleft",legend=c("actual","estimation","forecasting"),col=c("black","red","blue"),pch=10)
      })
    }
  })
}

#Running App
shinyApp(ui=tampilan_ARIMA,server=program_ARIMA)