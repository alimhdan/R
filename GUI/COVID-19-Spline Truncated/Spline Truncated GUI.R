library(shiny)
library(shinythemes)

#Matriks
MPL=function(x,eps=1e-009)
{
  x=as.matrix(x)
  xsvd=svd(x)
  diago=xsvd$d[xsvd$d>eps]
  if(length(diago)==1)
  {
    xplus=as.matrix(xsvd$v[,1])%*%t(as.matrix(xsvd$u[,1])/diago)
  }
  else
  { 
    xplus=xsvd$v[,1:length(diago)]%*%diag(1/diago)%*%t(xsvd$u[,1:length(diago)])
  }
  return(xplus)
}

#power=derajat=orde-1
trun=function(data,c,power)
{
  data[data<c]=c
  (data-c)^power
}

#gcv 1 titik knot
#m=orde
#k : ttk knot pada data
#k=seq(min(x)+0.1,max(x)-0.1,length=10)
gcv1<-function(x,y,m)
{
  n=length(y)
  r<-length(x)
  k<-x[-c(1,r)] #mengecualikan titik x pertama dan terakhir sebagai calon titik knot
  v<-length(k)
  Gcv<-matrix(nrow=v,ncol=1)
  for (j in 1:v)
  {
    w<-matrix(0,ncol=m+1,nrow=n)
    for (i in 1:m)
      w[,i]<-x^(i-1)
    for (i in m+1)
      w[,i]<-trun(x,k[j],m-1)	
    wtw<- t(w) %*% w
    z<- MPL(wtw) 
    beta<- z %*% (t(w) %*% y)
    h<- w %*% z %*% t(w)
    mu<-w%*%beta
    MSE<- t(y-mu) %*% (y-mu)/n
    I<-matrix(0,ncol=n,nrow=n)
    for(i in 1: n)
      I[i,i]<-1
    GCV<-(n^2*MSE)/(sum(diag(I-h)))^2
    Gcv[j]<-GCV
  }
  R<-matrix(c(k,Gcv),nrow=v)
  sort.R<-R[order(R[,2]),]
  S=sort.R[1:20,]
  cat("for spline with orde",m," and 1 knot, optimal knot =",S[1,1],"with minimum GCV =",S[1,2])
  cat("\n20 lowest of GCV and knots result:\n")
  cat("======================\n")
  cat("  No    knot   GCV\n")
  cat("======================\n")
  print(S)
}

#gcv 2 titik knot
gcv2=function(x,y,m)
{
  n=length(y)
  r<-length(x)
  t=x[-c(1,r)]
  comb=combn(t,2,fun=NULL)
  k=t(comb)
  k1=k[,1]
  k2=k[,2]
  v=length(k1)
  n=length(x)
  Gcv=matrix(nrow=v,ncol=1)
  for(j in 1:v)
  {
    w=matrix(0,ncol=m+2,nrow=n)
    for(i in 1:m)
      w[,i]=x^(i-1)
    for(i in m+1)
      w[,i]=trun(x,k1[j],m-1)
    for(i in m+2)
      w[,i]=trun(x,k2[j],m-1)
    wtw=t(w)%*%w
    z=MPL(wtw)
    beta=z%*%(t(w)%*%y)
    h=w%*%z%*%t(w)
    mu=w%*%beta
    MSE=t(y-mu)%*%(y-mu)/n
    I=matrix(0,ncol=n,nrow=n)
    for(i in 1:n)
      I[i,i]=1
    GCV=(n^2*MSE)/(sum(diag(I-h)))^2
    Gcv[j]=GCV
  }
  R=matrix(c(k1,k2,Gcv),nrow=v)
  sort.R=R[order(R[,3]),]
  S=sort.R[1:20,]
  cat("for spline with orde",m," and 2 knots, optimal knots = c(",S[1,1],",",S[1,2],") with minimum GCV =",S[1,3])
  cat("\n20 lowest of GCV and knots result:\n")
  cat("=========================\n")
  cat("  No knot1  knot2   GCV\n")
  cat("=========================\n")
  print(S)
}

model.spline<-function(prediktor,respon,m,knots=c(...))
{
  y<-respon
  n<-length(y)
  k<-length(knots)
  w<-matrix(0, ncol=m+k, nrow=n)
  for (i in 1:m)
    w[,i]<-prediktor^(i-1)
  for(i in (m+1):(m+k))
    w[,i]<-trun(prediktor,knots[i-m],m-1)
  wtw<-t(w)%*%w
  z=MPL(wtw)
  beta=z%*%(t(w)%*%y)
  h=w%*%z%*%t(w)
  yfits=w%*%beta
  MSE=t(y-yfits)%*%(y-yfits)/n
  R2=1-(sum((y-yfits)^2)/sum((y-mean(y))^2))
  MAPE=mean(abs(y-yfits)/y)*100
  I=matrix(0,ncol=n,nrow=n)
  for(i in 1:n)
    I[i,i]=1
  GCV<-(n^2*MSE)/((sum(diag(I-h)))^2)
  q<-seq(min(prediktor),max(prediktor),length=1000)
  u<-matrix(0,ncol=m+k,nrow=1000)
  for(i in 1:m)
    u[,i]<-q^(i-1)
  for(i in (m+1):(m+k))
    u[,i]<-trun(q,knots[i-m],m-1)
  festu<-u%*%beta
  cat("\n Spline orde",m)
  cat("\n Knots  = c( ",format(knots),")")
  cat("\ GCV  = ",format(GCV),"\n")
  cat("\n **************************************************")
  cat("\n      Coefficient         Estimation")
  cat("\n **************************************************")
  for(i in 1:(m+k))
    cat("\n     beta[",i-1,"]	          ",format(beta[i]))	
  cat("\n **************************************************")
  cat("\n GCV=",GCV,"")
  cat('\n MSE=',MSE,'')
  cat('\n R-square=',R2,'')
  cat('\n MAPE training=',MAPE,'%')
  plot(prediktor,respon, type="p",xlim=c(min(prediktor),max(prediktor)),
       ylim=c(min(respon)-5,max(respon)+5),
       xlab="x",ylab="y")
  title("Spline Fit")
  par(new=T)
  plot(q,festu, type="l",col="red",
       xlim=c(min(prediktor),max(prediktor)),
       ylim=c(min(respon)-5,max(respon)+5),
       xlab="  ",ylab="  ")
  #plot spline pada runtun waktu
  plot(y,type="p",col='black',ylim=c(min(y,yfits),max(y,yfits)),xlab="time(t)",ylab="data")
  title("Data Training")
  par(new=TRUE)
  plot(yfits,type="l",col="red",ylim=c(min(y,yfits),max(y,yfits)),xlab=" ",ylab=" ")
  legend("topleft",legend=c("actual","estimation"),col=c("black","red"),pch=10)
}

model.spline1<-function(prediktor,respon,m,knots=c(...))
{
  y<-respon
  n<-length(y)
  k<-length(knots)
  w<-matrix(0, ncol=m+k, nrow=n)
  for (i in 1:m)
    w[,i]<-prediktor^(i-1)
  for(i in (m+1):(m+k))
    w[,i]<-trun(prediktor,knots[i-m],m-1)
  wtw<-t(w)%*%w
  z=MPL(wtw)
  beta=z%*%(t(w)%*%y)
  h=w%*%z%*%t(w)
  yfits=w%*%beta
  MSE=t(y-yfits)%*%(y-yfits)/n
  R2=1-(sum((y-yfits)^2)/sum((y-mean(y))^2))
  I=matrix(0,ncol=n,nrow=n)
  for(i in 1:n)
    I[i,i]=1
  GCV<-(n^2*MSE)/((sum(diag(I-h)))^2)
  q<-seq(min(prediktor),max(prediktor),length=1000)
  u<-matrix(0,ncol=m+k,nrow=1000)
  for(i in 1:m)
    u[,i]<-q^(i-1)
  for(i in (m+1):(m+k))
    u[,i]<-trun(q,knots[i-m],m-1)
  festu<-u%*%beta
  cat("\n Spline orde",m)
  cat("\n Knots  = c( ",format(knots),")")
  cat("\ GCV  = ",format(GCV),"\n")
  cat("\n **************************************************")
  cat("\n      Coefficient         Estimation")
  cat("\n **************************************************")
  for(i in 1:(m+k))
    cat("\n     beta[",i-1,"]	          ",format(beta[i]))	
  cat("\n **************************************************")
  cat("\n GCV=",GCV,"")
  cat('\n MSE=',MSE,'')
  cat('\n R-square=',R2,'')
  #plot spline pada runtun waktu
  plot(prediktor,respon, type="p",xlim=c(min(prediktor),max(prediktor)),
       ylim=c(min(respon)-5,max(respon)+5),
       xlab="x",ylab="y")
  title("Spline Fit")
  par(new=T)
  plot(q,festu, type="l",col="red",
       xlim=c(min(prediktor),max(prediktor)),
       ylim=c(min(respon)-5,max(respon)+5),
       xlab="  ",ylab="  ")
}

spline.out<-function(x,y,x1,y1,m,knots=c(...))
{
  #m:orde
  v<-length(y1)
  n<-length(y)
  k<-length(knots)
  w<-matrix(0, ncol=m+k, nrow=n)
  for (i in 1:m)
    w[,i]<-x^(i-1)
  for(i in (m+1):(m+k))
    w[,i]<-trun(x,knots[i-m],m-1)
  wtw<-t(w)%*%w
  z<-MPL(wtw)
  beta<-z%*%t(w)%*%y
  h<-w%*%z%*%t(w)
  wbaru<-matrix(0, ncol=m+k, nrow=v)
  for (i in 1:m)
    wbaru[,i]<-x1^(i-1)
  for(i in (m+1):(m+k))
    wbaru[,i]<-trun(x1,knots[i-m],m-1)
  yhat<-wbaru%*%beta
  mape_out=mean(abs((y1-yhat)/y1))*100
  akt=matrix(y1,ncol=1)
  est=matrix(yhat,ncol=1)
  prediksi=matrix(c(akt,est),ncol=2)
  cat("======================================\n")
  cat("MAPE testing=",round(mape_out,digit=4),"%\n")
  cat("======================================\n")
  cat("   Actual    Estimation\n")
  cat("======================================\n")
  print(prediksi)
  cat("======================================\n")
  plot(y1,type="p",col='black',ylim=c(min(y1,yhat),max(y1,yhat)),xlab="time (t)",ylab="data")
  title("Data Testing")
  par(new=TRUE)
  plot(yhat,type="l",col="red",ylim=c(min(y1,yhat),max(y1,yhat)),xlab=" ",sub=paste("\nMAPE=",round(mape_out,digits=4),"%"),ylab=" ")
  legend("topleft",legend=c("actual","estimation"),col=c("black","red"),pch=10)
}


#membuat UI
tampilan<-fluidPage(theme = shinytheme("cerulean"),
                    titlePanel(tags$b("Spline Truncated Regression for Modeling Active Cases of COVID-19 in Indonesia")),
                    h4("by: Dr. Budi Warsito, S.Si., M.Si., Ali Mahmudan, & Statistics Department Team"),
                    navbarPage("Time Series Modeling",
                               tabPanel("Input Data",
                                        sidebarLayout(
                                          sidebarPanel(
                                            fileInput("input","Input data here:",accept = ".txt"),
                                            selectInput("prov","Choose Region:",choices = c("National","DKI Jakarta","West Java","Central Java","East Java")),
                                            radioButtons("inout","Choose Division of Training and Testing:",choices = c("70:30"=0.7,"75:25"=0.75,"80:20"=0.8,"85:15"=0.85,"90:10"=0.9))
                                          ),
                                          mainPanel(
                                            tabsetPanel(type = "pills",
                                                        tabPanel("Data Display",tableOutput("tabel")),
                                                        tabPanel("Descriptive Statistics",verbatimTextOutput("statdes"),plotOutput("plot"),plotOutput("plot1")))
                                        ))),
                               tabPanel("ACF and PACF",
                                        sidebarLayout(
                                          sidebarPanel(
                                            selectInput("plotpilih","Choose Plot (ACF or PACF):",choices = c("Plot ACF","Plot PACF")),
                                            br(),
                                            p("*The nonparametric time series model is modeled based on the significant lag in the Partial Autocorrelation Function (PACF), if the lag is significant at the q-th lag, the predictor variable is Zt-q and the response variable is Zt (Hendrian, 2021)")
                                          ),
                                        mainPanel(plotOutput("plot2"))
                                        )),
                               tabPanel("Optimal Knot",
                                        sidebarLayout(
                                          sidebarPanel(
                                            h4("Choose the Number of Optimal Knot and Orde"),
                                            textInput("lags","Input Number of Significant Lag:"),
                                            radioButtons("order","Choose the Number of Orde:",list("2","3","4","5")),
                                            actionButton("check","Check",class="btn-success")
                                          ),
                                          mainPanel(
                                            tabsetPanel(type="hidden",
                                                        tabPanel("Optimal Knot",verbatimTextOutput("hasilknot1"),verbatimTextOutput("hasilknot2")))
                                        ))),
                               tabPanel("Regression Model",
                                        sidebarLayout(
                                          sidebarPanel(
                                            h4("Regression Modeling Based on Optimal Knot and Orde"),
                                            selectInput("knot1","Number of Knot:",choices = c("One","Two")),
                                            radioButtons("order1","Number of Orde:",list("2","3","4","5")),
                                            textInput("knotoptimal","Input knot 1:"),
                                            textInput("knotoptimall","Input knot 2:"),
                                            actionButton("model","Model",class="btn-success")
                                          ),
                                          mainPanel(
                                            tabsetPanel(type = "hidden",
                                                        tabPanel("Estimation",verbatimTextOutput("estimasi"),plotOutput("plottime"),plotOutput("plotmodel"))
                                          ))
                                        )),
                               tabPanel("Model Evaluation",
                                        sidebarLayout(
                                          sidebarPanel(
                                            h4("Model Evaluation Based on Optimal Knot and Orde"),
                                            selectInput("knot2","Number of Knot:",choices = c("One","Two")),
                                            radioButtons("order2","Number of Orde:",list("2","3","4","5")),
                                            textInput("knotoptimal1","Input knot 1:"),
                                            textInput("knotoptimal2","Input knot 2:"),
                                            actionButton("model1","Evaluation",class="btn-success")
                                          ),
                                          mainPanel(
                                            tabsetPanel(type = "hidden",
                                                        tabPanel("Forecasting",verbatimTextOutput("peramalan"),plotOutput("plotmodel1"))
                                          ))
                               )
                    )))
#membuat Server
program<-function(input,output){
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
  output$plot2<-renderPlot({
    datainput=input$input
    if (is.null(datainput)){return()}
    datapakai=read.delim(datainput$datapath,header = T)
    if (input$prov=="National"){
      covid=datapakai[,2]
      n=length(covid)
      ins=as.numeric(input$inout)
      insample=n*ins
      covid1=covid[1:insample]
      if(input$plotpilih=="Plot ACF"){
        acf(covid1)
      }
      if(input$plotpilih=="Plot PACF"){
        pacf(covid1)
      }
    }
    if (input$prov=="DKI Jakarta"){
      covid=datapakai[,3]
      n=length(covid)-sum(is.na(covid))
      ins=as.numeric(input$inout)
      insample=n*ins
      covid1=covid[1:insample]
      if(input$plotpilih=="Plot ACF"){
        acf(covid1)
      }
      if(input$plotpilih=="Plot PACF"){
        pacf(covid1)
      }
    }
    if (input$prov=="West Java"){
      covid=datapakai[,4]
      n=length(covid)-sum(is.na(covid))
      ins=as.numeric(input$inout)
      insample=n*ins
      covid1=covid[1:insample]
      if(input$plotpilih=="Plot ACF"){
        acf(covid1)
      }
      if(input$plotpilih=="Plot PACF"){
        pacf(covid1)
      }
    }
    if (input$prov=="Central Java"){
      covid=datapakai[,5]
      n=length(covid)-sum(is.na(covid))
      ins=as.numeric(input$inout)
      insample=n*ins
      covid1=covid[1:insample]
      if(input$plotpilih=="Plot ACF"){
        acf(covid1)
      }
      if(input$plotpilih=="Plot PACF"){
        pacf(covid1)
      }
    }
    if (input$prov=="East Java"){
      covid=datapakai[,6]
      n=length(covid)-sum(is.na(covid))
      ins=as.numeric(input$inout)
      insample=n*ins
      covid1=covid[1:insample]
      if(input$plotpilih=="Plot ACF"){
        acf(covid1)
      }
      if(input$plotpilih=="Plot PACF"){
        pacf(covid1)
      }
    }
  })
  observeEvent(input$check,{
    datainput=input$input
    if (is.null(datainput)){return()}
    datapakai=read.delim(datainput$datapath,header = T)
    if (input$prov=="National"){
      covid=datapakai[,2]
      n=length(covid)
      ins=as.numeric(input$inout)
      insample=n*ins
      covid1=covid[1:insample]
      n1=length(covid1)
      lags=as.numeric(input$lags)
      y=covid1[(lags+1):n1]
      x=covid1[1:(n1-lags)]
      orde=as.numeric(input$order)
      output$hasilknot1<-renderPrint({gcv1(x,y,orde)})
      output$hasilknot2<-renderPrint({gcv2(x,y,orde)})
    }
    if (input$prov=="DKI Jakarta"){
      covid=datapakai[,3]
      n=length(covid)-sum(is.na(covid))
      ins=as.numeric(input$inout)
      insample=n*ins
      covid1=covid[1:insample]
      n1=length(covid1)
      lags=as.numeric(input$lags)
      y=covid1[(lags+1):n1]
      x=covid1[1:(n1-lags)]
      orde=as.numeric(input$order)
      output$hasilknot1<-renderPrint({gcv1(x,y,orde)})
      output$hasilknot2<-renderPrint({gcv2(x,y,orde)})
    }
    if (input$prov=="West Java"){
      covid=datapakai[,4]
      n=length(covid)-sum(is.na(covid))
      ins=as.numeric(input$inout)
      insample=n*ins
      covid1=covid[1:insample]
      n1=length(covid1)
      lags=as.numeric(input$lags)
      y=covid1[(lags+1):n1]
      x=covid1[1:(n1-lags)]
      orde=as.numeric(input$order)
      output$hasilknot1<-renderPrint({gcv1(x,y,orde)})
      output$hasilknot2<-renderPrint({gcv2(x,y,orde)})
    }
    if (input$prov=="Central Java"){
      covid=datapakai[,5]
      n=length(covid)-sum(is.na(covid))
      ins=as.numeric(input$inout)
      insample=n*ins
      covid1=covid[1:insample]
      n1=length(covid1)
      lags=as.numeric(input$lags)
      y=covid1[(lags+1):n1]
      x=covid1[1:(n1-lags)]
      orde=as.numeric(input$order)
      output$hasilknot1<-renderPrint({gcv1(x,y,orde)})
      output$hasilknot2<-renderPrint({gcv2(x,y,orde)})
    }
    if (input$prov=="East Java"){
      covid=datapakai[,6]
      n=length(covid)-sum(is.na(covid))
      ins=as.numeric(input$inout)
      insample=n*ins
      covid1=covid[1:insample]
      n1=length(covid1)
      lags=as.numeric(input$lags)
      y=covid1[(lags+1):n1]
      x=covid1[1:(n1-lags)]
      orde=as.numeric(input$order)
      output$hasilknot1<-renderPrint({gcv1(x,y,orde)})
      output$hasilknot2<-renderPrint({gcv2(x,y,orde)})
    }
  })
  observeEvent(input$model,{
    datainput=input$input
    if (is.null(datainput)){return()}
    datapakai=read.delim(datainput$datapath,header = T)
    if (input$prov=="National"){
      covid=datapakai[,2]
      n=length(covid)
      ins=as.numeric(input$inout)
      insample=n*ins
      covid1=covid[1:insample]
      n1=length(covid1)
      lags=as.numeric(input$lags)
      y=covid1[(lags+1):n1]
      x=covid1[1:(n1-lags)]
      orde=as.numeric(input$order1)
      knots1=as.numeric(input$knotoptimal)
      knots2=as.numeric(input$knotoptimall)
      if(is.na(knots2)){return()}
      if(input$knot1=="One"){
        #1 titik knot
        output$estimasi<-renderPrint({model.spline(x,y,orde,knots=c(knots1))})
        output$plotmodel<-renderPlot({model.spline(x,y,orde,knots=c(knots1))})
        output$plottime<-renderPlot({model.spline1(x,y,orde,knots=c(knots1))})
      }
      if(input$knot1=="Two"){
        #2 titik knot
        output$estimasi<-renderPrint({model.spline(x,y,orde,knots = c(knots1,knots2))})
        output$plotmodel<-renderPlot({model.spline(x,y,orde,knots = c(knots1,knots2))})
        output$plottime<-renderPlot({model.spline1(x,y,orde,knots = c(knots1,knots2))})
      }
    }
    if (input$prov=="DKI Jakarta"){
      covid=datapakai[,3]
      n=length(covid)-sum(is.na(covid))
      ins=as.numeric(input$inout)
      insample=n*ins
      covid1=covid[1:insample]
      n1=length(covid1)
      lags=as.numeric(input$lags)
      y=covid1[(lags+1):n1]
      x=covid1[1:(n1-lags)]
      orde=as.numeric(input$order1)
      knots1=as.numeric(input$knotoptimal)
      knots2=as.numeric(input$knotoptimall)
      if(is.na(knots2)){return()}
      if(input$knot1=="One"){
        #1 titik knot
        output$estimasi<-renderPrint({model.spline(x,y,orde,knots=c(knots1))})
        output$plotmodel<-renderPlot({model.spline(x,y,orde,knots=c(knots1))})
        output$plottime<-renderPlot({model.spline1(x,y,orde,knots=c(knots1))})
      }
      if(input$knot1=="Two"){
        #2 titik knot
        output$estimasi<-renderPrint({model.spline(x,y,orde,knots = c(knots1,knots2))})
        output$plotmodel<-renderPlot({model.spline(x,y,orde,knots = c(knots1,knots2))})
        output$plottime<-renderPlot({model.spline1(x,y,orde,knots = c(knots1,knots2))})
      }
    }
    if (input$prov=="West Java"){
      covid=datapakai[,4]
      n=length(covid)-sum(is.na(covid))
      ins=as.numeric(input$inout)
      insample=n*ins
      covid1=covid[1:insample]
      n1=length(covid1)
      lags=as.numeric(input$lags)
      y=covid1[(lags+1):n1]
      x=covid1[1:(n1-lags)]
      orde=as.numeric(input$order1)
      knots1=as.numeric(input$knotoptimal)
      knots2=as.numeric(input$knotoptimall)
      if(is.na(knots2)){return()}
      if(input$knot1=="One"){
        #1 titik knot
        output$estimasi<-renderPrint({model.spline(x,y,orde,knots=c(knots1))})
        output$plotmodel<-renderPlot({model.spline(x,y,orde,knots=c(knots1))})
        output$plottime<-renderPlot({model.spline1(x,y,orde,knots=c(knots1))})
      }
      if(input$knot1=="Two"){
        #2 titik knot
        output$estimasi<-renderPrint({model.spline(x,y,orde,knots = c(knots1,knots2))})
        output$plotmodel<-renderPlot({model.spline(x,y,orde,knots = c(knots1,knots2))})
        output$plottime<-renderPlot({model.spline1(x,y,orde,knots = c(knots1,knots2))})
      }
    }
    if (input$prov=="Central Java"){
      covid=datapakai[,5]
      n=length(covid)-sum(is.na(covid))
      ins=as.numeric(input$inout)
      insample=n*ins
      covid1=covid[1:insample]
      n1=length(covid1)
      lags=as.numeric(input$lags)
      y=covid1[(lags+1):n1]
      x=covid1[1:(n1-lags)]
      orde=as.numeric(input$order1)
      knots1=as.numeric(input$knotoptimal)
      knots2=as.numeric(input$knotoptimall)
      if(is.na(knots2)){return()}
      if(input$knot1=="One"){
        #1 titik knot
        output$estimasi<-renderPrint({model.spline(x,y,orde,knots=c(knots1))})
        output$plotmodel<-renderPlot({model.spline(x,y,orde,knots=c(knots1))})
        output$plottime<-renderPlot({model.spline1(x,y,orde,knots=c(knots1))})
      }
      if(input$knot1=="Two"){
        #2 titik knot
        output$estimasi<-renderPrint({model.spline(x,y,orde,knots = c(knots1,knots2))})
        output$plotmodel<-renderPlot({model.spline(x,y,orde,knots = c(knots1,knots2))})
        output$plottime<-renderPlot({model.spline1(x,y,orde,knots = c(knots1,knots2))})
      }
    }
    if (input$prov=="East Java"){
      covid=datapakai[,6]
      n=length(covid)-sum(is.na(covid))
      ins=as.numeric(input$inout)
      insample=n*ins
      covid1=covid[1:insample]
      n1=length(covid1)
      lags=as.numeric(input$lags)
      y=covid1[(lags+1):n1]
      x=covid1[1:(n1-lags)]
      orde=as.numeric(input$order1)
      knots1=as.numeric(input$knotoptimal)
      knots2=as.numeric(input$knotoptimall)
      if(is.na(knots2)){return()}
      if(input$knot1=="One"){
        #1 titik knot
        output$estimasi<-renderPrint({model.spline(x,y,orde,knots=c(knots1))})
        output$plotmodel<-renderPlot({model.spline(x,y,orde,knots=c(knots1))})
        output$plottime<-renderPlot({model.spline1(x,y,orde,knots=c(knots1))})
      }
      if(input$knot1=="Two"){
        #2 titik knot
        output$estimasi<-renderPrint({model.spline(x,y,orde,knots = c(knots1,knots2))})
        output$plotmodel<-renderPlot({model.spline(x,y,orde,knots = c(knots1,knots2))})
        output$plottime<-renderPlot({model.spline1(x,y,orde,knots = c(knots1,knots2))})
      }
    }
  })
  observeEvent(input$model1,{
    datainput=input$input
    if (is.null(datainput)){return()}
    datapakai=read.delim(datainput$datapath,header = T)
    if (input$prov=="National"){
      covid=datapakai[,2]
      n=length(covid)
      ins=as.numeric(input$inout)
      insample=n*ins
      covid1=covid[1:insample]
      n1=length(covid1)
      lags=as.numeric(input$lags)
      y=covid1[(lags+1):n1]
      x=covid1[1:(n1-lags)]
      orde=as.numeric(input$order2)
      knot1=as.numeric(input$knotoptimal1)
      knot2=as.numeric(input$knotoptimal2)
      if(is.na(knot2)){return()}
      outsample=1+insample
      dataout=covid[outsample:n]
      n2=length(dataout)
      yout=dataout[(lags+1):n2]
      xout=dataout[1:(n2-lags)]
      if(input$knot2=="One"){
        #1 titik knot
        output$peramalan<-renderPrint({spline.out(x,y,xout,yout,orde,knots=c(knot1))})
        output$plotmodel1<-renderPlot({spline.out(x,y,xout,yout,orde,knots=c(knot1))})
      }
      if(input$knot2=="Two"){
        #2 titik knot
        output$peramalan<-renderPrint({spline.out(x,y,xout,yout,orde,knots = c(knot1,knot2))})
        output$plotmodel1<-renderPlot({spline.out(x,y,xout,yout,orde,knots = c(knot1,knot2))})
      }
    }
    if (input$prov=="DKI Jakarta"){
      covid=datapakai[,3]
      n=length(covid)-sum(is.na(covid))
      ins=as.numeric(input$inout)
      insample=n*ins
      covid1=covid[1:insample]
      n1=length(covid1)
      lags=as.numeric(input$lags)
      y=covid1[(lags+1):n1]
      x=covid1[1:(n1-lags)]
      orde=as.numeric(input$order2)
      knot1=as.numeric(input$knotoptimal1)
      knot2=as.numeric(input$knotoptimal2)
      if(is.na(knot2)){return()}
      outsample=1+insample
      dataout=covid[outsample:n]
      n2=length(dataout)
      yout=dataout[(lags+1):n2]
      xout=dataout[1:(n2-lags)]
      if(input$knot2=="One"){
        #1 titik knot
        output$peramalan<-renderPrint({spline.out(x,y,xout,yout,orde,knots=c(knot1))})
        output$plotmodel1<-renderPlot({spline.out(x,y,xout,yout,orde,knots=c(knot1))})
      }
      if(input$knot2=="Two"){
        #2 titik knot
        output$peramalan<-renderPrint({spline.out(x,y,xout,yout,orde,knots = c(knot1,knot2))})
        output$plotmodel1<-renderPlot({spline.out(x,y,xout,yout,orde,knots = c(knot1,knot2))})
      }
    }
    if (input$prov=="West Java"){
      covid=datapakai[,4]
      n=length(covid)-sum(is.na(covid))
      ins=as.numeric(input$inout)
      insample=n*ins
      covid1=covid[1:insample]
      n1=length(covid1)
      lags=as.numeric(input$lags)
      y=covid1[(lags+1):n1]
      x=covid1[1:(n1-lags)]
      orde=as.numeric(input$order2)
      knot1=as.numeric(input$knotoptimal1)
      knot2=as.numeric(input$knotoptimal2)
      if(is.na(knot2)){return()}
      outsample=1+insample
      dataout=covid[outsample:n]
      n2=length(dataout)
      yout=dataout[(lags+1):n2]
      xout=dataout[1:(n2-lags)]
      if(input$knot2=="One"){
        #1 titik knot
        output$peramalan<-renderPrint({spline.out(x,y,xout,yout,orde,knots=c(knot1))})
        output$plotmodel1<-renderPlot({spline.out(x,y,xout,yout,orde,knots=c(knot1))})
      }
      if(input$knot2=="Two"){
        #2 titik knot
        output$peramalan<-renderPrint({spline.out(x,y,xout,yout,orde,knots = c(knot1,knot2))})
        output$plotmodel1<-renderPlot({spline.out(x,y,xout,yout,orde,knots = c(knot1,knot2))})
      }
    }
    if (input$prov=="Central Java"){
      covid=datapakai[,5]
      n=length(covid)-sum(is.na(covid))
      ins=as.numeric(input$inout)
      insample=n*ins
      covid1=covid[1:insample]
      n1=length(covid1)
      lags=as.numeric(input$lags)
      y=covid1[(lags+1):n1]
      x=covid1[1:(n1-lags)]
      orde=as.numeric(input$order2)
      knot1=as.numeric(input$knotoptimal1)
      knot2=as.numeric(input$knotoptimal2)
      if(is.na(knot2)){return()}
      outsample=1+insample
      dataout=covid[outsample:n]
      n2=length(dataout)
      yout=dataout[(lags+1):n2]
      xout=dataout[1:(n2-lags)]
      if(input$knot2=="One"){
        #1 titik knot
        output$peramalan<-renderPrint({spline.out(x,y,xout,yout,orde,knots=c(knot1))})
        output$plotmodel1<-renderPlot({spline.out(x,y,xout,yout,orde,knots=c(knot1))})
      }
      if(input$knot2=="Two"){
        #2 titik knot
        output$peramalan<-renderPrint({spline.out(x,y,xout,yout,orde,knots = c(knot1,knot2))})
        output$plotmodel1<-renderPlot({spline.out(x,y,xout,yout,orde,knots = c(knot1,knot2))})
      }
    }
    if (input$prov=="East Java"){
      covid=datapakai[,6]
      n=length(covid)-sum(is.na(covid))
      ins=as.numeric(input$inout)
      insample=n*ins
      covid1=covid[1:insample]
      n1=length(covid1)
      lags=as.numeric(input$lags)
      y=covid1[(lags+1):n1]
      x=covid1[1:(n1-lags)]
      orde=as.numeric(input$order2)
      knot1=as.numeric(input$knotoptimal1)
      knot2=as.numeric(input$knotoptimal2)
      if(is.na(knot2)){return()}
      outsample=1+insample
      dataout=covid[outsample:n]
      n2=length(dataout)
      yout=dataout[(lags+1):n2]
      xout=dataout[1:(n2-lags)]
      if(input$knot2=="One"){
        #1 titik knot
        output$peramalan<-renderPrint({spline.out(x,y,xout,yout,orde,knots=c(knot1))})
        output$plotmodel1<-renderPlot({spline.out(x,y,xout,yout,orde,knots=c(knot1))})
      }
      if(input$knot2=="Two"){
        #2 titik knot
        output$peramalan<-renderPrint({spline.out(x,y,xout,yout,orde,knots = c(knot1,knot2))})
        output$plotmodel1<-renderPlot({spline.out(x,y,xout,yout,orde,knots = c(knot1,knot2))})
      }
    }
  })
}
#Running App
shinyApp(ui=tampilan,server=program)