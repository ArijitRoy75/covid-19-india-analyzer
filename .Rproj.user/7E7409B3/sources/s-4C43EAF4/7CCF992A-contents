library(shiny)
library(plotly)
library(shinythemes)
library(shinyWidgets)
library(shinydashboard)
library(lubridate)
library(prophet)
library(DT)
library(stats)
options(scipen = 9999999)
x<<-"1"
df<-read.csv("https://api.covid19india.org/csv/latest/case_time_series.csv")
df$Date<-as.Date(df$Date,format="%d %b")
nrec<-length(df$Date)
latest_date<-df$Date[nrec]

latest_date<-df$Date[nrec]
if(latest_date==Sys.Date()){
    latest_date<-latest_date-1
    nrec<-nrec-1
}

formatted_ld<-paste(months(latest_date)," ",mday(latest_date),", ",year(latest_date),sep = "")
formatted_sd<-paste(months(df[1,1])," ",mday(df[1,1]),", ",year(df[1,1]),sep = "")

total_cases<-c(df$Total.Confirmed[nrec],df$Total.Deceased[nrec],df$Total.Recovered[nrec],df$Total.Confirmed[nrec]-(df$Total.Deceased[nrec]+df$Total.Recovered[nrec]))
trace_names<-gsub("[.]", " ", names(df)[-1])
col_names<-c("#CCCC00","#33ff64","#ff4f33")


log_form<-as.formula(y~c/(1+exp(-k*(x-m))))
gauss_form<-as.formula(y ~ a * exp(-0.5 * ((x-b)/c)**2))
start_g <- list(a=22601.82885084,b=184.39588789,c=43.63552041)
start_l<- list(c=9.85641503e+05, k=5.71045624e-02, m=1.47460740e+02)
x<-seq(1,nrec)
xnew<-seq(nrec-15,nrec+500)
x_date<-seq(df[nrec-15,1],df[nrec,1]+500,by="day")


df1<-df[,c(1,2)]
names(df1)<-c('ds','y')
nrec<-length(df$Date)
df1<-cbind(df1,x)
gauss_model <- nls(formula = gauss_form, data = df1, start = start_g)
ypred<-predict(gauss_model, list(x = xnew))
df_new_daily<-data.frame(x_date,round(ypred))
names(df_new_daily)<-c('date','Expected')


df1<-df[,c(1,3)]
names(df1)<-c('ds','y')
nrec<-length(df$Date)
df1<-cbind(df1,x)
log_model <-  nls(formula = log_form, data = df1, start = start_l)
ypred<-predict(log_model, list(x = xnew))
df_new_total<-data.frame(x_date,round(ypred))
names(df_new_total)<-c('date','Expected')

rm("df1","ypred")

m <- df_new_daily[which.max(df_new_daily$Expected), ]
m1 <- df_new_total[which.max(df_new_total$Expected), ]
n_stag<-which.max(df_new_total$Expected)
sat_date<-paste(months(m$date)," ",mday(m$date),", ",year(m$date),sep = "")
stag_date<-paste(months(m1$date)," ",mday(m1$date),", ",year(m1$date),sep = "")

accuracy_daily<-100*(1-abs(df_new_daily[16,2]-df[nrec,2])/df[nrec,2])
accuracy_total<-100*(1-abs(df_new_total[16,2]-df[nrec,3])/df[nrec,3])
avg_acc_daily<-100*mean(1-(abs(tail(df[,2],16)-df_new_daily[1:16,2])/tail(df[,2],16)))
max_acc_daily<-100*max(1-(abs(tail(df[,2],16)-df_new_daily[1:16,2])/tail(df[,2],16)))
max_acc_total<-100*max(1-(abs(tail(df[,3],16)-df_new_total[1:16,2])/tail(df[,3],16)))
avg_acc_total<-100*mean(1-(abs(tail(df[,3],16)-df_new_total[1:16,2])/tail(df[,3],16)))

remove_outliers<-function(ts,perc=0.01){
    svm_model<-e1071::svm(ts, nu=perc, type="one-classification",gamma=0.01,kernel = 'radial')
    out_svm<-predict(svm_model)
    index_svm <- which(out_svm==FALSE)
    ts1<-ts
    ts1[index_svm]<-NaN
    ts1<-forecast::na.interp(ts1)
    return(ts1)
}


for_list<-list()
for (i in c(4:7)) {
    df1<-tail(df[,c(1,i)],20)
    names(df1)<-c('ds','y')
    df1$y<-remove_outliers(df1$y)
    m2<-prophet(df1)
    future <- make_future_dataframe(m2, periods = 15, freq = 'day')
    forecast <- predict(m2, future)
    forecast$yhat<-round(forecast$yhat)
    for_list[[names(df[i])]]<-forecast
    
}

args <- commandArgs(trailingOnly = TRUE)

if (length(args) == 0) {
    port <- "12000"
} else if (length(args) == 1) {
    port <- args[1]
}

print(paste("Listening on port", port))

shiny::runApp(
    appDir = getwd(),
    host = "127.0.0.1",
    port = as.numeric(port)
)



