library(shiny)
library(plotly)
library(shinythemes)
library(shinyWidgets)
library(shinydashboard)
library(lubridate)
library(DT)
library(dplyr)
library(minpack.lm)
options(scipen = 9999999)
x<<-"1"
df<-read.csv("https://api.covid19india.org/csv/latest/case_time_series.csv")
df=df[,-1]
colnames(df)[1] <- "Date"
nrec<-length(df$Date)
df$Date<-as.Date(df$Date)
latest_date<-df$Date[nrec]
if(latest_date==Sys.Date()){
    df<-df[-nrec,]
    latest_date<-latest_date-1
    nrec<-nrec-1
}

formatted_ld<-paste(months(latest_date)," ",mday(latest_date),", ",year(latest_date),sep = "")
formatted_sd<-paste(months(df[1,1])," ",mday(df[1,1]),", ",year(df[1,1]),sep = "")

total_cases<-c(df$Total.Confirmed[nrec],df$Total.Deceased[nrec],df$Total.Recovered[nrec],df$Total.Confirmed[nrec]-(df$Total.Deceased[nrec]+df$Total.Recovered[nrec]))
trace_names<-gsub("[.]", " ", names(df)[-1])
col_names<-c("#CCCC00","#33ff64","#ff4f33")

end_time<-500
nlc <- nls.control(maxiter = 10000)

#gauss_form<-as.formula(y ~ a * exp(-0.5 * ((x-b)/c)**2))
start_g <- list(a=80345.021529,b=232.408833,c=52.199533)
x<-seq(1,nrec)
xnew<-seq(nrec-15,nrec+end_time)

df1<-df[,c(1,2)]
names(df1)<-c('ds','y')
nrec<-length(df$Date)
df1<-cbind(df1,x)
gauss_model <- nlsLM(y ~ a * exp(-0.5 * ((x-b)/c)**2), data = df1, start = start_g,control = nlc)
ypred<-predict(gauss_model, list(x = xnew))
while(round(tail(ypred, n=1))!=0)
{
    end_time<-end_time+90
    xnew<-seq(nrec-15,nrec+end_time)
    ypred<-predict(gauss_model, list(x = xnew))
}
x_date<-seq(df[nrec-15,1],df[nrec,1]+end_time,by="day")
df_new_daily<-data.frame(x_date,round(ypred))
names(df_new_daily)<-c('date','Expected')

total<-cumsum(c(df$Daily.Confirmed[1:(nrec-15)],df_new_daily[c(2:length(df_new_daily$date)),2]))

df_new_total<-data.frame(x_date,total[(nrec-15):length(total)])
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

#states_data<-read.csv("https://api.covid19india.org/csv/latest/state_wise.csv")
#states_data<-states_data[-1,]

# router <- make_router(
#     route("/", home_page),
#     route("dashboard", dashboard_page),
#     route("peakdata", peakdata_page),
#     route("about",about_page)
# )


port <- Sys.getenv('PORT')
#port<-'8888'

shiny::runApp(
    appDir = getwd(),
    host = '0.0.0.0',
    port = as.numeric(port)
)


