# my_packages = c("plyr","shiny","plotly","shinythemes","shinyWidgets","shinydashboard","lubridate","prophet","DT")
# 
# install_if_missing = function(p) {
#     if (p %in% rownames(installed.packages()) == FALSE) {
#         install.packages(p, dependencies = TRUE)
#     }
#     else {
#         cat(paste("Skipping already installed package:", p, "\n"))
#     }
# }
# invisible(sapply(my_packages, install_if_missing))


library(shiny)
library(plotly)
library(shinythemes)
library(shinyWidgets)
library(shinydashboard)
library(lubridate)
library(prophet)
library(DT)
options(scipen = 9999999)
x<<-"1"
df<-read.csv("https://api.covid19india.org/csv/latest/case_time_series.csv")
df$Date<-as.Date(df$Date,format="%d %b")
nrec<-length(df$Date)
latest_date<-df$Date[nrec]
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

# Define UI ----
ui <- fluidPage(theme = shinytheme("journal"),
                navbarPage("COVID-19 India Analyzer",
                           theme = shinytheme("darkly"),
                           position = "fixed-top",
                           windowTitle = "COVID-19 India Analyzer",
                           collapsible = TRUE,
                           fluid = TRUE,
                           #footer=includeHTML("./www/footer.html"),
                           #includeHTML("./www/index.html"),
                           tabPanel("Home",
                                    icon = icon("i2",class ="fa fa-home"),
                                    tags$br(),
                                    tags$br(),
                                    h1("Welcome to COVID-19 India Analyzer", align = "center"),
                                    tags$br(),
                                    tags$br(),
                                    h5("We, some data analytics enthusiasts have tried in analyzing & predicting the COVID-19 situation in India.",align="center"),
                                    tags$br(),
                                    tags$div(class="card",
                                    style="background: #199AF1;
                                             padding:5px;
                                    border-radius:10px;
                                    margin:auto;
                                    max-width:500px;",
                                    h2("Total Coronavirus Cases in India:",align = "center"),
                                    h5(paste("(Till",formatted_ld,"11:59 pm IST)"),align="center"),
                                    h2(formatC(df$Total.Confirmed[nrec],big.mark=","),align="center"),
                                    ),
                                    tags$br(),
                                    tags$br(),
                                    fluidRow(
                                        column(6,
                                               tags$div(class="card",
                                                        style="background: #F32915;
                                             padding:5px;
                                    border-radius:10px;
                                                        margin-bottom:40px; max-width:500px; margin-left:auto; margin-right:auto;"
                                                        ,
                                               h2("Deaths",align = "center"),
                                               h5(paste("(Till",formatted_ld,"11:59 pm IST)"),align="center"),
                                               h2(formatC(df$Total.Deceased[nrec],big.mark=","),align="center"),
                                               tags$br()
                                               )
                                        ),
                                        column(6,
                                               tags$div(class="card",
                                                        style="background: #39D800;
                                             padding:5px;
                                    border-radius:10px; max-width:500px; margin:auto;
                                                        ",
                                               h2("Recovered",align = "center"),
                                               h5(paste("(Till",formatted_ld,"11:59 pm IST)"),align="center"),
                                               h2(formatC(df$Total.Recovered[nrec],big.mark=","),align="center"),
                                               tags$br()
                                               )
                                        )
                                
                                    ),
                                    tags$br(),
                                    tags$br(),
                                    tags$div(class="card",
                                             style="background: #D8D700;
                                             padding:5px;
                                    border-radius:10px; max-width:500px; margin:auto;",
                                    h2("Total Active Cases",align = "center"),
                                    h5(paste("(Till",formatted_ld,"11:59 pm IST)"),align="center"),
                                    h2(formatC(df$Total.Confirmed[nrec]-(df$Total.Deceased[nrec]+df$Total.Recovered[nrec]),big.mark=","),
                                       align="center")
                                    ),
                                    tags$br(),
                                    tags$br(),
                                    tags$br(),
                                    h5("In this project, we have built a dashboard from opensource COVID-19 Data and tried to predict the rise COVID-19 cases for the next 365 days.", align="center"),
                                    tags$br()
                           ),
                           tabPanel(id="dash",
                                    "Daily Dashboard",
                                    tags$br(),
                                    icon = icon("i1",class ="fas fa-tachometer-alt"),
                                    tags$br(),
                                    h1("Daily Dashboard",align="center"),
                                    tags$br(),
                                    tags$br(),
                                    tags$br(),
                                    sidebarPanel(h2("Graph Parameters"),
                                                 awesomeCheckboxGroup("radio1",
                                                                      h3("Case Type"),
                                                                      choices = list("Confimed Cases"=1,
                                                                                     "Recovered Cases"=2,
                                                                                     "Deceased Cases"=3),
                                                                      selected = 1,
                                                                      status = "primary"),
                                                 awesomeRadio('radio2',
                                                              h3("Daily/Total:"),
                                                              choices = list("Daily"=1,
                                                                             "Total"=0),
                                                              selected = 1),
                                                 sliderInput("TimeSlider",
                                                             'Time Frame',
                                                             min=df[1,1],max=latest_date,
                                                             value=c(df[1,1],latest_date))),
                                    mainPanel(h2(textOutput("selected_var"),align="center"),
                                              plotlyOutput(outputId = "distPlot", width = "auto")),
                                    tags$hr()
                           ),
                           navbarMenu("Peak Data Analytics",
                                    icon = icon("ipred",class = "fas fa-chart-line"),
                                                tabPanel("Daily Estimates",
                                                         tags$br(),
                                                         tags$br(),
                                                         h1("Peak Data Analytics",align="center"),
                                                         tags$br(),
                                                         tags$br(),
                                                         h3("Forecasting Daily Rise in Number of Cases",align="center"),
                                                         tags$br(),
                                                         tabsetPanel(type = "pills",
                                                             tabPanel(title = "Graphs",
                                                                    box(
                                                                        plotlyOutput("daily_pred")
                                                                        ),
                                                                    box("Our forecasting model on daily rise of cases is based on fitting the count of daily rise of cases into gaussian model.",
                                                                        tags$br(),
                                                                        tags$br(),
                                                                        "The intuition behind using this is that we believe that there is an overall peak point after which the rise in number of cases is gonna fall.",
                                                                        tags$br(),
                                                                        tags$br(),
                                                                        "According to the incoming data and our forecasting model, We expect that this peak point or ",
                                                                        tags$b(tags$i("point of saturation"))," on ",
                                                                        tags$b(sat_date),
                                                                        " with esitmated number of cases being ",tags$b(m$Expected),".",
                                                                        tags$br(),
                                                                        tags$br(),
                                                                        paste("Our Algorithm has forecasted the latest count (i.e. as of ",
                                                                              latest_date,
                                                                              ") of daily cases as ",
                                                                              df_new_daily[16,2], 
                                                                              " compared to actual count of ",
                                                                              df[nrec,2]," i.e. with ",round(accuracy_daily,digits = 2),"% accuracy"),"."
                                                                    )
                                                             ),
                                                             tabPanel(title = "Data for Last 5 Days",
                                                                      tags$br(),
                                                                      DT::dataTableOutput("see_data_peak1")
                                                             )
                                                           )
                                                         ),
                                                tabPanel("Total Estimates",
                                                         tags$br(),
                                                         tags$br(),
                                                         h1("Peak Data Analytics",align="center"),
                                                         tags$br(),
                                                         tags$br(),
                                                            h3("Forecasting Total Rise in Number of Cases",align="center"),
                                                         tags$br(),
                                                         tabsetPanel(
                                                             tabPanel(title = "Graphs",
                                                                      tags$br(),
                                                                      box(
                                                                          plotlyOutput("total_pred")
                                                                          ),
                                                                      box("In the case of total estimates we have fit in a logistic model that predicts the",
                                                                tags$b(tags$i("point of stagnation")),
                                                                "(i.e the point after which the there will be minimum or no new daily cases) to be ",
                                                                tags$b(stag_date),
                                                                "with an expected total number of cases of being ",
                                                                tags$b(m1$Expected),".",
                                                                tags$br(),
                                                                tags$br(),
                                                                paste("Our Algorithm has forecasted the latest count (i.e. as of ",
                                                                      latest_date,
                                                                      ") of total cases as ",
                                                                      df_new_total[16,2], 
                                                                      " compared to actual count of ",
                                                                      df[nrec,3]," i.e. with ",round(accuracy_total,digits = 2),"% accuracy"),"."
                                                            )
                                                             ),
                                                            tabPanel(title = "Data for Last 5 Days",
                                                                     tags$br(),
                                                                     DT::dataTableOutput("see_data_peak2")
                                                                     )
                                                 )
                                                )
                                                
                                        
                           ),
                           tabPanel("Predictive Analytics",
                                    icon = icon("iref",class="fas fa-chart-area"),
                                    tags$br(),
                                    tags$br(),
                                    h1("Predictive Analytics",align="center"),
                                    tags$br(),
                                    tags$br(),
                                    h2(textOutput("prophet_title"),align="center"),
                                    tags$br(),
                                    fluidRow(
                                        sidebarPanel(
                                            h2("Parameters"),
                                            awesomeRadio("radio3",
                                                         h3("Choices"),
                                                         choices = as.list(trace_names[3:6]),
                                                         selected = trace_names[3])
                                        ),
                                        mainPanel(
                                            tabsetPanel(
                                                tabPanel(title = "Graph",
                                                         plotlyOutput("prophetPlot")),
                                                tabPanel(title = "Data for Last 5 days",
                                                         #h2(textOutput("tab_title"),align="center"),
                                                         tags$br(),
                                                         DT::dataTableOutput("see_data"))
                                            )
                                            
                                        )
                                    )
                                    ),
                           tabPanel("About The Team",
                                    icon = icon("iabt",class="far fa-address-card"),
                                    tags$br(),
                                    tags$br(),
                                    h1("About Us", align="center"),
                                    br(),
                                    tags$br(),
                                    tags$br(),
                                    includeHTML("./www/about the team.html"))
                           
                )
)
# Define server logic ----
server <- function(input, output, session) {
    output$distPlot <- renderPlotly({
        trace_no=as.numeric(input$radio1)
        rad<-as.numeric(input$radio2)
        temp<-c("Daily","Total")
        p<-plot_ly(data=df%>% filter(Date>=input$TimeSlider[1], Date<=input$TimeSlider[2]),
                   type = "scatter", mode = "lines")
        for (traces in trace_no){
            if(rad==1){
                p<-p %>% plotly::add_trace(x = ~Date, # converted Date as date
                                           y = as.formula(paste("~`", names(df)[((2*traces)-rad)+1], "`",sep='')),
                                           name = trace_names[(2*traces)-rad],
                                           line=list(color=col_names[traces]))
            }
            else{
                p<-p %>% plotly::add_trace(x = ~Date, # converted Date as date
                                           y = as.formula(paste("~`", names(df)[((2*traces)-rad)+1], "`",sep='')),
                                           name = trace_names[(2*traces)-rad],
                                           line=list(color=col_names[traces]),
                                           fill = 'tozeroy',
                                           fillcolor=col_names[traces])
            }
        }
        p<-p %>% 
            layout(yaxis=list(title='Number of Cases',
                              automargin = TRUE),
                   xaxis=list(title='Timeframe',
                              automargin = TRUE),
                   legend = list(orientation='h',y=-0.25, x=0.2,xanchor='centre'),
                   plot_bgcolor='rgb(0,0,0)',
                paper_bgcolor='rgb(0,0,0)',fig_bgcolor = "rgba(0, 0, 0, 0)")
    })
    observe({
        if(length(input$radio1)==1){
            x<<-input$radio1
        }
        if(length(input$radio1)==0){
            updateAwesomeCheckboxGroup(session,'radio1',selected=x)
        }
        if(input$TimeSlider[1]==input$TimeSlider[2]){
            if(input$TimeSlider[2]==df$Date[1]){
                updateSliderInput(session,'TimeSlider',
                                  value = c(input$TimeSlider[1],input$TimeSlider[1]+1))
            }
            else{
                updateSliderInput(session,'TimeSlider',
                                  value = c(input$TimeSlider[2]-1,input$TimeSlider[2]))
            }
        }
    })
    output$selected_var <- renderText({
        if(input$radio2=="1"){
            paste('Graphs of Daily Cases between',input$TimeSlider[1],'and',input$TimeSlider[2])
        }
        else{
            paste('Graphs of Total Cases between',input$TimeSlider[1],'and',input$TimeSlider[2])
        }
    })
    output$piePlot<-renderPlotly({
        
    })
    output$daily_pred<-renderPlotly({
        plot_ly(data=df,x = ~Date,
                y = ~Daily.Confirmed,
                type = "scatter", mode = "lines",
                line=list(color="#CCCC00"),name="Actual")%>%
            add_trace(data=df_new_daily,x = ~date,
                      y = ~Expected,
                      type = "scatter", mode = "lines",
                      line=list(color="#ff4f33",dash='dash'),name="Predicted")%>%
            add_annotations(x = m$date,
                            y = m$Expected,
                            text = paste("Expected Saturation Point (",m$Expected,")", " \n on: ",sat_date,sep=""),
                            xref = "x",
                            yref = "y",
                            showarrow = TRUE,
                            arrowhead = 7,
                            ax = 20,
                            ay = -40)%>%
            layout(yaxis=list(title='Number of Cases',
                              automargin = TRUE),
                   xaxis=list(title='Timeframe',
                              automargin = TRUE),
                   legend = list(orientation='h',y=-0.25, x=0.2,xanchor='centre'),
                   plot_bgcolor='rgb(0,0,0)',
                   paper_bgcolor='rgb(0,0,0)',fig_bgcolor = "rgba(0, 0, 0, 0)")
    })
    output$total_pred<-renderPlotly({
        plot_ly(data=df,x = ~Date,
                y = ~Total.Confirmed,
                type = "scatter", mode = "lines",
                line=list(color="#CCCC00"),name="Actual")%>%
            add_trace(data=df_new_total,#[c(1:n_stag),]
                      x = ~date,
                      y = ~Expected,
                      type = "scatter", mode = "lines",
                      line=list(color="#ff4f33",dash='dash'),name="Predicted")%>%
            add_annotations(x = m1$date,
                            y = m1$Expected,
                            text = paste("Expected Stagnating Point (",m1$Expected,")", " \n on: ",stag_date,sep=""),
                            xref = "x",
                            yref = "y",
                            showarrow = TRUE,
                            arrowhead = 7,
                            ax = 20,
                            ay = -40)%>%
            layout(yaxis=list(title='Number of Cases',
                              automargin = TRUE),
                   xaxis=list(title='Timeframe',
                              automargin = TRUE),
                   legend = list(orientation='h',y=-0.25, x=0.2,xanchor='centre'),
                   plot_bgcolor='rgb(0,0,0)',
                   paper_bgcolor='rgb(0,0,0)',fig_bgcolor = "rgba(0, 0, 0, 0)")
    })
    output$prophetPlot<-renderPlotly({
        opt<-gsub(" ",".",input$radio3)
        plot_ly(data=df,x = ~Date,
                y = as.formula(paste("~",opt)),
                type = "scatter", mode = "lines",
                line=list(color="#CCCC00"),name="Actual")%>%
            add_trace(data=for_list[[opt]],#[c(1:n_stag),]
                      x = ~ds[15:35],
                      y = ~yhat[15:35],
                      type = "scatter", mode = "lines",
                      line=list(color="#ff4f33"),name="Predicted")%>%
            layout(yaxis=list(title='Number of Cases',
                              automargin = TRUE),
                   xaxis=list(title='Timeframe',
                              automargin = TRUE),
                   legend = list(orientation='h',y=-0.25, x=0.2,xanchor='centre'),
                   plot_bgcolor='rgb(0,0,0)',
                   paper_bgcolor='rgb(0,0,0)',fig_bgcolor = "rgba(0, 0, 0, 0)")
    })
    output$prophet_title<-renderText({
        paste(input$radio3)
    })
    output$see_data<-DT::renderDataTable({
        opt<-gsub(" ",".",input$radio3)
        d<-for_list[[opt]]
        acc<-100*(1-(abs(d$yhat[16:20]-df[c((nrec-4):nrec),opt])/df[c((nrec-4):nrec),opt]))
        df2<-data.frame(as.Date(d$ds[16:20],format="%d %b %y"),d$yhat[16:20],df[c((nrec-4):nrec),opt],round(acc,digits = 4))
        names(df2)<-c("Date","Predicted","Actual","Accuracy (in %)")
        DT::datatable(df2,options = list(paging=FALSE,searching=FALSE,info=FALSE,initComplete = JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
            "}")))
    })
    output$tab_title<-renderText({
        paste(input$radio3)
    })
    output$see_data_peak2<-DT::renderDataTable({
        acc<-100*(1-(abs(df_new_total[c(12:16),2]-df[c((nrec-4):nrec),3])/df[c((nrec-4):nrec),3]))
        df2<-data.frame(df[c((nrec-4):nrec),1],df_new_total[c(12:16),2],df[c((nrec-4):nrec),3],round(acc,digits = 4))
        names(df2)<-c("Date","Predicted","Actual","Accuracy (in %)")
        DT::datatable(df2,options = list(paging=FALSE,searching=FALSE,info=FALSE,initComplete = JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
            "}")))
        #dat<-list(searching=FALSE)
    })
    output$see_data_peak1<-DT::renderDataTable({
        acc<-100*(1-(abs(df_new_daily[c(12:16),2]-df[c((nrec-4):nrec),2])/df[c((nrec-4):nrec),2]))
        df2<-data.frame(df[c((nrec-4):nrec),1],df_new_daily[c(12:16),2],df[c((nrec-4):nrec),2],round(acc,digits = 4))
        names(df2)<-c("Date","Predicted","Actual","Accuracy (in %)")
        DT::datatable(df2,options = list(paging=FALSE,searching=FALSE,info=FALSE,initComplete = JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
            "}")))
    })
}
shinyApp(ui = ui, server = server)




