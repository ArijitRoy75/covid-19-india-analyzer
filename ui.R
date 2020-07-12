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