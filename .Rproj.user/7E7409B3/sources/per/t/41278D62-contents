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