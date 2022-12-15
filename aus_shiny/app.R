library(shiny)
library(plotly)
library(tidyverse)
library(shinythemes)
library(DT)
library(googledrive)

temp<-tempfile(fileext = '.json')
download.file('https://www.dropbox.com/s/twobcoxscpv77h6/rcloud-298921-bdc6e1c25160.json?dl=1',temp)
drive_auth(path=temp)

d<-list()
n<-drive_ls()$id[1:6]

for (i in n){
  temp <- tempfile(fileext = '.csv')
  d[which(n==i)]<-list(read.csv(drive_download(
    as_id(i), path = temp, type='csv',overwrite = TRUE)$local_path, 
    fileEncoding="latin1")[,-1])
}

for (i in 1:6){
  assign(drive_ls()$name[i],d[i] %>% as.data.frame())
}

storage$Date <- storage$Date %>% as.Date()
pipeline$Date <- pipeline$Date %>% as.Date()
longford$Date<-longford$Date %>% as.Date()
lng$Date<-lng$Date %>% as.Date()
gsh[,4]<-gsh[,4] %>% as.Date()
gsh[,5]<-gsh[,5] %>% as.Date()
dwgm$gas_date<-dwgm$gas_date %>% as.Date()

## UI
ui<-navbarPage(theme = shinytheme("flatly"),
               "East Australia gas dashboard",
               tabPanel("Victoria DWGM",
                        actionButton(inputId = 'onewdwgm',label = '1w'),
                        actionButton(inputId = 'twowdwgm',label = '2w'),
                        downloadButton("downloadData5", " Export"),
                        plotlyOutput("DWGM")),
               tabPanel("Pipeline",
                        actionButton(inputId = 'threemonthpipe',label = '3m'),
                        actionButton(inputId = 'sixmonthpipe',label = '6m'),
                        actionButton(inputId = 'oneyearpipe',label = '1y'),
                        downloadButton("downloadData1", " Export"),
                        plotlyOutput("pipeline")),
               tabPanel("LNG Production",
                        actionButton(inputId = 'threemonthlng',label = '3m'),
                        actionButton(inputId = 'sixmonthlng',label = '6m'),
                        actionButton(inputId = 'oneyearlng',label = '1y'),
                        downloadButton("downloadData2", " Export"),
                        plotlyOutput("lng")),
               tabPanel("Longford Production",
                        actionButton(inputId = 'threemonth',label = '3m'),
                        actionButton(inputId = 'sixmonth',label = '6m'),
                        actionButton(inputId = 'oneyear',label = '1y'),
                        downloadButton("downloadData3", " Export"),
                        plotlyOutput("longford")),
               tabPanel("Storage",
                        actionButton(inputId = 'threemonthstorage',label = '3m'),
                        actionButton(inputId = 'sixmonthstorage',label = '6m'),
                        actionButton(inputId = 'oneyearstorage',label = '1y'),
                        downloadButton("downloadData4", " Export"),
                        plotlyOutput("storage")),
               tabPanel("Gas Supply Hub",
                        downloadButton("downloadData6", " Export"),
                        dataTableOutput("GSH")),
               tabPanel("Data Collection",
                        htmlOutput("text"))
)

server<-function(input,output){
  rv<-reactiveValues(data=longford)
  observeEvent(input$threemonth,ignoreInit = F, {rv$data<-longford[(nrow(longford)-90):nrow(longford),]})
  observeEvent(input$sixmonth,ignoreInit = T, {rv$data<-longford[(nrow(longford)-180):nrow(longford),]})
  observeEvent(input$oneyear,ignoreInit = T,{rv$data<-longford[(nrow(longford)-364):nrow(longford),]})
  output$downloadData3 <- downloadHandler(
    filename = "longford.csv",
    content = function(file) {
      write.csv(rv$data, file)
    }
  )
  output$longford<-renderPlotly({rv$data %>% plot_ly(x=~Date,y=~utilisation,type='scatter',mode='lines')%>%
      layout(hovermode="x unified",title = 'Longford production',
             yaxis = list(title = 'Utilisation rate (%)',
                          zeroline = TRUE)) %>%
      config(displayModeBar = F)
  })
  rv2<-reactiveValues(data=storage)
  observeEvent(input$threemonthstorage,{rv2$data<-storage %>% filter(!Date<max(storage$Date)-90)})
  observeEvent(input$sixmonthstorage,{rv2$data<-storage %>% filter(!Date<max(storage$Date)-180)})
  observeEvent(input$oneyearstorage,{rv2$data<-storage %>% filter(!Date<max(storage$Date)-364)})
  output$downloadData4 <- downloadHandler(
    filename = "storage.csv",
    content = function(file) {
      write.csv(rv2$data, file)
    }
  )
  output$storage<-renderPlotly({rv2$data %>% plot_ly(x=~Date,y=~Levels,color=~facility,type='scatter',mode='lines')%>%
      layout(hovermode="x unified",title = 'Storage',
             yaxis = list(title = 'Level (%)',
                          zeroline = TRUE)) %>%
      config(displayModeBar = F)
  })
  rv3<-reactiveValues(data=pipeline)
  observeEvent(input$threemonthpipe,{rv3$data<-pipeline %>% filter(!Date<max(pipeline$Date)-90)})
  observeEvent(input$sixmonthpipe,{rv3$data<-pipeline %>% filter(!Date<max(pipeline$Date)-180)})
  observeEvent(input$oneyearpipe,{rv3$data<-pipeline %>% filter(!Date<max(pipeline$Date)-364)})
  output$downloadData1 <- downloadHandler(
    filename = "pipeline.csv",
    content = function(file) {
      write.csv(rv3$data, file)
    }
  )
  output$pipeline<-renderPlotly({rv3$data %>% plot_ly(x = ~Date, y = ~flow, color = ~pipe, type = 'scatter', mode = 'lines') %>%
      layout(hovermode="x unified",title = 'Pipeline flows',
             yaxis = list(title = 'Flows (TJ/d)',
                          zeroline = TRUE)) %>%
      config(displayModeBar = F)
  })
  rv5<-reactiveValues(data=lng)
  observeEvent(input$threemonthlng,{rv5$data<-lng %>% filter(!Date<max(lng$Date)-90)})
  observeEvent(input$sixmonthlng,{rv5$data<-lng %>% filter(!Date<max(lng$Date)-180)})
  observeEvent(input$oneyearlng,{rv5$data<-lng %>% filter(!Date<max(lng$Date)-364)})
  output$downloadData2 <- downloadHandler(
    filename = "lng.csv",
    content = function(file) {
      write.csv(rv5$data, file)
    }
  )
  output$lng<-renderPlotly({rv5$data %>% plot_ly(x = ~Date, y = ~flow, color = ~pipe, type = 'scatter', mode = 'lines') %>%
      layout(hovermode="x unified",title = 'LNG production pipeline flows',
             yaxis = list(title = 'Flows (TJ/d)',
                          zeroline = TRUE)) %>%
      config(displayModeBar = F)
  })
  rv4<-reactiveValues(data=dwgm)
  observeEvent(input$onewdwgm,{rv4$data<-dwgm %>% filter(!gas_date<max(dwgm$gas_date)-6)})
  observeEvent(input$twowdwgm,{rv4$data<-dwgm %>% filter(!gas_date<max(dwgm$gas_date)-13)})
  output$downloadData5 <- downloadHandler(
    filename = "dwgm.csv",
    content = function(file) {
      write.csv(rv4$data, file)
    }
  )
  output$DWGM<-renderPlotly({rv4$data %>% plot_ly() %>% 
      add_trace(x = ~gas_date, y = ~total_gas_used, type = 'bar', textposition = "inside",name = 'Demand (TJ/d)',
                marker = list(color = '#C9EFF9'),
                hoverinfo = "text",
                text = ~paste(total_gas_used,'TJ',sep='')) %>% 
      add_trace(x = ~gas_date, y = ~price_value, type = 'scatter', mode = 'lines',name = 'Price (A$/GJ)', yaxis = 'y2',
                line = list(color = '#45171D'),
                hoverinfo = "text",
                text = ~paste('A$',round(price_value,2),sep='')) %>% 
      layout(title = 'Victoria DWGM daily 6am price',
             xaxis = list(type='date',title = "",tickformat = "%d %B<br>(%a)"),
             yaxis = list(side = 'left', title = 'Demand', showgrid = T, zeroline = T, range=c(min(dwgm$total_gas_used,na.rm = T)-100,max(dwgm$total_gas_used,na.rm = T)+50)),
             yaxis2 = list(side = 'right', overlaying = "y", title = 'Price', showgrid = F, zeroline = T,range=c(min(dwgm$price_value,na.rm = T)-1,max(dwgm$price_value,na.rm = T)+0.2)),
             legend = list(x = 100, y = 1.2)) %>% 
      config(displayModeBar = F)
  })
  output$downloadData6 <- downloadHandler(
    filename = "gsh.csv",
    content = function(file) {
      write.csv(gsh, file)
    }
  )
  output$GSH = DT::renderDataTable({
    datatable(gsh,
              style = "bootstrap")
  })
  output$text<-renderText("All information on this dashboard is publicly available courtesy of the <a href='https://aemo.com.au/en'>Australian Energy Market Operator (AEMO)</b></a>.<br><br><b>The data is updated twice daily at 0845 and 2045 UTC.</b></a><br><br>Visit the <a href=https://www.linkedin.com/in/jonahfoong/> dashboard creator</a><br>Link to <a href='https://github.com/jonfoong/aus_shiny_docker'> Github repo</a><br>Link to <a href='https://hub.docker.com/repository/docker/jonfoong/aus_shiny'> Dockerhub repo</a>")
}
shinyApp(ui, server)
