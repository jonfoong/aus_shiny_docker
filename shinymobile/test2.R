library(shiny)
library(shinyMobile)
library(plotly)
library(tidyverse)
library(shinythemes)
library(DT)
library(googledrive)

temp<-tempfile(fileext = '.json')
download.file('https://www.dropbox.com/s/twobcoxscpv77h6/rcloud-298921-bdc6e1c25160.json?dl=1',temp)
drive_auth(path=temp)

d<-list()
n<-drive_ls()$id
for (i in n){
  temp <- tempfile(fileext = '.csv')
  d[which(n==i)]<-list(read.csv(drive_download(
    as_id(i), path = temp, type='csv',overwrite = TRUE)$local_path)[,-1])
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


##UI

ui = f7Page(
  title = "East Australia gas dashboard",
  allowPWA = TRUE,
  f7TabLayout(
    navbar = f7Navbar(
      title = "East Australia gas dashboard",
      hairline = TRUE,
      shadow = TRUE,
      leftPanel = F,
      rightPanel = F,
      bigger = TRUE,
      transparent = FALSE
    ),
    f7Tabs(
      id = "tabset",
      f7Tab(
        tabName = "Victoria DWGM",
        actionButton(inputId = 'onewdwgm',label = '1w'),
        actionButton(inputId = 'twowdwgm',label = '2w'),
        downloadButton("downloadData5", " Export"),
        plotlyOutput("DWGM")),
      f7Tab(
        tabName = "Pipeline",
        actionButton(inputId = 'threemonthpipe',label = '3m'),
        actionButton(inputId = 'sixmonthpipe',label = '6m'),
        actionButton(inputId = 'oneyearpipe',label = '1y'),
        downloadButton("downloadData1", " Export"),
        plotlyOutput("pipeline"))
      )
  )
)

server<-function(input,output){
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
}
shinyApp(ui, server)

      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      