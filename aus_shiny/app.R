library(shiny)
library(plotly)
library(shinythemes)
library(googledrive)
library(DT)

# authenticate gdrive

drive_auth(path = "token.json")

# get id of rds file and download

all_ids <- drive_ls()

id <- all_ids$id[all_ids$name == "list_df.rds"]

# download to root folder

drive_download(as_id(id), overwrite = TRUE)

list_df <- readRDS("list_df.rds")

# assign dfs to vars

for (i in seq_along(list_df)){

  assign(names(list_df[i]), list_df[[i]])

}

# UI

ui <- navbarPage(theme = shinytheme("flatly"),
               "East Australia gas dashboard",
               tabPanel("Victoria DWGM",
                        actionButton(inputId = 'onewdwgm',label = '1w'),
                        actionButton(inputId = 'twowdwgm',label = '2w'),
                        downloadButton("downloadData_dwgm", " Export"),
                        plotlyOutput("DWGM")),
               tabPanel("Pipeline",
                        actionButton(inputId = 'threemonthpipe',label = '3m'),
                        actionButton(inputId = 'sixmonthpipe',label = '6m'),
                        actionButton(inputId = 'oneyearpipe',label = '1y'),
                        downloadButton("downloadData_pipeline", " Export"),
                        plotlyOutput("pipeline")),
               tabPanel("LNG Production",
                        actionButton(inputId = 'threemonthlng',label = '3m'),
                        actionButton(inputId = 'sixmonthlng',label = '6m'),
                        actionButton(inputId = 'oneyearlng',label = '1y'),
                        downloadButton("downloadData_lng", " Export"),
                        plotlyOutput("lng")),
               tabPanel("Longford Production",
                        actionButton(inputId = 'threemonth',label = '3m'),
                        actionButton(inputId = 'sixmonth',label = '6m'),
                        actionButton(inputId = 'oneyear',label = '1y'),
                        downloadButton("downloadData_longford", " Export"),
                        plotlyOutput("longford")),
               tabPanel("Storage",
                        actionButton(inputId = 'threemonthstorage',label = '3m'),
                        actionButton(inputId = 'sixmonthstorage',label = '6m'),
                        actionButton(inputId = 'oneyearstorage',label = '1y'),
                        downloadButton("downloadData_storage", " Export"),
                        plotlyOutput("storage")),
               tabPanel("Gas Supply Hub",
                        downloadButton("downloadData_gsh", " Export"),
                        dataTableOutput("GSH")),
               tabPanel("Data Collection",
                        htmlOutput("about"))
)

# server

server <- function(input, output){
  
  # dwgm
  
  rv_dwgm <- reactiveValues(data = dwgm)
  observeEvent(input$onewdwgm, 
               {rv_dwgm$data <- dwgm |> 
                 subset(!gas_date < max(dwgm$gas_date) - 6)})
  observeEvent(input$twowdwgm, 
               {rv_dwgm$data <- dwgm |> 
                 subset(!gas_date < max(dwgm$gas_date) - 13)})
  output$downloadData_dwgm <- downloadHandler(
    filename = "dwgm.csv",
    content = function(file) write.csv(rv_dwgm$data, file)
  )
  
  output$DWGM <- renderPlotly({
    rv_dwgm$data |> plot_ly() |> 
      add_trace(x = ~gas_date, y = ~total_gas_used, type = 'bar', textposition = "inside", name = 'Demand (TJ/d)',
                marker = list(color = '#C9EFF9'),
                hoverinfo = "text",
                text = ~paste(total_gas_used, 'TJ', sep='')) |> 
      add_trace(x = ~gas_date, y = ~price_value, type = 'scatter', mode = 'lines', name = 'Price (A$/GJ)', yaxis = 'y2',
                line = list(color = '#45171D'),
                hoverinfo = "text",
                text = ~paste('A$', round(price_value,2), sep='')) |> 
      layout(title = 'Victoria DWGM daily 6am price',
             xaxis = list(type='date', title = "", tickformat = "%d %B<br>(%a)"),
             yaxis = list(side = 'left', title = 'Demand', showgrid = T, zeroline = T, 
                          range = c(min(dwgm$total_gas_used, na.rm = T) - 100, max(dwgm$total_gas_used,na.rm = T) + 50)),
             yaxis2 = list(side = 'right', overlaying = "y", title = 'Price', showgrid = F, zeroline = T, 
                           range = c(min(dwgm$price_value, na.rm = T)-1, max(dwgm$price_value, na.rm = T) + 0.2)),
             legend = list(x = 100, y = 1.2)) |> 
      plotly::config(displayModeBar = F)
  })
  
  # pipeline 
  
  rv_pipeline <- reactiveValues(data = pipeline)
  observeEvent(input$threemonthpipe, 
               {rv_pipeline$data <- pipeline |> 
                 subset(!Date < max(pipeline$Date) - 90)})
  observeEvent(input$sixmonthpipe, 
               {rv_pipeline$data <- pipeline |> 
                 subset(!Date < max(pipeline$Date) - 180)})
  observeEvent(input$oneyearpipe, 
               {rv_pipeline$data <- pipeline |> 
                 subset(!Date < max(pipeline$Date) - 364)})
  output$downloadData_pipeline <- downloadHandler(
    filename = "pipeline.csv",
    content = function(file) write.csv(rv_pipeline$data, file)
  )
  
  output$pipeline <- renderPlotly({
    rv_pipeline$data |> 
      plot_ly(x = ~Date, y = ~flow, color = ~pipe, type = 'scatter', mode = 'lines') |>
      layout(hovermode = "x unified", title = 'Pipeline flows',
             yaxis = list(title = 'Flows (TJ/d)',
                          zeroline = TRUE)) |>
      plotly::config(displayModeBar = F)
  })
  
  # lng
  
  rv_lng <- reactiveValues(data = lng)
  observeEvent(input$threemonthlng, 
               {rv_lng$data <- lng |> 
                 subset(!Date < max(lng$Date) - 90)})
  observeEvent(input$sixmonthlng,
               {rv_lng$data <- lng |> 
                 subset(!Date < max(lng$Date) - 180)})
  observeEvent(input$oneyearlng, 
               {rv_lng$data <- lng |> 
                 subset(!Date < max(lng$Date) - 364)})
  output$downloadData_lng <- downloadHandler(
    filename = "lng.csv",
    content = function(file) write.csv(rv_lng$data, file)
  )
  
  output$lng <- renderPlotly({
    rv_lng$data |> 
      plot_ly(x = ~Date, y = ~flow, color = ~pipe, type = 'scatter', mode = 'lines') |>
      layout(hovermode = "x unified", title = 'LNG production pipeline flows',
             yaxis = list(title = 'Flows (TJ/d)',
                          zeroline = TRUE)) |>
      plotly::config(displayModeBar = F)
  })
  
  # longford
  
  rv_longford <- reactiveValues(data = longford)
  observeEvent(input$threemonth, ignoreInit = F, 
               {rv_longford$data <- longford |> 
                 subset(!Date < max(storage$Date) - 90)})
  observeEvent(input$sixmonth,ignoreInit = T, 
               {rv_longford$data <- longford |> 
                 subset(!Date < max(storage$Date) - 180)})
  observeEvent(input$oneyear,ignoreInit = T,
               {rv_longford$data <- longford |> 
                 subset(!Date < max(storage$Date) - 364)})
  output$downloadData_longford <- downloadHandler(
    filename = "longford.csv",
    content = function(file) write.csv(rv_longford$data, file)
  )
  
  output$longford <- renderPlotly({
    rv_longford$data |> 
      plot_ly(x = ~Date, y = ~utilisation, type = 'scatter', mode = 'lines') |>
      layout(hovermode = "x unified", title = 'Longford production',
             yaxis = list(title = 'Utilisation rate (%)',
                          zeroline = TRUE)) |>
      plotly::config(displayModeBar = F)
  })
  
  # storage
  
  rv_storage <- reactiveValues(data = storage)
  observeEvent(input$threemonthstorage, 
               {rv_storage$data <- storage |> 
                 subset(!Date < max(storage$Date) - 90)})
  observeEvent(input$sixmonthstorage, 
               {rv2$data <- storage |> 
                 subset(!Date < max(storage$Date) - 180)})
  observeEvent(input$oneyearstorage, 
               {rv2$data <- storage |> 
                 subset(!Date < max(storage$Date) - 364)})
  output$downloadData_storage <- downloadHandler(
    filename = "storage.csv",
    content = function(file) write.csv(rv_storage$data, file)
  )
  
  output$storage <- renderPlotly({
    rv_storage$data |> 
      plot_ly(x = ~Date, y = ~Levels, color = ~facility, type = 'scatter', mode = 'lines') |>
      layout(hovermode="x unified", title = 'Storage',
             yaxis = list(title = 'Level (%)',
                          zeroline = TRUE)) |>
      plotly::config(displayModeBar = F)
  })
  
  # gsh
  
  output$downloadData_gsh <- downloadHandler(
    filename = "gsh.csv",
    content = function(file) write.csv(gsh, file)
  )
  
  output$GSH = DT::renderDataTable({
    datatable(gsh, style = "bootstrap")
  })
  
  # Data Collection tab
  
  output$about <- renderText("All information on this dashboard is publicly available courtesy of the <a href='https://aemo.com.au/en' target='_blank'>Australian Energy Market Operator (AEMO)</b></a>.<br><br><b>The data is updated twice daily at 0845 and 2045 UTC + 10.</b></a><br><br>Visit the <a href='https://jonfoong.github.io/' target='_blank'> dashboard creator</a><br>Link to <a href='https://github.com/jonfoong/aus_shiny_docker' target='_blank'> Github repo</a><br>Link to <a href='https://hub.docker.com/r/jonfoong/aus_shiny' target='_blank'> Dockerhub repo</a>")
}

shinyApp(ui, server)
