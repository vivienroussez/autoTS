#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(dplyr)
library(tidyr)
library(shiny)
library(autoTS)
library(ggplot2)
library(plotly)
library(lubridate)
library(shinycssloaders)

## Create dummy data for example
# dates <- seq(lubridate::as_date("2005-01-01"),lubridate::as_date("2010-12-31"),"month")
# ts1 <- 1:length(dates)/10 + rnorm(length(dates))
# ts2 <- rnorm(length(dates),10,25)
# ex <- data.frame(date=dates,var1=ts1,var2=ts2)

ui <- navbarPage("AutoTS graphical interface",
                 tabPanel("Presentation - what do I need ?",
                          fluidPage(
                            includeMarkdown("Instructions.md")
                          )),
                 tabPanel("Prediction for single series",
                   fluidRow(
                     column(4,
                            wellPanel(
                              fileInput("file1", "Choose CSV File",
                                        accept = c(
                                          "text/csv",
                                          "text/comma-separated-values,text/plain",
                                          ".csv")
                              ),
                              radioButtons("separator","Separator",c("Comma"=",","Semicolon"=";"),inline = T),
                              radioButtons("decimal","Decimal separator",c("Dot"=".","Comma"=","),inline = T),
                              uiOutput("select_date"),
                              uiOutput("select_var"),
                              selectInput("freq","Seasonality",c("day","week","month","quarter"),selected = "month"),
                              selectInput("model_choice","Algorithm selection",
                                          choices = list("ARIMA"="my.sarima","Prophet"="my.prophet",
                                                         "ETS"="my.ets","BATS"="my.bats","TBATS"="my.tbats",
                                                         "STL"="my.stlm","Short term"="my.shortterm","Bagged estimator"="my.bagged",
                                                         "Best model"="best"),
                                          selected = "best"),
                              numericInput("test_length","Number of obs for test data",value = 12),
                              numericInput("pred_ahead","Number of periods for prediction",value=12),
                              downloadButton("downloadData", "Download predictions")
                            )
                     ),
                     column(8,
                            h3("Results of the training step"),
                            plotlyOutput("train") %>%
                              shinycssloaders::withSpinner(color = "#ff0000"),
                            h3("Results of the predictions"),
                            plotlyOutput("pred") %>%
                              shinycssloaders::withSpinner(color = "#ff0000")
                     )

                   )
                 )

)


# Define server logic required to draw a histogram
server <- function(input, output) {

  dat <- reactive({
    inFile <- input$file1
    if (is.null(inFile)) return(NULL)
    data <- read.csv(inFile$datapath, header = TRUE)
    return(data)
  })

  output$select_date <- renderUI({
    selectInput("date","Date variable",names(dat()),selected = names(dat())[1])
  })

  output$select_var <- renderUI({
    selectInput("var","Variable to predict",names(dat()),selected = names(dat())[2])
  })

  train <- reactive({
    validate(need(!is.null(dat()),"Waiting for data"))
    getBestModel(dat()[,input$date],dat()[,input$var],freq = input$freq,graph = F,n_test = input$test_length)
  })

  pred <- reactive({
    validate(need(!is.null(dat()),"Waiting for data"))
    data_ts <- prepare.ts(dat()[,input$date],dat()[,input$var],freq = input$freq)
    if (input$model_choice=="best") res <- my.predictions(data_ts,train()$best,n_pred = input$pred_ahead)
    else res <- my.predictions(data_ts,input$model_choice,n_pred = input$pred_ahead)
    return(res)
  })

  output$train <- renderPlotly({
    train()$graph.train + theme_light()
  })

  output$pred <- renderPlotly({
    dd <- pred()
    gather(dd,key="var",value = "val",-dates,-type) %>%
      mutate(type=ifelse(var=="actual.value","mean",type)) %>%
      filter(type=="mean") %>%
      ggplot(aes(dates,val,color=var)) + geom_line() + theme_light()
  })

  ### Global user instructions for the first panel
  output$instructions <- renderText({
    readLines("Instructions.html")
  })

  output$ex_table <- renderDataTable({
    read.csv("Example.csv",sep=input$separator,dec=input$decimal) %>% as.data.frame()
  })

  output$downloadData <- downloadHandler(
    filename = function() {
      paste("Prediction", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(pred(), file, row.names = FALSE)
    }
  )
}


# Run the application
shinyApp(ui = ui, server = server)

