library(shiny)
library(shinydashboard)
library(ggplot2)
library(tidyverse)
library(tidyquant)
library(plotly)
library("gsubfn")
library("lubridate")
library(shinyWidgets)

# Multi-day events
# Add option to select models in graphs
# Inputs colum-wise
# Garch Model

source("../Market_Momentum.R")
source("../test.R")
server <- function(input, output) {
    abr <- reactive({execute_safely(calc_abr(input$ticker_stock, input$ticker_bench, input$event_date, input$est, input$ant, input$adj), message = "Not a trading day")} )
    
    output$arplot <- renderPlot({ #plots stock returns
      ggplot(data = abr(), aes(x = dates_relative)) +
        geom_line(aes(y = stock_return, color = time_period)) +
        geom_line(aes(y = bench_return, color = "black")) +
        labs(x = 'Trading Days Before Event', y = 'Realised Returns') +
        geom_vline(aes(xintercept = 0), linetype = 2) +
        scale_x_reverse()
      })
    
    output$comp_arplot <- renderPlot({
      ar_compare_models <- abr() |>
        pivot_longer(cols = c(ConstantModel, MarketModel, CAPM), names_to = 'model', values_to = 'value') |>
        select(date, dates_relative, model, value )
      
      ggplot(ar_compare_models, aes(x = dates_relative)) +
        geom_line(aes(y= value, color = model)) +
        labs(x = 'Trading Days Before Event', y = 'Abnormal Returns') +
        geom_vline(aes(xintercept = 0), linetype = 2) +
        scale_x_reverse()
    })

    output$pstat <- renderTable(calc_stats(abr(), input$car_or_bhar))
    
    output$market_trends <- renderPlot({
      result <- calc_sma(ticker_stock = input$ticker_stock, from = input$start_date , to = input$end_date , time_period = input$periodicity, length = input$sma_periods)
      
      ggplot(result, aes(x = date)) +
        geom_line(aes(y = Value, color = Metric)) +
        labs(x="", y = "") +
        theme(legend.position="top")
      
    })
}

ui <- dashboardPage(
  dashboardHeader(title = h2("Time Series Tools")),
  dashboardSidebar(
    menuItem(h3("Event Study", icon("calendar")), tabName = "eventstudy"),
    menuItem(h3("Market Trends", icon = icon("line-chart")), tabName = "markettrends"),
    menuItem(h3("Market Sentiment",icon("bullhorn")), tabName = "marketsentiment"),
    menuItem(h3("About", icon("info-circle")), tabName = "about")
  ),
  
  dashboardBody(skin = "yellow", 
      tabItems(
          tabItem(tabName = "eventstudy", 
            div(style="display: inline-block;vertical-align:top; width: 150px;", numericInput("est", label = h3("Estimation"), value = 30, min = 1)),
            div(style="display: inline-block;vertical-align:top; width: 150px;", numericInput("ant", label = h3("Anticipation"), value = 10, min = 1)),
            div(style="display: inline-block;vertical-align:top; width: 150px;", dateInput("event_date", h3("Date input"), value = '2018-11-05')),
            div(style="display: inline-block;vertical-align:top; width: 150px;", numericInput("adj", label = h3("Adjustment"), value = 10, min = 1)),
            div(style="display: inline-block;vertical-align:top; width: 100px;",HTML("<br>")),
            div(style="display: inline-block;vertical-align:top; width: 150px;", textInput("ticker_stock", h3("Stock Ticker"), value = "ATVI")),
            div(style="display: inline-block;vertical-align:top; width: 150px;", textInput("ticker_bench", h3("Bench Ticker"), value = "^GSPC")),
            plotOutput("arplot"),
            plotOutput("comp_arplot"),
            div(style="display: width: 150px;", selectInput("car_or_bhar", h3("Aggregate Abnormal"), choices = list("CAR" = 1, "BHAR" = 2), selected = 1)),
            tableOutput("pstat")
              ), 
          tabItem(tabName= "markettrends",
            div(style="display: inline-block;vertical-align:top; width: 150px;", dateInput("start_date", label = h3("Start Date"), value = '2018-11-05')),
            div(style="display: inline-block;vertical-align:top; width: 150px;", dateInput("end_date", label = h3("End Date"), value = '2019-11-05')),
            div(style="display: inline-block;vertical-align:top; width: 150px;", selectInput("periodicity", h3("Periodicity"), c('daily', 'monthly'))),
            div(style="display: inline-block;vertical-align:top; width: 150px;", numericInput("sma_periods", label = h3("Periods"), value = 10, min = 1)),
            div(style="display: inline-block;vertical-align:top; width: 150px;", textInput("ticker_stock", h3("Stock Ticker"), value = "MSFT")),
            plotOutput("market_trends"),
            
        )
      )
    )
  )


shinyApp(ui = ui, server = server)

