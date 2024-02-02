#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Set defalut directory ----
setwd(dir="T:/BU STAT/Auto-formations/OOA/RSHINY/stockviz")

# Load packages ----
library(shiny)
library(quantmod)
library(plotly)
library(tidyverse)
library(scales)
library(skimr)
library(shinythemes)

# Source helpers ----
source("helpers.R")

# User interface ----
ui <- fluidPage(theme=shinytheme("cosmo"),
    titlePanel("Stocks vizualisation"),
    
    sidebarLayout(
        sidebarPanel(
            h3(strong("Welcome to stocks vizualisation web application")),
            br(),
            helpText("This app will help you to track stocks over time.
                    Information will be collected from Yahoo finance."),
            br(),
            helpText("For more information about data please visit ",
            a("Yahoo finance stocks data", 
              href="https://finance.yahoo.com/lookup")),
            textInput("symb", "Please, select a stock which you 
                        want to examine.", "SPY"),
            dateRangeInput("dates",
                           "Date range",
                           start = "2013-01-01",
                           end = as.character(Sys.Date())),
            
            br(),
            br(),
            checkboxInput("log", "Plot y axis on log scale",
                          value = FALSE),
            
            checkboxInput("adjust",
                          "Adjust prices for inflation", value = FALSE),
            br(),
            img(src = "EFOR.png", height = 70, width = 100),
            br(),
            "This app is a product of ", 
            span("EFOR", style = "color:blue")
        ),
        
        mainPanel(
            tabsetPanel( 
                tabPanel("Overview",
                    br(),
                    strong(textOutput("title")),
                    br(),
                    br(),
                    dataTableOutput("resume"),
                    plotlyOutput("plot")
                ),
                tabPanel("Forecast"
                )
            )
        )
    )
)

# Server logic
server <- function(input, output) {
    
    dataInput <- reactive({
        getSymbols(input$symb, src = "yahoo",
                   from = input$dates[1],
                   to = input$dates[2],
                   auto.assign = FALSE)
        
    })
    
    finalInput <- reactive({
        if (!input$adjust) return(dataInput())
        adjust(dataInput())
    })
    
    output$title<-renderText({
        paste0("Evolution of ", input$symb, " from ", input$dates[1], " to ", input$dates[2])
    })
    
    output$resume<-renderDataTable({
        df<- as.data.frame(finalInput())[c(-5,-6)]
        res<-skim(df)
        res<-res[c(2,5,6,7,11,12)]
        names(res)<-c("Variable","Mean","SD","Min","Max","Distr")
        res
    })
    
    output$plot <- renderPlotly({
        df<-as.data.frame(finalInput())[c(-5,-6)]
        df$date<-row.names(df)
        finaldata <- df %>%
            select(colnames(df)) %>%
            gather(key = "Legend", value = "value", -date)
        if (!input$log) 
            {finaldata$value<-finaldata$value} 
        else {finaldata$value<-log(finaldata$value)}
        startTime = as.Date(input$dates[1])
        endTime = as.Date(input$dates[2])
        start.end = c(startTime,endTime)
        p<-ggplot(data = finaldata, aes(x=as.Date(date), y=value, group=Legend))+ 
            geom_line(aes(color=Legend, group=Legend))+
            ggtitle("")+
            theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1))+
            xlab("Date")+
            ylab(if(!input$log) {"Value"} else {"Log value"})+
            (scale_x_date(limits=start.end,breaks=date_breaks("1 year")))
        ggplotly(p)
    })
    
}

# Run the app ----
shinyApp(ui, server)