library(shiny)
library(dplyr)
library(corrplot)
library(cluster)
library(fpc)
library(ggdendro)
library(ggplot2)
library(dendextend)
library(expss)

load("data/states.RData")
raw_data<-states.data
names(raw_data)<-c('pop_total','net_dom_immigration','am_migrant_abroad',
               'net_inter_immigration','birth_rate','mortality_rate','pop_under_65','pop_over_65')


ui<-navbarPage(title = "Unsupervised learning techniques",
  tabPanel("Home",
           fluidPage(titlePanel("What's up"),
                     sidebarLayout(
                       sidebarPanel(helpText("This webapp will help you to classify states of USA using 
                                       their demographic evolution datas."),
                                    p("You can choose between two unsupervised methods:"),
                                    strong("- K-Means algorithm"),br(),
                                    strong("- Hierarchical Ascending Classification"),
                                    br(),
                                    a("More information about K-means method",href="https://en.wikipedia.org/wiki/K-means_clustering"),
                                    br(),
                                    a("More information about CAH algorithm",href="https://en.wikipedia.org/wiki/Hierarchical_clustering"),
                                    selectInput("cor_method_all","Correlation method",choices=c("pearson","spearman"),
                                                selected="spearman")
                       ),
                       mainPanel(
                         tabsetPanel(
                           tabPanel("Overview",tableOutput("overview")),
                           tabPanel("Summary",tableOutput("summary")),
                           tabPanel("Correlation",plotOutput("cor"))
                         )
                      )
                    )
          )
  ),
  tabPanel("Vizualisation",
           fluidPage(titlePanel("Let's vizualise data"),
                     sidebarLayout(
                       sidebarPanel(
                         selectInput("var1","Choose a variable",choices=c("Total population",
                                                                              "Net domestic immigration",
                                                                              "Net international immigration",
                                                                              "Birth rate",
                                                                              "Mortality rate",
                                                                              "Population under 65 years",
                                                                              "Population over 65 years",
                                                                              "American migrants abroad"),
                                     selected="Total Population"),
                         numericInput("bins1","Numer of bins",min=10,max=100,value=10),
                         selectInput("var2","Choose a variable",choices=c("Total population",
                                                                              "Net domestic immigration",
                                                                              "Net international immigration",
                                                                              "Birth rate",
                                                                              "Mortality rate",
                                                                              "Population under 65 years",
                                                                              "Population over 65 years",
                                                                              "American migrants abroad"),
                                     selected="Total Population"),
                         numericInput("bins2","Numer of bins",min=10,max=100,value=10),
                         selectInput("cor_method","Correlation method",choices=c("pearson","spearman"),
                                     selected="spearman"),
                         selectInput("var3","Choose a variable",choices=c("Total population",
                                                                          "Net domestic immigration",
                                                                          "Net international immigration",
                                                                          "Birth rate",
                                                                          "Mortality rate",
                                                                          "Population under 65 years",
                                                                          "Population over 65 years",
                                                                          "American migrants abroad"),
                                     selected="Total Population"),
                         numericInput("bins3","Numer of bins",min=10,max=100,value=10),
                         selectInput("cor_method","Correlation method",choices=c("pearson","spearman"),
                                     selected="spearman"),
                         selectInput("var4","Choose a variable",choices=c("Total population",
                                                                          "Net domestic immigration",
                                                                          "Net international immigration",
                                                                          "Birth rate",
                                                                          "Mortality rate",
                                                                          "Population under 65 years",
                                                                          "Population over 65 years",
                                                                          "American migrants abroad"),
                                     selected="Total Population"),
                         numericInput("bins4","Numer of bins",min=10,max=100,value=10),
                         selectInput("cor_method","Correlation method",choices=c("pearson","spearman"),
                                     selected="spearman")
                       ),
                       mainPanel(
                         tabsetPanel(
                           tabPanel("Tables",
                                    fluidRow(
                                      column(6,textOutput("text1_table"),br(),tableOutput("table_viz1")),
                                      column(6,textOutput("text2_table"),br(),tableOutput("table_viz2"))
                                    ),
                                    fluidRow(
                                      column(6,textOutput("text3_table"),br(),tableOutput("table_viz3")),
                                      column(6,textOutput("text4_table"),br(),tableOutput("table_viz4"))
                                    )
                            ),
                           tabPanel("Graphics",
                                    fluidRow(
                                      column(6,textOutput("text1_plot"),br(),plotOutput("plot_viz1")),
                                      column(6,textOutput("text2_plot"),br(),plotOutput("plot_viz2"))
                                    ),
                                    fluidRow(
                                      column(6,textOutput("text3_plot"),br(),plotOutput("plot_viz3")),
                                      column(6,textOutput("text4_plot"),br(),plotOutput("plot_viz4"))
                                    )
                            )
                         )
                       )
                     )
           )
  ),
  navbarMenu("Choose",
             tabPanel("k-Means",
                      fluidPage(
                        
                      )),
             tabPanel("CAH",
                      fluidPage(
                        
                      ))
             )
  
)

# server function ----

server<-function(input,output){
  
  #head table----
  output$overview<-renderTable({
    head(raw_data,n=10) 
  },digits=2,align="c",bordered=TRUE,rownames=TRUE)
  
  #summary table----
  output$summary<-renderTable({
    summary(raw_data)
  })
  
  #correlation plot----
  statecor <- reactive({cor(raw_data,method=input$cor_method_all)})
  output$cor<-renderPlot({
    corrplot(as.matrix(statecor()), method='color', addCoef.col = "black", type="lower", 
             order="hclust", tl.col="black", tl.srt=10,title='Correlation matrice')
  })
  
  #summary table 1----
  output$text1_table<-renderText({paste("Description of ",input$var1)})
  
  output$table_viz1<-renderTable({
    var1_switch<-switch(input$var1,
                        "Total population"=raw_data$pop_total,
                        "Net domestic immigration"=raw_data$net_dom_immigration,
                        "Net international immigration"=raw_data$net_inter_immigration,
                        "Birth rate"=raw_data$birth_rate,
                        "Mortality rate"=raw_data$mortality_rate,
                        "Population under 65 years"=raw_data$pop_under_65,
                        "Population over 65 years"=raw_data$pop_over_65,
                        "American migrants abroad"=raw_data$am_migrant_abroad)
    as.array(summary(var1_switch))
    })
  
  #summary table 2----
  output$text2_table<-renderText({paste("Description of ",input$var2)})
  
  output$table_viz2<-renderTable({
    var2_switch<-switch(input$var2,
                        "Total population"=raw_data$pop_total,
                        "Net domestic immigration"=raw_data$net_dom_immigration,
                        "Net international immigration"=raw_data$net_inter_immigration,
                        "Birth rate"=raw_data$birth_rate,
                        "Mortality rate"=raw_data$mortality_rate,
                        "Population under 65 years"=raw_data$pop_under_65,
                        "Population over 65 years"=raw_data$pop_over_65,
                        "American migrants abroad"=raw_data$am_migrant_abroad)
    as.array(summary(var2_switch))
    })

  
  #summary table 3----
  output$text3_table<-renderText({paste("Description of ",input$var3)})
  
  output$table_viz3<-renderTable({
    var3_switch<-switch(input$var3,
                        "Total population"=raw_data$pop_total,
                        "Net domestic immigration"=raw_data$net_dom_immigration,
                        "Net international immigration"=raw_data$net_inter_immigration,
                        "Birth rate"=raw_data$birth_rate,
                        "Mortality rate"=raw_data$mortality_rate,
                        "Population under 65 years"=raw_data$pop_under_65,
                        "Population over 65 years"=raw_data$pop_over_65,
                        "American migrants abroad"=raw_data$am_migrant_abroad)
    as.array(summary(var3_switch))
  })
  
  #summary table 4----
  output$text4_table<-renderText({paste("Description of ",input$var4)})
  
  output$table_viz4<-renderTable({
    var4_switch<-switch(input$var4,
                        "Total population"=raw_data$pop_total,
                        "Net domestic immigration"=raw_data$net_dom_immigration,
                        "Net international immigration"=raw_data$net_inter_immigration,
                        "Birth rate"=raw_data$birth_rate,
                        "Mortality rate"=raw_data$mortality_rate,
                        "Population under 65 years"=raw_data$pop_under_65,
                        "Population over 65 years"=raw_data$pop_over_65,
                        "American migrants abroad"=raw_data$am_migrant_abroad)
    as.array(summary(var4_switch))
  })
  
  #Plot var1
  output$text1_plot<-renderText({paste('Visualization of ',input$var1)})
  output$plot_viz1<-renderPlot({
    var1_switch<-switch(input$var1,
                        "Total population"=raw_data$pop_total,
                        "Net domestic immigration"=raw_data$net_dom_immigration,
                        "Net international immigration"=raw_data$net_inter_immigration,
                        "Birth rate"=raw_data$birth_rate,
                        "Mortality rate"=raw_data$mortality_rate,
                        "Population under 65 years"=raw_data$pop_under_65,
                        "Population over 65 years"=raw_data$pop_over_65,
                        "American migrants abroad"=raw_data$am_migrant_abroad)
    ggplot(raw_data, aes(x=var1_switch)) + 
      geom_histogram(aes(y = ..density..),
                     colour = 1, fill = "white",bins=input$bins1) +
      geom_density(lwd = 1, colour = 5,
                   fill = 5, alpha = 0.25)+ggtitle(paste('Histogram of ',input$var1))+
      xlab(input$var2)+ylab("")
  })
  
  #Plot var2
  output$text2_plot<-renderText({paste('Visualization of ',input$var2)})
  output$plot_viz2<-renderPlot({
    var2_switch<-switch(input$var2,
                        "Total population"=raw_data$pop_total,
                        "Net domestic immigration"=raw_data$net_dom_immigration,
                        "Net international immigration"=raw_data$net_inter_immigration,
                        "Birth rate"=raw_data$birth_rate,
                        "Mortality rate"=raw_data$mortality_rate,
                        "Population under 65 years"=raw_data$pop_under_65,
                        "Population over 65 years"=raw_data$pop_over_65,
                        "American migrants abroad"=raw_data$am_migrant_abroad)
    ggplot(raw_data, aes(x=var2_switch)) + 
      geom_histogram(aes(y = ..density..),
                     colour = 1, fill = "white",bins=input$bins2) +
      geom_density(lwd = 1, colour = 6,
                   fill = 6, alpha = 0.25)+ggtitle(paste('Histogram of ',input$var2))+
      xlab(input$var2)+ylab("")
  })
  
  #Plot var3
  output$text3_plot<-renderText({paste('Visualization of ',input$var3)})
  output$plot_viz3<-renderPlot({
    var3_switch<-switch(input$var3,
                        "Total population"=raw_data$pop_total,
                        "Net domestic immigration"=raw_data$net_dom_immigration,
                        "Net international immigration"=raw_data$net_inter_immigration,
                        "Birth rate"=raw_data$birth_rate,
                        "Mortality rate"=raw_data$mortality_rate,
                        "Population under 65 years"=raw_data$pop_under_65,
                        "Population over 65 years"=raw_data$pop_over_65,
                        "American migrants abroad"=raw_data$am_migrant_abroad)
    ggplot(raw_data, aes(x=var3_switch)) + 
      geom_histogram(aes(y = ..density..),
                     colour = 1, fill = "white",bins=input$bins3) +
      geom_density(lwd = 1, colour = 7,
                   fill = 7, alpha = 0.25)+ggtitle(paste('Histogram of ',input$var3))+
      xlab(input$var3)+ylab("")
  })
  
  #Plot var4
  output$text4_plot<-renderText({paste('Visualization of ',input$var4)})
  output$plot_viz4<-renderPlot({
    var4_switch<-switch(input$var4,
                        "Total population"=raw_data$pop_total,
                        "Net domestic immigration"=raw_data$net_dom_immigration,
                        "Net international immigration"=raw_data$net_inter_immigration,
                        "Birth rate"=raw_data$birth_rate,
                        "Mortality rate"=raw_data$mortality_rate,
                        "Population under 65 years"=raw_data$pop_under_65,
                        "Population over 65 years"=raw_data$pop_over_65,
                        "American migrants abroad"=raw_data$am_migrant_abroad)
    ggplot(raw_data, aes(x=var4_switch)) + 
      geom_histogram(aes(y = ..density..),
                     colour = 1, fill = "white",bins=input$bins4) +
      geom_density(lwd = 1, colour = 8,
                   fill = 8, alpha = 0.25)+ggtitle(paste('Histogram of ',input$var4))+
      xlab(input$var4)+ylab("")
  })
  
  
  
}


shinyApp(ui = ui, server = server)
