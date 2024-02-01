library(shiny)
library(dplyr)
library(corrplot)
library(cluster)
library(fpc)
library(ggdendro)
library(ggplot2)
library(dendextend)
library(DT)
library(plotly)
library(factoextra)
library(usmap)
library(skimr)
library(shinythemes)

# Global code run once app is open----
#load("T:/BU STAT/Auto-formations/OOA/RSHINY/data/states.RData")
load("data/states.RData")
raw_data_old<-states.data
raw_data=raw_data_old
names(raw_data_old)<-c('Total population','Net Domestic Immigration (NDI)','American Migrants Abroad (AMO)',
                   'Net International Immigration (NII)','Birth rate','Mortality rate','Population under 65 years old','Population over 65 years old')
names(raw_data)<-c('pop_total','net_dom_immigration','am_migrant_abroad','net_inter_immigration','birth_rate','mortality_rate','pop_under_65','pop_over_65y')
raw_data_scale<-raw_data[2:8]
raw_data_scale=scale(raw_data_scale)

#Kmeans general results algorithms
datascale.kmean=kmeansruns(raw_data_scale, krange=1:12)
print_res<-as.data.frame(datascale.kmean$cluster)
print_res<-cbind(rownames(print_res),print_res)%>%as.data.frame
names(print_res)=c("full","cluster")
print_res=left_join(print_res,statepop,by="full")

#CAH general results 
datascale.dist = dist(raw_data_scale,method="euclidean", diag=FALSE) 
datascale.ag = hclust(datascale.dist,method="ward.D") 
inertie <- sort(datascale.ag$height, decreasing = TRUE)
dif=rep(0,length(inertie-1))
for (i in 1:length(inertie)){
  dif[i]=inertie[i+1]-inertie[i]
}
nb_cluster_opt=which(dif==min(dif,na.rm=TRUE))+1

# Code for app----

## User Interface code ----
ui<-navbarPage(title = "Unsupervised learning",
               tabPanel("Home",
                        fluidPage(theme = shinytheme("cosmo"),titlePanel("WELCOME "),
                                  sidebarLayout(
                                    sidebarPanel(helpText("This webapp will help you to classify states of USA using 
                                       their demographic datas."),
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
                                        tabPanel("Overview",br(),
                                                 p("This is an overview of demographics data of USA states's. You can download it by clicking here",a("here",href="https://github.com/oagbohouto86/rshiny_cah_kmean/tree/master/APP-4/data")),
                                                 br(),
                                                 tableOutput("overview")),
                                        tabPanel("Summary",br(), p("Here presented summary of demographics data"), br(), br(), dataTableOutput("summary")),
                                        tabPanel("Correlation matrix" ,h4(strong("Correlation between demographics indicator"),style="text-align:center"), plotOutput("cor", height = "600px" ))
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
                                      numericInput("bins1","Numer of bins",min=10,max=100,value=50),
                                      selectInput("var2","Choose a variable",choices=c("Total population",
                                                                                       "Net domestic immigration",
                                                                                       "Net international immigration",
                                                                                       "Birth rate",
                                                                                       "Mortality rate",
                                                                                       "Population under 65 years",
                                                                                       "Population over 65 years",
                                                                                       "American migrants abroad"),
                                                  selected="Net domestic immigration"),
                                      numericInput("bins2","Numer of bins",min=10,max=100,value=50),
                                      selectInput("var3","Choose a variable",choices=c("Total population",
                                                                                       "Net domestic immigration",
                                                                                       "Net international immigration",
                                                                                       "Birth rate",
                                                                                       "Mortality rate",
                                                                                       "Population under 65 years",
                                                                                       "Population over 65 years",
                                                                                       "American migrants abroad"),
                                                  selected="Net international immigration"),
                                      numericInput("bins3","Numer of bins",min=10,max=100,value=50),
                                      selectInput("var4","Choose a variable",choices=c("Total population",
                                                                                       "Net domestic immigration",
                                                                                       "Net international immigration",
                                                                                       "Birth rate",
                                                                                       "Mortality rate",
                                                                                       "Population under 65 years",
                                                                                       "Population over 65 years",
                                                                                       "American migrants abroad"),
                                                  selected="Birth rate"),
                                      numericInput("bins4","Numer of bins",min=10,max=100,value=50)
                                    ),
                                    mainPanel(
                                      fluidRow(
                                        column(6,textOutput("text1_plot"),plotlyOutput("plot_viz1")),
                                        column(6,textOutput("text2_plot"),plotlyOutput("plot_viz2"))
                                              ),
                                      br(),br(),
                                      fluidRow(
                                        column(6,textOutput("text3_plot"),plotlyOutput("plot_viz3")),
                                        column(6,textOutput("text4_plot"),plotlyOutput("plot_viz4"))
                                              )
                                    )

                                    )
                              )
                    ),
               navbarMenu("Clustering",
                          tabPanel("K-Means",
                                   fluidPage(titlePanel="Let's use k-means algorithms",
                                             sidebarLayout(
                                               sidebarPanel(
                                               p("Note: First tab present general result by using the optimal number of
                                                  cluster using minimization of within variance and maximization of between variance."),
                                               p("You can personnalize your clustering by choosing a number of cluster and visualize
                                                result on", strong("Build your own cluster") ,"tab."),
                                                numericInput("choosek","Please select a number of cluster",
                                                              min=2,max=12,value=2)
                                               ),
                                               mainPanel(tabsetPanel(
                                                 tabPanel("General results",br(),
                                                          fluidRow(plotlyOutput("map_general")),br(),
                                                          fluidRow(textOutput("optk"),br(),br(),
                                                                   column(6,textOutput("size_cluster1"),br(),dataTableOutput("cluster_1")),
                                                                   column(6,textOutput("size_cluster2"),br(),dataTableOutput("cluster_2"))
                                                          )
                                                 ),
                                                 tabPanel("Build your own cluster",br(),
                                                          textOutput("text_choosek"),
                                                          h5("These are you clusters"),br(),
                                                          plotlyOutput("map_choose"),br(),
                                                          helpText("Hint: Tap the numero or states names in 'Search' widgets to see profil of this states or member of this cluster"),br(),
                                                          dataTableOutput("cluster_choose")
                                                 )
                                               )
                                               )
                                             )
                                   )
                          ),
                          tabPanel("CAH",
                                   fluidPage(titlePanel="Let's use CAH algorithms",
                                             sidebarLayout(
                                               sidebarPanel(
                                                 helpText("In CAH algorithms, we use euclidean distance to compute distance matrix
                                     and WARD method to compute cluster based on euclidean distance."),
                                                 br(),
                                                 p("Note: First tab present general result by using the optimal number of
                                                  cluster using minimization of within variance and maximization of between variance."),
                                                 p("You can personnalize your clustering by choosing a number of cluster and visualize
                                                result on", strong("Build your own cluster") ,"tab."),
                                                 numericInput("choosek2","Please select a number of cluster",min=2,max=12,value=2)
                                               ),
                                               mainPanel(
                                                 tabsetPanel(
                                                   tabPanel(title = "General result",br(),
                                                            textOutput("optk2"),br(),
                                                            p(strong("This is below the optimal clusters using CAH algorithms")),br(),
                                                            plotOutput("dendo1",width=800,height=700)
                                                   ),
                                                   tabPanel(title="Build your own cluster",br(),
                                                            textOutput("text_choosek2"),br(),
                                                            h5("These are your clusters"),br(),
                                                            plotOutput("dendo2",width=800,height=700)
                                                   )
                                                 )
                                               )
                                             )
                                             
                                   )
                          )
               )
               
)

## server function ----

server<-function(input,output){
  
  #head table----
  output$overview<-renderTable({
    head(raw_data_old,n=80)},rownames = TRUE,options = list(pageLength = 10,autoWidth = TRUE),digits=3
  )
  
  #summary table----
  output$summary<-renderDataTable(options=list(pageLength=9,autowidth=TRUE),{
    res<-skim(raw_data_old)
    res<-res[c(2,5,9,6,7,11,12)]%>%mutate(across(where(is.numeric), round, 2))
    names(res)<-c("Variable","Mean","Median","SD","Min","Max","Distr")
    res
  })
  
  #correlation plot----
  statecor <- reactive({cor(raw_data_old,method=input$cor_method_all)})
  output$cor<-renderPlot({
    corrplot(as.matrix(statecor()), method='number', addCoef.col = "black", type="lower", 
             order="hclust", tl.col="black", tl.srt=20)
  })
  

  #Plot var1
  output$text1_plot<-renderText({paste('Visualization of ',input$var1)})
  output$plot_viz1<-renderPlotly({
    var1_switch<-switch(input$var1,
                        "Total population"=raw_data$pop_total,
                        "Net domestic immigration"=raw_data$net_dom_immigration,
                        "Net international immigration"=raw_data$net_inter_immigration,
                        "Birth rate"=raw_data$birth_rate,
                        "Mortality rate"=raw_data$mortality_rate,
                        "Population under 65 years"=raw_data$pop_under_65,
                        "Population over 65 years"=raw_data$pop_over_65,
                        "American migrants abroad"=raw_data$am_migrant_abroad)
    p<-ggplot(raw_data, aes(x=var1_switch)) + 
      geom_histogram(aes(y = ..density..),
                     colour = 1, fill = "white",bins=input$bins1) +
      geom_density(lwd = 1, colour = 5,
                   fill = 5, alpha = 0.25)+
      xlab(input$var1)+ylab("")
    ggplotly(p)
  })
  
  #Plot var2
  output$text2_plot<-renderText({paste('Visualization of ',input$var2)})
  output$plot_viz2<-renderPlotly({
    var2_switch<-switch(input$var2,
                        "Total population"=raw_data$pop_total,
                        "Net domestic immigration"=raw_data$net_dom_immigration,
                        "Net international immigration"=raw_data$net_inter_immigration,
                        "Birth rate"=raw_data$birth_rate,
                        "Mortality rate"=raw_data$mortality_rate,
                        "Population under 65 years"=raw_data$pop_under_65,
                        "Population over 65 years"=raw_data$pop_over_65,
                        "American migrants abroad"=raw_data$am_migrant_abroad)
    p<-ggplot(raw_data, aes(x=var2_switch)) + 
      geom_histogram(aes(y = ..density..),
                     colour = 1, fill = "white",bins=input$bins2) +
      geom_density(lwd = 1, colour = 6,
                   fill = 6, alpha = 0.25)+
      xlab(input$var2)+ylab("")
    ggplotly(p)
  })
  
  #Plot var3
  output$text3_plot<-renderText({paste('Visualization of ',input$var3)})
  output$plot_viz3<-renderPlotly({
    var3_switch<-switch(input$var3,
                        "Total population"=raw_data$pop_total,
                        "Net domestic immigration"=raw_data$net_dom_immigration,
                        "Net international immigration"=raw_data$net_inter_immigration,
                        "Birth rate"=raw_data$birth_rate,
                        "Mortality rate"=raw_data$mortality_rate,
                        "Population under 65 years"=raw_data$pop_under_65,
                        "Population over 65 years"=raw_data$pop_over_65,
                        "American migrants abroad"=raw_data$am_migrant_abroad)
    p<-ggplot(raw_data, aes(x=var3_switch)) + 
      geom_histogram(aes(y = ..density..),
                     colour = 1, fill = "white",bins=input$bins3) +
      geom_density(lwd = 1, colour = 7,
                   fill = 7, alpha = 0.25)+
      xlab(input$var3)+ylab("")
    ggplotly(p)
  })
  
  #Plot var4
  output$text4_plot<-renderText({paste('Visualization of ',input$var4)})
  output$plot_viz4<-renderPlotly({
    var4_switch<-switch(input$var4,
                        "Total population"=raw_data$pop_total,
                        "Net domestic immigration"=raw_data$net_dom_immigration,
                        "Net international immigration"=raw_data$net_inter_immigration,
                        "Birth rate"=raw_data$birth_rate,
                        "Mortality rate"=raw_data$mortality_rate,
                        "Population under 65 years"=raw_data$pop_under_65,
                        "Population over 65 years"=raw_data$pop_over_65,
                        "American migrants abroad"=raw_data$am_migrant_abroad)
    p<-ggplot(raw_data, aes(x=var4_switch)) + 
      geom_histogram(aes(y = ..density..),
                     colour = 1, fill = "white",bins=input$bins4) +
      geom_density(lwd = 1, colour = 8,
                   fill = 8, alpha = 0.25)+
      xlab(input$var4)+ylab("")
    ggplotly(p)
  })
  
  #Text for optimal number of k
  output$optk<-renderText({
    paste('Optimal number of cluster is: ',datascale.kmean$bestk)
  })
  
  #text for cluster 1 size
  output$size_cluster1<-renderText({
    paste("Size of cluster 1: ", datascale.kmean$size[1])
  })
  #Table for cluster 1
  output$cluster_1<-renderDataTable(options = list(
    pageLength = 20),{
      res<-as.data.frame(which(datascale.kmean$cluster==1))%>%rownames%>%as.data.frame
      names(res)=c("States of cluster 1")
      res
    })
  
  #text for cluster 2 size
  output$size_cluster2<-renderText({
    paste("Size of cluster 2: ", datascale.kmean$size[2])
  })
  #Table for cluster 2
  output$cluster_2<-renderDataTable(options = list(
    pageLength = 20),{
      res<-as.data.frame(which(datascale.kmean$cluster==2))%>%rownames%>%as.data.frame
      names(res)=c("States of cluster 2")
      res
    })
  
  #map cluster for general
  output$map_general<-renderPlotly({
    p<-plot_usmap(data=print_res,values="cluster",color = "black",labels=TRUE) + 
      scale_fill_continuous(low = "white", high = "blue", name = "Cluster") +
      theme(legend.position = "right")
    ggplotly(p,dynamicTicks = TRUE)%>%style( hoverinfo = "none")
  })
  #Text for choosen number of k KMEANS
  output$text_choosek<-renderText({
    paste('You choose to build ',input$choosek, " clusters")
  })
  
  #Table for choosen cluster KMEANS
  output$cluster_choose<-renderDataTable(options = list(
    pageLength = 20),{
      if(is.na(input$choosek)==TRUE){
        result=cbind(rownames(raw_data),raw_data)
        names(result)=c("States",names(result[,2:length(result)]))
        result
      }
      else {
        datascalekmean=kmeansruns(raw_data_scale, krange=input$choosek)
        res=as.data.frame(datascalekmean$cluster)
        result=as.data.frame(cbind(rownames(res),res[,1]))
        names(result)=c("states","cluster")
        result=result[order(result$cluster),]}
    })
  
  #map cluster for CHOOSEN CLUSTER
  output$map_choose<-renderPlotly({
    if (is.na(input$choosek)==TRUE){
      p<-plot_usmap(labels=TRUE)
      ggplotly(p,dynamicTicks = TRUE)%>%style( hoverinfo = "none")
    }
    else {
      datascalekmean=kmeansruns(raw_data_scale, krange=input$choosek)
      print_res=as.data.frame(datascalekmean$cluster)
      print_res<-cbind(rownames(print_res),print_res)%>%as.data.frame
      names(print_res)=c("full","cluster")
      print_res=left_join(print_res,statepop,by="full")
      p<-plot_usmap(data=print_res,values="cluster",color = "black",labels=TRUE) + 
        scale_fill_continuous(low = "white", high = "blue", name = "Cluster") +
        theme(legend.position = "right")
      ggplotly(p,dynamicTicks = TRUE)%>%style( hoverinfo = "none")}
  })
  
  #Text for choosen number of k KMEANS
  output$text_choosek<-renderText({
    paste('You choose to build ',input$choosek, " clusters")
  })
  
  #Text for optimal number of k in CAH
  output$optk2<-renderText({
    paste('Optimal number of cluster is: ',nb_cluster_opt)
  })
  
  #oPTIMAL DENDOGRAM
  output$dendo1<-renderPlot({
    ggplot(color_branches(datascale.ag, k = nb_cluster_opt), labels = TRUE)
    fviz_dend(datascale.ag,k = nb_cluster_opt, cex=1.10, show_labels = TRUE, rect = TRUE, color_labels_by_k = TRUE, ggtheme = theme_void())
  })
  
  #Text for number of cluster select CAH
  output$text_choosek2<-renderText({
    paste("You choose to build ",input$choosek2," clusters")
  })
  
  
  #Dendogram for chosen cluster using CAH
  output$dendo2<-renderPlot({
    datascale.dist = dist(raw_data_scale, method="euclidean", diag=FALSE)
    datascale.ag = hclust(datascale.dist, method="ward.D")
    ggplot(color_branches(datascale.ag, k = input$choosek2), labels = TRUE) 
    fviz_dend(datascale.ag,k = input$choosek2, cex=1.10, show_labels = TRUE, rect = TRUE, color_labels_by_k = TRUE, ggtheme = theme_void())
  })
}

# Run app code ----
shinyApp(ui = ui, server = server)



