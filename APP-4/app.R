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

# Global code run once app is open----
load("data/states.RData")
load("data/states.RData")
raw_data_old<-states.data
raw_data=raw_data_old
names(raw_data)<-c('pop_total','net_dom_immigration','am_migrant_abroad',
               'net_inter_immigration','birth_rate','mortality_rate','pop_under_65','pop_over_65')
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
ui<-navbarPage(title = "Unsupervised learning",
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
                           tabPanel("Summary",dataTableOutput("summary")),
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
                         selectInput("var4","Choose a variable",choices=c("Total population",
                                                                          "Net domestic immigration",
                                                                          "Net international immigration",
                                                                          "Birth rate",
                                                                          "Mortality rate",
                                                                          "Population under 65 years",
                                                                          "Population over 65 years",
                                                                          "American migrants abroad"),
                                     selected="Total Population"),
                         numericInput("bins4","Numer of bins",min=10,max=100,value=10)
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
                                      column(6,textOutput("text1_plot"),br(),plotlyOutput("plot_viz1")),
                                      column(6,textOutput("text2_plot"),br(),plotlyOutput("plot_viz2"))
                                    ),
                                    fluidRow(
                                      column(6,textOutput("text3_plot"),br(),plotlyOutput("plot_viz3")),
                                      column(6,textOutput("text4_plot"),br(),plotlyOutput("plot_viz4"))
                                    )
                            )
                         )
                       )
                     )
           )
  ),
  navbarMenu("Choose",
             tabPanel("K-Means",
                      fluidPage(titlePanel="Let's use k-means algorithms",
                                sidebarLayout(
                                  sidebarPanel(
                                    helpText("First tab present general result by using the optimal number of
                                             cluster using minimization of within variance and maximization of between variance.
                                             You can personnalize your clustering by choosing a number of cluster and visualize
                                             result on next tab."),
                                    numericInput("choosek","Please select a number of cluster",
                                                min=2,max=12,value=2)
                                  ),
                                  mainPanel(tabsetPanel(
                                    tabPanel("General results",
                                             fluidRow(textOutput("optk"),
                                               column(6,textOutput("size_cluster1"),dataTableOutput("cluster_1")),
                                               column(6,textOutput("size_cluster2"),dataTableOutput("cluster_2"))
                                             ),
                                             fluidRow(plotlyOutput("map_general"))
                                    ),
                                    tabPanel("Build your own cluster",
                                                textOutput("text_choosek"),br(),
                                                h5("These are you clusters"),
                                                helpText("Hint: Tap the numero or states names in 'Search' widgets to see profil of this states or member of this cluster"),
                                                dataTableOutput("cluster_choose"),
                                                plotlyOutput("map_choose")
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
                                     and WARD method to compute cluster based on distance."),
                            br(),
                            helpText("First tab present general result by using the optimal number of
                                     cluster using minimization of within variance and maximization of between variance.
                                     You can personnalize your clustering by choosing a number of cluster and visualize
                                     result on next tab."),
                            numericInput("choosek2","Please select a number of cluster",min=2,max=12,value=2)
                          ),
                          mainPanel(
                            tabsetPanel(
                              tabPanel(title = "General result",
                                       textOutput("optk2"),
                                       p(strong("This is below the optimal clusters using CAH algorithms")),
                                       plotOutput("dendo1",width=800,height=700)
                              ),
                              tabPanel(title="Build your own cluster",
                                       textOutput("text_choosek2"),
                                       h5("These are your clusters"),
                                       plotOutput("dendo2",width=800,height=700)
                              )
                            )
                          )
                        )
                        
                      )
                )
  )
  
)

# server function ----

server<-function(input,output){
  
  #head table----
  output$overview<-renderTable({
    head(raw_data,n=80)},rownames = TRUE,options = list(pageLength = 10,autoWidth = TRUE),digits=3
  )
  
  #summary table----
  output$summary<-renderDataTable(options=list(pageLength=6,autowidth=TRUE),{
    res<-summary(raw_data_old)%>%as.data.frame
    names(res)<-c("","Variables","Statistics")
    res[,2:3]
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
    res=as.data.frame(as.array(summary(var1_switch)))
    names(res)<-c("Statistics","")
    res
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
    res=as.data.frame(as.array(summary(var2_switch)))
    names(res)<-c("Statistics","")
    res
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
    res=as.data.frame(as.array(summary(var3_switch)))
    names(res)<-c("Statistics","")
    res
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
    res=as.data.frame(as.array(summary(var4_switch)))
    names(res)<-c("Statistics","")
    res
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
    pageLength = 4),{
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
    pageLength = 4),{
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
    paste('You have choosen to build ',input$choosek, " clusters")
  })
  
  #Table for choosen cluster KMEANS
  output$cluster_choose<-renderDataTable(options = list(
    pageLength = 6),{
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
    paste('You have choosen to build ',input$choosek, " clusters")
  })
  
  #Text for optimal number of k in CAH
  output$optk2<-renderText({
    paste('Optimal number of cluster is: ',nb_cluster_opt)
  })
  
  #oPTIMAL DENDOGRAM
  output$dendo1<-renderPlot({
    ggplot(color_branches(datascale.ag, k = nb_cluster_opt), labels = TRUE)
    fviz_dend(datascale.ag, k=nb_cluster_opt, show_labels = TRUE, rect = TRUE)
  })
  
  #Text for number of cluster select CAH
  output$text_choosek2<-renderText({
    paste("You have choosen to build ",input$choosek2," clusters")
  })
  
  
  #Dendogram for choosen cluster using CAH
  output$dendo2<-renderPlot({
    datascale.dist = dist(raw_data_scale,method="euclidean", diag=FALSE)
    datascale.ag = hclust(datascale.dist,method="ward.D")
    ggplot(color_branches(datascale.ag, k = input$choosek2), labels = TRUE) 
    fviz_dend(datascale.ag, k = input$choosek2, show_labels = TRUE, rect = TRUE)
  })
}


shinyApp(ui = ui, server = server)



