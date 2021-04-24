#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(caret)
library(tidyverse)
library(tidyquant)
library(dtwclust)
# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Clustering Stocks"),


    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            dateInput("start_date", "Start Date:", value = "2020-01-03'"),
            dateInput("end_date", "End Date:", value = "2020-04-01"),
            
            numericInput("num_clusters", "Number of Clusters:", 3, min = 2, max = 20),
        ),

        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(
                tabPanel("Plots",
                   plotOutput("distPlot"),
                   plotOutput("elbowPlot")),
                tabPanel("Table", DT::dataTableOutput("dis"))
            )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    ##load('D:\\James\\Documents\\tamu\\STAT656\\Homework\\project\\code\\STAT656_Project\\ShinyApp\\sp500-4year.RData')
    ##sp500Sectors = read.csv("D:\\James\\Documents\\tamu\\STAT656\\Homework\\project\\code\\STAT656_Project\\Li\\constituents_csv.csv") %>% 
        
        
    load('sp500-4year.RData')
    sp500Sectors = read.csv("constituents_csv.csv") %>% 
        mutate(sectorF = as.factor(Sector)) %>%
        mutate(sectorNumeric = as.numeric(sectorF))
    
    dates = as.Date(colnames(returns)[-1], format = "%Y-%m-%d") #Get rid of first index which is the stock
    
    stock_names = returns %>% select(1)
    clusters = reactive({
        returns_subset =  returns %>%
            select((min(which(dates > input$start_date)) + 1):(max(which(dates < input$end_date) + 1)))
        
        
        returns_subset = bind_cols(stock_names, returns_subset)
        
        clusters = tsclust(returns_subset %>% select(where(is.numeric)), k=input$num_clusters, distance="dtw_basic")
    })
    
    output$distPlot <- renderPlot({

        plot(clusters())
        
    })
    
    output$elbowPlot <- renderPlot({
        clust = clusters()
        plotW = rep(0,9)
        for(K in 2:10){
            plotW[K-1] = kmeans(clust@distmat,centers=K,nstart=25)$tot.withinss
        }
        plot(2:10, plotW, xlab="numClusters", title="Elbow Plot")
    })
    
    output$dis <-DT::renderDataTable(DT::datatable({
        clust = clusters()
        df = data.frame(stock = character(),
                        cluster = factor(),
                        sector = character())
        
        for (i in 1:length(clust@cluster)) {
            stock = returns$symbol[i]
            row = c(stock, clust@cluster[i], as.character(sp500Sectors[sp500Sectors$Symbol == stock,"sectorF"]))
            df = rbind(df, row)
        }
        colnames(df) = c("stock", "cluster", "sectorName")
        return(df)
    }))
}

# Run the application 
shinyApp(ui = ui, server = server)
