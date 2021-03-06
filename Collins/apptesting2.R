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
# library(data.table)
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
            
            # Copy the line below to make a set of radio buttons
            radioButtons("selSector", label = h3("Choose a Sector:"),
                         choices = list("Communication Services" = 1, "Consumer Discretionary" = 2, "Staples" = 3,
                                        "Energy" = 4, "Financials" = 5, "Health Care" = 6,
                                        "Industrials" = 7, "Information Technology" = 8, "Materials" = 9,
                                        "Real Estate" = 10, "Utilities" = 11, "All"=12), 
                         selected = 12),
            
            hr(),
            fluidRow(column(3, verbatimTextOutput("value")))
        ),
    
        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(
                tabPanel("Plots",
                         plotOutput("distPlot"),
                         plotOutput("elbowPlot")),
                tabPanel("Monthly Plots"
                         ,plotOutput("MonthlyPlots")),
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
    load('YearlyReturnsSDsp500-4year.RData')
    load('MonthlyReturnsSDsp500-4year.RData')
   
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
    
#Regular Clustering based on monthly end returns/volatility
    stockInfo = sp500Sectors %>% select(Symbol, sectorF)
    
    clustersbyMo = reactive({
        clustersbyMo =  dataMonthly %>%
            filter(YrMo >= format(as.Date(input$start_date, format="%Y-%m-%d"),"%Y%m")
                   & YrMo <= format(as.Date(input$end_date, format="%Y-%m-%d"),"%Y%m")) %>%
            select('symbol', 'YrMo', 'returns','sd')
        
        mls <- split(clustersbyMo, f=clustersbyMo$YrMo)
        myv <- c("returns","sd")
        
        kls <- lapply(X=mls, FUN=function(x){x$clust <- kmeans(x[, myv],
                                                               centers=input$num_clusters)$cluster ; return(x)})
        
        clustersbyMo <- do.call(rbind, kls)
        clustersbyMo <- clustersbyMo %>%
            left_join(stockInfo, ., by = c("Symbol" = "symbol")) %>% 
            filter( if (input$selSector  == "All") { sectorF == sectorF } else {  sectorF == input$selSector  }) %>%
            drop_na()
        
    })
    
    output$MonthlyPlots <- renderPlot({
        ggplot() +
            geom_point(data = clustersbyMo(), 
                       mapping = aes(x = clustersbyMo()$returns, 
                                     y = clustersbyMo()$sd, 
                                     colour = factor(clustersbyMo()$clust) )) +
            facet_wrap(~ clustersbyMo()$YrMo, scales = "free_y") +
            labs(title = 'Monthly Return and Volatility Clusters' ,
                 y = "Returns", 
                 x = "Volatility",
                 colour = "Clusters")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
