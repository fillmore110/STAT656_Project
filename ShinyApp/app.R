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
                tabPanel("Time Series Plots",
                   plotOutput("distPlot"),
                   plotOutput("elbowPlot")),
                tabPanel("Time Series Cluster Table", DT::dataTableOutput("dis")),
                tabPanel("Volatility Time Series Plots",
                         plotOutput("volDistPlot"),
                         plotOutput("volElbowPlot")),
                tabPanel("Volatility Time Series Cluster Table", DT::dataTableOutput("volDis")),
                tabPanel("Returns vs. Volatilty Confusion", plotOutput('confMatrix')),
                tabPanel("Month-End Plots"
                         ,plotOutput("MonthlyPlots")),
                tabPanel("Month-End Table", DT::dataTableOutput("MoTbl")),
                tabPanel("Year-End Plots"
                         ,plotOutput("YearlyPlots")),
                tabPanel("Year-End Table", DT::dataTableOutput("YrTbl"))
            )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    ##load('D:\\James\\Documents\\tamu\\STAT656\\Homework\\project\\code\\STAT656_Project\\ShinyApp\\sp500-4year.RData')
    ##sp500Sectors = read.csv("D:\\James\\Documents\\tamu\\STAT656\\Homework\\project\\code\\STAT656_Project\\Li\\constituents_csv.csv") %>% 
        
        
    load('sp500-4year.RData')
    load('sp500-4year-vol.RData')
    load('MonthlyReturnsSDsp500-4year.RData')
    load('YearlyReturnsSDsp500-4year.RData')
    
    sp500Sectors = read.csv("constituents_csv.csv") %>% 
        mutate(sectorF = as.factor(Sector)) %>%
        mutate(sectorNumeric = as.numeric(sectorF))
    
    dates = as.Date(colnames(returns)[-1], format = "%Y-%m-%d") #Get rid of first index which is the stock
    
    returns_vol = returns_vol %>%
        add_column(date=dates) %>%
        pivot_longer(cols=!date) %>%
        pivot_wider(names_from = date, values_from = value) %>%
        rename(symbol = name)
    
    stock_names = returns %>% select(1)
    clusters = reactive({
        returns_subset =  returns %>%
            select((min(which(dates > input$start_date)) + 1):(max(which(dates < input$end_date) + 1)))
        
        
        returns_subset = bind_cols(stock_names, returns_subset)
        
        clusters = tsclust(returns_subset %>% select(where(is.numeric)), k=input$num_clusters, distance="dtw_basic")
    })
    
    vol_clusters = reactive({
        returns_vol_subset =  returns_vol %>%
            select((min(which(dates > input$start_date)) + 1):(max(which(dates < input$end_date) + 1)))
        
        
        returns_vol_subset = bind_cols(stock_names, returns_vol_subset)
        
        vol_clusters = tsclust(returns_vol_subset %>% select(where(is.numeric)), k=input$num_clusters, distance="dtw_basic")
    })
    
    output$distPlot <- renderPlot({

        plot(clusters())
        
    })
    
    output$volDistPlot <- renderPlot({
        
        plot(vol_clusters())
        
    })
    
    output$elbowPlot <- renderPlot({
        clust = clusters()
        plotW = rep(0,9)
        for(K in 2:10){
            plotW[K-1] = kmeans(clust@distmat,centers=K,nstart=25)$tot.withinss
        }
        plot(2:10, plotW, xlab="numClusters", title="Elbow Plot")
    })
    
    output$volElbowPlot <- renderPlot({
        vol_clust = vol_clusters()
        plotW = rep(0,9)
        for(K in 2:10){
            plotW[K-1] = kmeans(vol_clust@distmat,centers=K,nstart=25)$tot.withinss
        }
        plot(2:10, plotW, xlab="numClusters", title="Volatility Elbow Plot")
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
    
    output$volDis <-DT::renderDataTable(DT::datatable({
        vol_clust = vol_clusters()
        df = data.frame(stock = character(),
                        cluster = factor(),
                        sector = character())
        
        for (i in 1:length(vol_clust@cluster)) {
            stock = returns$symbol[i]
            row = c(stock, vol_clust@cluster[i], as.character(sp500Sectors[sp500Sectors$Symbol == stock,"sectorF"]))
            df = rbind(df, row)
        }
        colnames(df) = c("stock", "cluster", "sectorName")
        return(df)
    }))
    
    output$confMatrix <- renderPlot({
        clust = clusters()
        vol_clust = vol_clusters()
        
        clust = fct_infreq(as.factor(clust@cluster))
        vol_clust = fct_infreq(as.factor(vol_clust@cluster))
        
        levels(clust) = 1:length(levels(clust))
        levels(vol_clust) = 1:length(levels(vol_clust))
        
        confusion_matrix = as.data.frame(table(clust, vol_clust))
        
        ggplot(data = confusion_matrix,
               mapping = aes(x = clust,
                             y = vol_clust)) +
            geom_tile(aes(fill = Freq)) +
            geom_text(aes(label = sprintf("%1.0f", Freq)), vjust = 1) +
            scale_fill_gradient(low = "blue",
                                high = "red",
                                trans = "log")
        
    })
    
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
        
        
    })
    output$MonthlyPlots <- renderPlot({
        ggplot() +
            geom_point(data = clustersbyMo(), 
                       mapping = aes(x = clustersbyMo()$returns, 
                                     y = clustersbyMo()$sd, 
                                     colour = factor(clustersbyMo()$clust) )) +
            facet_wrap(~ clustersbyMo()$YrMo, scales = "free_y") +
            labs(title = 'Month-End Return and Volatility Clusters using K-Means' ,
                 y = "Returns", 
                 x = "Volatility",
                 colour = "Clusters")
    })
    clustersbyMoSector = reactive({
        clustersbyMoSector =  dataMonthly %>%
            filter(YrMo >= format(as.Date(input$start_date, format="%Y-%m-%d"),"%Y%m")
                   & YrMo <= format(as.Date(input$end_date, format="%Y-%m-%d"),"%Y%m")) %>%
            select('symbol', 'YrMo', 'returns','sd')
        
        mls <- split(clustersbyMoSector, f=clustersbyMoSector$YrMo)
        myv <- c("returns","sd")
        
        kls <- lapply(X=mls, FUN=function(x){x$clust <- kmeans(x[, myv],
                                                               centers=input$num_clusters)$cluster ; return(x)})
        
        clustersbyMoSector <- do.call(rbind, kls)
        clustersbyMoSector <- left_join(stockInfo, clustersbyMoSector, by = c("Symbol" = "symbol")) %>%
            drop_na()  %>%
            mutate(returns = round(returns, 2), stDev = round(sd, 4)) %>%
            rename(sectorName = sectorF, cluster = clust) %>%
            select(-sd) %>% relocate(stDev, .after = returns)
        
    })
    
    output$MoTbl  = DT::renderDataTable({
        clustersbyMoSector()
    })
    
#Regular Clustering based on Year end returns/volatility
    clustersbyYr = reactive({
        clustersbyYr =  dataYearly
        
        mls <- split(clustersbyYr, f=clustersbyYr$year)
        myv <- c("returns","sd")
        
        kls <- lapply(X=mls, FUN=function(x){x$clust <- kmeans(x[, myv], centers=input$num_clusters)$cluster ; return(x)})
        
        clustersbyYr <- do.call(rbind, kls)
        
        
    })
    
    output$YearlyPlots <- renderPlot({
        ggplot() +
            geom_point(data = clustersbyYr(), 
                       mapping = aes(x = clustersbyYr()$returns, 
                                     y = clustersbyYr()$sd, 
                                     colour = factor(clustersbyYr()$clust) )) +
            facet_wrap(~ clustersbyYr()$year, scales = "free_y") +
            labs(title = 'Year-End Return and Volatility Clusters using K-Means' ,
                subtitle = '01-2017 through 02-2021 Aggregated by Year',
                 y = "Returns", 
                 x = "Volatility",
                 colour = "Clusters")
    })

    clustersbyYrSector  = reactive({
        clustersbyYrSector =  dataYearly
        
        mls <- split(clustersbyYrSector, f=clustersbyYrSector$year)
        myv <- c("returns","sd")
        
        kls <- lapply(X=mls, FUN=function(x){x$clust <- kmeans(x[, myv],
                                                               centers=input$num_clusters)$cluster ; return(x)})
        
        clustersbyYrSector <- do.call(rbind, kls)
        clustersbyYrSector <- left_join(stockInfo, clustersbyYrSector, by = c("Symbol" = "symbol")) %>%
            drop_na()  %>%
            mutate(returns = round(returns, 2), stDev = round(sd, 4)) %>%
            rename(sectorName = sectorF, cluster = clust) %>%
            select(-sd) %>% relocate(stDev, .after = returns)
    })
    
    
    output$YrTbl  = DT::renderDataTable({
        clustersbyYrSector()
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
