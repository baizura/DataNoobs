library(dplyr)
library(ggplot2)
library(rworldmap)
library(viridis)
library(leaflet)
library(r2d3) #d3 visualization
library(plotly)


server <- function(input, output) {
  
  
  #============================
  #     ValueBox
  #============================
  output$TP <- renderText({  
    ## Total Participant (TP)
    TP <- read.csv("./Data/summaryData.csv")
    TP <- TP %>% filter(AttrVar=="TP") %>% select(TotalVal)
    
    
    ## convert to integer and format it to have thousand seperator
    TP <- formatC(as.integer(TP), format="f", big.mark=",", digits=0)
  })
  
  output$TV <- renderText({    
    ## Total variables (TV)
    TV <- read.csv("./Data/summaryData.csv")
    TV <- TV %>% filter(AttrVar=="TV") %>% select(TotalVal)
    ## convert to integer and format it to have thousand seperator
    TV <- formatC(as.integer(TV), format="f", big.mark=",", digits=0)
  })
  
  
  output$TC <- renderText({  
    ## Total Country (TC)
    TC <- read.csv("./Data/summaryData.csv")
    TC <- TC %>% filter(AttrVar=="TC") %>% select(TotalVal)
    ## convert to integer and format it to have thousand seperator
    TC <- formatC(as.integer(TC), format="f", big.mark=",", digits=0)
  })
  
  
  output$ATE <- renderText({  
    ## Average time elapse(ATE) in seconds
    ATE <- read.csv("./Data/summaryData.csv")
    ATE <- ATE %>% filter(AttrVar=="ATE") %>% select(TotalVal)
    
    ## convert to integer and format it to have thousand seperator
    ATE <- formatC(as.integer(ATE), format="f", big.mark=",", digits=2)
  })
  #=============================
  
  output$CPDMap <- renderLeaflet({
    
    CPD <- read.csv("./Data/countryData.csv")
    
    #joinCountryData2Map to plot the country base on joinCode
    SPDF <- joinCountryData2Map( CPD
                                 ,joinCode = "ISO2"
                                 ,nameJoinColumn = "Country")
    
    # spdf is a sp::SpatialPolygonsDataFrame
    qpal <- colorNumeric(rev(viridis::plasma(15)),
                         SPDF$Total, n=15)
    
    CPDMap <- leaflet(SPDF, options =
                        leafletOptions(attributionControl = FALSE, minzoom=1.5)) %>%
      addPolygons(
        label=~stringr::str_c(
          NAME, ' ',
          formatC(Total, big.mark = ',', format='d')),
        labelOptions= labelOptions(direction = 'auto'),
        weight=1,color='#333333', opacity=1,
        fillColor = ~qpal(Total), fillOpacity = 1,
        highlightOptions = highlightOptions(
          color='#000000', weight = 2,
          bringToFront = TRUE, sendToBack = TRUE)
      ) %>%
      addLegend(
        "topright", pal = qpal, values = ~Total,
        title = htmltools::HTML("Participant Density"),
        opacity = 1 )
    
  })
  
  #==========================

  output$osriResult <- renderPlot({
    OSRI_summary <- read.csv("./Data/OSRI_summary.csv")
    plot(OSRI_summary[c(3,2,1)],
         pch = 19,
         cex = .7,
         bg = par("bg"),
         col = c("purple","green", "tomato"))
    
  })
  
  output$mascAndro <- renderPlot({
    OSRI_summary <- read.csv("./Data/OSRI_summary.csv")
        plot(OSRI_summary[c(2,1)],  # x = Androgyny y = Masculine
         pch = c(19, 17),
         cex = .7,
         col = c("purple","green"))
    
  })
  output$femAndro <- renderPlot({
    OSRI_summary <- read.csv("./Data/OSRI_summary.csv")
    plot(OSRI_summary[-1], # x = Androgyny y = Feminine
         pch = c(19, 17),
         cex = .7,
         col = c("purple","green"))
    
    
    
  })
  
  output$femMasc <- renderPlot({
    OSRI_summary <- read.csv("./Data/OSRI_summary.csv")
    plot(OSRI_summary[c(1,3)],
         type = 'p',
         pch = 19,
         cex = .7,
         col = c("purple", "green"))
    
  })
  
  
  output$volcanoPlot <- renderPlotly({
    OSRI_summary <- read.csv("./Data/OSRI_summary.csv")
    osriDF_new <- read.csv("./Data/osriDF_new.csv")
    plot_ly (osriDF_new,
             type = 'surface',
             z = ~volcano )
    
    v = plot_ly (OSRI_summary,
                 type = 'surface',
                 z = ~volcano ) %>% 
      layout(
             xaxis = list(showgrid = FALSE),
             yaxis = list(showgrid = FALSE))
    
    
  })
  
  output$bublePlot <- renderPlotly({
    by_country <- read.csv("./Data/by_country.csv")
    library(plotly)
    
    vline <- function(x = 0, color = "black") {
      list(
        type = "line", 
        y0 = 0, 
        y1 = 1, 
        yref = "paper",
        x0 = x, 
        x1 = x, 
        line = list(color = color)
      )
    }
    
    hline <- function(y = 0, color = "black") {
      list(
        type = "line", 
        x0 = 0, 
        x1 = 1, 
        xref = "paper",
        y0 = y, 
        y1 = y, 
        line = list(color = color)
      )
    }
    
    # top 20 countries
    plot_ly(by_country, x = ~mean_masc, y = ~mean_fem, text = ~IP, type = 'scatter', 
                      mode = 'markers', size = ~count, color = ~IP, colors = 'Paired',
                      #Choosing the range of the bubbles' sizes:
                      sizes = c(30, 90),
                      marker = list(opacity = 0.5, sizemode = 'diameter')) %>%
      layout(xaxis = list(showgrid = T),
             yaxis = list(showgrid = T),
             shapes = list(vline(3), hline(3)),
             showlegend = T)
  })
  
  
  output$twoDhist <- renderPlotly({
    result <- read.csv("./Data/result.csv")
    OSRI_summary <- read.csv("./Data/OSRI_summary.csv")
    count <- with(result, table(Fem_Score, Masc_Score))
    plot_ly(OSRI_summary, x = ~Masculine, y = ~Feminine, z = ~count) %>%
      layout(xaxis = list(showgrid = FALSE),
             yaxis = list(showgrid = FALSE)) %>% 
      add_histogram2d()
    
    
    
  })
}