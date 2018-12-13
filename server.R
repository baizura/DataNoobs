library(dplyr)
library(ggplot2)
library(rworldmap)
library(viridis)
library(leaflet)
library(r2d3) #d3 visualization


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
  
  output$d3 <- renderD3({
    r2d3(
      CPD <- read.csv("./Data/genderData.csv")
    )
  })
  
  
}