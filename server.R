library(dplyr)
library(ggplot2)
library(rworldmap)
library(viridis)
library(leaflet)
library(r2d3) #d3 visualization
library(plotly)


server <- function(input, output) {
  
  observe({
    intro <- read.csv("./Data/summaryData.csv")
    CPD <- read.csv("./Data/countryData.csv")
    by_gender <- read.csv("./Data/genderData.csv")
    OSRI_summary <- read.csv("./Data/OSRI_summary.csv")
    osriDF_new <- read.csv("./Data/osriDF_new.csv")
    by_country <- read.csv("./Data/by_country.csv")
    result <- read.csv("./Data/result.csv")
    
    #============================
    #     ValueBox
    #============================
    output$TP <- renderText({  
      ## Total Participant (TP)
      TP <- intro %>% filter(AttrVar=="TP") %>% select(TotalVal)
      
      ## convert to integer and format it to have thousand separator
      TP <- formatC(as.integer(TP), format="f", big.mark=",", digits=0)
    })
    
    output$TV <- renderText({    
      ## Total variables (TV)
      TV <- intro %>% filter(AttrVar=="TV") %>% select(TotalVal)
      
      ## convert to integer and format it to have thousand separator
      TV <- formatC(as.integer(TV), format="f", big.mark=",", digits=0)
    })
    
    output$TC <- renderText({  
      ## Total Country (TC)
      TC <- intro %>% filter(AttrVar=="TC") %>% select(TotalVal)
      
      ## convert to integer and format it to have thousand separator
      TC <- formatC(as.integer(TC), format="f", big.mark=",", digits=0)
    })
    
    
    output$ATE <- renderText({  
      ## Average time elapse(ATE) in seconds
      ATE <- intro %>% filter(AttrVar=="ATE") %>% select(TotalVal)
      
      ## convert to integer and format it to have thousand seperator
      ATE <- formatC(as.integer(ATE), format="f", big.mark=",", digits=2)
    })
    
    #=============================
    
    output$CPDMap <- renderLeaflet({
      
      #joinCountryData2Map to plot the country base on joinCode
      SPDF <- joinCountryData2Map( CPD
                                   ,joinCode = "ISO2"
                                   ,nameJoinColumn = "Country")
      
      # spdf is a sp::SpatialPolygonsDataFrame
      qpal <- colorNumeric(rev(viridis::plasma(15)),
                           SPDF$Total, n=15)
      
      CPDMap <- leaflet(SPDF, options = leafletOptions(attributionControl = FALSE, minzoom=1.5)) %>%
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
          title = htmltools::HTML("Total Participant"),
          opacity = 1 )
      
    })
    
    #==========================
    
    output$osriResult <- renderPlot({
      plot(OSRI_summary[c(3,2,1)],
           pch = 19,
           cex = .7,
           bg = par("bg"),
           col = c("purple","green", "tomato"))
      
    })
    
    output$mascAndro <- renderPlot({
      plot(OSRI_summary[c(2,1)],  # x = Androgyny y = Masculine
           pch = c(19, 17),
           cex = .7,
           col = c("purple","green"))
      
    })
    output$femAndro <- renderPlot({
      plot(OSRI_summary[-1], # x = Androgyny y = Feminine
           pch = c(19, 17),
           cex = .7,
           col = c("purple","green"))
      
      
      
    })
    
    output$femMasc <- renderPlot({
      plot(OSRI_summary[c(1,3)],
           type = 'p',
           pch = 19,
           cex = .7,
           col = c("purple", "green"))
      
    })
    
    
    output$volcanoPlot <- renderPlotly({
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
      count <- with(result, table(Fem_Score, Masc_Score))
      plot_ly(OSRI_summary, x = ~Masculine, y = ~Feminine, z = ~count) %>%
        layout(xaxis = list(showgrid = FALSE),
               yaxis = list(showgrid = FALSE)) %>% 
        add_histogram2d()
      
    })
    
    output$pieGender <- renderPlotly({
      
      by_gender %>% 
        plot_ly(labels = ~Gender, values = ~volume) %>%
        add_pie(hole = 0.6) %>%
        layout(showlegend = F,
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               showlegend = T) 
    })
    
   output$race <- renderPlot({

     # Androgyny Score by Race
     
     osriDF_new$race = factor(osriDF_new$race)
     ggplot(osriDF_new, aes(x = race, y = Androgyny_Score)) + 
       geom_boxplot() 
   })
   
   output$radar <- renderPlotly({
     rc <- plot_ly(
       type = 'scatterpolar',
       r = c(91668, 97791, 20159),
       theta = c('Male', 'Female', 'Other'),
       fill = 'toself',
       name = 'Gender'
     ) %>%
       layout(
         polar = list(
           radialaxis = list(
             visible = T,
             range = c(0,120000)
           )
         ),
         showlegend = T
       )
     })
   
   output$hist2D <- renderPlotly({
     plot_ly(by_country, x = ~mean_masc, y = ~mean_fem, z = ~mean_andro, 
             color = ~IP, colors = 'Paired',
             sizes = c(5, 150),
             marker = list(symbol = 'circle', opacity = 0.5, sizemode = 'diameter')) %>%
       add_markers() %>%
       layout(scene = list(xaxis = list(title = 'Masculinity'),
                           yaxis = list(title = 'Feminity'),
                           zaxis = list(title = 'Androgyny Score'),
                           title = 'Androgyny Result by Country'))
     
   })
    
  })
}