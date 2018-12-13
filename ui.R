## ui.R ##
##install.packages("shinydashboard")
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(rworldmap)
library(viridis)
library(leaflet)
library(r2d3) #d3 visualization
dashboardPage(
  dashboardHeader(),
  dashboardSidebar(),
  dashboardBody()
)

## app.R ##
library(shinydashboard)

ui <- dashboardPage(skin = "yellow",
                    dashboardHeader(title = "Data Noobs"),
                    dashboardSidebar(disable = TRUE
                                     
                                     
                    ),
                    dashboardBody(
                      
                      tags$head(tags$style(HTML('
                                                /* body */
                                                .content-wrapper, .right-side {
                                                background-color: #FFFFFF;
                                                }
                                                
                                                .small-box {height: 90px}
                                                
                                                
                                                '))),
                      mainPanel(
                        
                        tabsetPanel(
                          tabPanel("Summary",titlePanel("Open Sex Role Inventory Distribution (OSRI)"),
                                   fluidRow(
                                     # A static valueBox
                                     valueBox(textOutput("TP"), "Total Participants",width = 3, color = "aqua",icon = icon("users")),
                                     valueBox(textOutput("TV"), "Total Variables (44 Questions)",width = 3, color = "green",icon = icon("table")),
                                     valueBox(textOutput("TC"), "Total Country",width = 3, color = "fuchsia", icon=icon("globe-americas")),
                                     valueBox(textOutput("ATE"), "Average Time Elapse (in seconds)",width = 3, color = "lime", icon=icon("clock"))
                                     
                                   ),
                                   fluidRow(
                                     box(width = 12,leafletOutput("CPDMap", height = 350))
                                   )
                                   ,  style='width: 1280px; height: 500px'),
                          
                          tabPanel("Analysis",
                                   
                                   fluidRow(
                                     inputPanel(
                                       sliderInput("bar_max", label = "Max:",
                                                   min = 10, max = 110, value = 10, step = 20)),
                                     d3Output("d3")
                                   ),
                                   
                                   fluidRow(
                                     
                                   )
                                   
                                   ,  style='width: 1280px; height: 500px'),
                          
                          tabPanel("Documentation", titlePanel("User Manual"),
                                   
                                   fluidRow(
                                     
                                   ),
                                   
                                   fluidRow(
                                     
                                   )
                                   
                                   ,  style='width: 1280px; height: 500px')
                        )
                        
                      )
                      )
                      )
