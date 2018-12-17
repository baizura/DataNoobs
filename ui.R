## ui.R ##
##install.packages("shinydashboard")
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(rworldmap)
library(viridis)
library(leaflet)
library(r2d3) #d3 visualization
library(plotly)
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
                                     box(width = 6,
                                         title = "Top 20 Country: Feminity vs Masculinity", background = "yellow", solidHeader = TRUE,
                                         plotlyOutput("bublePlot", height = 350)
                                       
                                     ),
                                     box(
                                       width = 6,
                                       title = "Volcano: Gender by Country", background = "orange", solidHeader = TRUE,
                                       plotlyOutput("volcanoPlot", height = 350)
                                     )
                                   ),
                                   
                                   fluidRow(
                                     box(width = 3,
                                         title = "Masculine vs Androgyn",background = "blue", solidHeader = TRUE,
                                         plotOutput("mascAndro")),
                                     box(width = 3,
                                         title = "Feminine vs Androgyny",background = "yellow", solidHeader = TRUE,
                                         plotOutput("femAndro")),
                                     box(width = 3,
                                         title = "Feminine vs Masculine",background = "teal", solidHeader = TRUE,
                                         plotOutput("femMasc")),
                                     box(width = 3,
                                         title = "Feminine vs Masculine Traits",background = "olive", solidHeader = TRUE,
                                         plotlyOutput("twoDhist"))
                                     
                                   ),
                                   fluidRow(
                                     
                                     box(width = 12,
                                         title = "OSRI Result", background = "maroon", solidHeader = TRUE,
                                         plotOutput("osriResult", height = 500)
                                     )
                                     
                                   )
                                   
                                   ,  style='width: 1280px; height: 500px'),
                          
                          tabPanel("Documentation", titlePanel("User Manual"),
                                   
                                   fluidRow(
                                     h4("Summary Dashboard"),
                                     br(),
                                     p("1.	The dashboard shows the summary of the dataset that consists of total participants, total variables, total country and average elapse time."),
                                     p("2.	The map below shows the distributions of all the participants who took the survey/test."),
                                     p("3.	Place the cursor at any part of the map to know the details on the test for country of interest."), 
                                     p("4.	The colour on the map represent the participant density. The darker the colour, the higher number of participants."), 
                                     p("5.	Black colour represents no participation from that country. "),
                                     p("6.	User can enlarge the scale of the map by clicking the + and - button on the left side of the map.")
                                     
                                     
                                     
                                   ),
                                   
                                   fluidRow(
                                     
                                   )
                                   
                                   ,  style='width: 1280px; height: 500px')
                        )
                        
                      )
                      )
                      )
