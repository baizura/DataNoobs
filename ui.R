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
                          tabPanel("Introduction",titlePanel("Open Sex Role Inventory Distribution (OSRI)"),
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
                          tabPanel("Participant",
                                   fluidRow(
                                     br(),
                                     box(width = 6,
                                     title = "OSRI Participant by Gender", background = "teal", solidHeader = TRUE,
                                     plotlyOutput("pieGender", height = 400)),
                                     box(width = 6,
                                         title = "OSRI Participant by Gender", background = "maroon", solidHeader = TRUE,
                                         plotlyOutput("radar", height = 400))
                                     
                                   )
                            
                                   ,  style='width: 1280px; height: 500px'),
                          tabPanel("Gender Traits",
                                   fluidRow(
                                     br(),
                                     box(width = 6,
                                       title = "Volcano: Gender by Country", background = "orange", solidHeader = TRUE,
                                       plotlyOutput("volcanoPlot", height = 400)),
                                     box(
                                       width = 6,
                                         title = "Feminine vs Masculine Traits",background = "olive", solidHeader = TRUE,
                                         plotlyOutput("twoDhist", height = 400)))
                                     
                                   ,  style='width: 1280px; height: 500px'),
                          tabPanel("Country",
                                   fluidRow(
                                     br(),
                                     box(width = 12,
                                         title = "Top 20 Country: Feminity vs Masculinity", background = "yellow", solidHeader = TRUE,
                                         plotlyOutput("bublePlot", height = 400)
                                         
                                     )
                                   )
                                   
                                   ,  style='width: 1280px; height: 500px'),
                          tabPanel("Race",
                                   fluidRow(
                                     br(),
                                     box(width = 12,
                                         title = "Boxplot Androgyny Score by Race", background = "yellow", solidHeader = TRUE,
                                         plotOutput("race", height = 400)
                                         
                                     ),
                                     fluidRow(
                                       p("Lagend:1: Mixed race, 2: Asian, 3: Black, 4: Native American, 5: Native Australian, 6: White, 7: Other 0: NA")
                                       
                                     )
                                     
                                   )
                                   
                                   ,  style='width: 1280px; height: 500px'),
                          
                          tabPanel("Androgyny Score",
                                   
                                    
                                   fluidRow(
                                     br(),
                                     box(width = 12,
                                         title = "Androgyny Result by Country",background = "yellow", solidHeader = TRUE,
                                         plotlyOutput("hist2D", height = 400)
                                   ))
                                   
                                   
                                   
                                   ,  style='width: 1280px; height: 800px'),
                          
                          tabPanel("Documentation", 
                                   
                                   fluidRow(
                                     h5("Introduction Tab"),
                                     p("1.	The dashboard shows the summary of the dataset that consists of total participants, total variables, total country and average elapse time."),
                                     p("2.	The map below shows the distributions of all the participants who took the survey/test."),
                                     p("3.	Place the cursor at any part of the map to know the details on the test for country of interest.  "),
                                     p("4.	The colour on the map represent the participant density. The darker the colour, the higher number of participants. "),
                                     p("5.	Black colour represents no participation from that country. User can enlarge the scale of the map by clicking the + and - button on the left side of the map."),
                                     br(),
                                     h5("Participant Tab"),
                                     p(".	Use can view the participant by gender. Blue is male ,Orange is Female, green is no data / NA"),
                                     br(),
                                     h5("Gender Traits tab"),
                                     p(".	The Volcano chart will tell the gender distribution by country.User can move the cursor around the chart and can view different dimension of chart."),
                                     p(".	Feminine Vs Masculine Traits, user can experience the 2D histogram plot by enlarging the area of interest. The colour scale represents the count of participants and their test score.  "),
                                     br(),
                                     h5("Country Tab"),
                                     p(".	The bubble chart shows the TOP 20 country according to their masculinity and feminity. "),
                                     br(),
                                     h5("Race Tab"),
                                     p(".	The races are rate as:1=Mixed race, 2=Asian, 3=Black, 4=Native American, 5=Native Australian, 6=White, 7=Other 8=NA"),
                                     p(".	Each boxplot shows the distribution of the race according their test score. ")
                                     
                                   ),
                                   
                                   fluidRow(
                                     
                                   )
                                   
                                   ,  style='width: 1280px; height: 1400px')
                        )
                        
                      )
                      )
                      )
