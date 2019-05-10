library(shiny)
library(shinydashboard)
library(plotly)
library(readr)

sidebar <- dashboardSidebar(
  hr(),
  sidebarMenu(id="tabs",
              menuItem("IMDB", tabName = "IMDB", icon=icon("images")),
              menuItem("About", tabName = "About", icon = icon("laugh-squint"))
  )
)

body <- dashboardBody(
  tabItems(
    tabItem(
      tabName = "IMDB",
      fluidRow(
        column(width = 4,
               box( title ="Top 10 Genres", width = NULL,
                    background = "navy", solidHeader = TRUE,
                    plotlyOutput("genre")
                 )
               ),
        column(width = 4,
               box( title ="Share of Movies Making Profit or Loss", width = NULL,
                    background = "navy", solidHeader = TRUE,
                    plotlyOutput("profit")
                  )
               ),
        column(width = 4,
               box( title ="Distribution of IMDB score", width = NULL,
                    background = "navy", solidHeader = TRUE,
                    plotlyOutput("score")
                  )
               )
      ),
      fluidRow(
        column(width = 8,
               box( title ="Distribution of Budget and Revenue", width = NULL,
                    background = "navy", solidHeader = TRUE,
                    plotlyOutput("money")
               )
               ),
        column(width = 4,
               box( title ="Scores Before and After Year 2000", width = NULL,
                    background = "navy", solidHeader = TRUE,
                    plotlyOutput("year")
               ))
        
        
      )
      
    ),
    
  tabItem(tabName = "About",
          includeMarkdown("About.Rmd")
  )
)
)

ui <- dashboardPage(
  dashboardHeader(title = "Learn More About Movies: IMDB Data Analysis", titleWidth = 500),
  sidebar,
  body
)