library(shiny)
library(rgdal)
library(rgeos)
library(tmap)
library(leaflet)
library(spdep)

# TO DO BEFORE FINAL VERSION: 
### Enable state-specific plotting to improve render time
### Enable interactive LISA visualization in tab 



ui <- fluidPage(
  
  titlePanel("Exploring 2016 Election Data"),

    sidebarPanel(
      h3("What variable would you like to display?"),
      selectInput("var", 
                  label = "Choose the variable that will shade in the map",
                  choices = list("% of Vote Share for Hillary",
                                 "% Speaking Foreign Language", 
                                 "% Black",
                                 "% Hispanic"),
                                 
                  selected = "% of Vote Share for Hillary"),
      
      selectInput("state",
              label = "Select the abbreviation for the state you wish to map:",
              choices = list("AL", "AZ", "AR", "CA", "CO", "CT", "DE", "DC", "FL", "GA", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME",
                               "MD", "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR",
                               "PA", "RI", "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY"),
              selected = "AL")
    
    ),
    
    
    mainPanel(
        
        navbarPage("Election Data",
                   tabPanel("Choropleth Map", leafletOutput("demo_map")),
                   tabPanel("LISA", plotOutput("lisa"))
        
    ),
    
    h5("Note: LISA Functionality Not Yet Enabled; First plot takes ~10 seconds to load")
  )
  
)


server <- function(input, output) {
  merged.data <- readOGR("merged_demo_elect_data")
  

  output$demo_map <- renderLeaflet({
  
      plotvar <- switch(input$var, 
                     "% of Vote Share for Hillary" = "pct_hll",
                     "% Speaking Foreign Language" = "POP8152",
                     "% Black" = "RHI2252",
                     "% Hispanic" = "RHI7252")
      
      state <- input$state
      state.data <- merged.data[merged.data$stt_bbr == state,]
      
      working_map <- tm_shape(state.data) + tm_fill(plotvar, style="jenks", title = input$var) 
      tmap_leaflet(working_map)
     
      
      
      
      
    })
  
  output$lisa <- renderPlot({
    
    #### FILL IN FOR FINAL PROJECT
  })
  
}



shinyApp(ui = ui, server = server)
