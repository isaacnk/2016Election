library(shiny)
library(shinydashboard)
library(rgdal)
library(rgeos)
library(tmap)
library(leaflet)
library(spdep)

merged.data <- readOGR("merged_demo_elect_data")

ui <- dashboardPage(
  dashboardHeader(title = "2016 Election"),
  
  dashboardSidebar(sidebarMenu(
    menuItem("Home", tabName = "home", icon = icon("home")),
    menuItem("About", tabName = "about", icon = icon("info")),
    menuItem("Statistics", tabName = "stats", icon = icon("calculator")),
    menuItem("Tutorials", tabName = "tutorials", icon = icon("map")),
    menuItem("Downloads", tabName = "downloads", icon = icon("download"))
  )),
  
  
  dashboardBody(
    tabItems(
      
      #HOME PAGE LAYOUT
      tabItem(tabName = "home",
              fluidRow(
                box(
                  width = 4,
                  h3("What variable would you like to display?"),
                  br(), 
                  selectInput("homevar", 
                              label = "Choose the variable that will shade in the top map",
                              choices = list("% of Vote Share for Hillary",
                                             "% of Vote Share for Trump",
                                             "% Population Change 2010-14",
                                             "% Under 18",
                                             "% Over 65",
                                             "% Female",
                                             "% White", 
                                             "% Black",
                                             "% Hispanic",
                                             "% Foreign Language at Home",
                                             "% HS Graduate", 
                                             "% Bachelor's Degree", 
                                             "Mean Travel Time to Work",
                                             "% Homeowner",
                                             "% in Multiunit Housing",
                                             "Persons per Household",
                                             "Median Household Income",
                                             "Persons Below Poverty Line"),
                              
                              selected = "% of Vote Share for Hillary"),
                  br(),
                  br(),
                  selectInput("homevar2", 
                              label = "Choose the variable that will shade in the bottom map",
                              choices = list("% of Vote Share for Hillary",
                                             "% of Vote Share for Trump",
                                             "% Population Change 2010-14",
                                             "% Under 18",
                                             "% Over 65",
                                             "% Female",
                                             "% White", 
                                             "% Black",
                                             "% Hispanic",
                                             "% Foreign Language at Home",
                                             "% HS Graduate", 
                                             "% Bachelor's Degree", 
                                             "Mean Travel Time to Work",
                                             "% Homeowner",
                                             "% in Multiunit Housing",
                                             "Persons per Household",
                                             "Median Household Income",
                                             "Persons Below Poverty Line"),
                              
                              selected = "% Over 65"),
                  br(),
                  br(),
                  selectInput("homestate",
                              label = "Select the abbreviation for the state you wish to map:",
                              choices = list("AL", "AZ", "AR", "CA", "CO", "CT", "DE", "DC", "FL", "GA", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME",
                                             "MD", "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR",
                                             "PA", "RI", "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY"),
                              selected = "AL")
                ),
                box(
                  width = 8, 
                  leafletOutput("homemap", height = 300),
                  leafletOutput("homemap2", height = 300)
                ),
                box(width = 12,
                    textOutput(outputId = "cortext")
              )
      )),
      
      #ABOUT PAGE LAYOUT
      tabItem(tabName = "about",
              h1("About"),
              fluidRow(
                box(width = 12,
                    h2("Overview"),
                    p("This is an R Shiny Application by Isaac Kamber and Lorenz Menendez meant to offer 
                      you the chance to explore 2016 Election Data and related demographic information. Below, 
                      you can visualize county-level results & demographics by state. An in-depth discussion of the project
                      can be found in the 'About' tab. A statistical analysis concerning demographics and election results can
                      be found in the 'Statistics' tab. All data preparation and statistical analysis for this project was done in R
                      and is linked as Jupyter Notebook files in the 'Tutorials' tab. All data used is available for download in the 
                      'Downloads' tab,")),
                    br(),
                box(width = 12,
                    h2("Data Dictionary"),
                    p("INSERT DATA DICTIONARY STUFF")
                ), 
                br(),
                box(width = 12,
                    h2("Sources"),
                    p("MIT Election Data and Science Lab, 2018, 'County Presidential Election Returns 2000-2016', https://doi.org/10.7910/DVN/VOQCHQ, Harvard Dataverse, V1, UNF:6:ZaxsDvp6RsFitno8ZYlK7w== [fileUNF]"),
                    p("Hammer, Ben (2016). 2016 US Election (Version 8) [shapefile]. Retrieved from https://www.kaggle.com/benhamner/2016-us-election/activity")
                    ))
              

      ),
    
      
      #STATISTICS PAGE LAYOUT
      tabItem(tabName = "stats",
              h1("Statistics")),
      
      #TUTORIALS PAGE LAYOUT
      tabItem(tabName = "tutorials",
              h1("Tutorials"),
              fluidRow(
                box(
                  width = 12, 
                  p("Most of the work done on this project is documented through a series of 
                    Jupyter Notebook Tutorials that are available for download on the 'Downloads'
                    tab or on Github at the following links:"),
                  br(),
                  a("Data Cleaning", href = "https://github.com/isaacnk/GIS3/blob/master/Election%20Data%20Cleaning%20and%20Merging.ipynb"),
                  br(),
                  a("Regression", href = "https://github.com"), #####MODIFY W CORRECT URL
                  br(),
                  a("Factor Analysis", href = "https://github.com") ###### DITTO
                )
                
                
              )),
      
      #DOWNLOADS PAGE LAYOUT
      tabItem(tabName = "downloads",
              h1("Downloads"),
              fluidRow(
                box(
                  width = 12,
                  h4("Click to Download the County-Level Election/Demographics Data:"),
                  downloadButton(outputId = "shpdownload", label = "Download"),
                  br(),
                  p("Note: Explanation of variable names can be found in the 'About' Tab"),
                  br(),
                  h4("Click to Download the Data Cleaning Tutorial:"),
                  downloadButton(outputId = "cleantutdwn", label = "Download"),
                  br(),
                  br(),
                  h4("Click to Download the Source Code for this Application"),
                  downloadButton(outputId = "shinydwn", label = "Download"))
                )
              )
              
              
              
              )
    
  )
  
)




server <- function(input, output) {

  output$homemap <- renderLeaflet({
    
      plotvar <- switch(input$homevar, 
                        "% of Vote Share for Hillary" = "pct_hll",
                        "% of Vote Share for Trump" = "pct_trm",
                        "% Population Change 2010-14" = "PST1202",
                        "% Under 18" = "AGE2952",
                        "% Over 65" = "AGE7752",
                        "% Female" = "SEX2552",
                        "% White" = "RHI1252", 
                        "% Black"= "RHI2252",
                        "% Hispanic" = "RHI7252",
                        "% Foreign Language at Home" = "POP8152",
                        "% HS Graduate" = "EDU6352", 
                        "% Bachelor's Degree" = "EDU6852", 
                        "Mean Travel Time to Work" = "LFE3052",
                        "% Homeowner" = "HSG4452",
                        "% in Multiunit Housing" = "HSG0962",
                        "Persons per Household" = "HSD3102",
                        "Median Household Income" = "INC1102",
                        "Persons Below Poverty Line" = "PVY0202")

      state <- input$homestate
      state.data <- merged.data[merged.data$stt_bbr == state,]
    
      working_map <- tm_shape(state.data) + tm_fill(plotvar, style="jenks", title = input$homevar, id = "NAME") + tm_borders() 
      tmap_leaflet(working_map)
      
  })
  
  output$homemap2 <- renderLeaflet({
    
    plotvar2 <- switch(input$homevar2, 
                      "% of Vote Share for Hillary" = "pct_hll",
                      "% of Vote Share for Trump" = "pct_trm",
                      "% Population Change 2010-14" = "PST1202",
                      "% Under 18" = "AGE2952",
                      "% Over 65" = "AGE7752",
                      "% Female" = "SEX2552",
                      "% White" = "RHI1252", 
                      "% Black"= "RHI2252",
                      "% Hispanic" = "RHI7252",
                      "% Foreign Language at Home" = "POP8152",
                      "% HS Graduate" = "EDU6352", 
                      "% Bachelor's Degree" = "EDU6852", 
                      "Mean Travel Time to Work" = "LFE3052",
                      "% Homeowner" = "HSG4452",
                      "% in Multiunit Housing" = "HSG0962",
                      "Persons per Household" = "HSD3102",
                      "Median Household Income" = "INC1102",
                      "Persons Below Poverty Line" = "PVY0202")
    
    state <- input$homestate
    state.data <- merged.data[merged.data$stt_bbr == state,]
    
    working_map2 <- tm_shape(state.data) + tm_fill(plotvar2, style="jenks", title = input$homevar2, id = "NAME") + tm_borders() 
    tmap_leaflet(working_map2)
    
  })
  
   output$cortext <- renderText({
              v1 <- switch(input$homevar, 
                            "% of Vote Share for Hillary" = "pct_hll",
                            "% of Vote Share for Trump" = "pct_trm",
                            "% Population Change 2010-14" = "PST1202",
                            "% Under 18" = "AGE2952",
                            "% Over 65" = "AGE7752",
                            "% Female" = "SEX2552",
                            "% White" = "RHI1252", 
                            "% Black"= "RHI2252",
                            "% Hispanic" = "RHI7252",
                            "% Foreign Language at Home" = "POP8152",
                            "% HS Graduate" = "EDU6352", 
                            "% Bachelor's Degree" = "EDU6852", 
                            "Mean Travel Time to Work" = "LFE3052",
                            "% Homeowner" = "HSG4452",
                            "% in Multiunit Housing" = "HSG0962",
                            "Persons per Household" = "HSD3102",
                            "Median Household Income" = "INC1102",
                            "Persons Below Poverty Line" = "PVY0202")
               
               v2 <- switch(input$homevar2, 
                            "% of Vote Share for Hillary" = "pct_hll",
                            "% of Vote Share for Trump" = "pct_trm",
                            "% Population Change 2010-14" = "PST1202",
                            "% Under 18" = "AGE2952",
                            "% Over 65" = "AGE7752",
                            "% Female" = "SEX2552",
                            "% White" = "RHI1252", 
                            "% Black"= "RHI2252",
                            "% Hispanic" = "RHI7252",
                            "% Foreign Language at Home" = "POP8152",
                            "% HS Graduate" = "EDU6352", 
                            "% Bachelor's Degree" = "EDU6852", 
                            "Mean Travel Time to Work" = "LFE3052",
                            "% Homeowner" = "HSG4452",
                            "% in Multiunit Housing" = "HSG0962",
                            "Persons per Household" = "HSD3102",
                            "Median Household Income" = "INC1102",
                            "Persons Below Poverty Line" = "PVY0202")
               
               state <- input$homestate
               state.data <- merged.data[merged.data$stt_bbr == state,]
               
               cor_var1 <- as.numeric(state.data@data[,v1])
               cor_var2 <- as.numeric(state.data@data[,v2])
           
               cor <- cor.test(cor_var1, cor_var2, method = "pearson")
               paste("The correlation estimate between the two variables is ", 
                     cor$estimate, "with a p value of ", cor$p.value, sep = " ")})
  
  
  
  ##### CODE HANDLING SHAPEFILE DOWNLOAD WAS TAKEN FROM STACK OVERFLOW USER MOHO WU 
  ##### ORIGINAL THREAD CAN BE ACCESSED at https://stackoverflow.com/questions/41707760/download-a-shape-file-from-shiny
  output$shpdownload <- downloadHandler(
      filename = "merged_demo_elect_data.zip",
      content = function(file) {
        data =  merged.data
        # create a temp folder for shp files
        temp_shp <- tempdir()
        # write shp files
        writeOGR(data, temp_shp, "merged_demo_elect_data", "ESRI Shapefile", 
                 overwrite_layer = TRUE)
        # zip all the shp files
        zip_file <- file.path(temp_shp, "merged_demo_elect_data.zip")
        shp_files <- list.files(temp_shp,
                                "merged_demo_elect_data",
                                full.names = TRUE)
        # the following zip method works for me in linux but substitute with whatever method working in your OS 
        zip_command <- paste("zip -j", 
                             zip_file, 
                             paste(shp_files, collapse = " "))
        system(zip_command)
        # copy the zip file to the file argument
        file.copy(zip_file, file)
        # remove all the files created
        file.remove(zip_file, shp_files)
      }
    )
  
  # DOWNLOAD R SCRIPT FOR DATA CLEANING TUTORIAL
  output$cleantutdwn <- downloadHandler(
    filename = function() { 
      paste("DataCleaningScript", ".R", sep="")
    },
    content = function(file) {
      file.copy("DataCleaning.R", file)
    })
  
  
  # DOWNLOAD R CODE FOR THIS APP (SO META I KNOW)
  output$shinydwn <- downloadHandler(
    filename = function() { 
      paste("App", ".R", sep="")
    },
    content = function(file) {
      file.copy("ElectionShiny.R", file)
    })
  
}


shinyApp(ui = ui, server = server)
