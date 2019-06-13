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
                        p("PST045214	Population, 2014 estimate"),
                      p("PST040210	Population, 2010 (April 1) estimates base"),
                      p("PST120214	Population, percent change - April 1, 2010 to July 1, 2014"),
                      p("POP010210	Population, 2010"),
                      p("AGE135214	Persons under 5 years, percent, 2014"),
                      p("AGE295214	Persons under 18 years, percent, 2014"),
                      p("AGE775214	Persons 65 years and over, percent, 2014"),
                      p("SEX255214	Female persons, percent, 2014"),
                      p("RHI125214	White alone, percent, 2014"),
                      p("RHI225214	Black or African American alone, percent, 2014"),
                      p("RHI325214	American Indian and Alaska Native alone, percent, 2014"),
                      p("RHI425214	Asian alone, percent, 2014"),
                      p("RHI525214	Native Hawaiian and Other Pacific Islander alone, percent, 2014)",
                      p("RHI625214	Two or More Races, percent, 2014"),
                      p("RHI725214	Hispanic or Latino, percent, 2014"),
                      p("RHI825214	White alone, not Hispanic or Latino, percent, 2014"), 
                      p("POP715213	Living in same house 1 year & over, percent, 2009-2013"),
                      p("POP645213	Foreign born persons, percent, 2009-2013"),
                      p("POP815213	Language other than English spoken at home, pct age 5+, 2009-2013"),
                      p("EDU635213	High school graduate or higher, percent of persons age 25+, 2009-2013"),
                      p("EDU685213	Bachelor's degree or higher, percent of persons age 25+, 2009-2013"),
                      p("VET605213	Veterans, 2009-2013"),
                      p("LFE305213	Mean travel time to work (minutes), workers age 16+, 2009-2013"),
                      p("HSG010214	Housing units, 2014"),
                      p("HSG445213	Homeownership rate, 2009-2013"),
                      p("HSG096213	Housing units in multi-unit structures, percent, 2009-2013"),
                      p("HSG495213	Median value of owner-occupied housing units, 2009-2013"),
                      p("HSD410213	Households, 2009-2013"),
                      p("HSD310213	Persons per household, 2009-2013"),
                      p("INC910213	Per capita money income in past 12 months (2013 dollars), 2009-2013"),
                      p("INC110213	Median household income, 2009-2013"),
                      p("PVY020213	Persons below poverty level, percent, 2009-2013"),
                      p("BZA010213	Private nonfarm establishments, 2013"),
                      p("BZA110213	Private nonfarm employment,  2013"),
                      p("BZA115213	Private nonfarm employment, percent change, 2012-2013"),
                      p("NES010213	Nonemployer establishments, 2013"),
                      p("SBO001207	Total number of firms, 2007"),
                      p("SBO315207	Black-owned firms, percent, 2007"),
                      p("SBO115207	American Indian- and Alaska Native-owned firms, percent, 2007"),
                      p("SBO215207	Asian-owned firms, percent, 2007"),
                      p("SBO515207	Native Hawaiian- and Other Pacific Islander-owned firms, percent, 2007"),
                      p("SBO415207	Hispanic-owned firms, percent, 2007"),
                      p("SBO015207	Women-owned firms, percent, 2007"),
                      p("MAN450207	Manufacturers shipments, 2007 ($1,000)"),
                      p("WTN220207	Merchant wholesaler sales, 2007 ($1,000)"),
                      p("RTN130207	Retail sales, 2007 ($1,000)"),
                      p("RTN131207	Retail sales per capita, 2007"),
                      p("AFN120207	Accommodation and food services sales, 2007 ($1,000)"),
                      p("BPS030214	Building permits, 2014"),
                      p("LND110210	Land area in square miles, 2010"),
                      p("POP060210	Population per square mile, 2010")
                  )
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
            h1("Statistics"),

            fluidRow(
              box(width = 12,
                  h2("Overview"),
                  p("The goal of our project was to better understand why some counties voted for a different candidate during the 2016 U.S. Presidential Election than their neighbors. This is obviously a meaty question with many facets which become relevant every elefction cycle, so we decided to takle a more prescise question; What are the major characteristics of outlying counties, and how can they be used to analyze voting outcomes")

                  ),
              br(),

              box(width = 12,
                  h2("Finding Outlying Counties"),
                  p("We considered outlying counties to be those that appear as outliers after applying a Local Moran's I analysis to our dataset and plotting the results."),
                  p("We used a workflow in R and GeoDa to clean our data and run the analysis. In GeoDa, we created a Queen contiguity spatial weight matrix for all counties in all fifty states. Then, we calculated the Local Moran's I using the percent of voters who voted for Hillary Clinton as our variable of study. Using the percent voted for Donald Trump yields about the opposite result, but with some minor differences due to the existence of Third-Party candidates who gain a small percentage of the vote."),
                  p("In total, we found that there were approximately 70 counties that could be considered as outlying counties when it came to voting to Hillary Clinton. These counties were dispersed pretty well across the U.S., suggesting that it is not a regional or simply local phenomenon. Using this data, we continued our statistical journey by implementing Factor Analysis."),
                  p("For more information, see our Jupyter Notebook tutorial on Spatial Clustering")

                  ), 

              br(),

              box(width = 12,
                  h2("Factor Analysis using Principal Component Analysis (PCA)"),
                  p("Running a PCA helped us better understand which characteristics define the ~70 outlying counties by reducing the number of variables (or 'dimensions') so we can interpret overall trends. At the end of the analysis, we had a breakdown of how each demographic variable contributed to more general components which are themselves uncorrelated."),
                  p("The first step to implementing a PCA analysis is to verify that out data is actually suitable for this type of analysis. Even though we had around 50 variables, we needed to make sure that these variables are sufficiently correlated."),
                  p("To verify the correlation, we used R to create a correlation matrix between all of our variables. We noticed that there were multiple variables with high to very high correlations with percent voting. More formally, we calculated the Kaiser-Meyer-Olkin Statistic to verify that our data was sampled accurately. On a scale of 0 (Poorly Sampeld) to 1 (Excellently Sampled) our data had an overall KMO of 0.57. While this is over the threshold of 0.5, it is still considered very medocrily sampled data. With this out of the way, we ran the PCA"),
                  p("Using the 'FactoMineR' package, we ran a PCA analysis on the demographic and social variables after removing the dependent voting data. The output showed that the first principal component accounts for 35% of the variance, while top three components account for about 80% of the total variance in the demographic data for outlying counties. Therefore, we analyzed the three first components in more detail"),
                  p("Our output shows that four variables are highly correlated (>0.80) with the first component. These are: Bachelor's degree or higher, percent of persons age 25+ — Percent of Firms that Women Owned, Percent of persons who are high school graduates, and Percent of housing units in multi-unit structures"),

                  p("This suggests that the first component could be a measure of how 'urban' an outlying county is High Education/Graduation Rates in High School, Large amount of multi-unit housing, and Advanced Female Participation in Business"),

                  p("A single variable is highly correlated (>0.80) with the second component. That variable was 'Persons under 5 years, percent'. But, two variables are highly anticorrelated (< -0.80) with the second component. Those were 'Homeownership rate', and 'Percent of the population age 65+'"),

                  p("This suggests that the second component measures mostly how 'family-oriented, but not suburban or wealthy' the county is."),

                  p("One Variable is highly anticorrelated (< -0.80) with the third component, 'Percent Female'. This suggests that the Third component measures mostly how 'male' a county is."),

                  p("Overall, the PCA analysis tentatively suggests that the outlying counties can be characterized by how 'urban', 'family-oriented', or 'male' they are."),
                  
                  p("But, this is an exploratory assessment that is NOT supported by any academic studies or theorhetical reasoning as to why these outliying counties can be characterized as such. Additionally, this is only one interpretation out of many that can be gleaned from the same dataset simply by changing which components you study and how you define 'high' correlation or anticorrelation."),
                  
                  p("For more information, see our Jupyter Notebook tutorial on Factor Analysis")
                )
            )
      ),
      
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
                  a("Spatial Cluster Analysis", href = "https://github.com/isaacnk/2016Election/blob/master/Notebooks/Spatial%20Cluster%20Analysis.ipynb"),
                  br(),
                  a("Factor Analysis in R", href = "https://github.com/isaacnk/2016Election/blob/master/Notebooks/Factor%20Analysis.ipynb")
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
