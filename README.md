# Better Understanding the Role of Outliers in 2016 Election Data
Final Project for GIS III — Geocomputation with R

Isaac Kamber & Lorenz Menendez 
University of Chicago


### Overview
The 2016 United States Election was among the most polarizing in recent memory. Its results were completely unexpected, taking pollsters and career politicos by suprise. In the time since, analysts have struggled to answer the basic question: what happened? Why were the polls so wrong and what can we learn from the results? This project uses geocomputational techniques to begin answering that question. 

Through a comprehensive yet easy-to-use R Shiny Application with related Jupyter Notebook tutorials, it explores the role of "outlier" counties, defined as counties falling into the low-high or high-low categorization of a LISA Map, in election data. It looks as trends that distinguish these counties in terms of demographics and 

A guiding theme of this project is data accessibility. The data tells more stories than two people will be able to tell in one application or through a few tutorials. That is why the R Shiny Application has functionality geared towards the user exploring the data for him/herself. They can view county-level trends in thousands of variable combinations across all 50 states and the District of Columbia. Moreover, the documented tutorials and accessibility of the code (all available on this repo and linked in the application) instruct those unfamiliar with the geocomputational techniques in nearly all of the steps needed to replicate and expand on our work. 

### Goals and Objectives

The goal for this project is to understand why there exists counties that vote for a different candidate than surrounding counties. Using an exploratory spatial data analysis technique, we found that there are 72 counties whose voting patterns are significantly different than their surround neighbor counties. To explain this phenomenon, we are going to identify the socioeconomic and demographic characteristics of outlying counties and compare them to their neighboring counties.

We assume that a percentage vote for one specific candidate is a dependent variable to social, economic, and demographic indicators. Therefore, we are going to apply an exploratory data analysis technique called Principal Component Analysis (PCA) to distill our varibles down into principal (uncorrelated) components that can help us categorize the types of outliers.

The objectives of this project are as follows: 
  * Create a user-friendly R Shiny Application to facilitate interaction with the data and our analytical processes
  * Conduct an exploratory data analysis to determine the relative relations of different variables to election data
  * Use geocomputational techniques to explore clustering, particularly using Local Indicators of Spatial Autocorrelation (LISA) to highlight "outlier areas" defined as counties in the low-high or high-low category of a LISA Map 
  * Create Jupyter Notebook tutorials outlining some of the major steps of our project including: 
    * Data Cleaning
    * Factor Analysis
    * Spatial Cluster Analysis
  * Do everything with the greatest possible replicability

### Data Description

This project was inspired by Kaggle’s 2016 US election dataset provided by Ben Hammer. The dataset consists of a number of files containing information on county-level demographics statistics and primary election results for the 2016 election. However, due to certain issues with the Kaggle dataset’s lack of general election results, we decided to use MIT’s County Presidential Election Returns 2000-2016 dataset for election results numbers. We still chose to use the Kaggle dataset’s demographic information, as it contains all of the demographic information that we are interested in exploring. We are also using the United States Counties shapefile used in the Kaggle dataset.

Year: 2016
Spatial Resolution: County Level
Location: Across United States of America


Data Dictionary (Adapted from Ben Hammer's 2016 Election Dataset on Kaggle.com):

| column_name | description                                                            | 
|-------------|------------------------------------------------------------------------| 
| PST045214   | Population, 2014 estimate                                              | 
| PST040210   | Population, 2010 (April 1) estimates base                              | 
| PST120214   | Population, percent change - April 1, 2010 to July 1, 2014             | 
| POP010210   | Population, 2010                                                       | 
| AGE135214   | Persons under 5 years, percent, 2014                                   | 
| AGE295214   | Persons under 18 years, percent, 2014                                  | 
| AGE775214   | Persons 65 years and over, percent, 2014                               | 
| SEX255214   | Female persons, percent, 2014                                          | 
| RHI125214   | White alone, percent, 2014                                             | 
| RHI225214   | Black or African American alone, percent, 2014                         | 
| RHI325214   | American Indian and Alaska Native alone, percent, 2014                 | 
| RHI425214   | Asian alone, percent, 2014                                             | 
| RHI525214   | Native Hawaiian and Other Pacific Islander alone, percent, 2014        | 
| RHI625214   | Two or More Races, percent, 2014                                       | 
| RHI725214   | Hispanic or Latino, percent, 2014                                      | 
| RHI825214   | White alone, not Hispanic or Latino, percent, 2014                     | 
| POP715213   | Living in same house 1 year & over, percent, 2009-2013                 | 
| POP645213   | Foreign born persons, percent, 2009-2013                               | 
| POP815213   | Language other than English spoken at home, pct age 5+, 2009-2013      | 
| EDU635213   | High school graduate or higher, percent of persons age 25+, 2009-2013  | 
| EDU685213   | Bachelor's degree or higher, percent of persons age 25+, 2009-2013     | 
| VET605213   | Veterans, 2009-2013                                                    | 
| LFE305213   | Mean travel time to work (minutes), workers age 16+, 2009-2013         | 
| HSG010214   | Housing units, 2014                                                    | 
| HSG445213   | Homeownership rate, 2009-2013                                          | 
| HSG096213   | Housing units in multi-unit structures, percent, 2009-2013             | 
| HSG495213   | Median value of owner-occupied housing units, 2009-2013                | 
| HSD410213   | Households, 2009-2013                                                  | 
| HSD310213   | Persons per household, 2009-2013                                       | 
| INC910213   | Per capita money income in past 12 months (2013 dollars), 2009-2013    | 
| INC110213   | Median household income, 2009-2013                                     | 
| PVY020213   | Persons below poverty level, percent, 2009-2013                        | 
| BZA010213   | Private nonfarm establishments, 2013                                   | 
| BZA110213   | Private nonfarm employment,  2013                                      | 
| BZA115213   | Private nonfarm employment, percent change, 2012-2013                  | 
| NES010213   | Nonemployer establishments, 2013                                       | 
| SBO001207   | Total number of firms, 2007                                            | 
| SBO315207   | Black-owned firms, percent, 2007                                       | 
| SBO115207   | American Indian- and Alaska Native-owned firms, percent, 2007          | 
| SBO215207   | Asian-owned firms, percent, 2007                                       | 
| SBO515207   | Native Hawaiian- and Other Pacific Islander-owned firms, percent, 2007 | 
| SBO415207   | Hispanic-owned firms, percent, 2007                                    | 
| SBO015207   | Women-owned firms, percent, 2007                                       | 
| MAN450207   | Manufacturers shipments, 2007 ($1,000)                                 | 
| WTN220207   | Merchant wholesaler sales, 2007 ($1,000)                               | 
| RTN130207   | Retail sales, 2007 ($1,000)                                            | 
| RTN131207   | Retail sales per capita, 2007                                          | 
| AFN120207   | Accommodation and food services sales, 2007 ($1,000)                   | 
| BPS030214   | Building permits, 2014                                                 | 
| LND110210   | Land area in square miles, 2010                                        | 
| POP060210   | Population per square mile, 2010                                       | 


### Application Guide 

Pages: 

  * Home:
    * Dual maps displaying two results/demographics categories for a state to allow for visual comparison of overall trends and county clusters 
    * Simple correlation information on bottom of the page
    * Interactive functions on map and county labels appearing with mouse hover
    <img src="https://github.com/isaacnk/2016Election/blob/master/Home.png" />
    
  * About:
    * Project Overview 
    * Data Dictionary
    * Sources
    <img src="https://github.com/isaacnk/2016Election/blob/master/About.png" />
    
  * Statistics:
    * INSERT INFO HERE
    <img src="https://github.com/isaacnk/2016Election/blob/master/Statistics.png" />
    
  * Tutorials:
    * Jupyter Notebook walkthroughs for the following topics: 
       Data Cleaning
       Factor Analysis
       Spatial Cluster Analysis
    <img src="https://github.com/isaacnk/2016Election/blob/master/Tutorials.png" />
    
  * Downloads:
    * Download county demographics/results shapefile used to generate the maps on the Home page as a zipped folder
    * Download the Jupyter Notebook Tutorials
    * Download the source code to the R Shiny Application itself
    <img src="https://github.com/isaacnk/2016Election/blob/master/Downloads.png" />

### Figures

#### LISA Map of County Clusters
<img src="https://github.com/isaacnk/2016Election/blob/master/Hillary_Cluster.png?raw=true" />
This map shows where there are clusters of major support and clusters of little support for Hillary Clinton. Notice the outlying 'high-low' and 'low-high' clusters throughout the map.

#### Correlation Matrix between Percent of Vote for Hillary Clinton and selected demographic variables
<img src="https://github.com/isaacnk/2016Election/blob/master/Correlations%20Figure.png?raw=true" />
This shows the R output of a correlation matrix showing some variables with high correlation between percent of vote and demographic variables.

#### Kaiser-Meyer-Olkin (KMO) Test for Sampling Accuracy
<img src="https://github.com/isaacnk/2016Election/blob/master/KMO%20Figure.png?raw=true" />
Describes the result from KMO test. See Jupyter Notebook Tutorial for more information.

#### Scree Plot
<img src="https://github.com/isaacnk/2016Election/blob/master/Scree%20Plot.png" />
This plot shows how much variance each principal component is responsible for. Notice how the first three components are responsable for ~80% of the variance in the data.

Note: Additional figures can be found inside the R Shiny Application and in the Jupyter Notebook Tutorials.

### Future Work 

For the project to be more successful, we would like to expand on our Factor Analysis to include a regression analysis of Hillary Clinton's share of the vote with each of the three components. This would allow us to make accurate assumptions about which type of outlier county Clinton does well in, and which types of counties Donald Trump does well in.

### Sources

Demographics Data is sourced from: 
Hammer, Ben (2016). 2016 US Election (Version 8) [shapefile]. Retrieved from
https://www.kaggle.com/benhamner/2016-us-election/activity

Election Data is sourced from: 
MIT Election Data and Science Lab, 2018, "County Presidential Election Returns 2000-2016", https://doi.org/10.7910/DVN/VOQCHQ, Harvard Dataverse, V1, UNF:6:ZaxsDvp6RsFitno8ZYlK7w== [fileUNF]

Statistics and PCA Help Provided By:
* "Principal Component Analysis and Factor Analysis in R" by Ani Katchova (https://www.youtube.com/watch?v=Od8gfNOOS9o)
* Interpreting the output from PCA (https://stats.stackexchange.com/questions/4093/interpreting-pca-scores)
* "Understanding PCA with an Example" by Subhasree Chatterjee  (https://www.linkedin.com/pulse/understanding-pca-example-subhasree-chatterjee/)
