library(shiny)
library(dplyr)
library(stringr)
library(ggplot2)
library(data.table)
library(scales)
library(glue)
library(rmarkdown)

ui <- fluidPage(
  
  titlePanel("Analysis 1 – Comparing cities"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("country", 
                  h3("Select a country:"), 
                  choices = list("France" = "france", 
                                 "Netherlands" = "the-netherlands",
                                 "Spain"= "spain",
                                 "Italy"= "italy",
                                 "Germany"= "germany",
                                 "Belgium"= "belgium"
                                 ),
                  selected = "france"),
      
      uiOutput("checkbox"),
    
      selectInput("metric", 
                  h3("Select a feature:"), 
                  choices = list("Revenue (30 days)" = "revenue_30", 
                                 "Availability (30 days)" = "availability_30",
                                 "Price (30 days)" = "price_30",
                                 "Minimum Nights" = "minimum_nights",
                                 "Maximum Nights" = "maximum_nights",
                                 "Review Scores Rating" = "review_scores_rating"
                  ),
                  selected = "revenue_30")
    ),
    mainPanel(
      htmlOutput("mean"),
      plotOutput("distribution")
    )
  )
)