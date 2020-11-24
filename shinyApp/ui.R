library(shiny)
library(dplyr)
library(stringr)
library(ggplot2)
library(data.table)
library(scales)
library(glue)
library(rmarkdown)

ui <- fluidPage(
  
  tabsetPanel(
    tabPanel("Analysis 1 – Comparing cities", 
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
                 
                 uiOutput("select_city_tab1"),
                 
                 uiOutput("date_slider"),
                 
                 selectInput("feature_1", 
                             h3("Select a feature:"), 
                             choices = list("Revenue (30 days)" = "revenue_30", 
                                            "Availability (30 days)" = "availability_30",
                                            "Price (30 days)" = "price_30",
                                            "Minimum Nights" = "minimum_nights",
                                            "Maximum Nights" = "maximum_nights",
                                            "Review Scores Rating" = "review_scores_rating"
                             ),
                             selected = "revenue_30"),
                 
                 selectInput("feature_2", 
                             h3("Select another feature:"), 
                             choices = list("Room type" = "room_type", 
                                            "Number of Bedrooms" = "bedrooms"
                             ),
                             selected = NULL),
               ),
               mainPanel(
                 htmlOutput("mean_feature_1"),
                 plotOutput("distribution"),
                 plotOutput("distribution_multiple_features")
               ),
             )),
    tabPanel("Analysis 2 – Deep dive into a city",
             sidebarLayout(
               sidebarPanel(
                 
                 uiOutput("select_city_tab2"),
                 uiOutput("date_slider_tab2"),
                 
                 selectInput("feature_tab2", 
                             h3("Select a feature:"), 
                             choices = list("Revenue (30 days)" = "revenue_30", 
                                            "Availability (30 days)" = "availability_30",
                                            "Price (30 days)" = "price_30",
                                            "Minimum Nights" = "minimum_nights",
                                            "Maximum Nights" = "maximum_nights",
                                            "Review Scores Rating" = "review_scores_rating"
                                            
                             ),
                             selected = "revenue_30"),
                 
                 uiOutput("min_feature_1_tab2"),
                 uiOutput("max_feature_1_tab2"),
                 
                 selectInput("feature_proportion_tab2", 
                             h3("Select a plot feature:"), 
                             choices = list("Room type" = "room_type", 
                                            "Number of bedrooms" = "bedrooms",
                                            "Neighbourhood" = "neighbourhood_cleansed"
                                            
                             ),
                             selected = "room_type"),
                 
               ),
               mainPanel(
                 htmlOutput("map"),
                 plotOutput("plot_proportion")
               ),
             )
             )
  )
)