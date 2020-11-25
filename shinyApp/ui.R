library(shiny)
library(googleVis)
library(dplyr)
library(stringr)
library(ggplot2)
library(data.table)
library(glue)
library(rmarkdown)

ui <- fluidPage(
  
  tabsetPanel(
    tabPanel("Analysis 1 – Comparing cities",
             titlePanel("Tool Bar"),
             sidebarLayout(
               sidebarPanel(
                 helpText("Analysis 1 helps to compare Airbnb data for different cities for a given country. You can play with dates, feature 1 (numerical data) and feature 2 (categorical data)."),
                 selectInput("country", 
                             h3("Select country:"), 
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
                             h3("Select feature 1:"), 
                             choices = list("Revenue (over 30 days) in $" = "revenue_30", 
                                            "Availability (over 30 days) in days" = "availability_30",
                                            "Price (over 30 days) in $" = "price_30",
                                            "Minimum Nights" = "minimum_nights",
                                            "Maximum Nights" = "maximum_nights",
                                            "Review Scores Rating (%)" = "review_scores_rating"
                             ),
                             selected = "revenue_30"),
                 
                 selectInput("feature_2", 
                             h3("Select feature 2:"), 
                             choices = list("Room type" = "room_type", 
                                            "Number of Bedrooms" = "bedrooms"
                             ),
                             selected = NULL),
                 br(),
                 hr(),
                 helpText("Made with <3 by Michel & Marvin"),
               ),
               mainPanel(
                 htmlOutput("mean_feature_1"),
                 br(),
                 hr(),
                 plotOutput("distribution"),
                 br(),
                 hr(),
                 plotOutput("distribution_multiple_features")
               ),
             )),
    tabPanel("Analysis 2 – Deep dive into a city",
             titlePanel("Tool Bar"),
             sidebarLayout(
               sidebarPanel(
                 helpText("Analysis 2 helps to Deep dive into a city to analyse Airbnb data. You can play with dates, feature 1 (numerical data) and feature 2 (categorical data). For the map, you can apply different filters to reduce the amount of geopoints."),
                 
                 uiOutput("select_city_tab2"),
                 uiOutput("date_slider_tab2"),
                 
                 selectInput("feature1_tab2",
                             h3("Select feature 1:"), 
                             choices = list("Revenue (over 30 days) in $" = "revenue_30", 
                                            "Availability (over 30 days) in days" = "availability_30",
                                            "Price (over 30 days) in $" = "price_30",
                                            "Minimum Nights" = "minimum_nights",
                                            "Maximum Nights" = "maximum_nights",
                                            "Review Scores Rating (%)" = "review_scores_rating"
                                            
                             ),
                             selected = "revenue_30"),

                 selectInput("feature2_tab2",
                             h3("Select feature 2:"), 
                             choices = list("Room type" = "room_type", 
                                            "Number of bedrooms" = "bedrooms",
                                            "Neighbourhood (Top 4)" = "neighbourhood_cleansed"
                                            
                             ),
                             selected = "room_type"),
                 hr(),
                 h3("Map filters:"),
                 uiOutput("minmax_feature_1_tab2"),
                 br(),
                 hr(),
                 helpText("Made with <3 by Michel & Marvin"),
               ),
               mainPanel(
                 br(),
                 fluidRow(
                   column(6, htmlOutput("plot_proportion")),
                   column(6, htmlOutput("mean_feature_1_tab2")),
                 ),
                 br(),
                 hr(),
                 plotOutput("distribution_multiple_features_tab2"),
                 br(),
                 hr(),
                 uiOutput("map_title"),
                 uiOutput("min_localizaton"),
                 htmlOutput("map")
               ),
             )
             )
  )
)