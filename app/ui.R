library(shiny)
library(dplyr)
library(stringr)
library(ggplot2)
library(data.table)
library(scales)
library(glue)
library(rmarkdown)


ui <- fluidPage(
  titlePanel("Airbnb Analysis"),
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput("var", 
                         h3("Select cities:"), 
                         choices = list("Malaga" = "malaga", 
                                        "Mallorca" = "mallorca", 
                                        "Sevilla" = "sevilla"),
                         selected = "malaga")
      ),
    
    mainPanel(
      plotOutput("draw")
    )
  )
)