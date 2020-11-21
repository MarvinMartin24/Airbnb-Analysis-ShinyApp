library(shiny)
library(googleVis)
source("../utils/tools.R")

print("Start downloading...")
listings <- load_global_listings()
print("Done !")

server <- function(input, output) {
  
  listings_country <- reactive({
    listings %>% 
      filter(country == input$country)
  })
  
  output$checkbox <- renderUI({ 
    choice <- unique(listings_country()$city)
    checkboxGroupInput("city", "Select Cities", choices = choice, selected = choice[1])
  })

  output$mean <- renderGvis({
    
    listings_city <- listings_country() %>% 
      filter(city == input$city) ## Maybe use observe

    mean <- listings_city %>%
      group_by(city) %>%
      summarize(mean=round(mean(!! rlang::sym(input$metric), na.rm = T),2), .groups = 'keep')
    
    gvisColumnChart(mean, xvar='city', yvar='mean', 
                    options = list(width = 450, height = 300, title = "City Popularity"))
  })
  
}

