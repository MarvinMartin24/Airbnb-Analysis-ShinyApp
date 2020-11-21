library(shiny)
library(googleVis)
library(stringr)
source("../utils/tools.R")

print("Start downloading...")
listings <- load_global_listings()
print("Done !")

server <- function(input, output) {
  
  listings_country <- reactive({
    out <- listings %>% 
      filter(country == input$country)
  })
  
  listings_city <- reactive({
    
    if (is.null(input$city)){
      return(NULL)
    }
    
    listings_country() %>% 
      filter(city == input$city)
  })
  
  output$checkbox <- renderUI({ 
    choice <- unique(listings_country()$city)
    checkboxGroupInput("city", "Select Cities:", choices = choice, selected = NULL)
  })

  output$mean <- renderGvis({
    
    if (is.null(listings_city()))
      return(NULL)

    mean <- listings_city() %>%
      group_by(city) %>%
      summarize(mean=round(mean(!! rlang::sym(input$metric), na.rm = T),2), .groups = 'keep')
    
    gvisColumnChart(mean, xvar='city', yvar='mean', 
                    options = list(width = 550, height = 300, title = glue("Average {strsplit(input$metric,'_')[[1]][1]} per city")))
  })
  
  output$distribution <- renderPlot({
    
    if (is.null(listings_city()))
      return(NULL)
    
    my_scale <- function(){
      
      if (str_detect(input$metric, c("availability"))){
        list(
          scale_x_continuous(limits = c(1, 40)),
          xlab("Days")
        )
       
      } 
      else if (str_detect(input$metric, c("rating"))){
        list(
          scale_x_continuous(limits = c(1, 100)),
          xlab("Review Scores Rating"))
      } 
      else if (str_detect(input$metric, c("price")) || str_detect(input$metric, c("revenue"))){
        list(
          scale_x_log10(),
          xlab(glue("{strsplit(input$metric,'_')[[1]][1]} $")))
      } 
      else {
        list(
          scale_x_continuous(limits = c(
                              quantile(listings_city()[[input$metric]])[1], 
                              quantile(listings_city()[[input$metric]])[4] + quantile(listings_city()[[input$metric]])[3])
                             ),
          xlab(glue("{str_replace(input$metric,'_', ' ')}")))
      }
    }
    
    ggplot(listings_city(), aes(x=listings_city()[[input$metric]])) + 
      geom_density(aes(color = city, fill=city), alpha = 0.4, size=1) +
      geom_rug(aes(color = city), alpha = 0.4) + 
      my_scale() +
      ggtitle("Distribution per city ") + 
      ylab("Density") +
      theme(rect = element_rect(fill = "transparent"))
  })
  
}

