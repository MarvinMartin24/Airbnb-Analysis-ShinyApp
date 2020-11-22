library(shiny)
library(googleVis)
library(stringr)
source("../utils/tools.R")

print("Start downloading...")
listings <- load_global_listings() %>%
  mutate(latitudelongitude = str_c(latitude,":",longitude))
print("Done !")

server <- function(input, output) {
  
  listings_country <- reactive({
    listings %>% 
      filter(country == input$country)
  })
  
  listings_city <- reactive({
    if (is.null(input$city)){
      return(NULL)
    }

    listings_country() %>% 
      filter(city %in% input$city) 
  })
  
  listings_city_selected <- reactive({
    df <- listings_city()
    if (is.null(df)) {
      return(NULL)
    }
    
    range <- input$date_range
    if (is.null(range)) {
      return(NULL)
    }
    
    df$date <- as.Date(df$date)
    result <- subset(df, date >= range[1] & date <= range[2])
    return(result)
  })
  
  listings_city_multiple_features <- reactive({
    if (is.null(input$city) || is.null(input$feature_2)){
      return(NULL)
    }
    
    listings_city_selected() %>% 
      filter(!is.na(!! rlang::sym(input$feature_2)))
  })
  
  output$checkbox <- renderUI({
    choice <- unique(listings_country()$city)
    checkboxGroupInput("city", "Select Cities:", choices = choice, selected = NULL)
  })
  
  output$date_slider <- renderUI({
    df <- listings_city()
    dates <- as.Date(df$date)
    minmax <- range(df$date, na.rm = TRUE)
    dateRangeInput(inputId="date_range", h3("Select a date range"), min = minmax[1], start = minmax[1], max = minmax[2], end = minmax[2])
  })
  
  output$mean_feature_1 <- renderGvis({
    if (is.null(listings_city_selected()))
      return(NULL)

    mean <- listings_city_selected() %>%
      group_by(city) %>%
      summarize(mean=round(mean(!! rlang::sym(input$feature_1), na.rm = T),2), .groups = 'keep')
    
    gvisColumnChart(mean, xvar='city', yvar='mean', 
                    options = list(width = 550, height = 300, title = glue("Average {strsplit(input$feature_1,'_')[[1]][1]} per city")))
  })
  
  output$distribution <- renderPlot({
    
    if (is.null(listings_city_selected()))
      return(NULL)
    
    my_scale <- function(){
      
      if (str_detect(input$feature_1, c("availability"))){
        list(
          scale_x_continuous(limits = c(1, 40)),
          xlab("Days")
        )
       
      } 
      else if (str_detect(input$feature_1, c("rating"))){
        list(
          scale_x_continuous(limits = c(1, 100)),
          xlab("Review Scores Rating"))
      } 
      else if (str_detect(input$feature_1, c("price")) || str_detect(input$feature_1, c("revenue"))){
        list(
          scale_x_log10(),
          xlab(glue("{strsplit(input$feature_1,'_')[[1]][1]} $")))
      } 
      else {
        list(
          scale_x_continuous(limits = c(
                              quantile(listings_city_selected()[[input$feature_1]])[1], 
                              quantile(listings_city_selected()[[input$feature_1]])[4] + quantile(listings_city_selected()[[input$feature_1]])[3])
                             ),
          xlab(glue("{str_replace(input$feature_1,'_', ' ')}")))
      }
    }
    
    ggplot(listings_city_selected(), aes(x=listings_city_selected()[[input$feature_1]])) + 
      geom_density(aes(color = city, fill=city), alpha = 0.4, size=1) +
      geom_rug(aes(color = city), alpha = 0.4) + 
      my_scale() +
      ggtitle(glue("Distribution of {strsplit(input$feature_1,'_')[[1]][1]} per city")) + 
      ylab("Density")
  })
  
  output$distribution_multiple_features <- renderPlot({
    
    if (is.null(listings_city_multiple_features()))
      return(NULL)
    
    ggplot(listings_city_multiple_features() , aes(city, listings_city_multiple_features()[[input$feature_1]])) + 
      geom_boxplot(aes(colour = !! rlang::sym(input$feature_2)), na.rm = TRUE) + 
      scale_y_continuous(limits = quantile(listings_city_selected()[[input$feature_1]], c(0.1, 0.9), na.rm = TRUE)) +
      ggtitle(glue("Distribution of {strsplit(input$feature_1,'_')[[1]][1]} per city")) + 
      xlab("City") + 
      ylab(glue("{str_replace(input$feature_1,'_', ' ')}"))
  })
  
  output$map <- renderGvis({
    
    if (is.null(listings_city_selected()))
      return(NULL)

    gvisMap(listings_city_selected(), locationvar="latitudelongitude" , tipvar=input$feature_1, 
            options=list(showTip=TRUE, 
                         showLine=TRUE, 
                         enableScrollWheel=TRUE,
                         mapType='terrain', 
                         useMapTypeControl=TRUE))
  })
  
  
}

