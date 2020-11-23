library(shiny)
library(googleVis)
library(stringr)
source("../utils/tools.R")

print("Start downloading...")
listings <- load_global_listings() %>%
  mutate(latitudelongitude = str_c(latitude,":",longitude))
print("Done !")

server <- function(input, output) {
  
  ############################# Tab 1 ##################################
  
  # -------------------------- Reactive -----------------------------
  
  listings_country <- reactive({
    listings %>% 
      filter(country == input$country)
  })
  
  listings_city <- reactive({
    
    if (is.null(input$city_tab1)){
      return(NULL)
    }

    listings_country() %>% 
      filter(city %in% input$city_tab1) 
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
    
    if (is.null(input$city_tab1) | is.null(input$feature_2)){
      return(NULL)
    }
    
    listings_city_selected() %>% 
      filter(!is.na(!! rlang::sym(input$feature_2)))
  })
  
  # -------------------------- UI -----------------------------
  
  output$select_city_tab1 <- renderUI({ 
    choice <- unique(listings_country()$city)
    checkboxGroupInput("city_tab1", "Select Cities:", choices = choice, selected = NULL)
  })
  
  output$date_slider <- renderUI({
    df <- listings_city()
    dates <- as.Date(df$date)
    minmax <- range(df$date, na.rm = TRUE)
    dateRangeInput(inputId="date_range", h3("Select a date range"), min = minmax[1], start = minmax[1], max = minmax[2], end = minmax[2])
  })
  
  # -------------------------- Plots -----------------------------
  
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
      else if (str_detect(input$feature_1, c("price")) | str_detect(input$feature_1, c("revenue"))){
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
  
  
  ############################# Tab 2 ##################################
  
  # -------------------------- Reactive -----------------------------
  
  listings_city_tab2 <- reactive({
    if (is.null(input$city_tab2)){
      return(NULL)
    }

    listings_country() %>% 
      filter(city == input$city_tab2)
  })
  
  listings_city_selected_tab2 <- reactive({
    df <- listings_city_tab2()
    if (is.null(df)) {
      return(NULL)
    }
    
    range <- input$date_range_tab2
    if (is.null(range)) {
      return(NULL)
    }
    
    df$date <- as.Date(df$date)
    result <- subset(df, date >= range[1] & date <= range[2])
    return(result)
  })
  
  listings_city_tab2_feature <- reactive({
    if (is.null(listings_city_selected_tab2()) | is.null(input$range_min_feature) | is.null(input$range_max_feature)){
      return(NULL)
    }
    listings_city_selected_tab2() %>% 
      filter(!! rlang::sym(input$feature_tab2) <= input$range_max_feature & !! rlang::sym(input$feature_tab2) >= input$range_min_feature)
  })
  
  
  # -------------------------- UI -----------------------------
  
  output$select_city_tab2 <- renderUI({ 
    choice_tab2 <- unique(listings_country()$city)
    selectInput("city_tab2", "Select City:", choices = choice_tab2, selected = NULL)
  })
  
  output$date_slider_tab2 <- renderUI({
    df <- listings_city_tab2()
    dates <- as.Date(df$date)
    minmax <- range(df$date, na.rm = TRUE)
    dateRangeInput(inputId="date_range_tab2", h3("Select a date range"), min = minmax[1], start = minmax[1], max = minmax[2], end = minmax[2])
  })
  
  output$min_feature_1_tab2 <- renderUI({ 
    q <- quantile(listings_city_selected_tab2()[[input$feature_tab2]], na.rm = TRUE)
    numericInput("range_min_feature", 
                 label = "Minimun value:",
                 value = q[1])
  })
  
  output$max_feature_1_tab2 <- renderUI({ 
    q <- quantile(listings_city_selected_tab2()[[input$feature_tab2]], na.rm = TRUE)
    numericInput("range_max_feature", 
                label = "Maximun value:",
                value = q[3])
  })
  
  # -------------------------- Plots -----------------------------
  
  output$map <- renderGvis({
    
    
    if (is.null(listings_city_tab2_feature())){
      return(NULL)
    }
    Sys.sleep(0.3)
    gvisMap(listings_city_tab2_feature(), locationvar="latitudelongitude" , tipvar=input$feature_tab2, 
            options=list(showTip=TRUE, 
                         showLine=TRUE, 
                         enableScrollWheel=TRUE,
                         mapType='terrain', 
                         useMapTypeControl=TRUE))
  })
  
  
  ### proportion of each room type
  output$plot_room_type <- renderPlot({
    if (is.null(listings_city_tab2_feature())){
      return(NULL)
    }

    for (val in unique(listings_city_tab2_feature()$city)) {
      print(
        ggplot(filter(listings_city_tab2_feature(), city == val), aes(x=room_type)) + 
          geom_bar(aes(y = (..count..)/sum(..count..)), position="dodge", fill="darkgreen") + 
          geom_text(aes(label = percent(round((..count..)/sum(..count..),2)), y= ((..count..)/sum(..count..))), stat="count", vjust = -.25) +
          scale_y_continuous(labels =  percent_format()) +
          ggtitle(glue("Proportion of each room type in {val}")) + 
          xlab("Room type") + 
          ylab("Pourcentage")
      )
    }
  })
  
  
  ### proportion of each house size
  output$plot_house_size <- renderPlot({
    if (is.null(listings_city_tab2_feature())){
      return(NULL)
    }
  
    for (val in unique(listings_city_tab2_feature()$city)) {
      print(
        ggplot(filter(listings_city_tab2_feature(), city == val)  %>% filter(!is.na(bedrooms)) , aes(x=bedrooms)) + 
          geom_bar(aes(y = (..count..)/sum(..count..)), position="dodge", fill="steelblue") + 
          geom_text(aes(label = percent(round((..count..)/sum(..count..),2)), y= ((..count..)/sum(..count..))), stat="count", vjust = -.25) +
          ggtitle(glue("Proportion of each house size (# of bedroom) in {val}")) + 
          xlab("# of bedroom") + 
          ylab("Pourcentage")
      )
    }
  })
  
  ### proportion of each neighborhood
  output$plot_neighborhood <- renderPlot({
    if (is.null(listings_city_tab2_feature())){
      return(NULL)
    }
  
    for (val in unique(listings_city_tab2_feature()$city)) {
      top <- 20
      
      listings_top <- within(filter(listings_city_tab2_feature(), city == val), 
                             neighbourhood_cleansed <- factor(neighbourhood_cleansed, 
                                                              levels=tail(
                                                                names(sort(table(neighbourhood_cleansed),decreasing=F))
                                                                , top)
                             )) %>% filter(!is.na(neighbourhood_cleansed))
      
      print(
        ggplot(listings_top, aes(x=neighbourhood_cleansed)) + 
          geom_bar(aes(y = (..count..)/sum(..count..)), position="dodge") +
          coord_flip() + 
          scale_y_continuous(labels = percent_format()) + 
          theme(axis.text.y = element_text(size=4)) +
          ggtitle(glue("Proportion of each neighborhood in {val} (only top {top})")) + 
          xlab("Neighborhoods") + 
          ylab("Pourcentage")
      )
    }
  })
}

