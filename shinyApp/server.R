library(shiny)
library(googleVis)
library(stringr)
source("../utils/tools.R")

print("Start downloading...")
listings <- load_global_listings()
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
    df2 <- listings_city()
    dates <- as.Date(df2$date)
    minmax <- range(df2$date, na.rm = TRUE)
    dateRangeInput(inputId="date_range", h3("Select date range:"), min = minmax[1], start = minmax[1], max = minmax[2], end = minmax[2])
  })
  
  # -------------------------- Plots -----------------------------
  
  output$mean_feature_1 <- renderGvis({
    
    Sys.sleep(0.2)
    if (is.null(listings_city_selected()))
      return(NULL)

    mean <- listings_city_selected() %>%
      group_by(city) %>%
      summarize(mean=round(mean(!! rlang::sym(input$feature_1), na.rm = T),2), .groups = 'keep')
    
    gvisColumnChart(mean, xvar='city', yvar='mean', 
                    options = list(width = 750, height = 400, title = glue("Average {strsplit(input$feature_1,'_')[[1]][1]} per city")))
  })
  
  output$distribution <- renderPlot({
    Sys.sleep(0.2)
    if (is.null(listings_city_selected()))
      return(NULL)
    
    ggplot(listings_city_selected(), aes(x=listings_city_selected()[[input$feature_1]])) + 
      geom_density(aes(color = city, fill=city), alpha = 0.4, stat = "density", size=1) +
      geom_rug(aes(color = city), alpha = 0.4) + 
      scale_x_log10() +
      ggtitle(glue("Distribution of {strsplit(input$feature_1,'_')[[1]][1]} per city")) + 
      xlab(glue("{str_replace(input$feature_1,'_', ' ')}")) +
      ylab("Density")
  })
  
  output$distribution_multiple_features <- renderPlot({
    Sys.sleep(0.2)
    if (is.null(listings_city_multiple_features()))
      return(NULL)
    
    ggplot(listings_city_multiple_features() , aes(city, listings_city_multiple_features()[[input$feature_1]])) + 
      geom_boxplot(aes(colour = !! rlang::sym(input$feature_2)), na.rm = TRUE) + 
      scale_y_continuous(limits = quantile(listings_city_selected()[[input$feature_1]], c(0.1, 0.9), na.rm = TRUE)) +
      ggtitle(glue("Distribution of {strsplit(input$feature_1,'_')[[1]][1]} per city per {str_replace(input$feature_2,'_', ' ')}")) + 
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
    df3 <- listings_city_tab2()
    if (is.null(df3)) {
      return(NULL)
    }
    
    range <- input$date_range_tab2
    if (is.null(range)) {
      return(NULL)
    }
    
    df3$date <- as.Date(df3$date)
    result <- subset(df3, date >= range[1] & date <= range[2])
    return(result)
  })
  
  listings_city_tab2_feature <- reactive({
    if (is.null(listings_city_selected_tab2()) | is.null(input$range_min_feature) | is.null(input$range_max_feature)){
      return(NULL)
    }
    listings_city_selected_tab2() %>% 
      filter(!! rlang::sym(input$feature1_tab2) <= input$range_max_feature & !! rlang::sym(input$feature1_tab2) >= input$range_min_feature)
  })
  
  # -------------------------- UI -----------------------------
  
  output$select_city_tab2 <- renderUI({ 
    choice_tab2 <- unique(listings_country()$city)
    selectInput("city_tab2", h3("Select City:"), choices = choice_tab2, selected = NULL)
  })
  
  output$date_slider_tab2 <- renderUI({
    df4 <- listings_city_tab2()
    dates <- as.Date(df4$date)
    minmax <- range(df4$date, na.rm = TRUE)
    dateRangeInput(inputId="date_range_tab2", h3("Select date range:"), min = minmax[1], start = minmax[1], max = minmax[2], end = minmax[2])
  })
  
  output$minmax_feature_1_tab2 <- renderUI({ 
    q <- quantile(listings_city_selected_tab2()[[input$feature1_tab2]], na.rm = TRUE)
    
    fluidRow(
      column(6, numericInput("range_min_feature", label = glue("Min {strsplit(input$feature1_tab2,'_')[[1]][1]}:"), value = q[1])),
      column(6, numericInput("range_max_feature", label = glue("Max {strsplit(input$feature1_tab2,'_')[[1]][1]}:"), value = q[3])),
      column(12, numericInput("top_feature", label = "Number of localizations:", value = 50))
    )
  })
  
  output$min_localizaton <- renderUI({
    minmax_feat <- range(listings_city_selected_tab2()[[input$feature1_tab2]], na.rm = T)
    p(glue("Please use at most {nrow(listings_city_tab2_feature())} localizations and {strsplit(input$feature1_tab2,'_')[[1]][1]} between {minmax_feat[1]} and {minmax_feat[2]}."))
  })
  
  output$map_title <- renderUI({
    h4(glue("Map of {input$city_tab2} specifying {strsplit(input$feature1_tab2,'_')[[1]][1]}."))
  })
  
  # -------------------------- Plots -----------------------------
  
  output$plot_proportion <- renderGvis({
    Sys.sleep(0.2)
    if (is.null(listings_city_tab2_feature()) | is.null(input$feature2_tab2)){
      return(NULL)
    }
    
    
    feature <- input$feature2_tab2
    df5 <- listings_city_tab2_feature()[feature]
    listings_top <- top_n(as.data.frame(sort(table(na.omit(df5)), decreasing=T)), 20)
    
    
    if (length(names(listings_top)) != length(c(feature, 'freq'))){
      return(NULL)
    }
    names(listings_top) <- c(feature, 'freq')
    listings_top['freq'] <- listings_top['freq'] / sum(listings_top['freq'])
    
    gvisColumnChart(listings_top, xvar=feature, yvar='freq', 
                    options = list(width = 550, height = 300, title = glue("Proportion of {strsplit(input$feature2_tab2,'_')[[1]][1]}")))
  })
  
  output$mean_feature_1_tab2 <- renderGvis({
    Sys.sleep(0.2)
    if (is.null(listings_city_selected_tab2()))
      return(NULL)
    
    feature1 <- input$feature1_tab2
    feature2 <- input$feature2_tab2
    
    if (input$feature2_tab2 == "neighbourhood_cleansed"){
      listings_top <- top_n(as.data.frame(sort(table(na.omit(listings_city_tab2_feature()[feature2])), decreasing=T)), 4)
      df <- listings_city_selected_tab2()
      df <- df[df$neighbourhood_cleansed %in% unique(listings_top$Var1)[1:4],]
    } else{
      df <- listings_city_selected_tab2()
    } 
    
    mean <- na.omit(df) %>%
      group_by(!! rlang::sym(input$feature2_tab2)) %>%
      summarize(mean=round(mean(!! rlang::sym(input$feature1_tab2)), 2), .groups = 'keep')
    
    gvisColumnChart(mean, xvar=feature2, yvar='mean', 
                    options = list(width = 550, height = 300, title = glue("Average {strsplit(input$feature1_tab2,'_')[[1]][1]} per {strsplit(input$feature2_tab2,'_')[[1]][1]}")))
  })
  
  output$distribution_multiple_features_tab2 <- renderPlot({
    Sys.sleep(0.2)
    if (is.null(listings_city_selected_tab2()))
      return(NULL)
    
    feature1 <- input$feature1_tab2
    feature2 <- input$feature2_tab2
    
    if (input$feature2_tab2 == "neighbourhood_cleansed"){
      listings_top <- top_n(as.data.frame(sort(table(na.omit(listings_city_tab2_feature()[feature2])), decreasing=T)), 4)
      df <- listings_city_selected_tab2()
      df <- df[df$neighbourhood_cleansed %in% unique(listings_top$Var1)[1:4],]
    } else{
      df <- listings_city_selected_tab2()
    }  
    
    mean <- na.omit(df) %>%
      group_by(!! rlang::sym(input$feature2_tab2)) %>%
      summarize(mean=round(mean(!! rlang::sym(input$feature1_tab2)), 2), .groups = 'keep')


    ggplot(na.omit(df), aes_string(x=feature1)) + 
      geom_density(aes_string(colour = feature2, fill=feature2), alpha = 0.4, stat = "density", size=1) +
      geom_rug(aes_string(color = feature2), alpha = 0.4) +
      scale_x_log10()+
      geom_vline(data = mean, aes_string(xintercept = "mean", color = feature2), size=0.9, linetype="dashed") +
      ggtitle(glue("Distribution of {strsplit(input$feature1_tab2,'_')[[1]][1]} per city")) + 
      ylab("Density") + 
      xlab(glue("{str_replace(input$feature1_tab2,'_', ' ')}"))
    
  })
  
  output$map <- renderGvis({
    Sys.sleep(0.2)
    if (is.null(listings_city_tab2_feature())){
      return(NULL)
    }
    if(input$top_feature > nrow(listings_city_tab2_feature())){
      return(NULL)
    }
    
    gvisMap(sample_n(listings_city_tab2_feature(), input$top_feature), locationvar="latitudelongitude" , tipvar=input$feature1_tab2, 
            options=list(showTip=TRUE, 
                         showLine=TRUE, 
                         enableScrollWheel=TRUE,
                         mapType='terrain', 
                         useMapTypeControl=TRUE))
  })
}

