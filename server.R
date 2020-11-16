library(shiny)

listings <- read.csv(file.path("./data/global_listings.csv"))

server <- function(input, output) {

  
  output$draw <- renderPlot({
    mean_availability_30 <- listings %>%
      filter(city %in% input$var) %>%
      group_by(city) %>%
      summarize(mean=round(mean(availability_30),2), .groups = 'keep')
    
    ggplot(filter(listings, city %in% input$var), aes(x=availability_30, color=city, fill=city)) + 
    geom_density(aes(color = city),alpha=0.1, size=1) +
    scale_x_continuous(limits = c(1, 40)) +
    geom_vline(data = mean_availability_30, aes(xintercept = mean, color = city), size=1, linetype="dashed") +
    geom_text(data = mean_availability_30, aes(x=mean, y=0, label=mean, vjust = -0.5, hjust = 0, angle = 90)) +
    ggtitle("Average availability of over 30 days for each city") + 
    ylab("Density") +
    xlab("Availability (Days)")
  })
}

