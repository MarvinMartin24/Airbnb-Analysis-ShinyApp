library(dplyr)
library(stringr)
library(data.table)
library(glue)

extract_all_meta <- function(urls) {
  extract_meta <- function(link) str_split(link, '/', simplify=TRUE)[4:7]
  meta <- sapply(urls$listings_data_url, extract_meta)
  meta <- transpose(data.frame(meta))
  df <- data.frame(meta, urls)
  names(df) <- c("country", "region", "city", "date", "url")
  return(df)
}

prepare_data <- function(listings_city, calendar_city) {
  city <- unique(listings_city$city)
  date <- unique(listings_city$date)
  
  listings_city <- listings_city %>%  
    mutate(id = as.numeric(id))
  
  calendar_city <- calendar_city %>%  
    mutate(listing_id = as.numeric(listing_id))
  
  print(glue("Preparing {city} at {date}..."))
  
  columns_listings <- c("id","country", "region", "city","date", "neighbourhood_cleansed", "latitude", "longitude", 
                        "property_type", "room_type", "accommodates", "bedrooms", "beds", "price", "minimum_nights",  "maximum_nights", "review_scores_rating")
  
  listings_city <- listings_city %>% 
    select(columns_listings) %>% 
    arrange(id)
  
  listings_city$bedrooms <- ifelse(listings_city$bedrooms >= 5, "5+", listings_city$bedrooms)
  
  calendar_city <- calendar_city %>% 
    arrange(listing_id, date)
  
  calendar_city <- calendar_city %>%
    group_by(listing_id) %>%
    mutate(day_nb = row_number()) %>%
    ungroup()
  
  calendar_city <- calendar_city %>%
    mutate(available = ifelse(available=="t", 1, 0))
  
  calendar_city <- calendar_city %>%
    mutate(price = str_replace(price, "\\$", ""),
           adjusted_price = str_replace(adjusted_price, "\\$", ""))
  calendar_city <- calendar_city %>%
    mutate(price = str_replace(price, ",", ""),
           adjusted_price = str_replace(adjusted_price, ",", ""))
  calendar_city <- calendar_city %>%
    mutate(price = as.numeric(price),
           adjusted_price = as.numeric(adjusted_price))
  
  calendar_city <- calendar_city %>%
    mutate(revenue = price*(1-available))
  
  calendar_city <- calendar_city %>%
    group_by(listing_id) %>%
    summarise(availability_30 = sum(available[day_nb<=30], na.rm = TRUE),
              price_30 = mean(price[day_nb<=30 & available==0], na.rm = TRUE),
              revenue_30 = sum(revenue[day_nb<=30], na.rm = TRUE),
              .groups = "keep")
  
  
  listings_cleansed <- listings_city %>% left_join(calendar_city, by = c("id" = "listing_id"))
  
  write.csv(listings_cleansed, file.path("./data/cities/", glue("listing_{city}_{date}.csv")))
}

download_data <- function(df, countries, last_n_dates) {

  
  for (country in countries) {
    
    df_country <- df[df$country == country,]
    df_country <- df_country[order(as.Date(df_country$date, format="%Y/%m/%d")),]
    df_country <- top_n(group_by(df_country, city), last_n_dates)
    
    lapply(1:nrow(df_country), function(i) {
                                 print(glue("{i}/{nrow(df_country)}"))
                                 
                                 row <- df_country[i,]
                                 
                                 listings <- fread(row$url)
                                 listings$country <- row$country
                                 listings$region <- row$region
                                 listings$city <- row$city
                                 listings$date <- row$date
                                 
                                 calendars <- fread(str_replace(row$url, "listings.csv", "calendar.csv"))
                                 calendars$country <- row$country
                                 calendars$region <- row$region
                                 calendars$city <- row$city
                                 calendars$date <- row$date
                                 
                                 prepare_data(listings, calendars)
                                }
       )
  }
}

load_global_listings <- function(){
  files  <- list.files(file.path("./data/cities/"), pattern = '\\.csv')
  tables <- lapply(file.path("./data/cities/",files), read.csv, header = TRUE)
  final_df <- do.call(rbind , tables)
  final_df$X <- NULL
  write.csv(final_df, file.path("./data/",  "global_listings.csv"), row.names=FALSE)
}

#countries <- c("france", "spain", "the-netherlands", "germany", "belgium","italy") #
#urls <- read.csv(file.path("./data/all_data_urls.csv"))
#df <- extract_all_meta(urls)
#download_data(df, countries, 3)

# Final cmd
load_global_listings()


