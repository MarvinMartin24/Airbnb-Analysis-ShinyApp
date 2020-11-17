library(dplyr)
library(stringr)
library(data.table)
library(lubridate)

extract_all_meta <- function(urls) {
  extract_meta <- function(link) str_split(link, '/', simplify=TRUE)[4:7]
  meta <- sapply(urls$listings_data_url, extract_meta)
  meta <- transpose(data.frame(meta))
  df <- data.frame(meta, urls)
  names(df) <- c("country", "region", "city", "date", "url")
  return(df)
}

#get_df_from_url <- function(urls, cities, dates){
#  data <- fread(urls)
#  data$city <- cities
#  data$date <- dates
#  return(data)
#}

download_data <- function(df, countries, last_n_dates) {
  dfs <- list()
  dfs_cal <- list()
  for (country in countries) {
    df_country <- df[df$country == country,]
    df_country <- df_country[order(as.Date(df_country$date, format="%Y/%m/%d")),]
    df_country <- top_n(group_by(df_country, city), last_n_dates)
    
    urls <- df_country$url
    #test <- apply(urls, get_df_from_url,  cities = df_country$city, dates = df_country$date)
    #dfs <- append(dfs, list(rbindlist(test, fill=TRUE)))
    dfs <- append(dfs, list(rbindlist(lapply(urls, function(url) fread(url)), fill=TRUE)))    
    urls_cal <- lapply(urls, function(url) str_replace(url, "listings.csv", "calendar.csv"))
    dfs_cal <- append(dfs_cal, list(rbindlist(lapply(urls_cal, function(url) fread(url)), fill=TRUE)))
  }
  return(list(listings=dfs, calendars=dfs_cal))
}

prepare_data <- function(country_name, listings, calendar)
{
  print(country_name)
  print(unique(listings$city))
  
  all_cities_listing = c()
  
  #################### For each city of a given country ###################
  for (city in unique(listings$city)) {
    
    listings$country <- country_name
    columns <- c("city", "id", "neighbourhood_cleansed", "calendar_last_scraped", "latitude", "longitude", 
                  "property_type", "room_type", "accommodates", "bedrooms", "beds", "price", "minimum_nights",  "maximum_nights", "review_scores_rating")
    
    listings <- listings %>% 
      select(columns) %>% 
      arrange(id)
    
    listings$bedrooms <- ifelse(listings$bedrooms >= 5, "5+", listings$bedrooms)
    
    ####################### Clean Calendar ###################
    calendar <- calendar %>% 
      arrange(listing_id, date) %>% 
      mutate(available = ifelse(available=="t", 1, 0))%>% 
      mutate(price = str_replace(price, "\\$", ""), 
             adjusted_price = str_replace(adjusted_price, "\\$", "")) %>% 
      mutate(price = str_replace(price, ",", ""),
             adjusted_price = str_replace(adjusted_price, ",", "")) %>% 
      mutate(price = as.numeric(price),
             adjusted_price = as.numeric(adjusted_price))  %>% 
      mutate(revenue = price*(1-available))
      
    calendar <- calendar %>%
      group_by(listing_id) %>%
      mutate(day_nb = row_number()) %>%
      ungroup()
    
    calendar <- calendar %>%
      group_by(listing_id) %>%
      summarise(availability_30 = sum(available[day_nb<=30], na.rm = TRUE),
                price_30 = mean(price[day_nb<=30 & available==0], na.rm = TRUE),
                revenue_30 = sum(revenue[day_nb<=30], na.rm = TRUE))
    
    listings_cleansed <- listings %>% left_join(calendar, by = c("id" = "listing_id"))
    all_cities_listing <- c(all_cities_listing, listings_cleansed)
  }
  return(all_cities_listing)
}

#countries <- c("france", "the-netherlands")
#urls <- read.csv(file.path("./data/all_data_urls.csv"))
#df <- extract_all_meta(urls)
#fs <- download_data(df, countries, 1)

#all_countries_listing = c()
#for(i in 1:length(countries)){
#  country_name <- countries[i]
#  listings <- fs$listings[[i]]
#  calendars <- fs$calendars[[i]]
#  new_listing <- prepare_data(country_name, listings, calendars)
#  all_countries_listing = c(all_countries_listing, new_listing)
#}


###############################################################
