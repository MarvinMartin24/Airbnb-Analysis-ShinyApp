library(dplyr)
library(stringr)
library(data.table)

extract_all_meta <- function(urls) {
  extract_meta <- function(link) str_split(link, '/', simplify=TRUE)[4:7]
  meta <- sapply(urls$listings_data_url, extract_meta)
  meta <- transpose(data.frame(meta))
  df <- data.frame(meta, urls)
  names(df) <- c("country", "region", "city", "date", "url")
  return(df)
}

download_data <- function(df, countries, last_n_dates) {
  dfs <- list()
  dfs_cal <- list()
  for (country in countries) {
    df_country <- df[df$country == country,]
    df_country <- df_country[order(as.Date(df_country$date, format="%Y/%m/%d")),]
    df_country <- top_n(group_by(df_country, city), last_n_dates)

    dfs <- append(dfs, list(rbindlist(lapply(1:nrow(df_country), function(i) {
      row <- df_country[i,]
      new_df <- fread(row$url)
      new_df$country <- row$country
      new_df$region <- row$region
      new_df$city <- row$city
      new_df$date <- row$date
      return(new_df)
    }), fill=TRUE)))
    
    dfs_cal <- append(dfs_cal, list(rbindlist(lapply(1:nrow(df_country), function(i) {
      url <- df_country[i,]$url
      new_df <- fread(str_replace(url, "listings.csv", "calendar.csv"))
      return(new_df)
    }), fill=TRUE)))
  }
  return(list(listings=dfs, calendars=dfs_cal))
}

# urls <- read.csv(file.path("./data/all_data_urls.csv"))
# df <- extract_all_meta(urls)
# dfs <- download_data(df, c("france", the-netherlands"), 1)

# listings <- dfs$listings
# calendars <- dfs$calendars

# listings_france <- listings[[1]]
# listings_the_netherlands <- listings[[2]]

# calendar_france <- calendars[[1]]
# calendar_the_netherlands <- calendars[[2]]