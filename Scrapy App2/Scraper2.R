library(rvest)
library(tidyverse)
library(httr)
library(gtools)
library(lubridate)
library(stringi)

source('RestaurantScraper.R')
source('HotelsScraper.R')

scraper <- function(link,num_reviews){
  detect_page <- stri_detect_fixed(link,
                                   c("https://www.tripadvisor.ca/Restaurant_Review"))
  # RESTAURANT SCRAPER
  if(detect_page==TRUE){
    df <-restaurant_scraper(link=link,num_reviews = num_reviews)
  }
  # HOTEL SCRAPER
  else{
    df <- hotels_scraper(link=link,num_reviews = num_reviews)
     }
  
  df <- df %>% mutate(rating = as.character(rating)) %>% 
    mutate(rating = replace(rating, rating == '1', 'Terrible')) %>%
    mutate(rating = replace(rating, rating == '2', 'Poor')) %>% 
    mutate(rating = replace(rating, rating == '3', 'Average')) %>%
    mutate(rating = replace(rating, rating == '4', 'Very Good')) %>%
    mutate(rating = replace(rating, rating == '5', 'Excellent')) 
  
  df <- tibble::rowid_to_column(df, "ID")
  
  return(df)
}

