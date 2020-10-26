restaurant_scraper <- function(link,num_reviews){
  link <- link
  # split the link
  split_link <- unlist(strsplit(link,split = '-Reviews'))
  # Parte prima del link
  new_link <- paste(split_link[1],'-Reviews-or',sep= "")

  df <- data.frame()
  dates <- data.frame()
  x = 0
  for (i in 1:num_reviews){
    url <- paste(new_link,x,split_link[2],sep= "")
    x = x + 10
    aff_link <- url %>% read_html()
    
    reviews <- aff_link %>%
      html_nodes(xpath="//p[@class='partial_entry']") %>%
      html_text() %>%
      gsub("\n", "", .) %>%
      gsub("More","",.)
    
    reviews <- reviews[1:10]
    
    rating <- aff_link %>%
      html_nodes(xpath="//div[@class='ui_column is-9']") %>%
      html_nodes('.ui_bubble_rating') %>%
      html_attr('class') %>%
      gsub("ui_bubble_rating bubble_", "", .) %>%
      gsub("0", "", .) %>%
      as.integer()
    
    rating <- rating[1:10]
    
    review_date <- aff_link %>%
      html_nodes(xpath="//div[@class='prw_rup prw_reviews_stay_date_hsx']") %>%
      html_text() %>%
      gsub("Date of visit: ", "", .)
    
    review_date <- review_date[1:10]
    
    if(nrow(df) == 0){
      df <- data.frame(reviews,rating,stringsAsFactors = F)
    } else {
      temp <- df
      df <- rbind(temp, data.frame(reviews,rating,stringsAsFactors = F))
      print(paste(nrow(df),"Total Reviews Scraped"))
    }
    if(nrow(dates)==0){
      dates <- data.frame(review_date[!is.na(review_date)],stringsAsFactors = F)
    }
    else{
      temp_date <- dates
      dates <- rbind(temp_date,data.frame(review_date[!is.na(review_date)],stringsAsFactors = F))
      print(paste(nrow(dates),"Total dates Scraped"))
    }
    if(nrow(dates) != nrow(df)) break
  }
  df <- df[1:nrow(dates),]
  df <- cbind(df,dates)
  df <- df[-which(df$review_date == ""), ]
  names(df) <- c('review_body','rating','review_date')
  return(df)
}




