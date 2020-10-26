hotels_scraper <- function(link,num_reviews)
{
  link <- link
  # split the link
  split_link <- unlist(strsplit(link,split = '-Reviews'))
  # Parte prima del link
  new_link <- paste(split_link[1],'-Reviews-or',sep= "")
  
  dates <- data.frame()
  df <- data.frame()
  x = 0
  for(i in 1:num_reviews){
    url <- paste(new_link,x,split_link[2],sep= "")
    x = x + 5
    
    aff_link <- url %>% read_html()
    
    reviews <- aff_link %>%
      html_nodes(xpath=".//q[@class='IRsGHoPm']") %>%
      html_text() 
    
    rating <- aff_link %>%
      html_nodes(xpath="//div[@class='nf9vGX55']") %>%
      html_nodes('.ui_bubble_rating') %>%
      html_attr('class') %>%
      gsub("ui_bubble_rating bubble_", "", .) %>%
      gsub("0", "", .) %>%
      as.integer()
    
    review_date <- aff_link %>%
      html_nodes(xpath="//span[@class='_34Xs-BQm']") %>%
      html_text() %>%
      gsub("Date of stay: ", "", .)
    
    if (length(reviews)==0) break
    
    if(nrow(df) == 0){
      df <- data.frame(reviews[!is.na(reviews)],rating,stringsAsFactors = F)
    } else {
      temp <- df
      df <- rbind(temp, data.frame(reviews[!is.na(reviews)],rating,stringsAsFactors = F))
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
  difference <-  nrow(df) - nrow(dates)
  df <- df[1:nrow(df)-difference,]
  df <- cbind(df,dates)
  names(df) <- c('review_body','rating','review_date')
  return(df)
}



