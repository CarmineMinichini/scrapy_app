library(dplyr)

nrc_lexicon <- read.csv("nrc_real.csv")
nrc_lexicon <- nrc_lexicon[,2:3]

nrc_emotions <- function(df){
  df <- df
  reviews <- df %>% 
    filter(!is.na(review_body)) %>% 
    select(ID, review_body,rating) %>% 
    group_by(row_number()) %>% 
    ungroup()
  
  tidy_reviews <- reviews %>%
    unnest_tokens(word, review_body)
  
  tidy_reviews <- tidy_reviews %>%
    anti_join(stop_words)
  
  nrc_word_counts <- tidy_reviews %>%
    inner_join(nrc_lexicon) %>%
    count(word, sentiment, sort = TRUE) %>%
    ungroup()
  
  return(nrc_word_counts)
}