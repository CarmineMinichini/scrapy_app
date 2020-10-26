library(radarchart)
library(stringi)
server <- function(input, output, session) {
  
  data_frame <- reactive({
    validate(
      need(input$URL != "", "Please insert a URL")
    )
    link <- input$URL
    n_reviews=input$nreviews
    detect_page <- stri_detect_fixed(link,
                                     c("https://www.tripadvisor.ca/Restaurant_Review"))
    
    df <- scraper(link=link,num_reviews= n_reviews)
    
  })
  
  #DATAFRAME________________________________________________________________
  output$urldataframe <- DT::renderDataTable({
    df <- data_frame()
    period <- mdy(df$review_date,truncated=1)
    firstdate <- format.Date(min(period),"%B %Y")
    lastdate <- format.Date(max(period),"%B %Y")
    datatable(df[,2:4],caption=paste(nrow(df),'reviews between',firstdate,"and",lastdate),
              rownames = FALSE,
              colnames = c('Reviews','Ratings','Date'),options = list(pageLength=1,dom="tip"))
  })
  # Rating distribution________________________________________________________________
  rate_df <- reactive ({
    df <- data_frame()
    df$review_date = mdy(df$review_date,truncated=1)
    year_chosen =input$yearrating
    rate_df <- df %>% mutate(year=format(review_date,"%Y"))%>%
      filter(year==year_chosen) %>% 
      group_by(rating) %>% count()

  })
  
  output$ratingdist <- renderHighchart({
    df_rating <- rate_df()
    
    hc <- df_rating %>%
      hchart("pie", hcaes(x = rating, y = n,color=c("#1a0f00","#995900", "#008066", "#009999",'#003366')),
             name = "Reviews",title='Rating distribution') %>%
      hc_title(text='Rating distribution'
               ,style = list(fontWeight = "10", fontSize = "20px"),
               align = "center") %>%
      hc_plotOptions(series = list(animation=list(duration=3000))) %>%
      hc_tooltip(enabled=T)
    hc
  })
  # Reviews per year________________________________________________________________
  output$reviewperyear <- renderHighchart({
    df <- data_frame()
    df$review_date = mdy(df$review_date,truncated=1)
    rate_chosed <- input$chooserating
    
    df %>% mutate(year=format(review_date,"%Y")) %>% select(year,rating) %>%
      group_by(year,rating) %>% count() %>% filter(rating==rate_chosed) %>% 
      hchart("spline",hcaes(year,n),name="Number of reviews",color="black") %>%
      hc_title(text='Number of reviews per year'
               ,style = list(fontWeight = "10", fontSize = "20px"),
               align = "center") %>%
      hc_yAxis(title=list(text="# of Reviews")) %>%
      hc_plotOptions(series = list(animation=list(duration=3000))) %>%
      hc_tooltip(enabled=T)

  })
  
  # Common Words________________________________________________________________
  output$commonwords <- renderHighchart({
    
    df <- data_frame()

    review_words <- df %>%
      distinct(review_body, .keep_all = TRUE) %>%
      unnest_tokens(word, review_body, drop = FALSE) %>%
      distinct(ID, word, .keep_all = TRUE) %>%
      anti_join(stop_words, by = "word") %>%
      filter(str_detect(word, "[^\\d]")) %>%
      filter(nchar(word)>6) %>%
      group_by(word) %>%
      mutate(word_total = n()) %>%
      ungroup()
    
    word_counts <- review_words %>% count(word, sort = TRUE)
    
    word_counts %>%
      head(20) %>%
      mutate(word = wordStem(word)) %>% 
      mutate(word = reorder(word, n)) %>%
      hchart('bar',hcaes(x=word,y=n),name=" # of uses",
             color = "#008080", borderColor = "black") %>%
      hc_plotOptions(series = list(animation=list(duration=3000))) %>%
      hc_yAxis(title=list(text="# of uses")) %>%
      hc_title(text='Most common words'
               ,style = list(fontWeight = "10", fontSize = "20px"),
               align = "center")
    

  })
  # PosNeg Chart________________________________________________________________
  output$posneg <- renderPlot({
    df <- data_frame()
    
    reviews <- df %>% 
      filter(!is.na(review_body)) %>% 
      select(ID, review_body,rating) %>% 
      group_by(row_number()) %>% 
      ungroup()
    
    tidy_reviews <- reviews %>%
      unnest_tokens(word, review_body)
    
    tidy_reviews <- tidy_reviews %>%
      anti_join(stop_words)
  
    tidy_reviews %>%
      inner_join(get_sentiments("bing")) %>%
      count(word, sentiment, sort = TRUE) %>%
      acast(word ~ sentiment, value.var = "n", fill = 0) %>%
      comparison.cloud(colors = c('grey40','firebrick'),
                       max.words=input$knobwords)
    
  },bg="transparent")
  # Taste Radar Chart________________________________________________________________
  
  output$taste <- renderHighchart({
    df <- data_frame()
    
    nrc_word_counts <- nrc_emotions(df=df)
    
    nrc_radar_chart <- nrc_word_counts %>% filter(!grepl("positive|negative", sentiment))  
    word_tally <- nrc_radar_chart %>% group_by(sentiment) %>% count(wt=n)
    
    highchart() %>%
      hc_chart(polar = T) %>% 
      hc_xAxis(categories = word_tally$sentiment, 
               labels = list(style = list(fontSize= '14px')), title =NULL, tickmarkPlacement = "on", lineWidth = 3) %>% 
      hc_plotOptions(series = list(marker = list(enabled = F),
                                   animation=list(duration=3000))) %>% 
      hc_yAxis(gridLineInterpolation = "polygon", lineWidth = 0, min = 0) %>% 
      hc_add_series(name = "Emotions Score", word_tally$n, type ="area", color="#008080", pointPlacement = "on") %>%
      hc_add_series(name = "Emotions Score", word_tally$n, type ="line", color="black", pointPlacement = "on") %>%
      hc_legend(enabled=F) 

  })
  # WORD NETWORK ________________________________________________________________
  
  output$wordnetwork <- renderPlot({
    df <- data_frame()
    
    review_subject <- df %>% 
      unnest_tokens(word, review_body) %>% 
      anti_join(stop_words)
    
    #my_stopwords <- data_frame(word = c(as.character(1:10)))
    
    #review_subject <- review_subject %>% 
      #anti_join(my_stopwords)
    
    title_word_pairs <- review_subject %>% 
      pairwise_count(word, ID, sort = TRUE, upper = FALSE)
    
    set.seed(1234)
    title_word_pairs %>%
      filter(n >= input$knobn) %>%
      graph_from_data_frame() %>%
      ggraph(layout = "fr") +
      geom_edge_link(aes(edge_alpha = n, edge_width = n), edge_colour = "cyan4") +
      geom_node_point(size = 2) +
      geom_node_text(aes(label = name), repel = TRUE, 
                     point.padding = unit(0.1, "lines")) +
      ggtitle('') +
      theme_void() +
      theme(plot.title = element_text(hjust = 0.5))
    
  },bg="transparent")
  
  # Words evolution ________________________________________________________________
  
  output$searched_words <- renderHighchart({
    df <- data_frame()
    df$review_date <- mdy(df$review_date,truncated=1)
    
    df <- df %>%
      mutate(month = round_date(review_date, "month"))
    
    review_words <- df %>%
      distinct(review_body, .keep_all = TRUE) %>%
      unnest_tokens(word, review_body, drop = FALSE) %>%
      distinct(ID, word, .keep_all = TRUE) %>%
      anti_join(stop_words, by = "word") %>%
      filter(str_detect(word, "[^\\d]")) %>%
      group_by(word) %>%
      mutate(word_total = n()) %>%
      ungroup()
    
    reviews_per_month <- df %>%
      group_by(month) %>%
      dplyr::summarize(month_total = n())
    
    word_month_counts <- review_words %>%
      filter(word_total >= 5) %>%
      count(word, month) %>%
      complete(word, month, fill = list(n = 0)) %>%
      inner_join(reviews_per_month, by = "month") %>%
      mutate(percent = n / month_total) %>%
      mutate(year = format(month,"%b"))
    
    firstword <- tolower(input$first_word)
    secondword <- tolower(input$second_word)
    
    word_month_counts %>%
      filter(word %in% c(firstword, secondword)) %>%
     hchart("spline",hcaes(month,percent,group=word)) %>%
     hc_colors(c("#E87722", "black")) %>%
     hc_title(text='Words in terms of reviewers interest'
              ,style = list(fontWeight = "10", fontSize = "20px"),
              align = "center") %>%
     hc_yAxis(title=list(text="Percentage of reviews containing this term")) %>%
     hc_plotOptions(series = list(animation=list(duration=3000))) %>%
     hc_tooltip(enabled=T)
  })
  
  # TF_IDF________________________________________________________________________
  
  output$tfidf <- renderPlot({
    df <- data_frame()
    
    pop_df <- df %>% 
      unnest_tokens(word,review_body) %>%
      distinct() %>%
      anti_join(stop_words) %>%
      filter(nchar(word)>3) %>%
      count(rating,word,sort=TRUE) %>%
      ungroup() %>%
      bind_tf_idf(word,rating,n)
    
    top_pop_df <- pop_df %>%
      arrange(desc(tf_idf)) %>%
      mutate(word = factor(word, levels = rev(unique(word)))) %>%
      group_by(rating) %>% 
      slice(seq_len(5)) %>%
      ungroup() %>%
      arrange(rating, tf_idf) %>%
      mutate(row = row_number())
    
    top_pop_df %>% 
      mutate(rating_f=factor(rating,levels=c("Terrible","Poor",
                                            "Average","Very Good","Excellent"))) %>%
      ggplot(aes(x = row, tf_idf, 
                 fill = rating_f)) +
      geom_col(show.legend = FALSE) +
      labs(x = NULL, y = "TF-IDF importance") + 
      ggtitle("") +
      facet_wrap(~rating_f, ncol = 2, scales = "free") +
      scale_x_continuous(  # This handles replacement of row 
        breaks = top_pop_df$row, # notice need to reuse data frame
        labels = top_pop_df$word) + coord_flip() +
      scale_y_continuous(labels=comma) +
      theme_minimal()
    
  },bg="transparent")
  
}