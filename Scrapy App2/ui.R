# Libraries ####
library(shinydashboard)
library(shiny)
library(plotly)
library(DT)
library(rvest)
library(magrittr)
library(tidyverse)
library(ggplot2)
library(plotly)
library(curl)
#### Libraries pt.2 ####
library(dplyr)
library(readr)
library(lubridate)
library(tidytext)
library(stringr)
library(tidyr)
library(scales)
library(broom)
library(purrr)
library(widyr)
library(igraph)
library(ggraph)
library(SnowballC)
library(wordcloud)
library(reshape2)
library(gridExtra)
library(radarchart)
library(highcharter)
library(shinythemes)
library(grid)
library(widyr)
library(shinyjs)
library(shinyWidgets)
library(shinycustomloader)


source('EmotionLexicon.R')
source('Scraper2.R')
#source('RestaurantScraper.R')
#source('HotelsScraper.R')

Sys.setenv("LANGUAGE"="EN")

marquee_list <- list(marquee("This can take a while...",
                             style="font-size:30px;
                             font-family:Helvetica;
                             font-weight:100",
                             behavior = "slide"))

### APP ####
Sys.setenv(LANG = "en")
navbarPage(title="Scrapy",
           theme = "customize2.css",position = "fixed-top",
           tabPanel("Home",
                    fluidRow(
                      column(8,align="center", offset = 2,
                             # Welcome to TripScraper
                         h1(span(icon("feather-alt",lib='font-awesome')),"Scrapy"),
                         h2("See Instructions to know how Scrapy works"),
                        # Insert URL
                        textInput('URL','',width = "100%",
                                  value=''),
                        # Select number of reviews
                        #radioButtons('nreviews','Number of reviews?',list('200'=40,'500'=100,'1000'=200),
                                     #selected=200,inline=T),
                        prettyRadioButtons(
                          inputId = "nreviews",
                          label = "Number of reviews?", 
                          choices = c("200"=40, "500"=100, "1000"=200,"2000"=400,"5000"=1000,"7000"=1400),
                          icon = icon("check"), 
                          bigger = TRUE,
                          status = "primary",
                          animation = "jelly",inline = T,selected = 1000
                        ),
                        # display dataframe
                        withLoader(dataTableOutput('urldataframe',width = "100%"),type='text',
                                   loader = marquee_list),
                        # horizontal line
                        hr(),
                      ),
                      column(6,
                        # display rating dist
                        prettyRadioButtons(
                          inputId = "yearrating",
                          label = "Choose Year",
                          choices = c("2020", "2019", "2018", "2017"),
                          status = "danger",
                          inline=T,
                          fill=T,
                          selected = "2019",
                        ),
                        highchartOutput('ratingdist') %>% withLoader(type="html",
                                                                     loader='loader9'),
                        # horizontal line
                        ),
                      column(6,
                             radioGroupButtons(
                               inputId = "chooserating",
                               label = "Choose Rating :", 
                               choices = c("Terrible" = "Terrible", 
                                           "Poor" = "Poor", 
                                           "Average" = "Average",
                                           "Very Good" = "Very Good",
                                           "Excellent" = "Excellent"),
                               justified = T,
                               size="normal"
                             ),
                             highchartOutput('reviewperyear') %>% withLoader(type="html",
                                                                             loader='loader9')
                             ),
                      column(8,align="center",offset=2,
                             hr()
                             ),
                      column(6,
                        # most common words
                        highchartOutput('commonwords') %>% withLoader(type="html",
                                                                      loader='loader9'),
                      ),
                      column(6,
                             h2('Most common words?'),
                             h3('After removed stopwords(useless words) and stemmed we can see the most common words in
                               reviews.Stemming is the process of reducing inflected (or sometimes derived) 
                                words to their word stem, base or root format.')
                             ),
                      column(8,align="center",offset=2,
                             hr(),
                             ),
                      column(6,align="center",
                             h2('Which words influence the most Negative/Positive sentiment in reviews?'),
                             h3("Comparison cloud measure the rate at which word occurs compared to the average across 
                                documents.
                                The size of each word is mapped to its maximum deviation,
                                and its angular position is determined by the document where that maximum occurs."),
                             knobInput(
                               inputId = "knobwords",
                               label = "Choose how many words include",
                               value = 100,
                               min = 50,
                               max=300,
                               displayPrevious = TRUE,
                               lineCap = "round",
                               fgColor = "#008080",
                               inputColor = "black",
                               post=" words",
                               thickness=0.1,
                               fontSize= "20px"
                             ),
                             ),
                      column(6,
                             plotOutput("posneg",width="500px",height="600px") %>% withLoader(type="html",
                                                                                              loader='loader9')
                             ),
                      column(8,align="center",offset=2,
                             hr(),
                      ),
                      column(6,
                             highchartOutput("taste",width="600px",height = "400px") %>% withLoader(type="html",
                                                                                                    loader='loader9')
                             ),
                      column(6,
                             h2("Which emotion is the most common?"),
                             h3("The nrc lexicon categorizes words in a binary fashion (“yes”/“no”) 
                             based on eight basic emotions: anger,
                                anticipation, disgust, fear, joy, sadness, surprise, and trust."),
                             ),
                      column(8,align="center",offset=2,
                             hr()
                             ),
                      column(6,align="center",
                             h2("What words have the strongest relationship with each other?"),
                             h3("We want to understand the relationship between words in reviews. 
                                What sequences of words are common across review text?
                                Given a sequence of words, what word is most likely to follow? 
                                We can visualize bigrams(two consecutive words) in word networks:"),
                             knobInput(
                               inputId = "knobn",
                               label = "Choose how many times bigrams occurs",
                               value = 10,
                               min = 5,
                               max= 1500,
                               displayPrevious = TRUE,
                               lineCap = "round",
                               fgColor = "#E87722",
                               inputColor = "black",
                               pre ="≥ ",
                               post=" times",
                               thickness=0.1,
                               fontSize= "16px",
                               step=5
                             ),
                             ),
                      column(6,
                             plotOutput("wordnetwork") %>% withLoader(type="html",
                                                                loader='loader9'),
                             ),
                      column(8,align="center",offset=2,
                             hr()
                             ),
                      column(8,align="center",offset=2,
                             h2("Explore words behaviour over time"),
                             searchInput(
                               inputId = "first_word",
                               label = "hit 'Enter' to update the graph ", 
                               placeholder = "Type a word",
                               value="service",
                               btnSearch = icon("search"), 
                               btnReset = icon("remove"),
                               width = "50%"
                             ),
                             searchInput(
                               inputId = "second_word",
                               label = NULL, 
                               placeholder = "Type a word",
                               value = "food",
                               btnSearch = icon("search"), 
                               btnReset = icon("remove"),
                               width = "50%"
                             ),
                             highchartOutput('searched_words') %>% withLoader(type="html",
                                                                              loader='loader9')
                             ),
                      column(8,align="center",offset=2,hr()),
                      column(6,
                             h2("How important is a word?"),
                             h3("
                                The statistic tf-idf is intended to measure how important a word is to a document in 
                                a collection (or corpus)
                                of documents, for example,
                                to one novel in a collection of novels or to one reviews in a collection of reviews.
                                TF-IDF is a combination of term frequency (tf) and inverse document frequency(idf) 
                                this combination measure the frequency of a term adjusted for how rarely it is used.")
                             ),
                      column(6,
                             plotOutput("tfidf") %>% withLoader(type="html",
                                                                 loader='loader9')
                             )
                      )# fluidRow
           ),# tabPanel Home
           tabPanel(id="Instructions","Instructions",
                    column(8,align="center",offset=2,
                           hr(),
                           h3('Scrapy provides a text and sentiment analysis only on english hotel reviews,
                               To ensure a correct analysis be sure that your TripAdvisor link is setted on
                              English Language, anyway you can solve typing .ca  or .com as domain on your link.
                              Only Hotel/Restaurant pages are admitted. See example: '),
                           h4("https://www.tripadvisor.ca/Hotel_Review-g187791-d233379-Reviews-Grand_Hotel_Plaza-Rome_Lazio.html"),
                           h3("Scrapy's algorithm is setted to take only reviews that matched with a 
                              Date of Stay as provided by TripAdvisor in order to ensure an extensive and
                              complete analysis, so if you have selected 5000 reviews it\'s possible
                              that algorithm reach less reviews that correctly match with date of stay.
                              It's recommended to check number of English reviews on link before proceeding 
                              with analysis.
                              Scrapy can be a bit slow when you select over 1000 reviews, so please be patient.
                              Scrapy is under development,for some hotels the analysis could be incomplete."),
                           h2("Enjoy your Analysis!"),
                           h2(span(icon("feather-alt",lib='font-awesome'))),
                           hr(),
                           
                           ),
           ),
           tabPanel("About",
                    column(8,align="center",offset=2,
                    hr(),
                    h3("ScrapyApp is an open-source free application for sentiment analysis in TripAdvisor
                    Hotel/Restaurant reviews.
                       All the work was made up by me. 
                       You can find more applications and works on:"),
                    a("My Github",href = "https://github.com/CarmineMinichini"),
                    h3("Thank you for coming here!"),
                    h1(span(icon("smile",lib='font-awesome'))),
                    hr()
                    ),
                    )
           )# navbar Page