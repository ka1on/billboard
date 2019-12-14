library(shiny)
library(shinythemes)
library(scales)
library(png)
library(tidyverse)
library("htmltools")
library("vembedr")
top_20 <- read_rds("top_20.rds")
timelessness_20 <- read_rds("timelessness_20.rds")
timeless_words_graphic <- read_rds("timeless_words_graphic.rds")
diversity_plot <- read_rds("diversity_plot.rds")
density_plot <- read_rds("density_plot.rds")
popular_artists <- read_rds("popular_artists.rds")
sentiment_decades <- read_rds("sentiment_decades.rds")
lyrics_nrc_sub <- read_rds("lyrics_nrc_sub.rds")
model_table <- read_rds("model_table.rds")


ui <- navbarPage(theme = shinytheme("simplex"), "Billboard Top 100 Song Analysis",
                 tabPanel("About",
                          br(),
                          fluidRow(
                            img(src = "music.png"),
                            align = "center"
                          ),
                          h3("Purpose"),
                          br(),
                          p("As a music lover, I've spent hours pouring over my lyrics and raving about my favorite songs. 
                            While it may seem difficult to quantify the quality of an artform like music, for my project, I sought out to 
                            investigate how a song's lyrics affects its timelessness. 
                            Timelessness is a bit of an abstract concept, but using weekly Billboard charting data, it's 
                            possible to analyze a song's persistence over time. I decided to use text mining and sentiment analysis tools to break down the lyrical content of 
                            popular songs, and analyzed these metrics for songs across the years. Ultimately, I found that repetitive lyrics and a diverse lyrical vocabulary are both significantly 
                            and positively correlated with a song's timelessness, even while controlling for potential confounding variables like 
                            energy, valence, and tempo."),
                          br(),
                          h3("Data"),
                          br(),
                          p("I compiled a variety of data sources in order to complete my analysis. I needed to be able to analyze song popularity over time, 
                            audio features of songs (as control variables), and the lyrics. I used three datasets."),
                          p("The first dataset contains every weekly Hot 100 singles chart from Billboard.com between 8/2/1958 and 6/22/2019. There are 317,175 entries 
                            with details on the song and artist name, rank, date, and total number of weeks charting at that point."),
                          uiOutput("tab"),
                          p("The last dataset contains lyrics of 5701 Billbaord Top 100 Songs from 1960 to 2016. It's important to note that while combining this dataset with the prior 
                            two, I lost quite a few observations because of varying song titles and missing values. Ultimately, the regression is performed on a dataset of 4607 observations. 
                            The lyrical analysis however, breaks out the words for each song, and looks at 39,000+ observations."),
                          br(),
                          h3("About Me"),
                          br(),
                          p("My name is Katie Cao and I'm a junior at Harvard College studying Economics with a secondary in Psychology. I love music and data analysis and my 
                            dream job would be to work for Spotify. I can be reached via email at kcao@college.harvard.edu")
                          ),
                 tabPanel("Timelessness",
                          h3("How can we measure a song's success?", align = "center"),
                          br(),
                          sidebarLayout(
                            sidebarPanel(
                              h4("What is Timelessness?"),
                              br(),
                              p("For this project, we'll measure a song's success by looking at the song's timelessness, 
                              or its popularity over time. 
                              The graph to the right shows a song's Billboard Weekly Top 100 rank over time 
                              and its associated timelessness score."),
                              p("The timelessness score is out of 100, with 100 being the most timeless and 0 being 
                                the least timeless. Timelessness is calcuated using the area under the rank over time curve, 
                                weighted by the total number of weeks spent on the top 100 chart. This score accounts 
                                for each song's weekly rank and the number of weeks spent at those ranks, heavily 
                                favoring songs that chart for many weeks."),
                              br(),
                              selectInput("Select one of the top 20 most timeless songs.", "Choose a Song:",
                                          inputId = "song",
                                          choices = unique(top_20$song_label))
                              ),
                            
                            mainPanel(
                              plotOutput("plot1"),
                              fluidRow(
                                align = "center",
                                h2(textOutput("score"))
                              )

                            ))
                  ),
                 tabPanel("Lyrical Analysis",
                          h3("How have lyrics changed over time?", align = "center"),
                          tabsetPanel(
                                      tabPanel("Timeless Words",
                                              br(),
                                              h4("Some lyrics are persistent over time."),
                                              br(),
                                              p("The graphic below shows the 8 most frequently used words in song lyrics 
                                                for each decade."),
                                              br(),
                                              plotOutput("timeless_words")),
                                    tabPanel("Lexical Diversity",
                                             br(),
                                             h4("Lyrics use an increasing diverse vocalubary over time."),
                                             br(),
                                             p("The more varied a vocabulary a text possesses, the higher its lexical diversity. 
                                               Song Vocabulary is a representation of how many unique words are used in a song. 
                                               This can be shown with a simple graph of the average unique words per song over the years. After fitting both a linear model and 
                                               generalized additive model, the positive correlation between the year of release and lexical diversity is evident."),
                                             br(),
                                             plotOutput("diversity")),
                                    tabPanel("Repetitiveness",
                                             br(),
                                             h4("Lyrics have become more repetitive over time."),
                                             br(),
                                             p("Lexical density is the number of unique words divided by the total number of words, in a range of 0 to 1. 
                                               This is an indicator of word repetition, in that the lower the lexical density, the higher
                                               the repetitiveness of the song lyrics. The graph below a value of repetition found by calculating 1 - the lexical density.
                                               The linear model fitted to the graph shows that song lyrics have become more repetitive over time."),
                                             br(),
                                             plotOutput("density")),
                                      tabPanel("Sentiment Analysis",
                                               br(),
                                               h4("Lyric sentiments has remained relatively the same over time."),
                                               br(),
                                               p("Sentiment analysis is a type of text mining which aims to determine the opinion and subjectivity of its content. 
                                                 When applied to lyrics, the results can be representative of not only the artist's attitudes, but can also reveal 
                                                 pervasive, cultural influences. I used a lexicon-based approach to categorize song lyrics as either positive or negative, 
                                                 and as one of eight categories of emotion."),
                                               br(),
                                             sidebarLayout(
                                               sidebarPanel(
                                                 h4("Lyrical Sentiments by Decade"),
                                                 br(),
                                                 p("The graph to the right shows the total number of words in each category of emotional sentiment 
                                                   for each decade. Differences across the decades are not extremely obvious, but it appears that 
                                                   songs from earlier generations have more joyful lyrics relative to songs from the last decade.")
                                                 ),
                                               mainPanel(
                                                 plotOutput("decades_nrc"))
                                                 ),
                                             br(),
                                             h4("Lyric sentiments don't vary great in aggregate. However, they vary greatly by song."),
                                             br(),
                                             sidebarLayout(
                                               sidebarPanel(
                                                 h4("Lyrical Sentiments by Song"),
                                                 br(),
                                                 p("It's no surprise that comparing aggregated lyrics from hundreds of songs each decade may not show a 
                                                    significant change from decade to decade. However, sentiments do vary significantly from song to song, 
                                                    between songs from the same artist. The graphic to the right compares the sentiments of lyrics from 
                                                    from a selected artist."),
                                                 br(),
                                                 selectInput("Select one of the top 20 most popular Artists.", "Choose an Artist:",
                                                             inputId = "artist",
                                                             choices = popular_artists)
                                                 ),
                                     mainPanel(
                                       plotOutput("artist_nrc"))
                                     )))
                          ),
                 tabPanel("Predicting Timelessness",
                          h4("Model Description", align = "center"),
                          br(),
                          p("This model attempts to predict the timelessness score of a song using the Repetitiveness, Vocabulary Diversity, and Sentiment of the lyrics. 
                            The model uses controls for various song features including the Tempo, Energy, and Valence."),
                          br(),
                          h4("Results", align = "center"),
                          fluidRow(
                            align = "center",
                            htmlOutput("regression")
                          ),
                          br(),
                          p("According to the results of the regression, lyrical repetitiveness and vocabulary diversity are both significant predictors of the timeslessness 
                            of a song. Remember that timelessness is a percentile rank score based on weeks spent on top 100 chart and the associated rank. A song that repeats 
                            every word at least once has a 14.22 higher timelessness score than a song with all unique words. Every additional unique word in the lyrics is 
                            associated with a 0.02 increase in the timelessness score. Though the effect of a higher proportion of positive sentiment words in lyrics is not significant at 
                            the 0.01 level, it is still negatively associated, meaning on average, more timeless songs have more negative sentiments.")
                          )
              )

tags$script(HTML("
                        var p = document.getElementById('regression')
                 $(p).attr('align', 'center');"))
# Define server logic required to draw a histogram
server <- function(input, output) {
   output$plot1 <- renderPlot({
     ## the input should be the song pulled from top_20 df
     # graph weekly song rank over time for the inputted song
     top_20 %>%
       filter(song_label == input$song) %>%
       ggplot(aes(x = date, y = Week.Position)) +
       geom_line() +
       scale_y_reverse() +
       labs(x = "Date (Month-Year)", y = "Weekly Rank", title = paste(input$song, "Billboard Weekly Rank", sep = " :")) +
       theme_classic() + 
       theme(plot.title = element_text(hjust = 0.5)) +
       scale_x_date(labels = date_format("%m-%Y"))
   })
   output$score <- renderText({
     timelessness_20 <- read_rds("timelessness_20.rds")
     score <- timelessness_20 %>%
       filter(song_label == input$song) %>%
       mutate(timelessness_score_rounded = round(timelessness_score, digits = 2)) %>%
       pull(timelessness_score_rounded)
     
     paste("Timelessness Score:", score)
   })
   
   output$timeless_words <- renderPlot({
     timeless_words_graphic
   })
   
   output$diversity <- renderPlot({
     diversity_plot
   })
   
   output$density <- renderPlot({
     density_plot
   })
   
   output$decades_nrc <- renderPlot({
     sentiment_decades
   })
   
   output$artist_nrc <- renderPlot({
     lyrics_nrc_sub %>%
       filter(artist == input$artist) %>%
       count(title, sentiment, decade) %>%
       mutate(sentiment = reorder(sentiment, n), title = reorder(title, n)) %>%
       ggplot(aes(sentiment, n, fill = sentiment)) +
       geom_col() +
       facet_wrap(~title, ncol = 3, scales = "free") +
       theme_lyrics() +
       theme(panel.grid.major.x = element_blank(),
             axis.text.x = element_blank()) +
       labs(x = NULL, y = NULL) +
       ggtitle(paste(input$artist, "NRC Sentiment Lyrical Analysis", sep = " :")) +
       coord_flip()
     
   }, height = 750)
   
   getPage<-function() {
     return(includeHTML("regression.html"))
   }
   output$regression<-renderUI({getPage()})
   
   url <- a("this link", href="https://developer.spotify.com/documentation/web-api/reference/tracks/get-audio-features/")
   output$tab <- renderUI({
     tagList("The second dataset contains values for each of the 28,000+ tracks pulled from the Spotify Web API. A complete and thorough data dictionary can be referenced at", url)
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

