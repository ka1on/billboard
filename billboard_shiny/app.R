library(shiny)
library(shinythemes)
library(scales)
library(tidyverse)
top_20 <- read_rds("top_20.rds")
timelessness_20 <- read_rds("timelessness_20.rds")
timeless_words_graphic <- read_rds("timeless_words_graphic.rds")
diversity_plot <- read_rds("diversity_plot.rds")
density_plot <- read_rds("density_plot.rds")
popular_artists <- read_rds("popular_artists.rds")
sentiment_decades <- read_rds("sentiment_decades.rds")
lyrics_nrc_sub <- read_rds("lyrics_nrc_sub.rds")


ui <- navbarPage(theme = shinytheme("simplex"), "Billboard Top 100 Song Analysis",
                 tabPanel("About"),
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
                              plotOutput("plot1")
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
                                               This can be shown with a simple graph of the average unique words per song over the years."),
                                             br(),
                                             plotOutput("diversity")),
                                    tabPanel("Repetitiveness",
                                             br(),
                                             h4("Lyrics have become more repetitive over time."),
                                             br(),
                                             p("Lexical density is the number of unique words divided by the total number of words. 
                                               This is an indicator of word repetition, which is a critical songwriter's tool."),
                                             br(),
                                             plotOutput("density")),
                                      tabPanel("Sentiment Analysis",
                                               br(),
                                               h4("Lyric sentiments has remained relatively the same over time."),
                                               br(),
                                             sidebarLayout(
                                               sidebarPanel(
                                                 h4("Senti decades"),
                                                 br(),
                                                 p("For this project, we'll measure a song's success by looking at the song's timelessness, 
                                                   or its popularity over time. 
                                                   The graph to the right shows a song's Billboard Weekly Top 100 rank over time 
                                                   and its associated timelessness score."),
                                                 p("The timelessness score is out of 100, with 100 being the most timeless and 0 being 
                                                   the least timeless. Timelessness is calcuated using the area under the rank over time curve, 
                                                   weighted by the total number of weeks spent on the top 100 chart. This score accounts 
                                                   for each song's weekly rank and the number of weeks spent at those ranks, heavily 
                                                   favoring songs that chart for many weeks.")
                                                 ),
                                               
                                               mainPanel(
                                                 plotOutput("decades_nrc"))
                                                 ),
                                             br(),
                                             h4("Lyric sentiments don't vary great in aggregate. However, they vary greatly by song."),
                                             br(),
                                             sidebarLayout(
                                               sidebarPanel(
                                                 h4("Senti per"),
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
                                                 selectInput("Select one of the top 20 most popular Artists.", "Choose an Artist:",
                                                             inputId = "artist",
                                                             choices = popular_artists)
                                                 ),
                                     
                                     mainPanel(
                                       plotOutput("artist_nrc"))
                                     )))
                          ),
                 tabPanel("Predicting Timelessness")
              )

# Define server logic required to draw a histogram
server <- function(input, output) {
   output$plot1 <- renderPlot({
     ## the input should be the song pulled from top_15 df
     # graph weekly song rank over time for the inputted song
     top_20 <- read_rds("top_20.rds")
     top_20 %>%
       filter(song_label == input$song) %>%
       ggplot(aes(x = date, y = Week.Position)) +
       geom_line() +
       scale_y_reverse()
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
       ggtitle("NRC Sentiment Song Analysis") +
       coord_flip()
     
   }, height = 750)
}

# Run the application 
shinyApp(ui = ui, server = server)

