library(shiny)
library(shinythemes)
library(scales)
library(tidyverse)
top_20 <- read_rds("top_20.rds")
timelessness_20 <- read_rds("timelessness_20.rds")
timeless_words_graphic <- read_rds("timeless_words_graphic.rds")
diversity_plot <- read_rds("diversity_plot.rds")
density_plot <- read_rds("density_plot.rds")

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
                              selectInput("Select one of the top 20 most timeless songs.", "Choose a Song:",
                                          inputId = "song",
                                          choices = unique(top_20$song_label))
                              ),
                            
                            mainPanel(
                              plotOutput("plot1"),
                              fluidRow(
                                align = "center",
                                h3(textOutput("score")),
                                width = 12
                              )
                            ))
                  ),
                 tabPanel("Lyrical Analysis",
                          h3("How have lyrics changed over time?", align = "center"),
                          tabsetPanel(
                            tabPanel("Timeless Words",
                                    plotOutput("timeless_words")),
                            tabPanel("Lexical Diversity",
                                     plotOutput("diversity")),
                            tabPanel("Lexical Density",
                                     plotOutput("density"))
                            )
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
}

# Run the application 
shinyApp(ui = ui, server = server)

