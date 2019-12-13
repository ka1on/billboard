library(shiny)
library(tidyverse)
top_15 <- write_rds(top_15, "top_15.rds")

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Old Faithful Geyser Data"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        selectInput("selection", "Choose a Song:",
                    inputId = "song",
                    choices = unique(top_15$song_label))
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("plot1")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   output$plot1 <- renderPlot({
     ## the input should be the song pulled from top_15 df
     # graph weekly song rank over time for the inputted song
     top_15 %>%
       filter(song_label == input$song) %>%
       ggplot(aes(x = date, y = Week.Position)) +
       geom_line() +
       scale_y_reverse()
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

