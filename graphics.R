library(httr)
library(readxl)
library(lubridate)
library(gridExtra) #viewing multiple plots together
library(tidytext) #text mining
library(wordcloud2) #creative visualizations
library(tidyverse)

# from https://data.world/kcmillersean/billboard-hot-100-1958-2017

GET("https://query.data.world/s/mulps2bjunbt2g476gsrgwqjib5dnb", write_disk(tf <- tempfile(fileext = ".xlsx")))
features <- read_excel(tf)

# load in billboard weekly rank data

billboard <- read.csv("https://query.data.world/s/tpw4liiutfaztvftwockqqhu44yssi", header=TRUE, stringsAsFactors=FALSE) %>%
  mutate(date = mdy(WeekID), song_label = paste(Song, Performer, sep = " by "))

# create a timelessness score based on number of weeks spent in weekly top 100 and the rank.
# A natural way to score a single’s success on the charts would be to take an integral to find the area
# under the curves we’ve drawn.  I decided only to count areas where the single was in the top 10.
# In the below graph, the area of the region above the horizontal line would be found to give the score
# (more precisely, I took the sum of max(0, 11-chart rank) for each single over all weeks):

timelessness <- billboard %>%
  filter(Week.Position < 11) %>%
  group_by(song_label, SongID) %>%
  summarize(timelessness_score = sum(11 - Week.Position)) %>%
  arrange(desc(timelessness_score))

# take top 15 timeless songs for graphic
timelessness_15 <- timelessness %>%
  ungroup() %>%
  slice(1:15)

# top 15 data
top_15 <- timelessness_15 %>%
  inner_join(billboard, by = "song_label")


write_rds(top_15, "billboard_shiny/top_15.rds")

# join timelessness score data with song feature data by songID
data <- timelessness %>%
  inner_join(features, by = "SongID")

#can i print out the timelessness score..?

# Lyric analysis using NLP and Machine Learning

# first we clean the data
data(lyrics)
lyrics_clean <- lyrics %>%
  filter(!is.na(lyrics))

fix.contractions <- function(doc) {
  # "won't" is a special case as it does not expand to "wo not"
  doc <- gsub("won't", "will not", doc)
  doc <- gsub("can't", "can not", doc)
  doc <- gsub("n't", " not", doc)
  doc <- gsub("'ll", " will", doc)
  doc <- gsub("'re", " are", doc)
  doc <- gsub("'ve", " have", doc)
  doc <- gsub("'m", " am", doc)
  doc <- gsub("'d", " would", doc)
  # 's could be 'is' or could be possessive: it has no expansion
  doc <- gsub("'s", "", doc)
  return(doc)
}

# fix (expand) contractions
lyrics_clean$lyrics <- sapply(lyrics_clean$lyrics, fix.contractions)

# function to remove special characters
removeSpecialChars <- function(x) gsub("[^a-zA-Z0-9 ]", " ", x)
# remove special characters
lyrics_clean$lyrics <- sapply(lyrics_clean$lyrics, removeSpecialChars)
lyrics_clean$lyrics <- sapply(lyrics_clean$lyrics, tolower)

# now for the text mining/analytics
# we want to measure lexical complexity
# we will break out individual lyrics using tokenization
head(sample(stop_words$word, 15), 15)
undesirable_words <- c("chorus", "repeat", "lyrics", 
                       "theres", "bridge", "fe0f", "yeah", "baby", 
                       "alright", "wanna", "gonna", "chorus", "verse", 
                       "whoa", "gotta", "make", "miscellaneous", "2", 
                       "4", "ooh", "uurh", "pheromone", "poompoom", "3121", 
                       "matic", " ai ", " ca ", " la ", "hey", " na ", 
                       " da ", " uh ", " tin ", "  ll", "transcription",
                       "repeats")
#unnest and remove stop, undesirable
lyrics_filtered <- lyrics_clean %>%
  unnest_tokens(word, lyrics) %>%
  anti_join(stop_words) %>%
  distinct() %>%
  filter(!word %in% undesirable_words)

#maybe have the frequently used words by decade? have a bar plot and a wordcloud? timeless words?

#lexical density by time and then timelessness by lexical density

lex_diversity_per_year <- lyrics_clean %>%
  unnest_tokens(word, lyrics) %>%
  group_by(title,year) %>%
  summarise(lex_diversity = n_distinct(word)) %>%
  arrange(desc(lex_diversity)) 

lex_density_per_year <- lyrics_clean %>%
  unnest_tokens(word, lyrics) %>%
  group_by(title,year) %>%
  summarise(lex_density = n_distinct(word)/n()) %>%
  arrange(desc(lex_density))

data <- data %>%
  inner_join(lex_density_per_year, by = c("Song" = "title"))

data <- data %>%
  inner_join(lex_diversity_per_year, by = c("Song" = "title"))

data <- data %>%
  drop_na()

model <- lm(data, formula = timelessness_score ~ lex_density + lex_diversity + danceability)
tidy(model)


data %>%
  ggplot(aes(x = lex_density, y = timelessness_score)) +
  geom_point() +
  geom_smooth(formula = timelessness_score ~ lex_density, method = "glm")

data %>%
  ggplot(aes(x = danceability, y = timelessness_score)) +
  geom_point() +
  geom_smooth(method = "glm", formula = timelessness_score ~ danceability)