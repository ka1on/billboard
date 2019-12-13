library(httr)
library(readxl)
library(lubridate)
library(gridExtra) #viewing multiple plots together
library(tidytext) #text mining
library(wordcloud2) #creative visualizations
library(scales)
library(billboard)
library(mgcv)
library(MASS)
library(tidyverse)

# Billboard data source: https://data.world/kcmillersean/billboard-hot-100-1958-2017

# Load in billboard weekly top 100 songs rank data

billboard <- read.csv("https://query.data.world/s/tpw4liiutfaztvftwockqqhu44yssi", header=TRUE, stringsAsFactors=FALSE) %>%
  mutate(date = mdy(WeekID), song_label = paste(Song, Performer, sep = " by "))

# Load in spotify features of songs data

GET("https://query.data.world/s/mulps2bjunbt2g476gsrgwqjib5dnb", write_disk(tf <- tempfile(fileext = ".xlsx")))
features <- read_excel(tf)


# Create a timelessness score based on number of weeks spent in weekly top 100 and the rank.
# A natural way to score a single’s success on the charts would be to take an integral to find the area
# under the curves we’ve drawn.  I decided only to count areas where the single was in the top 10.
# In the below graph, the area of the region above the horizontal line would be found to give the score
# (more precisely, I took the sum of max(0, 11-chart rank) for each single over all weeks):

timelessness <- billboard %>%
  group_by(song_label, SongID) %>%
  summarize(timelessness_score_raw = sum((101 - Week.Position)*Weeks.on.Chart)) %>%
  
  # Scale the score out of 100, a percentile rank
  
  mutate(timelessness_score = (timelessness_score_raw/293883)*100) %>%
  arrange(desc(timelessness_score))

# Take top 20 timeless songs for graphic
timelessness_20 <- timelessness %>%
  ungroup() %>%
  slice(1:20)

# Add in the timelessness score to the billboard rank data for the top 20 songs
top_20 <- timelessness_20 %>%
  inner_join(billboard, by = "song_label")

# join timelessness score data with song feature data by songID
data <- timelessness %>%
  inner_join(billboard, by = "SongID") %>%
  inner_join(features, by = "SongID") %>%
  rename(song_label = song_label.x)

# The lyrics dataset is only 1960-2016, so the dataset will be limited to those.
#Lyrics from songs on Billboards Hot 100 from 1960 to 2016

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

#make lower case
lyrics_clean$lyrics <- sapply(lyrics_clean$lyrics, tolower)

#create the decade column
lyrics_clean <- lyrics_clean %>%
  mutate(decade = 
           ifelse(year %in% 1960:1969, "1960s",
             ifelse(year %in% 1970:1979, "1970s", 
                    ifelse(year %in% 1980:1989, "1980s", 
                           ifelse(year %in% 1990:1999, "1990s", 
                                  ifelse(year %in% 2000:2009, "2000s", 
                                         ifelse(year %in% 2010:2016, "2010s", 
                                                "NA"))))))) %>%
  mutate(song_label = paste(title, artist, sep = " by ")) %>%
  mutate(year = as.numeric(year))

lyrics_data <- lyrics_clean %>%
  inner_join(timelessness, by = "song_label")

# lyrics data only has 2708 variables

# now for the text mining/analytics, this is using the lyrics data alone, without looking at rank, so far
# we will look at changes over time

# we want to measure lexical complexity
# we will break out individual lyrics using tokenization

undesirable_words <- c("chorus", "repeat", "lyrics", "hook", "intro", "pre",
                       "theres", "bridge", "fe0f", "yeah", "baby", 
                       "alright", "wanna", "gonna", "chorus", "verse", 
                       "whoa", "gotta", "make", "miscellaneous", "1", "2", 
                       "3", "4", "ooh", "uurh", "pheromone", "poompoom", "3121", 
                       "matic", " ai ","ai", "outro", " ca ", " la ", "hey", " na ", 
                       " da ", " uh ", " tin ", "  ll", "transcription",
                       "repeats")
#unnest and remove stop, undesirable words
lyrics_filtered <- lyrics_clean %>%
  unnest_tokens(word, lyrics) %>%
  anti_join(stop_words) %>%
  distinct() %>%
  filter(!word %in% undesirable_words)

my_colors <- c("#E69F00", "#56B4E9", "#009E73", "#CC79A7", "#D55E00")
theme_lyrics <- function() 
{
  theme(plot.title = element_text(hjust = 0.5),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none")
}
# Frequently used words over time

timeless_words <- lyrics_filtered %>% 
  filter(decade != 'NA') %>%
  group_by(decade) %>%
  count(word, decade, sort = TRUE) %>%
  slice(seq_len(8)) %>%
  ungroup() %>%
  arrange(decade,n) %>%
  mutate(row = row_number()) 

timeless_words_graphic <- timeless_words %>%
  ggplot(aes(row, n, fill = decade)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "Song Count") +
  ggtitle("Timeless Words") + 
  theme_lyrics() +  
  facet_wrap(~decade, scales = "free", ncol = 6) +
  scale_x_continuous(  # This handles replacement of row 
    breaks = timeless_words$row, # notice need to reuse data frame
    labels = timeless_words$word) +
  coord_flip()

#lexical density by time and then timelessness by lexical density

lyrics_clean %>%
  count(artist) %>%
  arrange(desc(n))

lex_diversity_per_year <- lyrics_clean %>%
  unnest_tokens(word, lyrics) %>%
  group_by(song_label, year) %>%
  summarise(lex_diversity = n_distinct(word)) %>%
  arrange(desc(lex_diversity))

diversity_plot <- lex_diversity_per_year %>%
  ggplot(aes(year, lex_diversity)) +
  geom_point(color = my_colors[3],
             alpha = .4, 
             size = 1, 
             position = "jitter") + 
  stat_smooth(formula = y ~ x, color = "black", se = FALSE, method = "lm") +
  geom_smooth(color = "blue") +
  ggtitle("Lexical Diversity") +
  xlab("Year") + 
  ylab("Lexical Diversity") +
  scale_color_manual(values = my_colors) +
  theme_classic() + 
  theme_lyrics()

diversity_plot

lex_density_per_year <- lyrics_clean %>%
  unnest_tokens(word, lyrics) %>%
  group_by(song_label,year) %>%
  summarise(lex_density = n_distinct(word)/n()) %>%
  arrange(desc(lex_density))

density_plot <- lex_density_per_year %>%
  ggplot(aes(year, lex_density)) +
  geom_point(color = my_colors[3],
             alpha = .4, 
             size = 1, 
             position = "jitter") + 
  stat_smooth(formula = y ~ x, color = "black", se = FALSE, method = "lm") +
  geom_smooth(color = "blue") +
  ggtitle("Lexical Density") +
  xlab("Year") + 
  ylab("Lexical Density") +
  scale_color_manual(values = my_colors) +
  theme_classic() + 
  theme_lyrics()

density_plot

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




write_rds(top_20, "billboard_shiny/top_20.rds")
write_rds(timelessness_20, "billboard_shiny/timelessness_20.rds")
write_rds(timeless_words_graphic, "billboard_shiny/timeless_words_graphic.rds")
write_rds(diversity_plot, "billboard_shiny/diversity_plot.rds")
write_rds(density_plot, "billboard_shiny/density_plot.rds")