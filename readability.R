library(quanteda)
library(quanteda.textstats)
library(dplyr)
library(ggplot2)
library(tidyverse)

setwd("C:/Users/Th3RapidK1ller/Documents/LyricalEvolutionAnalysis")

# Read in the text
data_sample <- read.csv("data_sample.csv", header = TRUE)

# Create a corpus
corpus <- corpus(data_sample$lyrics)

# Calculate readability scores
readability <- textstat_readability(corpus, measure = c("Flesch.Kincaid", "SMOG"))

# Merge readability scores with the existing dataset
data_with_readability <- cbind(data_sample, readability)
word_counts <- textstat_summary(corpus, cache = TRUE)$tokens
data_with_readability <- cbind(data_with_readability, word_counts)

# Plotting
ggplot(data_with_readability, aes(x = decade, y = Flesch.Kincaid)) +
  geom_line(stat = "summary", fun = "mean", color = "blue") +
  geom_point(stat = "summary", fun = "mean", color = "red", size = 2) +
  labs(title = "Readability Evolution Over Decades",
       x = "Decade",
       y = "Average Flesch-Kincaid Readability") +
  theme_minimal()

ggplot(data_with_readability, aes(x = decade, y = SMOG)) +
  geom_line(stat = "summary", fun = "mean", color = "blue") +
  geom_point(stat = "summary", fun = "mean", color = "red", size = 2) +
  labs(title = "Readability Evolution Over Decades",
       x = "Decade",
       y = "SMOG") +
  theme_minimal()

# Plotting with number of songs and average readability
ggplot(data_with_readability, aes(x = decade, y = Flesch.Kincaid)) +
  geom_col(aes(fill = "Number of Songs"), position = "dodge") +
  geom_line(stat = "summary", fun = "mean", color = "blue", size = 1.5, aes(group = 1)) +
  geom_point(stat = "summary", fun = "mean", color = "red", size = 3) +
  labs(title = "Readability and Number of Songs Over Decades",
       x = "Decade",
       y = "Average Flesch-Kincaid Readability") +
  scale_fill_manual(values = c("Number of Songs" = "#009E73")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# select the most hardest and easiest song to read
hardest_song_flesch <- data_with_readability %>%
  arrange(desc(Flesch.Kincaid)) %>%
  select(artist, title, Flesch.Kincaid, word_counts) %>%
  head(2)

hardest_song_smog <- data_with_readability %>%
  arrange(desc(SMOG)) %>%
  select(artist, title, SMOG, word_counts) %>%
  head(2)

easiest_song_flesch <- data_with_readability %>%
  arrange(Flesch.Kincaid) %>%
  select(artist, title, Flesch.Kincaid, word_counts) %>%
  head(2)

easiest_song_smog <- data_with_readability %>%
  arrange(SMOG) %>%
  select(artist, title, SMOG, word_counts) %>%
  head(2)



# make a similar plot but this time also group by genre


ggplot(data_with_readability, aes(x = decade, y = Flesch.Kincaid)) +
  geom_col(aes(fill = tag), position = "dodge") +
  geom_line(stat = "summary", fun = "mean", color = "blue", size = 1.5, aes(group = 1)) +
  geom_point(stat = "summary", fun = "mean", color = "red", size = 3) +
  labs(title = "Readability and Number of Songs Over Decades (grouped by genre)",
       x = "Decade",
       y = "Average F.K") +
  scale_fill_manual(values = c("rock" = "#009E73", "pop" = "#F0E442", "rap" = "#0072B2", "rb" = "#D55E00", "country" = "#CC79A7", "misc" = "#56B4E9")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



ggplot(data_with_readability, aes(x = decade, y = SMOG)) +
  geom_col(aes(fill = tag), position = "dodge") +
  geom_line(stat = "summary", fun = "mean", color = "blue", size = 1.5, aes(group = 1)) +
  geom_point(stat = "summary", fun = "mean", color = "red", size = 3) +
  labs(title = "Readability and Number of Songs Over Decades (grouped by genre)",
       x = "Decade",
       y = "Average SMOG") +
  scale_fill_manual(values = c("rock" = "#009E73", "pop" = "#F0E442", "rap" = "#0072B2", "rb" = "#D55E00", "country" = "#CC79A7", "misc" = "#56B4E9")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

################### Reasoning with word counts ###################
word_counts <- textstat_summary(corpus, cache = TRUE)$tokens

# Merge word counts with the existing dataset
data_with_word_counts <- cbind(data_sample, word_counts)

# Plotting the average number of words per song over decades
ggplot(data_with_word_counts, aes(x = decade, y = word_counts)) +
  geom_line(stat = "summary", fun = "mean", color = "blue") +
  geom_point(stat = "summary", fun = "mean", color = "red", size = 2) +
  labs(title = "Average Number of Words Per Song Over Decades",
       x = "Decade",
       y = "Average Number of Words") +
  theme_minimal()

# Select the song with the most and least number of words
longest_song <- data_with_word_counts %>%
  arrange(desc(word_counts)) %>%
  select(artist, title, word_counts) %>%
  head(1)

shortest_song <- data_with_word_counts %>%
  arrange(word_counts) %>%
  select(artist, title, word_counts) %>%
  head(2)

###


