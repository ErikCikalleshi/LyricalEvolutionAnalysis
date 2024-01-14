

library(ggplot2)
library(tidyverse)
library(scales)
library(quanteda)
library(reshape2)


data <- read.csv("data_sample.csv")
#print(head(data))


cat("Genre of songs in the dataset\n")
genre <- unique(data[,"tag"])
print(genre)

# -Number of Songs per Genre

cat("\nNumber of songs per genre")
genre <- table(data$tag)
print(genre)


genre_df <- as.data.frame(genre)
genre_df$Genre <- names(genre)

# calculate total songs
total_songs <- sum(genre)

genre_df <- genre_df %>%
  mutate(Percentage = genre / sum(genre) * 100,
         Legend = paste(Genre, ": ", genre))

# ggplot bar plot
ggplot(genre_df, aes(x = Genre, y = Freq, fill = Legend)) +
  geom_bar(stat = "identity", color = "black") +
  geom_text(aes(label = sprintf("%.1f%%", Percentage)),
            position = position_stack(vjust = 0.5), 
            size = 3) +  
  scale_fill_brewer(palette = "Set3") +
  labs(title = "Number of Songs per Genre",
       x = "Genre",
       y = "Number of Songs") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  guides(fill = guide_legend(title = paste("Total Songs: ", total_songs), keywidth = 2, keyheight = 1))



# -Count of views per genre


cat("\nCount of views per genre\n")
views_per_genre <- tapply(data$views, data$tag, sum)
print(views_per_genre)


genre_views_df <- as.data.frame(views_per_genre)
genre_views_df$Genre <- names(genre)

# calculate total views
total_views <- sum(views_per_genre)

genre_views_df <- genre_views_df %>%
  mutate(Percentage = views_per_genre / sum(views_per_genre) * 100,
         Legend = paste(Genre, ": ", views_per_genre))

# ggplot bar plot
ggplot(genre_views_df, aes(x = Genre, y = views_per_genre, fill = Legend)) +
  geom_bar(stat = "identity", color = "black") +
  geom_text(aes(label = sprintf("%.1f%%", Percentage)),
            position = position_stack(vjust = 0.5), 
            size = 3) +  
  scale_fill_brewer(palette = "Set3") +
  
  labs(title = "Count of views per genre",
       x = "Genre",
       y = "Number of Views") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  guides(fill = guide_legend(title = paste("Total Views: ", total_views), keywidth = 2, keyheight = 1))




# -Number of Songs per Decade

cat("\nNumber of songs per decade")
song_per_decade <- table(data$decade)
print(song_per_decade)


# ggplot bar plot
ggplot(data, aes(x = as.factor(decade))) +
  geom_bar(fill = "#B7D2F3", color = "black") +
  labs(title = "Number of Songs per Decade",
       x = "Decade",
       y = "Number of Songs")



# -Geners per decade

cat("\nGeners per decade")
genre_per_decade <- table(data$decade, data$tag)
print(genre_per_decade)

# convert table to data frame
genre_per_decade_df <- as.data.frame(genre_per_decade)

# rename columns
colnames(genre_per_decade_df) <- c("Decade", "Genre", "Count")

# sort the decade in descending order
genre_per_decade_df$Decade <- factor(genre_per_decade_df$Decade, levels = rev(unique(genre_per_decade_df$Decade)))

# ggplot plot
ggplot(genre_per_decade_df, aes(x = Genre, y = Decade, fill = Count)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Count), vjust = 1) +
  scale_fill_gradient(low = "#B7D2F3", high = "#E5A8D4") +
  theme_minimal() +
  labs(title = "Geners per decade",
       x = "Genre",
       y = "Decade")


# -Views per decade

cat("\nViews per decade\n")
views_per_decade <- tapply(data$views, data$decade, sum)
print(views_per_decade)



views_per_decade_df <- as.data.frame(views_per_decade)
views_per_decade_df$Decade <- as.factor(names(views_per_decade))


# line plot
ggplot(views_per_decade_df, aes(x = Decade, y = views_per_decade, group = 1)) +
  geom_line(color = "black") +
  geom_point(color = "red") +  
  scale_y_continuous(labels = scales::comma_format(scale = 1e-6, suffix = "M")) +
  labs(title = "Views per decade",
       x = "Decade",
       y = "Views (Millions)") +
  
  theme_minimal()



# -Views per year

#cat("\nViews per year\n")
views_per_year <- tapply(data$views, data$year, sum)
#print(views_per_year)


views_per_year_df <- as.data.frame(views_per_year)
views_per_year_df$Year <- as.factor(names(views_per_year))

# desired interval for x-axis ticks
interval <- 15

# line plot
ggplot(views_per_year_df, aes(x = Year, y = views_per_year, group = 1)) +
  geom_line(color = "black") +
  scale_x_discrete(breaks = views_per_year_df$Year[seq(1, nrow(views_per_year_df), interval)],
                   labels = views_per_year_df$Year[seq(1, nrow(views_per_year_df), interval)]) +
  scale_y_continuous(labels = scales::comma_format(scale = 1e-6, suffix = "M")) +
  
  labs(title = "Views per year",
       x = "Year",
       y = "Views (Millions)") +
  
  theme_minimal()



# -Views per decade of genre

cat("\nViews per decade of genre\n")
views_per_decade_genre <- tapply(data$views, list(data$decade, data$tag), sum)
print(views_per_decade_genre)


views_per_decade_genre_df <- as.data.frame(views_per_decade_genre)
views_per_decade_genre_df$Decade <- as.factor(rownames(views_per_decade_genre_df))
#print(views_per_decade_genre_df)

for (i in 1:nrow(views_per_decade_genre_df)) {
  a <- views_per_decade_genre_df[i, ]
  
  # create data frame
  df <- data.frame(
    genre = names(a)[1:(length(a)-2)],
    views = as.numeric(a[1:(length(a)-2)])
  )
  
  # percentage of views for each genre
  df <- df %>%
    mutate(percentage = (views / sum(views)) * 100,
           Legend = paste(genre, ": ", round(percentage, 2), "%"))
  
  # donut chart with hole, label, and percentage labels
  p <- ggplot(df, aes(x = 2, y = percentage, fill = Legend)) +
    geom_bar(stat = "identity", width = 1) +
    geom_text(aes(label = ifelse(percentage > 3, paste(round(percentage, 2), "%"), "")),
              position = position_stack(vjust = 0.5),
              size = 4
    ) +
    coord_polar(theta = "y", start=200) +
    scale_fill_brewer(palette = "Set1",name = NULL) +
    theme_void() +
    theme(legend.position = "bottom") +
    annotate("text", x = 0, y = 0, label = a$Decade, size = 6, color = "black")
  
  print(p)
  
}




cat("\nBest song title, artist and genre per decade\n")
best_song_per_decade <- tapply(data$views, data$decade, max)
result <- data[data$views %in% best_song_per_decade, c("title", "tag", "artist", "views" , "decade")]
print(result[order(result$decade),])




# Word frequency in lyrics


# corpus object
corpus <- corpus(data$lyrics)

# preprocess the corpus
corpus <- tokens(corpus, what = "word", remove_punct = TRUE)
corpus <- tokens_remove(corpus, stopwords("en"))
corpus <- tokens_tolower(corpus)
corpus <- tokens_select(corpus, pattern = "^[a-z]+$", valuetype = "regex")

# document-feature matrix (dfm)
dfm <- dfm(corpus)

# calculate word frequencies
word_freq <- colSums(dfm)

# data frame with the word frequencies
word_freq_df <- data.frame(word = names(word_freq), freq = word_freq)

# sort data frame by frequency
word_freq_df <- word_freq_df[order(word_freq_df$freq, decreasing = TRUE), ]




# -Top 10 words

print(word_freq_df[1:10,])

top_10_words <- head(word_freq_df, 10)

# horizontal bar plot
bar_plot <- ggplot(top_10_words, aes(x = freq, y = reorder(word, freq))) +
  geom_bar(stat = "identity", fill = "#B7D2F3") +
  labs(title = "Top 10 Words Frequency",
       x = "Frequency (per 1000)",
       y = "Word") +
  scale_x_continuous(labels = scales::comma_format(scale = 1e-3), breaks = seq(0, max(top_10_words$freq), by = 10000)) +
  theme_minimal()

print(bar_plot)




# -Last 10 words with lowest frequency

print(tail(word_freq_df,10))

bottom_10_words <- tail(word_freq_df, 10)

# horizontal bar plot
bar_plot_bottom <- ggplot(bottom_10_words, aes(x = freq, y = reorder(word, freq))) +
  geom_bar(stat = "identity", fill = "#B7D2F3") +
  labs(title = "Bottom 10 Words Frequency",
       x = "Frequency",
       y = "Word") +
  scale_x_continuous(limits = c(0, 3), breaks = seq(0, 3, by = 1)) +
  theme_minimal()

print(bar_plot_bottom)




# Top 10 words per genre

# list to store word frequencies for each genre
word_freq_list <- list()

# iterate over each unique genre
for (tag in unique(data$tag)) {
  # subset corpus for current genre
  subset_corpus <- corpus[data$tag == tag]
  
  # document-feature matrix (dfm) for current genre
  dfm_genre <- dfm(subset_corpus)
  
  # calculate word frequencies for the current genre
  word_freq_by_genre <- colSums(dfm_genre)
  
  # store the word frequencies in the list
  word_freq_list[[tag]] <- word_freq_by_genre
}

# top words for each genre
top_words_by_genre <- lapply(word_freq_list, function(word_freq_by_genre) {
  head(sort(word_freq_by_genre, decreasing = TRUE), 10)
})
names(top_words_by_genre) <- unique(data$tag)
print(top_words_by_genre)


# -Genre similarities

# data frame for genre similarities
genre_similarities <- data.frame(matrix(0, nrow = length(unique(data$tag)), ncol = length(unique(data$tag))))
rownames(genre_similarities) <- colnames(genre_similarities) <- unique(data$tag)

# calculate cosine similarity between genres based on their top words
for (i in 1:(length(unique(data$tag)) - 1)) {
  for (j in (i + 1):length(unique(data$tag))) {
    genre1 <- unique(data$tag)[i]
    genre2 <- unique(data$tag)[j]
    
    words_genre1 <- names(word_freq_list[[genre1]])
    words_genre2 <- names(word_freq_list[[genre2]])
    
    common_words <- intersect(words_genre1, words_genre2)
    
    if (length(common_words) > 0) {
      vec_genre1 <- word_freq_list[[genre1]][common_words]
      vec_genre2 <- word_freq_list[[genre2]][common_words]
      
      cosine_similarity <- sum(vec_genre1 * vec_genre2) / (sqrt(sum(vec_genre1^2)) * sqrt(sum(vec_genre2^2)))
      
      genre_similarities[genre1, genre2] <- cosine_similarity
      genre_similarities[genre2, genre1] <- cosine_similarity
    }
  }
}

# convert data frame to a matrix
genre_sim_matrix <- as.matrix(genre_similarities)

# melt the matrix using reshape2::melt
melted_genre_sim <- melt(genre_sim_matrix)

# ggplot2 plot
ggplot(melted_genre_sim, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(value, 2)), vjust = 1) +
  scale_fill_gradient(low = "#B7D2F3", high = "#E5A8D4") +
  theme_minimal() +
  labs(title = "Genre Similarities",
       x = "Genre",
       y = "Genre")






