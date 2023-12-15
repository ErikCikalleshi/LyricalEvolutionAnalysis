setwd("C:/Users/Th3RapidK1ller/Documents/LyricalEvolutionAnalysis")
# Read in the data where language is English
data <- read.csv("data.csv", header = TRUE, stringsAsFactors = FALSE)
data <- data[data$language == "en",]
# print the number of rows
nrow(data)
# eliminate songs with year less < 1900
data <- data[data$year >= 1900,]
# for whatever reasons there are songs in the feature
data <- data[data$year <= 2023,]
# add a new column to data where we add the decade 
data$decade <- floor(data$year/10)*10
# get 1000 songs from each decade randomly

decades <- unique(data$decade)

for(decade in decades){
  # get the songs for the decade
  songs_decade <- data[data$decade == decade,]
  # randomly select up to 10,000 songs
  sample_size <- min(10000, nrow(songs_decade))
  songs_decade <- songs_decade[sample(nrow(songs_decade), sample_size),]
  # add the songs to the data frame
  if (decade == decades[1]){
    data_sample <- songs_decade
  } else {
    data_sample <- rbind(data_sample, songs_decade)
  }
}

# print the number of rows
nrow(data_sample)

# count how many songs are there for each genre
genre_counts <- table(data_sample$tag)

# Print the genre counts
print(genre_counts)

# print sum of genre counts
print(sum(genre_counts))

#export to csv
write.csv(data_sample, file = "data_sample.csv")