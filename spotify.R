library(spotifyr)
library(dplyr)
library(purrr)
library(tibble)
library(ggplot2)
library(lubridate)

# Set up the Spotify API credentials
Sys.setenv(SPOTIFY_CLIENT_ID = '521b056c02914891b76f828c31982313')
Sys.setenv(SPOTIFY_CLIENT_SECRET = '817ceb170a6340b3880b9dfc6ed2a8d0')
access_token <- get_spotify_access_token()

# Function to fetch artist data
get_artist_data <- function(artist_name) {
  artist <- search_spotify(artist_name, type = 'artist') %>%
    filter(name == artist_name) %>%
    slice(1)
  
  top_tracks <- get_artist_top_tracks(artist$id[1], market = "US")
  
  # Extract relevant data
  track_data <- top_tracks %>%
    mutate(
      track_id = id,   # Use track ID for joining
      track_name = name,
      popularity = popularity,
      duration_minutes = duration_ms / 60000,
      release_date = album.release_date,
      album_name = album.name
    ) %>%
    select(track_id, track_name, popularity, duration_minutes, release_date, album_name)
  
  # Get audio features for tracks
  track_features <- map_df(top_tracks$id, get_track_audio_features)
  
  # Combine top track data with track features using track_id for joining
  combined_data <- left_join(track_data, track_features, by = c("track_id" = "id"))
  
  return(combined_data)
}

# Fetch data for both Kendrick Lamar and Drake
kendrick_data <- get_artist_data("Kendrick Lamar")
drake_data <- get_artist_data("Drake")



# 1.Popularity Comparison

# Calculate average popularity for both artists
avg_popularity_kendrick <- mean(kendrick_data$popularity)
avg_popularity_drake <- mean(drake_data$popularity)

cat("Kendrick Lamar's average track popularity:", avg_popularity_kendrick, "\n")
cat("Drake's average track popularity:", avg_popularity_drake, "\n")


# 2.Track Feature Comparison: Energy and Danceability
# Plot comparison of track features
library(ggplot2)

# Combine both artists' data into one dataframe for comparison
combined_data <- bind_rows(
  kendrick_data %>% mutate(artist = "Kendrick Lamar"),
  drake_data %>% mutate(artist = "Drake")
)

# Energy comparison
ggplot(combined_data, aes(x = artist, y = energy)) +
  geom_boxplot() +
  labs(title = "Energy Comparison: Kendrick Lamar vs Drake", y = "Energy", x = "Artist") +
  theme_minimal()

# Danceability comparison
ggplot(combined_data, aes(x = artist, y = danceability)) +
  geom_boxplot() +
  labs(title = "Danceability Comparison: Kendrick Lamar vs Drake", y = "Danceability", x = "Artist") +
  theme_minimal()


# 3. Release Trends
# Plot release trends over time
combined_data$release_date <- as.Date(combined_data$release_date)

ggplot(combined_data, aes(x = release_date, color = artist)) +
  geom_density() +
  labs(title = "Release Trends Over Time: Kendrick Lamar vs Drake", x = "Release Date", y = "Density") +
  theme_minimal()



  