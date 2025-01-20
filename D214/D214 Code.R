### Libraries
library(lubridate)
library(ggplot2)
library(tidyverse)


###Data ggplot2###Data Cleaning 


GR <- read.csv("C:/Users/Nicole Haibach/OneDrive - Kleinschmidt Associates, Inc/Desktop/D214/goodreads_top100_from1980to2023_final.csv")
View(GR)

str(GR)
summary(GR)

colSums(is.na(GR))

GR$genres <- ifelse(grepl("Action|Adventure|Science Fiction", GR$genres), "Action",
                      ifelse(grepl("Romance", GR$genres), "Romance", "Other"))

table(GR$genres)

str(GR$publication_date)
GR$publication_date <- mdy(GR$publication_date)
GR$decade <- floor(year(GR$publication_date) / 10) * 10
str(GR$publication_date)
head(GR$publication_date)

GR <- GR %>% filter(decade >= 1980 & decade <= 2023)

### Visualize 

ggplot(GR, aes(x = rating_score)) +
  geom_histogram(binwidth = 0.1, fill = "blue", color = "black") +
  labs(title = "Distribution of Rating Scores", x = "Rating Score", y = "Frequency")
ggplot(GR, aes(x = rating_score)) +
  geom_histogram(binwidth = 0.1, fill = "blue", color = "black") +
  labs(title = "Distribution of Review Numbers", x = "num_reviews", y = "Frequency")


### Data Analysis

# Descriptive Statistics
descriptive_stats <- GR %>%
  group_by(genres) %>%
  summarize(
    avg_rating = mean(rating_score, na.rm = TRUE),
    avg_reviews = mean(num_reviews, na.rm = TRUE),
    book_count = n()
  )
print(descriptive_stats)

# Grouping Data by Decades and Genres
GR$decade <- floor(year(GR$publication_date) / 10) * 10
aggregated_data <- GR %>%
  group_by(genres, decade) %>%
  summarize(
    avg_rating_decade = mean(rating_score, na.rm = TRUE),
    avg_reviews_decade = mean(num_reviews, na.rm = TRUE)
  )
print(aggregated_data)

# Hypothesis Testing: Two-Sample T-Tests
t_test_rating <- t.test(
  rating_score ~ genres,
  data = GR %>% filter(genres %in% c("Action", "Romance"))
)
print(t_test_rating)

t_test_reviews <- t.test(
  num_reviews ~ genres,
  data = GR %>% filter(genres %in% c("Action", "Romance"))
)
print(t_test_reviews)

# Visualization: Trends Over Decades
ggplot(aggregated_data, aes(x = decade, y = avg_rating_decade, color = genres)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(title = "Average Rating by Genre Over Decades", x = "Decade", y = "Average Rating")

ggplot(aggregated_data, aes(x = decade, y = avg_reviews_decade, color = genres)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(title = "Average Number of Reviews by Genre Over Decades", x = "Decade", y = "Average Reviews")
