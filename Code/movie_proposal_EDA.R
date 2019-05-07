library(dplyr)
library(readr)
library(wordcloud)
library(jsonlite)
library(tidyverse)
library(ggplot2)
library(plotly)
library(lubridate)
library(gridExtra)
library(GGally)
library(network)

movie <- read_csv("tmdb_5000_movies.csv", na = "NA")
glimpse(movie)
apply(is.na(movie), 2, sum)
movie$release_date[is.na(movie$release_date)] = "2014-06-01"
movie$runtime[is.na(movie$runtime)] = c(94, 240)

credit <- read_csv("tmdb_5000_credits.csv", na = "NA")
glimpse(credit)
apply(is.na(credit), 2, sum)

credit <- credit %>% select(-title)
movie.full <- movie %>% 
  inner_join(credit, by = c("id" = "movie_id")) %>%
  mutate(year = year(release_date)) %>%
  select(-homepage, -original_title, -spoken_languages, 
         -production_countries, -status, -release_date)

rating <- read_csv("ratings.csv", na = "NA")
rating <- movie %>% select(id, title) %>%
  inner_join(rating, by = c("id" = "movieId")) %>%
  arrange(userId)
apply(is.na(rating), 2, sum)

genres <- movie.full %>%
  filter(nchar(genres) > 2) %>%
  mutate(js = lapply(genres, fromJSON)) %>%
  unnest(js) %>%
  select(id, title, genres = name) %>%
  mutate_if(is.character, factor)
keywords <- movie.full %>%
  filter(nchar(keywords) > 2) %>%
  mutate(js = lapply(keywords, fromJSON)) %>%
  unnest(js) %>%
  select(id, title, keywords = name) %>%
  mutate_if(is.character, factor)
company <- movie.full %>%
  filter(nchar(production_companies) > 2) %>%
  mutate(js = lapply(production_companies, fromJSON)) %>%
  unnest(js) %>%
  select(id, title, company = name) %>%
  mutate_if(is.character, factor)
cast <- movie.full %>%
  filter(nchar(cast) > 2) %>%
  mutate(js = lapply(cast, fromJSON)) %>%
  unnest(js) %>%
  select(id, title, cast = name) %>%
  mutate_if(is.character, factor)
crew <- movie.full %>%
  filter(nchar(crew) > 2) %>%
  mutate(js = lapply(crew, fromJSON)) %>%
  unnest(js) %>%
  select(id, title, crew = name) %>%
  mutate_if(is.character, factor)

temp <- movie.full %>% group_by(year) %>% summarise(n = n())
p <- ggplot(temp, aes(x = year, y = n)) + geom_line() +
  labs(title = "Number of Movies V.S. Year", x = "Year", y = "Number") + 
  theme(plot.title = element_text(hjust = 0.5))
p

genres %>% group_by(genres) %>% count() %>%
  ggplot(aes(x=reorder(genres, n), y=n)) +
  geom_col(fill="blue") + coord_flip() +
  labs(x="", y="Number of movies")

temp <- movie.full %>% select(-genres, -title) %>% 
  right_join(genres, by = "id") %>% 
  group_by(genres) %>% summarise(avepop = mean(popularity), 
                                 averev = mean(revenue))
plot1 <- ggplot(temp, aes(x = genres, y = avepop, fill = genres)) + geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5), plot.title = element_text(hjust = 0.5), legend.position = "none") + labs(title = "Average Popularity V.S. Genres", xlab = "Genres", ylab = "Popularity")
plot1

plot2 <- ggplot(temp, aes(x = genres, y = averev, fill = genres)) + geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5), plot.title = element_text(hjust = 0.5), legend.position = "none") + labs(title = "Average Revenue V.S. Genres", xlab = "Genres", ylab = "Revenue")
plot2

movie.full  %>% top_n(20, wt = vote_count) %>%
  ggplot(aes(x = reorder(title, vote_count), y = vote_count)) +
  geom_bar(stat='identity') + coord_flip(y=c(0, 15000)) +
  labs(x = "", y = "Number of Notes") +
  geom_text(aes(label = vote_count), hjust = -0.1, size = 3) +
  geom_text(aes(label = vote_average), y = 1000, size = 3, col = "yellow")

p <- ggscatmat(movie.full %>% select(budget, popularity, revenue, vote_average, vote_average))
p

set.seed(2019)
keywords_counts <- keywords %>% count(keywords)
par(mfrow=c(1, 2),bg="grey97")
wordcloud(keywords_counts$keywords, keywords_counts$n, max.words = 60, scale=c(1.5, .5), random.color = TRUE, random.order=FALSE, rot.per=0, colors=brewer.pal(9,"Set1"))

company_counts <- company %>% count(company)
wordcloud(company_counts$company, company_counts$n, max.words = 30, scale=c(1.5,.5), random.color = TRUE, random.order=FALSE, rot.per=0, colors=brewer.pal(6,"Set1"))

C <- mean(movie.full$vote_average)
m <- quantile(movie.full$vote_count, 0.75)
movie.full <- movie.full %>% mutate(weighted_score = (vote_average * vote_count + C * m) / (vote_count + m))
