# Source for this code
# https://towardsdatascience.com/explore-your-activity-on-netflix-with-r-how-to-analyze-and-visualize-your-viewing-history-e85792410706

# Load Libraries
library(dplyr)
library(tidyr)
library(lubridate)
library(zoo)
library(ggplot2)
library(HelpersMG)

# Download the Data from your netflix account (You must be logged in )
# Go to https://www.netflix.com/viewingactivity and press "Download all" at the end of the page

# Read on csv data
mynetflix <- read.csv("NetflixViewingHistory.csv")

# Data wrangling
mynetflix_serie <- mynetflix %>%
  as_tibble() %>%
  # Better format for Date
  mutate(
    Date = as.Date(Date, format = "%d/%m/%y"),
    Date = lubridate::date(Date)) %>%
  # Split Title into series, season and episode title
  separate(col = Title, into = c("title", "season", "ep_title"), sep = ': ') %>%
  na.omit(c(season, ep_title))

# Order by Date and have the number of episodes
marathone_mynetflix <- mynetflix_serie %>%
  count(title, Date) %>%
  # filter(n >= 5) %>%
  arrange(Date)

# Group by the Series Title
marathone_mynetflix_all <- marathone_mynetflix %>%
  group_by(title) %>%
  summarise(n = sum(n)) %>%
  arrange(-n)

# First plot
marathone_mynetflix_all %>%
  head(10) %>%
  ggplot(aes(x = reorder(title, n), y = n)) +
  geom_col(fill = "#0097d6") +
  coord_flip() +
  ggtitle("Top 10 Series") +
  labs(x = "Series", y = "Total watched episodes") +
  theme_minimal()

# Eps per day - Time Series
mynetflix_serie %>%
  count(Date) %>%
  arrange(-n) %>%
  ggplot(aes(x = Date, y = n)) +
  geom_bar(stat = "identity") +
  ggtitle("Episodes per day") +
  labs(x = "Date", y = "Watched Eps")

# Calendar heatmap
mynetflix_serie %>%
  count(Date) %>%
  arrange(Date) %>%
  mutate(wday = lubridate::wday(Date, label = TRUE, abbr = TRUE),
         wdayn = lubridate::wday(Date),
         month = lubridate::month(Date, label = TRUE, abbr = TRUE)) %>%
  ggplot(aes(x = wday, wdayn, fill = n)) +
  geom_tile(colour = "white") +
  facet_grid(lubridate::year(Date) ~ month) +
  scale_fill_gradient(low = "#FFD000", high = "#FF1919") +
  ggtitle("Episodes per day") +
  labs(x = "Week", y = "Day of the week") +
  labs(fill = "N. of Eps")

# Watched per day of the week
mynetflix_serie %>%
  arrange(Date) %>%
  mutate(wday = lubridate::wday(Date, label = TRUE, abbr = TRUE),
         wdayn = lubridate::wday(Date),
         month = lubridate::month(Date, label = TRUE, abbr = TRUE)) %>%
  count(wday) %>%
  ggplot(aes(wday, n)) +
  geom_col(fill = "#5b59d6") +
  coord_polar()  +
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(face = "bold"),
        plot.title = element_text(size = 16, face = "bold")) +
  ggtitle("Wacthed Eps per day of the week")

# Watched per month
mynetflix_serie %>%
  arrange(Date) %>%
  mutate(wday = lubridate::wday(Date, label = TRUE, abbr = TRUE),
         wdayn = lubridate::wday(Date),
         month = lubridate::month(Date, label = TRUE, abbr = TRUE)) %>%
  count(month) %>%
  ggplot(aes(month, n)) +
  geom_col(fill = "#5b59d6") +
  coord_polar()  +
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(face = "bold"),
        plot.title = element_text(size = 16, face = "bold")) +
  ggtitle("Wacthed Eps per month")
