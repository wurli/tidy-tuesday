library(dplyr)
library(purrr)
library(lubridate)
library(ggplot2)

data <- list(
  summary = "summary",
  challenges = "challenges",
  castaways = "castaways",
  viewers = "viewers",
  jury_votes = "jury_votes"
) %>%
  map(~readr::read_csv(sprintf(
    'https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-06-01/%s.csv', .
  )))

data$summary %>%
  mutate(season_length = time_length(interval(premiered, ended), "days")) %>%
  ggplot(aes(x = season_length, y = viewers_mean, colour = as.factor(season))) +
  geom_point() +
  scale_colour_discrete()
