library(dplyr)
library(tidyr)
library(lubridate)
library(forcats)
library(ggplot2)

data <- tidytuesdayR::tt_load("2021-05-25")

data$records %>%
  filter(shortcut == "No") %>%
  mutate(
    time = ifelse(type == "Three Lap", time / 3, time),
    year = year(date)
  ) %>%
  ggplot(aes(x = year, y = time, colour = type)) +
  geom_point() +
  facet_wrap(~track, scales = "free_y")

data$records %>%
  filter(shortcut == "No", type == "Three Lap") %>%
  mutate(record_end = date + days(record_duration)) %>%
  ggplot(aes(x = date, y = time, colour = player)) +
  geom_line(size = 1) +
  facet_wrap(~track)

