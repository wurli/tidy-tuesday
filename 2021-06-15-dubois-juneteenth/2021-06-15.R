library(tidytuesdayR)
library(tidyverse)

# data_2020_8 <- tt_load(2020, 8)
# data_2020_24 <- tt_load(2020, 24)
data_2020_25 <- tt_load(2020, 25)

data <- tt_load("2021-06-15")

data$tweets %>%
  mutate(hashtags = paste(str_extract_all(content, "#[A-Za-z]*"), collapse = ", ")) %>%
  filter(!is.na(content)) %>%
  pull(content, username) %>%
  imap(function(tweet, user) {
    cat(user, strrep("-", 50 - nchar(user)), "\n")
    cat(tweet, "\n\n")
  }) %>% invisible()

data$tweets %>%
  replace_na(list(location = "Other")) %>%
  rowwise() %>%
  summarise(
    across(everything(), identity),
    hashtag = str_extract_all(content, "#[A-Za-z]+")[[1]] %>% str_to_lower(),
    .groups = "drop"
  ) %>%
  group_by(hashtag) %>%
  mutate(hashtag_freq = n()) %>%
  ungroup() %>%
  left_join(
    count(., location) %>%
      mutate(location2 = ifelse(n > 20, location, "Other")),
    by = "location"
  ) %>%
  group_by(location2) %>%
  mutate(location_freq = n()) %>%
  ungroup() %>%
  filter(hashtag_freq > 3) %>%
  mutate(hashtag = fct_reorder(hashtag, hashtag_freq),
         location2 = fct_reorder(location2, location_freq)) %>%
  ggplot(aes(x = hashtag, fill = location2)) +
  geom_bar() +
  coord_flip() +
  scale_fill_viridis_d()
