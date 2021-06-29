library(tidytuesdayR)
library(tidyverse)
library(lubridate)
library(ggstream)

tuesdata <- tidytuesdayR::tt_load('2021-03-23')

unvotes    <- tuesdata$unvotes
roll_calls <- tuesdata$roll_calls
issues     <- tuesdata$issues

# From http://data.un.org/Data.aspx?d=SNAAMA&f=grID%3a101%3bcurrID%3aUSD%3bpcFlag%3a1
gdp        <- read_csv("2021-03-23-un-votes/un-gdp.csv")

unvotes %>%
  group_by(rcid) %>%
  summarise(
    votes = n(),
    yes = sum(vote == "yes") / votes,
    no = sum(vote == "no") / votes,
    abstain = sum(vote == "abstain") / votes,
    .groups = "drop"
  )

roll_calls %>%
  left_join(issues, by = "rcid") %>%
  mutate(issue = replace_na(issue, "Other")) %>%
  mutate(
    year = year(date),
    decade = year - year %% 5
  ) %>%
  filter(issue != "Other") %>%
  group_by(year, issue) %>%
  summarise(votes = n(), .groups = "drop") %>%
  ggplot(aes(x = year, y = votes, fill = issue, label = str_wrap(issue, 20))) +
  guides(fill = "none") +
  ggfx::as_reference(
    geom_stream(type = "proportional"),
    id = "background"
  ) +
  ggfx::with_blend(
    geom_stream_label(type = "proportional"),
    bg_layer = "background",
    blend_type = "over"
  ) +
  scale_fill_viridis_d()
