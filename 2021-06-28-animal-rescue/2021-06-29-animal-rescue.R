library(tidyverse)
library(tidytuesdayR)
library(lubridate)
library(sf)
library(patchwork)
library(ragg)

tuesdata <- tidytuesdayR::tt_load('2021-06-29')
animal_rescue_raw <- tuesdata$animal_rescues

animal_rescue <- animal_rescue_raw %>%
  mutate(
    date_time_of_call = parse_date_time(date_time_of_call, "d/m/y H:M"),
    animal_group_basic = ifelse(
      animal_group_parent %in% c("Bird", "Cat", "Dog", "Fox"),
      animal_group_parent, "Other"
    ),
    year = year(date_time_of_call)
  )

# 2019-2020 change data
changes <- animal_rescue %>%
  filter(year %in% 2019:2020) %>%
  group_by(borough_code, year) %>%
  summarise(calls = n(), .groups = "drop") %>%
  pivot_wider(names_from = year, values_from = calls, names_prefix = "calls_", values_fill = 0) %>%
  mutate(calls_change = (calls_2020 - calls_2019) / calls_2019)

# Data from https://data.london.gov.uk/dataset/statistical-gis-boundary-files-london
sf <- read_sf("2021-06-28-animal-rescue/statistical-gis-boundaries-london/ESRI/London_Borough_Excluding_MHW.shp") %>%
  rename_with(str_to_lower)

# Plotting functions -------------------

my_theme <- function() {
  theme_void() +
    theme(
      text = element_text(family = "Lato"),
      plot.title = element_text(family = "Roboto Slab", size = 18),
      plot.subtitle = element_text(family = "Lato"),
      plot.background = element_rect(fill = "grey95", colour = NA)
    )
}

plot_change_by_animal <- function(data) {

  plot_data <- data %>%
    filter(year %in% c(2019, 2020)) %>%
    group_by(year, animal_group_basic) %>%
    summarise(calls = n(), .groups = "drop") %>%
    group_by(year) %>%
    mutate(calls_proportion = calls / sum(calls)) %>%
    ungroup() %>%
    mutate(animal_group_basic = fct_reorder(animal_group_basic, calls, .desc = TRUE))

  max_calls <- max(plot_data$calls)
  nudge_text = max_calls / 35

  ggplot(plot_data, aes(x = animal_group_basic, y = calls, fill = animal_group_basic)) +
    geom_col(aes(alpha = factor(year)), position = "dodge") +
    scale_alpha_manual(
      breaks = c("2019", "2020"), values = c(0.4, 1)
    ) +
    geom_text(
      aes(label = calls, y = calls + nudge_text * 2),
      position = position_dodge2(1),
    ) +
    geom_text(
      aes(label = label),
      colour = "white",
      data = tibble(animal_group_basic = "Cat", calls = nudge_text, label = 2019:2020),
      position = position_dodge2(1),
      hjust = 1,
      size = 7,
      angle = -90
    ) +
    geom_text(
      aes(label = animal_group_basic, colour = animal_group_basic, y = -3 * nudge_text)
    ) +
    scale_y_continuous(limits = c(-4 * nudge_text, max_calls + nudge_text * 3)) +
    scale_fill_brewer(palette = "Dark2", aesthetics = c("colour", "fill")) +
    my_theme() +
    theme(legend.position = "none")
}

plot_shaded_boroughs <- function(data, col, breaks = c(TRUE, FALSE), values = c("red", "grey")) {

  data %>%
    ggplot(aes(fill = {{ col }})) +
    geom_sf(colour = NA) +
    scale_fill_manual(breaks = breaks, values = values) +
    guides(fill = "none") +
    my_theme() +
    theme(plot.background = element_rect(fill = NA, colour = NA))

}


plot_decreases <- function() {

  p1 <- animal_rescue %>%
    semi_join(filter(changes, calls_change <= 0), by = c("borough_code")) %>%
    plot_change_by_animal() +
    labs(
      title = "Decreased Calls",
      subtitle = paste(
        "While calls for most boroughs increased in 2020, there are",
        "some boroughs which saw decreases in calls. Where a decrease",
        "was seen, this was fairly evenly spread across",
        "calls for different animals.",
        sep = "\n"
      )
    )

  p2 <- sf %>%
    left_join(changes, by = c("gss_code" = "borough_code")) %>%
    mutate(shade = calls_change <= 0) %>%
    plot_shaded_boroughs(shade)

  p1 +
    inset_element(p2, 0.4, 0.5, 1.2, 1.18)

}

plot_increases <- function() {

  p1 <- animal_rescue %>%
    semi_join(filter(changes, calls_change > 0), by = c("borough_code")) %>%
    plot_change_by_animal() +
    labs(
      title = "Increased Calls",
      subtitle = paste(
        "Overall, most boroughs saw a marked increase in call-outs",
        "for animals between 2019 and 2020. However, not all animals",
        "saw the same increase. Call-outs for dogs, for example, barely",
        "changed at all, while calls for cats and birds",
        "increased significantly.",
        sep = "\n"
      )
    )

  p2 <- sf %>%
    left_join(changes, by = c("gss_code" = "borough_code")) %>%
    mutate(shade = calls_change > 0) %>%
    plot_shaded_boroughs(shade)

  p1 +
    inset_element(p2, 0.4, 0.5, 1.2, 1.18)

}

main_plot <- function() {
  sf %>%
    rename_with(str_to_lower) %>%
    left_join(
      changes,
      by = c("gss_code" = "borough_code")
    ) %>%
    ggplot(aes(fill = calls_change)) +
    geom_sf(colour = "grey", size = 0.1) +
    geom_sf_text(
      aes(label = scales::percent(calls_change, 1)),
      colour = "grey20",
      check_overlap = TRUE
    ) +
    scale_fill_viridis_c(labels = scales::percent, guide = "legend") +
    my_theme() +
    theme(
      legend.position = c(0.5, 0),
      legend.direction = "horizontal",
      legend.key.size = unit(0.5, "cm"),
      legend.title = element_text(hjust = 1, vjust = 1.5, size = 8),
      plot.title = element_text(family = "Roboto Slab", hjust = 0.5, size = 25),
      plot.subtitle = element_text(hjust = 0.5),
      plot.caption.position = "panel",
      plot.caption = element_text(hjust = 0)
    ) +
    labs(
      fill = "Increase in Calls\n2019-2020",
      title = "London Animal Rescues\nand COVID-19",
      subtitle = paste(
        "The number of animal rescue operations by the London Fire Brigade",
        "increased significantly between 2019 and 2020. The main causes of this",
        "are highly likely to relate to COVID-19",
        sep = "\n"
      ),
      caption = paste(
        "\nVisualisation: J. Scott | Data: London.gov\n#TidyTuesday"
      )
    )
}

# Produce the plots ---------------------
design <- "
AAABB
AAACC
"

agg_png("2021-06-28-animal-rescue/plot.png", width = 2000, height = 1300, res = 180)
main_plot() + plot_decreases() + plot_increases() + plot_layout(design = design)
invisible(dev.off())
