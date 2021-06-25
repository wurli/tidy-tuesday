library(tidyverse)
library(tidytuesdayR)
library(sf)
library(patchwork)
library(ggfx)
library(extrafont)

loadfonts(device = "win")

tuesdata <- tidytuesdayR::tt_load('2021-06-22')
parks_raw <- tuesdata$parks


states_cities_raw <- read_csv("https://raw.githubusercontent.com/kelvins/US-Cities-Database/main/csv/us_cities.csv")

states_cities <- states_cities_raw %>%
  rename(state = STATE_NAME, city = CITY, state_code = STATE_CODE) %>%
  distinct(state, city, state_code) %>%
  mutate(state_id = str_to_lower(state))

usa <- sf::st_as_sf(maps::map("state", fill=TRUE, plot = FALSE)) %>%
  rename(state_id = ID, geometry = geom)

parks <- parks_raw %>%
  rename(total_data = total_pct) %>%
  mutate(
    across(c(park_pct_city_data, pct_near_park_data), ~as.numeric(str_remove(., "%")) * 0.01),
    across(c(spend_per_resident_data), ~as.numeric(str_remove(., "\\$"))),
    across(contains("_pct_"), ~. * 0.01)
  ) %>%
  pivot_longer(
    ends_with(c("_data", "_points")),
    names_to = c("measure", ".value"),
    names_pattern = c("(.+)_(data|points)")
  ) %>%
  mutate(
    city = fct_recode(
      city,
      "Washington"       = "Washington, D.C.",
      "Washington"       = "Washington, DC",
      "Arlington"        = "Arlington, Virginia",
      "Saint Paul"       = "St. Paul",
      "Saint Louis"      = "St. Louis",
      "Saint Petersburg" = "St. Petersburg",
      "Arlington"        = "Arlington, Texas",
      "Winston Salem"    = "Winston-Salem",
      "Charlotte"        = "Charlotte/Mecklenburg County"
    ),
    description = case_when(
      measure == "med_park_size"      ~ "Median park size in acres",
      measure == "park_pct_city"      ~ "Parkland as a percantage of city area",
      measure == "pct_near_park"      ~ "Residents within a 10 minute walk to park",
      measure == "spend_per_resident" ~ "Spending per resident (USD)",
      measure == "basketball"         ~ "Basketball hoops per 10,000 residents",
      measure == "dogpark"            ~ "Dog parks per 100,000 residents",
      measure == "playground"         ~ "Playgrounds per 10,000 residents",
      measure == "rec_sr"             ~ "Recreation/senior centers per 20,000 residents",
      measure == "restroom"           ~ "Restrooms per 10,000 residents",
      measure == "splashground"       ~ "Splashgrounds/splashpads per 100,000 residents",
      measure == "amenities"          ~ "Amenities total (ie play areas)",
      measure == "total"              ~ "Total (varies in denominator per/year)"
    )
  )


violin_plots <- unique(parks$measure) %>%
  setdiff(c("total", "amenities")) %>%
  map(function(category) {

    labeller <- if (str_detect(category, "spend")) {
      scales::dollar
    } else if (category == "park_pct_city") {
      scales::percent_format(0.1)
    } else if (str_detect(category, "pct")) {
      scales::percent
    } else {
      identity
    }

    plot_data <- parks %>%
      filter(year == 2020, measure == category)

    plot_data %>%
      ggplot(aes(data, y = 1)) +
      geom_violin(fill = "steelblue", colour = NA) +
      scale_x_continuous(
        labels = function(x) ifelse(x == 0, "", labeller(x)),
        limits = c(0, NA)
      ) +
      labs(
        x = NULL, y = NULL,
        caption = unique(plot_data$description) %>% str_wrap(23)
      ) +
      theme_minimal() +
      theme(
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(colour = "grey90"),
        panel.grid.minor = element_blank(),
        text = element_text("Lato"),
        strip.text = element_text(hjust = 1),
        plot.caption = element_text(hjust = 0.5),
        plot.background = element_rect(fill = "#fdf6e3", color = NA)
      )
  }) %>%
  wrap_plots(guides = "collect", nrow = 2)

violin_plots

usa_plot <- parks %>%
  filter(measure == "total") %>%
  left_join(states_cities, by = "city") %>%
  group_by(state, state_code, state_id) %>%
  summarise(across(c(data, points), median, na.rm = TRUE), .groups = "drop") %>%
  left_join(usa, by = "state_id") %>%
  ggplot(aes(geometry = geometry, fill = data, label = state_code)) +
  with_shadow(
    geom_sf(colour = NA, na.rm = TRUE),
    x_offset = -3,
    y_offset = 3,
    sigma = 4
  ) +
  scale_fill_viridis_c() +
  geom_sf_text(colour = "grey90", na.rm = TRUE, check_overlap = TRUE) +
  theme_void() +
  theme(
    legend.position = c(0.1, 0.1),
    legend.direction = "horizontal",
    legend.title = element_text(vjust = 1.1),
    plot.background = element_rect(fill = "#fdf6e3", color = NA),
    plot.title = element_text("Roboto Slab", "bold", size = 30, colour = "#006400"),
    text = element_text("Lato")
  ) +
  labs(
    fill = c("Park quality\nPoints/100"),
    title = "Park Quality in the U.S.A",
    subtitle = paste(
      "Median overall park quality for each state is calculated",
      "using a combination of park size, accessibility, spending",
      "and amenities",
      sep = "\n"
    )
  )

usa_plot

design <- "
AAA
AAA
AAA
BBB
"

final <- usa_plot + violin_plots +
  plot_layout(design = design)

ggsave(
  "2021-06-21-parks-access/parks-access.png",
  plot = final,
  width = 25,
  height = 20,
  units = "cm"
)
