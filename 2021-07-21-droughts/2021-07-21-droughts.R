library(tidyverse)
library(lubridate)
library(patchwork)
library(ggstream)
library(geofacet)
library(ggfx)
library(ggforce)

tuesdata <- tidytuesdayR::tt_load('2021-07-20')
drought_raw <- tuesdata$drought
pal <- c(
  "#4C9B54", "#C1DE8F", "#FEFBBC", "#F1C86D", "#DE382C"
)

drought <- drought_raw %>%
  mutate(map_date = ymd(map_date)) %>%
  mutate(
    drought_lvl_num = case_when(
      drought_lvl == "None" ~ -1L,
      TRUE ~ as.integer(str_extract(drought_lvl, "\\d"))
    ),
    drought_lvl = fct_reorder(drought_lvl, drought_lvl_num)
  ) %>%

  # Some values are cumulative - this fixes that
  arrange(state_abb, map_date, drought_lvl_num) %>%
  group_by(state_abb, map_date) %>%
  mutate(across(starts_with(c("area", "pop")), ~case_when(
    sum(area_pct) <= 100  ~ .,
    drought_lvl == "None" ~ .,
    TRUE                  ~ . - lead(., default = 0)
  ))) %>%
  ungroup()

# drought %>%
#   filter(drought_lvl != "None") %>%
#   filter(state_abb == "WI") %>%
#   ggplot(aes(x = valid_start, node = drought_lvl, value = area_total, fill = drought_lvl)) +
#   geom_sankey_bump(type = "alluvial", color = "transparent", space = 0, smooth = 6) +
#   scale_fill_viridis_d(option = "A", alpha = .8)

drought %>%
  filter(state_abb != "PR") %>%
  arrange(valid_start) %>%
  group_by(
    state_abb,
    year = year(valid_start),
    drought = case_when(
      drought_lvl_num == -1    ~ "None",
      drought_lvl_num %in% 0:1 ~ "Some",
      drought_lvl_num %in% 2:4 ~ "Severe",
    )
  ) %>%
  summarise(area_pct = sum(area_pct), .groups = "drop") %>%
  group_by(state_abb, year) %>%
  mutate(area_pct = area_pct / sum(area_pct)) %>%
  ungroup() %>%
  group_by(state_abb) %>%
  mutate(
    label = ifelse(year == max(year) & drought == first(drought), state_abb, NA_character_),
    x = min(year) + (max(year) - min(year)) * 0.9,
    y = 0.9
  ) %>%
  ungroup() %>%
  ggplot(aes(x = year, y = area_pct)) +
  as_reference(
    geom_stream(aes(fill = drought), type = "proportional", n_grid = 2000),
    id = "bg"
  ) +
  with_blend(
    geom_text(
      aes(x = x, y = y, label = label),
      hjust = "right", vjust = "top", na.rm = TRUE,
      alpha = 0.5, fontface = "bold", colour = "blue"
    ),
    bg_layer = "bg",
    blend_type = "color_burn"
  ) +
  facet_geo(~state_abb) +
  theme_void() +
  scale_fill_manual(
    values = pal[c(1, 3, 5)], breaks = c("None", "Some", "Severe")
  ) +
  theme(strip.text = element_blank())

ggplot() +
  geom_area(aes(
    x = rep(1:10, 2),
    y = rep(c(6, 4), each = 10),
    fill = rep(c("red", "brown"), each = 10) %>% as_factor()
  ), position = "dodge") +
  geom_point(aes(x = 8, y = 5), size = 20, colour = "black") +
  scale_fill_identity()

tibble(
  x = rep(1:3, 10),
  y = rep(1:10, each = 3),
  colour = rep(letters[1:10], each = 3)
) %>%
  ggplot(aes(x = x, y = y)) +
  as_reference(
    geom_area(aes(fill = colour)),
    "bg"
  ) +
  with_blend(
    geom_text(
      aes(label = "Some test text"),
      data = tibble(x = 2, y = 30),
      fontface = "bold", size = 20, angle = 45, alpha = 0.8, colour = "blue"
    ),
    bg_layer = "bg", blend_type = "color_burn"
  ) +
  scale_fill_viridis_d()


