library(tidyverse)
library(ggstream)
library(ggrepel)
library(emoji)
library(ggtext)
library(patchwork)

tuesdata <- tidytuesdayR::tt_load("2021-07-27")

olympics <- tuesdata$olympics
regions <- tuesdata$regions

olympics_logo <- magick::image_read_svg(
  "https://upload.wikimedia.org/wikipedia/commons/5/5c/Olympic_rings_without_rims.svg"
)

sysfonts::font_add_google("Scheherazade")
showtext::showtext_auto()

clr <- list(
  Gold = "#FFD700",
  Silver = "#C0C0C0",
  Bronze = "#cd7f32",
  None = "grey80"
)

key <- tibble(
  medal = fct_inorder(names(clr)),
  height = c(1, 1, 1, -3),
  colour = unlist(clr),
  text_colour = colorspace::darken(colour, 0.2),
  label = c("Gold medals", "Silver medals", "Bronze medals", "No medal")
) %>%
  ggplot(aes(x = 0, y = height, fill = colour, label = label, group = medal)) +
  geom_col(data = ~filter(., medal != "None"), width = 1) +
  geom_col(data = ~filter(., medal == "None"), width = 0.6) +
  geom_text(
    aes(x = -1, y = c(2.5, 1.5, 0.5, -1.5), colour = text_colour),
    size = 5, hjust = "right", fontface = "bold"
  ) +
  annotate(
    "text", x = 0, y = 0, label = " Year", angle = -90, colour = "grey99",
    hjust = "left", size = 4.5, fontface = "bold"
  ) +
  scale_fill_identity() +
  scale_colour_identity() +
  geom_segment(aes(x = -.7, xend = .7, y = 0, yend = 0), size = 0.3, colour = "grey50") +
  theme_void() +
  xlim(-7.5, 1) +
  theme(panel.background = element_rect(fill = "grey99", colour = "grey90", size = 3))


plot <- olympics %>%

  filter(season == "Summer", year >= 1980) %>%

  left_join(regions, by = c("noc" = "NOC")) %>%
  rename(country = region) %>%

  mutate(medal = replace_na(medal, "None") %>% factor(names(clr))) %>%
  group_by(country, sport) %>%
  mutate(
    total_medals = sum(medal != "None"),
    total_golds = sum(medal == "Gold")
  ) %>%
  ungroup() %>%

  group_by(country) %>%
  filter(total_medals == max(total_medals)) %>%
  ungroup() %>%

  filter(total_medals >= 1) %>%

  group_by(country) %>%
  mutate(top_sports = paste(sort(unique(sport)), collapse = " / ")) %>%
  ungroup() %>%

  group_by(country, year, top_sports, medal) %>%
  summarise(n = n(), .groups = "drop") %>%
  mutate(n = ifelse(medal == "None", -n, n)) %>%

  mutate(label = glue::glue(
    "**{country}**<br><span style = 'font-size:10pt; ",
    "font-family: \"Scheherazade\"'>**{toupper(top_sports)}**</span>"
  )) %>%

  mutate(n_years = n_distinct(year)) %>%
  group_by(country) %>%
  filter(
    n_years - n_distinct(year[medal != "None"]) <= 4,
    n_years - n_distinct(year) <= 3
  ) %>%
  ungroup() %>%

  ggplot(aes(x = year, y = n, fill = medal)) +
  geom_col(data = ~filter(., medal != "None")) +
  geom_col(data = ~filter(., medal == "None"), width = 2.5) +
  geom_text(
    aes(label = year, y = -range / 25),
    colour = "grey99", angle = -90, size = 2.5, hjust = "left", fontface = "bold",
    data = ~count(., label, year, loss = medal == "None", wt = n) %>%
      group_by(label) %>%
      mutate(range = max(n[!loss]) - min(n[loss])) %>%
      ungroup() %>%
      filter(loss & abs(n) / range >= 0.4) %>%
      mutate(medal = NA)
  ) +
  geom_hline(yintercept = 0, size = 0.3, colour = "grey50") +
  scale_fill_manual(
    breaks = names(clr),
    values = unlist(clr),
    guide = FALSE
  ) +
  scale_y_continuous(
    #breaks = function(l) c(ceiling(l[1]), floor(l[2])),
    breaks = scales::extended_breaks(n = 5, Q = c(5, 10, 20)),
    labels = abs
  ) +
  facet_wrap(~label, scales = "free_y") +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    plot.title = element_text(size = 50),
    panel.grid.major.y = element_line(colour = "grey85", size = 0.2),
    axis.text.y = element_text(colour = "grey60", size = 8),
    axis.text.x = element_blank(),
    strip.text = element_textbox_simple(
      halign = 0.5, margin = margin(6, 0, 6, 0), size = 9
    ),
    plot.caption = element_textbox_simple(halign = 1),
    plot.background = element_rect(fill = "grey98", colour = "grey98", size = 30),
    panel.background = element_rect(fill = "grey98", colour = NA)
  ) +
  labs(
    x = NULL, y = NULL,
    title = "A test title",
    subtitle = paste(
      "This plot shows the performance of each country in the sport they",
      "have most excelled at in all olympic games since 1980. Countries are",
      "included based on the number of medals won and the frequency of their",
      "participation in events between 1980 and 2016.",
      sep = "\n"
    ),
    caption = "<br>Data from ***Kaggle*** | Visualisation by Jacob Scott"
  ) +
  inset_element(
    key, left = .78, bottom = .85, right = 1, top = 1,
    align_to = "full", clip = FALSE
  )

cowplot::ggdraw(plot) +
  cowplot::draw_image(
    olympics_logo,
    x = .47, y = .98, width = 0.22,
    hjust = 0, vjust = 1, halign = 1, valign = 1
  )


