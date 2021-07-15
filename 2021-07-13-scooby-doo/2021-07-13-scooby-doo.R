library(tidyverse)
library(patchwork)
library(ggstream)

tuesdata <- tidytuesdayR::tt_load('2021-07-13')
scoobydoo <- tuesdata$scoobydoo %>%
  mutate(across(where(is.character), ~ifelse(. == "NULL", NA_character_, .)))

# 1. Data Prep ---------------------------------------------------------------
# Do some data prep - break main dataset down into small tables, each in long
# /tidy format
fact <- scoobydoo %>%
  select(
    index, series_name, network, season, title, imdb, engagement, date_aired,
    run_time, format
  ) %>%
  mutate(across(
    c(imdb, engagement),
    ~as.numeric(ifelse(. == "NULL", NA_character_, .))
  ))

monsters <- scoobydoo %>%
  select(index, contains("monster")) %>%
  rename_with(~str_remove(., "monster_")) %>%
  mutate(real = as.logical(real)) %>%
  mutate(across(
    c(name, gender, type, subtype, species),
    ~replace_na(., "Unknown") %>% str_remove(",$")
  )) %>%
  mutate(across(
    c(name, gender, type, subtype, species),
    str_count, ",", .names = "{col}_commas"
  )) %>%
  mutate(n = pmax(name_commas, gender_commas, type_commas, subtype_commas, species_commas)) %>%
  mutate(across(
    c(name, gender, type, subtype, species),
    ~paste0(., strrep(",Unknown", n - get(paste0(cur_column(), "_commas"))))
  )) %>%
  select(-n, -ends_with("commas")) %>%
  separate_rows(name, gender, type, subtype, species, sep = ",") %>%
  mutate(type = case_when(
    grepl("Disguise|Disugise", type) ~ "Disguised",
    TRUE ~ type
  ))


caught <- scoobydoo %>%
  select(index, contains("caught")) %>%
  mutate(across(contains("caught"), as.logical)) %>%
  pivot_longer(
    contains("caught"),
    names_to = "caught_by",
    values_to = "flag",
    names_prefix = "caught_"
  ) %>%
  filter(flag) %>%
  select(-flag) %>%
  mutate(caught_by = ifelse(caught_by == "caught", "not caught", caught_by))

captured <- scoobydoo %>%
  select(index, contains("captured")) %>%
  mutate(across(contains("captured"), as.logical)) %>%
  pivot_longer(
    contains("captured"),
    names_to = "captured_by",
    values_to = "flag",
    names_prefix = "captured_"
  ) %>%
  filter(flag) %>%
  select(-flag)

unmasked <- scoobydoo %>%
  select(index, contains("unmask")) %>%
  mutate(across(contains("unmask"), as.logical)) %>%
  pivot_longer(
    contains("unmask"),
    names_to = "unmasked_by",
    values_to = "flag",
    names_prefix = "unmask_"
  ) %>%
  filter(flag) %>%
  select(-flag)

snack <- scoobydoo %>%
  select(index, contains("snack")) %>%
  mutate(across(contains("snack"), as.logical)) %>%
  pivot_longer(
    contains("snack"),
    names_to = "snack_eaten_by",
    values_to = "flag",
    names_prefix = "snack_"
  ) %>%
  filter(flag) %>%
  select(-flag)

setting <- scoobydoo %>%
  select(index, contains("setting")) %>%
  rename_with(~str_remove(., "setting_"))

suspects <- scoobydoo %>%
  select(index, contains(c("culprit", "suspect")), motive, arrested)

catchphrase <- scoobydoo %>%
  select(
    index, split_up, another_mystery, set_a_trap,
    jeepers, jinkies, my_glasses, just_about_wrapped_up, zoinks, groovy,
    scooby_doo_where_are_you, rooby_rooby_roo
  ) %>%
  mutate(across(-index, replace_na, 0)) %>%
  mutate(across(-index, as.numeric)) %>%
  pivot_longer(-index, names_to = "catchphrase", values_to = "times_said") %>%
  mutate(catchphrase = str_replace(catchphrase, "_", " "))

taglines <- scoobydoo %>%
  select(index, if_it_wasnt_for, and_that) %>%
  filter(!if_all(-index, is.na)) %>%
  mutate(if_it_wasnt_for1 = ifelse(
    grepl("meddling kids", if_it_wasnt_for),
    "you meddling kids",
    if_it_wasnt_for
  ))

minor_characters <- scoobydoo %>%
  select(index, batman, scooby_dum, scrappy_doo, hex_girls, blue_falcon) %>%
  pivot_longer(-index, names_to = "minor_character", values_to = "flag") %>%
  filter(flag) %>%
  select(-flag) %>%
  mutate(minor_character = str_replace(minor_character, "_", " ") %>% str_to_title())

voice_actors <- scoobydoo %>%
  select(index, ends_with("va")) %>%
  pivot_longer(
    -index,
    names_to = "character",
    values_to = "voice_actor",
    names_pattern = "(.+)_va"
  ) %>%
  mutate(character = str_to_title(character))

# 2. Analysis ----------------------------------------------------------------
fact %>%
  filter(format == "TV Series") %>%
  arrange(date_aired) %>%
  ggplot(aes(x = date_aired, y = imdb, colour = series_name)) +
  geom_point(na.rm = TRUE) +
  scale_colour_viridis_d()

fact %>%
  filter(!is.na(imdb)) %>%
  ggplot(aes(x = fct_reorder(series_name, imdb), y = imdb)) +
  geom_boxplot() +
  geom_point(aes(colour = season), alpha = 0.5) +
  coord_flip() +
  scale_colour_viridis_d()

fact %>%
  left_join(suspects, by = "index") %>%
  filter(!is.na(motive)) %>%
  group_by(
    month = lubridate::floor_date(date_aired, "month"),
    motive = ifelse(
      motive %in% c("Theft" ,
                    "Treasure"   ,
                    "Conquer"   ,
                    "Natural Resource",
                    "Smuggling"       ,
                    "Trespassing"    ,
                    "Abduction"       ), motive, "Other"
    )) %>%
  summarise(n = n(), .groups = "drop") %>%
  ggplot(aes(x = month, y = n, fill = motive)) +
  geom_stream(type = "proportional") +
  scale_fill_viridis_d()

test <- function(col) {
  fact %>%
    left_join(monsters, by = "index") %>%
    group_by(month = lubridate::floor_date(date_aired, "month"), {{ col }}) %>%
    summarise(n = n(), .groups = "drop") %>%
    ggplot(aes(x = month, y = n, fill = {{ col }})) +
    geom_stream(type = "proportional") +
    scale_fill_viridis_d()

}

test(type)

fact %>%
  left_join(minor_characters, by = "index") %>%
  filter(!is.na(minor_character)) %>%
  group_by(quarter = lubridate::quarter(date_aired, with_year = T), minor_character) %>%
  summarise(n = n(), .groups = "drop") %>%
  ggplot(aes(x = quarter, y = n, fill = minor_character)) +
  geom_stream() +
  scale_fill_viridis_d()

fact %>%
  filter(!is.na(imdb)) %>%
  group_by(quarter = lubridate::quarter(date_aired, with_year = T)) %>%
  summarise(imdb = median(imdb)) %>%
  ggplot(aes(x = quarter, y = imdb)) +
  geom_point() +
  geom_smooth()
