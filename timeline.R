library(tidyverse)

sets_raw <- read_csv("sets.csv")
themes_raw <- read_csv("themes.csv")

# recursive parent lookup. credit: zephryl
find_ancestor <- function(target_id) {
  dat <- cur_data()
  parent <- filter(dat, id == target_id)$parent_id
  if (is.na(parent)) target_id
  else find_ancestor(parent)
}

themes <- themes_raw %>% 
  mutate(ancestor_theme_id = map_dbl(id, find_ancestor)) 
