library(IRanges)
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
  mutate(ancestor_id = map_dbl(id, find_ancestor))

themes <- themes %>%
  left_join(themes %>% select(ancestor_id = id, ancestor_name = name))

sets <- sets_raw %>%
  left_join(themes %>% select(theme_id = id, ancestor_name))


run_range_cols <- function() {
  y <- cur_data()$year
  y <- y %>% unique %>% sort
  IRanges::reduce(IRanges(y)) %>% data.frame %>% select(start, end)
}


sets %>% 
  group_by(theme = ancestor_name) %>%
  summarize(rr = run_range_cols()) %>%
  unpack(cols = rr) 
