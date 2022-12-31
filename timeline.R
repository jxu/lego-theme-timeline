library(IRanges)
library(tidyverse)

# data import
sets_raw        <- read_csv("sets.csv")
themes_raw      <- read_csv("themes.csv")
selected_themes <- read_csv("selected_themes.csv")

# recursive parent lookup. credit: zephryl
find_ancestor <- function(target_id) {
  dat <- cur_data()
  parent <- filter(dat, id == target_id)$parent_id
  if (is.na(parent)) target_id
  else find_ancestor(parent)
}

# add (ancestor) theme name to sets table
themes <- themes_raw %>% 
  mutate(ancestor_id = map_dbl(id, find_ancestor))

themes <- themes %>%
  left_join(themes %>% select(ancestor_id = id, ancestor_name = name))

sets <- sets_raw %>%
  left_join(themes %>% select(theme_id = id, ancestor_name))

# compute year run ranges
run_range_cols <- function() {
  y <- cur_data()$year
  y <- y %>% unique %>% sort
  IRanges::reduce(IRanges(y)) %>% data.frame %>% select(start, end)
}

theme_years <- sets %>% 
  group_by(theme = ancestor_name) %>%
  summarize(rr = run_range_cols()) %>%
  unpack(cols = rr) 

# plot selected themes
theme_years %>%
  filter(theme %in% selected_themes$theme) %>%
  # end+1 to show bars until end of year
  ggplot(aes(x=start, xend=end+1, y=theme, yend=theme, color=theme)) +
  geom_segment(size=2, show.legend = FALSE) + 
  scale_y_discrete(limits=rev) + 
  theme_bw()
