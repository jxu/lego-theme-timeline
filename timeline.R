library(tidyverse)

sets_raw <- read_csv("sets.csv")
themes_raw <- read_csv("themes.csv")

themes <- 
  themes_raw %>% 
  rename(theme_id = id,
         theme_name = name,
         parent_theme_id = parent_id) %>% 
  add_column(ancestor_theme_id = NA)

for (i in 1:nrow(themes)) {
  orig_id <- themes[i,]$theme_id
  cur_id <- orig_id
  repeat {
    par_id <- themes[themes$theme_id == cur_id,]$parent_theme_id
    if (is.na(par_id)) break
    cur_id <- par_id
  }
 
  themes[themes$theme_id == orig_id,]$ancestor_theme_id = cur_id
}

themes <- themes %>%
  left_join(themes %>% select(theme_id, theme_name),
            by = c("ancestor_theme_id" = "theme_id")) %>%
  rename(ancestor_name = theme_name.y)
