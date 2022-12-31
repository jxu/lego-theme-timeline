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
  left_join(select(themes, ancestor_id = id, ancestor_name = name))

sets <- sets_raw %>%
  left_join(select(themes, theme_id = id, ancestor_name))

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

# sort theme by introduction year
theme_intro <- theme_years %>%
  group_by(theme) %>%
  summarize(intro = min(start)) %>%
  arrange(intro)

theme_order <- theme_intro$theme
selected_theme_order <- theme_intro %>%
  filter(theme %in% selected_themes$theme) %>%
  pull(theme)

# plot selected themes
color_map <- selected_themes$color
names(color_map) <- selected_themes$theme

# simple ggplot theming with centered title
theme_set(theme_bw() + theme(plot.title = element_text(hjust = 0.5)))

theme_years %>%
  filter(theme %in% selected_themes$theme) %>%
  # end+1 to show bars until end of year
  ggplot(aes(x=start, xend=end+1, y=theme, yend=theme, color=theme)) +
  geom_segment(linewidth=3, show.legend = FALSE) + 
  scale_y_discrete(limits=rev(selected_theme_order), position="right") + 
  scale_x_continuous(expand = c(0,0), breaks=seq(1950, 2030, 10)) +
  coord_cartesian(xlim=c(1945,2024)) + 
  scale_color_manual(values=color_map) +
  labs(title="Selected LEGO Themes Timeline", x="Year", y="Theme")

ggsave("selected_timeline.png", width=3000, height=1500, units="px")

# plot all themes in giant timeline
theme_years %>%
  ggplot(aes(x=start, xend=end+1, y=theme, yend=theme, color=theme)) + 
  geom_segment(linewidth=3, show.legend=F) +
  scale_y_discrete(limits=rev(theme_order), position="right") + 
  scale_x_continuous(expand = c(0,0), breaks=seq(1950, 2030, 10)) +
  coord_cartesian(xlim=c(1945,2024)) +
  labs(title="All LEGO Themes Timeline", x="Year", y="Theme")

ggsave("all_timeline.png", width=3000, height=5000, units="px")
