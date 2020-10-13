library(tidyverse)
library(ggrepel)
library(ggimage)
library(nflfastR)

options(scipen = 9999)

data = readRDS(url('https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2020.rds'))

# Week Variable
curr_week = 5

# Rusher Stats
player_carries = data %>%
  filter(week <= curr_week, rush == 1) %>%
  group_by(id, rusher) %>%
  summarize(
    carries = sum(rush),
    rush_yards = sum(yards_gained),
    YPC = sum(yards_gained)/sum(rush),
    team = last(posteam)
  )

# Team Stats
team_carries = data %>%
  filter(week <= curr_week, rush ==1) %>%
  group_by(posteam) %>%
  summarize(
    team_carries = sum(rush)
  )

# RB Targets
rb_targets = data %>%
  filter(week <= curr_week, pass == 1, !is.na(air_yards)) %>%
  group_by(receiver_id, receiver) %>%
  summarize(
    team = last(posteam),
    catches = sum(complete_pass),
    targets = sum(pass)
  )
  

# Joins
player_carries = player_carries %>%
  left_join(team_carries, by = c('team' = 'posteam')) %>%
  left_join(rb_targets, by = c('id' = 'receiver_id')) %>%
  left_join(teams_colors_logos, by = c('team.x' = 'team_abbr'))

player_carries$carry_share = player_carries$carries/player_carries$team_carries

# Filter
player_carries = player_carries %>%
  filter(carries > 30)

# RB Carries vs Targets
player_carries %>%
  ggplot(aes(x = carries, y = targets)) +
  #horizontal line with mean EPA
  #add points for the QBs with the right colors
  #cex controls point size and alpha the transparency (alpha = 1 is normal)
  geom_point(color = player_carries$team_color, cex = player_carries$carry_share*7.5, alpha = .6) +
  #add names using ggrepel, which tries to make them not overlap
  # geom_text_repel(aes(label=ifelse(carries>40,as.character(rusher),'')),hjust=0,vjust=0) +
  geom_text_repel(aes(label=rusher)) +
  # #add a smooth line fitting cpoe + epa
  stat_smooth(geom='line', alpha=0.5, se=FALSE, method='lm')+
  #titles and caption
  labs(x = "Carries",
       y = "Targets",
       title = "Fantasy RBs: Volume is King",
       caption = "Data: @nflfastR") +
  #uses the black and white ggplot theme
  theme_bw() +
  #center title with hjust = 0.5
  theme(
    plot.title = element_text(size = 14, hjust = 0.5, face = "bold")
  ) +
  #make ticks look nice
  #if this doesn't work, `install.packages('scales')`
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))
