library(tidyverse)
library(ggrepel)
library(ggimage)
library(nflfastR)

options(scipen = 9999)

data = readRDS(url('https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2020.rds'))

# Week Variable
curr_week = 6

# QB Stats
qbs <- data %>%
  filter(week <= curr_week, pass==1) %>%
  group_by(id, name) %>%
  summarize(
    pass_yards = sum(yards_gained),
    pass_tds = sum(pass_touchdown),
    int = sum(interception),
    n_dropbacks = sum(pass),
    team = last(posteam)
  )

sack_stats = data %>%
  filter(sack == 1) %>%
  group_by(id, name) %>%
  summarize(
    sack_yards = sum(yards_gained),
    sacks = sum(sack)
  )

# Join for Sack Stats and Colors
qbs = qbs %>%
  left_join(sack_stats, by = c('id' = 'id')) %>%
  left_join(teams_colors_logos, by = c('team' = 'team_abbr')) %>%
  filter(n_dropbacks > 60)

#Calculate ANY/A and sort
qbs$ANY_A = (qbs$pass_yards + 20*qbs$pass_tds - 45*qbs$int - qbs$sack_yards)/(qbs$n_dropbacks + qbs$sacks)
qbs = arrange(qbs, -ANY_A)

#Visualize
# Top Air Yard Viz
ggplot(qbs, aes(x=name.x, y=ANY_A)) + 
  geom_bar(aes(reorder(name.x,-ANY_A)),stat="identity", fill=qbs$team_color) +
  geom_image(aes(image = team_logo_espn), asp = 16 / 9) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1),
        plot.title = element_text(size = 14, hjust = 0.5, face = "bold")) +
  labs(x = "Quarterback",
       y = "Adjust Net Yards per Passing Att",
       title = "Another QB Metric - ANY/A",
       caption = "Author: @brendan_nfl | Data: @nflfastR")
  
  