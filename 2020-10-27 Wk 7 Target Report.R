library(tidyverse)
library(ggrepel)
library(ggimage)
library(nflfastR)

options(scipen = 9999)

data = readRDS(url('https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2020.rds'))

# Week Variable
curr_week = 7

# Receiver Stats
player_targets = data %>%
  filter(week <= curr_week, pass == 1, !is.na(air_yards)) %>%
  group_by(id, receiver) %>%
  summarize(
    team = last(posteam),
    catches = sum(complete_pass),
    targets = sum(pass),
    receiving_yards = sum(yards_gained),
    sum_ay = sum(air_yards),
  ) %>%
  arrange(-sum_ay)
# player_targets$YPR = player_targets$receiving_yards/player_targets$catches

#Drop ID
player_targets = player_targets[,-1]

# Team Stats
team_passes = data %>%
  filter(week <= curr_week, pass ==1, !is.na(air_yards)) %>%
  group_by(posteam) %>%
  summarize(
    team_passes = sum(pass),
    team_ay = sum(air_yards)
  )

# Join for Target Share & AY Share
player_targets = player_targets %>%
  left_join(team_passes, by = c('team' = 'posteam')) %>%
  left_join(teams_colors_logos, by = c('team' = 'team_abbr'))
player_targets$target_share = player_targets$targets/player_targets$team_passes
player_targets$ay_share = player_targets$sum_ay/player_targets$team_ay
player_targets$wopr = 1.5*player_targets$target_share + 0.7*player_targets$ay_share
player_targets$racr = player_targets$receiving_yards/player_targets$sum_ay

#Filter Target Share
player_targets = player_targets %>%
  filter(target_share >= 0.10 & targets >= curr_week*5) %>%
  arrange(-targets)

# Targets vs Target Share
player_targets %>%
  ggplot(aes(x = targets, y = target_share)) +
  #add points for the QBs with the right colors
  #cex controls point size and alpha the transparency (alpha = 1 is normal)
  geom_point(color = player_targets$team_color, cex = player_targets$catches/6, alpha = .6) +
  #add names using ggrepel, which tries to make them not overlap
  # geom_text_repel(aes(label=ifelse(targets>12,as.character(receiver),'')),hjust=0,vjust=0) +
  geom_text_repel(aes(label=receiver)) +
  #add a smooth line fitting cpoe + epa
  stat_smooth(geom='line', alpha=0.5, se=FALSE, method='lm')+
  #titles and caption
  labs(x = "Targets",
       y = "% of Target Share",
       title = "Fantasy WRs: Volume is King",
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
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5))


# Trying a table
# install.packages('gt')
library(gt)

player_targets = player_targets[,1:8]
player_targets$target_share = player_targets$targets/player_targets$team_passes
player_targets$ay_share = player_targets$sum_ay/player_targets$team_ay
player_targets$wopr = 1.5*player_targets$target_share + 0.7*player_targets$ay_share
player_targets$racr = player_targets$receiving_yards/player_targets$sum_ay
player_targets = player_targets[,-7:-8]

# Table emphasis? Default is Targets
player_targets = arrange(player_targets, -wopr)
# player_targets = arrange(player_targets, -racr)

customGreen0 = "#DeF7E9"
customGreen = "#71CA97"
customRed = "#ff7f7f"

# Make the table
player_targets %>% 
  # dplyr::slice(1:15) %>%
  # mutate(Rank = paste0('#',row_number())) %>%
  dplyr::slice(16:30) %>%
  mutate(Rank = paste0('#',row_number()+15)) %>%
  gt() %>%
  tab_header(title = 'Fantasy WRs: Volume is King') %>% 
  cols_move_to_start(columns = vars(Rank)) %>% 
  cols_label(
    receiver = 'Receiver',
    team = 'Team',
    catches = 'Rec',
    targets = 'Targ',
    receiving_yards = 'Yds',
    sum_ay = 'Air Yards',
    target_share = 'Target %',
    ay_share = 'Air Yard %',
    wopr = 'WOPR',
    racr = 'RACR'
  ) %>% 
  fmt_number(columns = vars(ay_share), decimals = 2) %>% 
  fmt_number(columns = vars(racr), decimals = 2) %>% 
  fmt_number(columns = vars(wopr), decimals = 2) %>%
  fmt_number(columns = vars(target_share), decimals = 2) %>% 
  tab_style(style = cell_text(size = 'x-large'), locations = cells_title(groups = 'title')) %>% 
  tab_style(style = cell_text(align = 'center', size = 'medium'), locations = cells_body()) %>% 
  tab_style(style = cell_text(align = 'left'), locations = cells_body(vars(receiver))) %>% 
  tab_source_note(source_note = '') %>% 
  data_color(
    columns = vars(target_share, ay_share, wopr),
    # columns = vars(receiving_yards, sum_ay, racr),
    # columns = vars(target_share, targets),
    colors = scales::col_numeric(palette = c(customGreen0, customGreen), domain = NULL),
    autocolor_text = FALSE
  ) %>%
  text_transform(
    locations = cells_body(vars(team)),
    fn = function(x) web_image(url = paste0('https://a.espncdn.com/i/teamlogos/nfl/500/',x,'.png'))
  ) %>% 
  cols_width(vars(team) ~ px(45)) %>% 
  tab_options(
    table.font.color = 'darkblue',
    data_row.padding = '2px',
    row_group.padding = '3px',
    column_labels.border.bottom.color = 'darkblue',
    column_labels.border.bottom.width = 1.4,
    table_body.border.top.color = 'darkblue',
    row_group.border.top.width = 1.5,
    row_group.border.top.color = '#999999',
    table_body.border.bottom.width = 0.7,
    table_body.border.bottom.color = '#999999',
    row_group.border.bottom.width = 1,
    row_group.border.bottom.color = 'darkblue',
    table.border.top.color = 'transparent',
    table.background.color = '#F2F2F2',
    table.border.bottom.color = 'transparent',
    row.striping.background_color = '#FFFFFF',
    row.striping.include_table_body = TRUE
  )
``