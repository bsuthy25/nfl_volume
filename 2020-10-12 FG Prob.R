library(tidyverse)
library(ggrepel)
library(ggimage)
library(nflfastR)

options(scipen = 9999)

# Load Multiple Seasons
seasons <- 2015:2020
pbp <- map_df(seasons, function(x) {
  readRDS(
    url(
      paste0("https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_",x,".rds")
    )
  )
})

# Add FG Zones
fg_data = pbp %>%
  filter(play_type == 'field_goal', field_goal_result != 'blocked') %>%
  mutate(
    field_pos = case_when(
      kick_distance <= 30 ~ "Under 30",
      kick_distance > 30 & kick_distance <= 35 ~ "30-35",
      kick_distance > 35 & kick_distance <= 40 ~ "35-40",
      kick_distance > 40 & kick_distance <= 45 ~ "40-45",
      kick_distance > 45 & kick_distance <= 50 ~ "45-50",
      kick_distance > 50 ~ "Over 50"
    )
  ) %>%
  select(field_pos, field_goal_result)

# Assign 1 an 0
fg_data$made[fg_data$field_goal_result == "made"] = 1
fg_data$made[fg_data$field_goal_result == "missed"] = 0

# Arrange field_pos
fg_data = fg_data %>% 
  mutate(field_pos = factor(field_pos, 
                            levels = c("Under 30","30-35","35-40","40-45","45-50", "Over 50")))

# Summarized by zone
sum_fg_data = fg_data %>%
  group_by(field_pos) %>%
  summarize(n = n(),
            makes = sum(made),
            pct = sum(made)/n())


# Pretty Table
customGreen0 = "#DeF7E9"
customGreen = "#71CA97"
customRed = "#ff7f7f"

library(gt)
# Make the table
sum_fg_data %>%
  gt() %>%
  tab_header(title = 'Field Goal % by Distance') %>% 
  cols_label(
    field_pos = 'Distance',
    n = 'Attempts',
    makes = 'Makes',
    pct = 'FG %',
  ) %>% 
  fmt_number(columns = vars(pct), decimals = 2) %>% 
  tab_style(style = cell_text(size = 'x-large'), locations = cells_title(groups = 'title')) %>% 
  tab_style(style = cell_text(align = 'center', size = 'medium'), locations = cells_body()) %>% 
  tab_source_note(source_note = '') %>% 
  data_color(
    columns = vars(pct),
    colors = scales::col_numeric(palette = c(customRed, customGreen), domain = NULL),
    autocolor_text = FALSE
  ) %>% 
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
