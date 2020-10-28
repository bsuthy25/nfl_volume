library(jsonlite)
library(tidyverse)
library(glue)

### ESPN League Information
### NOTE for this code to work you need to make your league "viewable to public" in league settings
# Ozzie's
league_id <- 208872 ### Copy this from your league's url
league_size <- 10

# # # Page's
# league_id <- 810644 ### Copy this from your league's url
# league_size <- 8

# #Anyone else's
# league_id <- 165895 ### Copy this from your league's url
# league_size <- 10

### Base URL
base_url <- "http://fantasy.espn.com/apis/v3/games/ffl/seasons/2020/segments/0/leagues"

### Teams data frame
y <- fromJSON(glue('{base_url}/{league_id}?view=mTeam'))
teams <- jsonlite::flatten(y$teams)
teams <- 
  select(teams, 
         'team_id' = id,
         'division_id' = divisionId,
         location,
         nickname,
         'abbreviation' = abbrev,
         'wins' = record.overall.wins,
         'losses' = record.overall.losses,
         'ties' = record.overall.ties) %>% 
  mutate('team' = paste(location, nickname, sep = '\n')) %>% 
  select(team_id, division_id, team, everything())

### Scores
x <- fromJSON(glue('{base_url}/{league_id}?view=mMatchup'))
scores <- 
  tibble('week' = x$schedule$matchupPeriodId,
         'home_id' = x$schedule$home$teamId,
         'away_id' = x$schedule$away$teamId,
         'home_score' = apply(x$schedule$home$pointsByScoringPeriod, 1, sum, na.rm = T),
         'away_score' = apply(x$schedule$away$pointsByScoringPeriod, 1, sum, na.rm = T))

### Join in team names
scores <- 
  scores %>% 
  inner_join(select(teams, team_id, team), by = c('home_id' = 'team_id')) %>% 
  rename('home_team' = 'team') %>% 
  inner_join(select(teams, team_id, team), by = c('away_id' = 'team_id')) %>% 
  rename('away_team' = 'team') %>% 
  mutate('result' = 
           case_when(home_score > away_score ~ home_team,
                     away_score > home_score ~ away_team,
                     home_score == away_score ~ 'tie')) %>% 
  filter(home_score > 0)

# # Page's League Workaround
# # write.csv(scores, file = "C:\\Users\\Brendan\\Documents\\Fantasy\\Page_Expected_Wins.csv")
# scores = read.csv('Page_Expected_Wins.csv')
# scores = scores[,-1]

### Actual Standings
wins <- 
  map_dbl(teams$team, ~{sum(scores$result == .x) + 
      0.5 * sum(scores$result == 'tie' & (scores$home_team == .x | scores$away_team == .x))})
n_weeks <- (nrow(scores) * 2 / league_size)
losses <-  n_weeks - wins
standings <- tibble('team' = teams$team,
                    'wins' = wins,
                    'losses' = losses)

### Figure out each team's rank in given week
week_ranks <- 
  select(scores, week, 'team' = home_team, 'score' = home_score) %>% 
  bind_rows(select(scores, week, 'team' = away_team, 'score' = away_score)) %>% 
  group_by(week) %>% 
  mutate('rank' = rank(desc(score), ties.method = 'average'))

### Expected Records
exp_standings <- 
  group_by(week_ranks, team) %>% 
  summarise('exp_wins' = sum((league_size - rank)/(league_size - 1)),
            'pts_per_week' = mean(score)) %>% 
  ungroup()

### Make Plot
df <- inner_join(standings, exp_standings, by = 'team')
theme_set(theme_bw() + 
            theme(plot.title = element_text(size = 16, hjust = 0.5),
                  axis.title = element_text(size = 14),
                  plot.subtitle = element_text(size = 12, hjust = 0.5),
                  legend.position = "bottom"))

ggplot(df, aes(x = exp_wins, y = wins)) + 
  geom_abline(intercept = 0, slope = 1, lty = 2, col = 'black') +
  geom_label(aes(label = team, fill = pts_per_week), alpha = 0.6, size = 3) +
  scale_fill_viridis_c()  +
  labs(x = 'Expected Wins',
       y = 'Actual Wins',
       fill = 'Points/Week',
       # title = 'Region League Expected Wins',
       title = 'Valpo Crew Expected Wins',
       # subtitle = glue('ESPN LeagueID: {league_id}'),
       caption = 'Code: @recspecs730 | Data: ESPN Fantasy Football API') +
  scale_x_continuous(limits = c(0, n_weeks), breaks = 0:n_weeks) +
  scale_y_continuous(limits = c(0, n_weeks), breaks = 0:n_weeks) 

# ggsave('region_league_week_7.png', height = 9/1.5, width = 16/1.5)  
# ggsave('valpo_crew_week_7.png', height = 9/1.5, width = 16/1.5)  