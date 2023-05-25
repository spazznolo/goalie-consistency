
# Load shot data for years 2007-2008 to 2022-2023
shots <- 
  map_dfr(c('data/shots_2007-2021.csv', 'data/shots_2022.csv'), 
          ~read_csv(.) %>%
            clean_names() %>% # Clean column names and make them consistent
            filter(home_skaters_on_ice == 5, away_skaters_on_ice == 5) %>% # Filter data for even strength (5v5)
            select(season, game_id, goalie_name = goalie_name_for_shot, goal, x_goal)) # Select relevant columns
  