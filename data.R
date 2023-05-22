
# insert download directory here
download_dir <- '~/Downloads/'

# download moneypuck data
#temp <- tempfile()
#download.file("https://peter-tanner.com/moneypuck/downloads/shots_2007-2021.zip", temp)
#unzip(temp, exdir = download_dir)

# create analysis set
shots <- 
  read_csv(paste0(download_dir, "shots_2007-2021.csv")) %>%
  clean_names() %>%
  select(season, game_id, goalie_id = goalie_id_for_shot, goalie_name = goalie_name_for_shot, goal, x_goal,
         home_skaters_on_ice, away_skaters_on_ice) %>%
  filter(home_skaters_on_ice == 5, away_skaters_on_ice == 5) 

goalie_summary <-
  shots %>%
  group_by(goalie_id, goalie_name, season) %>%
  summarize(
    shots = n(), 
    g = sum(goal), 
    save_pct = (shots - g)/shots,
    x_g = sum(x_goal), 
    x_gsaa = sum(x_goal) - sum(goal),
    x_gsaa_s = x_gsaa/shots,
    x_gsaa_xg = x_gsaa/x_g,
    .groups = 'drop'
    ) %>%
  filter(shots >= 800) %>%
  drop_na

