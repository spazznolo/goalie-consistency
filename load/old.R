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