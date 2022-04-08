
tibble(
  score_home = rpois(1000000, 6/2),
  score_away = rpois(1000000, 6/2) 
) %>%
  group_by(score_away) %>%
  summarize(
    pt_pct = ((sum(score_home > score_away)*2) + (sum(score_home == score_away)*1.5))/(n())
  )

read_csv('Untitled.csv') %>%
  clean_names %>%
  mutate(
    g_3 = ifelse(!is.na(x6) & g_5 < g_3, g_5, g_3),
    g_5 = ifelse(!is.na(x6) & g_5 > g_3, g_3, g_5)
    ) %>%
  group_by(g_3) %>%
  summarize(
    pt_pct = ((sum(g_5 > g_3)*2) + (sum(g_5 == g_3)*1.5))/(n())
  )







