

## game-to-game consistency


# load everything
source('libraries.R')
source('functions.R')


## game-to-game consistency

# create goalie dictionary
goalie_index <-
  tibble(
    goalie = LETTERS[1:5],
    goals = seq(2.75, 4.75, 0.50)
  )

# save results
results <- map2_dfr(goalie_index$goalie, goalie_index$goals, poission_season_simulations)

# numerically compare average standing points for each goalie
gt_std_pts <-
  results %>%
  group_by(Goalie = goalie) %>%
  summarize(
    `Expected Points` = round(mean(standing_points), 2),
    `Std Dev` = round(sd(standing_points), 2)
  ) %>%
  gt() %>%
  tab_options(
    table.background.color = 'black'
  )

gtsave(gt_std_pts, "goalie-one-one.png", path = '/Users/ada/Documents/GitHub/spazznolo.github.io/figs')

# visually compare average standing points for each goalie
results %>%
  rename(Goalie = goalie) %>%
  ggplot(aes(x = standing_points, y = rev(Goalie), fill = Goalie)) +
  geom_density_ridges(alpha = 0.7) +
  scale_x_continuous(limits = c(60, 120)) +
  labs(x = '\nStanding Points', y = 'Goalie\n') +
  scale_fill_manual(values = rev(heat.colors(10))) +
  scale_color_manual(values = rev(heat.colors(10))) +
  dark_theme() +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid.major = element_line(color = 'black'),
    panel.grid.minor = element_line(color = 'black')) 

ggsave(filename = 'goalie-one-two.png', path = '/Users/ada/Documents/GitHub/spazznolo.github.io/figs', width = 5, height = 3, device = 'png', dpi = 320)

# check significance
summary(lm(standing_points ~ 0 + goalie, results))


# apply generalized solution
inconsistent_game_results <- rpois(820000, 6/2) - rpois(820000, sample(seq(0, 6, 0.5), 820000, replace = TRUE))
inconsistent_season_results <- split(inconsistent_game_results, ceiling(seq_along(inconsistent_game_results)/82))
inconsistent_pts <- unlist(lapply(inconsistent_season_results, function(x) (sum(x > 0)*2) + (sum(x == 0)*1.5)))

average_game_results <- rpois(820000, 6/2) - rpois(820000, 6/2)
average_season_results <- split(average_game_results, ceiling(seq_along(average_game_results)/82))
average_pts <- unlist(lapply(average_season_results, function(x) (sum(x > 0)*2) + (sum(x == 0)*1.5)))

steady_game_results <- rpois(820000, 6/2) - 3
steady_season_results <- split(steady_game_results, ceiling(seq_along(steady_game_results)/82))
steady_pts <- unlist(lapply(steady_season_results, function(x) (sum(x > 0)*2) + (sum(x == 0)*1.5)))

tibble('inconsistent' = inconsistent_pts, 'average' = average_pts, 'steady' = steady_pts) %>%
  pivot_longer(inconsistent:steady) %>%
  mutate(Goalie = factor(name, levels = c('steady', 'average', 'inconsistent'))) %>%
  ggplot +
  geom_density(aes(x = value, group = Goalie, fill = Goalie), alpha = 0.6) +
  labs(x = '\nStanding Points', y = 'Density\n') +
  theme_minimal() +
  scale_fill_manual(values = (heat.colors(3))) +
  dark_theme() +
  theme(
    panel.grid.major = element_line(color = 'black'),
    panel.grid.minor = element_line(color = 'black')) 

ggsave(filename = 'goalie-one-three.png', path = '/Users/ada/Documents/GitHub/spazznolo.github.io/figs', width = 5, height = 3, device = 'png', dpi = 320)

