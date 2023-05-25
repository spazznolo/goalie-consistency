

## shot-to-shot consistency


# create sample seasons for goalies each with a .900 save %
season_shots = rep(c(rep(0, 9), 1), 82*30)
list_of_shot_sims <- replicate(10000, sample(season_shots, length(season_shots), replace = FALSE))
shot_sims <- lapply(1:ncol(list_of_shot_sims), function(i) list_of_shot_sims[, i])

# calculate entropy of sample seasons
ent_results <- map_dbl(shot_sims, norm_ent)

# simulate seasons
standing_pts <- map_dbl(shot_sims, std_pts)

# check correlation (0.011 on last run)
cor(ent_results, standing_pts)

# check significance
summary(lm(standing_pts ~ ent_results, tibble(ent_results, standing_pts)))

# visualize relationship between inter-shot entropy and expected standing points
tibble(ent_results, standing_pts) %>%
  arrange(ent_results) %>%
  mutate(ent_group =  cut(ent_results, breaks = quantile(ent_results, seq(0, 1, l=1000)), include.lowest = TRUE)) %>%
  group_by(ent_group) %>%
  summarize(
    standing_pts = mean(standing_pts),
    ent_results = mean(ent_results)
  ) %>%
  ggplot() +
  geom_point(aes(ent_results, standing_pts/10), alpha = 0.75, col = single_color) +
  labs(x = '\nEntropy', y = 'Expected Standing Points\n') +
  dark_theme() +
  theme(
    panel.grid.major = element_line(color = 'black'),
    panel.grid.minor = element_line(color = 'black'))  +
  annotate("text", x = 0.0445, y = 84.5, col = 'white',
           label = paste('r = ',round(cor(ent_results, standing_pts),3),sep=' '), size = 3)

ggsave(filename = 'goalie-one-threee.png', path = '/Users/ada/Documents/GitHub/spazznolo.github.io/figs', width = 5, height = 3, device = 'png', dpi = 320)

# visualize relationship between inter-shot entropy and expected standing points
tibble(ent_results, standing_pts) %>%
  arrange(ent_results) %>%
  mutate(`Entropy Grouping` =  cut(ent_results, breaks = quantile(ent_results, seq(0, 1, l=10)), include.lowest = TRUE)) %>%
  ggplot() +
  geom_density(aes(x = standing_pts/10, group = `Entropy Grouping`, fill = `Entropy Grouping`), alpha = 0.4) +
  labs(x = '\nStanding Points', y = 'Density\n') +
  scale_fill_manual(values = (heat.colors(10))) +
  dark_theme() +
  theme(
    panel.grid.major = element_line(color = 'black'),
    panel.grid.minor = element_line(color = 'black')) 

ggsave(filename = 'goalie-one-four.png', path = '/Users/ada/Documents/GitHub/spazznolo.github.io/figs', width = 5, height = 3, device = 'png', dpi = 320)

