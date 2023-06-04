
# Required packages: dplyr, MASS, fitdistrplus, ggplot2; loaded with 'load/libraries.R'

# Required objects:
# - shots (tibble): Contains shot data; created with 'load/data.R'
# - single_color (string): Specifies the fill color for single color plots; created with 'load/functions.R'
# - multiple_colors (function): Specifies the fill color for gradients; created with 'load/functions.R'


# Load necessary libraries from the 'libraries.R' file
source('load/libraries.R')  

# Load custom functions from the 'functions.R' file
source('load/functions.R')  

# Load data from the 'data.R' file
source('load/data.R')

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

# Save the table as a PNG file
gtsave(
  gt_std_pts, # Specify the table name
  "goalie-two-one.png", # Specify the file name
  path = '/Users/ada/Documents/projects/spazznolo.github.io/figs', # Specify the file path
  expand = 0 # Remove border
)


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


# Save the table as a PNG file
gtsave(
  gt_std_pts, # Specify the table name
  "goalie-two-two.png", # Specify the file name
  path = '/Users/ada/Documents/projects/spazznolo.github.io/figs', # Specify the file path
  expand = 0 # Remove border
)
