
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



# Create a progress bar to track the calculation progress
pb <- progress_bar$new(
  format = "  calculating entropy [:bar] :percent in :elapsed",
  total = nrow(goalie_summary),
  clear = TRUE,
  width = 71
)

# Calculate the summary statistics for goalie performance for goalies facing over 800 shots
goalie_summary <- shots %>%
  group_by(goalie_name, season) %>%
  summarize(
    shots = n(),  # Count the number of shots
    g = sum(goal),  # Sum the number of goals
    save_pct = (shots - g) / shots,  # Calculate the save percentage
    x_g = sum(x_goal),  # Sum the expected goals
    x_gsaa = sum(x_goal) - sum(goal),  # Calculate the goals saved above average
    x_gsaa_s = x_gsaa / shots,  # Calculate the goals saved above average per shot
    x_gsaa_xg = x_gsaa / x_g,  # Calculate the goals saved above average per expected goal
    .groups = 'drop'
  ) %>%
  filter(shots >= 800)  # Filter for goalies with at least 800 shots
drop_na()  # Drop any rows with missing values


# Calculate normalized entropy scores for goalie seasons
ent_results <- 
  shots %>%
  inner_join(goalie_summary, by = c('goalie_name', 'season')) %>%
  group_by(goalie_name, season, shots, g, x_g, x_gsaa, x_gsaa_s, x_gsaa_xg) %>%
  summarize(ent_pct = ent_test_xg(goal, x_goal, x_gsaa_xg), .groups = 'drop')

# goalie-season entropy to gsax
ent_results %>%
  ggplot(aes(ent_pct*100, x_gsaa_xg)) +
  geom_point(col = single_color, alpha = 0.7) +
  labs(x = '\nEntropy Percentile', y = 'Season GSAx/xG\n') +
  scale_x_continuous(limits = c(0, 100)) +
  dark_theme() +
  theme(
    panel.grid.major = element_line(color = 'black'),
    panel.grid.minor = element_line(color = 'black')) +
  annotate("text", x = 10, y = -0.35, col = 'white',
           label = paste('r = ',round(cor(ent_results$ent_pct, ent_results$x_gsaa_xg),3),sep=' '), size = 3)

# Save the plot as a PNG file
ggsave(
  filename = 'goalie-three-one.png',  # Specify the file name
  path = '/Users/ada/Documents/projects/spazznolo.github.io/figs',  # Specify the file path
  width = 5,  # Set the width of the plot
  height = 3,  # Set the height of the plot
  device = 'png',  # Specify the device to use for saving (PNG format)
  dpi = 320  # Set the resolution of the plot
)

# Check significance of relationship between entropy and performance
summary(lm(x_gsaa_xg ~ ent_pct, ent_results))


# goalie-season entropy to gsax
ent_results %>%
  group_by(goalie_name) %>%
  mutate(ent_pct_lead = lead(ent_pct)) %>%
  ungroup() %>%
  drop_na() %>%
  ggplot(aes(ent_pct*100, ent_pct_lead*100)) +
  geom_point(col = single_color, alpha = 0.7) +
  labs(x = '\nEntropy Percentile', y = 'Season GSAx/xG\n') +
  scale_x_continuous(limits = c(0, 100)) +
  dark_theme() +
  theme(
    panel.grid.major = element_line(color = 'black'),
    panel.grid.minor = element_line(color = 'black'))

# Save the table as a PNG file
gtsave(
  gt_std_pts, # Specify the table name
  "goalie-three-two.png", # Specify the file name
  path = '/Users/ada/Documents/projects/spazznolo.github.io/figs', # Specify the file path
  expand = 0 # Remove border
)

summary(lm(x_gsaa_xg ~ ent_pct, ent_results))


# goalie career summaries
career_entropy <- 
  ent_results %>%
  group_by(goalie_name) %>%
  filter(n() >= 5) %>%
  mutate(lag_x_gsaa_xg = lag(x_gsaa_xg)) %>%
  drop_na %>%
  summarize(
    seasons = n(),
    mean_ent = mean(ent_pct), 
    x_gsaa_xg_cor = cor(x_gsaa_xg, lag_x_gsaa_xg), 
    x_gsaa_xg_diff = mean(abs(x_gsaa_xg- lag_x_gsaa_xg)/lag_x_gsaa_xg), 
    x_gsaa_xg = mean(x_gsaa_xg)
  ) %>%
  ungroup() %>%
  drop_na


# streakiest
career_entropy %>% 
  slice_max(mean_ent, n = 10) %>%
  ggplot(aes(reorder(goalie_name, mean_ent), 100*mean_ent - 50, fill = reorder(goalie_name, mean_ent))) + 
  geom_bar(stat='identity',col='black',alpha = 0.7, size = 0.05) +
  scale_y_continuous(breaks = seq(0, 100, 5), labels = function(y) y + 50) +
  scale_fill_manual(values = multiple_colors(10)) +
  labs(x=NULL,y='\nCareer Average Entropy Percentile') +
  coord_flip() +
  guides(fill = 'none') +
  dark_theme() +
  theme(
    panel.grid.major = element_line(color = 'black'),
    panel.grid.minor = element_line(color = 'black')) 

# Save the plot as a PNG file
ggsave(
  filename = 'goalie-three-two.png',  # Specify the file name
  path = '/Users/ada/Documents/projects/spazznolo.github.io/figs',  # Specify the file path
  width = 5,  # Set the width of the plot
  height = 3,  # Set the height of the plot
  device = 'png',  # Specify the device to use for saving (PNG format)
  dpi = 320  # Set the resolution of the plot
)


# least streaky goalie careers (weighted by xg)
career_entropy %>% 
  slice_min(mean_ent, n = 10) %>%
  ggplot(aes(reorder(goalie_name, -mean_ent), 100*mean_ent, fill = reorder(goalie_name, -mean_ent))) + 
  geom_bar(stat='identity',col='black',alpha = 0.7, size = 0.05) +
  scale_y_continuous(breaks = seq(0, 100, 5), labels = function(y) y) +
  scale_fill_manual(values = multiple_colors(10)) +
  labs(x=NULL,y='\nCareer Average Entropy Percentile') +
  coord_flip() +
  guides(fill = 'none') +
  dark_theme() +
  theme(
    panel.grid.major = element_line(color = 'black'),
    panel.grid.minor = element_line(color = 'black')) 

# Save the plot as a PNG file
ggsave(
  filename = 'goalie-three-three.png',  # Specify the file name
  path = '/Users/ada/Documents/projects/spazznolo.github.io/figs',  # Specify the file path
  width = 5,  # Set the width of the plot
  height = 3,  # Set the height of the plot
  device = 'png',  # Specify the device to use for saving (PNG format)
  dpi = 320  # Set the resolution of the plot
)


career_entropy %>%
  arrange(mean_ent) %>%
  ggplot(aes(mean_ent*100, x_gsaa_xg)) +
  geom_point(col = single_color, alpha = 0.7) +
  labs(x = '\nCareer Average Entropy Percentile', y = 'Career Average GSAx/xG\n') +
  scale_x_continuous(limits = c(25, 75)) +
  dark_theme() +
  theme(
    panel.grid.major = element_line(color = 'black'),
    panel.grid.minor = element_line(color = 'black')) +
  annotate("text", x = 32, y = -0.075, col = 'white',
           label = paste('r = ',round(cor(career_entropy$mean_ent, career_entropy$x_gsaa_xg),3),sep=' '), size = 3)

# Save the plot as a PNG file
ggsave(
  filename = 'goalie-three-four.png',  # Specify the file name
  path = '/Users/ada/Documents/projects/spazznolo.github.io/figs',  # Specify the file path
  width = 5,  # Set the width of the plot
  height = 3,  # Set the height of the plot
  device = 'png',  # Specify the device to use for saving (PNG format)
  dpi = 320  # Set the resolution of the plot
)





