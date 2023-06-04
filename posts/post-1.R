
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




goalie_index <- 
  tibble(
    goalie = LETTERS[1:5],  # Create a tibble with goalie names as column 'goalie'
    goals = seq(2.75, 4.75, 0.50))  # Create a sequence of goals for each goalie


results <- 
  map2_dfr(
    goalie_index$goalie, goalie_index$goals, 
    poisson_season_simulations)  # Perform simulations for each goalie using their respective goal share


gt_std_pts <- 
  results %>%
  group_by(Goalie = goalie) %>%
  summarize(
    `Expected Points` = round(mean(standing_points), 2),  # Calculate the expected points for each goalie by averaging the standing points
    `Std Dev` = round(sd(standing_points), 2)  # Calculate the standard deviation of standing points for each goalie
  ) %>%
  gt() %>%
  tab_options(
    table.background.color = 'black'  # Set the table background color to black
  )

# Save the table as a PNG file
gtsave(
  gt_std_pts, # Specify the table name
  "goalie-one-one.png", # Specify the file name
  path = '/Users/ada/Documents/projects/spazznolo.github.io/figs', # Specify the file path
  expand = 0 # Remove border
  )


# Plot a comparison of average standing points for each goalie
results %>%
  rename(Goalie = goalie) %>%
  ggplot(aes(x = standing_points, y = rev(Goalie), fill = Goalie)) +  
  geom_density_ridges(alpha = 0.7) +  # Add density ridges to visualize the distribution of average standing points
  scale_x_continuous(limits = c(60, 120)) +  # Set the limits of the x-axis
  labs(x = '\nStanding Points', y = 'Goalie\n') +  # Set x and y axis labels
  scale_fill_manual(values = (multiple_colors(5))) +  # Customize the fill color of the markers
  scale_color_manual(values = (multiple_colors(5))) +  # Customize the color of the markers
  dark_theme() +  # Apply custom dark theme to the plot
  theme(
    axis.text.y = element_blank(),  # Hide y-axis labels
    axis.ticks.y = element_blank(),  # Hide y-axis ticks
    panel.grid.major = element_line(color = 'black'),  # Customize major grid lines to be black
    panel.grid.minor = element_line(color = 'black')  # Customize minor grid lines to be black
  )

# Save the plot as a PNG file
ggsave(
  filename = 'goalie-one-two.png',  # Specify the file name
  path = '/Users/ada/Documents/projects/spazznolo.github.io/figs',  # Specify the file path
  width = 5,  # Set the width of the plot
  height = 3,  # Set the height of the plot
  device = 'png',  # Specify the device to use for saving (PNG format)
  dpi = 320  # Set the resolution of the plot
)

# Check statistical significance of uncertainty on expected standing points
summary(lm(standing_points ~ 0 + goalie, results))


# Simulate season results for inconsistent goalie
inconsistent_game_results <- rpois(820000, 6/2) - rpois(820000, sample(seq(0, 6, 0.5), 820000, replace = TRUE))
inconsistent_season_results <- split(inconsistent_game_results, ceiling(seq_along(inconsistent_game_results)/82))
inconsistent_pts <- unlist(lapply(inconsistent_season_results, function(x) (sum(x > 0)*2) + (sum(x == 0)*1.5)))

# Simulate season results for average goalie
average_game_results <- rpois(820000, 6/2) - rpois(820000, 6/2)
average_season_results <- split(average_game_results, ceiling(seq_along(average_game_results)/82))
average_pts <- unlist(lapply(average_season_results, function(x) (sum(x > 0)*2) + (sum(x == 0)*1.5)))

# Simulate season results for steady goalie 
steady_game_results <- rpois(820000, 6/2) - 3
steady_season_results <- split(steady_game_results, ceiling(seq_along(steady_game_results)/82))
steady_pts <- unlist(lapply(steady_season_results, function(x) (sum(x > 0)*2) + (sum(x == 0)*1.5)))

# Plot simulated standing points for the synthetic goalie seasons
tibble(
  'inconsistent' = inconsistent_pts, 
  'average' = average_pts, 
  'steady' = steady_pts
) %>%
  pivot_longer(inconsistent:steady) %>%
  mutate(Goalie = factor(name, levels = c('steady', 'average', 'inconsistent'))) %>%
  ggplot +
  geom_density(aes(x = value, group = Goalie, fill = Goalie), alpha = 0.7) +
  labs(x = '\nStanding Points', y = 'Density\n') +
  theme_minimal() +
  scale_fill_manual(values = (multiple_colors(3))) +
  dark_theme() +
  theme(
    panel.grid.major = element_line(color = 'black'),
    panel.grid.minor = element_line(color = 'black')
  )

# Save the plot as a PNG file
ggsave(
  filename = 'goalie-one-three.png',  # Specify the file name
  path = '/Users/ada/Documents/projects/spazznolo.github.io/figs',  # Specify the file path
  width = 5,  # Set the width of the plot
  height = 3,  # Set the height of the plot
  device = 'png',  # Specify the device to use for saving (PNG format)
  dpi = 320  # Set the resolution of the plot
)
