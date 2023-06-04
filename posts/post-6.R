
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


beta_1 = 346.7239
beta_2 = 5603.3298

# Remove goalies whose career spans outside of the range of interest
goalies_to_remove <-
  shots %>% 
  filter(season %in% c(2007, 2022)) %>% 
  distinct(goalie_name) %>%
  filter(!(goalie_name %in% c('Carey Price', 'Brian Elliott', 'Jaroslav Halak', 'Jonathan Quick', 'Mike Smith',
                              'Semyon Varlamov', 'Tuukka Rask', 'Anton Khudobin', 'Jonathan Bernier',
                              'Pekka Rinne', 'Thomas Greiss', 'Corey Crawford', 'Cory Schneider',
                              'Curtis McElhinney')))

# Summary of goalie statistics in initial set
shots %>%
  group_by(goalie_name) %>%  # Group shots by goalie name
  summarize(
    seasons = n_distinct(season),
    shots = dplyr::n(),  # Count the number of shots
    goals = sum(goal),  # Sum the number of goals
    sv_pct = 1 - (goals/shots),  # Calculate save percentage
    exp_g = sum(x_goal),  # Sum expected goals
    gsa_exp = (sum(x_goal)/sum(goal)) - 1 + 0.5,  # Calculate GSA-x
    exp_f = 1 - (exp_g/shots),  # Calculate expected save percentage
    adj_sv_pct = 0.939 + ((sv_pct - exp_f)),  # Calculate adjusted save percentage
    inv_adj_sv_pct = 1 - adj_sv_pct,  # Calculate inverse adjusted save percentage
    adj_saves = adj_sv_pct*shots,  # Calculate adjusted saves
    .groups = 'drop'
  ) %>% 
  summarize(
    count = n(),
    across(c(seasons, shots, adj_sv_pct), list(mean, ~sum(.*(exp_g/sum(exp_g)))))
  )

# Summary of goalie statistics in filtered set
shots %>%
  anti_join(goalies_to_remove, by = 'goalie_name') %>% # Filter goalies who haven't played the first or last season of available data
  group_by(goalie_name) %>%  # Group shots by goalie name
  summarize(
    seasons = n_distinct(season),
    shots = dplyr::n(),  # Count the number of shots
    goals = sum(goal),  # Sum the number of goals
    sv_pct = 1 - (goals/shots),  # Calculate save percentage
    exp_g = sum(x_goal),  # Sum expected goals
    gsa_exp = (sum(x_goal)/sum(goal)) - 1 + 0.5,  # Calculate GSA-x
    exp_f = 1 - (exp_g/shots),  # Calculate expected save percentage
    adj_sv_pct = 0.939 + ((sv_pct - exp_f)),  # Calculate adjusted save percentage
    inv_adj_sv_pct = 1 - adj_sv_pct,  # Calculate inverse adjusted save percentage
    adj_saves = adj_sv_pct*shots,  # Calculate adjusted saves
    .groups = 'drop'
  ) %>% 
  summarize(
    count = n(),
    across(c(seasons, shots, adj_sv_pct), list(mean, ~sum(.*(exp_g/sum(exp_g)))))
    )


# Summary of goalie statistics in filtered set
shots %>%
  inner_join(goalie_data %>% distinct(goalie_name, .keep_all = TRUE), by = c('goalie_name')) %>%  # Join with goalie_data to get goalie dob
  group_by(goalie_name) %>%  # Group shots by goalie name
  summarize(
    seasons = n_distinct(season),
    shots = dplyr::n(),  # Count the number of shots
    goals = sum(goal),  # Sum the number of goals
    sv_pct = 1 - (goals/shots),  # Calculate save percentage
    exp_g = sum(x_goal),  # Sum expected goals
    gsa_exp = (sum(x_goal)/sum(goal)) - 1 + 0.5,  # Calculate GSA-x
    exp_f = 1 - (exp_g/shots),  # Calculate expected save percentage
    adj_sv_pct = 0.939 + ((sv_pct - exp_f)),  # Calculate adjusted save percentage
    inv_adj_sv_pct = 1 - adj_sv_pct,  # Calculate inverse adjusted save percentage
    adj_saves = adj_sv_pct*shots,  # Calculate adjusted saves
    .groups = 'drop'
  ) %>% 
  summarize(
    count = n(),
    across(c(seasons, shots, adj_sv_pct), list(mean, ~sum(.*(exp_g/sum(exp_g)))))
  )


# Plot cumulative distribution function for total shots faced during career for goalies who haven't played the first or last season of available data
shots %>%
  anti_join(goalies_to_remove, by = 'goalie_name') %>% # Filter goalies who haven't played the first or last season of available data
  count(goalie_name, name = 'shots_faced') %>%  # Count the number of shots faced for each goalie_name
  arrange(shots_faced) %>%
  mutate(cml_goalies = (1:n())/n()) %>%  # Calculate cumulative sum of normalized counts
  filter(shots_faced < 3000) %>%
  ggplot() +  # Start a ggplot object
  geom_hline(yintercept = c(0.250, 0.500, 0.750), col = 'white', alpha = 0.5, linetype = 'dashed') +  # Add horizontal dashed lines for areas of interest
  geom_vline(xintercept = c(33, 202, 2606), col = 'white', alpha = 0.5, linetype = 'dashed') +  # Add vertical dashed lines for areas of interest
  geom_line(aes(shots_faced, cml_goalies), col = single_color, stat = 'identity') +  # Add bar plot with counts and normalized counts
  dark_theme() +  # Apply custom dark theme to the plot
  scale_y_continuous(breaks = c(0.250, 0.500, 0.750), labels = scales::percent) +  # Add y-axis labels for areas of interest
  scale_x_continuous(breaks = c(33, 202, 2606)) +  # Add x-axis breaks for areas of interest
  theme(
    panel.grid.major = element_line(color = 'black')  # Customize major grid lines to be black
  ) +
  labs(x = 'Shots Faced') + 
  rremove("ylab")

# Save the plot as a PNG file
ggsave(
  filename = 'goalie-six-one.png',  # Specify the file name
  path = '/Users/ada/Documents/projects/spazznolo.github.io/figs',  # Specify the file path
  width = 5,  # Set the width of the plot
  height = 3,  # Set the height of the plot
  device = 'png',  # Specify the device to use for saving (PNG format)
  dpi = 320  # Set the resolution of the plot
)


# Plot cumulative distribution function for total seasons played during career for goalies who haven't played the first or last season of available data
shots %>%
  anti_join(goalies_to_remove, by = 'goalie_name') %>% # Filter goalies who haven't played the first or last season of available data
  distinct(goalie_name, season) %>%  # Select unique combinations of goalie_name and season
  count(goalie_name, name = 'seasons_played') %>%  # Count the number of occurrences for each goalie_name
  count(seasons_played, name = 'career_length_counts') %>%  # Count the number of goalies for each count of occurrences
  mutate(cml_career_lengths = cumsum(career_length_counts/sum(career_length_counts))) %>%  # Calculate cumulative sum of normalized counts
  ggplot() +  # Start a ggplot object
  geom_hline(yintercept = c(0.421, 0.743, 0.900, 1), col = 'white', alpha = 0.5, linetype = 'dashed') +  # Add horizontal dashed lines for areas of interest
  geom_vline(xintercept = c(1, 5, 12, 16), col = 'white', alpha = 0.5, linetype = 'dashed') +  # Add vertical dashed lines for areas of interest
  geom_bar(aes(seasons_played, cml_career_lengths), fill = single_color, stat = 'identity') +  # Add bar plot with counts and normalized counts
  dark_theme() +  # Apply custom dark theme to the plot
  scale_y_continuous(breaks = c(0.421, 0.743, 0.900, 1), labels = scales::percent) +  # Add y-axis labels for areas of interest
  scale_x_continuous(breaks = c(1, 5, 12, 16)) +  # Add x-axis breaks for areas of interest
  theme(
    panel.grid.major = element_line(color = 'black')  # Customize major grid lines to be black
  ) +
  labs(x = 'Seasons', y = '')  # Set x and y axis labels to empty

# Save the plot as a PNG file
ggsave(
  filename = 'goalie-six-two.png',  # Specify the file name
  path = '/Users/ada/Documents/projects/spazznolo.github.io/figs',  # Specify the file path
  width = 5,  # Set the width of the plot
  height = 3,  # Set the height of the plot
  device = 'png',  # Specify the device to use for saving (PNG format)
  dpi = 320  # Set the resolution of the plot
)




# Modify shots tibble to include goalie ages based on goalie dob in goalie_data
shots_with_ages <- 
  shots %>%
  group_by(season) %>%
  mutate(
    game_days = n_distinct(game_id),  # Count the number of distinct game_ids in each season
    day_of_year = case_when(
      game_id == min(game_id) ~ 245,  # Set the first game of the season to day 245
      game_id == max(game_id) ~ 518,  # Set the last game of the season to day 518
      TRUE ~ NA),
    day_of_year = 245 + (game_id - min(game_id))*(518 - 245)/game_days,  # Distribute game_days across day_of_year
    year = ifelse(day_of_year > 365, season + 1, season),  # Increment year if day_of_year exceeds 365
    game_date = ymd(paste0(year, '-01-01')),  # Set the game_date to January 1 of the corresponding year
    day_of_year = ifelse(day_of_year > 365, day_of_year - 365, day_of_year),  # Adjust day_of_year if it exceeds 365
    game_date = game_date + day_of_year) %>%  # Calculate the final game_date
  ungroup() %>%
  left_join(goalie_data %>% distinct(goalie_name, .keep_all = TRUE), by = c('goalie_name')) %>%  # Join with goalie_data to get goalie dob
  mutate(age = as.numeric(game_date - dob)/365) %>%  # Calculate goalie age
  drop_na(goalie_name)


shots_with_ages %>%
  count(goalie_name, dob) %>%
  filter(is.na(dob))


# Plot average save percentage by goalie age (grouped at first decimal)
shots_with_ages %>%
  mutate(age = round(age, 1)) %>% # Discretize age to the first decimal for grouping
  group_by(age) %>%
  summarize(
    shots = n(),  # Count the number of shots for each age
    goals = sum(goal),  # Sum the number of goals for each age
    saves = shots - goals,  # Calculate the number of saves for each age
    sv_pct = saves/shots,  # Calculate the save percentage for each age
    .groups = 'drop'
  ) %>%
  ggplot() +
  geom_point(aes(age, sv_pct, alpha = sqrt(shots)), col = single_color) +  # Plot age vs. save percentage with varying point size based on shots
  scale_y_continuous(limits = c(0.915, 0.965), labels = scales::percent) +  # Customize y-axis limits and labels
  dark_theme() +  # Apply custom dark theme to the plot
  theme(
    panel.grid.major = element_line(color = 'black'),  # Customize major grid lines to be black
    legend.position = "none"  # Remove the legend
  ) + 
  rremove("xlab") +
  rremove("ylab")

# Save the plot as a PNG file
ggsave(
  filename = 'goalie-six-five.png',  # Specify the file name
  path = '/Users/ada/Documents/projects/spazznolo.github.io/figs',  # Specify the file path
  width = 5,  # Set the width of the plot
  height = 3,  # Set the height of the plot
  device = 'png',  # Specify the device to use for saving (PNG format)
  dpi = 320  # Set the resolution of the plot
)


# Plot harmonic mean of change in save percentage from age n to age n + 1
raw_delta_ages <-
  shots_with_ages %>%
  mutate(age = round(age, 0)) %>%
  group_by(goalie_name, age) %>%  # Group shots by goalie name
  dplyr::summarize(
    shots = dplyr::n(),  # Count the number of shots
    goals = sum(goal),  # Sum the number of goals
    sv_pct = 1 - (goals/shots),  # Calculate save percentage
    exp_g = sum(x_goal),  # Sum expected goals
    gsa_exp = (sum(x_goal)/sum(goal)) - 1 + 0.5,  # Calculate GSA-x
    exp_f = 1 - (exp_g/shots),  # Calculate expected save percentage
    adj_sv_pct = ((sv_pct - exp_f)),  # Calculate adjusted save percentage
    inv_adj_sv_pct = 1 - adj_sv_pct,  # Calculate inverse adjusted save percentage
    adj_saves = adj_sv_pct*shots,  # Calculate adjusted saves
    .groups = 'drop'
  ) %>%
  group_by(goalie_name) %>%
  mutate(
    delta_sv_pct = adj_sv_pct - lag(adj_sv_pct)  # Calculate the change in save percentage for each goalie compared to the previous age
  ) %>%
  filter(shots > 100, lag(shots) > 100) %>%
  ungroup() %>%
  drop_na() %>%
  group_by(age) %>%
  summarize(
    occasions = n(),  # Count the number of goalies for each age
    delta_sv_pct = sum(delta_sv_pct*shots/sum(shots), na.rm = TRUE),  # Calculate the weighted mean change in save percentage for each age
    .groups = 'drop'
  ) %>%
  filter(age >= 22, age <= 38) %>%
  drop_na() %>%
  mutate(cml_delta = cumsum(delta_sv_pct)) %>%
  mutate(cml_delta = cml_delta - max(cml_delta))

raw_delta_ages %>%
  ggplot(aes(age, cml_delta, alpha = sqrt(occasions))) +
  geom_point(col = single_color) +  # Plot age vs. cumulative change in save percentage with varying point size based on the number of goalies
  geom_smooth(col = single_color, span = 1, alpha = 0.20, fill = 'white') +  # Add smoothed line to the plot
  scale_y_continuous(labels = scales::percent) +  # Customize y-axis labels as percentages
  labs(y = 'Mean Observed Change in SV%\n') +  # Set x and y axis labels
  dark_theme() +  # Apply custom dark theme to the plot
  theme(
    panel.grid.major = element_line(color = 'black'),  # Customize major grid lines to be black
    legend.position = "none"  # Remove the legend
  ) + 
  rremove("xlab")

# Save the plot as a PNG file
ggsave(
  filename = 'goalie-six-three.png',  # Specify the file name
  path = '/Users/ada/Documents/projects/spazznolo.github.io/figs',  # Specify the file path
  width = 5,  # Set the width of the plot
  height = 3,  # Set the height of the plot
  device = 'png',  # Specify the device to use for saving (PNG format)
  dpi = 320  # Set the resolution of the plot
)



shots_with_cml <-
  shots_with_ages %>%
  anti_join(goalies_to_remove, by = 'goalie_name') %>% # Filter goalies who haven't played the first or last season of available data
  arrange(goalie_name, season, game_id) %>%
  group_by(goalie_name) %>%
  mutate(
    cml_shots = 1:n(),
    cml_xg = cumsum(x_goal),
    cml_g = cumsum(goal),
    cml_sv_pct = 1 - (cml_g/cml_shots),
    exp_f = 1 - (cml_xg/cml_shots),
    adj_sv_pct = 0.939 + cml_sv_pct - exp_f,
    cml_adj_saves = adj_sv_pct*cml_shots,
    post_sv_pct = 1 - ((beta_1 + cml_shots - cml_adj_saves)/(beta_1 + beta_2 + cml_shots)),  # Calculate adjusted save percentage
    status = case_when(
      max(cml_shots) < 300 ~ '-0300',
      max(cml_shots) < 1500 ~ '-1500', 
      max(cml_shots) < 6000 ~ '-6000', 
      TRUE ~ '6000+')) %>%
  ungroup() 


1 - ((beta_1)/(beta_1 + beta_2))
# Plot of cumulative shots and pAdjSV% by status
shots_with_cml %>%
  group_by(status) %>%
  sample_n(5000) %>%
  ungroup() %>%
  ggplot() +
  geom_hline(yintercept = 0.941, alpha = 0.75, col = 'white', linetype = 'dashed') +  # Add a horizontal line
  geom_point(aes(cml_shots, post_sv_pct), alpha = 0.15, col = single_color, size = 0.1) +  # Add scatter plot points
  geom_line(aes(cml_shots, post_sv_pct, group = goalie_name), alpha = 0.25, col = single_color) +  # Add connected lines
  dark_theme() +
  facet_wrap(~status, scales = 'free') +  # Create facet panels based on status variable
  scale_y_continuous(labels = scales::percent) +  # Customize y-axis labels as percentages
  theme(
    panel.grid.major = element_line(color = 'black'),  # Customize major grid lines
    legend.key = element_rect(colour = NA, fill = NA),  # Remove legend key border
    strip.text = element_text(colour = 'white'),  # Customize facet strip text color
    strip.background = element_rect(colour = "black", fill = "black")  # Customize facet strip background
  ) +
  rremove("xlab") +
  rremove("ylab")


# Save the plot as a PNG file
ggsave(
  filename = 'goalie-six-three.png',  # Specify the file name
  path = '/Users/ada/Documents/projects/spazznolo.github.io/figs',  # Specify the file path
  width = 7.5,  # Set the width of the plot
  height = 4.5,  # Set the height of the plot
  device = 'png',  # Specify the device to use for saving (PNG format)
  dpi = 320  # Set the resolution of the plot
)

shots_with_cml %>%
  group_by(goalie_name) %>%
  slice(n()) %>%
  ungroup() %>%
  summarize(median(post_sv_pct))
  
shots_with_cml %>%
  group_by(goalie_name) %>%
  slice(n()) %>%
  ungroup() %>%
  arrange(desc(post_sv_pct))

shots_with_cml %>%
  filter(goalie_name == 'Braden Holtby') %>%
  slice(c(1, 1000, 5000, 10000, 15000, n())) %>%
  select(cml_shots, post_sv_pct) %>%
  data.frame()

shots_ex <-
  shots_with_cml %>% 
  group_by(status, cml_shots) %>%
  summarize(count = n(), sv_pct = median(post_sv_pct, na.rm = TRUE), .groups = 'drop') %>% 
  filter(count > 5) 

shots_ex_1 <-
  shots_ex %>%
  filter(cml_shots <= 1500) %>%
  ggplot() +
  geom_line(aes(cml_shots, sv_pct, col = status), alpha = 0.80) +
  geom_hline(yintercept = 0.941, alpha = 0.75, col = 'white', linetype = 'dashed') +  # Add a horizontal line
  scale_color_manual('', values = c("#FF0000", "#FFAA00", "#FFFF00", "white")) +
  labs(x = '\nCumulative Shots', y = 'pAdjSV%\n') +
  scale_y_continuous(breaks = c(0.936, 0.938, 0.940, 0.942), labels = scales::percent) +  # Customize y-axis labels as percentages
  dark_theme() +
  theme(
    panel.grid.major = element_line(color = 'black'),  # Customize major grid lines
    legend.key = element_rect(colour = NA, fill = NA)
  ) + rremove("ylab") + rremove("xlab")

shots_ex_2 <-
  shots_ex %>%
  filter(cml_shots <= 10000) %>%
  ggplot() +
  geom_line(aes(cml_shots, sv_pct, col = status), alpha = 0.80) +
  geom_hline(yintercept = 0.941, alpha = 0.75, col = 'white', linetype = 'dashed') +  # Add a horizontal line
  scale_color_manual('', values = c("#FF0000", "#FFAA00", "#FFFF00", "white")) +
  labs(x = '\nCumulative Shots', y = '') +
  scale_y_continuous(breaks = c(0.936, 0.938, 0.940, 0.942), labels = scales::percent) +  # Customize y-axis labels as percentages
  dark_theme() +
  theme(
    panel.grid.major = element_line(color = 'black'),  # Customize major grid lines
    legend.key = element_rect(colour = NA, fill = NA),
  ) + 
  rremove("ylab") + 
  rremove("xlab")


ggarrange(shots_ex_1, shots_ex_2, ncol=2, nrow=1, common.legend = TRUE, legend="top", labels = NULL) + bgcolor("black")


# Save the plot as a PNG file
ggsave(
  filename = 'goalie-six-four.png',  # Specify the file name
  path = '/Users/ada/Documents/projects/spazznolo.github.io/figs',  # Specify the file path
  width = 7.5,  # Set the width of the plot
  height = 4.5,  # Set the height of the plot
  device = 'png',  # Specify the device to use for saving (PNG format)
  dpi = 320  # Set the resolution of the plot
)


shots_with_cml %>% 
  group_by(status, cml_shots) %>%
  summarize(count = n(), age = mean(age, na.rm = TRUE), .groups = 'drop') %>% 
  filter(cml_shots < 5000, count > 3) %>% #filter(cml_shots == 1000)
  ggplot() +
  geom_line(aes(cml_shots, age, col = status)) +
  geom_hline(yintercept = 24.6, alpha = 0.75, col = 'white', linetype = 'dashed') +  # Add a horizontal line
  geom_hline(yintercept = 26.3, alpha = 0.75, col = 'white', linetype = 'dashed') +  # Add a horizontal line
  geom_hline(yintercept = 27.4, alpha = 0.75, col = 'white', linetype = 'dashed') +  # Add a horizontal line
  geom_vline(xintercept = 1000, alpha = 0.75, col = 'white', linetype = 'dashed') +  # Add a vertical line
  scale_y_continuous(breaks = c(23, 24.6, 26.3, 27.4, 29)) +  # Customize y-axis labels as percentages
  scale_color_manual('', values = c("#FF0000", "#FFAA00", "#FFFF00", "white")) +
  dark_theme() +
  theme(
    panel.grid.major = element_line(color = 'black'),  # Customize major grid lines
    legend.key = element_rect(colour = NA, fill = NA)
  ) + 
  rremove("ylab") + 
  rremove("xlab")

# Save the plot as a PNG file
ggsave(
  filename = 'goalie-six-six.png',  # Specify the file name
  path = '/Users/ada/Documents/projects/spazznolo.github.io/figs',  # Specify the file path
  width = 5,  # Set the width of the plot
  height = 3,  # Set the height of the plot
  device = 'png',  # Specify the device to use for saving (PNG format)
  dpi = 320  # Set the resolution of the plot
)




group_size = 500
season_statistics <-
  shots %>%
  arrange(goalie_name, season, game_id) %>%
  group_by(goalie_name) %>%
  mutate(shot_count = 1:n()) %>%
  ungroup() %>%
  mutate(shot_group = as.integer((shot_count - 1)/group_size)) %>%
  group_by(goalie_name, shot_group) %>%  # Group shots by goalie name
  dplyr::summarize(
    shots = dplyr::n(),  # Count the number of shots
    goals = sum(goal),  # Sum the number of goals
    sv_pct = 1 - (goals/shots),  # Calculate save percentage
    exp_g = sum(x_goal),  # Sum expected goals
    gsa_exp = (sum(x_goal)/sum(goal)) - 1 + 0.5,  # Calculate GSA-x
    exp_f = 1 - (exp_g/shots),  # Calculate expected save percentage
    adj_sv_pct = (0.9418437 + (sv_pct - exp_f)),  # Calculate adjusted save percentage
    inv_adj_sv_pct = 1 - adj_sv_pct,  # Calculate inverse adjusted save percentage
    adj_saves = adj_sv_pct*shots,  # Calculate adjusted saves
    adj_goals = shots - adj_saves,  # Calculate adjusted saves
    .groups = 'drop'
  )

model_lm <-
  shots_with_ages %>%
  arrange(goalie_name, season, game_id) %>%
  group_by(goalie_name) %>%
  mutate(shot_count = 1:n()) %>%
  ungroup() %>%
  group_by(goalie_name) %>%
  mutate(
    cml_shots = 1:n(),
    cml_xg = cumsum(x_goal),
    cml_g = cumsum(goal),
    cml_sv_pct = 1 - (cml_g/cml_shots),
    exp_f = 1 - (cml_xg/cml_shots),
    adj_sv_pct = 0.9399938 + cml_sv_pct - exp_f,
    cml_adj_saves = adj_sv_pct*cml_shots,
    post_sv_pct = (beta_1 + cml_shots - cml_adj_saves)/(beta_1 + beta_2 + cml_shots),  # Calculate adjusted save percentage
    post_sv_pct = 1 - post_sv_pct
  ) %>%
  ungroup() %>%
  mutate(shot_group = as.integer((shot_count - 1)/group_size)) %>%
  group_by(goalie_name, shot_group) %>%  # Group shots by goalie name
  select(goalie_name, shot_group, age, cml_shots, cml_adj_saves, cml_sv_pct, post_sv_pct) %>%
  group_by(goalie_name, shot_group) %>%
  slice(n()) %>%
  ungroup() %>%
  left_join(season_statistics, by = c('goalie_name', 'shot_group')) %>%
  group_by(goalie_name) %>%
  mutate(
    post_sv_pct_1 = lag(post_sv_pct, 1),
    adj_sv_pct_1 = lag(adj_sv_pct, 1),
    cml_shots_1 = lag(cml_shots, 1)
  ) %>%
  ungroup() %>%
  drop_na()


cor_vec = vector()

for (i in 1:500) {
  
  boot_lm <- 
    model_lm %>%
    sample_n(n(), replace = TRUE) %>%
    mutate(pred_adj_sv_pct = predict(lm(adj_sv_pct ~ post_sv_pct_1 + age + cml_shots_1, .), .))
  
  boot_rsq <-
    boot_lm %>%
    summarize(
      corr = cor(pred_adj_sv_pct, adj_sv_pct),
      adj_r_sq = summary(lm(adj_sv_pct ~ post_sv_pct_1 + age + cml_shots_1 + adj_sv_pct_1, .))$adj.r.squared) %>%
    pull(adj_r_sq)
  
  cor_vec[i] = boot_rsq
  
}

mean(cor_vec)
hist(cor_vec)
  
plot(boot_lm$pred_adj_sv_pct, boot_lm$adj_sv_pct)
print(summary(lm(adj_sv_pct ~ post_sv_pct_1 + age + cml_shots_1 + adj_sv_pct_1, boot_lm)))


shots_with_ages %>%
  distinct(goalie_name, season, game_id) %>%
  group_by(goalie_name) %>%
  mutate(goalie_game = 1:n()) %>%
  ungroup()
  


exp_f_by_season <-
  shots %>%
  group_by(season) %>%
  summarize(
    shots = dplyr::n(),  # Count the number of shots
    goals = sum(goal),  # Sum the number of goals
    sv_pct = 1 - (goals/shots),  # Calculate save percentage
    exp_g = sum(x_goal),  # Sum expected goals
    exp_f = 1 - (exp_g/shots)  # Calculate expected save percentage
  ) %>%
  select(season, season_exp_f = exp_f)

model <- loess(cml_delta ~ age, data = raw_delta_ages, span = 1)
xrange <- range(raw_delta_ages$age)
xseq <- seq(from=xrange[1], to=xrange[2], length=17)
pred <- predict(model, newdata = data.frame(age = xseq), se=TRUE)

y = unname(pred$fit)
y = y - max(y)
age_adjustment <- 
  tibble(age = xseq, age_adj = -y) %>%
  add_row(age = 20, age_adj = 0.00240) %>%
  add_row(age = 21, age_adj = 0.00180) %>%
  add_row(age = 39, age_adj = 0.00615) %>%
  add_row(age = 40, age_adj = 0.00715)



# Calculate career statistics for goalies
career_statistics <- 
  shots_with_ages %>%  # tibble containing shot data loaded by the 'data' script
  mutate(age = round(age, 0)) %>%
  drop_na() %>% 
  left_join(exp_f_by_season, by = 'season') %>%
  left_join(age_adjustment, by = 'age') %>% 
  group_by(goalie_name, season) %>%  # Group shots by goalie name
  dplyr::summarize(
    age = max(age),
    shots = dplyr::n(),  # Count the number of shots
    goals = sum(goal),  # Sum the number of goals
    sv_pct = 1 - (goals/shots),  # Calculate save percentage
    exp_g = sum(x_goal),  # Sum expected goals
    gsa_exp = (sum(x_goal)/sum(goal)) - 1 + 0.5,  # Calculate GSA-x
    exp_f = (1 - (exp_g/shots)) - mean(age_adj),  # Calculate expected save percentage
    adj_exp_g = (1 - exp_f)*shots,
    sv_pct_a_exp = sv_pct - exp_f,  # Calculate adjusted save percentage
    adj_sv_pct = mean_exp_f + sv_pct_a_exp,  # Calculate adjusted save percentage
    adj_saves = adj_sv_pct*shots,  # Calculate adjusted saves
    adj_goals = shots - adj_saves,  # Calculate adjusted saves
    .groups = 'drop'
  ) %>% 
  group_by(goalie_name) %>%
  summarize(
    age = max(age),
    shots = sum(shots),  # Count the number of shots
    goals = sum(goals),  # Sum the number of goals
    ex_g = sum(adj_exp_g),
    exp_f = 1 - (ex_g/shots),
    sv_pct = 1 - (goals/shots),  # Calculate save percentage
    adj_saves = sum(adj_saves),
    adj_goals = sum(adj_goals),
    adj_sv_pct = adj_saves/(adj_saves + adj_goals),
    inv_adj_sv_pct = 1 - adj_sv_pct,
    .groups = 'drop'
  ) %>%
  #dplyr::filter(exp_g > 10) %>%  # Filter rows where expected goals is greater than 10
  drop_na()  # Drop rows with missing values


# Calculate adjusted save percentage and posterior alpha and beta values
career_posteriors <- 
  career_statistics %>%
  mutate(
    post_sv_pct = (beta_1 + shots - adj_saves)/(beta_1 + beta_2 + shots),  # Calculate adjusted save percentage
    post_sv_pct = 1 - post_sv_pct, 
    alpha_post = beta_1 + shots - adj_saves,  # Calculate posterior alpha values
    beta_post = beta_2 + adj_saves,  # Calculate posterior beta values
    better_avg = map2_dbl(alpha_post, beta_post, ~mean(rbeta(100000, shape1 = .x, shape2 = .y) < 1 - mean(exp_f_by_season$season_exp_f)))  # Calculate proportion of values greater than median_sv_pct
  )

career_posteriors <- 
  career_statistics %>%
  mutate(
    post_sv_pct = (283.4727 + shots - adj_saves)/(283.4727 + 4536.9034 + shots),  # Calculate adjusted save percentage
    post_sv_pct = 1 - post_sv_pct, 
    alpha_post = 283.4727 + shots - adj_saves,  # Calculate posterior alpha values
    beta_post = 4536.9034 + adj_saves,  # Calculate posterior beta values
    better_avg = map2_dbl(alpha_post, beta_post, ~mean(rbeta(100000, shape1 = .x, shape2 = .y) < 1 - mean(exp_f_by_season$season_exp_f)))  # Calculate proportion of values greater than median_sv_pct
  )

career_posteriors %>%
  inner_join(shots_with_cml %>% distinct(goalie_name, status), by = 'goalie_name') %>%
  mutate(share = shots/sum(shots)) %>%
  group_by(status) %>%
  summarize(
    count = n(),
    better_exp = mean(post_sv_pct > exp_f),
    better_avg = mean(better_avg),
    harm_mean = sum((post_sv_pct > exp_f)*share),
    harm_mean = sum(better_avg*share)
  )
  
# weighted average age fenwick adjustment (age 21, 300 shots; age 22, 600 shots -> 0.33*adj_21 + 0.67*adj_22)

shots_with_ages %>%
  filter(goalie_name == 'Adin Hill')
