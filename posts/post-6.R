
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




# Plot cumulative distribution function for total seasons played during career for goalies who haven't played the first or last season of available data
shots %>%
  anti_join(shots %>% filter(season %in% c(2007, 2022)) %>% distinct(goalie_name), by = 'goalie_name') %>%
  distinct(goalie_name, season) %>%  # Select unique combinations of goalie_name and season
  count(goalie_name, name = 'seasons_played') %>%  # Count the number of occurrences for each goalie_name
  count(seasons_played, name = 'career_length_counts') %>%  # Count the number of goalies for each count of occurrences
  mutate(cml_career_lengths = cumsum(career_length_counts/sum(career_length_counts))) %>%  # Calculate cumulative sum of normalized counts
  ggplot() +  # Start a ggplot object
  geom_hline(yintercept = c(0.469, 0.700, 0.915, 1), col = 'white', alpha = 0.5, linetype = 'dashed') +  # Add horizontal dashed lines for areas of interest
  geom_vline(xintercept = c(1, 3, 8, 13), col = 'white', alpha = 0.5, linetype = 'dashed') +  # Add vertical dashed lines for areas of interest
  geom_bar(aes(seasons_played, cml_career_lengths), fill = single_color, stat = 'identity') +  # Add bar plot with counts and normalized counts
  dark_theme() +  # Apply custom dark theme to the plot
  scale_y_continuous(breaks = c(0.469, 0.700, 0.915, 1), labels = scales::percent) +  # Add y-axis labels for areas of interest
  scale_x_continuous(breaks = c(1, 3, 8, 13)) +  # Add x-axis breaks for areas of interest
  theme(
    panel.grid.major = element_line(color = 'black')  # Customize major grid lines to be black
  ) +
  labs(x = 'Seasons', y = '')  # Set x and y axis labels to empty

# Save the plot as a PNG file
ggsave(
  filename = 'goalie-six-one.png',  # Specify the file name
  path = '/Users/ada/Documents/projects/spazznolo.github.io/figs',  # Specify the file path
  width = 5,  # Set the width of the plot
  height = 3,  # Set the height of the plot
  device = 'png',  # Specify the device to use for saving (PNG format)
  dpi = 320  # Set the resolution of the plot
)


# Calculate career statistics for goalies facing more than 20 xG
career_statistics <- 
  shots %>%  # tibble containing shot data loaded by the 'data' script
  group_by(goalie_name) %>%  # Group shots by goalie name
  dplyr::summarize(
    shots = dplyr::n(),  # Count the number of shots
    goals = sum(goal),  # Sum the number of goals
    sv_pct = 1 - (goals/shots),  # Calculate save percentage
    exp_g = sum(x_goal),  # Sum expected goals
    gsa_exp = (sum(x_goal)/sum(goal)) - 1 + 0.5,  # Calculate GSA-x
    exp_f = 1 - (exp_g/shots),  # Calculate expected save percentage
    adj_sv_pct = (.9406 + (sv_pct - exp_f)),  # Calculate adjusted save percentage
    inv_adj_sv_pct = 1 - adj_sv_pct,  # Calculate inverse adjusted save percentage
    adj_saves = adj_sv_pct*shots,  # Calculate adjusted saves
    .groups = 'drop'
  ) %>%
  dplyr::filter(exp_g > 20) %>%  # Filter rows where expected goals is greater than 20
  drop_na()  # Drop rows with missing values





season_statistics <-
  shots %>%
  dplyr::group_by(goalie_name, season) %>%  # Group shots by goalie name
  dplyr::summarize(
    shots = dplyr::n(),  # Count the number of shots
    goals = sum(goal),  # Sum the number of goals
    sv_pct = 1 - (goals/shots),  # Calculate save percentage
    exp_g = sum(x_goal),  # Sum expected goals
    gsa_exp = (sum(x_goal)/sum(goal)) - 1 + 0.5,  # Calculate GSA-x
    exp_f = 1 - (exp_g/shots),  # Calculate expected save percentage
    adj_sv_pct = (.9406 + (sv_pct - exp_f)),  # Calculate adjusted save percentage
    adj_saves = adj_sv_pct*shots,  # Calculate adjusted saves
    .groups = 'drop') %>%
  group_by(goalie_name) %>%
  mutate(
    sv_pct_1 = lag(sv_pct, 1),
    sv_pct_2 = lag(sv_pct, 2),
    sv_pct_3 = lag(sv_pct, 3),
    adj_sv_pct_1 = lag(adj_sv_pct, 1),
    adj_sv_pct_2 = lag(adj_sv_pct, 2),
    adj_sv_pct_3 = lag(adj_sv_pct, 3),
    exp_g_1 = lag(exp_g, 1),
    exp_g_2 = lag(exp_g, 2),
    exp_g_3 = lag(exp_g, 3)
  ) %>%
  ungroup() %>%
  #dplyr::filter(exp_g > 10, exp_g_1 > 10) %>%  # Filter rows where expected goals is greater than 20
  drop_na()  # Drop rows with missing values


cor_vec = vector()

for (i in 1:1000) {
  
  cor_boot <- 
    season_statistics %>%
    sample_n(n(), replace = TRUE) %>%
    mutate(pred_adj_sv_pct = predict(lm(adj_sv_pct ~ adj_sv_pct_1, .), .)) %>%
    summarize(
      corr = cor(pred_adj_sv_pct, adj_sv_pct),
      adj_r_sq = summary(lm(adj_sv_pct ~ sv_pct_1 + sv_pct_2, .))$adj.r.squared) %>%
    pull(adj_r_sq)
  
  cor_vec[i] = cor_boot
  
}

hist(cor_vec)








shots %>%
  count(goalie_name, season) %>%
  count(goalie_name) %>%
  count(n) %>%
  mutate(nn = cumsum(nn/sum(nn)))



