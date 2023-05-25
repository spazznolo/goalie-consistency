
# Load necessary libraries from the 'libraries.R' file
source('libraries.R')  

# Load custom functions from the 'functions.R' file
source('functions.R')  

# Load data from the 'data.R' file
source('data.R')  

# Required packages: dplyr, ggplot2, fitdistrplus

# Required variables:
# - shots (data frame or tibble): Contains shot data, created in 'data' script
# - single_color: Specifies the fill color for the histogram
# - multiple_colors: Specifies the fill color for gradients

# Calculate career statistics for goalies
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

test_index <- sample(1:519, 119)
train_set <- hm %>% slice(-test_index)
test_set <- hm %>% slice(test_index)

gsa_exp_model <- lm(gsa_exp ~ gsa_exp_1 + gsa_exp_2 + gsa_exp_3, train_set)
summary(gsa_exp_model)
test_set %>%
  mutate(
    pred_gsa_exp = predict(gsa_exp_model, test_set)) %>%
  summarize(
    cor(pred_gsa_exp, gsa_exp), 
    mean(abs(pred_gsa_exp - gsa_exp)))


sum_mod <- summary(gsa_exp_model)

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
  distinct(goalie_name, season) %>%
  count(goalie_name) %>%
  count(n) %>%
  mutate(nn = cumsum(nn/sum(nn))) %>%
  ggplot() +
  geom_bar(aes(n, nn), fill = single_color, stat = 'identity') +
  dark_theme() +
  geom_hline(yintercept = c(0.274, 0.516, 0.774, 1), col = 'white', alpha = 0.5, linetype = 'dashed') +
  scale_y_continuous(breaks = c(0.274, 0.516, 0.774, 1), labels = scales::percent) +
  geom_vline(xintercept = c(1, 3, 7, 16), col = 'white', alpha = 0.5, linetype = 'dashed') +
  scale_x_continuous(breaks = c(1, 3, 7, 16)) +
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

