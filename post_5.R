

career_save_pcts <-
  shots %>%
  dplyr::group_by(goalie_name) %>%
  dplyr::summarize(
    shots = dplyr::n(),
    goals = sum(goal),
    sv_pct = 1 - (goals/shots),
    exp_g = sum(x_goal),
    gsa_exp = 1 + (sum(x_goal) - sum(goal))/sum(x_goal),
    exp_f = 1 - (exp_g/shots),
    .groups = 'drop') %>%
  dplyr::filter(exp_g > 10)


library(MASS)

# Fit prior distribution to the observed career save percentages

# Generate sequence of save percentage values
sv_pct_fits = seq(min(career_save_pcts$gsa_exp), max(career_save_pcts$gsa_exp), length = 60)

# Fit a beta distribution to the career save percentages using maximum likelihood estimation
goalie_prior = fitdistr(career_save_pcts$gsa_exp, "weibull")

# Extract the estimated shape parameters from the fitted beta distribution
shape_0 = goalie_prior$estimate[1]
scale_0 = goalie_prior$estimate[2]

# Calculate the prior probability densities for the sequence of save percentage values
prior_points = dweibull(sv_pct_fits, shape = shape_0, scale = scale_0)

# Normalize probabilities to sum to 1
prior_points = prior_points/sum(prior_points)



# Create a histogram of career save percentages overlaid with the prior probability densities
ggplot() +
  geom_histogram(data = career_save_pcts, aes(gsa_exp), fill = single_color, col = 'black', alpha = 0.75, bins = 30) +
  geom_line(aes(sv_pct_fits, prior_points*nrow(career_save_pcts)*2), col = 'white', lwd = 1, alpha = 0.75) +
  dark_theme() +
  theme(
    panel.grid.major = element_line(color = 'black'),  # Customize major grid lines
    axis.text.y = element_blank(),  # Remove y-axis text
    axis.ticks.y = element_blank()  # Remove y-axis ticks
  ) +
  labs(x = '', y = '')

# Save the plot as a PNG file
ggsave(
  filename = 'goalie-five-one.png',  # Specify the file name
  path = '/Users/ada/Documents/projects/spazznolo.github.io/figs',  # Specify the file path
  width = 5,  # Set the width of the plot
  height = 3,  # Set the height of the plot
  device = 'png',  # Specify the device to use for saving (PNG format)
  dpi = 320  # Set the resolution of the plot
)

