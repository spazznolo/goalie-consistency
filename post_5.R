
shots %>% summarize(sv_pct = 1 - (sum(goal)/n()))

career_save_pcts <-
  shots %>%
  dplyr::group_by(goalie_name) %>%
  dplyr::summarize(
    shots = dplyr::n(),
    goals = sum(goal),
    sv_pct = 1 - (goals/shots),
    exp_g = sum(x_goal),
    gsa_exp = (sum(x_goal)/sum(goal)) - 1 + 0.5,
    exp_f = 1 - (exp_g/shots),
    adj_sv_pct = .943 + (sv_pct - exp_f),
    .groups = 'drop') %>%
  dplyr::filter(exp_g > 20) %>%
  drop_na()


career_save_pcts %>%
  ggplot() +
  geom_point(aes(shots, adj_sv_pct))

library(MASS)

# Fit prior distribution to the observed career save percentages

# Generate sequence of save percentage values
sv_pct_fits = seq(min(career_save_pcts$adj_sv_pct), max(career_save_pcts$adj_sv_pct), length = 50)

# Fit a beta distribution to the career save percentages using maximum likelihood estimation
goalie_prior = fitdistr(career_save_pcts$adj_sv_pct, "weibull")

# Extract the estimated shape parameters from the fitted beta distribution
shape_0 = goalie_prior$estimate[1]
scale_0 = goalie_prior$estimate[2]

# Calculate the prior probability densities for the sequence of save percentage values
prior_points = dweibull(sv_pct_fits, shape = shape_0, scale = scale_0)

# Normalize probabilities to sum to 1
prior_points = prior_points/sum(prior_points)


# Fit a beta distribution to the career save percentages using maximum likelihood estimation
goalie_prior = fitdist(career_save_pcts$adj_sv_pct, "beta")

# Extract the estimated shape parameters from the fitted beta distribution
shape_0 = goalie_prior$estimate[1]
scale_0 = goalie_prior$estimate[2]

# Calculate the prior probability densities for the sequence of save percentage values
prior_points_gamma = dbeta(sv_pct_fits, shape1 = shape_0, shape2 = scale_0)

# Normalize probabilities to sum to 1
prior_points_gamma = prior_points_gamma/sum(prior_points_gamma)

# Create a histogram of career save percentages overlaid with the prior probability densities
ggplot() +
  geom_histogram(data = career_save_pcts, aes(adj_sv_pct), fill = single_color, col = 'black', alpha = 0.75, bins = 25) +
  geom_line(aes(sv_pct_fits, prior_points*nrow(career_save_pcts)*2), col = 'white', lwd = 1, alpha = 0.75) +
  geom_line(aes(sv_pct_fits, prior_points_gamma*nrow(career_save_pcts)*2), col = 'grey', lwd = 1, alpha = 0.75) +
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

gsa_exp_player = .953
exp_g_player = 4129
posterior_weibull = function(q){
  (q^gsa_exp_player)*exp(-q*exp_g_player)*(q^(shape_0 - 1))*exp(-(q/scale_0)^shape_0)}


#Find maximum posterior density

#Skater A

MaxRate.A = optimize(posterior_weibull, c(0.9, 1), maximum = TRUE)$maximum
MaxDensity.A = posterior_weibull(MaxRate.A)





