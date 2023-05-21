
# Required packages: dplyr, ggplot2, fitdistrplus

# Required variables:
# - shots (data frame or tibble): Contains shot data
# - single_color: Specifies the fill color for the histogram


# Calculate career save percentages for goalies
career_save_pcts <- 
  shots %>%  # Assuming 'shots' is a data frame or tibble containing shot data
  group_by(goalie_name) %>%  # Group shots by goalie name
  summarize(
    shots = n(),  # Count the number of shots for each goalie
    goals = sum(goal),  # Calculate the total number of goals allowed
    saves = shots - goals, # Calculate the total number of saves made by subtracting the total goals from the total shots
    mean_sv_pct = 1 - mean(goal),  # Calculate the mean save percentage by subtracting the mean of goals from 1
    .groups = 'drop') %>%  # Drop grouping information
  filter(shots > 750) %>%  # Keep goalies with more than 750 shots
  drop_na()  # Remove rows with missing values



# Create a histogram of mean save percentages
career_save_pcts %>%
  ggplot() +
  geom_histogram(aes(mean_sv_pct), fill = single_color, col = 'black', alpha = 0.75) +  # Plot histogram with specified aesthetics
  dark_theme() +  # Apply a dark theme to the plot
  theme(
    panel.grid.major = element_line(color = 'black'),  # Customize major grid lines
    axis.text.y = element_blank(),  # Remove y-axis text
    axis.ticks.y = element_blank()  # Remove y-axis ticks
  ) +
  labs(x = '', y = '') +  # Set x and y axis labels as blank
  xlim(c(0.91, 0.95))  # Set the x-axis limits

# Save the plot as a PNG file
ggsave(
  filename = 'goalie-four-one.png',  # Specify the file name
  path = '/Users/ada/Documents/projects/spazznolo.github.io/figs',  # Specify the file path
  width = 5,  # Set the width of the plot
  height = 3,  # Set the height of the plot
  device = 'png',  # Specify the device to use for saving (PNG format)
  dpi = 320  # Set the resolution of the plot
)




# Generate sequence of save percentage values
sv_pct_fits = seq(0.91, 0.95, length = 60)

# Fit a beta distribution to the career save percentages using maximum likelihood estimation
goalie_prior = fitdist(career_save_pcts$mean_sv_pct, "beta", method = 'mle')

# Extract the estimated shape parameters from the fitted beta distribution
alpha_0 = goalie_prior$estimate[1]
beta_0 = goalie_prior$estimate[2]

# Calculate the prior probability densities for the sequence of save percentage values
prior_points = dbeta(sv_pct_fits, shape1 = alpha_0, shape2 = beta_0)

# Normalize probabilities to sum to 1
prior_points = prior_points/sum(prior_points)

# Create a histogram of career save percentages overlaid with the prior probability densities
ggplot() +
  geom_histogram(data = career_save_pcts, aes(mean_sv_pct), fill = single_color, col = 'black', alpha = 0.75, bins = 30) +
  geom_line(aes(sv_pct_fits, prior_points*182*2), col = 'white', lwd = 1, alpha = 0.75) +
  dark_theme() +
  theme(
    panel.grid.major = element_line(color = 'black'),  # Customize major grid lines
    axis.text.y = element_blank(),  # Remove y-axis text
    axis.ticks.y = element_blank()  # Remove y-axis ticks
  ) +
  labs(x = '', y = '') +
  scale_y_continuous()

# Save the plot as a PNG file
ggsave(
  filename = 'goalie-four-twoo.png',  # Specify the file name
  path = '/Users/ada/Documents/projects/spazznolo.github.io/figs',  # Specify the file path
  width = 5,  # Set the width of the plot
  height = 3,  # Set the height of the plot
  device = 'png',  # Specify the device to use for saving (PNG format)
  dpi = 320  # Set the resolution of the plot
)


median_sv_pct <- median(career_save_pcts$mean_sv_pct)

career_save_pcts %>%
  mutate(
    adj_sv_pct = (alpha_0 + saves)/(alpha_0 + beta_0 + shots),
    alpha_post = alpha_0 + saves,
    beta_post = beta_0 + goals,
    better_avg = pmap_dbl(., ~mean(rbeta(10000, shape1 = alpha_post, shape2 = beta_post) > median_sv_pct))
  ) %>%
  arrange(desc(better_avg))

comp_h2h
comp_mean
mean(rbeta(10000, shape1 = alpha_0 + vasilevskiy$saves, shape2 = beta_0 + vasilevskiy$goals) > median_sv_pct)
mean(rbeta(10000, shape1 = 26105, shape2 = 1685) > median_sv_pct)
mean(rbeta(10000, shape1 = alpha_0 + 23482, shape2 = beta_0 + 1484) > median_sv_pct)


vasilevsky_points = dbeta(sv_pct_fits, shape1 = alpha_0 + 18024, shape2 = beta_0 + 1131)
vasilevsky_points = vasilevsky_points/sum(vasilevsky_points)

sway_points = dbeta(sv_pct_fits, shape1 = alpha_0 + 2034, shape2 = beta_0 + 125)
sway_points = sway_points/sum(sway_points)

vasy_avg = (alpha_0 + 18024)/(alpha_0 + beta_0 + 19155)
sway_avg = (alpha_0 + 2034)/(alpha_0 + beta_0 + 2159)

ggplot() +
  geom_vline(aes(xintercept = median_sv_pct), lwd = 0.75, col = 'grey', alpha = 0.35, linetype = 'dashed') +
  geom_line(aes(sv_pct_fits, vasilevsky_points*100000), col = 'white', lwd = 0.75, alpha = 0.75) +
  geom_vline(aes(xintercept = vasy_avg), lwd = 0.75, col = 'white', alpha = 0.75, linetype = 'dashed') +
  geom_line(aes(sv_pct_fits, sway_points*100000), col = 'grey', lwd = 0.75, alpha = 0.75) +
  geom_vline(aes(xintercept = sway_avg), lwd = 0.75, col = 'grey', alpha = 0.75, linetype = 'dashed') +
  geom_histogram(aes(sample(sv_pct_fits, 100000, prob = prior_points, replace = TRUE)), fill = single_color, col = 'black', alpha = 0.35, bins = 60) +
  dark_theme() +
  theme(
    panel.grid.major = element_line(color = 'black'),
    axis.text.y=element_blank(),
    axis.ticks.y=element_blank()) +
  labs(x = '', y = '') +
  scale_y_continuous()

ggsave(filename = 'goalie-four-three.png', path = '/Users/ada/Documents/projects/spazznolo.github.io/figs', width = 5, height = 3, device = 'png', dpi = 320)







career_save_pcts <-
  shots %>%
  dplyr::group_by(goalie_name) %>%
  dplyr::summarize(
    shots = dplyr::n(),
    saves = shots - sum(goal),
    goals = sum(goal),
    mean_sv_pct = 1 - mean(goal),
    .groups = 'drop') %>%
  drop_na()

library(VGAM)

# negative log likelihood of data given alpha; beta
ll <- function(alpha, beta) {
  -sum(dbetabinom.ab(career_save_pcts$saves, career_save_pcts$shots, alpha, beta, log = TRUE))
}

m <- mle(ll, start = list(alpha = 9, beta = 10), method = "L-BFGS-B", lower=c(0.0001,0.0001), upper = c(Inf, Inf))
coef(m)


prior_points_1 = dbeta(sv_pct_fits, shape1 = 4056, shape2 = 285)
prior_points_1 = prior_points_1/sum(prior_points_1)

ggplot() +
  geom_line(aes(sv_pct_fits, prior_points*182*2), col = 'white', lwd = 1, alpha = 0.75) +
  geom_line(aes(sv_pct_fits, prior_points_1*182*2), col = 'white', lwd = 1, alpha = 0.75) +
  dark_theme() +
  theme(
    panel.grid.major = element_line(color = 'black'),
    axis.text.y=element_blank(),
    axis.ticks.y=element_blank()) +
  labs(x = '', y = '') +
  scale_y_continuous()


