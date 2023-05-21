

# insert download directory here
download_dir <- '~/Downloads/'

# download moneypuck data
temp <- tempfile()
download.file("https://peter-tanner.com/moneypuck/downloads/shots_2007-2020.zip", temp)
unzip(temp, exdir = download_dir)

# create analysis set
goalies <- 
  read_csv(paste0(download_dir, "shots_2007-2020.csv")) %>%
  clean_names() %>%
  dplyr::select(season, game_id, goalie_id = goalie_id_for_shot, goalie_name = goalie_name_for_shot, goal, x_goal)


goalies %>%
  dplyr::filter(goalie_name %in% c('Tristan Jarry', 'Jack Campbell', 'Carter Hart', 'Carey Price', 'Matt Murray')) %>%
  dplyr::group_by(goalie_name, game_id) %>%
  dplyr::summarize(gsa_exp = sum(x_goal) - sum(goal)) %>%
  ggplot() +
  geom_density(aes(gsa_exp, fill = goalie_name), alpha = 0.2)

goalies %>%
  dplyr::filter(goalie_name %in% c('Tristan Jarry', 'Jack Campbell', 'Carter Hart', 'Roberto Luongo',
                                   'Carey Price', 'Matt Murray', 'Igor Shesterkin', 'Jonathan Quick')) %>%
  dplyr::group_by(goalie_name, season, game_id) %>%
  dplyr::summarize(gsa_exp = (sum(x_goal) - sum(goal)), .groups = 'drop') %>%
  dplyr:: group_by(goalie_name) %>%
  dplyr::summarize(games = dplyr::n(), mean(gsa_exp), sd(gsa_exp))

goalie_rates <-
  goalies %>%
  dplyr::group_by(goalie_name, season, game_id) %>%
  dplyr::summarize(
    shots = dplyr::n(),
    goals = sum(goal),
    exp_g = sum(x_goal),
    gsa_exp = (sum(x_goal) - sum(goal))/sum(x_goal),
    .groups = 'drop') %>%
  dplyr::filter(shots > 10, gsa_exp > -5)


library(MASS)

h = hist(goalie_rates$gsa_exp,
         col = "cadetblue4",
         border = "black",
         xlab = "Primary Points per Hour",
         main = "Defensemen",
         breaks = 30)

xfit = seq(min(goalie_rates$gsa_exp), max(goalie_rates$gsa_exp), length = 40)

goalie_prior = fitdistr(goalie_rates$gsa_exp, "normal")
d.density = dnorm(xfit, mean = goalie_prior$estimate[1], sd = goalie_prior$estimate[2])
d.density = d.density * diff(h$mids[1:2]) * length(goalie_rates$gsa_exp)
lines(xfit, d.density, col = "black", lwd = 2)

prior_mu = goalie_prior$estimate[1]
prior_sigma = goalie_prior$estimate[2]

data_mu = 0.00250
data_sigma = 0.0230
n = 93

post_mu = ((prior_mu/prior_sigma^2) + ((n * data_mu)/data_sigma^2))/((1/prior_sigma^2) + (n/data_sigma^2))
post_sigma = sqrt(1/((1/prior_sigma^2) + (n/data_sigma^2)))

d.density = dnorm(xfit, mean = post_mu, sd = post_sigma)
d.density = d.density * diff(h$mids[1:2]) * length(goalie_rates$gsa_exp)
lines(xfit, d.density, col = "black", lwd = 2)



prior_mu = goalie_prior$estimate[1]
prior_sigma = goalie_prior$estimate[2]
data_mu = 0.123
data_sigma = 1.73
n = 593

post_mu = ((prior_mu/prior_sigma^2) + ((n * data_mu)/data_sigma^2))/((1/prior_sigma^2) + (n/data_sigma^2))
post_sigma = sqrt(1/((1/prior_sigma^2) + (n/data_sigma^2)))

d.density = dnorm(xfit, mean = post_mu, sd = post_sigma)
d.density = d.density * diff(h$mids[1:2]) * length(goalie_rates$gsa_exp)
lines(xfit, d.density, col = "black", lwd = 2)

t.test(rnorm(593, 0.09, 0.06), rnorm(101, -0.04, 0.095), paired = FALSE)


prior_mu = goalie_prior$estimate[1]
prior_sigma = goalie_prior$estimate[2]
data_mu = 0.150
data_sigma = 1.69
n = 543

post_mu = ((prior_mu/prior_sigma^2) + ((n * data_mu)/data_sigma^2))/((1/prior_sigma^2) + (n/data_sigma^2))
post_sigma = sqrt(1/((1/prior_sigma^2) + (n/data_sigma^2)))

post_mu
post_sigma

mean(rnorm(10000, post_mu, post_sigma) > 0)
