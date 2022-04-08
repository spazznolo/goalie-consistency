

# load everything
source('libraries.R')
source('functions.R')
source('data.R')


# create normalized entropy scores for goalie seasons
ent_results <- 
  goalies %>%
  inner_join(goalie_summary, by = c('goalie_name', 'goalie_id', 'season')) %>%
  group_by(goalie_name, season, shots, g, save_pct) %>%
  summarize(ent_pct = ent_test(goal), .groups = 'drop')


# goalie season entropy to save percent
ent_results %>%
  ggplot(aes(ent_pct*100, save_pct*100)) +
  geom_point(col = 'yellow', alpha = 0.7) +
  labs(x = '\nEntropy Percentile', y = 'Season Save %\n') +
  scale_x_continuous(limits = c(0, 100)) +
  dark_theme() +
  theme(
    panel.grid.major = element_line(color = 'black'),
    panel.grid.minor = element_line(color = 'black')) +
  annotate("text", x = 12.5, y = 90.5, col = 'white',
           label = paste('r = ',round(cor(ent_results$ent_pct, ent_results$save_pct),3),sep=' '), size = 3)

ggsave(filename = 'goalie-two-one.png', path = '/Users/ada/Documents/GitHub/spazznolo.github.io/figs', width = 5, height = 3, device = 'png', dpi = 320)

summary(lm(save_pct ~ ent_pct, ent_results))


# create goalie career summaries
career_entropy <- 
  ent_results %>%
  group_by(goalie_name) %>%
  filter(n() >= 5) %>%
  mutate(lag_save_pct = lag(save_pct)) %>%
  drop_na %>%
  summarize(
    seasons = n(),
    mean_ent = mean(ent_pct), 
    save_pct_cor = cor(save_pct, lag_save_pct), 
    save_pct_diff = mean(abs(save_pct- lag_save_pct)/lag_save_pct), 
    save_pct = mean(save_pct),
    x_gsaa_s = mean(x_gsaa_s)
  ) %>%
  ungroup() %>%
  drop_na


# least consistent goalie careers
career_entropy %>% 
  slice_max(mean_ent, n = 10) %>%
  ggplot(aes(reorder(goalie_name, mean_ent), 100*mean_ent - 50, fill = reorder(goalie_name, mean_ent))) + 
  geom_bar(stat='identity',col='black',alpha = 0.65) +
  scale_y_continuous(breaks = seq(0, 100, 5), labels = function(y) y + 50) +
  scale_fill_manual(values = rev(heat.colors(15)[1:10])) +
  labs(x=NULL,y='\nCareer Average Entropy Percentile') +
  coord_flip() +
  guides(fill = 'none') +
  dark_theme() +
  theme(
    panel.grid.major = element_line(color = 'black'),
    panel.grid.minor = element_line(color = 'black')) 

ggsave(filename = 'goalie-two-two.png', path = '/Users/ada/Documents/GitHub/spazznolo.github.io/figs', width = 5, height = 3, device = 'png', dpi = 320)


# good goalies tend to be streakier than bad goalies!
career_entropy %>%
  ggplot(aes(mean_ent*100, save_pct*100)) +
  geom_point(col = 'yellow', alpha = 0.7) +
  labs(x = '\nCareer Average Entropy Percentile', y = 'Career Save %\n') +
  scale_x_continuous(limits = c(0, 100)) +
  dark_theme() +
  theme(
    panel.grid.major = element_line(color = 'black'),
    panel.grid.minor = element_line(color = 'black')) +
  annotate("text", x = 10, y = 94.1, col = 'white',
           label = paste('r = ',round(cor(career_entropy$mean_ent, career_entropy$save_pct),3),sep=' '), size = 3)

ggsave(filename = 'goalie-two-three.png', path = '/Users/ada/Documents/GitHub/spazznolo.github.io/figs', width = 5, height = 3, device = 'png', dpi = 320)

summary(lm(save_pct ~ mean_ent, career_entropy))


# career consistency doesn't seem to be related to season-to-season consistency
career_entropy %>%
  arrange(mean_ent) %>%
  ggplot() +
  geom_point(aes(mean_ent*100, save_pct_diff*100), col = 'yellow') +
  labs(x = '\nCareer Average Entropy Percentile', y = 'Career Average YoY Delta Save % \n') +
  scale_x_continuous(limits = c(0, 100)) +
  dark_theme() +
  theme(
    panel.grid.major = element_line(color = 'black'),
    panel.grid.minor = element_line(color = 'black'))  +
  annotate("text", x = 20, y = 1.25, col = 'white', size = 3,
           label = paste('r = ', round(cor(career_entropy$mean_ent, career_entropy$save_pct_diff),3), sep=' '))

ggsave(filename = 'goalie-two-four.png', path = '/Users/ada/Documents/GitHub/spazznolo.github.io/figs', width = 5, height = 3, device = 'png', dpi = 320)

summary(lm(save_pct_diff ~ mean_ent, career_entropy))

