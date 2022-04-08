

## shot-to-shot consistency (with real data)


# load everything
source('libraries.R')
source('functions.R')
source('data.R')


# progress bar
pb <- progress_bar$new(
  format = "  calculating entropy [:bar] :percent in :elapsed",
  total = nrow(goalie_summary), clear = TRUE, width= 71
)


# normalized entropy scores for goalie seasons
ent_results <- 
  goalies %>%
  inner_join(goalie_summary, by = c('goalie_name', 'goalie_id', 'season')) %>%
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

ggsave(filename = 'goalie-three-one.png', path = '/Users/ada/Documents/GitHub/spazznolo.github.io/figs', width = 5, height = 3, device = 'png', dpi = 320)

summary(lm(x_gsaa_xg ~ ent_pct, ent_results))


# goalie-season entropy to gsax
ent_results %>%
  group_by(goalie_name) %>%
  mutate(ent_pct_lead = lead(ent_pct)) %>%
  drop_na %>%
  ggplot(aes(ent_pct*100, ent_pct_lead*100)) +
  geom_point(col = single_color, alpha = 0.7) +
  labs(x = '\nEntropy Percentile', y = 'Season GSAx/xG\n') +
  scale_x_continuous(limits = c(0, 100)) +
  dark_theme() +
  theme(
    panel.grid.major = element_line(color = 'black'),
    panel.grid.minor = element_line(color = 'black')) +
  annotate("text", x = 10, y = -0.35, col = 'white',
           label = paste('r = ',round(cor(ent_results$ent_pct, ent_results$ent_pct_lead),3),sep=' '), size = 3)

ggsave(filename = 'goalie-three-one.png', path = '/Users/ada/Documents/GitHub/spazznolo.github.io/figs', width = 5, height = 3, device = 'png', dpi = 320)

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


# least consistent goalie careers (weighted by xg)
career_entropy %>% 
  slice_max(mean_ent, n = 10) %>%
  ggplot(aes(reorder(goalie_name, mean_ent), 100*mean_ent - 50, fill = reorder(goalie_name, mean_ent))) + 
  geom_bar(stat='identity',col='black',alpha = 0.7, size = 0.05) +
  scale_y_continuous(breaks = seq(0, 100, 5), labels = function(y) y + 50) +
  scale_fill_manual(values = multiple_colors) +
  labs(x=NULL,y='\nCareer Average Entropy Percentile') +
  coord_flip() +
  guides(fill = 'none') +
  dark_theme() +
  theme(
    panel.grid.major = element_line(color = 'black'),
    panel.grid.minor = element_line(color = 'black')) 

ggsave(filename = 'goalie-three-two.png', path = '/Users/ada/Documents/GitHub/spazznolo.github.io/figs', width = 5, height = 3, device = 'png', dpi = 320)

# least consistent goalie careers (weighted by xg)
career_entropy %>% 
  slice_min(mean_ent, n = 10) %>%
  ggplot(aes(reorder(goalie_name, -mean_ent), 100*mean_ent, fill = reorder(goalie_name, -mean_ent))) + 
  geom_bar(stat='identity',col='black',alpha = 0.7, size = 0.05) +
  scale_y_continuous(breaks = seq(0, 100, 5), labels = function(y) y) +
  scale_fill_manual(values = multiple_colors) +
  labs(x=NULL,y='\nCareer Average Entropy Percentile') +
  coord_flip() +
  guides(fill = 'none') +
  dark_theme() +
  theme(
    panel.grid.major = element_line(color = 'black'),
    panel.grid.minor = element_line(color = 'black')) 

ggsave(filename = 'goalie-three-three.png', path = '/Users/ada/Documents/GitHub/spazznolo.github.io/figs', width = 5, height = 3, device = 'png', dpi = 320)

career_entropy %>%
  ggplot(aes(mean_ent*100)) +
  geom_histogram(fill = single_color, bins = 20, alpha = 0.7) +
  labs(x = '\nCareer Average Entropy Percentile', y = NULL) +
  scale_x_continuous(limits = c(0, 100), breaks = c(0, 25, 50, 75, 100)) +
  scale_y_continuous(limits = c(0, 20), breaks = c(0, 5, 10, 15, 20)) +
  dark_theme() +
  theme(
    panel.grid.major = element_line(color = 'black'),
    panel.grid.minor = element_line(color = 'black')) 

# good goalies tend to be streakier than bad goalies!
career_entropy %>%
  arrange(mean_ent) %>%
  ggplot(aes(mean_ent*100, x_gsaa_xg)) +
  geom_point(col = "#FFD500", alpha = 0.7) +
  labs(x = '\nCareer Average Entropy Percentile', y = 'Career Average GSAx/xG\n') +
  scale_x_continuous(limits = c(0, 100)) +
  dark_theme() +
  theme(
    panel.grid.major = element_line(color = 'black'),
    panel.grid.minor = element_line(color = 'black')) +
  annotate("text", x = 10, y = -0.075, col = 'white',
           label = paste('r = ',round(cor(career_entropy$mean_ent, career_entropy$x_gsaa_xg),3),sep=' '), size = 3)

ggsave(filename = 'goalie-three-four.png', path = '/Users/ada/Documents/GitHub/spazznolo.github.io/figs', width = 5, height = 3, device = 'png', dpi = 320)

summary(lm(x_gsaa_xg ~ mean_ent, career_entropy))


# career consistency doesn't seem to be related to season-to-season consistency
career_entropy %>%
  arrange(mean_ent) %>%
  ggplot() +
  geom_point(aes(mean_ent*100, x_gsaa_xg_diff), col = single_color) +
  labs(x = '\nCareer Average Entropy Percentile', y = 'Career Average YoY Delta GSAx/xG \n') +
  scale_x_continuous(limits = c(0, 100)) +
  scale_y_continuous(limits = c(-1, 1)) +
  dark_theme() +
  theme(
    panel.grid.major = element_line(color = 'black'),
    panel.grid.minor = element_line(color = 'black'))  +
  annotate("text", x = 15, y = -0.75, col = 'white', size = 3,
           label = paste('r = ', round(cor(career_entropy$mean_ent, career_entropy$x_gsaa_xg_diff),3), sep=' '))

ggsave(filename = 'goalie-three-five.png', path = '/Users/ada/Documents/GitHub/spazznolo.github.io/figs', width = 5, height = 3, device = 'png', dpi = 320)

summary(lm(x_gsaa_xg_diff ~ mean_ent, career_entropy))

