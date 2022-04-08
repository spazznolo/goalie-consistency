

## introduction


# load everything
source('libraries.R')


ggplot() +
  geom_bar(aes(x=rpois(100000, 3)), fill = single_color, alpha = 0.7) +
  labs(x = '\nGoals Allowed', y = NULL) +
  scale_x_continuous(limits = c(-0.5, 10), breaks = seq(0, 10, 1)) +
  dark_theme() +
  theme(
    panel.grid.major = element_line(color = 'black'),
    panel.grid.minor = element_line(color = 'black')) 

ggsave(filename = 'goalie-zero-one.png', path = '/Users/ada/Documents/GitHub/spazznolo.github.io/figs', width = 5, height = 3, device = 'png', dpi = 320)
