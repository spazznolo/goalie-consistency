

# calculate normalized entropy of a series
# used in posts: two, three
norm_ent <- function(s){

  s1 <- which(s %in% 1)
  
  iet <- c(s1[1], diff(s1), length(s) + 1 - tail(s1,1))
  
  iet <- iet/(length(s)+1)
  
  h <- 1+((sum(log(iet)*iet))/log(sum(s)+1))
  
  return(h)
  
}

# 
# used in posts: one, two
ent_test <- function(s) {
  
  n <- 10000
  x <- replicate(n, sample(s, length(s), replace = FALSE))
  x <- lapply(1:ncol(x), function(i) x[, i])
  y <- map_dbl(x, norm_ent)
  
  return(sum(y < norm_ent(s))/n)
  
}

# 
# used in posts: one
poission_season_simulations <- function(goalie_, goal_share_) {
  
  total_goals = 5.5
  goal_share_opp = total_goals - goal_share_
  
  results_ <- replicate(10000,
                        c(rpois(41, total_goals/2) - rpois(41, goal_share_), 
                          rpois(41, total_goals/2) - rpois(41, goal_share_opp))
  )
  
  standing_points <- apply(results_, 2, function(x) (sum(x == 0)*1.5) + (sum(x > 0)*2))
  
  return(
    tibble(
      goalie = goalie_,
      standing_points)
  )
  
}


norm_ent_xg <- function(s, t){
  
  s2 <- cumsum(t)
  s1 <- s2[which(s %in% 1)]
  
  iet <- c(t[1], diff(s1), sum(t)+1-tail(t,1))
  
  iet <- iet/(sum(t)+1)
  
  h <- 1+((sum(log(iet)*iet))/log(sum(t)+1))
  
  return(h)
  
}


ent_test_xg <- function(goals_, x_g_, x_gsaa_xg_) {
  
  pb$tick()
  
  n <- 10000
  x_g_adj_ <- x_g_ - (x_g_*x_gsaa_xg_)
  
  ok <- replicate(n, x_g_adj_)
  ok <- lapply(1:ncol(ok), function(i) ok[, i])
  
  x <- replicate(n, rbinom(length(x_g_adj_), 1, x_g_adj_))
  x <- lapply(1:ncol(x), function(i) x[, i])
  
  z <- map2_dbl(x, ok, norm_ent_xg)
  
  return(sum(z < norm_ent_xg(goals_, x_g_adj_))/n)
  
}


# create consistent theme for all plots
# used in posts: all
dark_theme <- function() {
  
  theme(
    panel.grid.major = element_line(color = '#99a1b3'),
    panel.grid.minor = element_line(color = 'black'),
    panel.background = element_rect(fill = 'black'),
    panel.border=element_blank(),
    plot.background=element_rect(fill = "black", color = "black"),
    axis.text.x = element_text(color = 'white'),
    axis.text.y = element_text(color = 'white'),
    plot.title = element_text(face = 'bold', color = 'white', hjust = 0.5),
    plot.subtitle = element_text(face = 'bold', color = 'white'),
    axis.title.x = element_text(color = 'white'),
    axis.title.y = element_text(color = 'white'),
    legend.background = element_rect(fill = "black", color = NA),
    legend.key = element_rect(color = "gray", fill = "black"),
    legend.title = element_text(color = "white"),
    legend.text = element_text(color = "white")
  )
  
}

single_color = "#FFD500"
multiple_colors = rev(heat.colors(30)[21:30])
