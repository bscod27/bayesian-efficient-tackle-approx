setwd('~/05. Scripts/08. Big Data Bowl 2024/code')
library(tidyverse)
library(fuzzyjoin)
library(gganimate)
library(cowplot)
library(gifski)
library(magick)
library(animation)
library(latex2exp)


##### Load the data and merge #####
games <- read.csv('../data/games.csv')
players <- read.csv('../data/players.csv')
plays <- read.csv('../data/plays.csv')
tackles <- read.csv('../data/tackles.csv')
basic <- read.csv('../data/basic.csv')
advanced <- read.csv('../data/advanced.csv')

df_bdb <- tackles %>% 
  mutate(trial = 1) %>% 
  left_join(plays, by = c('gameId', 'playId')) %>% 
  left_join(players, by = 'nflId') %>% 
  left_join(games, by  = 'gameId') %>% 
  select(
    gameId, nflId, displayName, defensiveTeam, position, playId, 
    week, quarter, down, tackle, assist, pff_missedTackle, trial
    ) %>% 
  mutate(
    success = ifelse(pff_missedTackle == 0, 1, 0),
    faiure = ifelse(pff_missedTackle == 1, 1, 0),
    position =
      case_when(
        position %in% c('ILB', 'MLB') ~ 'Inside Linebacker',  
        position %in% c('OLB','DE') ~ 'Edge Defender',
        position %in% c('FS', 'SS') ~ 'Safety',
        position %in% c('DT', 'NT') ~ 'Interior Lineman',
        position %in% c('CB', 'DB') ~ 'Cornerback'
      ),
    displayName = gsub(' Jr.$', '', displayName),
    displayName = gsub(' [I]+$', '', displayName),
  ) 


##### Load the sportsreference data #####
sportsreference <- basic %>% 
  select(Player, GS, Tm, Age, Pos, Comb, FF, Sk, Solo, Ast, TFL) %>% 
  left_join(
    advanced %>% select(Player, Tm, Age, Pos, Prss, MTkl), 
    by = c('Player', 'Tm', 'Age', 'Pos')
    ) %>% 
  filter(!is.na(Comb)) 

df_sportsref <- sportsreference %>%
  mutate_if(is.numeric, ~replace_na(., 0)) %>% 
  mutate(
    Player = gsub('\\*', '', Player),
    Player = gsub('\\+', '', Player),
    Player = gsub(' Jr.$', '', Player),
    Player = gsub(' [I]+$', '', Player),
    Player = case_when(
        Player == 'Chauncey Gardner-Johnson' ~ 'C.J. Gardner-Johnson',
        Player == 'Sebastian Joseph-Day' ~ 'Sebastian Joseph',
        Player == 'Matthew Ioannidis' ~ 'Matt Ioannidis', 
        TRUE ~ Player
      ), 
    Pos = case_when(
      Pos %in% c('RILB', 'MLB', 'LILB', 'LB', 'LB/RLB', 'MLB/RILB', 'LILB/RILB') ~ 'Inside Linebacker',
      Pos %in% c('LLB', 'RLB', 'ROLB', 'LOLB', 'LOLB/ROLB', 'ROLB/LILB', 'LOLB/RDE', 'RDE', 'LDE', 'RDE/LDE', 'LDT/LDE', 'DE', 'LDE/RDE', 'DE/LDE', 'DL') ~ 'Edge Defender',
      Pos %in% c('LDT', 'RDT', 'NT', 'DT', 'LDT/RDT', 'RDT/RDE', 'RDT/LDE', 'DT/NT') ~ 'Interior Lineman',
      Pos %in% c('FS', 'SS', 'SS/FS', 'FS/SS', 'S') ~ 'Safety',
      Pos %in% c('RCB', 'LCB', 'LCB/RCB', 'CB', 'CB/RCB', 'DB', 'DB/FS') ~ 'Cornerback', 
      TRUE ~ Pos
    ),
    Tm = case_when(
      Tm == 'GNB' ~ 'GB', 
      Tm == 'KAN' ~ 'KC', 
      Tm == 'LAR' ~ 'LA', 
      Tm == 'LVR' ~ 'LV', 
      Tm == 'NOR' ~ 'NO', 
      Tm == 'NWE' ~ 'NE', 
      Tm == 'SFO' ~ 'SF', 
      Tm == 'TAM' ~ 'TB', 
      TRUE ~ Tm
    ),
    attempted = Comb + MTkl,
    max_SkTFL = ifelse(Sk > TFL, Sk, TFL),
    impact = (1.5*FF + 1.25*max_SkTFL + (Solo-FF-max_SkTFL)*1 + Ast*.5)/attempted,
    impact_above = (1.5*FF + (Solo-FF-max_SkTFL)*1 + Ast*.5)/attempted,
  ) %>% 
  filter(Pos %in% c('Inside Linebacker', 'Edge Defender', 'Interior Lineman', 'Cornerback', 'Safety')) %>% 
  filter(GS >= 9) %>% 
  arrange(desc(impact))


##### Figure out the priors #####
Estimate.MoM <- function(mu, sigsq) {
  a <- mu * (((mu * (1-mu)) / sigsq) - 1)
  b <- (1-mu) * (((mu * (1-mu)) / sigsq) - 1)
  return(data.frame(a, b))
}

stats_team <- df_sportsref %>% 
  group_by(Tm) %>% 
  summarize(
    team_avg=mean(impact), 
    team_var=var(impact), 
    team_n = n()
    ) %>% 
  filter(!(Tm %in% c('2TM', '3TM')))

stats_pos <- df_sportsref %>% 
  group_by(Pos) %>% 
  summarize(
    pos_avg=mean(impact), 
    pos_var=var(impact), 
    pos_n=n()
    ) %>% 
  filter(complete.cases(.))

joined <- df_bdb %>% 
  select(displayName, defensiveTeam, position) %>% 
  unique(.) %>% 
  stringdist_left_join( # fuzzy join
    df_sportsref,
    by = c('displayName'='Player', 'position'='Pos')
    ) %>% 
  select(-Player, -Tm, -Pos)

priors <- joined %>% 
  left_join(stats_team, by = c('defensiveTeam'='Tm')) %>% 
  left_join(stats_pos, by = c('position'='Pos')) %>% 
  mutate(
    impact = ifelse(!is.na(impact), impact, ((team_avg * team_n) + (pos_avg * pos_n)) / (team_n + pos_n)),
    mu = impact,
    sigsq = var(impact, na.rm=TRUE),  
    a = Estimate.MoM(mu, sigsq)$a,
    b = Estimate.MoM(mu, sigsq)$b
  ) %>% 
  arrange(desc(impact), displayName) %>% 
  select(displayName, defensiveTeam, position, attempted=attempted, impact, a, b)


##### Calculate cumulative likelihood across the weeks #####
player_week_summary <- df_bdb %>% 
  group_by(displayName, defensiveTeam, position, week) %>% 
  summarize(x=sum(success), n=sum(trial))

likelihood <- data.frame(
    sapply(unique(df_bdb[, c('displayName', 'defensiveTeam', 'position')]), function(i) rep(i, 9))
  ) %>%
  arrange(displayName, defensiveTeam, position) %>% 
  mutate(week = rep(1:9, n_distinct(displayName, defensiveTeam))) %>% 
  left_join(player_week_summary, by = c('displayName', 'defensiveTeam', 'position', 'week')) %>% 
  mutate_if(is.numeric, ~replace_na(., 0)) %>% 
  group_by(displayName, defensiveTeam, position) %>% 
  mutate(
    x_cumulative = cumsum(x), 
    n_cumulative = cumsum(n), 
  )


##### Calculate BITE #####
out <- likelihood %>% 
  select(-x, -n) %>% 
  left_join(
    priors %>% select(displayName, defensiveTeam, position, a, b), 
    by = c('displayName', 'defensiveTeam', 'position')) %>% 
  select(displayName, position, defensiveTeam, everything(.)) %>% 
  mutate(BITE = (a + x_cumulative) / (a + b + n_cumulative))

# example distribution for Foresade Oluokun
png('../results/example_distributions.png', units='in', width = 7.5, height = 5, res = 500)
sample <- out %>% filter(displayName == 'Foyesade Oluokun') %>% filter(week==9)
x <- seq(0, 1, length.out = 1000)
plot(
  density(qbeta(x, sample$a, sample$b)), 
  xlim=c(0.59, 1), ylim=c(0,20), xlab='Score',
  main=TeX('Prior, likelihood, posterior, and $\\it{BITE}$ for Foyesade Oluokun (Week 9)'), 
  col='red'
  ) # prior
lines(density(qbinom(x, sample$n_cumulative, sample$x_cumulative/sample$n_cumulative)/sample$n_cumulative), col='blue') # likelihood
lines(density(qbeta(x, sample$a+sample$x_cumulative, sample$b+sample$n_cumulative-sample$x_cumulative)), col='black')
legend('topleft', legend = c('Prior', 'Likelihood (scaled)', 'Posterior', TeX('$\\it{BITE}$')), col = c('red', 'blue', 'black', 'black'),lty=c(1,1,1,3))
abline(v=sample$BITE, col = 'black', lty=3)
dev.off()


##### Player ratings ##### 
Display.Rankings <- function(pos, wk=9) {
  stats <- out %>% 
    as.data.frame(.) %>% 
    filter(position == pos, week == wk) %>% 
    arrange(desc(BITE)) %>% 
    select(displayName, defensiveTeam, x_cumulative, n_cumulative, a, b, BITE) %>% 
    mutate(BITE_STD = (BITE-mean(BITE))/sd(BITE))
  return(stats)
}

write.csv(Display.Rankings('Edge Defender'), '../results/rankings_ED.csv')
write.csv(Display.Rankings('Interior Lineman'), '../results/rankings_ILM.csv')
write.csv(Display.Rankings('Inside Linebacker'), '../results/rankings_ILB.csv')
write.csv(Display.Rankings('Cornerback'), '../results/rankings_CB.csv')
write.csv(Display.Rankings('Safety'), '../results/rankings_S.csv')


##### Load and manipulate the tracking data #####
# load the data
tracking <- readRDS('../data/tracking_week_9.rds') %>% 
  select(-s, -a) %>% 
  mutate(week = 8) %>%  # join based on the week prior
  left_join(plays, by = c('gameId', 'playId')) %>%
  left_join(tackles, by = c('gameId', 'playId', 'nflId')) %>% 
  left_join(out, by = c('displayName', 'defensiveTeam', 'week')) 

# define function for Euclidean Distance (in feet)
Euclidean.Dist <- function(x, y) {sqrt(sum((x - y)^2))} # in terms of feet

# slice example play
example <- tracking %>% filter(playId == 746, gameId == 2022110602, ) %>% arrange(frameId, nflId)

# calculate distances between football and LoS
stats <- data.frame()
direction <- unique(example$playDirection)
los_coords <- example %>% filter(displayName=='football') %>% select(x, y) %>% slice(1)

for (frame in unique(example$frameId)) {
  print(paste0(frame, '...'))
  sliced <- example %>% filter(frameId == frame)
  football_coords <- sliced %>% filter(displayName=='football') %>% select(x, y)
  
  for (player in unique(sliced$displayName)) {
    # how far is each defender from the football? 
    player_coords <- sliced %>% filter(displayName==player) %>% select(x, y)
    player_dist <- Euclidean.Dist(player_coords, football_coords)
    
    # is the football below or above the line of scrimmage? 
    if (los_coords$x == football_coords$x) {
      football_dist <- 0
    } 
    
    if (direction == 'right') {
      if (los_coords$x < football_coords$x) {
        football_dist <- Euclidean.Dist(los_coords$x, football_coords$x)
      } else {
        football_dist <- (-1)*Euclidean.Dist(los_coords$x, football_coords$x)
      }
    }
    
    if (direction == 'left') {
      if (los_coords$x < football_coords$x) {
        football_dist <- (-1)*Euclidean.Dist(los_coords$x, football_coords$x)
      } else {
        football_dist <- Euclidean.Dist(los_coords$x, football_coords$x)
      }
    }
    
    # store statistics
    stats <- rbind(stats, data.frame(frame, player, player_dist, football_dist))
  }
}

Transform.BITE <- function(player, football, a, b, x, n) {
  input <- ifelse(football<0, player+football, player+sqrt(football))
  probs <- 1/(1+exp(0.5*(input)))
  return(qbeta(probs, a+x, b+n-x))
}

# show what this plot looks like
png('../results/inverse_sigmoid.png', units='in', width = 5, height = 5, res = 500)
x <- seq(-20, 20, length.out = 100000)
y <- 1/(1+exp(0.5*(x)))
plot(
  x, y, 'l', xlab=TeX('$\\it{d_t}$ (in yards)'), ylab=TeX('$\\it{p_t}$'), 
  main=TeX('$\\it{p_t} = \\frac{1}{1+exp\\{0.5\\it{d_t}\\}}$')
  )
abline(h=0.5, lty = 2, col = 'grey')
abline(v=0, lty = 2, col = 'grey')
dev.off()

# transform the priors and BITE statistics
example.play <- example %>% 
  left_join(stats, by = c('frameId'='frame', 'displayName'='player')) %>% 
  mutate(
    BITE_frame = ifelse(
      is.na(BITE), NA, 
      Transform.BITE(player_dist, football_dist, a, b, x_cumulative, n_cumulative)
      )
    ) %>%
  select(
    playDescription, frameId, nflId, displayName, jerseyNumber, defensiveTeam, possessionTeam,
    x, y, player_dist, football_dist,x_cumulative, n_cumulative, a, b, BITE, BITE_frame, event
    )


##### Visualize the example play #####
options(gganimate.dev_args = list(height=500, width = 500, units = 'px', res = 100))

# produce gif1
## citation: https://rstudio-pubs-static.s3.amazonaws.com/494188_154898728be3411ebab39050ca8a8dcd.html
xmin <- 0
xmax <- 160/3
hash.right <- 38.35
hash.left <- 12
hash.width <- 3.3

# set boundaries for a given play
ymin <- max(round(min(example.play$x, na.rm = TRUE) - 10, -1), 0)
ymax <- min(round(max(example.play$x, na.rm = TRUE) + 10, -1), 120)
df.hash <- expand.grid(x = c(0, 23.36667, 29.96667, xmax), y = (10:110))
df.hash <- df.hash %>% filter(!(floor(y %% 5) == 0))
df.hash <- df.hash %>% filter(y < ymax, y > ymin)

# animate the play
animate.play <- example.play %>% 
  mutate(
    team = case_when(
      displayName=='football' ~ 'football', 
      is.na(BITE_frame) ~ possessionTeam,
      TRUE  ~ defensiveTeam  
    )
  ) %>% 
  ggplot() +
  scale_size_manual(values = c(6, 6, 4), guide = FALSE) +
  scale_shape_manual(values = c(21, 21, 16), guide = FALSE) +
  scale_fill_manual(values = c("#0085CA", "#000000", "#964B00"), guide = FALSE) +
  scale_colour_manual(values = c("#0085CA", "#000000", "#964B00"), guide = FALSE) +
  annotate("text", x = df.hash$x[df.hash$x < 55/2], 
           y = df.hash$y[df.hash$x < 55/2], label = "_", hjust = 0, vjust = -0.2) + 
  annotate("text", x = df.hash$x[df.hash$x > 55/2], 
           y = df.hash$y[df.hash$x > 55/2], label = "_", hjust = 1, vjust = -0.2) + 
  annotate("segment", x = xmin, 
           y = seq(max(10, ymin), min(ymax, 110), by = 5), 
           xend =  xmax, 
           yend = seq(max(10, ymin), min(ymax, 110), by = 5)) + 
  annotate("text", x = rep(hash.left, 11), y = seq(10, 110, by = 10), 
           label = c("G   ", seq(10, 50, by = 10), rev(seq(10, 40, by = 10)), "   G"), 
           angle = 270, size = 4) + 
  annotate("text", x = rep((xmax - hash.left), 11), y = seq(10, 110, by = 10), 
           label = c("   G", seq(10, 50, by = 10), rev(seq(10, 40, by = 10)), "G   "), 
           angle = 90, size = 4) + 
  annotate("segment", x = c(xmin, xmin, xmax, xmax), 
           y = c(ymin, ymax, ymax, ymin), 
           xend = c(xmin, xmax, xmax, xmin), 
           yend = c(ymax, ymax, ymin, ymin), colour = "black") + 
  geom_point(
    aes(x = (xmax-y), y = x, size = team, shape = team, fill = team, group = nflId, color = team),
    alpha = 0.7
    ) +
  geom_text(
    data = example.play, 
    aes(x = (xmax-y), y = x, label = jerseyNumber), 
    colour = "white", vjust = 0.35, size = 3.5
    ) + 
  ylim(ymin, ymax) + 
  coord_fixed() +
  theme_nothing() +
  transition_time(frameId)  +
  ease_aes('linear') + 
  NULL

# ensure timing of play matches 10 frames-per-second
play.length.ex <- length(unique(example.play$frameId))

gif1 <- animate(
  animate.play, fps = 10, nframe = play.length.ex, 
  renderer = gifski_renderer('../results/anim1.gif')
  )

# produce gif2
times <- example.play %>% 
  filter(displayName == 'Shaq Thompson') %>% 
  mutate(
    lagged = lag(football_dist), 
    los_change = ifelse(football_dist > 0 & lagged < 0, 1, 0)
  ) %>% 
  filter(los_change == 1) %>% 
  mutate(event = 'los_cross') %>% 
  select(event, time=frameId) %>% 
  rbind(
    example.play %>% 
      group_by(event) %>% 
      summarize(time=unique(frameId)) %>% 
      filter(!is.na(event))
  ) %>% 
  filter(event %in% c('ball_snap', 'los_cross', 'tackle')) 

animate.bite <- example.play %>%
  filter(jerseyNumber %in% c(25, 7, 38, 95)) %>% 
  filter(!is.na(BITE_frame)) %>% 
  ggplot(aes(x = frameId, y = BITE_frame*100, group = jerseyNumber)) +
  geom_line() +
  geom_point(size = 7, fill = "#0085CA", color = "#0085CA") +  # Filled blue points
  geom_text(aes(label = jerseyNumber), color = 'white', size = 3) +  # White-colored text inside points
  labs(x = TeX('\\it{t}'), y = TeX('$\\it{BITE_t}$ (scaled to a 0-100 range)'), title = NULL) +
  transition_reveal(along = frameId) +
  theme_minimal() +
  geom_vline(data = times, aes(xintercept = time, color = event), linetype = "dashed") +
  scale_color_discrete(labels = c('Ball snapped', 'Ball crosses LoS', 'Tackle made')) +
  theme(legend.title = element_blank(), legend.box = "horizontal", legend.position = "bottom")

gif2 <- animate(
  animate.bite, fps = 10, nframe = play.length.ex,
  renderer = gifski_renderer('../results/anim2.gif')
  )

# combine the plots side-by-side
## citation: https://www.connorrothschild.com/post/tidy-tuesday-powerlifting
a_mgif <- image_read(gif1)
b_mgif <- image_read(gif2)
c_mgif <- image_read('../results/example_play.gif')

vid.gif <- c_mgif[1]
stats.gif <- image_append(c(a_mgif[3], b_mgif[3]), stack = FALSE)
new.gif <- image_append(c(vid.gif, stats.gif), stack = TRUE)

for(i in 2:length(c_mgif)){
  print(paste0(i, '...'))
  vid <- c_mgif[i]
  stats <- image_append(c(a_mgif[i+2], b_mgif[i+2]), stack = FALSE)
  combined <- image_append(c(vid, stats), stack = TRUE) 
  new.gif <- c(new.gif, combined)
}

anim_save('../results/animation.gif', new.gif)
