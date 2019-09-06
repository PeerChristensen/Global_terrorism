

library(maps)
library(ggthemes)
library(gganimate)

df <- read_csv("global_terror_clean.csv")

world <- ggplot() +
  borders("world", colour = "gray85", fill = "gray80") +
  theme_map() 

world +
  geom_point(aes(x = longitude, y = latitude, size = nkill),
             data = df, 
             colour = 'darkred', alpha = .05) +
  scale_size_continuous(range = c(2, 10), 
                        breaks = c(250, 500, 750, 1000)) +
  labs(size = 'nkill')

map2 <- world +
  geom_point(aes(x = longitude, y = latitude, size = nkill),
             data=df,colour = 'darkred', alpha = .5) +
  scale_size_continuous(range = c(2, 8), breaks = c(10, 20, 40, 60)) +
  # transition_states(year,
  #                   transition_length = 2,
  #                   state_length = 1) +
  # ggtitle('Year: {closest_state}')
  transition_time(year) +
  labs(title = "Year: {round(frame_time,0)}") +
  ease_aes('linear')

animate(map2,fps=3,nframes=n_distinct(df$year))

anim_plot <- excl_sem_plot +
  labs(title = 'K: {round(frame_time,0)}') +
  transition_time(as.numeric(K)) +
  ease_aes('linear')
animate(anim_plot, nframes = , fps = 0.5)
