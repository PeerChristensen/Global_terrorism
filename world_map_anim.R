
library(tidyverse)
library(maps)
library(ggthemes)
library(gganimate)

df <- read_csv("global_terror_clean.csv")

df <- df %>% filter(nkill>0)

world <- ggplot() +
  borders("world", colour = "gray85", fill = "gray80") +
  theme_map() +
  theme(plot.background = element_rect(fill="#2B2828"),
        plot.title = element_text(size= 18,colour="snow"),
        legend.position = "none") +
  coord_map(xlim=c(-180,180)) +
  scale_y_continuous(limits=c(-60,90)) +
  theme(panel.background = element_blank())

world +
  geom_point(aes(x = longitude, y = latitude, size = log(nkill)),
             data = df, 
             colour = 'darkred', alpha = .03) +
  scale_size_continuous(range = c(2, 10), 
                        breaks = c(250, 500, 750, 1000))

map2 <- world +
  geom_point(aes(x = longitude, y = latitude, size = nkill),
             data=df,colour = 'darkred', alpha = .5) +
  scale_size_continuous(range = c(2, 12)) +
  # transition_states(year,
  #                   transition_length = 2,
  #                   state_length = 1) +
  # ggtitle('Year: {closest_state}')
  transition_time(year) +
  labs(title = "Year: {round(frame_time,0)}") +
  exit_fade() +
  ease_aes('linear') 

animate(map2,fps=3,nframes=n_distinct(df$year))
anim_save("myGif.gif")


