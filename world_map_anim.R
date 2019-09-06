

library(maps)
library(ggthemes)

world <- ggplot() +
  borders("world", colour = "gray85", fill = "gray80") +
  theme_map() 

d$year <- lubridate::year(d$date)

world +
  geom_point(aes(x = longitude, y = latitude, size = nkill),
             data = d, 
             colour = 'darkred', alpha = .5) +
  scale_size_continuous(range = c(1, 8), 
                        breaks = c(250, 500, 750, 1000)) +
  labs(size = 'nkill')

map2 <- world +
  geom_point(aes(x = longitude, y = latitude, size = nkill),
             data=d,colour = 'darkred', alpha = .5) +
  scale_size_continuous(range = c(1, 8), breaks = c(250, 500, 750, 1000)) +
  transition_states(year,
                    transition_length = 6,
                    state_length = 3) +
  ggtitle('Now showing {closest_state}')

map2
