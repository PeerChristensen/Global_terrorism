
library(tidyverse)
library(maps)
library(ggthemes)
library(gganimate)
library(ggpointdensity)
library(scico)

df <- read_csv("global_terror_clean.csv")

df <- df %>% filter(nkill>0)

get_density <- function(x, y, ...) {
  dens <- MASS::kde2d(x, y, ...)
  ix <- findInterval(x, dens$x)
  iy <- findInterval(y, dens$y)
  ii <- cbind(ix, iy)
  return(dens$z[ii])
}


df$density<- get_density(df$longitude, df$latitude, n = 100)

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
  # geom_point(aes(x = longitude, y = latitude, size = log(nkill)),
  #            data = df, 
  #            colour = 'darkred', alpha = .03) +
  #scale_size_continuous(range = c(2, 10), 
  #                      breaks = c(250, 500, 750, 1000)) +
  #geom_point(aes(longitude, latitude, color = log(density),size=nkill),
  #           alpha=.5, data=df) + 
  geom_pointdensity(aes(x=longitude,y=latitude,size=nkill),data=df,adjust = 5) +
  scale_color_scico("lajolla",direction = -1,begin=.5,end=1)

map2 <- world +
  #geom_point(aes(x = longitude, y = latitude, size = nkill),
  #           data=df,colour = 'darkred', alpha = .2) +
  geom_point(aes(longitude, latitude, color = "darkred",size=nkill),
             alpha=.5, data=df) + 
  scale_size_continuous(range = c(2, 12)) +
  # transition_states(year,
  #                   transition_length = 2,
  #                   state_length = 1) +
  # ggtitle('Year: {closest_state}')
  transition_time(year) +
  labs(title = "Year: {round(frame_time,0)}")

animate(map2,fps=3,nframes=n_distinct(df$year))

anim_save("myGif.gif")

# rayshader
p <- world +
  # geom_point(aes(x = longitude, y = latitude, size = log(nkill)),
  #            data = df,
  #            colour = 'darkred', alpha = .03) +
  scale_size_continuous(range = c(2, 10),
                       breaks = c(250, 500, 750, 1000)) +
  geom_point(aes(longitude, latitude, color = log(density)),
            alpha=.5, data=df) 
#  geom_pointdensity(aes(x=longitude,y=latitude,size=nkill),data=df,adjust = 5) +
 # scale_color_scico("lajolla",direction = -1,begin=.5,end=1)

plot_gg(p, width = 3.5, multicore = TRUE, windowsize = c(800, 800), 
        zoom = 0.85, phi = 35, theta = 30, sunangle = 225, soliddepth = -100)
Sys.sleep(0.2)
render_snapshot(clear = TRUE)
