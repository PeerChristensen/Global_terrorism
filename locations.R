# where attacks occur

library(tidyverse)
library(hrbrthemes)
library(ggsci)
library(gganimate)
library(ggthemes)
library(viridis)

df <- read_csv("global_terror_clean.csv") 

df %>%
  group_by(region_txt) %>%
  count() %>%
  ggplot(aes(reorder(region_txt,n),n,fill=n)) +
  geom_col(colour=NA) +
  coord_flip() +
  theme_modern_rc() +
  scale_fill_material("red")

df %>%
  group_by(country_txt) %>%
  count() %>%
  ungroup()%>%
  top_n(20) %>%
  ggplot(aes(reorder(country_txt,n),n,fill=n)) +
  geom_col(colour=NA) +
  coord_flip() +
  theme_modern_rc() +
  scale_fill_material("red")

df %>%
  filter(city != "Unknown") %>%
  mutate(city2 = paste0(city,", ",country_txt)) %>%
  group_by(city2) %>%
  count() %>%
  ungroup()%>%
  top_n(20) %>%
  ggplot(aes(reorder(city2,n),n,fill=n)) +
  geom_col(colour=NA) +
  coord_flip() +
  theme_modern_rc() +
  scale_fill_material("red")


# regions  by year
regions <- df %>%
  group_by(year,region_txt) %>%
  count() %>%
  ungroup() %>%
  complete(year,region_txt,fill=list(n=0)) %>%
  group_by(region_txt) %>%
  mutate(cs = cumsum(n)) %>% 
  ungroup() %>%
  group_by(year) %>%
  arrange(year,desc(cs)) %>%
  mutate(order = rev(row_number())) %>%
  mutate(Rank = rank(-cs),
         Value_rel = cs/cs[Rank==1],
         Value_lbl = paste0(" ",round(cs/1e9)))  %>%
  ungroup() 

p<-regions %>%
  ggplot(aes(order,Value_rel,fill=region_txt,group=cs)) +
  #geom_col(colour=NA) +
  geom_tile(aes(y=cs/2,height=cs),width=0.9,show.legend = F)+
  coord_flip(clip="off") +
  theme_modern_rc() +
  scale_fill_viridis_d() +
  labs(title = "Year: {closest_state}") +
  transition_states(year, transition_length = 4, state_length = 1) +
  theme(legend.position = "none",
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        plot.margin = margin(1,1,1,6, "cm")) +
  geom_text(aes(y = 0, label = paste(region_txt, " ")), vjust = 0.2, hjust = 1,colour="snow") 

#animate(p,fps=2.5,nframes = n_distinct(regions$year)*2)
animate(p, 100, fps = 25, duration = 20, width = 800, height = 600)
