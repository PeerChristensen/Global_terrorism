# How many attacks by terrorist groups

library(tidyverse)
library(hrbrthemes)
library(ggsci)
library(gganimate)

df <- read_csv("global_terror_clean.csv") 

# attacks
attacks <- df %>%
  filter(gname != "Unknown") %>%
  group_by(gname) %>%
  count() %>%
  arrange(desc(n)) %>%
  ungroup() %>%
  top_n(25) %>%
  ggplot(aes(reorder(gname,n),n, fill = n)) +
  geom_col(colour=NA) +
  scale_fill_material("red") +
  theme_modern_rc() +
  coord_flip()
attacks

# casualties
kills <- df %>%
  filter(gname != "Unknown") %>%
  group_by(gname) %>%
  summarise(n = sum(nkill,na.rm=T)) %>%
  arrange(desc(n)) %>%
  ungroup() %>%
  top_n(25) %>%
  ggplot(aes(reorder(gname,n),n, fill = n)) +
  geom_col(colour=NA) +
  scale_fill_material("red") +
  theme_modern_rc() +
  coord_flip()
kills

# attacks animated
attacks +
  transition_states(year, transition_length = 4, state_length = 1) +
  view_follow(fixed_x = TRUE)  +
  labs(title = 'GDP per Year : {closest_state}',  
       subtitle  =  "Top 10 Countries",
       caption  = "GDP in Billions USD | Data Source: World Bank Data")

# casualties animated
