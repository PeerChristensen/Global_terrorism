# where attacks occur

library(tidyverse)
library(hrbrthemes)
library(ggsci)

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

