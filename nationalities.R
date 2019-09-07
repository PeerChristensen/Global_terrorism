# nationalities of terrorists

library(tidyverse)
library(hrbrthemes)
library(ggsci)

df <- read_csv("global_terror_clean.csv") 

df %>%
  group_by(natlty1_txt) %>%
  count() %>%
  ungroup() %>%
  top_n(30,n) %>%
  ggplot(aes(reorder(natlty1_txt,n),n,fill=n)) +
  geom_col(colour=NA) +
  coord_flip() +
  theme_modern_rc() +
  scale_fill_material("red")
