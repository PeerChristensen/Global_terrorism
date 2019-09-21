# targets

library(tidyverse)
library(hrbrthemes)
library(ggsci)
library(scales)

df <- read_csv("global_terror_clean.csv") 

#type
df %>%
  group_by(targtype1_txt) %>%
  count() %>%
  arrange(desc(n)) %>%
  ungroup() %>%
  ggplot(aes(reorder(targtype1_txt,n),n, fill = n)) +
  geom_col(colour=NA) +
  scale_fill_material("red") +
  theme_modern_rc() +
  coord_flip()

# subtype
df %>%
  filter(!is.na(targsubtype1_txt)) %>%
  group_by(targsubtype1_txt) %>%
  count() %>%
  arrange(desc(n)) %>%
  ungroup() %>%
  top_n(25) %>%
  ggplot(aes(reorder(targsubtype1_txt,n),n, fill = n)) +
  geom_col(colour=NA) +
  scale_fill_material("red") +
  theme_modern_rc() +
  coord_flip()

#target1
df %>%
  filter(!is.na(target1)) %>%
  group_by(target1) %>%
  count() %>%
  arrange(desc(n)) %>%
  ungroup() %>%
  top_n(25) %>%
  mutate(pct = n/sum(n)*100) %>%
  ggplot(aes(reorder(target1,pct),pct, fill = log(pct))) +
  geom_col(colour=NA) +
  scale_fill_material("red") +
  theme_modern_rc() +
  theme(panel.grid.major.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "none") +
  scale_y_percent(scale=1) +
  coord_flip()
