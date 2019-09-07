
# How are attack types distributed?
# How efficient are individual attack types wrt nkills, and over time?

library(tidyverse)
library(hrbrthemes)
library(ggsci)

df <- read_csv("global_terror_clean.csv") 

# n attack by type
df %>%
  group_by(attacktype1_txt) %>%
  count() %>%
  ggplot(aes(reorder(attacktype1_txt,n),n, fill = n)) +
  geom_col(colour=NA) +
  scale_fill_material("red") +
  theme_modern_rc() +
  coord_flip()

# efficiency overall

df %>%
  group_by(attacktype1_txt) %>%
  summarise(n = n(),
            kills = sum(nkill,na.rm=T)) %>%
  mutate(mean_nkill = kills / n) %>%
  ggplot(aes(reorder(attacktype1_txt,mean_nkill),mean_nkill, fill = mean_nkill)) +
  geom_col(colour=NA) +
  scale_fill_material("red") +
  theme_modern_rc() +
  coord_flip()

# efficiency by year
df %>%
  filter(attacktype1_txt %in% c("Armed Assault","Bombing/Explosion","10186","Hostage Taking (Kidnapping)")) %>%
  group_by(year,attacktype1_txt) %>%
  summarise(n = n(),
            kills = sum(nkill,na.rm=T)) %>%
  mutate(mean_nkill = kills / n) %>%
  ggplot(aes(year,mean_nkill, fill = mean_nkill)) +
  geom_col(colour=NA) +
  facet_wrap(~attacktype1_txt,scales="free",ncol=2) +
  scale_fill_material("red") +
  theme_modern_rc() +
  theme(strip.text.x = element_text(size = 12, color = "snow"))


