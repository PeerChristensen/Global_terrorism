# Weapons

library(tidyverse)
library(hrbrthemes)
library(ggsci)

df <- read_csv("global_terror_clean.csv") 

#type
df %>%
  mutate(weaptype1_txt = recode(weaptype1_txt,"Vehicle (not to include vehicle-borne explosives, i.e., car or truck bombs)" 
="Vehicle")) %>%
  group_by(weaptype1_txt) %>%
  count() %>%
  arrange(desc(n)) %>%
  ungroup() %>%
  ggplot(aes(reorder(weaptype1_txt,n),n, fill = n)) +
  geom_col(colour=NA) +
  scale_fill_material("red") +
  theme_modern_rc() +
  coord_flip()

# subtype
df %>%
  filter(!is.na(weapsubtype1_txt)) %>%
  mutate(weapsubtype1_txt = recode(weapsubtype1_txt,"Vehicle (not to include vehicle-borne explosives, i.e., car or truck bombs)" 
                                ="Vehicle")) %>%
  group_by(weapsubtype1_txt) %>%
  count() %>%
  arrange(desc(n)) %>%
  ungroup() %>%
  ggplot(aes(reorder(weapsubtype1_txt,n),n, fill = n)) +
  geom_col(colour=NA) +
  scale_fill_material("red") +
  theme_modern_rc() +
  coord_flip()
