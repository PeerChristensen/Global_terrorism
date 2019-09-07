# Which were the attacks with highest number of casualties?
# Do they have something in common?

library(tidyverse)
library(hrbrthemes)
library(ggsci)

df <- read_csv("global_terror_clean.csv") %>%
  mutate(id = paste(year,city,country_txt,attacktype1_txt))

df %>%
  select(date,id,nkill,nwound) %>%
  distinct(id, .keep_all = T) %>%
  arrange(desc(nkill)) %>%
  top_n(25,nkill) %>%
  ggplot(aes(x = reorder(id,nkill),y=nkill,fill=log(nkill))) +
  geom_col(colour=NA) +
  coord_flip() +
  theme_modern_rc() +
  scale_fill_material("red")
  
