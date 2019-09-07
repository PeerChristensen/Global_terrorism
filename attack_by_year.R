# How many attacks per year? 
#   what is the trend?

library(tidyverse)
library(hrbrthemes)
library(ggsci)

df <- read_csv("global_terror_clean.csv") 

# attacks
df %>%
  group_by(year) %>%
  count() %>%
  ggplot(aes(year,n, fill = n)) +
  geom_col(colour=NA) +
  scale_fill_material("red") +
  theme_modern_rc()

# casualties
df %>%
  group_by(year) %>%
  summarise(n = sum(nkill,na.rm=T)) %>%
  ggplot(aes(year,n, fill = n)) +
  geom_col(colour=NA) +
  scale_fill_material("red") +
  theme_modern_rc()


