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
  ggplot(aes(year,n)) +
  theme_modern_rc() +
  geom_line(colour="darkred", size = 1)

# casualties
df %>%
  group_by(year) %>%
  summarise(n = sum(nkill,na.rm=T)) %>%
  ggplot(aes(year,n)) +
  geom_line(colour="darkred", size = 1) +
  theme_modern_rc()


