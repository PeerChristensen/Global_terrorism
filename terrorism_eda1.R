
library(readxl)
library(tidyverse)

df <- read_excel("globalterrorismdb_0718dist.xlsx")

remove <- df %>% 
  map_df(~ sum(is.na(.))) %>% 
  select_if(.>0) %>% 
  gather() %>% 
  mutate(na_prop = value/nrow(df)) %>%
  filter(na_prop>.2) %>%
  pull(key)

remove <- remove[!remove %in% c("summary","nperps")]

df <- df %>% select(-remove,-extended, -country,-region,-provstate,-specificity,-vicinity,-starts_with("crit"),
                    -doubtterr,-ends_with("type1"), -ends_with("type2"),-ends_with("type3"),
              -natlty1,-property,-ishostkid,-dbsource,-starts_with("INT"))

df <- df %>% 
  mutate(iday = if_else(iday==0,1,iday),
         imonth = if_else(imonth==0,1,imonth)) %>%
  mutate(date = lubridate::ymd(paste(iyear,imonth,iday))) %>%
  select(-iyear,-imonth,-iday)

df <- df %>% mutate_if(is.character,factor)

df$year <- lubridate::year(df$date)

df <- df %>% filter(longitude > -2e+07)

write_csv(df,"global_terror_clean.csv")
#summary(glm(factor(success) ~ region_txt,data=df,family="binomial"))

              