library(readxl)
library(dtplyr)
library(tidyverse)

df <- read_excel("globalterrorismdb_0718dist.xlsx")

df <- df %>% 
  dtplyr::lazy_dt()

df <- df$parent

table(df$attacktype1_txt)

