
library(highcharter)

d <- df %>% 
  filter(nkill>0) %>%
  select(country_txt,city,attacktype1_txt,longitude,latitude,nkill) %>%
  mutate(location = glue::glue("{city}, {country_txt}"),
         lat=latitude,
         lon=longitude,
         z = nkill) %>%
  select(location,attacktype1_txt,lat,lon,z)

x <- c("Location", "Attack type", "Casualties")
y <- c("{point.location}", "{point.attacktype1_txt}", "{point.z}")
tltip <- tooltip_table(x,y)

hcmap("custom/world", showInLegend = FALSE) %>%
  hc_add_series(data = d,
                type = "mapbubble",
                name="attack info",maxSize = '2%',
                color="darkred") %>% 
  hc_mapNavigation(enabled = TRUE) %>%
  hc_chart(backgroundColor = "#1E1E1E") %>%
  hc_legend(enabled = F) %>%
  hc_tooltip(useHTML = TRUE, headerFormat = "", pointFormat = tltip) 

