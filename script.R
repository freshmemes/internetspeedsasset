# Messing around with some different data viz on internet speeds.

# Idea 1. Line graph of top 5 fastest countries and the US, average speeds plotted over time.

# Idea 2. Map of U.S. Showing states' internet user concentrations (as proportion of total state population) in an elevation/gradient format.

# Idea 3. Map of U.S. showing states with fastest internet.

library(tidyverse)
library(readr)
library(stringr)
# library(readxl)
# library(lubridate)

# Idea 1. ------------------------------------------------------------------------------------------------

spdata <- as.tibble(read.csv("us_speeds_over_time.csv"))

spdata2 <- spdata %>% 
  gather(country, speed, UNITED.STATES:FINLAND)

spdata2$country = stringr::str_replace(spdata2$country, "\\.", "\\ ")

spdata3 <- spdata2 %>% 
  mutate(speed = speed / 1000) %>% 
  filter(stringr::str_detect(Quarter, "^Q4")) %>% 
  mutate(Quarter = stringr::str_replace(Quarter, "^Q4\\ ", "20")) %>% 
  rename(year = Quarter) %>% 
  mutate(year = as.numeric(year))

ggplot(spdata3, aes(year, speed, color = country)) + 
  geom_line(size = 1) +
  geom_point() +
  labs(title = "Historical Average Speeds of Current Top 5 Fastest Countries and the U.S.")

# Idea 1. Optional ---------------------------------------------------------------------------------------

# library(plotly)
# plotly::plot_ly(data = spdata3, x = ~year, y = ~speed, color = ~country, mode = 'lines+markers', type = 'scatter') %>% 
#   layout(title = 'Historical Average Speeds of Current Top 5 Fastest Countries and the U.S.')
