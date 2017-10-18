# Messing around with some different data viz on internet speeds.

# Idea 1. Line graph of top 5 current fastest countries and the US, average speeds plotted over time.

# Idea 2. Similar to Idea 1, but with top 5 fastest and top 5 slowest U.S. states.

# Idea 3. Map of U.S. Showing states' internet user concentrations (as proportion of total state population) in an elevation/gradient format.

# Idea 4. Map of U.S. showing states with fastest internet.

library(tidyverse)
library(readr)
library(stringr)
# library(readxl)
# library(lubridate)

# connect to plotly API ----------------------------------------------------------------------------------

# library(plotly)
# Sys.setenv("plotly_username" = "freshmemes")
# Sys.setenv("plotly_api_key" = "ZkgDGWCjSBpV4kQJR3sz")

# Idea 1. ------------------------------------------------------------------------------------------------

spdata <- as.tibble(read.csv("us_speeds_over_time.csv"))

spdata2 <- spdata %>% 
  gather(country, speed, UNITED.STATES:FINLAND)

spdata2$country = stringr::str_replace(spdata2$country, "\\.", "\\ ")

spdata3 <- spdata2 %>% 
  mutate(speed = speed / 1000) %>% 
  filter(stringr::str_detect(Quarter, "(^Q4)|(17$)")) %>% 
  mutate(Quarter = stringr::str_replace(Quarter, "^Q(4|1)\\ ", "20")) %>% 
  rename(year = Quarter) %>% 
  mutate(year = as.numeric(year))

ggplot(spdata3, aes(year, speed, color = country)) + 
  geom_line(size = 1) +
  geom_point() +
  labs(title = "Historical Average Speeds of Current Top 5 Fastest Countries and the U.S.") +
  scale_x_continuous(breaks = c(2008, 2010, 2012, 2014, 2016))

# Idea 1. Optional (create and upload plotly) ------------------------------------------------------------

# p <- plotly::plot_ly(data = spdata3, x = ~year, y = ~speed, color = ~country, mode = 'lines+markers', type = 'scatter') %>%
#   layout(title = 'Historical Average Speeds of Current Top 5 Fastest Countries and the U.S.')
# plotly_POST(p, filename = "countries_speeds_over_time", sharing = "public")

# Idea 2. ------------------------------------------------------------------------------------------------

states <- as.tibble(read.csv("states_speeds_over_time.csv"))

stnames <- c("District of Columbia", "Delaware", "Massachusetts", "Rhode Island", "Maryland", "Kentucky", "Arkansas", "Mississippi", "New Mexico", "Idaho")

states2 <- states %>% 
  gather(state, speed, U.S....ALABAMA:U.S....WYOMING)

states2$state = stringr::str_replace(states2$state, "U\\.S(\\.{4})", "")
states2$state = stringr::str_replace_all(states2$state, "\\.", "\\ ")

states3 <- states2 %>%
  mutate(state = paste0(substring(state, 1, 1), tolower(substring(state, 2, str_length(state))))) 
states3$state = stringr::str_replace(states3$state, "District of columbia", "District of Columbia")
states3$state = stringr::str_replace(states3$state, "Rhode island", "Rhode Island")
states3$state = stringr::str_replace(states3$state, "New mexico", "New Mexico")


states3 <- states3 %>% 
  filter(state %in% stnames) %>% 
  filter(stringr::str_detect(Quarter, "(^Q4)|(17$)")) %>% 
  mutate(Quarter = stringr::str_replace(Quarter, "^Q(4|1)\\ ", "20")) %>% 
  rename(year = Quarter) %>% 
  mutate(year = as.numeric(year))

ggplot(states3, aes(year, speed, color = state)) +
  geom_line(size = 1) + 
  geom_point() +
  labs(title = "Historical Average Speeds of Current Fastest and Slowest States") + 
  scale_x_continuous(breaks = c(2008, 2010, 2012, 2014, 2016))

# Idea 2. Optional (create and upload plotly) ------------------------------------------------------------

# p2 <- plotly::plot_ly(data = states3, x = ~year, y = ~speed, color = ~state, mode = 'lines+markers', type = 'scatter') %>%
#   layout(title = 'Historical Average Speeds of Current Top 5 Fastest and Slowest States')
# plotly_POST(p2, filename = "states_speeds_over_time", sharing = "public")
