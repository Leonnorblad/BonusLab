
# Reminder: Put in ggplot2 in the package
library(ggplot2)
library(nycflights13)
library(dplyr)


# Mean delay of flights for different airports by longitude and latitude
# delays from 'flights' data
# airport information from 'airports"
data(flights)
data(airports)
airports %>% filter(faa=="LGA")

left_join(flights, airports, 
          by = c("origin" = "faa")) %>%
  select(dep_delay, arr_delay, lat, lon, dest) %>%
  filter(dest=="IAH")
  

visualize_airport_delays <- function(){
  
}

