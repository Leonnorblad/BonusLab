library(ggplot2)
library(nycflights13)
library(dplyr)
library(RColorBrewer)

visualize_airport_delays <- function(){
  # Imprt the data
  data(flights)
  data(airports)
  
  # Data and plot
  left_join(flights, airports, by = c("origin" = "faa")) %>% # Joins flights data and airports data
    select(dep_delay, lat, lon, origin) %>%                  # Keeps delay, latitude, longitude and origin
    group_by(origin) %>%                                     # Group data by origin
    summarize(Mean_delay = mean(dep_delay, na.rm=TRUE),      # Calculate mean for every origin
              lat = unique(lat),                             # Latitude of airport
              lon = unique(lon)) %>%                         # Longitude of airport
    na.omit() %>%                                            # Remove NA (a few origins had missing coordinated)
    ggplot() +                                               # Plot
    geom_point(aes(x=lat,                                    # x=longitude
                   y=lon,                                    # y=latitude
                   color=Mean_delay),                        # Color of points = mean delay
               size=2.5) +                                   # Size of points
    theme_bw() +                                             # Black and white theme
    labs(color="Mean delay",                                 # Legend title
         x="Latutude",                                       # x title
         y="Longitude",                                      # y title
         title = "Mean departure delay") +                   # plot title
    theme(axis.text = element_text(size=11),                 # Adjust axis textsize
          axis.title = element_text(size=12,                 # Adjust axis title textsize
                                    face="bold"),            # Bold axis text
          plot.title = element_text(size=18,                 # Plot title size
                                    face="bold",             # Bold plot title
                                    hjus=0.5),               # Center plot title
          legend.title = element_text(size=12),              # Legend title size 
          legend.text = element_text(size=11))               # Legend text size
}
visualize_airport_delays()
