# install packages
library(tidyverse)

# read in data for French National Train System
full_trains <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-02-26/full_trains.csv")

# find businest national departure for each Paris Train Station in 2017
paris_national <- full_trains %>%
  filter(departure_station %in% c("PARIS EST","PARIS LYON","PARIS MONTPARNASSE","PARIS NORD"),
         service == "National",
         year == 2017) %>%
  group_by(departure_station, arrival_station) %>%
  summarise(total_num_departures = sum(total_num_trips)) %>%
  arrange(desc(departure_station), desc(total_num_departures))

# filter to more than 5000 annual departures
paris_national_top <- paris_national %>%
  filter(total_num_departures >= 5000)

# create faceted bar graph for destinations
ggplot() + geom_bar(aes(x = reorder(arrival_station, total_num_departures), y = total_num_departures, fill = departure_station), data = paris_national_top, stat = "identity") + 
  facet_grid(~departure_station) + 
  coord_flip() +
  theme_bw() + 
  labs(title = "Top National Destinations Vary By Paris Train Station",
       caption = "National Routes With Over 5000 Annual Departures",
       y = "Total Number of 2017 Departures",
       x = "") + 
  geom_text(aes(y = total_num_departures, x = arrival_station, label = total_num_departures), 
            data = paris_national_top, hjust = 1.25, size = 3, color = "white")+
  theme(legend.position = "",
        plot.title = element_text(face = "bold"),
        panel.grid.minor = element_line(color = "white"),
        panel.grid.major.x = element_line(color = "white")) + 
  scale_fill_manual(values = c("#e41a1c","#377eb8","#4daf4a","#984ea3"))
        
  
  
