# which airlines saw the largest variation in fatal accident rate* 
# between 1985 - 1999 and 2000 - 2014?
# *fatal accident rate = fatalities / fatal_accidents

# install packages
library(tidyverse)
library(tidyr)

# read in data from github
airline_safety_data <- read_csv("Documents/Airline_Safety_538.csv")

# create new metric for comparison: fatal accident rate
grouped_airline_safety <- airline_safety_data %>%
  filter(type_of_event != "incidents") %>%
  group_by(airline, year_range) %>%
  spread(type_of_event, n_events) 
  mutate(fatal_accident_rate = ifelse(fatal_accidents == 0 | fatalities == 0, 0, (fatalities / fatal_accidents)))

# do airlines with more incidents have more fatalities?
airline_total_data <- grouped_airline_safety %>%
  group_by(airline) %>%
  summarise(tot_avail_seat_km_per_week = sum(avail_seat_km_per_week),
            tot_fatal_accidents = sum(fatal_accidents),
            tot_fatalities = sum(fatalities)) %>%
  mutate(airline_size = ntile(tot_avail_seat_km_per_week, 5),
         us_airline = ifelse(airline %in% c("Alaska Airlines*","American*","Delta / Northwest*","Hawaiian Airlines",
                                            "Southwest Airlines","United / Continental*","US Airways / America West*"),
                                            1, 0))

# plot avail seat km per week and fatalities 
ggplot() + 
  geom_point(aes(x = tot_avail_seat_km_per_week, y = tot_fatalities, color = as.factor(airline_size)), data = airline_total_data) + 
  geom_smooth(aes(x = tot_avail_seat_km_per_week, y = tot_fatalities), method = "lm", se = FALSE, color ="grey", data = airline_total_data) + 
  theme_minimal() + 
  scale_color_manual(name = "Airline Size", 
                     labels = c("Category 1", "Category 2", "Category 3", "Category 4", "Category 5"),
                     values = c("#66c2a5","#fc8d62","#8da0cb","#e78ac3","#a6d854")) + 
  labs(x = "Total Available Seat KM Per Week",
       y = "Total Airline Fatalities (1985 - 2014)",
       title = "Slight Correlation Between Airline Size And Fatalities",
       caption = "Available Seat KM Per Week on Log10 Scale",
       subtitle = "Airlines Grouped Into Categories By Size") + 
  scale_y_continuous(limits = c(0,800), breaks = c(0,100,200,300,400,500,600,700,800)) + 
  scale_x_log10(limits = c(500000000, 15000000000)) +
  theme(panel.grid.minor.y = element_line(color = "white")) 

# How do us airlines compare to global airlines for total fatalities?
ggplot() + 
  geom_point(aes(x = tot_avail_seat_km_per_week, y = tot_fatalities, color = as.factor(us_airline)), data = airline_total_data) + 
  geom_smooth(aes(x = tot_avail_seat_km_per_week, y = tot_fatalities), method = "lm", se = FALSE, color = "black", data = airline_total_data) + 
  theme_minimal() + 
  scale_x_log10(limits = c(500000000, 15000000000)) + 
  scale_y_continuous(limits = c(0,800), breaks = c(0,100,200,300,400,500,600,700,800)) + 
  labs(title = "How Do US Airlines Compare To Other Airlines?",
       y = "Total Airlines Fatalities (1985 - 2014)",
       x = "Total Available Seat KM Per Week",
       caption = "Available Seat KM Per Week on Log10 Scale") + 
  scale_color_manual(name = "", labels = c("Rest of World", "US Airline"), values = c("light grey","navy blue")) +
  theme(panel.grid.minor.y = element_line(color = "white")) +
  annotate("text", label = "Alaska", x = 1900000000, y = 115, size = 3, color = "black") + 
  annotate("text", label = "American", x = 10000000000, y = 548, size = 3, color = "black") + 
  annotate("text", label = "Delta/Northwest", x = 12000000000, y = 487, size = 3, color = "black") + 
  annotate("text", label = "Hawaiian", x = 1000000000, y = 26, size = 3, color = "black") + 
  annotate("text", label = "Southwest", x = 6500000000, y = 26, size = 3, color = "black") + 
  annotate("text", label = "United/Continental", x = 12000000000, y = 400, size = 3, color = "black") + 
  annotate("text", label = "US Airways/America West", x = 5000000000, y = 275, size = 3, color = "black")

  

  
