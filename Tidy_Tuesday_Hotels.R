###########################################################
# Tidy Tuesday 2020-02-11                                 #
# Which Market Segment has the highest cancellation rate? #                               
###########################################################

# load packages
library(tidyverse)

# read data
hotels <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-11/hotels.csv')

# aggregate data by market segment 
booking_cancel_rate <- hotels %>%
  group_by(market_segment) %>%
  filter(market_segment != "Undefined") %>%
  summarise(booking_count = n(),
            num_cancellations = sum(is_canceled),
            cancellation_rate = round((num_cancellations/booking_count)*100, 2))

# create plot
ggplot(data = booking_cancel_rate) + 
  geom_bar(aes(x = market_segment, y = booking_count, fill = cancellation_rate), stat = "identity")  + 
  scale_fill_continuous(type = "viridis") + 
  theme_minimal() + 
  scale_y_continuous(breaks = seq(0,60000,10000), limits = c(0,60000), labels = scales::comma) +
  theme(plot.background = element_rect(fill = "black"),
        axis.text = element_text(color = "white", face = "bold"),
        axis.title = element_text(color = "white", face = "bold"),
        legend.text = element_text(color = "white"),
        legend.title = element_text(color = "white", face = "bold"),
        plot.title = element_text(color = "white", face = "bold", size = 20),
        plot.caption = element_text(color = "white", face = "bold"),
        plot.subtitle = element_text(color = "white", face = "italic"),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank()) + 
  labs(title = "Cancellation Rate by Market Segment",
       subtitle = "Group bookings account for 1/6 of total bookings and have highest Cancellation Rate",
       x = "",
       y = "Total Booking Count",
       fill = "Cancellation Rate",
       caption = "Data sourced from Antonio, Almeida and Nunes from 2015 - 2017") 


