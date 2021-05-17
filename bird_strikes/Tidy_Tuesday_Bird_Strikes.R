# Tidy Tuesday 2019-07-23
# Airline Bird Strikes
# At what stage of flight do most bird strikes happen?

# Load Packages 
library(tidyverse)
library(ggthemes)

# read in dataset
wildlife_impacts <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-07-23/wildlife_impacts.csv")

# same phase of flight has different labels -- need to combine together
table(wildlife_impacts$phase_of_flt)

# clean stage of flight
wildlife_impacts_cleaned <- wildlife_impacts %>%
  mutate(phase_of_flt = ifelse(phase_of_flt %in% c("approach", "Approach", "APPROACH"), "Approach",
                        ifelse(phase_of_flt %in% c("Arrival", "ARRIVAL"), "Arrival",
                        ifelse(phase_of_flt %in% c("climb", "Climb", "CLIMB"), "Climb",
                        ifelse(phase_of_flt %in% c("Departure", "DEPARTURE"), "Departure",
                        ifelse(phase_of_flt %in% c("landing roll", "Landing roll", "Landing Roll", "LANDING ROLL"), "Landing",
                        ifelse(phase_of_flt %in% c("take-off run", "Take-off run", "Take-off Run", "TAKE-OFF RUN"), "Take-Off",
                        ifelse(phase_of_flt == "En Route", "En Route",
                        ifelse(phase_of_flt == "Descent", "Descent",
                        ifelse(phase_of_flt == "Local", "Local",
                        ifelse(phase_of_flt == "Parked", "Parked",
                        ifelse(phase_of_flt == "Taxi", "Taxi", "Unknown"))))))))))))

# after combining these are the updated categories
table(wildlife_impacts_cleaned$phase_of_flt)

# explore damage variable for inclusion
# N None M Minor, M Uncertain, S Substantial, D Destroyed
table(wildlife_impacts_cleaned$damage)

# replace values with interpretable descriptions
wildlife_impacts_cleaned_damage <- wildlife_impacts_cleaned %>%
  mutate(damage = ifelse(damage == "M", "Minor",
                   ifelse(damage == "M?", "Uncertain",
                   ifelse(damage == "N", "None", "Substantial"))))

# replacing NA values in both variables 
final_table <- wildlife_impacts_cleaned_damage %>%
  mutate(damage = replace_na(damage, "Uncertain"),
         phase_of_flt = replace_na(phase_of_flt, "Unknown"))

# create summary table with percent of damage by each phase of flight 
# using percentage as scales for each phase are disproportionate
perc_table <- final_table %>%
  group_by(phase_of_flt, damage) %>%
  summarise(num_accidents = n())
              
# create percentage of total phase of flight
final_perc_table <- perc_table %>%
  group_by(phase_of_flt) %>%
  mutate(percent = round((num_accidents / sum(num_accidents) * 100), 2)) %>%
  arrange(damage)

# reorder for plotting based on percentage column
final_perc_table$damage <- reorder(final_perc_table$damage, rowSums(final_perc_table[4]))

# create stacked bar chart
plot <- ggplot(arrange(final_perc_table, damage)) + 
  geom_bar(aes(x = phase_of_flt, y = percent, fill = damage), stat = "identity") + 
  scale_fill_manual(values = c("red", "orange", "#0c2fbb", "navy blue")) + 
  scale_y_continuous(breaks = seq(0,100,20)) + 
  scale_x_discrete(limits = c("En Route","Climb", "Descent", "Unknown","Take-Off",
                              "Approach","Taxi","Landing","Departure","Arrival","Local","Parked")) +
  labs(title = "Most Bird Strikes Result in No or Uncertain Damage",
       x = "",
       y = "Percent of Total Damage",
       subtitle = "En Route Bird Strikes Result in the Most Substantial Damage",
       caption = "Sourced from r4datascience/tidytuesday repo") + 
  theme_economist_white() +
  theme(legend.title = element_blank(),
        legend.text = element_text(size = 10),
        panel.grid.major.y = element_line(color = "black", size = .4))

ggsave("Tidy_Tuesday_Bird_Strikes.png")  
