# load packages
library(tidyverse)

# read in data
park_visits <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-09-17/national_parks.csv")
state_pop <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-09-17/state_pop.csv")
gas_price <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-09-17/gas_price.csv")

# Looking at the top 10 National Parks (by total visits) over time
# First, figure out the top 10 National Parks
top <- park_visits %>%
  filter(year == "Total" & unit_type == "National Park") %>%
  arrange(desc(visitors)) %>%
  select(unit_name, visitors)
print(top)

# Create a list for filtering
parks <- c("Great Smoky Mountains National Park","Grand Canyon National Park","Yosemite National Park",
           "Rocky Mountain National Park","Yellowstone National Park","Acadia National Park",
           "Olympic National Park","Grand Teton National Park","Shenandoah National Park","Zion National Park")

# Filter final dataset to only top 10 parks, and create indicator variable for parks of interest to highlight
# Remove years prior to 1940 as all park attendance too low for comparison
park_visits_filter <- park_visits %>%
  filter(unit_name %in% parks & year != "Total" & year >= 1940) %>%
  mutate(year = as.numeric(year),
         park_name = ifelse(unit_name == "Acadia National Park", "Acadia National Park",
                     ifelse(unit_name == "Grand Canyon National Park", "Grand Canyon National Park", 
                     ifelse(unit_name == "Great Smoky Mountains National Park", "Great Smoky Mountains National Park",
                     ifelse(unit_name == "Shenandoah National Park", "Shenandoah National Park", "Other* National Parks")))))

# Plot yearly visitors for each park
ggplot(data = park_visits_filter) + 
  geom_line(aes(x = year, y = visitors, group = unit_name, color = park_name, alpha = park_name)) +
  scale_x_continuous(breaks = seq(1940, 2020, 10)) + 
  scale_y_continuous(labels = scales::comma, breaks = seq(0, 12000000, 2000000)) + 
  labs(title = "Visits to Top Ten National Parks over Time",
       y = "Yearly Visitors",
       x = "",
       caption = "*Other includes Yosemite, Rocky Mountain, Yellowstone, Olympic, Zion and Grand Teton",
       subtitle = "Top Defined by Total Number of Visitors") + 
  theme_minimal() + 
  scale_color_manual(values = c("Red", "Blue", "Dark Green", "Grey", "Purple")) + 
  scale_alpha_manual(values = c(1,1,1,.4,1)) + 
  theme(legend.title = element_blank(),
        legend.position = "bottom") 

# Stories flushed out by graph, and indicated by different colors
# 1. All Parks saw a large decrease/no attendance during the time of WW2
# 2. Great Smoky Mountains National Park has been the most popular since 1940, by a considerable amount.
# 3. In the late 1970's attendance at Shenandoah dropped off, and has continued declining since.
# 4. Attendance at Acadia peaked in 1989 -- and then dropped dramatically, and has stayed low since. What happened?
# 5. Grand Canyon National Park didn't emerge as the 2nd busiest until around 1990, and has stayed in 2nd place since.
# 6. All Top 10 National Parks have seen an continued increase in visitors since 2010.
