library(readr)
library(tidyverse)
library(tidyr)
decathlon_data <- read_rds("raw_data/decathlon.rds")
view(decathlon_data)
glimpse(decathlon_data)

#removing row names & cleaning names
clean_decathlon_data <- decathlon_data %>% 
  rownames_to_column(var = "name") %>% 
  janitor::clean_names() %>% 
  mutate_if(is.character, str_to_title) %>% 
  mutate(competition = as.character(competition)) #factor may cause issues later

#long formatting
long_decathlon_data <- clean_decathlon_data %>% 
  pivot_longer(-c(name, points, competition, rank),
names_to = "discipline",
values_to = "score")

glimpse(long_decathlon_data)

write.csv(long_decathlon_data, file = "long_decathlon_data.csv")
