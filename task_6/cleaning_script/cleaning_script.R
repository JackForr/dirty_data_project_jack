###reading in and investigating
library(tidyverse)
library(readr)
library(stringr)
dogs <- read.csv("raw_data/dog_survey.csv")
glimpse(dogs)
view(dogs)
###cleaning
dogs <- dogs %>% 
  distinct()%>% 
  unite("full_name", first_name, last_name, sep = " ") %>% 
  select(-c("X", "X.1"))

dogs %>% 
  select(full_name) %>% # checking duplicate removal has worked
  group_by(full_name) %>% 
  filter(n()>1)

###amount spent to numeric - make sure all vlaues positive
dogs <- dogs$amount_spent_on_dog_food = as.numeric(gsub("£", "", dogs$amount_spent_on_dog_food))

dogs <- dogs %>% 
  mutate(amount_spent_on_dog_food = as.numeric(amount_spent_on_dog_food))


