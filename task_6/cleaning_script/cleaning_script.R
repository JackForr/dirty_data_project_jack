###reading in and investigating
library(tidyverse)
library(readr)
library(stringr)
dogs <- read.csv("raw_data/dog_survey.csv")
glimpse(dogs)
###cleaning
dogs <- dogs %>% 
  distinct()%>% 
  unite("full_name", first_name, last_name, sep = " ") %>% 
  select(-c("X", "X.1"))

dogs %>% 
  select(full_name) %>% # checking duplicate removal has worked
  group_by(full_name) %>% 
  filter(n()>1)

dogs <- dogs %>% 
  mutate(dog_size = na_if(dog_size, "N/A"),
         dog_size = na_if(dog_size, "No"),
         dog_size = na_if(dog_size, "NO"),
         dog_size = na_if(dog_size, "-"),
         dog_size = na_if(dog_size, "S,L,L"))

dogs <- dogs %>% 
  mutate(dog_size = if_else(dog_size == "Smallish", "S", dog_size),
         dog_size = if_else(dog_size == "Medium sized", "M", dog_size),
         dog_size = if_else(dog_size == "large", "L", dog_size))

gender_outliers <- c("", "Don’t know", "Unkown", "M,M,F", "—", "-", "Unknown")

dogs <- dogs %>% 
  mutate(dog_gender = if_else(dog_gender == "Male", "M", dog_gender),
         dog_gender = if_else(dog_gender == "male", "M", dog_gender),
         dog_gender = if_else(dog_gender == "MALE", "M", dog_gender),
         dog_gender = if_else(dog_gender == "femlae", "F", dog_gender),
         dog_gender = if_else(dog_gender == "Female", "F", dog_gender),
         dog_gender = if_else(dog_gender == "female", "M", dog_gender),
         dog_gender = if_else(dog_gender %in% gender_outliers, "Unknown", dog_gender),
         dog_gender = coalesce(dog_gender, "Unknown"))

###fix column types
dogs <- dogs %>% 
  mutate(amount_spent_on_dog_food = str_replace_all(amount_spent_on_dog_food, "£", ""),
         amount_spent_on_dog_food = as.numeric(amount_spent_on_dog_food))
         
dogs <- filter(dogs, amount_spent_on_dog_food >= 0)


write.csv(dogs, file = "dogs.csv")



