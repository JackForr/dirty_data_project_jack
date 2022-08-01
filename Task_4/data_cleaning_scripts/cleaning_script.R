library(readxl)
library(tidyverse)
library(tidyr)
halloween_candy_2015 <- read_excel("raw_data/boing-boing-candy-2015.xlsx") %>% 
  janitor::clean_names()
halloween_candy_2016 <- read_excel("raw_data/boing-boing-candy-2016.xlsx") %>% 
  janitor::clean_names()
halloween_candy_2017 <- read_excel("raw_data/boing-boing-candy-2017.xlsx") %>% 
  janitor::clean_names()

#checking column types
names(halloween_candy_2016)
names(halloween_candy_2015)
names(halloween_candy_2017)

#removing unneccesary columns
halloween_candy_2016 <- halloween_candy_2016 %>% select(-c(107:123))

halloween_candy_2015 <- halloween_candy_2015 %>%
  select(-c(97:107))

halloween_candy_2017 <- halloween_candy_2017 %>% 
  select(-c(110:120))


#pivoting 2015&16

long_2015 <- halloween_candy_2015 %>% 
  pivot_longer(-c(timestamp, how_old_are_you, are_you_going_actually_going_trick_or_treating_yourself),
               names_to = "candy_type",
               values_to = "reaction")

long_2016 <- halloween_candy_2016 %>% 
  pivot_longer(-c(timestamp, how_old_are_you, are_you_going_actually_going_trick_or_treating_yourself, your_gender, which_country_do_you_live_in, which_state_province_county_do_you_live_in),
               names_to = "candy_type",
               values_to = "reaction")



#removing q prefix 
halloween_candy_2017 <- halloween_candy_2017 %>% 
  rename_all(~stringr::str_replace(., "^q[0-9]_", "")) %>% 
  rename_all(~stringr::str_replace(., "^q[0-9]{2}_", ""))
  

#pivoting 2017
long_2017 <- halloween_candy_2017 %>% 
  pivot_longer(-c(internal_id, going_out, gender, age, country, state_province_county_etc),
               names_to = "candy_type",
               values_to = "reaction")

#removing missing reactions
long_2015 <- long_2015 %>% 
  drop_na(reaction)

long_2016 <- long_2016 %>% 
  drop_na(reaction)

long_2017 <- long_2017 %>% 
  drop_na(reaction)

#renaming columns & matching types
long_2017 <- long_2017 %>% 
  rename("state_or_province" = state_province_county_etc,
         "year" = internal_id) 

long_2016 <- long_2016 %>% 
  rename("going_out" = are_you_going_actually_going_trick_or_treating_yourself,
         "gender" = your_gender,
         "age" = how_old_are_you,
         "country" = which_country_do_you_live_in,
         "state_or_province" = which_state_province_county_do_you_live_in,
         "year" = timestamp
         ) %>% 
  mutate(`year` = as.double(`year`))

long_2015 <- long_2015 %>% 
  rename("age" = how_old_are_you,
         "going_out" = are_you_going_actually_going_trick_or_treating_yourself,
         "year" = timestamp
         ) %>% 
  mutate(`year` = as.double(`year`))

#recode to define year of observation

long_2017$year <- 2017

long_2016$year <- 2016

long_2015$year <- 2015


#joining datasets
 
full_data <- full_join(long_2016, long_2015) %>% 
    full_join(., long_2017)

glimpse(full_data)

#cleaning variables

full_data <- full_data %>% 
  mutate(going_out = coalesce(going_out, "unknown"),
         age = coalesce(age, "unknown"),
         country = coalesce(country, "unknown"),
         state_or_province = coalesce(state_or_province, "unknown"),
         reaction = na_if(reaction, "3 or higher"),
         reaction = na_if(reaction, "1.0"),
         reaction = na_if(reaction, "2.0"),
         reaction = na_if(reaction, "Friday"),
         reaction = na_if(reaction, "Sunday"))

full_data %>% 
str_detect("[aA]merica")

unique(full_data$country)



write.csv(full_data, file = "full_data.csv")

  

  

