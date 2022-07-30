library(readxl)
library(tidyverse)
library(tidyr)
halloween_candy_2015 <- read_excel("raw_data/boing-boing-candy-2015.xlsx") %>% 
  janitor::clean_names()
halloween_candy_2016 <- read_excel("raw_data/boing-boing-candy-2016.xlsx") %>% 
  janitor::clean_names()
halloween_candy_2017 <- read_excel("raw_data/boing-boing-candy-2017.xlsx") %>% 
  janitor::clean_names()
view(halloween_candy_2015)
view(halloween_candy_2016)
view(halloween_candy_2017)

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



#removing q aprefix 
halloween_candy_2017 <- halloween_candy_2017 %>% 
  rename_all(~stringr::str_replace(., "^q[0-9]_", "")) %>% 
  rename_all(~stringr::str_replace(., "^q[0-9]{2}_", ""))
  

#pivoting 2017
long_2017 <- halloween_candy_2017 %>% 
  pivot_longer(-c(internal_id, going_out, gender, age, country, state_province_county_etc),
               names_to = "candy_type",
               values_to = "reaction")


#joining datasets
  full_join(long_2015, long_2016, by = "candy_type") %>% 
    full_join(., long_2017, by = "candy_type")
  
#vector limit exhausted????
  

