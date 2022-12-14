### read in data 
library(readxl)
library(tidyverse)
library(tidyr)
halloween_candy_2015 <- read_excel("raw_data/boing-boing-candy-2015.xlsx") %>% 
  janitor::clean_names()
halloween_candy_2016 <- read_excel("raw_data/boing-boing-candy-2016.xlsx") %>% 
  janitor::clean_names()
halloween_candy_2017 <- read_excel("raw_data/boing-boing-candy-2017.xlsx") %>% 
  janitor::clean_names()

glimpse(halloween_candy_2015)
glimpse(halloween_candy_2016)
glimpse(halloween_candy_2017)

### remove unneccesary columns
halloween_candy_2016 <- halloween_candy_2016 %>% select(-c(107:123))

halloween_candy_2015 <- halloween_candy_2015 %>%
  select(-c(97:107))

halloween_candy_2017 <- halloween_candy_2017 %>% 
  select(-c(110:120))

###pivot
long_2015 <- halloween_candy_2015 %>% 
  pivot_longer(-c(timestamp, how_old_are_you, are_you_going_actually_going_trick_or_treating_yourself),
               names_to = "candy_type",
               values_to = "reaction")

long_2016 <- halloween_candy_2016 %>% 
  pivot_longer(-c(timestamp, how_old_are_you, are_you_going_actually_going_trick_or_treating_yourself, your_gender, which_country_do_you_live_in, which_state_province_county_do_you_live_in),
               names_to = "candy_type",
               values_to = "reaction")

halloween_candy_2017 <- halloween_candy_2017 %>% #removing prefix to col names
  rename_all(~stringr::str_replace(., "^q[0-9]_", "")) %>% 
  rename_all(~stringr::str_replace(., "^q[0-9]{2}_", ""))

long_2017 <- halloween_candy_2017 %>% 
  pivot_longer(-c(internal_id, going_out, gender, age, country, state_province_county_etc),
               names_to = "candy_type",
               values_to = "reaction")

### matching column names and types
long_2017 <- long_2017 %>% 
  rename("state_or_province" = state_province_county_etc,
         "year" = internal_id) %>% 
  mutate(`year` = as.numeric(`year`),
         `age` = as.integer(`age`)) #coercing NAs to non-integer entries

long_2016 <- long_2016 %>% 
  rename("going_out" = are_you_going_actually_going_trick_or_treating_yourself,
         "gender" = your_gender,
         "age" = how_old_are_you,
         "country" = which_country_do_you_live_in,
         "state_or_province" = which_state_province_county_do_you_live_in,
         "year" = timestamp) %>% 
  mutate(`year` = as.numeric(`year`),
         `age` = as.integer(`age`)) #coercing NAs to non-integer entries

long_2015 <- long_2015 %>% 
  rename("age" = how_old_are_you,
         "going_out" = are_you_going_actually_going_trick_or_treating_yourself,
         "year" = timestamp) %>% 
  mutate(`year` = as.numeric(`year`),
         `age` = as.integer(`age`)) #coercing NAs to non-integer entries

### defining year of observation
long_2017$year <- 2017
long_2016$year <- 2016
long_2015$year <- 2015

### join
full_data <- full_join(long_2016, long_2015) %>% 
  full_join(., long_2017)

### coalesce/drop missing values
full_data <- full_data %>% 
  mutate(going_out = coalesce(going_out, "unknown"),
         country = coalesce(country, "unknown"),
         state_or_province = coalesce(state_or_province, "unknown"),
         reaction = na_if(reaction, "3 or higher"),
         reaction = na_if(reaction, "1.0"),
         reaction = na_if(reaction, "2.0"),
         reaction = na_if(reaction, "Friday"),
         reaction = na_if(reaction, "Sunday")) %>% 
  drop_na(reaction) #dropping as other info seems pointless with no reaction to candy

### cleaning observations
usa_outliers = c("Alaska", "California", "EUA", "Merica", "Murica", "murrika",
                 "New Jersey", "New York", "North Carolina", "Pittsburgh", 
                 "The Yoo Ess of Aaayyyyyy", "Trumpistan", "U S", "u s a", "u.s.",
                 "U.s.", "U.S.", "u.s.a.", "U.S.A.", "UD", "us", "Us", "US", 
                 "US of A", "USSA", "'merica")

silly_values = c("A tropical island south of the equator", "A", "Atlantis",
                 "Canae", "cascadia ", "Cascadia", "Denial", "Earth", 
                 "Fear and Loathing", "god's country", "I don't know anymore",
                 "insanity lately", "there isn't one for old men", 
                 "soviet canuckistan", "Narnia", "Neverland",
                 "one of the best ones", "See above", "Somewhere", 
                 "subscribe to dm4uz3 on youtube",
                 "The republic of Cascadia", "this one")

US_deviations <- c("usa", "USA", "United States of America", "uSA", "united states",  "United States", "USA!", "USA (I think but it's an election year so who can really tell)", "Usa" , "America", "Units States", "United states", "USA USA USA", "the best one - usa" , "USA! USA! USA!", "united states of america", "USA!!!!!!", "USA! USA!", "United Sates", "Sub-Canadian North America... 'Merica", "UNited States", "United Stetes", "america", "Not the USA or Canada", "USA USA USA USA", "United  States of America" , "United State", "United staes", "USAUSAUSA", "The United States", "The United States of America", "USA? Hard to tell anymore..", "USa", "I pretend to be from Canada, but I am really from the United States.", "Ahem....Amerca", "United Stated", "United Statss", "united States", "N. America", "United Statea", "USA USA USA!!!!")   

canadian_deviations <- c("canada", "Can", "Canada`", "CANADA")

UK_deviations <- c("uk", "Uk", "United Kindom", "England", "United Kingdom", "endland", "Scotland", "england")

full_data <- full_data %>%
  mutate(country = if_else(country %in% silly_values, NA_character_, country),
         country = if_else(country %in% usa_outliers, "US", country),
         country = if_else(country %in% US_deviations, "US", country),
         country = if_else(country %in% canadian_deviations, "Canada", country),
         country = if_else(country %in% UK_deviations, "UK", country))

### writing clean data 
write.csv(full_data, file = "full_data.csv")
