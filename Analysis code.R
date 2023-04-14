library(tidyverse)
library(readxl)

covid_2022 <- read.csv("covid_data_04_09_2022.csv")
covid_2020 <- read.csv("covid_data_20_01_2020.csv")
gdp_rate <- read.csv("GDP Growth Rate by Country.csv")
gdp_per_capita <- read.csv("GDP Per Capita by Country.csv")
infation <- read.csv("Inflation Rate by Country.csv")

#First let's take a look of our datasets
#Go for covid_2022 first

str(covid_2022) #our vectors "total_recovered", "new recovered" and "active cases" seems to be treated as characters when they are actually numbers
head(covid_2022) #We have several NA cases and the column names are not easy-look
covid_2022 <- janitor::clean_names(covid_2022) #cleaning our colnames
na.s <- apply(covid_2022, 2, function(x) sum(is.na(x)))# Use apply() with is.na() and sum() to count the number of missing values for each element
na.s
covid_2022 %>% filter(is.na(x)) #As we observe the NA values from the column x are continents and total, so we do not want them, as well as the x column which give us the row number
covid_2022 <- covid_2022 %>% filter(!is.na(x)) %>%  select(-x)
# now inspecting data we see that the the vector that belong to numeric vector but are characthers have commas "," or signs "+" that do not allow R treat them like that
length(grep("\\,", covid_2022$total_recovered))
length(grep("\\,", covid_2022$active_cases))
covid_2022$total_recovered <- gsub(",", "", covid_2022$total_recovered) #getting rid of the commas ","
covid_2022$active_cases <- gsub(",", "", covid_2022$active_cases)
length(grep("\\+", covid_2022$new_recovered)) #now the "+" plus signs
covid_2022$new_recovered <- gsub("\\+", "", covid_2022$new_recovered)
covid_2022$new_recovered <- gsub(",", "", covid_2022$new_recovered)

#confirming we have gotten rid of those signs
length(grep("\\,", covid_2022$total_recovered))
length(grep("\\,", covid_2022$active_cases))
length(grep("\\+", covid_2022$new_recovered)) 
length(grep("\\,", covid_2022$new_recovered)) 

covid_2022 <- covid_2022 %>%
  mutate_at(vars(total_recovered, active_cases), as.numeric)

#Go for covid 2020 now

str(covid_2020)
head(covid_2020) #We have several NA cases and the column names are not easy-look
covid_2020 <- janitor::clean_names(covid_2020) #cleaning our colnames
na.s <- apply(covid_2020, 2, function(x) sum(is.na(x)))# Use apply() with is.na() and sum() to count the number of missing values for each element
na.s












