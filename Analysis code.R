library(tidyverse)
library(readxl)

covid_2022 <- read.csv("covid_data_04_09_2022.csv")
covid_2020 <- read.csv("covid_data_20_01_2020.csv")
gdp_rate <- read.csv("GDP Growth Rate by Country.csv")
gdp_per_capita <- read.csv("GDP Per Capita by Country.csv")
infation <- read.csv("Inflation Rate by Country.csv")

#First let's take a look of our datasets
#Go for COVID 2022 first

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

#time to convert to integers all numeric values
#watching the structured of the vector before change the current data
str(covid_2022 %>%
  mutate_at(vars(total_recovered, new_recovered, 
                 active_cases, deaths_1m_pop), as.integer)) 
#converting data
covid_2022 <- covid_2022 %>%
  mutate_at(vars(total_recovered, new_recovered, 
                 active_cases, deaths_1m_pop), as.integer)

names(covid_2022) <- paste0(names(covid_2022), "_2022")

#---------------------------------------------------------------------
#Go for COVID 2020 now

str(covid_2020) #death per 1 million is num, but it should be int
head(covid_2020) #We have several NA cases and the column names are not easy-look
covid_2020 <- janitor::clean_names(covid_2020) #cleaning our colnames
na.s <- apply(covid_2020, 2, function(x) sum(is.na(x)))# Use apply() with is.na() and sum() to count the number of missing values for each element
na.s
#columns "continent" and "who_region" are not useful for the analysis as the 2022'data has not these columns, and we can compare data by continent
covid_2020 <- covid_2020[, !names(covid_2020) %in% 
                           c("continent",  "who_region")] 
#covid_2020 <- covid_2020 %>% select(-continent, -who_region)
#another way above
covid_2020$deaths_1m_pop <- as.integer(covid_2020$deaths_1m_pop)

names(covid_2020) <- paste0(names(covid_2020), "_2020")

#------------------------------------------------------------------
#Now we want to merge those two COVID data sets

common_elements <- intersect(covid_2020$country_region_2020, 
                             covid_2022$country_other_2022)
length(common_elements)
length(covid_2020$country_region_2020)
length(covid_2022$country_other_2022)

#We have 2 key vector to join those two data sets, but we do not know how many elements in the vectors are equal
#We have to be conscious about the capital letters or different spelling format, 
#for instance, we could have mexico, México or Mexico, and R will display that they are different things










