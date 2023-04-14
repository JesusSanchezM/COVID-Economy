library(tidyverse)
library(readxl)

covid_2022 <- read.csv("covid_data_04_09_2022.csv")
covid_2020 <- read.csv("covid_data_20_01_2020.csv")
gdp_rate <- read.csv("GDP Growth Rate by Country.csv")
gdp_per_capita <- read.csv("GDP Per Capita by Country.csv")
infation <- read.csv("Inflation Rate by Country.csv")

covid_2020 <- covid_2020 %>% rename(Country.Name=Country.Region)
covid_2022 <- covid_2022 %>% rename(Country.Name=Country..Other)