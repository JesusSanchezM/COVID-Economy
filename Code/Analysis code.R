library(tidyverse) 
library(readxl) #read excel files
library(gridExtra) #arranges of graphs
library(plotly) #interactive graphics

file.choose() # Using this command to copy the directory path
covid_2022 <- read.csv("C:\\Users\\Jesus Sanchez\\Desktop\\ALEXIS\\Portafolio\\1er proyecto_covid_global economy\\Worldmeter_covid_data\\covid_economy\\Data\\covid_data_04_09_2022.csv")
covid_2020 <- read.csv("C:\\Users\\Jesus Sanchez\\Desktop\\ALEXIS\\Portafolio\\1er proyecto_covid_global economy\\Worldmeter_covid_data\\covid_economy\\Data\\covid_data_20_01_2020.csv")
gdp_rate <- read.csv("C:\\Users\\Jesus Sanchez\\Desktop\\ALEXIS\\Portafolio\\1er proyecto_covid_global economy\\Worldmeter_covid_data\\covid_economy\\Data\\GDP Growth Rate by Country.csv")
gdp_per_capita <- read.csv("C:\\Users\\Jesus Sanchez\\Desktop\\ALEXIS\\Portafolio\\1er proyecto_covid_global economy\\Worldmeter_covid_data\\covid_economy\\Data\\GDP Per Capita by Country.csv")
infation <- read.csv("C:\\Users\\Jesus Sanchez\\Desktop\\ALEXIS\\Portafolio\\1er proyecto_covid_global economy\\Worldmeter_covid_data\\covid_economy\\Data\\Inflation Rate by Country.csv")

#First let's take a look of our data sets
#Go for COVID 2022 first

str(covid_2022) #our vectors "total_recovered", "new recovered" and "active cases" seems to be treated as characters when they are actually numbers
head(covid_2022) 
na.s <- apply(covid_2022, 2, function(x) sum(is.na(x)))# Use apply() with is.na() and sum() to count the number of missing values for each element
na.s #We have several NA cases and the column names are not easy-look
covid_2022 <- janitor::clean_names(covid_2022) #cleaning our col names

covid_2022 %>% filter(is.na(X.)) #As we observe the NA values from the column x are continents and total, so we do not want them, as well as the x column which give us the row number
covid_2022 <- covid_2022 %>% filter(!is.na(X.)) %>%  select(-X.)
head(covid_2022)
# now inspecting data we see that the the vector that belong to numeric vector but are characthers have commas "," or signs "+" that do not allow R treat them like that
length(grep("\\,", covid_2022$Total.Recovered))
length(grep("\\,", covid_2022$Active.Cases))
covid_2022$Total.Recovered <- gsub(",", "", covid_2022$Total.Recovered) #getting rid of the commas ","
covid_2022$Active.Cases <- gsub(",", "", covid_2022$Active.Cases)
length(grep("\\+", covid_2022$New.Recovered)) #now the "+" plus signs
covid_2022$New.Recovered <- gsub("\\+", "", covid_2022$New.Recovered)
covid_2022$New.Recovered <- gsub(",", "", covid_2022$New.Recovered)

#confirming we have gotten rid of those signs
length(grep("\\,", covid_2022$Total.Recovered))
length(grep("\\,", covid_2022$Active.Cases))
length(grep("\\+", covid_2022$Active.Cases)) 
length(grep("\\,", covid_2022$New.Recovered)) 

#time to convert to integers all numeric values
#watching the structured of the vector before change the current data
str(covid_2022 %>%
  mutate_at(vars(Total.Recovered, New.Recovered, 
                 Active.Cases, Deaths..1M.pop), as.integer))#Warning message stands fro NAs
#converting data
covid_2022 <- covid_2022 %>%
  mutate_at(vars(Total.Recovered, New.Recovered, 
                 Active.Cases, Deaths..1M.pop), as.integer)

names(covid_2022) <- paste0(names(covid_2022), "_2022")
head(covid_2022)
str(covid_2022) # This data is cleaned

#---------------------------------------------------------------------
#Go for COVID 2020 now

str(covid_2020) #death per 1 million is num, but it should be int
head(covid_2020) #We have several NA cases and the column names are not easy-look
covid_2020 <- janitor::clean_names(covid_2020) #cleaning our colnames
na.s <- apply(covid_2020, 2, function(x) sum(is.na(x)))# Use apply() with is.na() and sum() to count the number of missing values for each element
na.s
#columns "continent" and "who_region" are not useful for the analysis since the 2022's data has not these columns, and we can compare data by continent
covid_2020 <- covid_2020[, !names(covid_2020) %in% 
                           c("continent",  "who_region")] 
#covid_2020 <- covid_2020 %>% select(-continent, -who_region)
#another way above
covid_2020$deaths_1m_pop <- as.integer(covid_2020$deaths_1m_pop)
names(covid_2020) <- paste0(names(covid_2020), "_2020")
head(covid_2020)
str(covid_2020) #Ready, keep move on

#------------------------------------------------------------------
#Now we want to merge those two COVID data sets

common_elements <- intersect(covid_2020$country_region_2020, 
                             covid_2022$Country..Other_2022)
length(common_elements)
length(covid_2020$country_region_2020)
length(covid_2022$Country..Other_2022)

#We have 2 key vector to join those two data sets, but we do not know how many elements in the vectors are equal
#We have to be conscious about the capital letters or different spelling format, 
#for instance, we could have mexico, México or Mexico, and R will display that they are different countries 

setdiff(covid_2020$country_region_2020, covid_2022$Country..Other_2022) #what are the different countries?
#As we observe words with accent marks are misspelling and Brunei has an extra space

covid_2022 %>% select(Country..Other_2022) %>% 
  filter(substr(Country..Other_2022,1,1)== "B")
covid_2022 %>% select(Country..Other_2022) %>% 
  filter(substr(Country..Other_2022,1,1)== "R")
covid_2022 %>% select(Country..Other_2022) %>% 
  filter(substr(Country..Other_2022,1,1)== "C")
#Countries from covid_2020 misspelling are in covid_2022

covid_2020[covid_2020=="RÃ©union"] <- "Réunion"
covid_2020[covid_2020=="Brunei "] <- "Brunei"
covid_2020[covid_2020=="CuraÃ§ao"] <- "Curaçao"

setdiff(covid_2020$country_region_2020, covid_2022$Country..Other_2022)
#We have correct all values

covid_2020 <- covid_2020 %>% rename(Country=country_region_2020)
covid_2022 <- covid_2022 %>% rename(Country=Country..Other_2022)

#time to merge those two data sets
covid_2020_2022 <- merge(x = covid_2020, 
                             y = covid_2022, by="Country", 
                             all=F)
head(covid_2020_2022)

cv <- covid_2020_2022 %>% arrange(desc(total_cases_2020)) %>% head(15)

par(mfrow = c(1, 1))

p1 <- ggplot(cv) + 
  geom_bar(aes(x=Country, y=cv$Total.Cases_2022/1000000, fill="2022"), stat="identity", color="black")+
  geom_bar(aes(x=Country, y=total_cases_2020/1000000, fill="2020"), stat="identity", color="black")+
  scale_fill_manual(name="Cases", values=c("2022"="orange", "2020"="blue"), 
                    labels=c("2022"="2022", "2020"="2020")) + theme_classic() +
  labs(title="COVID's cases per millon 2020-2022", 
       x="Country", y="Cases/1,000,000", 
       caption="Source: Data collected from Worlmeter \nOwn elaboration")+
  theme(legend.position = "right", 
        plot.title = element_text(hjust=0.5),
        plot.caption = element_text(hjust=1),
        axis.text.x = element_text(angle=45))
p1


p3 <- ggplot(cv) + 
  geom_bar(aes(x=Country, y=Total.Cases_2022/1000000, fill="2022"), stat="identity", color="black")+
  geom_bar(aes(x=Country, y=total_deaths_2020/1000000, fill="2020"), stat="identity", color="black")+
  scale_fill_manual(name="Deaths", values=c("2022"="orange", "2020"="blue"), 
                    labels=c("2022"="2022", "2020"="2020")) + theme_classic() +
  labs(title="COVID's cumulative deaths per millon 2020-2022", 
       x="Country", y="Cases/1,000,000", 
       caption="Source: Data collected from Worlmeter \nOwn elaboration")+
  theme(legend.position = "right", 
        plot.title = element_text(hjust=0.5),
        plot.caption = element_text(hjust=1),
        axis.text.x = element_text(angle=45))
p3

p1 <- ggplot(cv) + 
  geom_bar(aes(x=Country, y=cv$Total.Deaths_2022/1000000, fill=""), stat="identity", 
           col="black",show.legend=F)+
  scale_fill_manual(values=c("salmon"))+
  labs(title="COVID's cumulative deaths per millon 2022", 
       x="", y="Cases/1,000,000")+  theme_classic()+
  theme(legend.position = "none", 
        plot.title = element_text(hjust=0.5),
        axis.text.x = element_text(angle=45)) 

p2 <- ggplot(cv) + 
  geom_bar(aes(x=Country, y=total_deaths_2020/1000000, fill="turquoise"), stat="identity", 
           col="black", show.legend = F)+
  scale_fill_manual(values=c("turquoise"))+
  labs(title="COVID's cumulative deaths per millon 2020", 
       x="Country", y="Cases/1,000,000")  +
  theme_classic()+
  theme(legend.position = "none", 
        plot.title = element_text(hjust=0.5),
        axis.text.x = element_text(angle=45)) 

p4 <- ggplot(cv) + 
  geom_bar(aes(x=Country, y=Total.Deaths_2022/1000000, fill="2022"), stat="identity", color="black")+
  geom_bar(aes(x=Country, y=total_deaths_2020/1000000, fill="2020"), stat="identity", color="black")+
  scale_fill_manual(name="Deaths", values=c("2022"="salmon", "2020"="turquoise"), 
                    labels=c("2022"="2022", "2020"="2020")) + theme_classic() +
  labs(title="COVID's cumulative deaths per millon 2020-2022", 
       x="", y="Cases/1,000,000") +
  theme(legend.position = "right", 
        plot.title = element_text(hjust=0.5),
        axis.text.x = element_text(angle=45))

gridExtra::grid.arrange(p4, p1, p2, layout_matrix = rbind(c(1, 1), c(2, 3)))
# ggplotly(p4)

#-----------------------------------------------------------------------

library(tidyverse)
library(corrplot)

# Create a correlation matrix
matriz_cor <- cor(covid_2020[, !(names(covid_2020) %in% "Country")], use = "pairwise.complete.obs")

# Customize axis labels
col_names <- names(covid_2020)[!(names(covid_2020) %in% "Country")]
num_labels <- seq_along(col_names)
corrplot(matriz_cor, method = "circle", tl.col = "black", tl.srt = 45, tl.cex = 0.8, tl.pos = "lt", col.names = num_labels)



features <- covid_2020_2022 %>% 
  select(population_2020, total_cases_2020, total_deaths_2020, 
         )

rcorr(as.matrix(na.omit(features)))

library("Hmisc")

summary(lm(covid_2020_2022$total_cases_2020 ~ 
           covid_2020_2022$total_deaths_2020))

ggplot() + 
  geom_point(covid_2020_2022, mapping=aes(x = total_cases_2020,
                                  y = total_deaths_2020)) + theme_classic()

ggplot() + 
  geom_boxplot(mapping = aes(x))

filter(covid_2020_2022$total_cases_2020 <=
         quantile(covid_2020_2022$total_cases_2020, .75))

x <- covid_2020_2022 %>% select(total_cases_2020) %>%  
  filter(total_cases_2020 <= quantile(total_cases_2020, .50))

as.vector(x)

boxplot(covid_2020_2022$total_cases_2020)
boxplot(x)


