library(tidyverse)
library(readxl)

covid_2022 <- read.csv("covid_data_04_09_2022.csv")
covid_2020 <- read.csv("covid_data_20_01_2020.csv")
gdp_rate <- read.csv("GDP Growth Rate by Country.csv")
gdp_per_capita <- read.csv("GDP Per Capita by Country.csv")
infation <- read.csv("Inflation Rate by Country.csv")

covid_2020 <- covid_2020 %>% rename(Country.Name=Country.Region)
covid_2022 <- covid_2022 %>% rename(Country.Name=Country..Other)

covid_2020[covid_2020=="USA"] <- "United States"
covid_2020[covid_2020=="S. Korea"] <- "South Korea"
covid_2020[covid_2020=="UK"] <- "United Kingdom"
covid_2022[covid_2022=="USA"] <- "United States"
covid_2022[covid_2022=="S. Korea"] <- "South Korea"
covid_2022[covid_2022=="UK"] <- "United Kingdom"

covid_gdp_rate_2020 <- merge(x = covid_2020, 
                             y = gdp_rate, by="Country.Name", 
                             all=T)
covid_gdp_rate_2022 <- merge(x = covid_2022, 
                             y = gdp_rate, by="Country.Name", 
                             all=T)
all_togheter_2022 <- merge(x=covid_gdp_rate_2022,
                      y=gdp_per_capita, by ="Country.Name",
                      all=T)
all_togheter_2022 <- merge(x=all_togheter_2022,
                      y=infation, by="Country.Name",
                      all=T)

# rm(all_togheter_2022)

colnames(all_togheter_2022)
head(all_togheter_2022)
filter(gdp_per_capita,Country.Name=="Afghanistan")
filter(gdp_rate, Country.Name=="Afghanistan")
filter(infation, Country.Name=="Afghanistan")

all_togheter_2022 <- rename(all_togheter_2022, 
                            "gdp_per_capita_2021"="X2021.y",  
       "gdp_per_capita_2020"="X2020.y", 
       "gdp_per_capita_2019"="X2019.y",        
       "gdp_per_capita_2018"="X2018.y", 
       "gdp_per_capita_2017"="X2017.y", 
       "gdp_rate_2021"="X2021.x",        
       "gdp_rate_2020"="X2020.x",
       "gdp_rate_2019"="X2019.x",
       "gdp_rate_2018"="X2018.x",        
       "gdp_rate_2017"="X2017.x", 
       "inflation_rate_2021"="X2021", 
       "inflation_rate_2020"="X2020",          
       "inflation_rate_2019"="X2019", 
       "inflation_rate_2018"="X2018", 
       "inflation_rate_2017"="X2017")

all_togheter_2022 <-all_togheter_2022[order(all_togheter_2022$Total.Cases, decreasing=TRUE), ]
all_togheter_2022 <- all_togheter_2022[-(1:8),]
rm(all_togheter_2022_1)

#Eliminar columnas con las palabras:
 # europe, north america, ... y total:

data_2020 %>% ggplot(aes(Country.Name, TotalCases, fill=gdp_per_capita_2020)) + 
  geom_bar(stat="identity", position="stack")


rm(all_togheter_2022)



all_togheter_2022 <-all_togheter_2022 %>%
  filter(!Country.Name %in% c("Europe", "Africa", "South America",
                             "Oceania", "Asia", "North America",
                             "Total:"))

all_togheter_2022 %>%
  filter(Country.Name %in% c("Europe", "Africa", "South America",
                             "Oceania", "Asia", "North America",
                             "Total:", "World"))


a <- (data_2020[order(data_2020$Population, decreasing=T),])[,c("Country.Name","inflation_rate_2019")]
a <- data_2022[order(data_2022$Total.Cases),]
a <- data_2020

a %>% ggplot(aes(Country.Name, Population, fill=Death_ratio)) +
               geom_bar(stat="identity", position="stack")+
  theme(axis.text = element_text(angle=45))

#Quitamos las columnas que no son iguales 


colnames(data_2020)
colnames(data_2022)


data_2020 <- select(data_2020, -Continent, WHO.Region,-NewRecovered, -NewDeaths, -TotalRecovered,
                    NewCases, -ActiveCases, -ActiveCases)
data_2022 <- select(data_2022, -New.Recovered, -New.Deaths, -Total.Recovered,
                    New.Cases, -Active.Cases, -Active.Cases)
data_2022 <- data_2022[,-2]

data_2020 <-data_2020 %>% rename(Total.Cases=TotalCases,
                     Total.Deaths=TotalDeaths,
                     Serious..Critical=Serious.Critical,
                     Tot.Cases..1M.pop=Tot.Cases.1M.pop,
                     Deaths..1M.pop=Deaths.1M.pop,
                     Total.Tests=TotalTests,
                     Tests...1M.pop=Tests.1M.pop,
                     Deaths..1M.pop=Deaths.1M.pop
                     )

data_2020 <-data_2020 %>% rename(Total.Cases=TotalCases,
                                 New.Cases=NewCases,
                                 Total.Deaths=TotalDeaths,
                                 New.Deaths=NewDeaths,
                                 Total.Recovered=TotalRecovered,
                                 New.Recovered=NewRecovered,
                                 Active.Cases=ActiveCases,
                                 Serious..Critical=Serious.Critical,
                                 Tot.Cases..1M.pop=Tot.Cases.1M.pop,
                                 Deaths..1M.pop=Deaths.1M.pop,
                                 Total.Tests=TotalTests,
                                 Tests...1M.pop=Tests.1M.pop,
                                 Deaths..1M.pop=Deaths.1M.pop
)

all_data <- bind_rows(data_2020, data_2022)
library()

data_2020 %>% ggplot(aes(Country.Name, Total.Cases, fill=factor(Death_ratio))) +
  geom_bar(stat="identity", position="stack")+
  theme(axis.text = element_text(angle=45))+
  scale_fill_manual(values = c("blue", "orange", "red",
                               "green", "white", "black",
                               "yellow", "gray", "pink",
                               "gold", "blue", "orange", "red",
                               "green", "white", "black",
                               "yellow", "gray", "pink",
                               "gold"))

?scale_color_gradient()

a <- c(1,1,2)
b <- c("USA", "Chile", "USA")
p <- c(0.3, 0.4, 0.7)
c <- data.frame(b, a, p)

c %>% 
  ggplot(aes(x=b, y=a, fill=p)) +
  geom_bar(stat="identity", position="stack") 

data_2022 %>% ggplot(aes(x=reorder(Country.Name, +Total.Cases), Total.Cases, fill=Death_ratio)) +
  geom_bar(stat="identity", position="stack")+
  labs(x="Country Name", y="Total Cases",
       title="Total cases and deaths caused by covid",
       subtitle="2022 data") + coord_flip()

data_2020 %>% ggplot(aes(Country.Name, Total.Cases, fill=Death_ratio)) +
  geom_bar(stat="identity", position="stack")+
  theme(axis.text = element_text(angle=45))+
  labs(x="Country Name", y="Total cases",
       title="Total cases and deaths caused by covid",
       subtitle="2020 data")

a <- all_data[order(all_data$Death_ratio, decreasing=T ), ]

#world map

mapdata <- map_data("world")
mapdata <- mapdata %>% rename("Country.Name" = "region")
mapdata[mapdata=="USA"] <- "United States"

map_data <- right_join(mapdata, data_2022, by="Country.Name")
map_data <- map_data %>% filter(!is.na(map_data$gdp_rate_2021))

ggplot(map_data, aes(long, lat, group=group))+
  geom_polygon(aes(fill=Death_ratio), color="black")

#total cases rate 2022

all_togheter_2022 <- mutate(all_togheter_2022, 
       total.cases.rate=Total.Cases/Population)

all_togheter_2022 %>% ggplot() + 
  geom_smooth(aes(gdp_per_capita_2021, total.cases.rate))+
  geom_point(aes(gdp_per_capita_2021, total.cases.rate))

all_togheter_2022 %>% lm(formula=total.cases.rate ~gdp_per_capita_2021) %>% 
  summary()

all_togheter_2022 %>% lm(formula=gdp_per_capita_2021~total.cases.rate) %>% 
  summary()

#total cases rate 2020

all_togheter_2020 <- mutate(all_togheter_2020, 
                            total.cases.rate=TotalCases/Population)

all_togheter_2020 %>% ggplot() + 
  geom_smooth(aes(gdp_per_capita_2020, total.cases.rate))+
  geom_point(aes(gdp_per_capita_2020, total.cases.rate))

all_togheter_2020 %>% lm(formula=total.cases.rate ~gdp_per_capita_2020) %>% 
  summary()

all_togheter_2020 %>% lm(formula=gdp_per_capita_2020~total.cases.rate) %>% 
  summary()

colnames(all_data)


a <- data.frame(Country.Name =all_data$Country, 
                i_2017=all_data$gdp_per_capita_2017,
                i_2018=all_data$inflation_rate_2018,
                i_2019=all_data$inflation_rate_2019,
                i_2020=all_data$inflation_rate_2020,
                i_2021=all_data$inflation_rate_2021
                )

df_transpose <- data.frame(t(a[-1]))
colnames(df_transpose) <- a[, 1]

df_transpose <- data.frame(t(all_data[-1]))
colnames(df_transpose) <- all_data[, 1]

a <- df_transpose[21:25,]

ggplot() + geom_bar(aes(a$`United States`))


data_2020 <- mutate(data_2020, 
                    Death_ratio=Total.Deaths/Total.Cases)
#we created in a first step our new Death_ratio column
data_2020 %>% ggplot(aes(x=reorder(Country.Name, +Total.Cases), Total.Cases, fill=Death_ratio)) +
  geom_bar(stat="identity", position="stack")+
  labs(x="Country Name", y="Total Cases",
       title="Total cases and deaths caused by covid",
       subtitle="2020 data") + coord_flip()

rm(data_2020$)

getwd()

setwd(choose.dir())

tinytex::install_tinytex()



install.packages("gapminder")
library(gapminder)
p <- ggplot(
  gapminder, 
  aes(x = gdpPercap, y=lifeExp, size = pop, colour = country)
) +
  geom_point(show.legend = FALSE, alpha = 0.7) +
  scale_color_viridis_d() +
  scale_size(range = c(2, 12)) +
  scale_x_log10() +
  labs(x = "GDP per capita", y = "Life expectancy")
p
p + transition_time(year) +
  labs(title = "Year: {frame_time}")





