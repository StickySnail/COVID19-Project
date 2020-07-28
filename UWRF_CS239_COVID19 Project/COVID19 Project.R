# CS239 Final Project COVID19
# JEREMY SEO

# IN MY PROJECT, I AIMED TO END UP DISPLAYING MEANINGFUL VISUAL RESOURCES.
# I PRIMARILY FOCUSED ON EXTRACTING DATA BASED ON COUNTRIES WHICH ARE IMPORTATN FOR ME.
# ONE OF ATTRIBUTES OF MY PROJECT IS THAT USER WON'T NEED TO CHANGE ANY CODES BESIDES
# THE FRIST CODE WHICH WILL DETEREMINE INPUT DATA ONCE THE BASIC DATA UPDATED.
# THIS PROGRAM ANTICIPATES THE DATA COMEING FROM SAME RESOURCE PROVIDER.
# BY GOING TRHOUGH EVERY LINE OF THIS PROJECT, YOU WILL BE INTRODUCED DIFFERENT
# GRAPHS.

# LIBRARIES I UTILIZED
library(tidyverse)
library(readxl)
library(ggplot2)
library(dplyr)

# to convert from scientific number to numeric number.
options(scipen = 999)

# to read xlsx data and assign into covid_19_main
covid_19_main <- read_xlsx("COVID-19-geographic-disbtribution-worldwide-2020-05-02.xlsx")
glimpse(covid_19_main)

#############China#############
# death graph of China
covid_19_china_death <- covid_19_main %>%
  filter(countriesAndTerritories == "China") %>%
  arrange(dateRep)%>%
  ggplot(aes(dateRep,deaths)) +
  geom_line() +
  ggtitle("DEATH GRAPH of CHINA") +
  xlab("Months") +
  ylab("Deaths")
covid_19_china_death

# add deaths culmulative (data.frame)
covid_19_china_death_cum <- covid_19_main %>%
  filter(countriesAndTerritories == "China") %>% 
  arrange(dateRep) %>%
  mutate(total_deaths = cumsum(deaths))
covid_19_china_death_cum %>% glimpse()


# deaths culmulative %>% Graph
covid_19_china_death_cum %>%
  ggplot(aes(dateRep,total_deaths)) +
  geom_line() +
  ggtitle("CULMULATIVE DEATHS GRAPH of CHINA") +
  xlab("Months")+
  ylab("Total_Deaths")

# cases of China
covid_19_china_cases <- covid_19_main %>%
  filter(countriesAndTerritories == "China") %>%
  arrange(dateRep)
  
# cases grpah of China
covid_19_china_cases_Graph <- covid_19_china_cases %>%
  ggplot(aes(dateRep,cases)) +
  geom_line() +
  ggtitle("CASES GRAPH of CHINA") +
  xlab("Months")+
  ylab("Cases")
covid_19_china_cases_Graph

# add cases culmulative (data.frame)
covid_19_china_cases_cum <- covid_19_main %>%
  mutate(dateRep = as.Date(dateRep)) %>%
  filter(countriesAndTerritories == "China") %>% 
  arrange(dateRep) %>%
  mutate(total_cases = cumsum(cases))
covid_19_china_death_cum %>% glimpse()


# cases culmulative %>% Graph
covid_19_china_cases_cum %>%
  ggplot(aes(dateRep,total_cases)) +
  geom_line() +
  ggtitle("CULMULATIVE CASES graph of CHINA") +
  xlab("Months")+
  ylab("Total_Cases")


######### SOUTHKOREA ##########
# death graph of South Korea 
covid_19_korea_death <- covid_19_main %>%
  filter(countriesAndTerritories == "South_Korea") %>%
  arrange(dateRep)%>%
  ggplot(aes(dateRep,deaths)) +
  geom_line() +
  ggtitle("DEATH GRAPH of KOREA") +
  xlab("Months")
covid_19_korea_death

# add deaths culmulative (data.frame)
covid_19_korea_death_cum <- covid_19_main %>%
  filter(countriesAndTerritories == "South_Korea") %>% 
  arrange(dateRep) %>%
  mutate(total_deaths = cumsum(deaths))
covid_19_korea_death_cum %>% glimpse()


# deaths culmulative %>% Graph
covid_19_korea_death_cum %>%
  ggplot(aes(dateRep,total_deaths)) +
  geom_line() +
  ggtitle("CULMULATIVE DEATH GRAPH of KOREA") +
  xlab("Months")

# cases grpah of KOREA
covid_19_korea_cases <- covid_19_main %>%
  filter(countriesAndTerritories == "South_Korea") %>%
  arrange(dateRep) %>%
  ggplot(aes(dateRep,cases)) +
  geom_line() +
  ggtitle("CASES GRAPH of KOREA") +
  xlab("Months")
covid_19_korea_cases

# add cases culmulative (data.frame)
covid_19_korea_cases_cum <- covid_19_main %>%
  mutate(dateRep = as.Date(dateRep)) %>%
  filter(countriesAndTerritories == "South_Korea") %>% 
  arrange(dateRep) %>%
  mutate(total_cases = cumsum(cases))
covid_19_korea_cases_cum %>% glimpse()


# cases culmulative %>% Graph
covid_19_korea_cases_cum %>%
  ggplot(aes(dateRep,total_cases)) +
  geom_line() +
  ggtitle("CULMULATIVE CASES GRAPH of KOREA") +
  xlab("Months")

######### America ###########
# death graph of America 
covid_19_usa_death <- covid_19_main %>%
  filter(countryterritoryCode == "USA") %>%
  arrange(dateRep)%>%
  ggplot(aes(dateRep,deaths)) +
  geom_line() +
  ggtitle("DEATH GRAPH of USA") +
  xlab("Months")
covid_19_usa_death

# add deaths culmulative (data.frame)
covid_19_usa_death_cum <- covid_19_main %>%
  filter(countryterritoryCode == "USA") %>% 
  arrange(dateRep) %>%
  mutate(total_deaths = cumsum(deaths))
covid_19_usa_death_cum %>% glimpse()


# deaths culmulative %>% Graph
covid_19_usa_death_cum %>%
  ggplot(aes(dateRep,total_deaths)) +
  geom_line() +
  ggtitle("CULMULATIVE DEATH GRAPH of USA") +
  xlab("Months")

# cases grpah of America
covid_19_usa_cases <- covid_19_main %>%
  filter(countryterritoryCode == "USA") %>%
  arrange(dateRep) %>%
  ggplot(aes(dateRep,cases)) +
  geom_line() +
  ggtitle("CASES GRAPH of USA") +
  xlab("Months")
covid_19_usa_cases

# add cases culmulative (data.frame)
covid_19_usa_cases_cum <- covid_19_main %>%
  mutate(dateRep = as.Date(dateRep)) %>%
  filter(countryterritoryCode == "USA") %>% 
  arrange(dateRep) %>%
  mutate(total_cases = cumsum(cases))
covid_19_usa_cases_cum %>% glimpse()


# cases culmulative %>% Graph
covid_19_usa_cases_cum %>%
  ggplot(aes(dateRep,total_cases)) +
  geom_line() +
  ggtitle("CULMULATIVE CASES GRAPH of USA") +
  xlab("Months")



######## ADVANCED WORKING ##########
# Set events WHO declared
who_events <- tribble(
  ~ date, ~ event,
  "2020-01-30", "Global health\nemergency declared",
  "2020-03-11", "Pandemic\ndeclared"
) %>%
  mutate(date = as.Date(date))


# Annotate Cases Graph by WHO announcemtn _ CHINA
covid_19_china_cases_cum %>%
  mutate(dateRep = as.Date(dateRep)) %>%
  ggplot(aes(dateRep,total_cases)) +
  geom_line() +
  ggtitle("CULMULATIVE CASES GRAPH of CHINA") +
  theme(plot.title = element_text(size=15, face="bold")) +
  xlab("Months")+
  ylab("Total_Cases") +
  geom_vline(aes(xintercept = as.numeric(date)), 
             data= who_events, linetype = "dashed") +
  geom_text(aes(x = date, label =event), data = who_events, color ='red',
            size = 3.5, y =40000)

# AMERICA
covid_19_usa_cases_cum %>%
  mutate(dateRep = as.Date(dateRep)) %>%
  ggplot(aes(dateRep,total_cases)) +
  geom_line() +
  ggtitle("CULMULATIVE CASES GRAPH of AMERICA") +
  theme(plot.title = element_text(size=15, face="bold")) +
  xlab("Months")+
  ylab("Total_Cases") +
  geom_vline(aes(xintercept = as.numeric(date)), 
             data= who_events, linetype = "dashed") +
  geom_text(aes(x = date, label =event), data = who_events, color ='blue',
            size = 3.5, y =600000)


# EVENTS KOREAN GOVT DECLARED
kor_events <- tribble(
  ~ date, ~ event,
  "2020-02-19", "DaeGu\nMass Infecton ",
  "2020-03-23", "Social Distancing\nStarts"
) %>%
  mutate(date = as.Date(date))

# ANNOTAE WHAT KOR_GOVT DECLARED
covid_19_korea_cases_cum %>%
  mutate(dateRep = as.Date(dateRep)) %>%
  ggplot(aes(dateRep, total_cases)) +
  geom_line() +
  expand_limits(y = 13000) +
  ggtitle("CULMULATIVE CASES GRAPH of KOREA") +
  theme(plot.title = element_text(size=15, face="bold")) +
  xlab("Months")+
  ylab("Total_Cases") +
  geom_vline(aes(xintercept = as.numeric(date)), 
             data= who_events, linetype = "dashed") +
  geom_text(aes(x = date, label =event), data = kor_events, color ='purple',
            size = 3.5, y =12000)


# combine data frames
three_countries_cum_cases <-
  rbind(covid_19_korea_cases_cum, covid_19_usa_cases_cum, covid_19_china_cases_cum)
three_countries_cum_cases

# CHINA KOREA USA
plt_cum_confirmed_cases_china_vs_usa_vs_korea <-
  ggplot(three_countries_cum_cases, aes(dateRep, total_cases, color = countryterritoryCode)) +
  geom_line() +
  ggtitle("CULMULATIVE CASES GRAPH of THREE COUNTRIES") +
  theme(plot.title = element_text(size=15, face="bold")) +
  xlab("Months")+
  ylab("Total_Cases") +
  geom_vline(aes(xintercept = as.numeric(date)), 
             data= who_events, linetype = "dashed") +
  geom_text(aes(x = date, label =event), data = who_events, color ='black',
            size = 3.5, y =600000)

# to see final output
plt_cum_confirmed_cases_china_vs_usa_vs_korea