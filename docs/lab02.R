library(tidyverse)
library(knitr)
library(readxl)
library(zoo)


pop = read_excel('data/PopulationEstimates.xls', skip = 2) %>%
select(pop2019 = POP_ESTIMATE_2019, fips = FIPStxt)

covid_url = read_csv('https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv')

covid = read_csv(covid_url)


newData = inner_join(pop, covid, by = 'fips') %>%
  filter(state == 'California') %>%
  group_by(county) %>%
  mutate(new_cases = cases - lag(cases)) %>%
  filter(date)

mostCases = newData %>%
  group_by(county) %>%
  summarise(cases = sum(cases, na.rm = TRUE)) %>%
  arrange(-cases) %>%
  head(5)
  #slice_max(cases, n = 5)

mostNewCases = newData %>%
  select(county, new_cases) %>%
  group_by(county) %>%
  summarise(new_cases = sum(new_cases, na.rm = TRUE)) %>%
  arrange(-new_cases) %>%
  slice_max(new_cases, n = 5)

casesPerCapita = newData %>%
  group_by(county, pop2019) %>%
  summarise(cases = sum(cases, na.rm = TRUE)) %>%
  mutate(cases_per_capita = cases / pop2019) %>%
  arrange(-cases_per_capita) %>%
  head(5)

newCasesPerCapita = newData %>%
  select(county, new_cases, pop2019) %>%
  group_by(county, pop2019) %>%
  summarise(new_cases = sum(new_cases, na.rm = TRUE)) %>%
  mutate(new_cases_per_capita = new_cases / pop2019) %>%
  arrange(-new_cases_per_capita)
  head(5)


?summarise()
currentPop = newData %>%
  filter(date == max(date)) %>%
  group_by(county)
