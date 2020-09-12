library(tidyverse)
library(knitr)
library(readxl)
library(zoo)
library(ggthemes)
library(scales)


pop = read_excel('data/PopulationEstimates.xls', skip = 2) %>%
  select(pop2019 = POP_ESTIMATE_2019, fips = FIPStxt)

covid_url = 'https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv'

covid = read_csv(covid_url)

state_1 = 'California'

# Question 1:

newData = inner_join(pop, covid, by = 'fips') %>%
  filter(state == state_1) %>%
  group_by(county) %>%
  mutate(new_cases = cases - lag(cases))


mostCases = newData %>%
  group_by(county) %>%
  summarise(cases = sum(cases, na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(-cases) %>%
  slice_max(cases, n = 5)

knitr::kable(mostCases, caption = paste(state_1, 'counties with the most cumulative cases'),
             col.names = c('County', 'Cases'))

mostNewCases = newData %>%
  select(county, new_cases) %>%
  group_by(county) %>%
  summarise(new_cases = sum(new_cases, na.rm = TRUE)) %>%
  arrange(-new_cases) %>%
  slice_max(new_cases, n = 5)

knitr::kable(mostNewCases, caption = paste(state_1, 'counties with the most new cases'),
             col.names = c('County', 'New Cases'))

casesPerCapita = newData %>%
  group_by(county, pop2019) %>%
  summarise(cases = sum(cases, na.rm = TRUE)) %>%
  mutate(cases_per_capita = cases / pop2019) %>%
  select(county, cases_per_capita) %>%
  arrange(-cases_per_capita) %>%
  head(5)

knitr::kable(casesPerCapita, caption = paste(state_1, 'counties with the most cumaltive cases per capita'),
             col.names = c('County', 'Cases per capita'))

newCasesPerCapita = newData %>%
  group_by(county, pop2019) %>%
  mutate(new_cases = cases - lag(cases)) %>%
  summarise(new_cases = sum(new_cases, na.rm = TRUE)) %>%
  mutate(new_cases_per_capita = new_cases / pop2019) %>%
  select(county, new_cases_per_capita) %>%
  arrange(-new_cases_per_capita) %>%
  head(5)

knitr::kable(casesPerCapita, caption = paste(state_1, 'counties with the most new cases per capita'),
             col.names = c('County', 'New Cases per capita'))


last14days = newData %>%
  filter(date > max(date) - 13) %>%
  group_by(county, pop2019) %>%
  summarise(new_cases = sum(new_cases)) %>%
  ungroup() %>%
  mutate(casePer100 = new_cases / (pop2019 / 100000)) %>%
  filter(casePer100 <= 100) %>%
  pull(county)

length(last14days)


# Question 2:


fourStates = covid %>%
  filter(state %in% c('California', 'New York', 'Washington', 'Florida')) %>%
  group_by(state, date) %>%
  summarise(cases = sum(cases, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(state) %>%
  mutate(new_cases = cases - lag(cases),
         sevenDayRollingMean = rollmean(new_cases, 7, fill = NA, align = 'right')) %>%
  filter(new_cases >= 0)

dailyNewCases_ggplot = ggplot(data = fourStates, aes(x = date, y = new_cases)) +
  geom_col(aes(y = new_cases), col = 'aquamarine4', fill = 'aquamarine3') +
  geom_line(aes(y = sevenDayRollingMean), col = 'darkgreen', size = 1) +
  labs(title = 'Daily new cases in California, Florida, Louisiana, New York',
       x = 'Date',
       y = 'New cases',
       subtitle = 'Data Source: The New York Times') +
  facet_wrap(~state, scale = 'free_y') +
  theme_economist() +
  theme(aspect.ratio = .5) +
  theme(axis.text.x = element_text(angle = 90, face = "bold"))


dailyNewCases_ggplot

ggsave(dailyNewCases_ggplot, file = 'img/daily-new-cases-in-fourStates.png', width = 10)

state1 = 'California'
state2 = 'New York'
state3 = 'Louisiana'
state4 = 'Florida'

newCasesPerCap_4 = inner_join(pop, covid, by = 'fips') %>%
  filter(state %in% c(state1, state2, state3, state4)) %>%
  group_by(state, date) %>%
  summarise(cases = sum(cases), pop2019 = sum(pop2019)) %>%
  ungroup() %>%
  group_by(state) %>%
  mutate(new_cases = cases - lag(cases)) %>%
  mutate(new_cases_per_capita = (new_cases / pop2019),
         newCasesRollingMean = rollmean(new_cases_per_capita, 7, fill = NA, align = 'right'))


newCasesPerCap4_ggplot = ggplot(data = newCasesPerCap_4, aes(x = date, y = new_cases_per_capita)) +
  geom_bar(aes(y = new_cases_per_capita), col = 'cyan4', fill = 'cornflowerblue', stat="identity") +
  geom_line(aes(y = newCasesRollingMean), col = 'darkblue', size = 1) +
  labs(title = paste('Daily new cases per capita in',state1, ',',state2, ',', state3, ',', state4),
       x = 'Date',
       y = 'New cases per capita',
       subtitle = 'Data Source: The New York Times') +
  facet_wrap(~state, scale = 'free_y') +
  scale_y_continuous(name = "New cases per capita", labels = comma) +
  theme_economist() +
  theme(aspect.ratio = .5) +
  theme(axis.text.x = element_text(angle = 90, face = "bold"))
# theme(x.axis.text = element_text(format = big.mark(“,”))


newCasesPerCap4_ggplot

ggsave(newCasesPerCap4_ggplot, file = 'img/daily-new-cases-per-capita-in-fourStates.png', width = 10)


# Question 3:

centers_csv = 'https://mikejohnson51.github.io/spds/data/county-centroids.csv'

centers = read_csv(centers_csv

covid_centroids = left_join(covid, centers, by = 'fips') %>%
  group_by(date, county) %>%
  summarise(sumX = sum(LON), sumY = sum(LAT), cases = sum(cases),
            weightMeanCenter = weighted.mean(sumX, sumY, cases))



class(covid_centroids)
covidCenters = inner_join(centers, covid, by = 'county') %>%
  group_by(date, county) %>%
  summarise(sumX = sum(LON), sumY = sum(LAT), cases = sum(cases),
            weightMeanCenter = weighted.mean(sumX, sumY, cases))

















