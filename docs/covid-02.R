

library(tidyverse)
library(sf)
library(scales)
library(zoo)
library(plotly)


pop = readxl::read_excel('data/PopulationEstimates.xls', skip = 2) %>%
  select(state = State, pop_19 = POP_ESTIMATE_2019, fips = FIPStxt)

covid_url = 'https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv'
covid = read_csv(covid_url)

tmp_state = 'California'
shp = USAboundaries::us_states() %>%
  filter(name == tmp_state)

# Question 1:

covid = inner_join(covid, select(pop, pop_19, fips), by = 'fips')

ca_covid = covid %>% filter(state == tmp_state) %>%
  group_by(county) %>%
  mutate(new_cases = cases - lag(cases)) %>%
  mutate(cases_per_cap = cases/pop_19, new_cases_per_cap = new_cases/pop_19)

counties = USAboundaries::us_counties() %>%
  filter(state_name == tmp_state)

# CASES PER 100,000 RESIDENTS
cases_per_100k = ca_covid %>%
  filter(date > max(date) - 7) %>%
  group_by(county, pop_19) %>%
  mutate(cases_100k = new_cases / (pop_19 / 100000)) %>%
  mutate(cases_100k = (sum(cases_100k)/(7))) %>%
  mutate(state = 'CA')

# 7 - DAY ROLLING MEAN
ca_covid = ca_covid %>%
  group_by(county, date) %>%
  mutate(total_cases = sum(cases, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(county) %>%
  mutate(new_cases = cases - lag(cases),
         rolling_mean = rollmean(new_cases, 7, fill = NA, align = 'right'))

# 7 - DAY ROLLING MEAN PER CAPITA
ca_covid  = ca_covid %>%
  group_by(county, date) %>%
  mutate(pop_19 = sum(pop_19)) %>%
  ungroup() %>%
  group_by(county) %>%
  mutate(rolling_mean_per_cap = rollmean(new_cases_per_cap, 7, fill = NA, align = 'right'))

covid_tmp = covid %>%
  filter(state == tmp_state) %>%
  group_by(county) %>%
  mutate(new_cases = cases - lag(cases)) %>%
  mutate(cases_per_cap = cases/pop_19, new_cases_per_cap = new_cases/pop_19)
ca_rolling = covid_tmp %>%
  group_by(state, date) %>%
  summarise(cases = sum(cases, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(state) %>%
  mutate(new_cases = cases - lag(cases),
         rolling_mean = rollmean(new_cases, 7, fill = NA, align = 'right'))
# filter(new_cases >= 0)
#
# roll_mean_daily = roll_mean %>%
#   arrange(desc(date)) %>%
#   slice(n = 1)

# 7 - DAY ROLLING MEAN PER CAPITA


ca_rolling_per_cap  = covid_tmp %>%
  group_by(state, date) %>%
  summarise(cases = sum(cases), pop_19 = sum(pop_19)) %>%
  mutate(new_cases_per_cap = (new_cases/pop_19),
         rolling_mean_per_cap = rollmean(new_cases_per_cap, 7, fill = NA, align = 'right'))

# roll_mean_per_cap_daily = roll_mean_per_cap %>%
#   arrange(desc(date)) %>%
#   slice(n = 1)

temp2 = ca_covid %>% group_by(county) mutate(roll2)

# JOIN CASES PER 100K, ROLLING MEAN, & ROLLING MEAN PER CAPITA DATA WITH COVID DATA FRAME
# ca_covid = left_join(ca_covid, select(per_100, county, cases_per_100), by = 'county')
# ca_covid = left_join(ca_covid, select(county_rolling, county, rolling_mean), by = 'county')
# ca_covid = left_join(ca_covid, select(roll_mean_per_cap_daily, county, new_cases_per_cap,
#                                       rolling_mean_per_cap), by = 'county')


# CASE FATALITY RATE ------ Take the number of COVID-19 deaths in population
# Divide that by the total number of COVID-19 cases and multiply by 100.
# This is typically expressed as a percentage.

# CREATE DATAFRAME FOR CA COUNTIES + GEOMETRIES
ca_spatial = ca_covid %>%
  group_by(county) %>%
  arrange(desc(date)) %>%
  slice(n = 1)


# SELECT DESIRED COLUMNS FROM COUNTIES SF DATAFRAME

counties = counties %>% select(state = state_name, county = name, fips = geoid)
# counties = counties %>% select(state, county, fips)

# JOIN DATAFRAME OF 58 COUNTIES W/ RESPECTIVE GEOMETRIES
ca_spatial = left_join(ca_spatial, select(counties, fips), by = 'fips') %>%
  st_as_sf()

temp7 = cases_per_100k %>% group_by(county) %>%
  arrange(desc(date)) %>%
  slice(n =1)

ca_spatial = left_join(ca_spatial, select(temp7, county, cases_100k), by = 'county')


# COLOR PALLETES
# RColorBrewer::display.brewer.all(n=4, exact.n=FALSE)

nb.cols = 10
col1 = RColorBrewer::brewer.pal(9,"Blues")

# FILL COLOR
num = length(unique(ca_spatial$cases_per_100))
col4 = colorRampPalette(RColorBrewer::brewer.pal(9,"YlOrRd"))(num)

# scale_fill_manual(values = pal1)

font = list(
  family = 'Courier',
  size = 15,
  color = 'white')
label = list(
  bgcolor = '#232F34',
  bordercolor = 'transparent',
  font = font)
########################################################
####################### PLOTS ##########################
########################################################

####################################################################################
####################################################################################

# COUNTY ROLLING MEAN PER CAPITA PLOT
tmp2 = ca_covid %>% filter(new_cases >= 0) %>% rename('Rolling mean per capita' = rolling_mean_per_cap, count)

gg_county_rolling = ggplot(data = tmp2, aes(x= Date, y = `Rolling mean per capita`)) +
  geom_line(aes(col = County), size = .5) +
  labs(title = paste('County level 7-day rolling mean  - ', tmp_state),
       x = 'Date',
       y = 'New cases per capita',
       col = 'County',
       subtitle = 'Data Source: The New York Times') +
  theme(axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        axis.title.x = element_text(face = "bold", size = 16),
        axis.title.y = element_text(face = "bold", size = 16, hjust = 0.5),
        plot.title = element_text(face = 'bold', size = 20),
        plot.subtitle = element_text(size = 14),
        legend.title = element_text(face = "bold", size = 16, hjust = 0.5),
        legend.title.align = 0.5,
        legend.text = element_text(face = "bold", size = 12))
plot_county_rolling = ggplotly(gg_county_rolling) %>%
  style(hoverlabel = label) %>%
  layout(font = font)

####################################################################################
####################################################################################

# SO CAL ROLLING MEAN PER CAPITA PLOTS
so_cal_counties = c('Los Angeles', 'Orange', 'San Diego', 'San Luis Obispo', 'Santa Barbara', 'Ventura')
so_cal = ca_covid %>% filter(county %in% so_cal_counties)
so_cal$county = factor(so_cal$county,levels=c('San Luis Obispo', 'Santa Barbara',
                                              'Ventura', 'Los Angeles', 'Orange', 'San Diego'))

gg_so_cal_roll_mean = ggplot(data = so_cal, aes(x = date, y = rolling_mean_per_cap)) +
  geom_col(aes(y = rolling_mean_per_cap), col = 'aquamarine4', fill = 'aquamarine3') +
  geom_line(aes(y = rolling_mean_per_cap), col = 'darkgreen', size = 1) +
  labs(title = 'Daily new cases  -  Southern California',
       x = 'Date',
       y = 'New cases',
       subtitle = 'Data Source: The New York Times') +
  theme(axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        axis.title.x = element_text(face = "bold", size = 16),
        axis.title.y = element_text(face = "bold", size = 16, hjust = 0.5),
        plot.title = element_text(face = 'bold', size = 22),
        plot.subtitle = element_text(size = 14),
        legend.title = element_text(face = "bold", size = 16, hjust = 0.5),
        legend.title.align = 0.5,
        legend.text = element_text(face = "bold", size = 12),
        strip.text = element_text(face = 'bold', size=16)) +
  facet_wrap(~county) +
  scale_y_continuous(labels = scales::comma)

plot_so_cal_roll_mean = ggplotly(gg_so_cal_roll_mean) %>%
  style(hoverlabel = label) %>%
  layout(font = font)

####################################################################################
####################################################################################

# CA COUNTY MAP ----- CASES PER 100K
gg_cases_per_100 = ggplot() +
  geom_sf(data = shp) +
  geom_sf(data = ca_spatial, aes(fill = cases_100k,
                                 text = paste0('', cases_100k),
                                 text2 = county),
          col = 'black', size = .3) +
  labs(fill = 'Cases per 100,000 residents')

ggplotly(gg_cases_per_100, tooltip = c('text2', 'text')) %>%
  style(hoverlabel = label) %>%
  layout(font = font)

####################################################################################
####################################################################################

# CA STATE ROLLING MEAN GRAPH
gg_ca_roll_mean = ggplot(data = ca_rolling, aes(x = date, y = new_cases)) +
  geom_col(aes(y = new_cases), col = 'aquamarine4', fill = 'aquamarine3') +
  geom_line(aes(y = rolling_mean), col = 'darkgreen', size = 1) +
  labs(title = paste('Daily new cases  - ', tmp_state),
       x = 'Date',
       y = 'New cases',
       subtitle = 'Data Source: The New York Times') +
  theme(axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        axis.title.x = element_text(face = "bold", size = 16),
        axis.title.y = element_text(face = "bold", size = 16, hjust = 0.5),
        plot.title = element_text(face = 'bold', size = 20),
        plot.subtitle = element_text(size = 14),
        legend.title = element_text(face = "bold", size = 16, hjust = 0.5),
        legend.title.align = 0.5,
        legend.text = element_text(face = "bold", size = 12))

plot_ca_roll = ggplotly(gg_ca_roll_mean) %>%
  style(hoverlabel = label) %>%
  layout(font = font)

####################################################################################
####################################################################################

# COUNTY LOCKDOWN STATUS
# CRITERIA:
# Widespread ----- daily new cases per 100k >= 7.0
# Substantial ----- 7.0 > daily new cases per 100k >= 4.0
# Moderate ----- 3.9 >= daily new cases per 100k >= 1.0
# Minimal ----- 1.0 > daily new cases per 100k

tmp1 = ca_spatial %>% mutate(status = case_when(cases_100k >= 7.0 ~ 'Widespread',
                                                7.0 > cases_100k & cases_100k >= 4.0 ~ 'Substantial',
                                                3.9 > cases_100k & cases_100k >= 1.0 ~ 'Moderate',
                                                1.0 > cases_100k ~ 'Minimal'))
# cases_100k >= 7.0 ~ 'Widespread',
# cases_100k >= 7.0 ~ 'Widespread'))
gg_lockdown = ggplot() +
  geom_sf(data = shp) +
  geom_sf(data = tmp1, aes(fill = status),
          # text = paste0('', cases_100k),
          # text2 = county),
          col = 'black', size = .3) +
  labs(fill = 'Risk level',
       title = "California's county risk levels") +
  theme(axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        axis.title.x = element_text(face = "bold", size = 16),
        axis.title.y = element_text(face = "bold", size = 16, hjust = 0.5),
        plot.title = element_text(face = 'bold', size = 20),
        plot.subtitle = element_text(size = 14),
        legend.title = element_text(face = "bold", size = 16, hjust = 0.5),
        legend.title.align = 0.5,
        legend.text = element_text(face = "bold", size = 12))

plotly_lockdown = ggplotly(gg_lockdown) %>%
  style(hoverlabel = label) %>%
  layout(font = font)



####################################################################################
####################################################################################

ggplot() +
  geom_sf(data = shp) +
  geom_sf(data = ca_spatial, aes(fill = rolling_mean), col = 'black', size = .3)


ggplot() +
  geom_sf(data = shp) +
  geom_sf(data = ca_spatial, aes(fill = rolling_mean_per_cap), col = 'black', size = .3)

####################################################################################
####################################################################################

