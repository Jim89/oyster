# Step 0 - prep -----------------------------------------------------------
library(tidyverse)
library(forcats)
library(httr)
library(jsonlite)
library(readr)

credentials <- list(app_id = tfl_app_id, app_key = tfl_app_key)

# Step 1 - get data -------------------------------------------------------
# Accidents
url <- "https://api.tfl.gov.uk/AccidentStats/2015"
req <- GET(url, query = credentials)
accidents <- req %>% 
  content("text") %>% 
  fromJSON() %>% 
  as_data_frame()


pop <- read_csv("./api/population-estimates-single-year-age.csv")
colnames(pop) <- colnames(pop) %>% tolower()

# Step 2 - prep data ------------------------------------------------------
accidents <- accidents %>% 
  mutate(severity = as.factor(severity),
         borough = as.factor(borough),
         severity = fct_infreq(severity),
         borough= fct_infreq(borough)) %>% 
  select(borough, severity, lat, lon, location, date) %>% 
  mutate(date_cln = gsub("T", " ", date),
         date_cln = gsub("Z", " ", date_cln),
         date_cln = lubridate::ymd_hms(date_cln))

pop <- pop %>% 
  filter(year == 2014) %>% 
  select(borough, `all persons`) %>% 
  rename(persons = `all persons`)
  
joined <- accidents %>% 
  left_join(pop)

joined %>% 
  group_by(borough) %>% 
  summarise(total_incidents = n(),
            persons = mean(persons)) %>% 
  na.omit() %>% 
  mutate(rate = total_incidents / persons) %>% 
  ggplot(aes(x = persons, y = total_incidents, colour = total_incidents)) +
  geom_point(size = 4) +
  geom_smooth()

# Step 3 - plot -----------------------------------------------------------
accidents %>% 
  ggplot(aes(x = fct_rev(borough))) +
  geom_bar(aes(fill = severity), colour = "white") +
  scale_fill_brewer(palette = "Dark2") +
  coord_flip() +
  theme_minimal()

