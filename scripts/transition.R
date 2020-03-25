

# fr: Il s'agit de données de transition
# en: This is temporary transition data

####################################################################

big <- read_csv("https://raw.githubusercontent.com/ulklc/covid19-timeseries/master/countryReport/raw/rawReport.csv")
big <- big %>% 
  select(-c(countryCode, region)) %>% 
  rename(Lat = lat,
         Long = lon,
         Country = countryName) %>% 
  mutate(day = format(day, "%m/%d/%Y"),
         State = Country) %>%
  select(State, everything())


T_retablis <<- big %>%
  select(-c(confirmed, death)) %>%
  pivot_wider(names_from = day, values_from = recovered)


T_cas <<- big %>%
  select(-c(recovered, death)) %>%
  pivot_wider(names_from = day, values_from = confirmed)


T_morts <<- big %>%
  select(-c(confirmed, recovered)) %>%
  pivot_wider(names_from = day, values_from = death)