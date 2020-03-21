library(tidyverse)
library(xml2)
library(sp)
library(rworldmap)
library(rgdal)

T_cas <- read_csv('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv')
T_retablis <- read_csv('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Recovered.csv')
T_morts <- read_csv('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv')
geodata <- rgdal::readOGR("scripts/custom.geo.json")
####################################################################

clean <- function(data){
  data <- data %>% 
    rename(State = 'Province/State', Country = 'Country/Region') %>%
    mutate(State = coalesce(State, Country))
  return(data)
}
  
T_cas <<- clean(T_cas)
T_retablis <<- clean(T_retablis)
T_morts <<- clean(T_morts)
####################################################################

continent <- function(lon, lat){
  #' Retourne le continent où se trouve le point
  #' lon: Longitude, lat: Latitude
  coord <- data.frame(lon = lon, lat = lat)
  countriesSP <- getMap(resolution = 'low')
  pointsSP <- SpatialPoints(coord,
                            proj4string = CRS(proj4string(countriesSP))
                            )  
  indices <- over(pointsSP, countriesSP)
  
  return(indices$REGION)
}
####################################################################

latest <- function(t = ncol(T_cas) - 4){
  #' Retourne les données les plus récentes à l'instant t
  #' t: temps, entier >= 1
  
  if (t > ncol(T_morts) - 4 | t > ncol(T_retablis) - 4){
    t <- min(ncol(T_morts), ncol(T_retablis)) - 4
  }
  
  data <- T_cas %>% 
    select(1:4, t+4) %>% 
    rename(Cas = tail(names(.), 1)) %>% 
    left_join((T_retablis %>% 
                 select(1, t+4) %>% 
                 rename(Retablis = tail(names(.), 1))
              )
    ) %>% 
    left_join((T_morts %>% 
                 select(1, t+4) %>% 
                 rename(Morts = tail(names(.), 1))
               )
    ) %>% 
    mutate(
      Continent = continent(lon = `Long`, lat = `Lat`)
    ) %>% 
    select(1:2, Continent, everything())
  
  return(data)
}
####################################################################

brief <- function(group = NULL, t = ncol(T_cas) - 4){
  #' Renvoie un résumé à un instant t
  #' group: NULL, 'Country' ou 'Continent'
  #' t: temps, entier >= 1
  
  if(is_empty(group)){
    data <- latest(t) %>% 
      mutate(group = 'Monde')
    group <- 'group'
  } else {
    data <- latest(t) %>% 
      rename('group' = group)
  }
  data <- data %>% 
    group_by(group) %>% 
    summarise(Cas = sum(Cas, na.rm = T),
              Retablis = sum(Retablis, na.rm = T),
              Morts = sum(Morts, na.rm = T))
  return(data)
}
####################################################################

actus <- function(hl, gl, rows = 10){
  #' Renvoie un tibble rowsx1 de liens html
  #' rows: nombre de lignes à renvoyer, entier =< 100
  
  flux <- read_xml(
    paste0(
      "https://news.google.com/rss/search?q=coronavirus&hl=",
      hl)
    )
  item_title <- flux %>% xml_find_all("//item/title") %>% xml_text()
  item_link <- flux %>% xml_find_all("//item/link") %>% xml_text()
  
  data <- tibble(item_title, item_link) %>%
    separate(item_title, c("title", "source"), " - ") %>% 
    select(title, source, item_link) %>%
    mutate(Actus = paste0("<a href='", item_link, "' target='_blank'>", title, "</a><em> - ",source,"<em>")) %>% 
    select(Actus) %>% 
    top_n(rows)
  return(data)
}