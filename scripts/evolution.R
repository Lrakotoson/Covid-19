library(tidyverse)

#############################################################

regions <- latest() %>%
  distinct(Continent = as.character(Continent)) %>%
  na.omit() %>% unlist() %>% as.vector() %>% c("Monde")

#############################################################

map_evolution <- function(region, time, colonne){
  #' Renvoie un plotly en de la région à un moment t
  #' region: Région/Continent
  #' time: argument t de latest, t >= 1
  if (region == "Asia"){
    long <- 94
    lat <- 40
    zoom <- 2
  }else if (region == "North America"){
    long <- -102
    lat <- 55
    zoom <- 2
  }else if (region == "South America"){
    long <- -59
    lat <- -18
    zoom <- 1.8
  }else if (region == "Europe"){
    long <- 14
    lat <- 54
    zoom <- 2.5
  }else if (region == "Australia"){
    long <- 134
    lat <- -28
    zoom <- 3
  }else if (region == "Africa"){
    long <- 26
    lat <- 2
    zoom <- 2.1
  }else{
    long <- 18
    lat <- 35
    zoom <- 1.2
  }
  
  if (colonne == "Morts"){
    color <- "sandybrown"
  } else if (colonne == "Retablis"){
    color <- "seagreen"
  } else {
    color <- "red"
  }
  
  plot <- latest(time) %>%
    mutate(info = paste0("Nombre de cas: ", Cas,
                         "\n Retablis: ", Retablis,
                         "\n Deces: ", Morts),
           value = .[[colonne]]
    ) %>%
    plot_ly(
      lat = ~Lat,
      lon = ~Long,
      marker = list(color = color, size = ~log(1+value), sizeref=0.1, opacity=0.4),
      type = 'scattermapbox',
      text = ~State,
      hovertext = ~info,
      hovertemplate = paste(
        "<b>%{text}</b><br><br>",
        "%{hovertext}",
        "<extra></extra>"
      )) %>%
    layout(
      title = paste0("\n<b>", region,
                     "</b>: <b style='color:",color,"'>", colonne,
                     "</b> au <b>", as.Date(names(T_cas)[time+4], "%m/%d/%y"),
                     "</b>"),
      mapbox = list(
        style = 'carto-positron',
        zoom = zoom,
        center = list(lon = long, lat= lat)),
      margin = list(
        l = 0, r = 0,
        b = 0, t = 0,
        pad = 0
      )
    )
  return(plot)
}
#############################################################

timeserie <- function(region = "Monde"){
  #' Renvoie un tibble des données en séries temporelles
  #' region: Region/Continent
  
  fusion <- function(table, name, region = "Monde"){
    #' Renvoie un tibble de la table en série temporelle
    #' name: nom de la variable Cas/Morts/Retablis
    #' region: Region/Continent
    
    if (region != "Monde"){
      table <- table %>%
        mutate(Region = continent(Long, Lat)) %>%
        filter(Region  == region) %>%
        select(-Region)
    }
    table<- table %>% 
      select(-c(1:4)) %>%
      replace(is.na(.), 0) %>%
      summarise_all(funs(sum)) %>%
      gather(key = "Date") %>%
      mutate(Date = as.Date(Date,format="%m/%d/%y")) %>%
      rename(!!name := value)
    return(table)
  }
  
  data <- fusion(T_cas, "Cas", region) %>%
    left_join(fusion(T_retablis, "Retablis", region), "Date") %>% 
    left_join(fusion(T_morts, "Morts", region), "Date") %>% 
    mutate(
      `taux retablissement` = ifelse(Cas == 0, 0, round(Retablis*100/Cas, 2)),
      `taux de mortalite` = ifelse(Cas == 0, 0, round(Morts*100/Cas, 2))
    )
  
  return(data)
}
#############################################################

ts_evolution <- function(region = "Monde", logscale = F){
  #' Renvoie un ramCharts de l'évolution au cours du temps
  #' region: Region/Continent
  #' logscale: bool, échelle logarithmique
  
  data <- timeserie(region)
  if (logscale){
    data <- data %>%
      mutate(Cas = log(Cas+1), Morts = log(Morts+1), Retablis = log(Retablis+1))
  }
  
  plot <- data %>%
    mutate(Date = as.POSIXct(paste(Date, "00:00:00"))) %>%
    amTimeSeries("Date", c("Cas", "Retablis", "Morts"),
                 color = c("#b33939", "#16a085", "#e67e22"),
                 type = "smoothedLine",
                 fillAlphas = c(0.1, 0.2, 0.3),
                 main = paste("Evolution:", region)
    )
  return(plot)
}
#############################################################

rate_evolution <- function(region = "Monde"){
  #' Renvoie un ramCharts des taux au cours du temps
  #' region: Region/Continent
  
  plot <- timeserie(region) %>%
    mutate(Date = as.POSIXct(paste(Date, "00:00:00"))) %>%
    amTimeSeries("Date", c("taux retablissement", "taux de mortalite"),
                 color = c("#16a085", "#e67e22"),
                 type = "smoothedLine",
                 fillAlphas = 0.2,
                 main = paste("Taux (%) :", region)
    )
  return(plot)
}