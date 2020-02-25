library(tidyverse)

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
                     "</b> au <b>", as.Date(names(T_cas)[time], "%m/%d/%y"),
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