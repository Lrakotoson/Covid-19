

#############################################################

regions <- latest() %>%
  distinct(Continent = as.character(Continent)) %>%
  na.omit() %>% unlist() %>% as.vector()

#############################################################

map_evolution <- function(region, time, colonne, titre = T, main = F){
  #' Renvoie un plotly en de la région à un moment t
  #' region: Région/Continent
  #' time: argument t de latest, t >= 1
  #' colonne: Cas/Retablis/Morts
  #' titre: bool, ajout du titre en fonction de la variable
  
  if (region == "Asia"){
    long <- 94
    lat <- 40
    zoom <- 1.6
  }else if (region == "North America"){
    long <- -102
    lat <- 55
    zoom <- 1.4
  }else if (region == "South America"){
    long <- -59
    lat <- -18
    zoom <- 1.2
  }else if (region == "Europe"){
    long <- 14
    lat <- 54
    zoom <- 2
  }else if (region == "Australia"){
    long <- 134
    lat <- -28
    zoom <- 2.6
  }else if (region == "Africa"){
    long <- 26
    lat <- 2
    zoom <- 1.7
  }else{
    long <- 18
    lat <- 35
    zoom <- ifelse(main, 1.2, 0.2)
  }
  
  if (colonne == "Morts"){
    color <- "sandybrown"
  } else if (colonne == "Retablis"){
    color <- "seagreen"
  } else {
    color <- "red"
  }
  
  plot <- latest(time) %>%
    mutate(info = paste0(i18n()$t("Nombre de cas: "), Cas, "\n ",
                         i18n()$t("Retablis: "), Retablis, "\n ",
                         i18n()$t("Deces: "), Morts),
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
      title = ifelse(titre,
                     paste0("\n<b>", i18n()$t(region),
                            "</b>: <b style='color:",color,"'>", i18n()$t(colonne),
                            "</b> - <b>", as.Date(names(T_cas)[time+4], "%m/%d/%y"),
                            "</b>"),
                     ""),
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

timeserie <- function(region = i18n()$t("Monde")){
  #' Renvoie un tibble des données en séries temporelles
  #' region: Region/Continent
  
  fusion <- function(table, name, region = i18n()$t("Monde")){
    #' Renvoie un tibble de la table en série temporelle
    #' name: nom de la variable Cas/Morts/Retablis
    #' region: Region/Continent
    
    if (region != i18n()$t("Monde")){
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
    replace(., is.na(.), 0) %>% 
    mutate(
      `taux retablissement` = ifelse(Cas == 0, 0, round(Retablis*100/Cas, 2)),
      `taux de mortalite` = ifelse(Cas == 0, 0, round(Morts*100/Cas, 2))
    )
  
  return(data)
}
#############################################################

nw_evolution <- function(region = i18n()$t("Monde"), colonne){
  #' Renvoie un ramCharts des nouvelles données
  #' region: Region/Continent
  #' colonne: Cas/Retablis/Morts
  
  if (colonne == "Cas"){
    color = "#b33939"
  } else if (colonne == "Retablis"){
    color = "#16a085"
  } else {
    color = "#e67e22"
  }
  
  nwcolonne <- paste(
    i18n()$t("Nouveaux "),
    i18n()$t(colonne)
  )
  
  data <- timeserie(region)%>%
    mutate(Cas = Cas - lag(Cas),
           Retablis = Retablis - lag(Retablis),
           Morts = Morts - lag(Morts)) %>% 
    select(1, colonne) %>% na.omit() %>%
    rename(!!nwcolonne := colonne) %>%
    mutate_if(is.numeric, function(x) ifelse(x<0, 0, x))
  
  
  plot <- data %>%
    mutate(Date = as.POSIXct(paste(Date, "00:00:00"))) %>%
    amTimeSeries("Date", nwcolonne,
                 color = color,
                 type = "column",
                 fillAlphas = 0.5
    )
  return(plot)
}
#############################################################

ts_evolution <- function(region = i18n()$t("Monde"), logscale = F){
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
    rename(
      !!i18n()$t("Cas") := "Cas",
      !!i18n()$t("Retablis") := "Retablis",
      !!i18n()$t("Morts") := "Morts"
    ) %>% 
    amTimeSeries("Date", colnames(.)[2:4],
                 color = c("#b33939", "#16a085", "#e67e22"),
                 type = "smoothedLine",
                 fillAlphas = c(0.1, 0.2, 0.3),
                 main = paste(
                   i18n()$t("Evolution:"),
                   i18n()$t(region))
    )
  return(plot)
}
#############################################################

rate_evolution <- function(region = i18n()$t("Monde")){
  #' Renvoie un ramCharts des taux au cours du temps
  #' region: Region/Continent
  
  plot <- timeserie(region) %>%
    mutate(Date = as.POSIXct(paste(Date, "00:00:00"))) %>%
    rename(
      !!i18n()$t("taux retablissement") := "taux retablissement",
      !!i18n()$t("taux de mortalite") := "taux de mortalite"
    ) %>% 
    amTimeSeries("Date", colnames(.)[5:6],
                 color = c("#16a085", "#e67e22"),
                 type = "smoothedLine",
                 fillAlphas = 0.2,
                 main = paste(
                   i18n()$t("Taux"),
                   "(%) :", i18n()$t(region))
    )
  return(plot)
}