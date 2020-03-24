library(leaflet)

#############################################################

comparemap <- function(Country1, Country2 = i18n()$t("Monde"), t = ncol(T_cas)){
  #' Renvoie un leaflet du monde avec les pays sélectionnés
  #' Country1, Country2: pays
  #' t: temps, entier >= 1
  
  level_key <- c(
    'Congo (Kinshasa)' = 'Democratic Republic of the Congo',
    "Cote d'Ivoire" = "Ivory Coast",
    "Czechia" = "Czech Republic",
    "Korea, South" = "South Korea",
    "North Macedonia" = "Macedonia",
    "Serbia" = "Republic of Serbia",
    "Taiwan*" = "Taiwan",
    "US" = "United States of America"
  )
  
  
  if(Country1 %in% names(level_key)){
    Country1 <- level_key[Country1]
  }
  if(Country2 %in% names(level_key)){
    Country2 <- level_key[Country2]
  }
  
  base <- geodata
  stats <- brief("Country", t)
  stats$group <- recode(stats$group, !!!level_key)
  
  base@data <- tibble(group = base$admin) %>%
    left_join(stats) %>%
    rename(admin = group) %>%
    filter(admin == Country1 | admin == Country2) %>%
    right_join(base@data) %>%
    mutate(admin = factor(admin))
  
  pal <- colorNumeric("YlOrRd", NULL)
  
  plot <- leaflet(base) %>%
    addProviderTiles("CartoDB.Positron") %>% 
    addPolygons(
      stroke = F, smoothFactor = 0.1,
      fillOpacity = 1,
      fillColor = ~pal(Cas),
      label = ~paste(
        admin, "|",
        Cas, i18n()$t("Cas"),
        Morts, i18n()$t("Morts"),
        Retablis, i18n()$t("Retablis"))
    ) %>% 
    addLegend(
      pal = pal,
      values = ~(Cas)
    )
  
  return(plot)
}
#############################################################

compare_data <- function(situation, Country1, Country2, t1, t2 = ncol(T_cas)-4){
  #' Renvoie un tible de la sitation de deux/un pays pour une periode
  #' Situation: Cas/Morts/Retablis/Letalite/Actifs/Nouveaux, str
  #' Country1, Country2: Pays, str
  #' t1, t2: entier t1<2
  
  aggreg <- F
  if (situation == "Retablis"){
    data <- cbind(
      T_retablis[,1:4],
      T_retablis[,5:ncol(T_morts)]*100/(T_cas[,5:ncol(T_morts)] + 1)
    )
  } else if (situation == "Morts"){
    data <- cbind(
      T_morts[,1:4],
      T_morts[,5:ncol(T_morts)]*100/(T_morts[,5:ncol(T_morts)] + T_retablis[,5:ncol(T_morts)] + 1)
    )
  } else if (situation == "Letalite"){
    
    # (temp)
    T_cas <- T_cas_g
    T_morts <- T_morts_g
    
    data <- cbind(
      T_morts[,1:4],
      T_morts[,5:ncol(T_morts)]*100/(T_cas[,5:ncol(T_morts)] + 1)
    )
  } else if (situation == "Actifs"){
    aggreg <- T
    data <- cbind(
      T_cas[,1:4],
      T_cas[,5:ncol(T_cas)] - (T_morts[,5:ncol(T_cas)] + T_retablis[,5:ncol(T_cas)])
    )
  } else {
    aggreg <- T
    data <- T_cas_g # (temp)
  }
  
  data <- data %>%
    filter(Country %in% c(Country1, Country2)) %>%
    select(c(2, all_of(t1+4):all_of(t2+4))) %>%
    group_by(Country)%>%{
      if(aggreg)
        summarise_all(.,funs(sum))
      else
        summarise_all(.,funs(mean))
    }%>%
    replace(is.na(.), 0) %>%
    gather(key = Date, value = value, 2:ncol(.)) %>% 
    spread_(key = names(.)[1],value = 'value') %>%
    mutate(Date = as.Date(Date,format="%m/%d/%y")) %>%
    arrange(Date)
  
  if (situation == "Nouveaux"){
    data <- data %>% 
      mutate(!!Country1 := .[[Country1]] - lag(.[[!!Country1]]),
             !!Country2 := .[[Country2]] - lag(.[[!!Country2]])
      ) %>% na.omit() %>% 
      mutate_if(is.numeric, function(x) ifelse(x<0, 0, x))
  }
  
  return(data)
}
#############################################################

fitLM <- function(date, y, Country, pol = 1, pred = 10){
  #' Renvoie un tibble des prédictions du lm
  #' date: vecteur date
  #' y: vecteur y
  #' Country: nom du pays
  #' pol: lm polynomiale 1 à 3
  #' pred: nombre de jours à prédire
  
  x <- as.numeric(date)
  x_pred <- as.numeric(unique(c(date, date+pred)))
  y <- log(y+1)
  
  model <- lm(y~poly(x, pol))
  y_pred <- predict.lm(model, newdata = list(x = x_pred))
  
  name <- paste0("pred.", Country)
  pred <- tibble(Date = as.Date(x_pred, origin = "1970-01-01"),
                 !!name := exp(y_pred))
  
  return(pred)
}
#############################################################

compare_situation <- function(Situation, Country1, Country2, t1, t2, logscale = F, reg = 0, pred = 10){
  #' Renvoie un rAmCharts de l'évolution de la situation avec une régression
  #' Situation: Cas/ Retablis/ Morts
  #' Country1, Country2: pays
  #' t1, t2: période, entier t1 < t2
  #' reg: {0,1,2,3} reg polynomial, si 0 aucune reg
  #' pred: nb jours à prédire si reg > 0
  
  fill_alphas <- 0
  if (Situation == "Morts"){
    main <- i18n()$t("Taux de mortalite")
  } else if (Situation == "Retablis"){
    main <- i18n()$t("Taux de retablissement")
  } else if (Situation == "Letalite"){
    main <- i18n()$t("Taux de letalite")
    fill_alphas <- c(0.3, 0.3, 0.2, 0.2)
  } else if (Situation == "Actifs"){
    main <- i18n()$t("Nombre de Cas Actifs")
    fill_alphas <- c(0.3, 0.3, 0.2, 0.2)
  } else {
    main <- paste(
      i18n()$t("Situation:"),
      i18n()$t(Situation))
  }
  
  data <- compare_data(Situation, Country1, Country2, t1, t2)
  
  if (reg > 0){
    data <- data %>%
      right_join(fitLM(data$Date, data[[Country1]], Country1, reg, pred)) %>%
      right_join(fitLM(data$Date, data[[Country2]], Country2, reg, pred))
  }
  
  if (logscale){
    data[,2:ncol(data)] <- log(data[,2:ncol(data)] +1)
  }
  
  
  plot <- data %>%
    mutate(Date = as.POSIXct(paste(Date, "00:00:00"))) %>%
    amTimeSeries("Date", colnames(.)[2:ncol(.)],
                 color = c("#eb3b5a", "#2d98da", "#20bf6b", "#4b6584"),
                 type = c(rep("smoothedLine",2), rep("line",2)),
                 linetype = c(0, 5, 0, 5),
                 fillAlphas = fill_alphas,
                 bullet = c(rep("bubble",2), rep("round",2)),
                 bulletSize = c(0, 0, 5, 5),
                 linewidth = c(4, 4, 1, 1),
                 main = main,
                 scrollbar = F
    )
  
  return(plot)
}
#############################################################

compare_new <- function(Country1, Country2, t1, t2){
  #' Renvoie un amTimeSeries de l'évolution des nouveaux cas
  #' Country1, Country2: pays
  #' t1, t2: entier t1 < t2
  
  plot <- compare_data("Nouveaux", Country1, Country2, t1, t2) %>%
    mutate(Date = as.POSIXct(paste(Date, "00:00:00"))) %>%
    amTimeSeries(
      "Date", c(Country1, Country2),
      color = c("#eb3b5a","#2d98da"),
      type = "column",
      fillAlphas = 1,
      scrollbar = F,
      main = i18n()$t("Nouveaux Cas"),
    )
  return(plot)
}