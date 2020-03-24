library(rmarkdown)
library(shiny)

port <- Sys.getenv('PORT')

setwd('/app')

rmarkdown::run(
  'Coronavirus.Rmd'
  , shiny_args = list(
    host = '0.0.0.0',
    port = as.numeric(port)
  )
)