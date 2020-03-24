# init.R
# MIT License
#

my_packages = c(
    "rmarkdown", "flexdashboard", "tidyverse",
    "plotly", "shiny", "DT", "devtools", "rAmCharts",
    "bsplus", "shiny.i18n", "leaflet", "xml2",
    "sp", "rworldmap", "rgdal"
    )


install_if_missing = function(p) {
  if (p %in% rownames(installed.packages()) == FALSE) {
    install.packages(p)
  }
}

invisible(sapply(my_packages, install_if_missing))
# install pool from Github
invisible(devtools::install_github("rstudio/pool"))