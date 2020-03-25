FROM openanalytics/r-shiny

MAINTAINER Loic Rakotoson "contact@loicrakotoson.com"
 
# Install dependency libraries
RUN apt-get update && apt-get install -y  \
            libxml2-dev \
            libudunits2-dev \
            libssh2-1-dev \
            libcurl4-openssl-dev \
            libsasl2-dev \
            libv8-dev\
            # following libs not installed as no Database connection needed
            #libmariadbd-dev \  
            #libmariadb-client-lgpl-dev \
            #unixodbc-dev \
            #libpq-dev \
            && rm -rf /var/lib/apt/lists/*
	
RUN sudo apt-get update && sudo apt-get install -y libgdal-dev libproj-dev
# install needed R packages
RUN    R -e "install.packages(c('tidyverse', 'flexdashboard', 'knitr', 'plotly', 'shiny', 'rAmCharts', 'bsplus', 'shiny.i18n', 'leaflet', 'xml2', 'sp', 'rworldmap', 'rgdal'), dependencies = TRUE, repo='http://cran.r-project.org')"

# make directory and copy Rmarkdown flexdashboard file in it
RUN mkdir -p /bin
COPY Coronavirus.Rmd    /bin/Coronavirus.Rmd
COPY scripts /bin/scripts
COPY translation /bin/translation
COPY www /bin/www

RUN mkdir -p /opt/shinyproxy/
RUN wget https://www.shinyproxy.io/downloads/shinyproxy-2.3.0.jar -O /opt/shinyproxy/shinyproxy.jar
COPY application.yml /opt/shinyproxy/application.yml

RUN chmod -R 755 /opt/shinyproxy/
RUN useradd shinyproxy -u 1000 -d /opt/shinyproxy && chown -R shinyproxy /opt/shinyproxy/
WORKDIR /opt/shinyproxy/config

# make all app files readable (solves issue when dev in Windows, but building in Ubuntu)
RUN chmod -R 755 /bin

# expose port on Docker container
EXPOSE 3838

# run flexdashboard as localhost and on exposed port in Docker container
CMD ["java", "-jar", "/opt/shinyproxy/shinyproxy.jar"]
#CMD ["R", "-e", "rmarkdown::run('/bin/Coronavirus.Rmd', shiny_args = list(port = 3838, host = '0.0.0.0'))"]