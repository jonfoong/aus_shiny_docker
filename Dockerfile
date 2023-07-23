# Base image https://hub.docker.com/u/oliverstatworx/
FROM oliverstatworx/base-r-tidyverse:latest
## create directories
RUN mkdir -p /code
RUN mkdir -p /output
RUN mkdir -p /token
## copy files
COPY /code/install_packages.R /code/install_packages.R
COPY /code/scrape_data.R /code/scrape_data.R
COPY /token/token.json /token/token.json
## run the script
RUN Rscript /code/install_packages.R
CMD Rscript /code/scrape_data.R