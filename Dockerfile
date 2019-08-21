
FROM rocker/tidyverse

ENV TGT_DB="shiny"

RUN apt update
RUN apt install -y libpq-dev
RUN Rscript -e "install.packages(c('RPostgreSQL','readxl'))"

RUN mkdir /var/data

ADD R/* /

CMD Rscript /custodian.R
