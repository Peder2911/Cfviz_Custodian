
FROM rocker/tidyverse

ENV TGT_DB="shiny"

RUN apt update
RUN apt install -y libpq-dev
RUN Rscript -e "install.packages(c('RPostgreSQL','readxl'))"

RUN mkdir /var/script
RUN mkdir /var/data

ADD R/* /var/script/

CMD Rscript /var/script/custodian.R
