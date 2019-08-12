
FROM rocker/tidyverse

RUN apt update
RUN apt install -y libpq-dev
RUN Rscript -e "install.packages(c('RPostgreSQL','yaml','readxl'))"

RUN mkdir /var/script
RUN mkdir /var/data
RUN mkdir /var/config
RUN mkdir /var/logs

ADD R/* /var/script/

CMD Rscript /var/script/custodian.R
