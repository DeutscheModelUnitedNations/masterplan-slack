FROM rhub/r-minimal@sha256:70615c70eaf55354c39f10b1b76a474566851254442e95129fc4125bcf3b3316


RUN apk update
RUN apk add curl-dev
#RUN apk-get update && apk-get install -y --no-install-recommends \
#    libcurl4-gnutls-dev \
#    libssl-dev \
#    && rm -rf /var/lib/apl/lists/*
RUN installr -d shiny purrr dplyr stringr magrittr data.table readxl openxlsx httr slackr listviewer reactR googlesheets4

COPY . /usr/local/src/myscripts
WORKDIR /usr/local/src/myscripts

EXPOSE 8888

CMD ["R", "-e", "shiny::runApp('.')"]