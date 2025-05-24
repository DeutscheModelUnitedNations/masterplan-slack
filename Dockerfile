FROM rhub/r-minimal@sha256:4d464d12989b9259186f1d05a96629a9a4757d4c033a3e8da8fd22d1926b826d


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