FROM rhub/r-minimal@sha256:f17af8b07fce2802092cdbbeab7975116ac759ae0c714d1a357ffe65c257f984

RUN apk update && \
    apk add --no-cache curl-dev tzdata

ENV TZDIR=/usr/share/zoneinfo

RUN installr -d shiny bslib shinyjs stringdist purrr dplyr stringr magrittr data.table readxl openxlsx httr slackr listviewer reactR googlesheets4

COPY . /usr/local/src/myscripts
WORKDIR /usr/local/src/myscripts

EXPOSE 8888

CMD ["R", "-e", "shiny::runApp('.')"]