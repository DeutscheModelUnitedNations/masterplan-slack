FROM r-base@sha256:0563681bc4b3a1ad6faaa767f8e8b0c2bfb818f921840c032fc9510058b345bb

RUN apt-get update && apt-get install -y --no-install-recommends \
    libcurl4-gnutls-dev \
    libssl-dev \
    && rm -rf /var/lib/apt/lists/*
RUN install.r utils stat shiny purrr dplyr stringr magrittr data.table readxl openxlsx httr slackr listviewer reactR googlesheets4

COPY . /usr/local/src/myscripts
WORKDIR /usr/local/src/myscripts

EXPOSE 8888

CMD ["R", "-e", "shiny::runApp('.')"]