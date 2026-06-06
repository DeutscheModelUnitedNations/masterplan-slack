###Code von Maximilian Ilzhöfer für DMUN e.V.
###Versenden von Slack-Nachrichten aus einem standartisierten Masterplan

options(shiny.host = "0.0.0.0")
options(shiny.port = 8888)

#Clear all Variables
rm(list = ls(all.names=TRUE))

library(shiny)
library(bslib)
library(shinyjs)
library(stringdist)
#library(tidyverse)
library(purrr)
library(dplyr)
library(stringr)
library(magrittr)
library(data.table)
library(readxl)
library(openxlsx)
library(slackr)
library(listviewer)
library(reactR)
library(httr)
library(googlesheets4)
library(jsonlite)


#Informationen zur Google-Auth
googleauth.email<-Sys.getenv("GOOGLEAUTH_EMAIL")
googleauth.secret<-Sys.getenv("GOOGLEAUTH_SECRET")

##Alles pro ws
#Token zum anmelden
env.token.munbw<-Sys.getenv("SLACK_TOKEN_MUNBW")
env.token.munbb<-Sys.getenv("SLACK_TOKEN_MUNBB")
env.token.munsh<-Sys.getenv("SLACK_TOKEN_MUNSH")
env.token.dmun<-Sys.getenv("SLACK_TOKEN_DMUN")

#Channel ID fürs testen
test.channel.munbw<-Sys.getenv("TEST_CHANNEL_MUNBW")
test.channel.munbb<-Sys.getenv("TEST_CHANNEL_MUNBB")
test.channel.munsh<-Sys.getenv("TEST_CHANNEL_MUNSH")
test.channel.dmun<-Sys.getenv("TEST_CHANNEL_DMUN")

#Default Google Sheet
default.sheet.munbw<-Sys.getenv("DEFAULT_SHEET_MUNBW")
default.sheet.munbb<-Sys.getenv("DEFAULT_SHEET_MUNBB")
default.sheet.munsh<-Sys.getenv("DEFAULT_SHEET_MUNSH")
default.sheet.dmun<-Sys.getenv("DEFAULT_SHEET_DMUN")


#Initialer Login zu MUNBW-Slack
slackr_setup(
  channel = "#general",
  token = env.token.munbw,
  config_file = "~/.slackr",
  echo = TRUE,
  cache_dir = ""
)

#Initialprojekt
current.project<-"MUNBW"
test.channel<-test.channel.munbw

#Hier werden die Infos zum Google-Service Login in ein JSON gepackt
google_auth_json<-toJSON(list(type= "service_account",
                              private_key=googleauth.secret,
                              client_email=googleauth.email,
                              token_uri="https://oauth2.googleapis.com/token"), 
                         auto_unbox = TRUE)
#Um hier tatsächlich den Aufruf zu machen
gs4_auth(path = google_auth_json)



#Datatable aufbauen würs wechseln von Workspaces
workspace.table<-data.table(workspace=as.character(c("MUNBW","MUNBB","MUN-SH","DMUN")),slack.token=as.character(c(env.token.munbw,env.token.munbb,env.token.munsh,env.token.dmun)),test.channel=as.character(c(test.channel.munbw,test.channel.munbb,test.channel.munsh,test.channel.dmun)),default.sheet=as.character(c(default.sheet.munbw,default.sheet.munbb,default.sheet.munsh,default.sheet.dmun)))

#Initialer download der Infos
channels<-slackr_channels()
user_orig<-slackr_users()
user<-as.data.table(user_orig)
user%<>%filter(deleted==FALSE)
age.slack<-format(Sys.time(), format = "%A, %d. %B %Y %H:%M:%S")

# Hilfsfunktionen ------------------------------------------------------------
# Namens-Matching: normalisiert (Kleinschreibung, Umlaute, Diakritika, Satzzeichen),
# vergleicht token-sortiert per Jaro-Winkler und beruecksichtigt Teilmengen
# (z. B. fehlende Mittelnamen / Initialen). Robuster als reines adist().
normalize_name <- function(x) {
  x <- as.character(x); x[is.na(x)] <- ""
  # Code-Point-basiert -> komplett locale-unabhaengig (wichtig fuer Alpine/C-Locale).
  # Faltet Gross->Klein, Umlaute/ss, gaengige Diakritika; alles andere -> Leerzeichen.
  fold_one <- function(s) {
    cps <- utf8ToInt(enc2utf8(s))
    if (length(cps) == 0) return("")
    pieces <- vapply(cps, function(cp) {
      if (cp >= 65 && cp <= 90)  return(intToUtf8(cp + 32))
      if (cp >= 97 && cp <= 122) return(intToUtf8(cp))
      if (cp >= 48 && cp <= 57)  return(intToUtf8(cp))
      switch(as.character(cp),
             "228"="ae","246"="oe","252"="ue","223"="ss",
             "196"="ae","214"="oe","220"="ue",
             "233"="e","232"="e","234"="e","235"="e",
             "225"="a","224"="a","226"="a","227"="a","229"="a",
             "237"="i","236"="i","238"="i","239"="i","305"="i",
             "243"="o","242"="o","244"="o","245"="o","248"="o",
             "250"="u","249"="u","251"="u",
             "241"="n","231"="c","253"="y","255"="y",
             " ")
    }, character(1))
    paste(pieces, collapse = "")
  }
  out <- vapply(x, fold_one, character(1), USE.NAMES = FALSE)
  out <- gsub("\\s+", " ", out)
  trimws(out)
}
token_sort <- function(x) {
  toks <- strsplit(x, " ", fixed = TRUE)[[1]]
  toks <- toks[toks != ""]
  paste(sort(toks), collapse = " ")
}
name_similarity <- function(a, b) {
  na <- normalize_name(a); nb <- normalize_name(b)
  if (na == "" || nb == "") return(0)
  ta <- strsplit(na, " ", fixed = TRUE)[[1]]; ta <- ta[ta != ""]
  tb <- strsplit(nb, " ", fixed = TRUE)[[1]]; tb <- tb[tb != ""]
  jw <- stringdist::stringsim(token_sort(na), token_sort(nb), method = "jw", p = 0.1)
  inter <- length(intersect(ta, tb))
  setsim <- if (length(ta) && length(tb)) inter / max(length(ta), length(tb)) else 0
  max(jw, setsim)
}
best_match <- function(name, candidates) {
  candidates <- as.character(candidates)
  if (length(candidates) == 0) return(list(name = NA_character_, score = 0))
  qn <- normalize_name(name); qs <- token_sort(qn)
  qt <- strsplit(qn, " ", fixed = TRUE)[[1]]; qt <- qt[qt != ""]
  cn <- normalize_name(candidates)               # Kandidaten nur einmal normalisieren
  sc <- vapply(seq_along(candidates), function(k) {
    nb <- cn[k]
    if (qn == "" || nb == "") return(0)
    tb <- strsplit(nb, " ", fixed = TRUE)[[1]]; tb <- tb[tb != ""]
    jw <- stringdist::stringsim(qs, token_sort(nb), method = "jw", p = 0.1)
    inter <- length(intersect(qt, tb))
    setsim <- if (length(qt) && length(tb)) inter / max(length(qt), length(tb)) else 0
    max(jw, setsim)
  }, numeric(1))
  j <- which.max(sc)
  list(name = candidates[j], score = unname(sc[j]))
}
match_quality <- function(score) {
  if (is.na(score)) return("poor")
  if (score >= 0.90) "good" else if (score >= 0.75) "weak" else "poor"
}
# Null/Leer-Default
or_default <- function(a, b) {
  if (is.null(a) || length(a) == 0) return(b)
  if (length(a) == 1 && (is.na(a) || a == "")) return(b)
  a
}

# Define UI -------------------------------------------------------------------
# bslib re-skin. ALL input/output IDs are preserved exactly so the server below
# is untouched. Server-side modalDialogs inherit this theme automatically.

dmun_theme <- bs_theme(
  version = 5,
  primary   = "#2563A8",   # DMUN blue (from logo swoosh)
  secondary = "#5B9BD5",
  success   = "#2E8B57",
  base_font = font_collection("Segoe UI", "system-ui", "-apple-system",
                              "Helvetica Neue", "Arial", "sans-serif"),
  "border-radius"      = "0.65rem",
  "card-border-color"  = "rgba(0,0,0,.06)",
  "card-cap-bg"        = "rgba(37,110,168,.06)"
)

# small helper for the numbered step badges in card headers
step_header <- function(n, title, subtitle = NULL) {
  div(
    class = "d-flex align-items-center gap-2",
    span(
      class = "d-inline-flex justify-content-center align-items-center fw-bold",
      style = paste0(
        "width:26px;height:26px;border-radius:50%;",
        "background:#2563A8;color:#fff;font-size:.85rem;flex:0 0 auto;"),
      n
    ),
    div(
      span(class = "fw-semibold", title),
      if (!is.null(subtitle)) div(class = "text-muted small", subtitle)
    )
  )
}

ui <- page_sidebar(
  theme  = dmun_theme,
  window_title = "DMUN TMK-Bot",
  fillable = FALSE,

  useShinyjs(),

  tags$head(
    tags$link(rel = "shortcut icon", href = "favicon.ico"),
    tags$style(HTML("
      .tmk-status { white-space:pre-line; line-height:1.4; }
      .card-header { font-weight:600; }
      .tmk-result:empty { display:none; }
      /* selectize dropdowns are re-parented to <body>; keep them above cards */
      .selectize-dropdown { z-index: 3000 !important; }
      .tmk-msg { white-space: pre-wrap; font-family: SFMono-Regular,Consolas,Menlo,monospace; font-size:.8rem; background:#fbfcfe; border:1px solid rgba(0,0,0,.08); border-radius:.45rem; padding:10px 12px; margin:.6rem 0 0; max-height:340px; overflow:auto; }
      .tmk-badges .badge { font-weight:600; }
      .tmk-dot { display:inline-block; width:.6rem; height:.6rem; border-radius:50%; background:#9aa4b2; flex:0 0 auto; }
      .tmk-dot.on { background:#2e9e5b; box-shadow:0 0 0 .18rem rgba(46,158,91,.18); }
      .tmk-conn { line-height:1.4; }
    ")),
    tags$script(HTML("
      // Race-Fix: Beim Wechsel des Blattes blockt R waehrend des Einlesens.
      // Der Client zeigt solange die ALTEN Spalten. Deshalb sperren wir den
      // Namens-Dropdown sofort clientseitig (vor dem Server-Roundtrip) und
      // geben ihn erst frei, wenn der Server die neuen Spalten gemeldet hat.
      $(document).on('shiny:inputchanged', function(e){
        if (e.name === 'excel.sheet') {
          var el = document.getElementById('first.name');
          if (el && el.selectize) { el.selectize.disable(); }
        }
      });
      Shiny.addCustomMessageHandler('tmk_firstname_ready', function(x){
        var el = document.getElementById('first.name');
        if (el && el.selectize) { el.selectize.enable(); }
      });
    "))
  ),

  # ---- Title bar -----------------------------------------------------------
  title = div(
    class = "d-flex align-items-center justify-content-between w-100",
    div(
      class = "d-flex align-items-center gap-3",
      img(src = "dmunlogo.png", height = "34px"),
      div(
        div(class = "fw-bold", style = "font-size:1.15rem;line-height:1.1;",
            "TMK-Bot"),
        div(class = "text-muted small",
            "Personalisierte Zeitpl\u00e4ne \u00fcber Slack versenden")
      )
    ),
    input_dark_mode(id = "dark_mode", mode = "light")
  ),

  # ---- Sidebar: setup ------------------------------------------------------
  sidebar = sidebar(
    width = 360,
    title = "Konfiguration",
    padding = c(12, 14),

    # Step 1 - connection
    card(
      card_header(step_header("1", "Verbindung")),
      card_body(
        uiOutput("slack.status"),
        div(
          class = "d-grid gap-2",
          actionButton("change_ws", "Workspace wechseln",
                       icon = icon("right-left"),
                       class = "btn-outline-primary btn-sm"),
          actionButton("google_ws", "Google-Tools",
                       icon = icon("table"),
                       class = "btn-outline-secondary btn-sm"),
          actionButton("refresh_slack", "Slack-Daten aktualisieren",
                       icon = icon("rotate"),
                       class = "btn-outline-secondary btn-sm")
        )
      )
    ),

    # Step 2 - data source
    card(
      card_header(step_header("2", "Datenquelle")),
      card_body(
        fileInput("orig.file", label = "Masterplan (.xlsx)",
                  multiple = FALSE, accept = ".xlsx",
                  buttonLabel = "Durchsuchen",
                  placeholder = "Masterplan hochladen"),
        helpText("Alternativ \u00fcber \"Google-Tools\" ein Sheet anbinden."),
        # selectizeInput (not selectInput) so we can pass dropdownParent = "body".
        # updateSelectInput() in the server still targets these correctly.
        selectizeInput("excel.sheet", label = "Blatt w\u00e4hlen",
                       choices = "Bitte zuerst Excel hochladen",
                       options = list(dropdownParent = "body")),
        selectizeInput("first.name", label = "Erste Personen-Spalte",
                       choices = "Bitte zuerst Excel hochladen",
                       options = list(dropdownParent = "body"))
      )
    )
  ),

  # ---- Main: build, review, send ------------------------------------------
  layout_columns(
    col_widths = 12,
    gap = "1rem",

    # Step 3 - build + preview
    card(
      card_header(
        step_header("3", "Nachrichten erstellen & pr\u00fcfen",
                    "Treffer pr\u00fcfen, Empf\u00e4nger ggf. korrigieren, dann versenden.")
      ),
      card_body(
        min_height = "540px",
        div(
          class = "d-flex justify-content-between align-items-center mb-3 flex-wrap gap-2",
          div(
            class = "d-flex align-items-center gap-3 flex-wrap",
            shinyjs::disabled(
              actionButton("exclude_empty", "Leere Zeitpl\u00e4ne ausschlie\u00dfen",
                           icon = icon("eraser"),
                           class = "btn-outline-secondary btn-sm")
            )
          ),
          shinyjs::disabled(
            actionButton("build.messages", "Nachrichten erstellen",
                         icon = icon("wand-magic-sparkles"),
                         class = "btn-primary")
          )
        ),
        uiOutput("messages_ui")
      )
    ),

    # Step 4 - send
    card(
      card_header(step_header("4", "Versenden")),
      card_body(
        layout_columns(
          col_widths = c(7, 5),
          class = "align-items-center",
          div(
            checkboxInput("test", "An Test-Channel senden (kein Versand an echte User)",
                          value = TRUE),
            div(class = "text-muted small",
                "Test-Schalter zuerst ausschalten, um wirklich zu versenden.")
          ),
          div(
            class = "d-grid",
            shinyjs::disabled(
              actionButton("slack.send", "Nachrichten versenden",
                           icon = icon("paper-plane"),
                           class = "btn-success btn-lg")
            )
          )
        ),
        div(class = "tmk-result alert alert-info mt-3 mb-0",
            role = "status",
            textOutput("slack.output"))
      )
    )
  )
)

server <- function(input, output, session) {
  ##Hier werden 2 Variablen definiert, welche global geändert werden können
  #Die für den Text
  variables<-reactiveValues()
  #Der für die Output-Message
  slack.output.message<-reactiveValues(msg=NULL)
  #Und für den Status-Text
  slack.status.message<-reactiveValues(msg=NULL)
  #Und für den Status-Text
  google.status.message<-reactiveValues(msg=NULL)
  #Und für den Status-Text
  mapping.status.message<-reactiveValues(msg=NULL)
  #init
  input.google<-reactiveValues(sheet=NULL)

  # --- UI-Gating: Buttons erst freischalten, wenn der vorherige Schritt erledigt ist
  # "Nachrichten erstellen": erst aktiv, wenn eine Datenquelle geladen und die
  # erste Personen-Spalte gewaehlt wurde (first.name ist dann keine Platzhalter-Zeile).
  observe({
    shinyjs::toggleState(
      "build.messages",
      condition = isTruthy(input$first.name) &&
        input$first.name != "Bitte zuerst Excel hochladen"
    )
  })
  # "Nachrichten versenden": erst aktiv, wenn Nachrichten erzeugt wurden.
  observe({
    shinyjs::toggleState(
      "slack.send",
      condition = !is_empty(variables$final.list)
    )
  })
  
  #Hier wird die Slack-Status-Nachricht erstellt
  slack.status<-function(){
    cur.status<-auth_test()
    list(
      connected = isTRUE(cur.status$ok),
      team      = if (isTRUE(cur.status$ok)) cur.status$team else NA_character_,
      user      = if (isTRUE(cur.status$ok)) cur.status$user else NA_character_,
      age       = age.slack
    )
  }
  slack.status.message$msg<-slack.status()
  #Und hier gerendert
  output$slack.status<-renderUI({
    info <- slack.status.message$msg
    if (is.null(info)) return(NULL)
    if (isTRUE(info$connected)) {
      div(
        class = "tmk-conn",
        div(class = "d-flex align-items-center gap-2 mb-1",
            span(class = "tmk-dot on"),
            span(class = "fw-semibold", "Verbunden")),
        div(class = "small",
            "Workspace ", strong(info$team), " · User ", strong(info$user)),
        div(class = "small text-muted",
            paste0("Daten aktualisiert: ", info$age))
      )
    } else {
      div(
        class = "tmk-conn",
        div(class = "d-flex align-items-center gap-2",
            span(class = "tmk-dot off"),
            span(class = "fw-semibold", "Nicht verbunden"))
      )
    }
  })
  
  #Hier ist die Logik zum wechseln des Workspaces hinterlegt
  #zuerst das Pop-Up
  WS_Popup <- function() {
    modalDialog(
      title = "Wähle den Workspace aus",
      actionButton("change_dmun", "DMUN"),
      actionButton("change_munbb", "MUNBB"),
      actionButton("change_munbw", "MUNBW"),
      actionButton("change_munsh", "MUN-SH"),
      
      footer = tagList(
        modalButton("Abbrechen")
      ),
      easyClose = TRUE
    )
  }
  # Zeige das Pop-Up wenn der Button genutzt wird
  observeEvent(input$change_ws, {
    showModal(WS_Popup())
  })
  ##Funktion für WS-Wechsel
  change.ws<-function(ws=""){
    #Holt Infos aus Tabelle
    cur.ws.change<-workspace.table[workspace==ws]
    #Macht den Wechsel. In einem Try um den Connection-Fehler zu unterdrücken
    try(
      slackr_setup(
        channel = "#general",
        token = cur.ws.change$slack.token,
        config_file = "~/.slackr",
        echo = TRUE,
        cache_dir = ""
      ),silent = TRUE
    )
    #Jetzt noch variablen ändern
    test.channel<<-cur.ws.change$test.channel
    current.project<<-ws
    #Auch noch die Abfragen wieder machen aber nur, wenn wir auch authentifiziert sind
    
    cur.status<-auth_test()
    if(cur.status$ok==TRUE){
      channels<<-slackr_channels()
      user_orig<<-slackr_users()
      user<<-as.data.table(user_orig)
      user<<-user%>%filter(deleted==FALSE)
      age.slack<<-format(Sys.time(), format = "%A, %d. %B %Y %H:%M:%S")
    }
    
    
    #Pop-Up schließen
    removeModal()
    slack.status.message$msg<-slack.status()
  }
  
  
  ##Mach was aus dem klick (Das wiederholt sich für jeden Button)
  observeEvent(input$change_dmun, {
    change.ws(ws="DMUN")
  })
  observeEvent(input$change_munbw, {
    change.ws(ws="MUNBW")
  })
  observeEvent(input$change_munsh, {
    change.ws(ws="MUN-SH")
  })
  observeEvent(input$change_munbb, {
    change.ws(ws="MUNBB")
  })
  
  #Hier ist die Logik für das Google-Zeugs
  Google_Popup <- function(default.sheet="") {
    modalDialog(
      title = "Google-Actions",
      textInput("google.sheet.id","Link zum Sheet",value = default.sheet),
      textOutput("google"),
      div(actionButton("use_google_sheet","Als input nutzen"),actionButton("google_sheet_mapping","Sheet Mapping")),
      
      footer = tagList(
        modalButton("Abbrechen")
      ),
      easyClose = FALSE,
      size="l"
    )
  }
  
  # Zeige das Pop-Up wenn der Button genutzt wird
  observeEvent(input$google_ws, {
    showModal(Google_Popup(default.sheet=workspace.table[workspace==current.project,default.sheet]))
  })
  #Hier wird das Google-Sheet heruntergeladen in einem try aufgrund möglicher Fehler
  google.sheet<-reactive({
    
    if(nchar(input$google.sheet.id)>5){
      tryCatch( { result <- gs4_get(input$google.sheet.id); print(result) }
                , error = function(e) {result <<- NULL})
      return(result)
    }
  })
  ##Hier wird die Nachricht im Pop-Up generiert. Sagt eigentlich nur, ob verbindung da ist oder nicht
  output$google<-renderText({
    google.status.message
    sheet<-google.sheet()
    if(is.null(sheet$name)==FALSE){
      paste0("Das eingelesene Sheet heißt ",sheet$name)
    }else{
      paste("Kein Google-Sheet gefunden. Stelle sicher, dass es auch ein Sheet und keine .xlsx ist und dass dmun-slack-bot@dmun-auth.iam.gserviceaccount.com Berechtigungen auf das sheet hat")
    }
  })
  
  #Hier wird das Sheet als input genutzt (wenn der Button genutzt wird)
  observeEvent(input$use_google_sheet,{
    input.google$sheet<<-google.sheet()
    removeModal()
  })
  
  #Hier wird das Sheet für das Mapping (wenn der Button genutzt wird)
  observeEvent(input$google_sheet_mapping,{
    input.google$sheet<<-google.sheet()
    #Gehe direkt ins nächste Pop-Up
    showModal(Google_map_Popup())
  })
  
  
  #Hier ist die Logik für das Google-Mapping-Zeugs
  Google_map_Popup <- function() {
    modalDialog(
      title = "Google-Mapping",
      flowLayout(
        selectInput("map.sheet.sheet",label = "Wähle das Blatt",choices = "Bitte zuerst Excel hochladen" ),
        selectInput("map.map.sheet",label = "Wähle das Mapping-Blatt",choices = "Bitte zuerst Excel hochladen" ),
        selectInput("map.first.name",label = "Was ist der erste Name?",choices = "Bitte zuerst Excel hochladen" ),
        selectInput("map.map.name",label = "Was ist die Mapping-Spalte?",choices = "Bitte zuerst Excel hochladen" )
      ),
      div(actionButton("do_google_sheet_mapping","Mapping durchführen")),
      textOutput("mapping.status"),
      footer = tagList(
        modalButton("Schließen")
      ),
      easyClose = FALSE,
      size="l"
    )
  }
  
  #Hier wird das Mapping im Sheet gemacht
  observeEvent(input$do_google_sheet_mapping,{
    message<-as.character()
    #Hole die Infos aus anderen Funktionen
    main.sheet<-read.sheet.map.all()
    mapping.sheet<-read_sheet(input.google$sheet$spreadsheet_id,sheet = input$map.map.sheet)
    mapping.row.name<-input$map.map.name
    
    #Die Tabelle etwas umstellen und selektieren
    mapping.rows<-main.sheet%>%mutate(rowno=1:nrow(.))%>%relocate(rowno)%>%filter(!!as.symbol(mapping.row.name)!=0)%>%select(-(starts_with("sum")))
    
    #Finde heraus, wo die Namen anfangen
    col.names<-colnames(mapping.rows)
    first.name.col<-match(input$map.first.name,col.names)
    mapping.col<-match(mapping.row.name,col.names)
    col.names<-colnames(main.sheet)
    first.name.sheet.col<-match(input$map.first.name,col.names)
    
    #Init Progress-Bar
    progress <- shiny::Progress$new()
    progress$set(message = "Mapping", value = 0)
    #Gehe jede Reihe durch
    for (i in 1:nrow(mapping.rows)) {
      
      #Da es sein kann, dass mehrere Gruppen gemappt werden sollten wird das hier abgefangen
      cur.mapping.row<-mapping.rows[i,]
      mapping.group<-cur.mapping.row[[mapping.col]]%>%str_split(.,";",n=Inf)%>%unlist()
      #Jetzt für jede Gruppe
      for (j in 1:length(mapping.group)) {
        cur.mapping.group<-mapping.group[j]
        #Wenn Mapping Alle dann einfach bei allen eine 1 setzten
        if (tolower(cur.mapping.group)=="alle"){
          cur.mapping.row[first.name.col:ncol(cur.mapping.row)]<-1
        } else{
          #Welche Personen alle in der Mapping-Gruppe sind
          cur.mapping.people<-mapping.sheet%>%filter_at(1,all_vars(. == cur.mapping.group))%>%select(2)%>%as.character()%>%str_split(.,";",n=Inf)
          #auf 1 Setzten wenn vorhanden
          cur.mapping.row[colnames(cur.mapping.row)%in%cur.mapping.people[[1]]]<-1
        }
        
      }
      
      
      
      #und gleich nach google schreiben
      cur.mapping.write<-cur.mapping.row[first.name.col:ncol(cur.mapping.row)]
      
      progress$inc(1/nrow(mapping.rows), detail = paste0("Mappe nun Reihe ",cur.mapping.row$rowno+1," für Gruppe ",cur.mapping.row[[mapping.col]]))
      range_write(ss=input.google$sheet$spreadsheet_id,data=cur.mapping.write,sheet = input$map.sheet.sheet,range=(cell_limits(c(cur.mapping.row$rowno+1, first.name.sheet.col), c(cur.mapping.row$rowno+1, first.name.sheet.col+ncol(cur.mapping.write)))),col_names = FALSE,reformat = FALSE)
      #Return-Nachricht schreiben
      message<-paste0(message,"Reihe ",cur.mapping.row$rowno+1," für Gruppe ",cur.mapping.row[[mapping.col]]," gemappt ")
      
    }
    #In Variable
    mapping.status.message$msg<<-message
    
    
  })
  
  #Hier wird die Return-Nachricht gerendert
  output$mapping.status<-renderText({
    mapping.status.message$msg
  })
  
  #Hier werden die Sheets ausgelesen, wenn Sie hochgeladen wurden
  observe({
    if(is.null(input.google$sheet)==FALSE){
      list.sheets<-sheet.sheets()
      excel<-read.sheet.all()
      excel<-read.sheet.map.all()
    }
  })
  
  #Hier wird das Google Sheet eingelesen und die Cols werden eingelesen und entsprechend angezeigt
  read.sheet.all<-reactive({
    if(is.null(input$orig.file)==TRUE&&input$excel.sheet!="Bitte zuerst Excel hochladen"){
      read.sheet.all<-range_read(input.google$sheet$spreadsheet_id,sheet = input$excel.sheet,col_types = "c")
      col.names<-colnames(read.sheet.all)
      
      updateSelectInput(
        session = getDefaultReactiveDomain(),
        "first.name",
        label = "Wähle den ersten Namen",
        choices = col.names,
        selected = NULL
      )
      # Spalten stehen -> Namens-Dropdown wieder freigeben (siehe JS im head)
      session$sendCustomMessage("tmk_firstname_ready", TRUE)
      return(read.sheet.all)
    }
    
  })
  
  #Hier wird das Google Sheet eingelesen und die Cols werden eingelesen und entsprechend angezeigt
  read.sheet.map.all<-reactive({
    if(is.null(input$map.sheet.sheet)==FALSE&&input$map.sheet.sheet!="Bitte zuerst Excel hochladen"){
      read.sheet.map.all<-range_read(input.google$sheet$spreadsheet_id,sheet = input$map.sheet.sheet)
      col.map.names<-colnames(read.sheet.map.all)
      updateSelectInput(
        session = getDefaultReactiveDomain(),
        "map.first.name",
        label = "Wähle den ersten Namen",
        choices = col.map.names,
        selected = NULL
      )
      updateSelectInput(
        session = getDefaultReactiveDomain(),
        "map.map.name",
        #label = "Wähle den ersten Namen",
        choices = col.map.names,
        selected = NULL
      )
      return(read.sheet.map.all)
    }
    
  })
  
  
  #Hier werden die Sheets auch in die Auswahl übernommen
  sheet.sheets<-reactive({
    if(is.null(input$orig.file)==TRUE){
      list.sheets<-input.google$sheet$sheets$name
      updateSelectInput(
        session = getDefaultReactiveDomain(),
        "excel.sheet",
        label = "Wähle das Blatt",
        choices = list.sheets,
        selected = NULL
      )
      updateSelectInput(
        session = getDefaultReactiveDomain(),
        "map.sheet.sheet",
        label = "Wähle das Blatt",
        choices = list.sheets,
        selected = NULL
      )
      updateSelectInput(
        session = getDefaultReactiveDomain(),
        "map.map.sheet",
        #label = "Wähle das Blatt",
        choices = list.sheets,
        selected = NULL
      )
      
    }
  })
  
  #Hier wird geschaut, ob ein Excel hochgeladen wurde und die Sheets werden ausgeladen
  observe({
    if(is.null(input$orig.file)==FALSE){
      list.sheets<-excel.sheets()
      excel<-read.excel.all()
    }
  })
  #Hier wird das Sheet eingelesen und die Cols werden eingelesen und entsprechend angezeigt
  read.excel.all<-reactive({
    if(is.null(input$orig.file)==FALSE&&input$excel.sheet!="Bitte zuerst Excel hochladen"){
      read.excel.all<-read.xlsx(input$orig.file$datapath,sheet = input$excel.sheet)
      col.names<-colnames(read.excel.all)
      updateSelectInput(
        session = getDefaultReactiveDomain(),
        "first.name",
        label = "Wähle den ersten Namen",
        choices = col.names,
        selected = NULL
      )
      # Spalten stehen -> Namens-Dropdown wieder freigeben (siehe JS im head)
      session$sendCustomMessage("tmk_firstname_ready", TRUE)
      return(read.excel.all)
    }
    
  })
  #Hier werden die Sheets auch in die Auswahl übernommen
  excel.sheets<-reactive({
    if(is.null(input$orig.file)==FALSE){
      list.sheets<-excel_sheets(input$orig.file$datapath)
      updateSelectInput(
        session = getDefaultReactiveDomain(),
        "excel.sheet",
        label = "Wähle das Blatt",
        choices = list.sheets,
        selected = NULL
      )
      
    }
  })
  #Hier wird ausgegeben, welches die letzte Kopfzeile ist
  last.head.col<-reactive({
    excel<-read.excel.all()
    #Hier wird für die Google-Sheets noch Reihen ausgeschlossen, welche mit den folgenden Keywörtern anfangen. 
    sheet<-read.sheet.all()%>%select(-(starts_with(c("mapp","group","gruppe","sum"))))
    if(is.null(excel)==FALSE){
      col.names<-colnames(excel)
      last.head.col<-match(input$first.name,col.names)-1
      return(last.head.col)
    }
    if(is.null(sheet)==FALSE){
      col.names<-colnames(sheet)
      last.head.col<-match(input$first.name,col.names)-1
      return(last.head.col)
    }
  })
  
  #Hier werden nach dem Button die Nachrichten erzeugt
  build.slack.messages<-observeEvent(input$build.messages,{
    #Init von Variablen
    output.list <- list()
    slack.output.message$msg<-NULL
    #Infos von vorherigen Funktionen holen
    if(is.null(input$orig.file)==FALSE){
      input.masterplan<-read.excel.all()
    } else {
      #Hier wird für die Google-Sheets noch Reihen ausgeschlossen, welche mit den folgenden Keywörtern anfangen.
      input.masterplan<-read.sheet.all()%>%select(-(starts_with(c("mapp","group","gruppe","sum"))))
    }
    
    last.head.col.num<-last.head.col()
    #Kopfzeile generieren
    input.masterplan<-cbind(input.masterplan[1:last.head.col.num],input.masterplan[(last.head.col.num+1):(ncol(input.masterplan))]%>%mutate_all(~replace(., is.na(.), 0)))
    
    
    #Funktion, damit, wenn die Spalte Teaminfo existiert Personen wissen sollen, wer sonst noch die Aufgabe macht)
    if("Team Info"%in%colnames(input.masterplan)){
      team.info<-input.masterplan[,'Team Info']%>%unlist()%>%as.numeric()
      input.masterplan%<>%rename("(Teammitglieder)"="Team Info")
      input.masterplan[,"(Teammitglieder)"]%<>%replace(., is.na(.), 0)
      for (i in 1:nrow(input.masterplan)) {
        cur.row.masterplan<-input.masterplan[i,]
        if(cur.row.masterplan$`(Teammitglieder)`==1){
          cur.team<-cur.row.masterplan %>% select(where(~ isTRUE(.x == 1))) %>% names()
          cur.team<-cur.team[!(cur.team%in%"(Teammitglieder)")]
          input.masterplan[i,"(Teammitglieder)"]<-paste0("(",paste(cur.team,sep = "", collapse = ","),")")
        }
        
      }
      
      input.masterplan[,"(Teammitglieder)"]%<>%replace(., .==0, "")
      
    }
    
    #Jetzt für alle individuell die Liste aufbauen
    for (i in 1:(ncol(input.masterplan)-last.head.col.num)) {
      
      cur.col.id<-i+last.head.col.num
      cur.col<-input.masterplan[c(1:(last.head.col.num),cur.col.id)]
      cur.recipient<-colnames(cur.col)[last.head.col.num+1]
      cur.col.relevant<-cur.col[cur.col[last.head.col.num+1]==1,]
      setDT(cur.col.relevant)
      cur.message<-rbind(as.list(names(cur.col.relevant)),cur.col.relevant)
      #Bestes Slack-Match per normalisiertem Namensvergleich (best_match oben)
      bm<-best_match(cur.recipient, user[,real_name])
      cur.message<-cur.message[,1:last.head.col.num]
      #Kopfzeile nutzt den Namen aus dem Masterplan (unabhaengig vom Slack-Treffer)
      slack.message_head<-capture.output(write.table(c(paste0("Zeitplan für ", cur.recipient),paste0( "Tag: ",input$excel.sheet)),"",quote = FALSE, row.names = FALSE, na="-", sep="\t",append = TRUE, col.names = FALSE))
      slack.message_head<-paste0(slack.message_head,collapse = "\n")
      slack.message_body<-capture.output(write.table(cur.message[,1:last.head.col.num],"",quote = FALSE, row.names = FALSE, na="-", sep="\t",append = TRUE, col.names = FALSE))
      slack.message_body<-paste0(slack.message_body,collapse = "\n")
      output.list[[cur.recipient]]<-list(
        name.excel    = as.character(cur.recipient),
        name.slack    = as.character(bm$name),
        match.score   = bm$score,
        match.quality = match_quality(bm$score),
        n.items       = nrow(cur.col.relevant),
        include       = TRUE,
        message.head  = as.character(slack.message_head),
        message.body  = as.character(slack.message_body)
      )
      
    }
    
    
    variables$final.list<-output.list
    variables$final.list
    return(output.list)
  })
  
  # ---- Nachrichten-Vorschau (ersetzt den frueheren JSON-Editor) ------------
  # Pro Empfaenger: Treffer-Guete, korrigierbarer Slack-Empfaenger, Senden-Schalter
  # und lesbare Vorschau (reagiert auf das Format).
  output$messages_ui <- renderUI({
    fl <- variables$final.list
    if (is_empty(fl)) {
      return(div(class = "text-muted",
                 "Noch keine Nachrichten erstellt. Datenquelle laden und auf \u201eNachrichten erstellen\u201c klicken."))
    }
    all.users <- sort(unique(as.character(user[, real_name])))
    cards <- lapply(seq_along(fl), function(i) {
      e <- fl[[i]]
      # Auswahl beibehalten (isolate), damit ein Re-Render Korrekturen nicht zuruecksetzt
      sel <- or_default(isolate(input[[paste0("slackpick_", i)]]), e$name.slack)
      inc <- isolate(input[[paste0("include_", i)]])
      if (is.null(inc)) inc <- isTRUE(e$include)
      badge <- switch(e$match.quality,
                      good = span(class = "badge bg-success", "Sicher"),
                      weak = span(class = "badge bg-warning text-dark", "Unsicher"),
                      poor = span(class = "badge bg-danger", "Kein guter Treffer"),
                      span(class = "badge bg-secondary", "?"))
      empty.badge <- if (isTRUE(e$n.items == 0))
        span(class = "badge bg-secondary ms-1", "Leerer Zeitplan") else NULL
      preview.txt <- paste(c(e$message.head, e$message.body), collapse = "\n")
      card(
        class = "mb-2",
        card_body(
          div(class = "d-flex justify-content-between align-items-start gap-2 flex-wrap tmk-badges",
              div(
                div(class = "fw-semibold", e$name.excel),
                div(class = "small text-muted",
                    paste0("Bester Treffer: ", e$name.slack,
                           "  \u00b7  ", round(e$match.score * 100), "% \u00c4hnlichkeit"))
              ),
              div(badge, empty.badge)
          ),
          layout_columns(
            col_widths = c(8, 4), class = "mt-2 align-items-end",
            selectizeInput(paste0("slackpick_", i), "Slack-Empf\u00e4nger",
                           choices = all.users, selected = sel,
                           options = list(dropdownParent = "body")),
            div(class = "pb-2",
                checkboxInput(paste0("include_", i), "Senden", value = inc))
          ),
          tags$pre(class = "tmk-msg", preview.txt)
        )
      )
    })
    tagList(cards)
  })

  # "Leere Zeitplaene ausschliessen": Senden-Haken der leeren auf FALSE
  observeEvent(input$exclude_empty, {
    fl <- variables$final.list
    for (i in seq_along(fl)) {
      if (isTRUE(fl[[i]]$n.items == 0)) {
        updateCheckboxInput(session, paste0("include_", i), value = FALSE)
      }
    }
  })
  observe({
    fl <- variables$final.list
    has.empty <- !is_empty(fl) &&
      any(vapply(fl, function(e) isTRUE(e$n.items == 0), logical(1)))
    shinyjs::toggleState("exclude_empty", condition = has.empty)
  })

  # Slack-Daten (User/Channels) neu laden, ohne Workspace zu wechseln
  observeEvent(input$refresh_slack, {
    cur.status <- auth_test()
    if (isTRUE(cur.status$ok)) {
      progress <- shiny::Progress$new()
      progress$set(message = "Slack-Daten werden aktualisiert", value = 0.5)
      try({
        channels  <<- slackr_channels()
        user_orig <<- slackr_users()
        user      <<- as.data.table(user_orig)
        user      <<- user %>% filter(deleted == FALSE)
        age.slack <<- format(Sys.time(), format = "%A, %d. %B %Y %H:%M:%S")
      }, silent = TRUE)
      progress$close()
    }
    slack.status.message$msg <- slack.status()
  })

  # ---- Versand ------------------------------------------------------------
  # Indizes der zu sendenden Eintraege (Senden-Haken aktiv)
  included_idx <- function() {
    fl <- variables$final.list
    if (is_empty(fl)) return(integer(0))
    which(vapply(seq_along(fl), function(i) {
      inc <- input[[paste0("include_", i)]]
      if (is.null(inc)) isTRUE(fl[[i]]$include) else isTRUE(inc)
    }, logical(1)))
  }
  # Aktuell gewaehlter Slack-Empfaenger (Korrektur oder bester Treffer)
  chosen_recipient <- function(i) {
    or_default(input[[paste0("slackpick_", i)]], variables$final.list[[i]]$name.slack)
  }

  # Bestaetigungs-Dialog bei ECHTEM Versand (Test-Schalter aus)
  confirm_send_modal <- function() {
    fl  <- variables$final.list
    idx <- included_idx()
    sub <- fl[idx]
    qual <- vapply(sub, function(e) e$match.quality, character(1))
    g <- sum(qual == "good"); w <- sum(qual == "weak"); p <- sum(qual == "poor")
    empt <- sum(vapply(sub, function(e) isTRUE(e$n.items == 0), logical(1)))
    modalDialog(
      title = "Echte Nachrichten versenden?",
      p(strong(length(idx)), " Nachricht(en) gehen an echte Slack-User \u2013 nicht an den Test-Channel."),
      tags$ul(
        tags$li(paste0(g, " sichere Treffer")),
        tags$li(paste0(w, " unsichere Treffer")),
        tags$li(paste0(p, " ohne guten Treffer")),
        tags$li(paste0(empt, " mit leerem Zeitplan"))
      ),
      if (p > 0) div(class = "alert alert-warning mb-0",
                     "Es gibt Empf\u00e4nger ohne guten Namens-Treffer. Bitte oben pr\u00fcfen \u2013 diese k\u00f6nnten an die falsche Person gehen."),
      footer = tagList(
        modalButton("Abbrechen"),
        actionButton("confirm_send", "Senden best\u00e4tigen", class = "btn-danger")
      ),
      easyClose = FALSE
    )
  }

  # Eigentlicher Versand
  do_send <- function() {
    fl  <- variables$final.list
    idx <- included_idx()
    if (length(idx) == 0) {
      slack.output.message$msg <- "Es sind keine Empf\u00e4nger zum Senden ausgew\u00e4hlt."
      return(invisible(NULL))
    }
    errs <- character(0)
    n.sent <- 0L
    progress <- shiny::Progress$new()
    progress$set(message = "Sende Nachricht", value = 0)
    on.exit(progress$close())
    for (k in seq_along(idx)) {
      i <- idx[k]
      cur.entry <- fl[[i]]
      chosen <- chosen_recipient(i)
      slack.user.id <- user[real_name == chosen, id][1]
      if (isTRUE(input$test)) slack.user.id <- test.channel
      slack.message <- c(cur.entry$message.head, cur.entry$message.body)
      progress$inc(1 / length(idx),
                   detail = paste0("Verschicke ", k, " von ", length(idx), " f\u00fcr ", chosen))
      cur.payload <- slackr_msg(txt = slack.message, channel = slack.user.id, as_user = TRUE)
      n.sent <- n.sent + 1L
      if (!isTRUE(cur.payload[['ok']])) errs <- c(errs, chosen)
    }
    # Lokale Zeit (Europe/Berlin) statt externer API
    cur.time <- format(Sys.time(), "%H:%M:%S", tz = "Europe/Berlin")
    if (length(errs) > 0) {
      slack.output.message$msg <- paste0(
        "Es gab gerade ", cur.time, " einen Fehler beim Versenden von ", length(errs),
        " Nachricht(en) (", paste(errs, collapse = ", "), "). Die \u00fcbrigen ",
        n.sent - length(errs), " wurden versendet.")
    } else if (cur.time > "23:00:00" || cur.time < "06:00:00") {
      slack.output.message$msg <- paste0(
        "Herzlichen Gl\u00fcckwunsch, es wurden nun (", cur.time, ") ", n.sent,
        " Nachrichten verschickt. Jetzt darfst du beruhigt ins Bett gehen :)")
    } else if (cur.time > "20:00:00") {
      slack.output.message$msg <- paste0(
        "Herzlichen Gl\u00fcckwunsch, es wurden nun (", cur.time, ") ", n.sent,
        " Nachrichten verschickt. Jetzt hast du dir ein Bier verdient.")
    } else {
      slack.output.message$msg <- paste0(
        "Herzlichen Gl\u00fcckwunsch, es wurden nun (", cur.time, ") ", n.sent,
        " Nachrichten verschickt.")
    }
  }

  # Klick auf "Versenden": Test -> direkt, echt -> erst Bestaetigung
  observeEvent(input$slack.send, {
    if (isTRUE(input$test)) {
      do_send()
    } else {
      showModal(confirm_send_modal())
    }
  })
  observeEvent(input$confirm_send, {
    removeModal()
    do_send()
  })

  #Hier wird die Return-Nachricht gerendert
  output$slack.output<-renderText({
    slack.output.message$msg
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
