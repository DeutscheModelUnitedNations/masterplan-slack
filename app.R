###Code von Maximilian Ilzhöfer für DMUN e.V.
###Versenden von Slack-Nachrichten aus einem standartisierten Masterplan

options(shiny.host = "0.0.0.0")
options(shiny.port = 8888)

#Clear all Variables
rm(list = ls(all.names=TRUE))

library(shiny)
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

# Define UI
ui <- fluidPage(
  
  tags$head(tags$link(rel = "shortcut icon", href = "favicon.ico")),
  img(src='dmunlogo.png', style = "float:right;margin:10px 0px"),
  # Application title
  titlePanel("DMUN TMK-Bot"),
  verbatimTextOutput("slack.status"),
  actionButton("change_ws", "Workspace wechseln"),
  actionButton("google_ws", "Google-Tools"),
  flowLayout(    fileInput("orig.file",label = "Masterplan",multiple = FALSE,accept = ".xlsx",buttonLabel = "Browse",placeholder = "Bitte hier den Masterplan hochladen"),
                 selectInput("excel.sheet",label = "Wähle das Blatt",choices = "Bitte zuerst Excel hochladen" ),
                 selectInput("first.name",label = "Was ist der erste Name?",choices = "Bitte zuerst Excel hochladen" )),
  actionButton("build.messages", "Erstelle die Nachrichten",width = "210px"),
  reactjsonOutput( "final.list" , height = "80%"),
  flowLayout(actionButton("slack.send", "Versende die Nachrichten",width = "210px"),
             checkboxInput("test","Nachricht geht an Test-Channel",TRUE,width = "250px")),
  textOutput("slack.output"),
  
  
  
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
  
  #Hier wird die Slack-Status-Nachricht erstellt
  slack.status<-function(){
    cur.status<-auth_test()
    if(cur.status$ok==TRUE){
      return<-paste0("Derzeit ist eine Verbindung zum Slack-Workspace ",cur.status$team," mit dem User ",cur.status$user," hergestellt.\nAlter der Slack-Daten: ",age.slack)
    }else{
      return<-paste("Derzeit ist keine Verbindung zu Slack hergestellt")
    }
    return(return)
  }
  slack.status.message$msg<-slack.status()
  #Und hier gerendert
  output$slack.status<-renderText({
    
    slack.status.message$msg
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
      read.sheet.all<-range_read(input.google$sheet$spreadsheet_id,sheet = input$excel.sheet)
      col.names<-colnames(read.sheet.all)
      
      updateSelectInput(
        session = getDefaultReactiveDomain(),
        "first.name",
        label = "Wähle den ersten Namen",
        choices = col.names,
        selected = NULL
      )
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
          cur.team<-colnames(cur.row.masterplan)[as.logical(unlist(cur.row.masterplan)==1)]
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
      distance<-adist(cur.recipient,user[,real_name])
      pos.minimum<-which.min(distance)
      likly.slack.user<-user[pos.minimum]
      cur.message<-cur.message[,1:last.head.col.num]
      slack.message_head<-capture.output(write.table(c(paste0("Zeitplan für ", likly.slack.user$real_name),paste0( "Tag: ",input$excel.sheet)),"",quote = FALSE, row.names = FALSE, na="-", sep="\t",append = TRUE, col.names = FALSE))
      slack.message_head<-paste0(slack.message_head,collapse = "\n")
      slack.message_body<-capture.output(write.table(cur.message[,1:last.head.col.num],"",quote = FALSE, row.names = FALSE, na="-", sep="\t",append = TRUE, col.names = FALSE))
      slack.message_body<-paste0(slack.message_body,collapse = "\n")
      output.list[[cur.recipient]]<-list(name.excel=as.character(cur.recipient),name.slack=as.character(likly.slack.user$real_name),message.head=as.character(slack.message_head),message.body=as.character(slack.message_body))
      
    }
    
    
    variables$final.list<-output.list
    variables$final.list
    return(output.list)
  })
  
  #Hier wird die Json-Liste gerendert
  output$final.list <- renderReactjson({
    variables$final.list
    if(is_empty(variables$final.list)==FALSE){
      reactjson( as.list(variables$final.list) )
    }
    
    
  })
  ##Der hier ist verantwortlich, dass die Änderungen im Json auch zurückgeschrieben werden. 
  observeEvent(input$final.list_edit, {
    str(input$final.list_edit, max.level=2)
    variables$final.list<-input$final.list_edit$value$updated_src
  })
  ##Der hier ist verantwortlich, dass die Löschungen im Json auch zurückgeschrieben werden. 
  observeEvent(input$final.list_delete, {
    str(input$final.list_delete, max.level=2)
    variables$final.list<-input$final.list_delete$value$updated_src
  })
  #Der hier verschickt die Liste nach dem Button
  build.slack.messages<-observeEvent(input$slack.send,{
    #Init Variablen
    payload<-list()
    err.payload<-list()
    err.payload.name<-NULL
    #Init Progress-Bar
    progress <- shiny::Progress$new()
    progress$set(message = "Sende Nachricht", value = 0)
    edited.list<-variables$final.list
    for (i in 1:length(edited.list)) {
      cur.entry<-edited.list[[i]]
      
      slack.user.id<-user[real_name==cur.entry$name.slack,id]
      #hier wird die User-Id durch die am anfang definierte überschrieben, wenn der Button auf True ist
      if(input$test==TRUE){
        slack.user.id<-test.channel
      }
      
      slack.message<-c(cur.entry$message.head,cur.entry$message.body)
      #im Payload wird einfach die Rückmeldung von Slack gespeichert
      progress$inc(1/length(edited.list), detail = paste0("Verschicke nun Nachricht ",i," von ",length(edited.list), " für ", cur.entry$name.slack))
      payload[[cur.entry$name.slack]]<-slackr_msg(
        txt = slack.message,
        channel = slack.user.id,
        as_user=TRUE
      )
      cur.payload<-payload[[cur.entry$name.slack]]
      #Hier wird geschaut, ob es Fehler beim Senden gab
      if(cur.payload[['ok']]==FALSE){
        err.payload[names(payload)[i]]<-list(payload[[i]])
        
        err.payload.name<-c(err.payload.name,names(payload)[i])
      }
    }
    #Hier wird die Return-Nachricht generiert
    cur.time<-format(Sys.time(),"%H:%M:%S")
    #Hole die aktuelle Zeit von einer API, weil die Serverzeit ist nicht relevant
    cur.time.API<-content(GET("https://timeapi.io/api/Time/current/zone?timeZone=Europe/Berlin"))
    cur.time<-substr(cur.time.API$dateTime,12,19)
    
    if(length(err.payload)>0){
      slack.output.message$msg<-paste0("Es gab gerade ", cur.time ," ein Fehler beim Versenden von ",length(err.payload) ," Nachrichten von ", paste(err.payload.name,sep = " ",collapse = ","), " die restlichen ", length(payload)-length(err.payload)," Nachrichten wurden versendet")
    }else{
      
      if(cur.time>"23:00:00"||cur.time<"06:00:00"){
        slack.output.message$msg<-paste0("Herzlichen Glückwunsch es wurden nun (",cur.time,") ",length(payload)," Nachrichten verschickt. Jetzt darfst du beruhigt ins Bett gehen :)")
      }else if (cur.time>"20:00:00"){
        slack.output.message$msg<-paste0("Herzlichen Glückwunsch es wurden nun (",cur.time,") ",length(payload)," Nachrichten verschickt. Jetzt hast du dir ein Bier verdient.")
      }else{
        slack.output.message$msg<-paste0("Herzlichen Glückwunsch es wurden nun (",cur.time,") ",length(payload)," Nachrichten verschickt.")
      }
      
    }
    
    
  })
  #Hier wird die Return-Nachricht gerendert
  output$slack.output<-renderText({
    slack.output.message$msg
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
