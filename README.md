# TMK-Bot – Personalisierte Zeitpläne über Slack

Eine R-/Shiny-Anwendung für DMUN e.V., die aus einem zentralen **Masterplan**
für jede Person einen **individuellen Zeitplan** erzeugt und ihn per
Slack-Direktnachricht verschickt. Statt jeder/jedem Teilnehmenden manuell den
passenden Ausschnitt zu schicken, lädt man eine Tabelle hoch (oder bindet ein
Google Sheet an), prüft die Treffer und versendet alles in einem Durchgang.

---

## Was die App macht

1. **Masterplan einlesen** – als hochgeladene `.xlsx` oder direkt aus einem
   Google Sheet.
2. **Blatt und erste Personen-Spalte wählen** – damit weiß die App, welche
   Spalten zur Programm-Beschreibung gehören und ab welcher Spalte die
   einzelnen Personen beginnen.
3. **Pro Person einen Zeitplan bauen** – aus allen Programmzeilen, in denen die
   jeweilige Person mit einer `1` markiert ist.
4. **Slack-Empfänger zuordnen** – über einen Namens-Abgleich, mit Anzeige der
   Treffergüte und manueller Korrekturmöglichkeit.
5. **Versenden** – als Slack-DM an die echten Nutzer oder testweise an einen
   Test-Channel.

---

## Aufbau des Masterplans

Die Tabelle ist breit aufgebaut: links die **Kopf-/Programmspalten**, rechts je
**eine Spalte pro Person**.

| Uhrzeit | Programmpunkt   | Ort     | Anna Berger | Jonas Keller | … |
|---------|-----------------|---------|:-----------:|:------------:|---|
| 09:00   | Anreise         | Foyer   | 1           | 1            |   |
| 10:30   | Eröffnungsplenum| Saal A  | 1           |              |   |
| 14:00   | Lobbying GV     | Saal B  | 1           | 1            |   |

- Alles **links** der gewählten *„Erste Personen-Spalte"* gilt als
  Kopfinformation und landet im Zeitplan jeder Person.
- Jede **Personen-Spalte** enthält eine `1` in genau den Zeilen, an denen die
  Person teilnimmt. Leere Zellen werden automatisch als `0` behandelt.
- Hat eine Person keine einzige `1`, entsteht ein **leerer Zeitplan** (siehe
  unten).

Beispiel-Ergebnis für *Anna Berger*:

```
Zeitplan für Anna Berger
Tag: Tag 1 – Mittwoch
09:00   Anreise          Foyer
10:30   Eröffnungsplenum Saal A
14:00   Lobbying GV      Saal B
```

---

## Funktionen im Detail

### Geführter Ablauf (4 Schritte)
Die Oberfläche (Bootstrap 5 / `bslib`) führt in vier nummerierten Schritten
durch *Verbindung → Datenquelle → Nachrichten prüfen → Versenden*. Ein
Hell-/Dunkel-Umschalter ist oben rechts verfügbar.

### Intelligenter Namens-Abgleich
Die Zuordnung Masterplan-Name → Slack-Nutzer läuft über einen normalisierten,
token-sortierten **Jaro-Winkler-Vergleich** kombiniert mit einer
Token-Überschneidung. Damit werden zuverlässig erkannt:

- vertauschte Reihenfolge („Keller, Jonas" ↔ „Jonas Keller"),
- fehlende Zweit- bzw. Mittelnamen und Initialen,
- Umlaute und Akzente (locale-unabhängig, läuft auch im Container ohne
  UTF-8-Locale),
- kleinere Tippfehler.

Für jede Person zeigt die App die **Ähnlichkeit in Prozent** und eine farbige
Bewertung:

- 🟢 **Sicher** (≥ 90 %)
- 🟡 **Unsicher** (75–89 %) – bitte kurz prüfen
- 🔴 **Kein guter Treffer** (< 75 %) – sehr wahrscheinlich kein passender
  Slack-Nutzer

Wichtig: Der Abgleich liefert immer einen Vorschlag, aber bei niedriger
Ähnlichkeit ist das nur die „am wenigsten falsche" Wahl. Deshalb lässt sich pro
Person der **Slack-Empfänger über ein durchsuchbares Dropdown manuell
korrigieren**.

### Lesbare Nachrichten-Vorschau
Jede Nachricht wird pro Empfänger als Karte mit Klartext-Vorschau angezeigt –
so, wie sie in Slack ankommt. Über das Empfänger-Dropdown und den
**„Senden"-Schalter** lässt sich jede Karte einzeln anpassen oder ausschließen.

### Umgang mit leeren Zeitplänen
Personen ohne einen einzigen Programmpunkt werden mit dem Hinweis
**„Leerer Zeitplan"** markiert, aber **nicht automatisch entfernt**. Über den
Button **„Leere Zeitpläne ausschließen"** lassen sich alle leeren mit einem
Klick vom Versand ausnehmen; einzeln geht es weiterhin über den
„Senden"-Schalter.

### Sicherheitsabfrage vor echtem Versand
Solange **„An Test-Channel senden"** aktiv ist, gehen alle Nachrichten an den
hinterlegten Test-Channel. Wird der Schalter deaktiviert, erscheint vor dem
echten Versand eine **Bestätigungsabfrage** mit Übersicht (Anzahl sicherer,
unsicherer, schlechter Treffer und leerer Zeitpläne). Erst nach „Senden
bestätigen" werden die DMs verschickt.

### Mehrere Workspaces
Die App kann zwischen vier Slack-Workspaces wechseln: **MUNBW** (Standard beim
Start), **MUNBB**, **MUN-SH** und **DMUN**. Beim Wechsel werden Token,
Test-Channel und das Standard-Sheet des jeweiligen Workspaces übernommen.

### Slack-Daten aktualisieren
Nutzer- und Channel-Listen werden beim Start einmal geladen. Mit
**„Slack-Daten aktualisieren"** lassen sie sich neu abrufen, ohne den Workspace
wechseln zu müssen – nützlich, wenn kurz vorher neue Personen in den Workspace
eingeladen wurden.

### Google-Sheets-Anbindung
Über **„Google-Tools"** kann statt einer hochgeladenen Datei ein Google Sheet
als Datenquelle genutzt werden („Als input nutzen"). Zusätzlich gibt es eine
**Mapping-Funktion**, die Gruppen-Zuordnungen automatisch in individuelle
`1`-Markierungen umwandelt und ins Sheet zurückschreibt – ausführlich
beschrieben im Abschnitt [Google-Mapping](#google-mapping-gruppen--einzelpersonen).

### Lokale Zeit
Die Abschlussmeldung nach dem Versand nutzt die lokale Zeit (Europe/Berlin) –
keine Abhängigkeit mehr von einer externen Zeit-API.

---

## Google-Mapping (Gruppen → Einzelpersonen)

Viele Programme werden auf **Gruppen-Ebene** geplant („GV", „Presse",
„Vorbereitungsteam" …), während der Masterplan eine `1` pro **Einzelperson**
braucht. Das Google-Mapping nimmt einem diese Übersetzung ab: Es liest, welche
Gruppe(n) einer Programmzeile zugeordnet sind, schlägt die Mitglieder der Gruppe
nach und trägt für jede dieser Personen automatisch eine `1` ein – direkt im
Google Sheet.

Die Funktion arbeitet mit **zwei Blättern (Tabs) innerhalb desselben
Google-Spreadsheets**:

- **Plan-Blatt** – der eigentliche Zeitplan, in den die `1`-Markierungen
  geschrieben werden. Aufbau wie ein normaler Masterplan (Kopfspalten links,
  eine Spalte je Person rechts) plus **einer Spalte mit der Gruppen-Zuordnung**.
- **Mapping-Blatt** – eine einfache Nachschlage-Tabelle. Es zählen nur die
  **ersten beiden Spalten**: Spalte 1 = Gruppenname, Spalte 2 =
  Mitglieder als `;`-getrennte Liste (die Namen müssen exakt den
  Personen-Spaltenüberschriften im Plan-Blatt entsprechen).

### Beispiel

**Mapping-Blatt** (z. B. Tab „Mapping"):

| Gruppe | Mitglieder                 |
|--------|----------------------------|
| GV     | Anna Berger;Jonas Keller   |
| Presse | Sophie Wagner              |

**Plan-Blatt vorher** (Spalte „Gruppe" = Mapping-Spalte):

| Uhrzeit | Programmpunkt    | Gruppe       | Anna Berger | Jonas Keller | Sophie Wagner |
|---------|------------------|--------------|:-----------:|:------------:|:-------------:|
| 09:00   | Anreise          | Alle         |             |              |               |
| 10:30   | GV-Sitzung       | GV           |             |              |               |
| 14:00   | Pressekonferenz  | Presse;GV    |             |              |               |

**Plan-Blatt nachher** (durch das Mapping eingetragen):

| Uhrzeit | Programmpunkt    | Gruppe       | Anna Berger | Jonas Keller | Sophie Wagner |
|---------|------------------|--------------|:-----------:|:------------:|:-------------:|
| 09:00   | Anreise          | Alle         | 1           | 1            | 1             |
| 10:30   | GV-Sitzung       | GV           | 1           | 1            |               |
| 14:00   | Pressekonferenz  | Presse;GV    | 1           | 1            | 1             |

Regeln, die dabei greifen:

- **Mehrere Gruppen pro Zeile** sind möglich, getrennt durch `;`
  (z. B. `Presse;GV`) – die Mitglieder aller genannten Gruppen werden gesetzt.
- Der Sonderwert **`Alle`** (Groß-/Kleinschreibung egal) setzt eine `1` für
  **alle** Personen-Spalten dieser Zeile.
- Zeilen, deren Gruppen-Zelle **leer oder `0`** ist, werden übersprungen.
- Spalten, deren Name mit `sum` beginnt, werden ignoriert.
- Bereits vorhandene `1`-Markierungen bleiben erhalten; das Mapping ergänzt nur.

### Ablauf

1. **Google-Tools** öffnen und die **Sheet-ID** des Spreadsheets eingeben. Das
   Service-Account (siehe Konfiguration) muss **Bearbeitungsrechte** auf das
   Sheet haben – das Mapping schreibt zurück, Leserechte allein genügen nicht.
2. Auf **„Sheet Mapping"** klicken.
3. Im Dialog die vier Angaben wählen:
   - **Wähle das Blatt** – das Plan-Blatt, in das geschrieben wird.
   - **Wähle das Mapping-Blatt** – die Nachschlage-Tabelle.
   - **Was ist der erste Name?** – die erste Personen-Spalte im Plan-Blatt.
   - **Was ist die Mapping-Spalte?** – die Spalte im Plan-Blatt mit der
     Gruppen-Zuordnung.
4. **„Mapping durchführen"** – die App geht alle betroffenen Zeilen durch,
   trägt die `1`en ein und schreibt sie direkt ins Google Sheet. Eine
   Statuszeile meldet, welche Zeilen/Gruppen verarbeitet wurden.
5. Anschließend das Plan-Blatt wie gewohnt als Eingabe nutzen
   („Als input nutzen") und mit dem normalen Ablauf (Blatt + erste
   Personen-Spalte wählen → Nachrichten erstellen → versenden) fortfahren.

> **Hinweis:** Da direkt ins Sheet geschrieben wird, empfiehlt sich vorher eine
> Sicherungskopie des Spreadsheets. Stimmen Mitgliedernamen im Mapping-Blatt
> nicht exakt mit den Spaltenüberschriften im Plan-Blatt überein, werden die
> betreffenden Personen nicht markiert.

---

## Slack-API-Token erstellen

Die App benötigt pro Workspace ein **Bot-Token** (beginnt mit `xoxb-`). So
erhältst du es:

1. **App anlegen** – auf <https://api.slack.com/apps> einloggen (mit einem
   Account des Ziel-Workspaces), *„Create New App" → „From scratch"* wählen,
   einen aussagekräftigen Namen vergeben (z. B. „DMUN Zeitplan-Bot") und den
   Ziel-Workspace auswählen.
2. **Berechtigungen (Scopes) setzen** – im linken Menü *„OAuth & Permissions"*
   öffnen, zum Abschnitt *„Bot Token Scopes"* scrollen und über *„Add an OAuth
   Scope"* folgende Scopes hinzufügen:
   - `chat:write` – Nachrichten / Direktnachrichten senden
   - `users:read` – Nutzerliste abrufen (Grundlage für den Namens-Abgleich)
   - `channels:read`, `groups:read`, `im:read`, `mpim:read` – Channels und
     Konversationen auflisten
   - `im:write` – Direktnachricht-Konversationen öffnen
3. **App installieren** – oben auf *„Install to Workspace"* klicken und mit
   *„Allow"* bestätigen.
4. **Token kopieren** – unter *„OAuth & Permissions"* das **„Bot User OAuth
   Token"** (`xoxb-…`) kopieren und in die passende Umgebungsvariable eintragen
   (`SLACK_TOKEN_MUNBW`, `_MUNBB`, `_MUNSH` oder `_DMUN`).
5. **Bot in den Test-Channel einladen** – im Test-Channel
   `/invite @DeinAppName` ausführen, sonst darf der Bot dort nicht posten. Die
   Channel-ID (beginnt mit `C…`) steht in den *Channel-Details* ganz unten und
   gehört in die passende `TEST_CHANNEL_*`-Variable.

Für jeden weiteren Workspace die Schritte wiederholen und das Token in die
jeweilige Variable legen. Wird nur ein Workspace genutzt, reicht dessen Token;
die übrigen Variablen können leer bleiben.

**Hinweise:**

- Bot-Token laufen nicht ab, können von Slack aber **widerrufen** werden – etwa
  wenn das Token öffentlich auftaucht. Token deshalb nie in den Code oder ins
  Repository schreiben, sondern ausschließlich als Umgebungsvariable.
- Direktnachrichten erscheinen beim Empfänger **als Nachricht des Bots / der
  App**, nicht als Nachricht einer echten Person. App-Name und -Icon daher
  sinnvoll wählen – genau das sehen die Teilnehmenden. (Der ältere
  `as_user`-Parameter, der Nachrichten im Namen eines Nutzers verschickte, ist
  von Slack abgekündigt.)
- Nach jeder Scope-Änderung muss die App **erneut installiert** werden, damit
  die neuen Berechtigungen greifen.

---

## Konfiguration (Umgebungsvariablen)

Alle Zugangsdaten werden über Umgebungsvariablen gesetzt (z. B. in
`docker-compose.yaml` oder einer `.env`):

| Variable | Zweck |
|----------|-------|
| `SLACK_TOKEN_MUNBW` / `_MUNBB` / `_MUNSH` / `_DMUN` | Slack-Bot-Token je Workspace |
| `TEST_CHANNEL_MUNBW` / `_MUNBB` / `_MUNSH` / `_DMUN` | Channel-ID für Testversand je Workspace |
| `DEFAULT_SHEET_MUNBW` / `_MUNBB` / `_MUNSH` / `_DMUN` | Standard-Google-Sheet je Workspace |
| `GOOGLEAUTH_EMAIL` | Service-Account-E-Mail (Google Sheets) |
| `GOOGLEAUTH_SECRET` | Privater Schlüssel des Service-Accounts |
| `TZ` | Zeitzone des Containers (`Europe/Berlin`) |

Der Slack-Bot braucht Rechte zum Auflisten von Nutzern/Channels und zum Senden
von Direktnachrichten. Das Google-Service-Account muss Zugriff auf die
verwendeten Sheets haben.

---

## Starten

### Mit Docker (empfohlen)

```bash
docker compose up --build
```

Die App ist anschließend unter `http://localhost:3838` erreichbar (Port siehe
`docker-compose.yaml`). Zugangsdaten vorher als Umgebungsvariablen setzen.

### Lokal mit R

```r
# einmalig benötigte Pakete
install.packages(c(
  "shiny", "bslib", "shinyjs", "stringdist", "purrr", "dplyr", "stringr",
  "magrittr", "data.table", "readxl", "openxlsx", "httr", "slackr",
  "listviewer", "reactR", "googlesheets4"
))

# Umgebungsvariablen setzen (s. o.), dann:
shiny::runApp("app.R")
```

---

## Hinweise & Grenzen

- **Erst testen:** Vor dem ersten echten Versand einer Konferenz einmal mit
  aktivem Test-Channel durchlaufen – das prüft Namens-Treffer,
  Empfänger-Korrekturen und die Bestätigungsabfrage im Zusammenspiel.
- **Treffer immer kontrollieren:** Besonders gelbe und rote Bewertungen vor dem
  Versand prüfen. Ein schlechter automatischer Treffer kann sonst an die
  falsche Person gehen.
- **Gemeinsame Nutzung:** Die App verwendet einen einzelnen, global geteilten
  Slack-Login und globalen Zustand. Wenn zwei Personen sie gleichzeitig
  bedienen (insbesondere Workspace-Wechsel), können sie sich gegenseitig
  überschreiben. Für den Einsatz durch eine Person zur Zeit ist das unkritisch.

---

*Code von Maximilian Ilzhöfer für DMUN e.V.*
