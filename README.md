# TMK-Bot – Personalisierte Zeitpläne über Slack

Eine SvelteKit-Anwendung für DMUN e.V., die aus einem zentral gepflegten
**Zeitplan** für jede Person einen **individuellen Ausschnitt** erzeugt und
ihn per Slack-Direktnachricht verschickt. Admins pflegen Personen, Tage und
Programmpunkte direkt in der App (Daten liegen in MySQL), prüfen die
Slack-Treffer und versenden alles in einem Durchgang. Alle anderen
eingeloggten Nutzer sehen ausschließlich ihren eigenen Zeitplan.

---

## Was die App macht

1. **Personen anlegen** – Name, optional eine E-Mail zur eindeutigen
   Zuordnung.
2. **Locations & Gruppen pflegen** – Orte mit Koordinaten (Karte auf
   OpenStreetMap-Basis) und Personen-Gruppen zum Bulk-Zuordnen.
3. **Tage & Programmpunkte pflegen** – pro Tag eine Liste von Punkten
   (Uhrzeit, Programmpunkt, Location) und wer daran teilnimmt.
4. **Pro Person eine Nachricht bauen** – aus allen Programmpunkten, denen die
   jeweilige Person zugeordnet ist.
5. **Slack-Empfänger zuordnen** – über einen Namens-Abgleich, mit Anzeige der
   Treffergüte und manueller Korrekturmöglichkeit.
6. **Versenden** – als Slack-DM an die echten Nutzer oder testweise an einen
   Test-Channel.

Nicht-Admins sehen beim Login stattdessen nur eine schreibgeschützte Ansicht
ihres eigenen Zeitplans.

---

## Rollen & Login

Der Login läuft über eine vorgeschaltete Traefik-Middleware
(`traefik-forward-auth`, Google als Provider). Sie loggt Nutzer:innen per
Google ein und reicht die E-Mail im Header `X-Forwarded-User` an die App
durch – dafür muss die Traefik-Middleware
`authResponseHeaders=X-Forwarded-User` konfiguriert haben, sonst kommt der
Header nie an. Die App selbst macht **kein** eigenes Login.

- **Admin**, wenn die E-Mail in `ADMIN_EMAILS` steht (kommagetrennt). Admins
  pflegen Personen/Tage/Programmpunkte und versenden Nachrichten.
- **Alle anderen** sehen nur ihren eigenen Zeitplan, zugeordnet über ihre
  Login-E-Mail (siehe unten).

### Zuordnung Login-E-Mail → Person

DMUN-Adressen folgen dem Schema `v.nachname@dmun.de` (erster Buchstabe des
Vornamens + Nachname). Beim Login wird zuerst geprüft, ob eine Person eine
**exakt passende, explizit hinterlegte E-Mail** hat – falls ja, gewinnt diese
immer. Andernfalls wird die Login-E-Mail gegen das Namensschema geraten
(gleicher Jaro-Winkler-Abgleich wie beim Slack-Matching), aber **nur bei
sehr hoher Ähnlichkeit** automatisch akzeptiert. Bei Unsicherheit gibt es
lieber gar keinen Treffer als einen falschen – die betroffene Person sieht
dann "kein Zeitplan gefunden" und ein Admin kann in der Personenverwaltung
eine E-Mail direkt hinterlegen, um die Zuordnung eindeutig zu machen.

---

## Funktionen im Detail

### Intelligenter Namens-Abgleich (Slack)
Die Zuordnung Personen-Name → Slack-Nutzer läuft über einen normalisierten,
token-sortierten **Jaro-Winkler-Vergleich** kombiniert mit einer
Token-Überschneidung. Damit werden zuverlässig erkannt: vertauschte
Reihenfolge, fehlende Zweit-/Mittelnamen, Umlaute/Akzente, kleinere
Tippfehler.

Für jede Person zeigt die App die **Ähnlichkeit in Prozent** und eine
farbige Bewertung:

- 🟢 **Sicher** (≥ 90 %)
- 🟡 **Unsicher** (75–89 %) – bitte kurz prüfen
- 🔴 **Kein guter Treffer** (< 75 %)

Der Slack-Empfänger lässt sich pro Person über ein durchsuchbares Feld
manuell korrigieren.

### Lesbare Nachrichten-Vorschau
Jede Nachricht wird pro Empfänger als Karte mit Klartext-Vorschau angezeigt.
Über das Empfänger-Feld und den **„Senden"-Schalter** lässt sich jede Karte
einzeln anpassen oder ausschließen.

### Umgang mit leeren Zeitplänen
Personen ohne einen einzigen Programmpunkt werden mit dem Hinweis
**„Leerer Zeitplan"** markiert, aber nicht automatisch entfernt. Über den
Button **„Leere Zeitpläne ausschließen"** lassen sich alle leeren mit einem
Klick vom Versand ausnehmen.

### Sicherheitsabfrage vor echtem Versand
Solange **„An Test-Channel senden"** aktiv ist, gehen alle Nachrichten an den
hinterlegten Test-Channel. Wird der Schalter deaktiviert, erscheint vor dem
echten Versand eine **Bestätigungsabfrage** mit Übersicht (Anzahl sicherer,
unsicherer, schlechter Treffer und leerer Zeitpläne).

### Mehrere Workspaces
Die App kann zwischen vier Slack-Workspaces wechseln: **MUNBW**, **MUNBB**,
**MUN-SH** und **DMUN**. Beim Wechsel werden Token und Test-Channel des
jeweiligen Workspace übernommen.

### Locations mit Karte
Locations haben einen Namen und optional eine Koordinate (Klick auf die
Karte beim Anlegen). Ist ein Programmpunkt mit einer Location verknüpft,
sehen sowohl Admins (im Editor) als auch die betroffenen Personen (im
eigenen Zeitplan) eine kleine OpenStreetMap-Karte mit Marker dazu – keine
API-Keys nötig, die Kartenkacheln kommen direkt von OSM.

### Gruppen
Gruppen sind nur ein Zuordnungs-Werkzeug: eine benannte Menge von Personen,
die sich beim Bearbeiten eines Programmpunkts mit einem Klick komplett
hinzufügen lässt, statt jede Person einzeln anzuhaken. Einzelne Personen
lassen sich danach trotzdem noch abwählen.

---

## Slack-API-Token erstellen

Die App benötigt pro Workspace ein **Bot-Token** (beginnt mit `xoxb-`).

1. **App anlegen** – auf <https://api.slack.com/apps> einloggen, *„Create New
   App" → „From scratch"*, Namen vergeben, Ziel-Workspace wählen.
2. **Bot Token Scopes** hinzufügen: `chat:write`, `users:read`,
   `channels:read`, `groups:read`, `im:read`, `mpim:read`, `im:write`.
3. **App installieren** – *„Install to Workspace"* → *„Allow"*.
4. **Token kopieren** – das **Bot User OAuth Token** in die passende
   Umgebungsvariable eintragen (`SLACK_TOKEN_MUNBW`, `_MUNBB`, `_MUNSH` oder
   `_DMUN`).
5. **Bot in den Test-Channel einladen** – `/invite @DeinAppName`, die
   Channel-ID gehört in die passende `TEST_CHANNEL_*`-Variable.

Bot-Token nie in den Code oder ins Repository schreiben, sondern
ausschließlich als Umgebungsvariable setzen.

---

## Konfiguration (Umgebungsvariablen)

| Variable | Zweck |
|----------|-------|
| `SLACK_TOKEN_MUNBW` / `_MUNBB` / `_MUNSH` / `_DMUN` | Slack-Bot-Token je Workspace |
| `TEST_CHANNEL_MUNBW` / `_MUNBB` / `_MUNSH` / `_DMUN` | Channel-ID für Testversand je Workspace |
| `AUTH_EMAIL_HEADER` | Header mit der Login-E-Mail (Default `X-Forwarded-User`) |
| `ADMIN_EMAILS` | Kommagetrennte Liste von E-Mails mit Admin-Rechten |
| `DB_HOST` / `DB_PORT` / `DB_USER` / `DB_PASSWORD` / `DB_NAME` | MySQL-Zugangsdaten |
| `TZ` | Zeitzone des Containers (`Europe/Berlin`) |

---

## Starten

```bash
cp .env.example .env   # ausfuellen
docker compose up --build
```

Startet die App **und** eine MySQL-Datenbank (Volume `masterplan-mysql`) für
lokales Testen. Die App ist unter `http://localhost:8080` erreichbar. Diese
`docker-compose.yaml` ist bewusst ohne Traefik gehalten, damit sie überall
einfach hochfährt.

### Echtes Deployment (hinter Traefik)

Die Traefik-Labels (Middleware `dmun-team-auth`, Host-Regel, TLS) liegen in
einem eigenen Overlay, das nur beim scharfen Deployment dazukommt:

```bash
docker compose -f docker-compose.yaml -f docker-compose.prod.yaml up -d --build
```

Setzt ein extern erreichbares `traefik`-Docker-Netzwerk voraus.

### Lokale Entwicklung

```bash
npm install
npm run dev
```

Braucht eine erreichbare MySQL-Instanz (Env-Vars s. o., z. B. per
`docker compose up mysql`) sowie – um Admin- bzw. Nutzeransicht zu testen –
einen `X-Forwarded-User`-Header, den man ohne die vorgeschaltete
Traefik-Middleware z. B. per Browser-Extension (etwa ModHeader) oder per
`curl` setzt.

---

## Hinweise & Grenzen

- **Erst testen:** Vor dem ersten echten Versand einmal mit aktivem
  Test-Channel durchlaufen.
- **Treffer immer kontrollieren:** Besonders gelbe und rote Bewertungen vor
  dem Versand prüfen.
- **E-Mail-Zuordnung prüfen:** Bei Namen, die vom Schema `v.nachname@dmun.de`
  abweichen (Doppelnamen, untypische Schreibweisen), lieber direkt eine
  E-Mail bei der Person hinterlegen statt sich auf das automatische Raten zu
  verlassen.

---

*Ursprünglich als R-/Shiny-Anwendung von Maximilian Ilzhöfer für DMUN e.V.,
seither auf SvelteKit/MySQL portiert.*
