# Bewerbungsportfolio – Statistische R-Tools & Mathematische Web-Applikationen

Dieses Repository dokumentiert eigenentwickelte Tools aus dem Bereich Datenanalyse und angewandter Mathematik.  
Ziel ist es, komplexe Sachverhalte effizient zu analysieren und visuell nachvollziehbar darzustellen – insbesondere im Kontext datengetriebener Entscheidungsfindung.

---

## Projektübersicht

### `r-tools/` – Statistische Analysefunktionen in R
Das Verzeichnis enthält sechs umfangreiche Funktionen zur automatisierten Datenanalyse:

- **own_Regression.R**  
  Einfache lineare Regression mit Shapiro-Wilk-Test, Q-Q-Plot, Residualdiagnostik

- **own_multipleregression.R**  
  Multiple Regression mit VIF-Analyse, Residuenplots, Breusch-Pagan-Test, Korrelationsmatrix

- **own_multipleregression_interactions.R**  
  Multiple Regression mit Interaktionseffekten, inkl. automatisierter Interaktionsplots und Johnson-Neyman-Verfahren

- **own_ANOVA.R**  
  Automatische Auswahl zwischen ANOVA und Kruskal-Wallis-Test je nach Voraussetzung, inklusive Visualisierung

Alle Funktionen enthalten Visualisierungen mit `ggplot2` sowie klar strukturierte Ergebnislisten.

---

# Math Visualizer – Kurvendiskussionsrechner (Python & LaTeX)

Dieses Tool dient der automatisierten Kurvendiskussion für beliebige Polynomfunktionen. Ziel ist es, mathematische Inhalte interaktiv und visuell verständlich aufzubereiten – insbesondere für Schüler*innen oder den didaktischen Einsatz.

---

## Funktionsübersicht

- Automatisierte Polynomdivision mit LaTeX-Ausgabe (über `polynom.sty`)
- Geplante Erweiterungen: Hoch- und Tiefpunkte, Wendepunkte, Nullstellen, Monotonieanalyse
- Web-Interface mit Flask (HTML, CSS, JavaScript)
- Backend-Rechenlogik in Python (z. B. `Rechenweg.py`, `Kurvendiskussion.py`)

---

## Projektstruktur

- `app.py` – Hauptanwendung (Flask)
- `modules/` – Rechenlogik (Kurvendiskussion, Rechenweg, Polynomdivision)
- `templates/` – HTML-Frontend
- `static/` – CSS und JavaScript
- `latex_packages/` – LaTeX-Komponenten zur Visualisierung (u. a. `polynom.sty`)
- `loading_gifs/` – Ladeanimationen (optional)

---

## Entwicklungsstand

Das Projekt befindet sich in aktiver Entwicklung.  
Folgende Punkte sind in Planung und sollen bis Oktober 2025 realisiert werden:

- Vollständige Ausgabe der Rechenwege
- Exportfunktionen (z. B. PDF, Screenshot)
- Eingabevalidierung und Nutzerführung
- Deployment als Web-App oder lokal installierbare Anwendung

---

## Lokale Ausführung

1. Abhängigkeiten installieren (z. B. Flask, SymPy)
2. Starten mit:

```bash
python app.py

## Autor

**Aurelio Nwamusse**  
B.Sc. Angewandte Mathematik – Hochschule Darmstadt  
Stipendiat des Evangelischen Studienwerks Villigst  
E-Mail: aurelio.nwamusse@stud.h-da.de  
LinkedIn: [Profil hier verlinken]

---

## Lizenz

Alle Inhalte dieses Repositories sind urheberrechtlich geschützt.  
Eine Nutzung, Verbreitung oder Bearbeitung ist nur mit ausdrücklicher schriftlicher Genehmigung des Autors gestattet.
