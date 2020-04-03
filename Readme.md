# CoPE - Covid Predictor Estimator

## **Anleitung**

Version: 1.2

Datum: 2.04.2020

## **Inhalt**

[1 Einführung](#einführung)

[1.1 Aufruf](#aufruf)

[1.2 Algorithmus](#algorithmus)

[2 Eingaben](#eingaben)

[2.1 Region](#region)

[2.2 Reduzierende Massnahmen](#reduzierende-massnahmen)

[2.3 Expertenparameter Infektionsverlauf](#expertenparameter-infektionsverlauf)

[2.4 Krankenhausaufenthalt](#krankenhausaufenthalt)

[2.5 Einstellung der Darstellung](#einstellung-der-darstellung)

[3 Daten und Darstellung](#daten-und-darstellung)

[3.1 Kumulierte Infizierte](#kumulierte-infizierte)

[3.2 Verlauf Infizierte](#verlauf-infizierte)

[3.3 Plätze im Krankenhaus/Intensivstation](#plaetze-im-krankenhaus)

[3.4 Tägliche Reproduktionsrate](#taegliche-reproduktionsrate)

[4 Vorgehensweise](#vorgehensweise)

[5 Literatur](#literatur)
 
**Hinweis**: Für die Richtigkeit der Berechnungen in dieser App wird nicht
garantiert. Alle Formeln sind offen
(<https://github.com/uwesterr/CoronaPredict>) und können von
fachkundigen Benutzern verifiziert, geprüft und für eigene Zwecke gerne
verwendet werden (bitte mit Referenz).

Eine Verwendung erfolgt auf eigene Verantwortung.

Fragen beantworten wir gerne unter: thomas.gneiting@admos.de

Aktuelle Versionen finden Sie unter: https://covid.admos.de

Für Hinweise und Fehlerkorrekturen sind wir dankbar.

29.3.2020, Dr. Thomas Gneiting

<a name="einführung"></a>
## 1. Einführung

Die Online-App "Rechenmodel Verlauf Covid19 Infektionen und deren
Auswirkung" soll eine Hilfestellung für alle Entscheidungsträger im
Bereich der Planung der notwendigen Ressourcen in der Corona-Pandemie
darstellen.

<a name="aufruf"></a>
### 1.1 Aufruf

Die App kann mit <https://covid19.admos.de> in jedem Browser aufgerufen
werden.

<a name="algorithmus"></a>
### 1.2 Algorithmus

Bei der Entwicklung der App wurde ein vereinfachter Algorithmus
angewandt. Im Gegensatz zum allgemeingültigen SIR Modell, werden
verschiedene Randbedingungen als gegeben angenommen:

-   Es handelt sich um eine Epidemie

-   Es findet eine Sättigung der Infizierten statt

Eine Berechnung der Daten erfolgt in Tagesschritten. Hierbei erfolgt die
Berechnung der neu Infizierten aus der täglichen Reproduktionsrate.
Diese wird linear mit der Annäherung an die Sättigung reduziert.

Durch diese Berechnungsmethode können an beliebigen Tagen die
Auswirkungen von administrativen Massnahmen mit einfliessen.

Die Startwerte der Berechnungen werden automatisch aus dem Verlauf der
real erfassten Daten bestimmt. Diese Daten werden direkt vom Datenserver
xxxx abgeholt.

<a name="eingaben"></a>
## 2. Eingaben

<a name="region"></a>
### 2.1 Region

<img src=".//media/Region.PNG" width="350">

Auswahlmöglichkeiten: Deutschland -- Bundesländer -- Land- und Stadtkreise

Treffen Sie die Auswahl über die Dropdown-Liste oder geben Sie die gewünschte Region direkt im Feld ein (zuerst "----" löschen).

<a name="reduzierende-massnahmen"></a>
### 2.2 Reduzierende Massnahmen

<img src=".//media/RedMassnahmen.png" width="350">

Hier können Daten von getroffenen Massnahmen zur Reduzierung der
Reproduktionsrate angegeben werden.

Als Voreinstellung sind angegeben:

16.3.2020: Schliessung von Schulen und Kitas

23.3.2020: Einführung der Kontaktbeschränkungen

<a name="expertenparameter-infektionsverlauf"></a>
### 2.3 Expertenparameter Infektionsverlauf

Die Expertenparameter für den Infektionsverlauf und den
Krankenhausaufenthalt wurden aus den Quellen [1] und [2] übernommen.
Sie können vom Benutzer angepasst werden.

<img src=".//media/Expertenparameter.png" width="350">

<a name="krankenhausaufenthalt"></a>
### 2.4 Krankenhausaufenthalt

<img src=".//media/Krankenhausaufenthalt.png" width="350">

Die Parameter für den erwarteten Krankenhausaufenthalt wurden aus [1] abgeleitet. Die Grafik aus [1] definiert auch die verschiedenen Zeiten und den jeweiligen Versatz zwischen den Zuständen:

<img src=".//media/image6.png" width="550">

<a name="einstellung-der-darstellung"></a>
### 2.5 Einstellung der Darstellung

Hier kann das Zeitintervall für die Darstellung angegeben werden. Des Weiteren kann zwischen linearer und logarithmischer Skala der y-Achse umgeschaltet werden.

<img src=".//media/Datum.png" width="350">

<a name="daten-und-darstellung"></a>
## 3. Daten und Darstellung

<a name="kumulierte-infizierte"></a>
### 3.1 Kumulierte Infizierte

<img src=".//media/KumInf.png" width="1000">

Kumulierte Infizierte: Gesamtzahl der positiv Getesteten

-   Erfasste Infizierte: Diese Zahl wird online von ... übernommen

-   Erfasste Infizierte berechnet: Ausgabe der Modellierung. Diese Kurve
    wird z.T. automatisch an die realen Fallzahlen angepasst bzw. wird
    durch die Massnahmen beeinflusst.

Die Darstellung im logarithmischen Maßstab (links) erleichtert die
Anpassung vor allem im Anfangsstadium sowie das Erkennen der Auswirkung
von Maßnahmen.

Die lineare Darstellung (rechts) ermöglicht eine genauere Visualisierung der
Daten vor allem im gezoomten Bild.

Alle Grafiken sind interaktiv und erlauben z.B. Zoom und einen Marker, mit dem die tagesaktuellen Zahlen dargestellt werden können:

<img src=".//media/Marker.png" width="350">

Alle Interaktionen können im Header des Diagramms ativiert werden.

<a name="verlauf-infizierte"></a>
### 3.2 Verlauf Infizierte

<img src=".//media/VerlaufInf.png" width="350">

Aktuell Infizierte berechnet:
  * Berechnete Anzahl der Personen, die positiv getestet wurden und die zurzeit ansteckend sind.

Neu Infizierte berechnet:
  * Berechnete Anzahl der Personen, die positiv getestet wurden und die an diesem Tag zu den Aktuell Infizierten neu dazugekommen sind.

<a name="plaetze-im-krankenhaus"></a>
### 3.3 Plätze im Krankenhaus/Intensivstation

<img src=".//media/Krankenhaus.png" width="350">

Plätze im Krankenhaus:
  * Erwartete Anzahl der Personen, für die ein normales Krankenhausbett zur Verfügung gestellt werden sollte.

Plätze auf Intensivstation:
  * Erwartete Anzahl der Personen, für die ein Platz auf einer Intensivstation zur Verfügung gestellt werden sollte.

Diese Berechnungen erfolgen nach dem in [1] dargestellten Modell. Die Parameter hierfür können in [2.4](#krankenhausaufenthalt) eingestellt werden.

<a name="taegliche-reproduktionsrate"></a>
### 3.4 Tägliche Reproduktionsrate

<img src=".//media/Rt.png" width="350">

Die tägliche Reproduktionsrate Rt beschreibt die Zahl derer, die ein Infizierter im Durchschnitt pro Tag ansteckt.
Sie kann durch getroffene Maßnahmen verändert werden.
Im Zeitabschnitt vor den getroffenen Maßnahmen bestimmt hauptsächlich die unveränderte Reproduktionsrate den Kurvenverlauf.

<a name="vorgehensweise"></a>
## 4. Vorgehensweise

<img src=".//media/KumInfLog.png" width="450">

1.	Wählen Sie die zu betrachtende Region aus.
2.	Passen Sie evtl. das Anfangsdatum der Darstellung an. Dies sollte bei der ersten Erfassung von Infizierten liegen.
3.	Tragen Sie das Datum von getroffenen Maßnahmen zur Reduzierung der Ausbreitung von Covid19 ein. Verwenden Sie den Prozentsatz zur Reduzierung der Reproduktionsrate Rt, um die berechnete Kurve den realen Daten anzupassen. Dies kann am besten in der logarithmischen Darstellung durchgeführt werden.

#### Expertenparameter Infektionsverlauf

Die Expertenparameter wurden für Fallzahlen Covid19 Betroffener im dargestellten Zeitraum für Deutschland ermittelt und das Modell wurde darauf kalibriert.

Wenn eigene Daten (z.B. für einen Kreis) vorhanden sind, können die Parameter darauf angepasst werden.

Sind keine Daten vorhanden, können die Expertendaten für den Infektionsverlauf auf diesen Standardwerten belassen werden.

Wir verwenden hauptsächlich die tägliche Reproduktionsrate Rt als Parameter zur Anpassung der Kurven in dem Bereich vor der ersten Maßnahme (siehe hier auch [2] zur Beurteilung der Werte).

<a name="literatur"></a>
## 5. Literatur

  **[1]**   an der Heiden M, Buchholz U: Modellierung von Beispielszenarien der SARS-CoV-2-Epidemie 2020 in Deutschland. | DOI 10.25646/6571.2
  ------- -------------------------------------------------------------------------------------------------------------------------------------
          

![](.//media/image15.png)

  **[2]**   Kucharski AJ, Russel TW, et al.: Early dynamics of transmission and control of COVID-19: a mathematical modelling study
  ------- -------------------------------------------------------------------------------------------------------------------------------------
![](.//media/image16.png)
