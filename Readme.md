# CoPE - Covid Predictor Estimator

## **Anleitung**

Version: 0.24

Datum: 10.05.2020

## **Inhalt**

[1 Einführung](#einführung)

[  1.1 Aufruf](#aufruf)

[  1.2 Algorithmus](#algorithmus)

[2 Eingaben](#eingaben)

[  2.1 Region](#region)

[  2.2 Reduzierende Massnahmen](#reduzierende-massnahmen)

[  2.3 Expertenparameter Infektionsverlauf](#expertenparameter-infektionsverlauf)

[  2.4 Krankenhausaufenthalt](#krankenhausaufenthalt)

[  2.5 Einstellung der Darstellung](#einstellung-der-darstellung)

[3 Daten und Darstellung](#daten-und-darstellung)

[  3.1 Meldung der Infizierten der letzten Woche pro 100.000 Einwohner](#meldungen-letzte-woche)

[  3.2 Kumulierte Infizierte](#kumulierte-infizierte)

[  3.3 Verlauf Infizierte](#verlauf-infizierte)

[  3.4 Plätze im Krankenhaus/Intensivstation](#plaetze-im-krankenhaus)

[4 Vorgehensweise](#vorgehensweise)

[5 Literatur](#literatur)
 
**Hinweis**: Für die Richtigkeit der Berechnungen in dieser App wird nicht
garantiert. Alle Formeln sind offen (<https://github.com/uwesterr/CoronaPredict>) und können von
fachkundigen Benutzern verifiziert, geprüft und für eigene Zwecke gerne verwendet werden (bitte mit Referenz).

Eine Verwendung erfolgt auf eigene Verantwortung.
Fragen beantworten wir gerne unter: covid19@admos.de
Aktuelle Versionen finden Sie unter: https://covid.admos.de
Für Hinweise und Fehlerkorrekturen sind wir dankbar.

10.5.2020, Dr. Thomas Gneiting, Dr. Uwe Sterr

<a name="einführung"></a>
## 1. Einführung

Die Online-App "Rechenmodel Verlauf Covid19 Infektionen und deren Auswirkung" soll eine Hilfestellung für alle Entscheidungsträger im Bereich der Planung der notwendigen Ressourcen in der Corona-Pandemie darstellen. 

<a name="aufruf"></a>
### 1.1 Aufruf

Die App kann mit <https://covid19.admos.de> in jedem Browser aufgerufen werden.

<a name="algorithmus"></a>
### 1.2 Algorithmus

Bei der Entwicklung der App wurde ein vereinfachter Algorithmus angewandt. Im Gegensatz zum allgemeingültigen SIR Modell, werden verschiedene Randbedingungen als gegeben angenommen:

-   Es handelt sich um eine Epidemie
-   Es findet eine Sättigung der Infizierten statt  



Eine Berechnung der Daten erfolgt in Tagesschritten. Hierbei werden die neu Infizierten an einem Tag aus den Infizierten des Vortags und der täglichen Reproduktionsrate berechnet.Diese wird linear mit der Annäherung an die Sättigung reduziert.

Durch diese Berechnungsmethode können an beliebigen Tagen die Auswirkungen von administrativen Massnahmen mit einfliessen indem die Reproduktionsrate reduziert bzw. erhöht wird.

Die Parameter des Modells werden durch Optimierungsverfahren (Genetic Algorithm) an die bis zum aktuellen Zeitpunk verfügbaren realen Daten angepasst. Es handelt sich hierbei um:
1. Fallzahllen, die vom Robert-Koch Institut auf dem Datenserver des [Corona Hub von Esri](https://npgeo-corona-npgeo-de.hub.arcgis.com/datasets/ef4b445a53c1406892257fe63129a8ea_0?geometry=-23.139%2C46.270%2C39.395%2C55.886&selectedAttribute=faelle_100000_EW) veröffentlicht werden. Die Fallzahlen sind für die regionalen Gliederungen Land-/Stadtkreise, Bundesländer und Deutschland verfügbar. 
2. Belegzahlen der Kliniken für Baden-Württemberg. Diese werden nach Landkreisen zusammengefasst und stellen die aktuell gemeldete Belegung an stationären Betten und Beatmungsbetten auf Intensivstationen (ICU) dar. Diese Zahlen werden von der Firma convexis (convexis.de) freundlicherweise zur Verfügung gestellt und ebefalls täglich aktualisiert. 

Es werden die Parameter von 2 Teilmodellen durch die erwähnten Algorithmen angepasst:  

1. Ausbreitung der Infektionen
2. Belegung der Krankenhausbetten

<a name="eingaben"></a>
## 2. Eingaben

<a name="region"></a>
### 2.1 Region

<img src=".//media/Region.PNG" width="350">

Auswahlmöglichkeiten: Deutschland -- Bundesländer -- Land- und Stadtkreise

Treffen Sie die Auswahl über die Dropdown-Liste oder suchen Sie die gewünschte Region direkt im Feld (zuerst "---" löschen). Alternativ kann die Region auch über die Karte ausgewählt werden: 

<img src=".//media/karteauswahl.jpg" width="350">

<a name="reduzierende-massnahmen"></a>
### 2.2 Reduzierende Massnahmen

<img src=".//media/RedMassnahmen.PNG" width="350">

Hier können Daten von getroffenen Massnahmen zur Reduzierung der Reproduktionsrate angegeben werden. Die Reduzierung setzt sich aus dem Datum an dem die Massnahme in Kraft getreten ist und der Reduzierung der täglichen Reproduktionsrate Rt zusammen. 

Als Voreinstellung sind angegeben:

16.3.2020: Schliessung von Schulen und Kitas  
23.3.2020: Einführung der Kontaktbeschränkungen

Diese Daten können lokal abweichen. Baden-Württemberg hat die Schulen z.B. erst am 17.3. geschlossen. 
Der Optimieralgorithmus bestimmt die Reduzierungsfaktoren an den gegebenen Daten aus den erfassten Infizierten. 
Benutzer können Datum und Reduktionsfaktor manuell über die Schieberegler anpassen. Ziel dabei sollte die möglichst gute Überstimmung der Modellkurve (blau) mit den realen Werten (Punkte) zu erhalten. 

<a name="expertenparameter-infektionsverlauf"></a>
### 2.3 Expertenparameter Infektionsverlauf

Die Expertenparameter für den Infektionsverlauf und den Krankenhausaufenthalt wurden aus den Quellen [1] und [2] abgeleitet. Die Standardwerte wurden so eingestellt, dass der Verlauf der Pandemie für Deutschland gut angepasst werden konnte. Sie können ebenfalls vom Benutzer angepasst werden um andere Szenarien zu visualisieren.

<img src=".//media/Expertenparameter.PNG" width="350">

<a name="krankenhausaufenthalt"></a>
### 2.4 Krankenhausaufenthalt

<img src=".//media/Krankenhausaufenthalt.PNG" width="900">

Die Parameter für den erwarteten Krankenhausaufenthalt wurden zunächst aus [1] abgeleitet. Nachdem der Zugriff auf die Krankenhausdaten in Baden-Württemberg vorhanden war, konnten die Modellparameter für die realen Daten der Landkreise aus Baden-Württemberg angepasst werden. Als Standardparameter für die restlichen Regionen werden die Parameter für das gesamte Bundesland Baden-Württemberg herangezogen, das mit einer Bevölkerung von ca. 11 Millionen Menschen eine ausreichende statistische Grundlage bildet. 

Die folgende Grafik in Anlehnung an [1] definiert auch die verschiedenen Zeiten und den jeweiligen Versatz zwischen den Zuständen:

<img src=".//media/ParameterKrankenhaus.png" width="900">

<a name="einstellung-der-darstellung"></a>
### 2.5 Einstellung der Darstellung

Hier kann das Zeitintervall für die Darstellung angegeben werden. Des Weiteren kann zwischen linearer und logarithmischer Skala der y-Achse umgeschaltet werden. 

<img src=".//media/Datum.PNG" width="350">

<a name="daten-und-darstellung"></a>
## 3. Daten und Darstellung

<a name="meldungen-letzte-woche"></a>
### 3.1 Meldung der Infizierten der letzten Woche pro 100.000 Einwohner

---

<img src=".//media/masszahl_100K_woche.png" width="500">

---

Dieses Diagramm zeigt die Anzahl der Infizierten pro 100.000 Einwohner der gewählten Region sowie das Limit von 50. 

<a name="kumulierte-infizierte"></a>
### 3.2 Kumulierte Infizierte

<img src=".//media/KumInf.png" width="1000">

Kumulierte Infizierte: Gesamtzahl der positiv Getesteten

-   Erfasste Infizierte: Diese Zahl wird online vom Datenserver des [Corona Hub von Esri](https://npgeo-corona-npgeo-de.hub.arcgis.com/datasets/ef4b445a53c1406892257fe63129a8ea_0?geometry=-23.139%2C46.270%2C39.395%2C55.886&selectedAttribute=faelle_100000_EW) übernommen

-   Erfasste Infizierte berechnet: Ausgabe der Modellierung. Diese Kurve wird durch den Optimieralgorithmus automatisch an die realen Fallzahlen angepasst. Der Algorithmus berechnet auch die Wirksamkeit einer Massnahme zu einem bestimmten Zeitpunkt. 
-   Zusätzlich können die Kurven durch die Schieberegler feinjustiert werden. Bzw. es können Szenarien (was wäre z.B. ohne Massnahmen geschehen etc. ) getestet werden.
-   Die berechneten Kurven haben ein 95% Konfidenzintervall mit einer Dauer von 3 Wochen überlagert, das den Bereich der möglichen Unsicherheit darstellt. 
    
Die Darstellung im logarithmischen Maßstab (links) erleichtert die Anpassung vor allem im Anfangsstadium sowie das Erkennen der Auswirkung von Maßnahmen.

Die lineare Darstellung (rechts) ermöglicht eine genauere Visualisierung der Daten vor allem im gezoomten Bild.

Alle Grafiken sind interaktiv und erlauben z.B. Zoom und einen oder mehrere Marker, mit dem die tagesaktuellen Zahlen dargestellt werden können:

<img src=".//media/Marker.PNG" width="500">

Alle Interaktionen können im Header des Diagramms ativiert werden.

<a name="verlauf-infizierte"></a>
### 3.3 Verlauf Infizierte

<img src=".//media/VerlaufInf.png" width="450">

Aktuell Infizierte berechnet:
  * Berechnete Anzahl der Personen, die positiv getestet wurden und die zurzeit ansteckend sind.

Neu Infizierte berechnet:
  * Berechnete Anzahl der Personen, die positiv getestet wurden und die an diesem Tag zu den Aktuell Infizierten neu dazugekommen sind.

<a name="plaetze-im-krankenhaus"></a>
### 3.4 Plätze im Krankenhaus/Intensivstation

<img src=".//media/Krankenhaus.png" width="450">

Plätze im Krankenhaus:
  * KH erfasst: Für Regionen in Baden-Württemberg werden hier die aktuellen Belegungszahlen von Covid19 Patienten in normaler stationärer Behandlung angezeigt. Die Daten werden online von Convexis GmbH abgerufen. Für die anderen Regionen Deutschlands steht dieser Service leider nicht zur Verfügung. 
  * KH berechnet: Erwartete Anzahl der Personen, für die ein normales Krankenhausbett zur Verfügung gestellt werden sollte.

Plätze auf Intensivstation:
  * Intensiv erfasst: Für Regionen in Baden-Württemberg werden hier die aktuellen Belegungszahlen von Covid19 Patienten in Intensiv-Behandlung mit Beatmung angezeigt. Die Daten werden online von Convexis GmbH abgerufen. Für die anderen Regionen Deutschlands steht dieser Service leider nicht zur Verfügung. 
  * Intensiv berechnet: Erwartete Anzahl der Personen, für die ein Platz auf einer Intensivstation zur Verfügung gestellt werden sollte.

Diese Berechnungen erfolgen entsprechend dem in [[1]](#lit1) dargestellten Modell. Die Parameter hierfür werden für Regionen in Baden-Württemberg automatisch ermittelt und können in [2.4](#krankenhausaufenthalt) eingestellt werden.
Die für Baden-Württemberg ermittelten Werte sind als Standardwerte auf alle Regionen in Deutschland übertragen worden. 


<a name="vorgehensweise"></a>
## 4. Vorgehensweise

<img src=".//media/KumInfLog.png" width="450">

1.	Wählen Sie die zu betrachtende Region aus.
2.	Passen Sie evtl. das Anfangsdatum der Darstellung an. Dies sollte bei der ersten Erfassung von Infizierten liegen.
3.	Tragen Sie das Datum von getroffenen Maßnahmen zur Reduzierung der Ausbreitung von Covid19 ein. Verwenden Sie den Prozentsatz zur Reduzierung der Reproduktionsrate Rt, um die berechnete Kurve den realen Daten anzupassen. Dies kann am besten in der logarithmischen Darstellung durchgeführt werden.
4.	In der Regel erfordert die Sterblichkeitsrate aufgrund regionaler Unterschiede ebenfalls geringe Anpassungen.  


<a name="literatur"></a>
## 5. Literatur
<a name="lit1">[1]</a> an der Heiden M, Buchholz U: Modellierung von Beispielszenarien der SARS-CoV-2-Epidemie 2020 in Deutschland. | DOI 10.25646/6571.2

<a name="lit2">[2]</a> Kucharski AJ, Russel TW, et al.: Early dynamics of transmission and control of COVID-19: a mathematical modelling study




Ende
Ende
Ende



