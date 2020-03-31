# CoPE - Covid Predictor Estimator

![](.//media/image1.png)

## **Anleitung**

Version: 1.0

Datum: 30.03.2020

## **Inhalt**

[1 Einführung 3](#einführung)

[1.1 Aufruf 3](#aufruf)

[1.2 Algorithmus 3](#algorithmus)

[2 Eingaben 4](#eingaben)

[2.1 Region 4](#region)

[2.2 Reduzierende Massnahmen 4](#reduzierende-massnahmen)

[2.3 Expertenparameter Infektionsverlauf
4](#expertenparameter-infektionsverlauf)

[2.4 Krankenhausaufenthalt 5](#krankenhausaufenthalt)

[2.5 Einstellung der Darstellung 5](#einstellung-der-darstellung)

[3 Daten und Darstellung 7](#daten-und-darstellung)

[4 Vorgehensweise 9](#vorgehensweise)

[5 Literatur 10](#literatur)

Hinweis: Für die Richtigkeit der Berechnungen in dieser App wird nicht
garantiert. Alle Formeln sind offen
(<https://github.com/uwesterr/CoronaPredict>) und können von
fachkundigen Benutzern verifiziert, geprüft und für eigene Zwecke gerne
verwendet werden (bitte mit Referenz).

Eine Verwendung erfolgt auf eigene Verantwortung.

Fragen beantworten wir gerne unter: thomas.gneiting\@admos.de

Aktuelle Versionen finden Sie unter: https://covid.admos.de

Für Hinweise und Fehlerkorrekturen sind wir dankbar.

29.3.2020, Dr. Thomas Gneiting

Einführung
==========

Die Online-App "Rechenmodel Verlauf Covid19 Infektionen und deren
Auswirkung" soll eine Hilfestellung für alle Entscheidungsträger im
Bereich der Planung der notwendigen Resourcen in der Corona-Pandemie
darstellen.

Aufruf
------

Die App kann mit <https://covid19.admos.de> in jedem Browser aufgerufen
werden.

Algorithmus
-----------

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

Eingaben
========

Region
------

![](.//media/image2.png)

Auswahl: Deutschland -- Bundesländer -- Land- und Stadtkreise

Reduzierende Massnahmen
-----------------------

![](.//media/image3.png)

Hier können Datum von getroffen Massnahmen zur Reduzierung der
Reproduktionsrate angegeben werden.

Als Voreinstellung sind angegeben:

16.3. Schliessung von Schulen und Kitas

23.3. Einführung der Kontaktbeschränkungen

Expertenparameter Infektionsverlauf
-----------------------------------

Die Expertenparameter für den Infektionsverlauf und den
Krankenhausaufenthalt wurden aus den Quellen \[1\] und \[2\] übernommen.
Sie können vom Benutzer angepasst werden.

![](.//media/image4.png)

Krankenhausaufenthalt
---------------------

![](.//media/image5.png)

Die Parameter für den erwarteten Krankenhausaufenthalt wurden aus \[1\]
abgeleitet. Die Grafik aus \[1\] definiert auch die verschiedenen Zeiten
und den jeweiligen Versatz zwischen den Zuständen:

![](.//media/image6.png)

Einstellung der Darstellung
---------------------------

![](.//media/image7.png)
Daten und Darstellung
=====================

![](.//media/image8.png)
![](.//media/image9.png)

Kumulierte Infizierte: Gesamtzahl der positiv Getesteten

-   Erfasste Infizierte: Diese Zahl wird online von ... geholt

-   Erfasste Infizierte berechnet: Ausgabe der Modellierung. Diese Kurve
    wird z.T. automatisch an die realen Fallzahlen angepasst bzw. wird
    durch die Massnahmen beeinflusst.

Die Darstellung im logarithmischen Maßstab (links) erleichtert die
Anpassung vor allem im Anfangsstadium sowie das Erkennen der Auswirkung
von Maßnahmen.

Die lineare Darstellung (rechts) ergibt eine genauere Visualisierung der
Daten vor allem im gezoomten Bild.

Alle Grafiken sind interaktiv und erlauben z.B. Zoom bzw. haben einen
Marker:

![](.//media/image10.png)

![](.//media/image11.png)

![](.//media/image12.png)

![](.//media/image13.png)

Vorgehensweise
==============

\-\-\-\-\-\-\-- bis hierher bearbeitet !!
\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\--

Nach Auswahl der Region

![](.//media/image14.png)

Verwenden Sie Datum und Prozentsatz, um die berechnete Kurve den realen
Daten für die gewählte Region anzupassen.

Expertenparameter

Die Expertenparameter wurden für Fallzahlen Covid19 Betroffener im
dargestellten Zeitraum für Deutschland ermittelt und das Modell wurde
darauf kalibriert.

Wenn eigene Daten (z.B. für einen Kreis) vorhanden sind, können die
Parameter darauf angepasst werden.

Sind keine Daten vorhanden, können die Expertendaten für den
Infektionsverlauf auf diesen Standardwerten belassen werden.

Wir verwenden die tägliche Reproduktionsrate Rt als hauptsächlichen
Parameter zur Anpassung der Kurven in dem Bereich, bis die erste
Massnahme sich auswirkt (siehe hier auch \[2\] zur Beurteilung der
Werte)

Literatur
=========

  \[1\]   an der Heiden M, Buchholz U: Modellierung von Beispielszenarien der SARS-CoV-2-Epidemie 2020 in Deutschland. \| DOI 10.25646/6571.2
  ------- -------------------------------------------------------------------------------------------------------------------------------------
          

![](.//media/image15.png)

![](.//media/image16.png)
