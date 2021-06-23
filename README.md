Dies ist das Begleitrepository zur Veröffentlichung [TBA](https://...). Inhalt ist:

 * [stvu_analyse.R](stvu_analyse.R): Dieses Analyseprogramm wurde dem Forschungsdatenzentrum in Kiel übermittelt, um per kontrollierter Datenfernverarbeitung (KDFV) die Straßenverkehrsunfallstatistik des Bundes aus dem Jahr 2014 für unsere Zwecke zu analysieren. Die Ergebnisse des Durchlaufs von [stvu_analyse.R](stvu_analyse.R) befinden sich in [Rohdaten](Rohdaten).
 * [exposure.csv](exposure.csv): Zu Unfallursachen zugehörige Betriebssituationen mit den von uns vergebenen Häufigkeitsklassen (das Attribut confidence gibt es, wie sicher wir uns dieser Einschätzung sind). Quelle: FDZ der Statistischen Ämter des Bundes und der Länger, Straßenverkehrsunfälle, 2014, eigene Berechnungen.
 * [ASIL_determination.R](ASIL_determination.R): Mit diesem Programm errechnen wir aus obigen Analyseergebnissen die Schweregrade (severiy level) S0–S3 und kombinieren diese mit zusätzlich gegebenen Häufigkeitsklassen (exposure level) E0–E4 aus [exposure.csv](exposure.csv) zu einem ASIL.
 * [ASIL.csv](ASIL.csv): Das Ergebnis eines Durchlaufs von [ASIL_determination.R](ASIL_determination.R).
