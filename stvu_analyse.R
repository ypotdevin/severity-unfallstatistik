################################################################################
#
# Fuehren Sie dieses R-Skript am besten mit dem Befehl
#
#     Rscript stvu_analyse.R
#
# von mittels der Kommandozeile aus. Es liest die StVU-Daten aus der Datei mit
# dem Pfad stvu_dateipfad (siehe unten) aus und erstellt 5 Dateien in dem Ordner
# ausgabe_ordner (siehe unten), welche die Ergebnisse der Auswertung darstellen.
#
################################################################################



########################## Benoetigte Bibliotheken #############################
# STATA-Dateien als Dataframe in R einlesen (read.dta)
library(foreign)

# Sammlung von Funktionen zur Aufbereitung und Weiterverarbeitung von Dataframes
# Ggf. muessen diese Pakete noch installiert werden.
library(tidyr)
library(dplyr)
################################################################################



###################       Anzupassende Dateipfade        #######################
# Bitte ergaenzen
stvu_dateipfad <- <Pfad/zu/den/STVU/Daten/inkl/Name/und/Endung>

# Bitte ergaenzen, ohne slash am Ende. In diesem Ordner wird der Unterordner
# zur_veroeffentlichung anlegt. Dort Abgelegte Dateien moegen zur
# Veroeffentlichung freigegeben werden.
# Auch wird der Unterordner zur_pruefung angelegt, in dem ggf. Dateien landen,
# welche ausschliesslich zur Durchfuehrung der Geheimhaltungspruefung dienen.
ausgabe_ordner <- <Pfad/zum/Ordner>
################################################################################



################################################################################
# Titel des Projekts:        <Entwicklung und Evaluierung eines ÖPNV-on-Demand-
#                             Geschaeftsmodells mit autonomen Fahrzeugen fuer
#                             den oeffentlichen Personennahverkehr in
#                             laendlichen Regionen>
# Datengrundlage:                      <Strassenverkehrsunfalldaten (StVU) 2014>
# Dateiname des Programmcodes:                                  <stvu_analyse.R>
# erstellt:                                                         <10.03.2021>
# von:                                                         <Yannik Potdevin>
# E-Mail:                                           <ypo@informatik.uni-kiel.de>
# Tel.:                                                          <0431 880 7554>
# Dateinamen der Output-Files:                         <verh_allg_uursachen.csv,
#                                                       verh_pers_bursachen.csv,
#                                                              gesamtsummen.csv>
#
#
# Grundriss des Programms:  <Dieses Programm stellt 2 Anfragen an die StVU.
#                            In beiden werden Unfaelle nach gemeinsamer Ursache
#                            gruppiert und zugehoerige Unfallkennzahlen
#                            ermittelt.
#                            Weiterhin werden Gesamtsummen der obigen
#                            Unfallkennzahlen vor der Gruppierung gebildet um
#                            einzuschaetzen, wie stark nach der Gruppierung
#                            Doppeltzaehlungen diese Kennzahlen aufblaehen.>
#
#
# Verwendete globale Variablen:                 <stvu_dateipfad, ausgabe_ordner>
#
# Ansonsten werden nur lokale Variablen (innerhalb von Funktionen) eingesetzt.



################################################################################
###                       Nachfolgend: Aufbereitung                          ###
################################################################################

# Diese Funktion konvertiert die StVU-Daten aus dem STATA-Format in ein
# Dataframe-Objekt. Der Dataframe wird dann auf die relevanten Merkmale
# eingeschraenkt und es wird sichergestellt, dass nur Unfaelle aus dem
# Erhebungsjahr 2014 betrachtet werden, bei denen Personenschaden vorliegt.
# Ausserdem wird forciert, dass pro Unfall zu jedem Beteiligten nur eine
# Beobachtung vorliegt. Zuletzt werden die Auspraegungen zu numerischen
# Merkmalen vom Typ Character in einen numerischen Typ umgewandelt.
vorverarbeitung <- function(dateipfad) {
    stvu <- read.dta(
        file = dateipfad
    )
    relevante_merkmale <- c("uid",
                            "ujahr",
                            "ukategorie",
                            "uart",
                            "uursache1",
                            "uursache2",
                            "uanzbet",
                            "uanzgetoe",
                            "uanzschwer",
                            "uanzleicht",
                            "bordnnr",
                            "bursache1",
                            "bursache2",
                            "bursache3",
                            "bunfallfolge",
                            "bmanzahlmitf",
                            "banzfzbenutz",
                            "bverkbet")
    stvu %>%
        select(relevante_merkmale) %>%
        transform(
            uanzbet = as.numeric(uanzbet),
            uanzgetoe = as.numeric(uanzgetoe),
            uanzschwer = as.numeric(uanzschwer),
            uanzleicht = as.numeric(uanzleicht),
            bmanzahlmitf = as.numeric(bmanzahlmitf),
            banzfzbenutz = as.numeric(banzfzbenutz),
            bverkbet = as.numeric(bverkbet)
        ) %>%
        replace_na(list(
            uanzgetoe = 0,
            uanzschwer = 0,
            uanzleicht = 0,
            bmanzahlmitf = 0,
            banzfzbenutz = 0)
        ) %>%
        filter(ujahr == 2014 & ukategorie %in% 1:3) %>%
        distinct(uid, bordnnr, .keep_all = TRUE)
}



# Die IDs jener Unfaelle, bei denen der Hauptverursacher ein vermutlich
# zukuenftig automatisierbares Kraftfahrzeug fuhr. Grund fuer diese Auswahl ist
# die Absicht der Reduktion auf jene Unfaelle, die massgeblich einem solchen
# Fahrzeug zuzurechnen sind. Unfaelle die anderen Fahrzeugen oder Fussgaengern
# hauptsaechlich zuzurechnen sind, sollen hier nicht betrachtet werden.
# Ergebniskopf:
# -------
# | uid |
# -------
relevante_uids <- function(stvu) {
    mit_verkehrsbeteiligungskategorie(stvu) %>%
        filter(bordnnr == "01" & bverkbet_kat == 1) %>%
        select(uid)
}



# Projektion der Daten auf Unfaelle (ohne Doppelnennungen) und die Haeufigkeiten
# zugehoeriger Unfallausgaenge.
# Ergebniskopf:
# ---------------------------------------------
# | uid | uanzgetoe | uanzschwer | uanzleicht |
# ---------------------------------------------
unfallausgaenge <- function(stvu) {
    stvu %>%
        select(uid, uanzgetoe, uanzschwer, uanzleicht) %>%
        distinct(uid, .keep_all = TRUE)
}



# Die Verkehrsbeteiligung wird in 3 Kategorien unterteilt. Kategorie 1 bilden
# die vermutlich zukuenftig automatisierbaren Kraftfahrzeuge. Vereinfacht
# gesprochen sind dies schwere Kraftfahrzeuge mit mindestens 3 Raedern, aber
# nicht Schienenfahrzeuge (61-62). Ausgeschlossen werden auch Zweiraeder
# (1-15, 71 & 72), weil hier eine Automatisierung zweifelhaft ist (wegen der
# gewollt manuellen Fahrweise), und sonstige Fahrzeuge (91-92).
# Kategorie 2 bilden alle Fahrzeuge, die nicht in Katerogie 1 enthalten sind.
# Dies muessen nicht zwangslaeufig Kraftfahrzeuge sein.
# Kategorie 3 bilden alle Varianten von Fussgaengern.
# Kategorie -1 ist eine Fehlerkategorie (diese sollte nicht vergeben werden).
# Ergebniskopf:
# -----------------------------------------
# | ...[Eingabespalten]... | bverkbet_kat |
# -----------------------------------------
mit_verkehrsbeteiligungskategorie <- function(stvu) {
    stvu %>%
        mutate(bverkbet_kat = case_when(
                bverkbet %in% 21:59                     ~  1,
                bverkbet %in% 1:15                      ~  2,
                bverkbet %in% c(61, 62, 71, 72, 91, 92) ~  2,
                bverkbet %in% c(81:84, 93)              ~  3,
                TRUE                                    ~ -1
            )
        )
}



# Ermittlung einer unteren Schranke fuer die Zahl der gefaehrdeten Personen pro
# Unfall. Als gefaehrdet gelten dabei ...
#   1. alle Benutzer eines Fahrzeugs, sobald einer von ihnen (Fahrzeugfuehrer
#      oder Mitfahrer) verletzt wurde
#   2. Fahrzeugbenutzer eines Fahrzeugs, welches mit einem anderen Fahrzeug
#      zusammenstiess, auch wenn unter den Nutzern niemand verletzt ist
#   3. die Verletzten (insbesonde Nichtfahrzeugbenutzer)
# Die so mittels der Beteiligten ermittelten Gefaehrdetenzahlen werden pro
# Unfall aufsummiert.
# Ergebniskopf:
# -------------------------
# | uid | uanzgefaehrdete |
# -------------------------
gefaehrdete <- function(stvu) {
    stvu <- mit_verkehrsbeteiligungskategorie(stvu)
    ist_verletzt <- stvu$bunfallfolge %in% 1:3
    ist_fzbenutzer <- stvu$bverkbet_kat %in% 1:2
    verletzte <- ist_verletzt + stvu$bmanzahlmitf
    ist_fzzusammenstoss <- stvu$uart %in% 1:5
    max_vb <- pmax(verletzte, stvu$banzfzbenutz)
    stvu %>%
        mutate(
            gefaehrdete = case_when(
                ist_fzbenutzer & verletzte > 0       ~ max_vb,
                ist_fzbenutzer & ist_fzzusammenstoss ~ max_vb,
                TRUE                                 ~ verletzte
            )
        ) %>%
        group_by(uid) %>%
        summarize(
            uanzgefaehrdete = sum(gefaehrdete)
        )
}



# Ermittlung der Zahl der Fahrzeugbenutzer pro Unfall. (Nur zu Diagnosezwecken)
# Ergebniskopf:
# ----------------------
# | uid | uanzfzbenutz |
# ----------------------
fahrzeugbenutzer <- function(stvu) {
    stvu %>%
        group_by(uid) %>%
        summarize(
            uanzfzbenutz = sum(banzfzbenutz),
        )
}

# Ermittlung der Zahl der Beteiligten pro Unfall. (Nur zu Pruefung der
# Zulassungsregeln).
# Ergebniskopf:
# -----------------
# | uid | uanzbet |
# -----------------
beteiligte <- function(stvu) {
    stvu %>%
        select(uid, uanzbet) %>%
        distinct(uid, .keep_all = TRUE)
}



################################################################################
###                         Nachfolgend: Auswertung                          ###
################################################################################

# Hilfsfunktion fuer `verhaeltnisse_allgemeiner_uursachen` und
# `verhaeltnisse_personenbezogener_uursachen`.
# Ergebniskopf:
# ---------------------------------------------------------------------------
# | `grp_by` | unfallzahl | sum_uanzbet | sum_uanzgetoe | sum_uanzschwer |...
# ---------------------------------------------------------------------------
#   --------------------------------------------------------------
#   ...| sum_uanzleicht | sum_uanzfzbenutz | sum_uanzgefaehrdete |
#   --------------------------------------------------------------
verhaeltnisse_ursachen <- function(stvu, ursachen, grp_by) {
    ursachen %>%
        semi_join(relevante_uids(stvu), by = "uid") %>%
        inner_join(beteiligte(stvu), by = "uid") %>%
        inner_join(unfallausgaenge(stvu), by = "uid") %>%
        inner_join(fahrzeugbenutzer(stvu), by = "uid") %>%
        inner_join(gefaehrdete(stvu), by = "uid") %>%
        group_by(!!grp_by) %>%
        summarize(
            unfallzahl = n(),
            sum_uanzbet = sum(uanzbet),
            sum_uanzgetoe = sum(uanzgetoe),
            sum_uanzschwer = sum(uanzschwer),
            sum_uanzleicht = sum(uanzleicht),
            sum_uanzfzbenutz = sum(uanzfzbenutz),
            sum_uanzgefaehrdete = sum(uanzgefaehrdete)
        ) %>%
        arrange(desc(sum_uanzbet))
}


# Diese Funktion zaehlt pro Auspraegung einer allgemeinen Unfallursache, wie
# viele Unfaelle diese Auspraegung entweder mittels uursache1 oder uursache2
# zuzuordnen sind.
# Zu den Unfaellen jeder Auspraegung werden die Unfallfolgen und die
# Gefaehrdungen ermitteln
# Ergebnis ist eine Tabelle in der pro allgemeiner Unfallursache dessen
# Haeufigkeit und Unfallauswirkungen im Kontext anderer allgemeiner
# Unfallursachen betrachtet werden kann. Die Nutzung des Wortes Verhaeltnis
# deutet darauf hin, dass die Zahl der Beteiligungen nicht absolut interpretiert
# werden kann (da Doppeltzaehlungen auftreten), jedoch im Verhaeltnis zu den
# anderen.
verhaeltnisse_allgemeiner_uursachen <- function(stvu) {
    # Auspraegungen zu uursache1 sammeln
    erste_uursache <- stvu %>%
        select(uid, uursache1) %>%
        rename(uursache = uursache1)
    # Nichtleere Auspraegungen zu uursache2 sammeln
    zweite_uursache <- stvu %>%
        filter(uursache2 != "") %>%
        select(uid, uursache2) %>%
        rename(uursache = uursache2)
    # Durch das Zusammenkleben der 1. und 2. Unfallursache wird eine
    # moeglicherweise bestehende Rangfolge der Unfallursachen verworfen und alle
    # diese sind nun per Konstruktion gleichgewichtig.
    # Weiterhin erzwingt die Anwendung von 'distinct', dass jeder Unfall jeweils
    # mit hoechtens einem Zaehler in eine Unfallursachengruppe eingeht.
    # Anmerkung: Der Betrachtungsgegenstand sind Unfaelle und nicht die
    # Unfallbeteiligten.
    uursachen <- bind_rows(erste_uursache, zweite_uursache) %>%
        distinct(uid, uursache, .keep_all = TRUE)
    verhaeltnisse_ursachen(stvu, uursachen, quo(uursache))
}



# Wie verhaeltnisse_allgemeiner_uursachen, nur wird personenbezogenes
# Fehlverhalten, statt allgemeiner Unfallursachen, untersucht.
verhaeltnisse_personenbezogener_uursachen <- function(stvu) {
    # Auspraegungen zu bursache1 sammeln
    erste_bursache <- stvu %>%
        select(uid, bursache1) %>%
        rename(bursache = bursache1)
    # Nichtleere Auspraegungen zu bursache2 sammeln
    zweite_bursache <- stvu %>%
        select(uid, bursache2) %>%
        filter(bursache2 != "") %>%
        rename(bursache = bursache2)
    # Nichtleere Auspraegungen zu bursache3 sammeln
    dritte_bursache <- stvu %>%
        select(uid, bursache3) %>%
        filter(bursache3 != "") %>%
        rename(bursache = bursache3)
    # Durch das Zusammenkleben der 1., 2. und 3. Unfallursache wird eine
    # moeglicherweise bestehende Rangfolge der Unfallursachen verworfen und alle
    # diese sind nun per Konstruktion gleichgewichtig.
    # Weiterhin erzwingt die Anwendung von 'distinct', dass jeder Unfall jeweils
    # mit hoechtens einem Zaehler in eine Unfallursachengruppe eingeht.
    # Anmerkung: Der Betrachtungsgegenstand sind Unfaelle und nicht die
    # Unfallbeteiligten. Daher ist es unerheblich, wie mittels wie vieler
    # Beteiligter ein Unfall in die Gruppe gleicher Ursache eingeht.
    bursachen <- bind_rows(erste_bursache, zweite_bursache, dritte_bursache) %>%
        distinct(uid, bursache, .keep_all = TRUE)
    verhaeltnisse_ursachen(stvu, bursachen, quo(bursache))
}

# Ermittlung der eingangs angesprochenen Gesamtsummen.
gesamtsummen <- function(stvu) {
    relevante_uids(stvu) %>%
        inner_join(beteiligte(stvu), by = "uid") %>%
        inner_join(unfallausgaenge(stvu), by = "uid") %>%
        inner_join(fahrzeugbenutzer(stvu), by = "uid") %>%
        inner_join(gefaehrdete(stvu), by = "uid") %>%
        summarise(
            gesamtsumme_unfaelle = n(),
            gesamtsumme_uanzbet = sum(uanzbet),
            gesamtsumme_uanzgetoe = sum(uanzgetoe),
            gesamtsumme_uanzschwer = sum(uanzschwer),
            gesamtsumme_uanzleicht = sum(uanzleicht),
            gesamtsumme_uanzfzbenutz = sum(uanzfzbenutz),
            gesamtsumme_uanzgefaehrdete = sum(uanzgefaehrdete)
        )
}



################################################################################
###         Nachfolgend: Geheimhaltungsregeln und Hilfsfunktionen            ###
################################################################################

# Die Geheimhaltung findet laut FDZ Sachsen-Anhalt auf Beteiligtenebene statt.
# Jede Gruppe zu der nicht mindestens 3 Beteiligte (in Summe) beitragen, ist
# zu sperren.
mindestfallzahlregel <- function(stvu) {
    if ("sum_uanzbet" %in% colnames(stvu)) {
        filter(stvu, sum_uanzbet >= 3)
    } else if ("beteiligtenzahl" %in% colnames(stvu)) {
        filter(stvu, beteiligtenzahl >= 3)
    } else if ("sum_beteiligtenzahl" %in% colnames(stvu)) {
        filter(stvu, sum_beteiligtenzahl >= 3)
    } else {
        warning("Achtung: Mindestfallzahlregel wurde nicht angewandt.")
    }
}

# Laut FDZ Sachsen-Anhalt ist in meinem Fall die einzig anwendbare Regel die
# Mindestfallzahlregel.
zulassungsregeln <- function(stvu) {
    stvu %>% mindestfallzahlregel
}

# Verwirf Spalten, die nur zur Diagnose gedacht sind.
ohne_diagnosespalten <- function(stvu) {
  stvu %>% select(-sum_uanzbet, -sum_uanzfzbenutz)
}

# Schreibe den Dataframe `pruef_daten` unter `dateiname` als .csv-Datei in den
# Unterordner "zur_pruefung" und schreibe den Dataframe `veroeff_daten` unter
# `dateiname` als .csv-Datei in den Unterordner "zur_veroeffentlichung".
write_stvu <- function(pruef_daten, veroeff_daten, dateiname) {
    if (!missing(pruef_daten)) {
        write.csv(
            pruef_daten,
            file.path(ausgabe_ordner, "zur_pruefung", dateiname),
            row.names = FALSE
        )
    }
    write.csv(
        veroeff_daten,
        file.path(ausgabe_ordner, "zur_veroeffentlichung", dateiname),
        row.names = FALSE
    )
}



# Diese Hauptfunktion ruft die oben definierten Funktionen in der richtigen
# Reihenfolge auf und speichert die Analyseergebnisse im ausgabe_ordner als .csv
# Dateien ab.
main <- function() {
    stvu <- vorverarbeitung(stvu_dateipfad)
    pruef_verh_allg_uursachen <- verhaeltnisse_allgemeiner_uursachen(stvu)
    pruef_verh_pers_bursachen <- verhaeltnisse_personenbezogener_uursachen(stvu)
    summen <- gesamtsummen(stvu)

    dir.create(file.path(ausgabe_ordner, "zur_pruefung"))
    dir.create(file.path(ausgabe_ordner, "zur_veroeffentlichung"))

    veroeff_verh_allg_uursachen <- pruef_verh_allg_uursachen %>%
        zulassungsregeln %>%
        ohne_diagnosespalten
    write_stvu(
        pruef_daten = pruef_verh_allg_uursachen,
        veroeff_daten = veroeff_verh_allg_uursachen,
        dateiname = "verh_allg_uursachen.csv"
    )

    veroeff_verh_pers_bursachen <- pruef_verh_pers_bursachen %>%
        zulassungsregeln %>%
        ohne_diagnosespalten
    write_stvu(
        pruef_daten = pruef_verh_pers_bursachen,
        veroeff_daten = veroeff_verh_pers_bursachen,
        dateiname = "verh_pers_bursachen.csv"
    )

    write_stvu(
        veroeff_daten = summen,
        dateiname = "gesamtsummen.csv"
    )
}

main()

