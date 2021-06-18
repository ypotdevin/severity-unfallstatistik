library(dplyr)
library(tidyr)

# Die Severity einer Unfallursache berechnen wir gemaess ISO 26262, Part 3,
# appendix B, table B.1, wobei wir AIS{1, 2} mit "leicht verletzt" gleichsetzen,
# AIS{3, 4} mit "schwer verletzt" und AIS{5, 6} mit "getoetet".
severity_fun <- function(ursachen) {
    g <- ursachen$sum_uanzgetoe
    s <- ursachen$sum_uanzschwer + g
    l <- ursachen$sum_uanzleicht + s
    ursachen %>%
        mutate(
            severity = case_when(
                g / sum_uanzgefaehrdete >= 0.1 ~ 3,
                s / sum_uanzgefaehrdete >= 0.1 ~ 2,
                l / sum_uanzgefaehrdete >= 0.1 ~ 1,
                TRUE                           ~ 0
            )
        ) %>%
        select(ursache, severity)
}



asil_fun <- function(ursachen) {
    se_sum <- ursachen$severity + ursachen$exposure
    ursachen %>% mutate(
        ASIL = case_when(
            se_sum == 7     ~  "D",
            se_sum == 6     ~  "C",
            se_sum == 5     ~  "B",
            se_sum == 4     ~  "A",
            se_sum %in% 2:3 ~ "QM",
            TRUE            ~  ""
        )
    )
}

main <- function() {
    verh_allg_uursachen <-
        read.csv("verh_allg_uursachen.csv") %>%
        rename(ursache = uursache)
    verh_pers_bursachen <-
        read.csv("verh_pers_bursachen.csv") %>%
        rename(ursache = bursache)
    verh_ursachen <-
        bind_rows(verh_allg_uursachen, verh_pers_bursachen) %>%
        filter(!is.na(ursache)) %>%
        arrange(ursache)
    sev <- severity_fun(verh_ursachen)
    exp <- read.csv("exposure.csv") %>%
        transform(exposure = as.numeric(exposure))
    asil <- left_join(sev, exp, by = "ursache") %>% asil_fun
    write.csv(asil, "ASIL.csv", row.names = FALSE)
}

main()
