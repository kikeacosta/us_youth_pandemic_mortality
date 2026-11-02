# All-cause deaths and population by year x five-year age group (0-24) x
# Hispanic origin x race, US, queried from the CDC WONDER API
# (https://wonder.cdc.gov/wonder/help/wonder-api.html).
#
#   D76  Multiple Cause of Death 1999-2020 (final, bridged race)      -> d76_*.csv
#   D157 Multiple Cause of Death 2018-2023, Single Race (final)       -> d157_*.csv
#
# D76: two queries, (1) year x age x Hispanic origin (all origins), and (2) year
# x age x Hispanic origin x race restricted to Not Hispanic or Latino (2186-2).
# D157: query (1) only; the single-race groups come from a web export of the
# 2018-2024 single-race file (dts_ac_hisp_race6_age_2018_2024.csv, see README). Rows with suppressed counts (< 10) are omitted by WONDER.
# The parameter scaffold of each form is sent in full; WONDER rejects partial
# requests. Posting a request implies acceptance of WONDER's data-use
# restrictions (the "I Agree" button of the web form), which is why the files
# are only (re)created when missing: run from the project root
#
#   Rscript data_input/all_cause_race/wonder_api_queries.R
#
# WONDER asks for at most one automated query every two minutes.

suppressMessages({library(httr); library(xml2); library(dplyr); library(readr); library(tidyr)})

out_dir <- "data_input/all_cause_race"
ages    <- c("1", "1-4", "5-9", "10-14", "15-19", "20-24")

param <- function(p, name, values){
  p[[length(p) + 1]] <- sprintf("<parameter><name>%s</name>%s</parameter>", name,
                                paste0("<value>", values, "</value>", collapse = ""))
  p
}

# D76 (bridged race): scaffold of the 1999-2020 form
build_d76 <- function(years, by_race, hisp){
  id <- "D76"; V <- function(x) paste0(id, ".", x); p <- list()
  p <- param(p, "B_1", paste0(V("V1"), "-level1")); p <- param(p, "B_2", V("V51"))
  p <- param(p, "B_3", V("V17")); p <- param(p, "B_4", if (by_race) V("V8") else "*None*")
  p <- param(p, "B_5", "*None*")
  p <- param(p, "M_1", V("M1")); p <- param(p, "M_2", V("M2")); p <- param(p, "M_3", V("M3"))
  p <- param(p, paste0("F_", V("V1")), years)
  for (v in c("V10", "V2", "V27", "V9")) p <- param(p, paste0("F_", V(v)), "*All*")
  p <- param(p, paste0("I_", V("V1")), paste(years, collapse = "; "))
  p <- param(p, paste0("I_", V("V10")), "*All* (The United States)")
  p <- param(p, paste0("I_", V("V2")), "*All* (All Causes of Death)")
  p <- param(p, paste0("I_", V("V27")), "*All* (The United States)")
  p <- param(p, paste0("I_", V("V9")), "*All* (The United States)")
  for (f in c("V10", "V1", "V27", "V2", "V9")) p <- param(p, paste0("O_", f, "_fmode"), "freg")
  p <- param(p, "O_aar", "aar_none"); p <- param(p, "O_aar_pop", "0000")
  p <- param(p, "O_age", V("V51")); p <- param(p, "O_javascript", "on"); p <- param(p, "O_location", V("V9"))
  p <- param(p, "O_precision", "1"); p <- param(p, "O_rate_per", "100000"); p <- param(p, "O_show_totals", "false")
  p <- param(p, "O_timeout", "600"); p <- param(p, "O_title", "all-cause deaths 0-24 by race")
  p <- param(p, "O_ucd", V("V2")); p <- param(p, "O_urban", V("V19"))
  p <- param(p, paste0("VM_", V("M6"), "_", V("V10")), "")
  for (v in c("V17", "V1_S", "V7", "V8")) p <- param(p, paste0("VM_", V("M6"), "_", V(v)), "*All*")
  for (v in c("V1", "V10", "V2", "V27", "V9")) p <- param(p, paste0("V_", V(v)), "")
  for (v in c("V11", "V12", "V19", "V20", "V21", "V22", "V23", "V24", "V25", "V4", "V5", "V52", "V7", "V8"))
    p <- param(p, paste0("V_", V(v)), "*All*")
  p <- param(p, paste0("V_", V("V17")), hisp)
  p <- param(p, paste0("V_", V("V51")), ages); p <- param(p, paste0("V_", V("V6")), "00")
  p <- param(p, "action-Send", "Send")
  for (v in c("V1", "V2", "V27", "V9")) p <- param(p, paste0("finder-stage-", V(v)), "codeset")
  p <- param(p, "stage", "request")
  paste0("<request-parameters>", paste(unlist(p), collapse = ""), "</request-parameters>")
}

# D157 (single race): scaffold of the 2018-2023 single-race form (multiple-cause
# finders V13/V15/V16/V26 with their paired _AND boxes, location finders V30/V31,
# single-race variables V42-V45 and the O_race option)
build_d157 <- function(years, by_race, hisp){
  id <- "D157"; V <- function(x) paste0(id, ".", x); p <- list()
  p <- param(p, "O_ucd", V("V2")); p <- param(p, paste0("F_", V("V2")), "*All*")
  p <- param(p, paste0("I_", V("V2")), "*All* (All Causes of Death)"); p <- param(p, paste0("V_", V("V2")), "")
  p <- param(p, "O_V2_fmode", "freg")
  p <- param(p, paste0("F_", V("V1")), years); p <- param(p, paste0("I_", V("V1")), paste(years, collapse = " "))
  p <- param(p, paste0("V_", V("V1")), years); p <- param(p, "O_V1_fmode", "freg")
  p <- param(p, "O_location", V("V9")); p <- param(p, paste0("F_", V("V9")), "*All*")
  p <- param(p, paste0("I_", V("V9")), "*All* (The United States)"); p <- param(p, paste0("V_", V("V9")), "")
  p <- param(p, "O_V9_fmode", "freg")
  p <- param(p, paste0("F_", V("V10")), "*All*"); p <- param(p, paste0("I_", V("V10")), "*All* (The United States)")
  p <- param(p, paste0("V_", V("V10")), "")
  p <- param(p, paste0("F_", V("V27")), "*All*"); p <- param(p, paste0("I_", V("V27")), "*All* (The United States)")
  p <- param(p, paste0("V_", V("V27")), "*All*"); p <- param(p, "O_V27_fmode", "freg")
  p <- param(p, paste0("V_", V("V6")), "00")
  for (v in c("V4", "V5", "V52", "V7", "V11", "V12", "V18", "V19", "V20", "V21", "V22", "V23", "V24", "V25",
              "V42", "V43", "V44", "V45"))
    p <- param(p, paste0("V_", V(v)), "*All*")
  p <- param(p, paste0("V_", V("V51")), ages)
  p <- param(p, paste0("V_", V("V17")), hisp)
  for (v in c("V30", "V31")) {
    p <- param(p, paste0("F_", V(v)), "*All*"); p <- param(p, paste0("V_", V(v)), "")
    p <- param(p, paste0("O_", v, "_fmode"), "freg")
  }
  p <- param(p, "O_mcd", V("V13"))
  for (v in c("V13", "V15", "V16", "V26")) {
    key <- if (v %in% c("V15", "V16")) "L_" else "F_"
    p <- param(p, paste0(key, V(v)), "*All*"); p <- param(p, paste0("V_", V(v)), "")
    p <- param(p, paste0("V_", V(v), "_AND"), ""); p <- param(p, paste0("O_", v, "_fmode"), "fadv")
  }
  p <- param(p, "O_age", V("V51")); p <- param(p, "O_urban", V("V19")); p <- param(p, "O_aar", "aar_none")
  p <- param(p, "O_aar_pop", "0000"); p <- param(p, "O_rate_per", "100000"); p <- param(p, "O_precision", "1")
  p <- param(p, "O_show_totals", "false"); p <- param(p, "O_timeout", "300"); p <- param(p, "O_mmode", "on")
  p <- param(p, "O_javascript", "on"); p <- param(p, "O_race", V("V42"))
  p <- param(p, "B_1", paste0(V("V1"), "-level1")); p <- param(p, "B_2", V("V51")); p <- param(p, "B_3", V("V17"))
  p <- param(p, "B_4", if (by_race) V("V42") else "*None*"); p <- param(p, "B_5", "*None*")
  p <- param(p, "M_1", V("M1")); p <- param(p, "M_2", V("M2")); p <- param(p, "M_3", V("M3"))
  paste0("<request-parameters>", paste(unlist(p), collapse = ""), "</request-parameters>")
}

# response: <data-table><r><c l="label"/>...<c v="value"/></r>...; labels that
# repeat the previous row are omitted, so they are carried forward
parse_tab <- function(txt, nlab){
  doc  <- read_xml(txt)
  msg  <- xml_find_all(doc, "//message")
  if (length(msg)) cat("MESSAGES:", paste(xml_text(msg), collapse = " | "), "\n")
  rows <- xml_find_all(doc, "//data-table/r")
  if (!length(rows)) return(NULL)
  labs <- rep(NA_character_, nlab); out <- list()
  for (r in rows){
    cs <- xml_find_all(r, "c"); l <- xml_attr(cs, "l"); v <- xml_attr(cs, "v")
    li <- which(!is.na(l)); m <- length(li)
    labs[(nlab - m + 1):nlab] <- l[li]
    out[[length(out) + 1]] <- c(labs, v[is.na(l)])
  }
  as_tibble(do.call(rbind, out), .name_repair = "minimal") %>%
    setNames(c(paste0("lab", seq_len(nlab)), "deaths", "population", "crude_rate"))
}

run <- function(ds, xml, nlab, out){
  f <- file.path(out_dir, paste0(out, ".csv"))
  if (file.exists(f)) { cat(out, "exists, skipped\n"); return(invisible(NULL)) }
  r <- POST(paste0("https://wonder.cdc.gov/controller/datarequest/", ds),
            body = list(request_xml = xml, accept_datause_restrictions = "true"),
            encode = "form", timeout(600))
  txt <- content(r, as = "text", encoding = "UTF-8")
  cat(ds, out, "status", status_code(r), "\n")
  tb <- parse_tab(txt, nlab)
  if (is.null(tb)) { cat("no rows returned\n"); return(invisible(NULL)) }
  write_csv(tb, f); cat("rows", nrow(tb), "\n"); print(head(tb, 3))
  Sys.sleep(125)
}

run("D76",  build_d76(as.character(2010:2020), by_race = FALSE, hisp = "*All*"),  3, "d76_ac_year_age_hisp_2010_2020")
run("D76",  build_d76(as.character(2010:2020), by_race = TRUE,  hisp = "2186-2"), 4, "d76_ac_year_age_nonhisp_race_2010_2020")
run("D157", build_d157(as.character(2018:2023), by_race = FALSE, hisp = "*All*"),  3, "d157_ac_year_age_hisp_2018_2023")
