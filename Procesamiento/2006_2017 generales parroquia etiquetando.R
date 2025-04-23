setwd('I:/Mi unidad/electoral_obs')
library(tidyverse)
library(readxl)
library(writexl)
library(dplyr)
library(Hmisc)
library(readr)
library(tidyr)
library(haven)
library(sjlabelled)

##calling datasets
presi20061v <- read_csv("CNE_bdd/2006_generales_1v.p.csv")
presi20062v<- read_csv("CNE_bdd/2006_generales_2v.p.csv")
presi20091v <- read_csv("CNE_bdd/2009_generales_1v.p.csv")
presi20131v <- read_csv("CNE_bdd/2013_generales_1v.p.csv")
presi20171v <- read_csv("CNE_bdd/2017_generales_1v.p.csv")
presi20172v<- read_csv("CNE_bdd/2017_generales_2v.p.csv")
presi20061v <- presi20061v %>% filter(DIGNIDAD_CODIGO == 1)
presi20091v <- presi20091v %>% filter(DIGNIDAD_CODIGO == 1)
presi20131v <- presi20131v %>% filter(DIGNIDAD_CODIGO == 1)
presi20171v <- presi20171v %>% filter(DIGNIDAD_CODIGO == 1)
presi20171v <- presi20171v %>% rename(OP_CODIGO = CANDIDATO_CODIGO)
presi20172v <- presi20172v %>% rename(OP_CODIGO = CANDIDATO_CODIGO)

rename_cols <- function(df) {
  df %>% rename(
    COD_PROVINCIA = PROVINCIA_CODIGO,
    COD_CANTON = CANTON_CODIGO,
    COD_PARROQUIA = PARROQUIA_CODIGO,
    COD_OP = OP_CODIGO)}
presi20061v <- presi20061v |> rename_cols()
presi20062v <- presi20062v |> rename_cols()
presi20091v <- presi20091v |> rename_cols()
presi20131v <- presi20131v |> rename_cols()
presi20171v <- presi20171v |> rename_cols()
presi20172v <- presi20172v |> rename_cols()

rename_cols <- function(df) {
  df %>% rename(
    VOTOS = CANDIDATO_VOTOS,
    NULOS = VOTOS_NULOS,
    BLANCOS = VOTOS_EN_BLANCO)}
presi20061v <- presi20061v |> rename_cols()
presi20062v <- presi20062v |> rename_cols()
presi20091v <- presi20091v |> rename_cols()
presi20131v <- presi20131v |> rename_cols()
rm(rename_cols)

selecting <- function(df) {
  df %>% select(
    COD_PROVINCIA,COD_CANTON,COD_PARROQUIA,COD_OP, VOTOS, NULOS, BLANCOS)}
presi20061v <- presi20061v |> selecting()
presi20062v <- presi20062v |> selecting()
presi20091v <- presi20091v |> selecting()
presi20131v <- presi20131v |> selecting()
presi20171v <- presi20171v |> selecting()
presi20172v <- presi20172v |> selecting()
rm(selecting)


##calling etiquetas
candidatos2006 <- read_sav("CNE_bdd/etiquetas/candidatos 2006.sav")
candidatos2006 <- as_label(candidatos2006)
candidatos2006 <- as.data.frame(candidatos2006)
candidatos2009 <- read_sav("CNE_bdd/etiquetas/candidatos 2009.sav")
candidatos2009 <- as_label(candidatos2009)
candidatos2009 <- as.data.frame(candidatos2009)
candidatos2013 <- read_sav("CNE_bdd/etiquetas/candidatos 2013.sav")
candidatos2013 <- as_label(candidatos2013)
candidatos2013 <- as.data.frame(candidatos2013)
candidatos2017 <- read_sav("CNE_bdd/etiquetas/candidatos 2017.sav")
candidatos2017 <- as_label(candidatos2017)
candidatos2017 <- as.data.frame(candidatos2017)

parroquias2006 <- read_sav("CNE_bdd/etiquetas/parroquias 2006.sav")
parroquias2006 <- as_label(parroquias2006)
parroquias2006 <- as.data.frame(parroquias2006)
parroquias2009 <- read_sav("CNE_bdd/etiquetas/parroquias 2009.sav")
parroquias2009 <- as_label(parroquias2009)
parroquias2009 <- as.data.frame(parroquias2009)
parroquias2013 <- read_sav("CNE_bdd/etiquetas/parroquias 2013.sav")
parroquias2013 <- as_label(parroquias2013)
parroquias2013 <- as.data.frame(parroquias2013)
parroquias2017 <- read_sav("CNE_bdd/etiquetas/parroquias 2017.sav")
parroquias2017 <- as_label(parroquias2017)
parroquias2017 <- as.data.frame(parroquias2017)

filtering <- function(df) {
  df %>% filter(
    DIGNIDAD_CODIGO == 1)}
rename_ <- function(df) {
  df %>% rename(
    COD_OP = OP_CODIGO)}
rename_cols <- function(df) {
  df %>% rename(
    COD_PROVINCIA = PROVINCIA_CODIGO,
    COD_CANTON = CANTON_CODIGO,
    COD_PARROQUIA = PARROQUIA_CODIGO)}
candidatos2006 <- candidatos2006 |> filtering()
candidatos2009 <- candidatos2009 |> filtering()
candidatos2013 <- candidatos2013 |> filtering()
candidatos2017 <- candidatos2017 |> filtering()
candidatos2006 <- candidatos2006 |> rename_()
candidatos2009 <- candidatos2009 |> rename_()
candidatos2013 <- candidatos2013 |> rename_()
candidatos2017 <- candidatos2017 %>% 
  rename(COD_OP = CANDIDATO_CODIGO)
parroquias2006 <- parroquias2006 |> rename_cols()
parroquias2009 <- parroquias2009 |> rename_cols()
parroquias2013 <- parroquias2013 |> rename_cols()
parroquias2017 <- parroquias2017 |> rename_cols()
select_ <- function(df) {
  df %>% select(COD_OP, CANDIDATO_NOMBRE)}
select_2 <- function(df) {
  df %>% select(COD_PROVINCIA, PROVINCIA_NOMBRE, COD_CANTON, CANTON_NOMBRE,
                COD_PARROQUIA, PARROQUIA_NOMBRE)}
candidatos2006 <- candidatos2006 |> select_()
candidatos2009 <- candidatos2009 |> select_()
candidatos2013 <- candidatos2013 |> select_()
candidatos2017 <- candidatos2017 |> select_()
parroquias2006 <- parroquias2006 |> select_2()
parroquias2009 <- parroquias2009 |> select_2()
parroquias2013 <- parroquias2013 |> select_2()
parroquias2017 <- parroquias2017 |> select_2()

presi20061v <- presi20061v %>% 
  left_join(candidatos2006, by = "COD_OP") %>%
  left_join(parroquias2006, by = c("COD_PROVINCIA", "COD_CANTON", "COD_PARROQUIA"))
presi20062v <- presi20062v %>% 
  left_join(candidatos2006, by = "COD_OP") %>%
  left_join(parroquias2006, by = c("COD_PROVINCIA", "COD_CANTON", "COD_PARROQUIA"))
presi20091v <- presi20091v %>% 
  left_join(candidatos2009, by = "COD_OP") %>%
  left_join(parroquias2009, by = c("COD_PROVINCIA", "COD_CANTON", "COD_PARROQUIA"))
presi20131v <- presi20131v %>% 
  left_join(candidatos2013, by = "COD_OP") %>%
  left_join(parroquias2013, by = c("COD_PROVINCIA", "COD_CANTON", "COD_PARROQUIA"))
presi20171v <- presi20171v %>% 
  left_join(candidatos2017, by = "COD_OP") %>%
  left_join(parroquias2017, by = c("COD_PROVINCIA", "COD_CANTON", "COD_PARROQUIA"))
presi20172v <- presi20172v %>% 
  left_join(candidatos2017, by = "COD_OP") %>%
  left_join(parroquias2017, by = c("COD_PROVINCIA", "COD_CANTON", "COD_PARROQUIA"))

rm(candidatos2006,candidatos2009,candidatos2013,candidatos2017,parroquias2006,
   parroquias2009,parroquias2013,parroquias2017,filtering,rename_,rename_cols,
   select_, select_2)

###uniendo los votos por parroquias repetidos por diferencias entre géneros

process_data <- function(df) {
  resultados_rec <- df %>%
    group_by(COD_PROVINCIA, PROVINCIA_NOMBRE, COD_CANTON, CANTON_NOMBRE
             , COD_PARROQUIA, PARROQUIA_NOMBRE,CANDIDATO_NOMBRE) %>%
    summarise(VOTOS = sum(VOTOS),
              .groups = "drop") %>%
    pivot_wider(names_from = CANDIDATO_NOMBRE, 
                values_from = VOTOS, 
                values_fill = 0)
  summary_data <- df %>%
    group_by(COD_PARROQUIA) %>%
    summarise(votos_validos = sum(VOTOS),
              votos_blancos = max(BLANCOS),
              votos_nulos = max(NULOS), 
              .groups = "drop")
  left_join(resultados_rec, summary_data, by = "COD_PARROQUIA")
}
df_list <- list(presi20061v, presi20062v, presi20091v, presi20131v, presi20171v, presi20172v)
names(df_list) <- c("presi20061v", "presi20062v", "presi20091v", "presi20131v",
                    "presi20171v","presi20172v")
result_list <- lapply(df_list, process_data)
for (i in seq_along(result_list)) {
  new_name <- paste0("res_p", substr(names(result_list)[i], 6, 11))
  assign(new_name, result_list[[i]])}

sum(res_p20061v$`CORREA DELGADO RAFAEL`)


readr::write_excel_csv(res_p20061v, "CNE_bdd/intermedias/presi20061v.p.csv", na = "")
readr::write_excel_csv(res_p20062v, "CNE_bdd/intermedias/presi20062v.p.csv", na = "")
readr::write_excel_csv(res_p20091v, "CNE_bdd/intermedias/presi20091v.p.csv", na = "")
readr::write_excel_csv(res_p20131v, "CNE_bdd/intermedias/presi20131v.p.csv", na = "")
readr::write_excel_csv(res_p20171v, "CNE_bdd/intermedias/presi20171v.p.csv", na = "")
readr::write_excel_csv(res_p20172v, "CNE_bdd/intermedias/presi20172v.p.csv", na = "")








##########33run once
presi20061v <- read_csv("CNE_bdd/procesadas/presi20061v.p.csv")
presi20062v <- read_csv("CNE_bdd/procesadas/presi20062v.p.csv")
presi20091v <- read_csv("CNE_bdd/procesadas/presi20091v.p.csv")
presi20131v <- read_csv("CNE_bdd/procesadas/presi20131v.p.csv")
presi20171v <- read_csv("CNE_bdd/procesadas/presi20171v.p.csv")
presi20172v <- read_csv("CNE_bdd/procesadas/presi20172v.p.csv")

vote_columns_2006 <- c("RC_val", "AN_val","GG_val", "LR_val")
presi20061v <- presi20061v %>%
  mutate(across(all_of(vote_columns_2006), as.numeric))
presi20061v$winner <- colnames(presi20061v[vote_columns_2006])[max.col(presi20061v[vote_columns_2006], ties.method = "first")]
presi20061v$winner_pct <- apply(presi20061v[vote_columns_2006], 1, max)
presi20061v$category <- cut(
  presi20061v$winner_pct, 
  breaks = c(0, 40, 50, 60, 100), 
  labels = c("<40", "40-50", "50-60", ">60"), 
  right = FALSE
)
presi20061v$winner_percentage <- paste0(presi20061v$winner, "_", presi20061v$category)
presi20061v <- presi20061v %>%
  select(-winner, -winner_pct,-category)
describe(presi20061v$winner_percentage)

vote_columns20062v <- c("RC_val", "AN_val")
presi20062v <- presi20062v %>%
  mutate(across(all_of(vote_columns20062v), as.numeric))
presi20062v$winner <- colnames(presi20062v[vote_columns20062v])[max.col(presi20062v[vote_columns20062v], ties.method = "first")]
presi20062v$winner_pct <- apply(presi20062v[vote_columns20062v], 1, max)
presi20062v$category <- cut(
  presi20062v$winner_pct, 
  breaks = c(0, 60, 70, 100), 
  labels = c("50-60", "60-70",">70"), 
  right = FALSE
)
presi20062v$winner_percentage <- paste0(presi20062v$winner, "_", presi20062v$category)
presi20062v <- presi20062v %>%
  select(-winner, -winner_pct,-category)
describe(presi20062v$winner_percentage)

vote_columns20091v <- c("RC_val", "AN_val","LG_val")
presi20091v <- presi20091v %>%
  mutate(across(all_of(vote_columns20091v), as.numeric))
presi20091v$winner <- colnames(presi20091v[vote_columns20091v])[max.col(presi20091v[vote_columns20091v], ties.method = "first")]
presi20091v$winner_pct <- apply(presi20091v[vote_columns20091v], 1, max)
presi20091v$category <- cut(
  presi20091v$winner_pct, 
  breaks = c(0, 40, 50, 60, 70, 100), 
  labels = c("<40", "40-50", "50-60", "60-70",">70"), 
  right = FALSE
)
presi20091v$winner_percentage <- paste0(presi20091v$winner, "_", presi20091v$category)
presi20091v <- presi20091v %>%
  select(-winner, -winner_pct,-category)
describe(presi20091v$winner_percentage)

vote_columns20131v <- c("RC_val", "GL_val")
presi20131v <- presi20131v %>%
  mutate(across(all_of(vote_columns20131v), as.numeric))
presi20131v$winner <- colnames(presi20131v[vote_columns20131v])[max.col(presi20131v[vote_columns20131v], ties.method = "first")]
presi20131v$winner_pct <- apply(presi20131v[vote_columns20131v], 1, max)
presi20131v$category <- cut(
  presi20131v$winner_pct, 
  breaks = c(0, 40, 50, 60, 70, 100), 
  labels = c("<40", "40-50", "50-60", "60-70",">70"), 
  right = FALSE
)
presi20131v$winner_percentage <- paste0(presi20131v$winner, "_", presi20131v$category)
presi20131v <- presi20131v %>%
  select(-winner, -winner_pct,-category)
describe(presi20131v$winner_percentage)

vote_columns20171v <- c("LM_val", "GL_val", "CV_val")
presi20171v <- presi20171v %>%
  mutate(across(all_of(vote_columns20171v), as.numeric))
presi20171v$winner <- colnames(presi20171v[vote_columns20171v])[max.col(presi20171v[vote_columns20171v], ties.method = "first")]
presi20171v$winner_pct <- apply(presi20171v[vote_columns20171v], 1, max)
presi20171v$category <- cut(
  presi20171v$winner_pct, 
  breaks = c(0, 40, 50, 60, 70, 100), 
  labels = c("<40", "40-50", "50-60", "60-70",">70"), 
  right = FALSE
)
presi20171v$winner_percentage <- paste0(presi20171v$winner, "_", presi20171v$category)
presi20171v <- presi20171v %>%
  select(-winner, -winner_pct,-category)
describe(presi20171v$winner_percentage)

vote_columns20172v <- c("LM_val", "GL_val")
presi20172v <- presi20172v %>%
  mutate(across(all_of(vote_columns20172v), as.numeric))
presi20172v$winner <- colnames(presi20172v[vote_columns20172v])[max.col(presi20172v[vote_columns20172v], ties.method = "first")]
presi20172v$winner_pct <- apply(presi20172v[vote_columns20172v], 1, max)
presi20172v$category <- cut(
  presi20172v$winner_pct, 
  breaks = c(0, 60, 70, 100), 
  labels = c("50-60", "60-70",">70"), 
  right = FALSE
)
presi20172v$winner_percentage <- paste0(presi20172v$winner, "_", presi20172v$category)
presi20172v <- presi20172v %>%
  select(-winner, -winner_pct,-category)
describe(presi20172v$winner_percentage)

convert_val_tot_to_numeric <- function(data) {
  data %>%
    mutate(across(
      .cols = ends_with(c("_val", "_tot")),
      .fns = as.numeric
    ))
}
presi20061v <- convert_val_tot_to_numeric(presi20061v)
presi20062v <- convert_val_tot_to_numeric(presi20062v)
presi20091v <- convert_val_tot_to_numeric(presi20091v)
presi20131v <- convert_val_tot_to_numeric(presi20131v)
presi20171v <- convert_val_tot_to_numeric(presi20171v)
presi20172v <- convert_val_tot_to_numeric(presi20172v)

readr::write_excel_csv(presi20061v, "CNE_bdd/procesadas/presi20061v.p.csv", na = "")
readr::write_excel_csv(presi20062v, "CNE_bdd/procesadas/presi20062v.p.csv", na = "")
readr::write_excel_csv(presi20091v, "CNE_bdd/procesadas/presi20091v.p.csv", na = "")
readr::write_excel_csv(presi20131v, "CNE_bdd/procesadas/presi20131v.p.csv", na = "")
readr::write_excel_csv(presi20171v, "CNE_bdd/procesadas/presi20171v.p.csv", na = "")
readr::write_excel_csv(presi20172v, "CNE_bdd/procesadas/presi20172v.p.csv", na = "")


