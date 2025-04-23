setwd('I:/Mi unidad/electoral_obs')
library(tidyverse)
library(readxl)
library(writexl)
library(dplyr)
library(Hmisc)
library(readr)
library(tidyr)

###calling datasets
presi20211v <- read_csv("CNE_bdd/2021_generales_1v.csv")
presi20212v<- read_csv("CNE_bdd/2021_generales_2v.csv")
presi20231v <- read_csv("CNE_bdd/2023_generales_1v.csv")
presi20232v <- read_csv("CNE_bdd/2023_generales_2v.csv")

presi20211v <- presi20211v %>% filter(DIGNIDAD_CODIGO == 1)
presi20231v <- presi20231v %>% filter(DIGNIDAD_CODIGO == 11)
presi20232v <- presi20232v %>% filter(DIGNIDAD_CODIGO == 11)
presi20211v <- presi20211v %>% rename(CANDIDATO_NOMBRE = CANDIDATO_NOMBRE_RESULTADOS)
presi20231v <- presi20231v %>% rename(ZONA_NOMBRE = NOMBRE_ZONA)
presi20232v <- presi20232v %>% rename(ZONA_NOMBRE = NOMBRE_ZONA)
rename_cols <- function(df) {
  df %>% rename(
    COD_PROVINCIA = PROVINCIA_CODIGO,
    COD_CIRCUNSCRIPCION = CIRCUNSCRIPCION_CODIGO,
    COD_CANTON = CANTON_CODIGO,
    COD_PARROQUIA = PARROQUIA_CODIGO,
    COD_ZONA = ZONA_CODIGO,
    COD_RECINTO = RECINTO_CODIGO,
    COD_OP = OP_CODIGO,
    `Nombre Provincia`= PROVINCIA_NOMBRE,
    `Nombre Cantón`= CANTON_NOMBRE,
    `Nombre Parroquia`= PARROQUIA_NOMBRE,
    `Nombre Zona`= ZONA_NOMBRE,
    `Nombre Candidato`= CANDIDATO_NOMBRE)}
presi20211v <- presi20211v |> rename_cols()
presi20212v <- presi20212v |> rename_cols()
presi20231v <- presi20231v |> rename_cols()
presi20232v <- presi20232v |> rename_cols()
rm(rename_cols)
cod_asign <- function(df) {
  df %>% mutate(COD_ZONA_UNICO = paste0(COD_PARROQUIA, "-", COD_ZONA)) %>% 
    mutate(COD_ACTA = paste0(COD_PARROQUIA, "-", COD_ZONA,"-", JUNTA_CODIGO,"-", JUNTA_SEXO))%>% 
    mutate(COD_RECINTO = ifelse(COD_RECINTO==0, 
                         paste0(COD_PARROQUIA, "-", COD_ZONA), 
                         COD_RECINTO))%>% 
    mutate(COD_RECINTO = ifelse(is.na(COD_RECINTO), 
                                paste0(COD_PARROQUIA, "-", COD_ZONA), 
                                COD_RECINTO))}
presi20211v <- presi20211v |> cod_asign()
presi20212v <- presi20212v |> cod_asign()
presi20231v <- presi20231v |> cod_asign()
presi20232v <- presi20232v |> cod_asign()
rm(cod_asign)

####Preparando resultados por recinto pivoteados

result_recinto <- function(df) {
  df %>%
    group_by(COD_PROVINCIA, `Nombre Provincia`, COD_CIRCUNSCRIPCION,
             COD_CANTON,`Nombre Cantón`,
             COD_PARROQUIA, `Nombre Parroquia`, COD_ZONA_UNICO, COD_ZONA, `Nombre Zona`,
             COD_RECINTO,COD_OP, `Nombre Candidato`) %>%
    summarise(
      votos_organizacion = sum(VOTOS, na.rm = TRUE),
      votos_blancos = sum(BLANCOS, na.rm = TRUE),
      votos_nulos = sum(NULOS, na.rm = TRUE),
      juntas_procesadas = n_distinct(COD_ACTA),
      .groups = "drop"   
    )
  }
presi20211v.r <- presi20211v |> result_recinto()
presi20212v.r <- presi20212v |> result_recinto()
presi20231v.r <- presi20231v |> result_recinto()
presi20232v.r <- presi20232v |> result_recinto()
rm(result_recinto) 

process_data <- function(df) {
  resultados_rec <- df %>%
    group_by(COD_PROVINCIA, `Nombre Provincia`, COD_CIRCUNSCRIPCION, 
             COD_CANTON, `Nombre Cantón`, COD_PARROQUIA, `Nombre Parroquia`, 
             COD_ZONA_UNICO, COD_ZONA, `Nombre Zona`, COD_RECINTO, `Nombre Candidato`) %>%
    summarise(votos_organizacion = sum(votos_organizacion),
              .groups = "drop") %>%
    pivot_wider(names_from = `Nombre Candidato`, 
                values_from = votos_organizacion, 
                values_fill = 0)
  summary_data <- df %>%
    group_by(COD_RECINTO) %>%
    summarise(votos_validos = sum(votos_organizacion),
              votos_blancos = max(votos_blancos),
              votos_nulos = max(votos_nulos), 
              .groups = "drop")
  left_join(resultados_rec, summary_data, by = "COD_RECINTO")
}
df_list <- list(presi20211v.r, presi20212v.r, presi20231v.r, presi20232v.r)
names(df_list) <- c("presi20211v.r", "presi20212v.r", "presi20231v.r", "presi20232v.r")
result_list <- lapply(df_list, process_data)
for (i in seq_along(result_list)) {
  new_name <- paste0("resultados_rec_", substr(names(result_list)[i], 6, 11))
  assign(new_name, result_list[[i]])}

result_list <- list(resultados_rec_20211v, resultados_rec_20212v,
                    resultados_rec_20231v, resultados_rec_20232v)
names(result_list) <- c("resultados_rec_20211v", "resultados_rec_20212v",
                        "resultados_rec_20231v", "resultados_rec_20232v")
add_total_votes <- function(df) {
  df %>% mutate(sufragantes = votos_validos + votos_blancos + votos_nulos)}
result_list <- lapply(result_list, add_total_votes)
list2env(result_list, envir = .GlobalEnv)

rm(i, new_name, add_total_votes,process_data, df_list, result_list,
   presi20211v,presi20212v,presi20231v,presi20232v,
   presi20211v.r,presi20212v.r,presi20231v.r,presi20232v.r)

readr::write_excel_csv(resultados_rec_20211v, "CNE_bdd/intermedias/presi20211v.rr.csv", na = "")
readr::write_excel_csv(resultados_rec_20212v, "CNE_bdd/intermedias/presi20212v.rr.csv", na = "")
readr::write_excel_csv(resultados_rec_20231v, "CNE_bdd/intermedias/presi20231v.rr.csv", na = "")
readr::write_excel_csv(resultados_rec_20232v, "CNE_bdd/intermedias/presi20232v.rr.csv", na = "")


###preparando por juntas pivoteadas


result_junta <- function(df) {
  df %>%
    group_by(COD_PROVINCIA, `Nombre Provincia`, COD_CIRCUNSCRIPCION,
             COD_CANTON,`Nombre Cantón`,
             COD_PARROQUIA, `Nombre Parroquia`, COD_ZONA_UNICO, COD_ZONA,  
             `Nombre Zona`, COD_RECINTO, COD_ACTA, COD_OP,  `Nombre Candidato`) %>%
    summarise(
      votos_organizacion = sum(VOTOS, na.rm = TRUE),
      votos_blancos = sum(BLANCOS, na.rm = TRUE),
      votos_nulos = sum(NULOS, na.rm = TRUE),
      .groups = "drop"   
    )
}
presi20211v.j <- presi20211v |> result_junta()
presi20212v.j <- presi20212v |> result_junta()
presi20231v.j <- presi20231v |> result_junta()
presi20232v.j <- presi20232v |> result_junta()
rm(result_junta) 


process_data <- function(df) {
  resultados_j <- df %>%
    group_by(COD_PROVINCIA, `Nombre Provincia`, COD_CIRCUNSCRIPCION, 
             COD_CANTON, `Nombre Cantón`, COD_PARROQUIA, `Nombre Parroquia`, 
             COD_ZONA_UNICO, COD_ZONA, `Nombre Zona`, COD_RECINTO, COD_ACTA, `Nombre Candidato`) %>%
    summarise(votos_organizacion = sum(votos_organizacion),
              .groups = "drop") %>%
    pivot_wider(names_from = `Nombre Candidato`, 
                values_from = votos_organizacion, 
                values_fill = 0)
  summary_data <- df %>%
    group_by(COD_ACTA) %>%
    summarise(votos_validos = sum(votos_organizacion),
              votos_blancos = max(votos_blancos),
              votos_nulos = max(votos_nulos), 
              .groups = "drop")
  left_join(resultados_j, summary_data, by = "COD_ACTA")
}
df_list <- list(presi20211v.j, presi20212v.j, presi20231v.j, presi20232v.j)
names(df_list) <- c("presi20211v.j", "presi20212v.j", "presi20231v.j", "presi20232v.j")
result_list <- lapply(df_list, process_data)
for (i in seq_along(result_list)) {
  new_name <- paste0("resultados_j_", substr(names(result_list)[i], 6, 11))
  assign(new_name, result_list[[i]])}

result_list <- list(resultados_j_20211v, resultados_j_20212v,
                    resultados_j_20231v, resultados_j_20232v)
names(result_list) <- c("resultados_j_20211v", "resultados_j_20212v",
                        "resultados_j_20231v", "resultados_j_20232v")
add_total_votes <- function(df) {
  df %>% mutate(sufragantes = votos_validos + votos_blancos + votos_nulos)}
result_list <- lapply(result_list, add_total_votes)
list2env(result_list, envir = .GlobalEnv)

rm(i, new_name, add_total_votes,process_data, df_list, result_list,
   presi20211v,presi20212v,presi20231v,presi20232v,
   presi20211v.r,presi20212v.r,presi20231v.r,presi20232v.r)

readr::write_excel_csv(resultados_j_20211v, "CNE_bdd/intermedias/presi20211v.jj.csv", na = "")
readr::write_excel_csv(resultados_j_20212v, "CNE_bdd/intermedias/presi20212v.jj.csv", na = "")
readr::write_excel_csv(resultados_j_20231v, "CNE_bdd/intermedias/presi20231v.jj.csv", na = "")
readr::write_excel_csv(resultados_j_20232v, "CNE_bdd/intermedias/presi20232v.jj.csv", na = "")
