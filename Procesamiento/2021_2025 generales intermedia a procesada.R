setwd('I:/Mi unidad/electoral_obs')
library(tidyverse)
library(readxl)
library(writexl)
library(dplyr)
library(Hmisc)
library(readr)
library(tidyr)

###RECINTOS

presi20211v.r <- read_csv("CNE_bdd/intermedias/presi20211v.r.csv")
presi20212v.r <- read_csv("CNE_bdd/intermedias/presi20212v.r.csv")
presi20231v.r <- read_csv("CNE_bdd/intermedias/presi20231v.r.csv")
presi20232v.r <- read_csv("CNE_bdd/intermedias/presi20232v.r.csv")
presi20211v.j <- read_csv("CNE_bdd/intermedias/presi20211v.j.csv")
presi20212v.j <- read_csv("CNE_bdd/intermedias/presi20212v.j.csv")
presi20231v.j <- read_csv("CNE_bdd/intermedias/presi20231v.j.csv")
presi20232v.j <- read_csv("CNE_bdd/intermedias/presi20232v.j.csv")




##CONVIRTIENDO A PARROQUIAS

presi20211v.p <- presi20211v.r %>%
    group_by(COD_PROVINCIA, `Nombre Provincia`, COD_CIRCUNSCRIPCION,
             COD_CANTON,`Nombre Cantón`,
             COD_PARROQUIA, `Nombre Parroquia`) %>%
    summarise(
      `ANDRES ARAUZ GALARZA` = sum(`ANDRES ARAUZ GALARZA`, na.rm = TRUE),
      `GUILLERMO LASSO MENDOZA` = sum(`GUILLERMO LASSO MENDOZA`, na.rm = TRUE),
      `YAKU PEREZ` = sum(`YAKU PEREZ`, na.rm = TRUE),
      `XAVIER HERVAS` = sum(`XAVIER HERVAS`, na.rm = TRUE),
      `OTROS` = sum(`OTROS`, na.rm = TRUE),
      votos_validos = sum(votos_validos, na.rm = TRUE),
      votos_blancos = sum(votos_blancos, na.rm = TRUE),
      votos_nulos = sum(votos_nulos, na.rm = TRUE),
      sufragantes = sum(sufragantes, na.rm = TRUE),
      num_zonas = n_distinct(COD_ZONA_UNICO),
      num_recintos = n_distinct(COD_RECINTO),
      .groups = "drop"   
    )
presi20212v.p <- presi20212v.r %>%
  group_by(COD_PROVINCIA, `Nombre Provincia`, COD_CIRCUNSCRIPCION,
           COD_CANTON,`Nombre Cantón`,
           COD_PARROQUIA, `Nombre Parroquia`) %>%
  summarise(
    `ARAUZ GALARZA ANDRES DAVID` = sum(`ARAUZ GALARZA ANDRES DAVID`, na.rm = TRUE),
    `LASSO MENDOZA GUILLERMO ALBERTO SANTIAGO` = sum(`LASSO MENDOZA GUILLERMO ALBERTO SANTIAGO`, na.rm = TRUE),
    votos_validos = sum(votos_validos, na.rm = TRUE),
    votos_blancos = sum(votos_blancos, na.rm = TRUE),
    votos_nulos = sum(votos_nulos, na.rm = TRUE),
    sufragantes = sum(sufragantes, na.rm = TRUE),
    num_zonas = n_distinct(COD_ZONA_UNICO),
    num_recintos = n_distinct(COD_RECINTO),
    .groups = "drop"   
  )
presi20231v.p <- presi20231v.r %>%
  group_by(COD_PROVINCIA, `Nombre Provincia`, COD_CIRCUNSCRIPCION,
           COD_CANTON,`Nombre Cantón`,
           COD_PARROQUIA, `Nombre Parroquia`) %>%
  summarise(
    `DANIEL NOBOA AZIN` = sum(`DANIEL NOBOA AZIN`, na.rm = TRUE),
    `LUISA GONZALEZ` = sum(`LUISA GONZALEZ`, na.rm = TRUE),
    `FERNANDO VILLAVICENCIO` = sum(`FERNANDO VILLAVICENCIO`, na.rm = TRUE),
    `JAN TOPIC` = sum(`JAN TOPIC`, na.rm = TRUE),
    `OTROS` = sum(`OTROS`, na.rm = TRUE),
    votos_validos = sum(votos_validos, na.rm = TRUE),
    votos_blancos = sum(votos_blancos, na.rm = TRUE),
    votos_nulos = sum(votos_nulos, na.rm = TRUE),
    sufragantes = sum(sufragantes, na.rm = TRUE),
    num_zonas = n_distinct(COD_ZONA_UNICO),
    num_recintos = n_distinct(COD_RECINTO),
    .groups = "drop"   
  )
presi20232v.p <- presi20232v.r %>%
  group_by(COD_PROVINCIA, `Nombre Provincia`, COD_CIRCUNSCRIPCION,
           COD_CANTON,`Nombre Cantón`,
           COD_PARROQUIA, `Nombre Parroquia`) %>%
  summarise(
    `DANIEL NOBOA AZIN` = sum(`DANIEL NOBOA AZIN`, na.rm = TRUE),
    `LUISA GONZALEZ` = sum(`LUISA GONZALEZ`, na.rm = TRUE),
    votos_validos = sum(votos_validos, na.rm = TRUE),
    votos_blancos = sum(votos_blancos, na.rm = TRUE),
    votos_nulos = sum(votos_nulos, na.rm = TRUE),
    sufragantes = sum(sufragantes, na.rm = TRUE),
    num_zonas = n_distinct(COD_ZONA_UNICO),
    num_recintos = n_distinct(COD_RECINTO),
    .groups = "drop"   
  )





##Convirtiendo a cantones

presi20211v.c <- presi20211v.r %>%
  group_by(COD_PROVINCIA, `Nombre Provincia`, COD_CIRCUNSCRIPCION,
           COD_CANTON,`Nombre Cantón`) %>%
  summarise(
    `ANDRES ARAUZ GALARZA` = sum(`ANDRES ARAUZ GALARZA`, na.rm = TRUE),
    `GUILLERMO LASSO MENDOZA` = sum(`GUILLERMO LASSO MENDOZA`, na.rm = TRUE),
    `YAKU PEREZ` = sum(`YAKU PEREZ`, na.rm = TRUE),
    `XAVIER HERVAS` = sum(`XAVIER HERVAS`, na.rm = TRUE),
    `OTROS` = sum(`OTROS`, na.rm = TRUE),
    votos_validos = sum(votos_validos, na.rm = TRUE),
    votos_blancos = sum(votos_blancos, na.rm = TRUE),
    votos_nulos = sum(votos_nulos, na.rm = TRUE),
    sufragantes = sum(sufragantes, na.rm = TRUE),
    num_zonas = n_distinct(COD_ZONA_UNICO),
    num_recintos = n_distinct(COD_RECINTO),
    .groups = "drop"   
  )
presi20212v.c <- presi20212v.r %>%
  group_by(COD_PROVINCIA, `Nombre Provincia`, COD_CIRCUNSCRIPCION,
           COD_CANTON,`Nombre Cantón`) %>%
  summarise(
    `ARAUZ GALARZA ANDRES DAVID` = sum(`ARAUZ GALARZA ANDRES DAVID`, na.rm = TRUE),
    `LASSO MENDOZA GUILLERMO ALBERTO SANTIAGO` = sum(`LASSO MENDOZA GUILLERMO ALBERTO SANTIAGO`, na.rm = TRUE),
    votos_validos = sum(votos_validos, na.rm = TRUE),
    votos_blancos = sum(votos_blancos, na.rm = TRUE),
    votos_nulos = sum(votos_nulos, na.rm = TRUE),
    sufragantes = sum(sufragantes, na.rm = TRUE),
    num_zonas = n_distinct(COD_ZONA_UNICO),
    num_recintos = n_distinct(COD_RECINTO),
    .groups = "drop"   
  )
presi20231v.c <- presi20231v.r %>%
  group_by(COD_PROVINCIA, `Nombre Provincia`, COD_CIRCUNSCRIPCION,
           COD_CANTON,`Nombre Cantón`) %>%
  summarise(
    `DANIEL NOBOA AZIN` = sum(`DANIEL NOBOA AZIN`, na.rm = TRUE),
    `LUISA GONZALEZ` = sum(`LUISA GONZALEZ`, na.rm = TRUE),
    `FERNANDO VILLAVICENCIO` = sum(`FERNANDO VILLAVICENCIO`, na.rm = TRUE),
    `JAN TOPIC` = sum(`JAN TOPIC`, na.rm = TRUE),
    `OTROS` = sum(`OTROS`, na.rm = TRUE),
    votos_validos = sum(votos_validos, na.rm = TRUE),
    votos_blancos = sum(votos_blancos, na.rm = TRUE),
    votos_nulos = sum(votos_nulos, na.rm = TRUE),
    sufragantes = sum(sufragantes, na.rm = TRUE),
    num_zonas = n_distinct(COD_ZONA_UNICO),
    num_recintos = n_distinct(COD_RECINTO),
    .groups = "drop"   
  )
presi20232v.c <- presi20232v.r %>%
  group_by(COD_PROVINCIA, `Nombre Provincia`, COD_CIRCUNSCRIPCION,
           COD_CANTON,`Nombre Cantón`) %>%
  summarise(
    `DANIEL NOBOA AZIN` = sum(`DANIEL NOBOA AZIN`, na.rm = TRUE),
    `LUISA GONZALEZ` = sum(`LUISA GONZALEZ`, na.rm = TRUE),
    votos_validos = sum(votos_validos, na.rm = TRUE),
    votos_blancos = sum(votos_blancos, na.rm = TRUE),
    votos_nulos = sum(votos_nulos, na.rm = TRUE),
    sufragantes = sum(sufragantes, na.rm = TRUE),
    num_zonas = n_distinct(COD_ZONA_UNICO),
    num_recintos = n_distinct(COD_RECINTO),
    .groups = "drop"   
  )




##Calculando porcentajes
calculate_percent <- function(numerator, denominator) {
  round(as.numeric(numerator) / as.numeric(denominator) * 100, 2)
}

presi20211v.j <- presi20211v.j %>%
  mutate(
    AA_val = calculate_percent(`ANDRES ARAUZ GALARZA`, votos_validos),
    GL_val = calculate_percent(`GUILLERMO LASSO MENDOZA`, votos_validos),
    YP_val = calculate_percent(`YAKU PEREZ`, votos_validos),
    XH_val = calculate_percent(`XAVIER HERVAS`, votos_validos),
    OT_val = calculate_percent(`OTROS`, votos_validos),
    AA_tot = calculate_percent(`ANDRES ARAUZ GALARZA`, sufragantes),
    GL_tot = calculate_percent(`GUILLERMO LASSO MENDOZA`, sufragantes),
    YP_tot = calculate_percent(`YAKU PEREZ`, sufragantes),
    XH_tot = calculate_percent(`XAVIER HERVAS`, sufragantes),
    OT_tot = calculate_percent(`OTROS`, sufragantes),
    BL_tot = calculate_percent(votos_blancos, sufragantes),
    NU_tot = calculate_percent(votos_nulos, sufragantes)
  )
presi20211v.r <- presi20211v.r %>%
  mutate(
    AA_val = calculate_percent(`ANDRES ARAUZ GALARZA`, votos_validos),
    GL_val = calculate_percent(`GUILLERMO LASSO MENDOZA`, votos_validos),
    YP_val = calculate_percent(`YAKU PEREZ`, votos_validos),
    XH_val = calculate_percent(`XAVIER HERVAS`, votos_validos),
    OT_val = calculate_percent(`OTROS`, votos_validos),
    AA_tot = calculate_percent(`ANDRES ARAUZ GALARZA`, sufragantes),
    GL_tot = calculate_percent(`GUILLERMO LASSO MENDOZA`, sufragantes),
    YP_tot = calculate_percent(`YAKU PEREZ`, sufragantes),
    XH_tot = calculate_percent(`XAVIER HERVAS`, sufragantes),
    OT_tot = calculate_percent(`OTROS`, sufragantes),
    BL_tot = calculate_percent(votos_blancos, sufragantes),
    NU_tot = calculate_percent(votos_nulos, sufragantes)
  )
presi20211v.p <- presi20211v.p %>%
  mutate(
    AA_val = calculate_percent(`ANDRES ARAUZ GALARZA`, votos_validos),
    GL_val = calculate_percent(`GUILLERMO LASSO MENDOZA`, votos_validos),
    YP_val = calculate_percent(`YAKU PEREZ`, votos_validos),
    XH_val = calculate_percent(`XAVIER HERVAS`, votos_validos),
    OT_val = calculate_percent(`OTROS`, votos_validos),
    AA_tot = calculate_percent(`ANDRES ARAUZ GALARZA`, sufragantes),
    GL_tot = calculate_percent(`GUILLERMO LASSO MENDOZA`, sufragantes),
    YP_tot = calculate_percent(`YAKU PEREZ`, sufragantes),
    XH_tot = calculate_percent(`XAVIER HERVAS`, sufragantes),
    OT_tot = calculate_percent(`OTROS`, sufragantes),
    BL_tot = calculate_percent(votos_blancos, sufragantes),
    NU_tot = calculate_percent(votos_nulos, sufragantes)
  )
presi20211v.c <- presi20211v.c %>%
  mutate(
    AA_val = calculate_percent(`ANDRES ARAUZ GALARZA`, votos_validos),
    GL_val = calculate_percent(`GUILLERMO LASSO MENDOZA`, votos_validos),
    YP_val = calculate_percent(`YAKU PEREZ`, votos_validos),
    XH_val = calculate_percent(`XAVIER HERVAS`, votos_validos),
    OT_val = calculate_percent(`OTROS`, votos_validos),
    AA_tot = calculate_percent(`ANDRES ARAUZ GALARZA`, sufragantes),
    GL_tot = calculate_percent(`GUILLERMO LASSO MENDOZA`, sufragantes),
    YP_tot = calculate_percent(`YAKU PEREZ`, sufragantes),
    XH_tot = calculate_percent(`XAVIER HERVAS`, sufragantes),
    OT_tot = calculate_percent(`OTROS`, sufragantes),
    BL_tot = calculate_percent(votos_blancos, sufragantes),
    NU_tot = calculate_percent(votos_nulos, sufragantes)
  )
presi20212v.j <- presi20212v.j %>%
  mutate(
    AA_val = calculate_percent(`ARAUZ GALARZA ANDRES DAVID`, votos_validos),
    GL_val = calculate_percent(`LASSO MENDOZA GUILLERMO ALBERTO SANTIAGO`, votos_validos),
    AA_tot = calculate_percent(`ARAUZ GALARZA ANDRES DAVID`, sufragantes),
    GL_tot = calculate_percent(`LASSO MENDOZA GUILLERMO ALBERTO SANTIAGO`, sufragantes),
    BL_tot = calculate_percent(votos_blancos, sufragantes),
    NU_tot = calculate_percent(votos_nulos, sufragantes)
  )
presi20212v.r <- presi20212v.r %>%
  mutate(
    AA_val = calculate_percent(`ARAUZ GALARZA ANDRES DAVID`, votos_validos),
    GL_val = calculate_percent(`LASSO MENDOZA GUILLERMO ALBERTO SANTIAGO`, votos_validos),
    AA_tot = calculate_percent(`ARAUZ GALARZA ANDRES DAVID`, sufragantes),
    GL_tot = calculate_percent(`LASSO MENDOZA GUILLERMO ALBERTO SANTIAGO`, sufragantes),
    BL_tot = calculate_percent(votos_blancos, sufragantes),
    NU_tot = calculate_percent(votos_nulos, sufragantes)
  )
presi20212v.p <- presi20212v.p %>%
  mutate(
    AA_val = calculate_percent(`ARAUZ GALARZA ANDRES DAVID`, votos_validos),
    GL_val = calculate_percent(`LASSO MENDOZA GUILLERMO ALBERTO SANTIAGO`, votos_validos),
    AA_tot = calculate_percent(`ARAUZ GALARZA ANDRES DAVID`, sufragantes),
    GL_tot = calculate_percent(`LASSO MENDOZA GUILLERMO ALBERTO SANTIAGO`, sufragantes),
    BL_tot = calculate_percent(votos_blancos, sufragantes),
    NU_tot = calculate_percent(votos_nulos, sufragantes)
  )
presi20212v.c <- presi20212v.c %>%
  mutate(
    AA_val = calculate_percent(`ARAUZ GALARZA ANDRES DAVID`, votos_validos),
    GL_val = calculate_percent(`LASSO MENDOZA GUILLERMO ALBERTO SANTIAGO`, votos_validos),
    AA_tot = calculate_percent(`ARAUZ GALARZA ANDRES DAVID`, sufragantes),
    GL_tot = calculate_percent(`LASSO MENDOZA GUILLERMO ALBERTO SANTIAGO`, sufragantes),
    BL_tot = calculate_percent(votos_blancos, sufragantes),
    NU_tot = calculate_percent(votos_nulos, sufragantes)
  )
presi20231v.j <- presi20231v.j %>%
  mutate(
    DN_val = calculate_percent(`DANIEL NOBOA AZIN`, votos_validos),
    LG_val = calculate_percent(`LUISA GONZALEZ`, votos_validos),
    FV_val = calculate_percent(`FERNANDO VILLAVICENCIO`, votos_validos),
    JT_val = calculate_percent(`JAN TOPIC`, votos_validos),
    OT_val = calculate_percent(`OTROS`, votos_validos),
    DN_tot = calculate_percent(`DANIEL NOBOA AZIN`, sufragantes),
    LG_tot = calculate_percent(`LUISA GONZALEZ`, sufragantes),
    FV_tot = calculate_percent(`FERNANDO VILLAVICENCIO`, sufragantes),
    JT_tot = calculate_percent(`JAN TOPIC`, sufragantes),
    OT_tot = calculate_percent(`OTROS`, sufragantes),
    BL_tot = calculate_percent(votos_blancos, sufragantes),
    NU_tot = calculate_percent(votos_nulos, sufragantes)
  )
presi20231v.r <- presi20231v.r %>%
  mutate(
    DN_val = calculate_percent(`DANIEL NOBOA AZIN`, votos_validos),
    LG_val = calculate_percent(`LUISA GONZALEZ`, votos_validos),
    FV_val = calculate_percent(`FERNANDO VILLAVICENCIO`, votos_validos),
    JT_val = calculate_percent(`JAN TOPIC`, votos_validos),
    OT_val = calculate_percent(`OTROS`, votos_validos),
    DN_tot = calculate_percent(`DANIEL NOBOA AZIN`, sufragantes),
    LG_tot = calculate_percent(`LUISA GONZALEZ`, sufragantes),
    FV_tot = calculate_percent(`FERNANDO VILLAVICENCIO`, sufragantes),
    JT_tot = calculate_percent(`JAN TOPIC`, sufragantes),
    OT_tot = calculate_percent(`OTROS`, sufragantes),
    BL_tot = calculate_percent(votos_blancos, sufragantes),
    NU_tot = calculate_percent(votos_nulos, sufragantes)
  )
presi20231v.p <- presi20231v.p %>%
  mutate(
    DN_val = calculate_percent(`DANIEL NOBOA AZIN`, votos_validos),
    LG_val = calculate_percent(`LUISA GONZALEZ`, votos_validos),
    FV_val = calculate_percent(`FERNANDO VILLAVICENCIO`, votos_validos),
    JT_val = calculate_percent(`JAN TOPIC`, votos_validos),
    OT_val = calculate_percent(`OTROS`, votos_validos),
    DN_tot = calculate_percent(`DANIEL NOBOA AZIN`, sufragantes),
    LG_tot = calculate_percent(`LUISA GONZALEZ`, sufragantes),
    FV_tot = calculate_percent(`FERNANDO VILLAVICENCIO`, sufragantes),
    JT_tot = calculate_percent(`JAN TOPIC`, sufragantes),
    OT_tot = calculate_percent(`OTROS`, sufragantes),
    BL_tot = calculate_percent(votos_blancos, sufragantes),
    NU_tot = calculate_percent(votos_nulos, sufragantes)
  )
presi20231v.c <- presi20231v.c %>%
  mutate(
    DN_val = calculate_percent(`DANIEL NOBOA AZIN`, votos_validos),
    LG_val = calculate_percent(`LUISA GONZALEZ`, votos_validos),
    FV_val = calculate_percent(`FERNANDO VILLAVICENCIO`, votos_validos),
    JT_val = calculate_percent(`JAN TOPIC`, votos_validos),
    OT_val = calculate_percent(`OTROS`, votos_validos),
    DN_tot = calculate_percent(`DANIEL NOBOA AZIN`, sufragantes),
    LG_tot = calculate_percent(`LUISA GONZALEZ`, sufragantes),
    FV_tot = calculate_percent(`FERNANDO VILLAVICENCIO`, sufragantes),
    JT_tot = calculate_percent(`JAN TOPIC`, sufragantes),
    OT_tot = calculate_percent(`OTROS`, sufragantes),
    BL_tot = calculate_percent(votos_blancos, sufragantes),
    NU_tot = calculate_percent(votos_nulos, sufragantes)
  )
presi20232v.j <- presi20232v.j %>%
  mutate(
    DN_val = calculate_percent(`DANIEL NOBOA AZIN`, votos_validos),
    LG_val = calculate_percent(`LUISA GONZALEZ`, votos_validos),
    DN_tot = calculate_percent(`DANIEL NOBOA AZIN`, sufragantes),
    LG_tot = calculate_percent(`LUISA GONZALEZ`, sufragantes),
    BL_tot = calculate_percent(votos_blancos, sufragantes),
    NU_tot = calculate_percent(votos_nulos, sufragantes)
  )
presi20232v.r <- presi20232v.r %>%
  mutate(
    DN_val = calculate_percent(`DANIEL NOBOA AZIN`, votos_validos),
    LG_val = calculate_percent(`LUISA GONZALEZ`, votos_validos),
    DN_tot = calculate_percent(`DANIEL NOBOA AZIN`, sufragantes),
    LG_tot = calculate_percent(`LUISA GONZALEZ`, sufragantes),
    BL_tot = calculate_percent(votos_blancos, sufragantes),
    NU_tot = calculate_percent(votos_nulos, sufragantes)
  )
presi20232v.p <- presi20232v.p %>%
  mutate(
    DN_val = calculate_percent(`DANIEL NOBOA AZIN`, votos_validos),
    LG_val = calculate_percent(`LUISA GONZALEZ`, votos_validos),
    DN_tot = calculate_percent(`DANIEL NOBOA AZIN`, sufragantes),
    LG_tot = calculate_percent(`LUISA GONZALEZ`, sufragantes),
    BL_tot = calculate_percent(votos_blancos, sufragantes),
    NU_tot = calculate_percent(votos_nulos, sufragantes)
  )
presi20232v.c <- presi20232v.c %>%
  mutate(
    DN_val = calculate_percent(`DANIEL NOBOA AZIN`, votos_validos),
    LG_val = calculate_percent(`LUISA GONZALEZ`, votos_validos),
    DN_tot = calculate_percent(`DANIEL NOBOA AZIN`, sufragantes),
    LG_tot = calculate_percent(`LUISA GONZALEZ`, sufragantes),
    BL_tot = calculate_percent(votos_blancos, sufragantes),
    NU_tot = calculate_percent(votos_nulos, sufragantes)
  )


####Preparando variable de ganador con porcentaje por parroquias

vote_columns_20211v <- c("AA_val", "GL_val", "YP_val", "XH_val", "OT_val")
presi20211v.p$winner <- colnames(presi20211v.p[vote_columns_20211v])[max.col(presi20211v.p[vote_columns_20211v], ties.method = "first")]
presi20211v.p$winner_pct <- apply(presi20211v.p[vote_columns_20211v], 1, max)
presi20211v.p$category <- cut(
  presi20211v.p$winner_pct, 
  breaks = c(0, 50, 60, 100), 
  labels = c("<50", "50-60", ">60"), 
  right = FALSE
)
presi20211v.p$winner_percentage <- paste0(presi20211v.p$winner, "_", presi20211v.p$category)
presi20211v.p <- presi20211v.p %>%
  select(-winner, -winner_pct,-category)
describe(presi20211v.p$winner_percentage)

vote_columns_20212v <- c("AA_val", "GL_val")
presi20212v.p$winner <- colnames(presi20212v.p[vote_columns_20212v])[max.col(presi20212v.p[vote_columns_20212v], ties.method = "first")]
presi20212v.p$winner_pct <- apply(presi20212v.p[vote_columns_20212v], 1, max)
presi20212v.p$category <- cut(
  presi20212v.p$winner_pct, 
  breaks = c(50, 60, 70, 100), 
  labels = c("50-60", "60-70", ">70"), 
  right = FALSE
)
presi20212v.p$winner_percentage <- paste0(presi20212v.p$winner, "_", presi20212v.p$category)
presi20212v.p <- presi20212v.p %>%
  select(-winner, -winner_pct,-category)
describe(presi20212v.p$winner_percentage)

vote_columns_20231v <- c("LG_val", "DN_val", "FV_val", "JT_val", "OT_val")
presi20231v.p$winner <- colnames(presi20231v.p[vote_columns_20231v])[max.col(presi20231v.p[vote_columns_20231v], ties.method = "first")]
presi20231v.p$winner_pct <- apply(presi20231v.p[vote_columns_20231v], 1, max)
presi20231v.p$category <- cut(
  presi20231v.p$winner_pct, 
  breaks = c(0, 50, 60, 100), 
  labels = c("<50", "50-60", ">60"), 
  right = FALSE
)
presi20231v.p$winner_percentage <- paste0(presi20231v.p$winner, "_", presi20231v.p$category)
presi20231v.p <- presi20231v.p %>%
  select(-winner, -winner_pct,-category)
describe(presi20231v.p$winner_percentage)

vote_columns_20232v <- c("LG_val", "DN_val")
presi20232v.p$winner <- colnames(presi20232v.p[vote_columns_20232v])[max.col(presi20232v.p[vote_columns_20232v], ties.method = "first")]
presi20232v.p$winner_pct <- apply(presi20232v.p[vote_columns_20232v], 1, max)
presi20232v.p$category <- cut(
  presi20232v.p$winner_pct, 
  breaks = c(50, 60, 70, 100), 
  labels = c("50-60", "60-70", ">70"), 
  right = FALSE
)
presi20232v.p$winner_percentage <- paste0(presi20232v.p$winner, "_", presi20232v.p$category)
presi20232v.p <- presi20232v.p %>%
  select(-winner, -winner_pct,-category)
describe(presi20232v.p$winner_percentage)




vote_columns_20211v <- c("AA_val", "GL_val", "YP_val", "XH_val", "OT_val")
presi20211v.r$winner <- colnames(presi20211v.r[vote_columns_20211v])[max.col(presi20211v.r[vote_columns_20211v], ties.method = "first")]
presi20211v.r$winner_pct <- apply(presi20211v.r[vote_columns_20211v], 1, max)
presi20211v.r$category <- cut(
  presi20211v.r$winner_pct, 
  breaks = c(0, 50, 60, 100), 
  labels = c("<50", "50-60", ">60"), 
  right = FALSE
)
presi20211v.r$winner_percentage <- paste0(presi20211v.r$winner, "_", presi20211v.r$category)
presi20211v.r <- presi20211v.r %>%
  select(-winner, -winner_pct,-category)
describe(presi20211v.r$winner_percentage)

vote_columns_20212v <- c("AA_val", "GL_val")
presi20212v.r$winner <- colnames(presi20212v.r[vote_columns_20212v])[max.col(presi20212v.r[vote_columns_20212v], ties.method = "first")]
presi20212v.r$winner_pct <- apply(presi20212v.r[vote_columns_20212v], 1, max)
presi20212v.r$category <- cut(
  presi20212v.r$winner_pct, 
  breaks = c(50, 60, 70, 100), 
  labels = c("50-60", "60-70", ">70"), 
  right = FALSE
)
presi20212v.r$winner_percentage <- paste0(presi20212v.r$winner, "_", presi20212v.r$category)
presi20212v.r <- presi20212v.r %>%
  select(-winner, -winner_pct,-category)
describe(presi20212v.r$winner_percentage)

vote_columns_20231v <- c("LG_val", "DN_val", "FV_val", "JT_val", "OT_val")
presi20231v.r$winner <- colnames(presi20231v.r[vote_columns_20231v])[max.col(presi20231v.r[vote_columns_20231v], ties.method = "first")]
presi20231v.r$winner_pct <- apply(presi20231v.r[vote_columns_20231v], 1, max)
presi20231v.r$category <- cut(
  presi20231v.r$winner_pct, 
  breaks = c(0, 50, 60, 100), 
  labels = c("<50", "50-60", ">60"), 
  right = FALSE
)
presi20231v.r$winner_percentage <- paste0(presi20231v.r$winner, "_", presi20231v.r$category)
presi20231v.r <- presi20231v.r %>%
  select(-winner, -winner_pct,-category)
describe(presi20231v.r$winner_percentage)

vote_columns_20232v <- c("LG_val", "DN_val")
presi20232v.r$winner <- colnames(presi20232v.r[vote_columns_20232v])[max.col(presi20232v.r[vote_columns_20232v], ties.method = "first")]
presi20232v.r$winner_pct <- apply(presi20232v.r[vote_columns_20232v], 1, max)
presi20232v.r$category <- cut(
  presi20232v.r$winner_pct, 
  breaks = c(50, 60, 70, 100), 
  labels = c("50-60", "60-70", ">70"), 
  right = FALSE
)
presi20232v.r$winner_percentage <- paste0(presi20232v.r$winner, "_", presi20232v.r$category)
presi20232v.r <- presi20232v.r %>%
  select(-winner, -winner_pct,-category)



readr::write_excel_csv(presi20211v.j, "CNE_bdd/procesadas/presi20211v.j.csv", na = "")
readr::write_excel_csv(presi20212v.j, "CNE_bdd/procesadas/presi20212v.j.csv", na = "")
readr::write_excel_csv(presi20231v.j, "CNE_bdd/procesadas/presi20231v.j.csv", na = "")
readr::write_excel_csv(presi20232v.j, "CNE_bdd/procesadas/presi20232v.j.csv", na = "")
readr::write_excel_csv(presi20211v.p, "CNE_bdd/procesadas/presi20211v.p.csv", na = "")
readr::write_excel_csv(presi20212v.p, "CNE_bdd/procesadas/presi20212v.p.csv", na = "")
readr::write_excel_csv(presi20231v.p, "CNE_bdd/procesadas/presi20231v.p.csv", na = "")
readr::write_excel_csv(presi20232v.p, "CNE_bdd/procesadas/presi20232v.p.csv", na = "")
readr::write_excel_csv(presi20211v.r, "CNE_bdd/procesadas/presi20211v.r.csv", na = "")
readr::write_excel_csv(presi20212v.r, "CNE_bdd/procesadas/presi20212v.r.csv", na = "")
readr::write_excel_csv(presi20231v.r, "CNE_bdd/procesadas/presi20231v.r.csv", na = "")
readr::write_excel_csv(presi20232v.r, "CNE_bdd/procesadas/presi20232v.r.csv", na = "")
readr::write_excel_csv(presi20211v.c, "CNE_bdd/procesadas/presi20211v.c.csv", na = "")
readr::write_excel_csv(presi20212v.c, "CNE_bdd/procesadas/presi20212v.c.csv", na = "")
readr::write_excel_csv(presi20231v.c, "CNE_bdd/procesadas/presi20231v.c.csv", na = "")
readr::write_excel_csv(presi20232v.c, "CNE_bdd/procesadas/presi20232v.c.csv", na = "")


rm()


##TRATANDO LA BASE 2025
  
presi20251v.j <- read_csv("CNE_bdd/procesadas/presi20251v.j.temp.csv")
presi20251v.r <- read_csv("CNE_bdd/procesadas/presi20251v.r.temp.csv")


presi20251v.p <- presi20251v.j %>%
  group_by(COD_PROVINCIA, `Nombre Provincia`, COD_CIRCUNSCRIPCION,
           COD_CANTON,`Nombre Cantón`,
           COD_PARROQUIA, `Nombre Parroquia`) %>%
  summarise(
    `Daniel Noboa` = sum(`Daniel Noboa`, na.rm = TRUE),
    `Luisa González` = sum(`Luisa González`, na.rm = TRUE),
    `Leonidas Iza` = sum(`Leonidas Iza`, na.rm = TRUE),
    `Andrea González` = sum(`Andrea González`, na.rm = TRUE),
    `Otros` = sum(`Otros`, na.rm = TRUE),
    votos_validos = sum(votos_validos, na.rm = TRUE),
    votos_blancos = sum(votos_blancos, na.rm = TRUE),
    votos_nulos = sum(votos_nulos, na.rm = TRUE),
    sufragantes = sum(sufragantes, na.rm = TRUE),
    num_zonas = n_distinct(COD_ZONA),
    num_recintos = n_distinct(COD_RECINTO),
    .groups = "drop"   
  )
calculate_percent <- function(numerator, denominator) {
  round(as.numeric(numerator) / as.numeric(denominator) * 100, 2)
}
presi20251v.p <- presi20251v.p %>%
  mutate(
    DN_val = calculate_percent(`Daniel Noboa`, votos_validos),
    LG_val = calculate_percent(`Luisa González`, votos_validos),
    LI_val = calculate_percent(`Leonidas Iza`, votos_validos),
    AG_val = calculate_percent(`Andrea González`, votos_validos),
    OT_val = calculate_percent(`Otros`, votos_validos),
    DN_tot = calculate_percent(`Daniel Noboa`, sufragantes),
    LG_tot = calculate_percent(`Luisa González`, sufragantes),
    LI_tot = calculate_percent(`Leonidas Iza`, sufragantes),
    AG_tot = calculate_percent(`Andrea González`, sufragantes),
    OT_tot = calculate_percent(`Otros`, sufragantes),
    BL_tot = calculate_percent(votos_blancos, sufragantes),
    NU_tot = calculate_percent(votos_nulos, sufragantes)
  )
vote_columns_20251v <- c("LG_val", "DN_val", "LI_val", "AG_val", "OT_val")
presi20251v.p$winner <- colnames(presi20251v.p[vote_columns_20251v])[max.col(presi20251v.p[vote_columns_20251v], ties.method = "first")]
presi20251v.p$winner_pct <- apply(presi20251v.p[vote_columns_20251v], 1, max)
presi20251v.p$category <- cut(
  presi20251v.p$winner_pct, 
  breaks = c(0, 50, 60, 100), 
  labels = c("<50", "50-60", ">60"), 
  right = FALSE
)
presi20251v.p$winner_percentage <- paste0(presi20251v.p$winner, "_", presi20251v.p$category)
presi20251v.p <- presi20251v.p %>%
  select(-winner, -winner_pct,-category)
describe(presi20251v.p$winner_percentage)
readr::write_excel_csv(presi20251v.p, "CNE_bdd/procesadas/presi20251v.p.temp.csv", na = "")







