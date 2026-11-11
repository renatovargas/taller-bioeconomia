# Procesamiento de los cuadros de oferta y utilización (COU) de Argentina
# =========================================================================
# Convierte los COU de INDEC (todos los años de data/config/arg_config.xlsx)
# en una sola base de datos plana con un campo de año, le agrega la
# clasificación bioeconómica y la guarda en data/bd/arg_scn.rds.
#
# Adaptado del script original de A. Mondaini para el COU de 2018.
#
# Dos archivos de configuración gobiernan la rutina:
#   data/config/arg_config.xlsx   qué leer: un renglón por año, con archivo,
#                                 hojas y factor de conversión de unidades.
#   data/config/arg_lookups.xlsx  cómo clasificar: un renglón por código de
#                                 INDEC (productos, actividades, transacciones),
#                                 con los años en que el código está vigente
#                                 (Desde, Hasta). Las tres categorías (hoja
#                                 "categorias") siguen el SCN 2025: Bioeconomía,
#                                 Bioeconomía parcial y Fuera de la bioeconomía.
#
# Filas y columnas se asignan POR CÓDIGO y no por posición, porque INDEC
# cambió la nomenclatura entre años (hoja "cambios" de arg_lookups.xlsx):
#   2019: producto nuevo "715 CSCD" y columna nueva "CSCD".
#   2020: "651" y "652" se publican juntos como "651/652".
#   2021: desaparece el producto "841 L" (locutorios).
#   2023: INDEC publica en millones de pesos (antes, miles).
# La base usa millones de pesos para todos los años.
#
# La rutina se detiene si un código del cuadro no está en los lookups, si un
# código vigente falta en el cuadro, o si los totales de INDEC no cuadran.
# Para agregar un año: copie el .xls en data/indec-cou/, agregue su renglón en
# arg_config.xlsx y, si INDEC cambió códigos, ajuste Desde/Hasta en los lookups.
#
# Ejecutar desde la raíz del proyecto:
#   Rscript scripts/01-procesar-cou-argentina.R

library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)

ruta_config <- "data/config/arg_config.xlsx"
ruta_lookups <- "data/config/arg_lookups.xlsx"
dir_cou <- "data/indec-cou"
salida <- "data/bd/arg_scn.rds"

limpiar <- function(x) str_squish(as.character(x))

# 1. Configuración y lookups
# --------------------------
config <- read_excel(ruta_config, sheet = "config")

leer_lookup <- function(hoja) {
  read_excel(ruta_lookups, sheet = hoja, col_types = "text") |>
    mutate(across(everything(), limpiar)) |>
    mutate(across(any_of(c("Desde", "Hasta", "Orden")), as.integer))
}
productos <- leer_lookup("productos")
actividades <- leer_lookup("actividades")
transacciones <- leer_lookup("transacciones")
especiales <- leer_lookup("filas_especiales")

# Los nombres de las categorías salen de la hoja "categorias" (por código)
source("scripts/categorias.R")
nombre_cat <- setNames(categorias$Categoría, categorias$Código)
productos <- productos |> mutate(`Bioeconomía Productos` = unname(nombre_cat[`Código Bioeconomía Productos`]))
actividades <- actividades |> mutate(`Bioeconomía Actividades` = unname(nombre_cat[`Código Bioeconomía Actividades`]))
stopifnot(!anyNA(productos$`Bioeconomía Productos`), !anyNA(actividades$`Bioeconomía Actividades`))

stopifnot(
  !anyDuplicated(c(actividades$`Código Clasificación Actividades`, na.omit(actividades$`Alias INDEC`))),
  !anyDuplicated(productos$`Código Nomenclatura Local de Productos`),
  !anyDuplicated(actividades$`Código Clasificación Actividades`),
  !anyDuplicated(paste(transacciones$Cuadro, transacciones$`Código columna`))
)

# Códigos vigentes en un año
vigente <- function(d, a) d |> filter(Desde <= a, is.na(Hasta) | Hasta >= a)

# Versiones: los años que comparten versión deben compartir todos los códigos
# vigentes, y dos versiones distintas deben diferir en al menos uno.
firma <- function(a) {
  paste(c(
    vigente(productos, a)$`Código Nomenclatura Local de Productos`,
    vigente(actividades, a)$`Código Clasificación Actividades`,
    with(vigente(transacciones, a), paste(Cuadro, `Código columna`))
  ), collapse = "|")
}
versiones <- config |>
  mutate(firma = vapply(Año, firma, "")) |>
  group_by(`Versión nomenclatura`) |>
  summarise(años = paste(range(Año), collapse = "-"), n_firmas = n_distinct(firma), firma = first(firma))
if (any(versiones$n_firmas > 1) || anyDuplicated(versiones$firma)) {
  print(versiones)
  stop("Versión nomenclatura de arg_config.xlsx no coincide con la vigencia de los lookups", call. = FALSE)
}
print(select(versiones, `Versión nomenclatura`, años))

# INDEC escribe algunos códigos de actividad de otra forma (hoja de utilización)
de_alias <- function(x) {
  a <- actividades |> filter(!is.na(`Alias INDEC`))
  i <- match(x, a$`Alias INDEC`)
  if_else(is.na(i), x, a$`Código Clasificación Actividades`[i])
}

comparar <- function(en_cuadro, en_lookup, que, anio) {
  sobran <- setdiff(en_cuadro, en_lookup)
  faltan <- setdiff(en_lookup, en_cuadro)
  if (length(sobran) || length(faltan)) {
    stop(
      sprintf("%s, %d. ", que, anio),
      if (length(sobran)) paste0("En el cuadro pero no vigentes en los lookups: ", paste(sobran, collapse = ", "), ". "),
      if (length(faltan)) paste0("Vigentes en los lookups pero no en el cuadro: ", paste(faltan, collapse = ", "), "."),
      call. = FALSE
    )
  }
}

# 2. Lectura de una hoja
# ----------------------
# Devuelve la hoja en formato largo con códigos de fila y de columna.
desdoblar <- function(cfg, cuadro) {
  anio <- cfg$Año
  hoja <- if (cuadro == "Oferta") cfg$`Hoja oferta` else cfg$`Hoja utilización`
  m <- read_excel(
    file.path(dir_cou, cfg$Archivo),
    sheet = hoja, col_names = FALSE, col_types = "text", .name_repair = "minimal"
  ) |>
    as.matrix()
  etiqueta <- limpiar(m[, 1])
  codigo <- limpiar(m[, 2])
  enc_fila <- which(etiqueta == "Descripción")[1]
  encabezado <- limpiar(m[enc_fila, ])
  num <- function(x) as.numeric(x) * cfg$`Factor a millones`

  # Columnas: actividades (de la 3 hasta el primer total) y transacciones
  primer_total <- if (cuadro == "Oferta") "OPB" else "UI"
  j_tot <- which(encabezado == primer_total)[1]
  j_act <- 3:(j_tot - 1)
  encabezado[j_act] <- de_alias(encabezado[j_act])
  act_v <- vigente(actividades, anio) |> arrange(Orden)
  comparar(encabezado[j_act], act_v$`Código Clasificación Actividades`, paste("Actividades", cuadro), anio)
  stopifnot(identical(encabezado[j_act], act_v$`Código Clasificación Actividades`))

  tr_v <- vigente(transacciones, anio) |> filter(Cuadro == cuadro)
  j_resto <- seq(j_tot, ncol(m))
  j_resto <- j_resto[!is.na(encabezado[j_resto]) & encabezado[j_resto] != ""]
  comparar(encabezado[j_resto], tr_v$`Código columna`, paste("Transacciones", cuadro), anio)
  j_tr <- j_resto[encabezado[j_resto] %in% tr_v$`Código columna`[tr_v$Tipo == "Transacción"]]

  # Filas: productos (de la siguiente al encabezado hasta "Total") y especiales
  i_fin <- which(etiqueta == "Total")[1] - 1
  i_prod <- (enc_fila + 1):i_fin
  comparar(codigo[i_prod], vigente(productos, anio)$`Código Nomenclatura Local de Productos`,
           paste("Productos", cuadro), anio)
  esp_v <- especiales |> filter(str_detect(Cuadros, cuadro), `Código fila` != "VAB")
  i_esp <- match(esp_v$`Etiqueta INDEC`, etiqueta)
  if (anyNA(i_esp)) stop("Filas especiales no encontradas en ", hoja, call. = FALSE)

  filas <- tibble(
    i = c(i_prod, i_esp),
    `Tipo fila` = c(rep("Producto", length(i_prod)), rep("Ajuste", length(i_esp))),
    `Código fila` = c(codigo[i_prod], esp_v$`Código fila`)
  )
  cols <- tibble(
    j = c(j_act, j_tr),
    `Tipo columna` = c(rep("Actividad", length(j_act)), rep("Transacción", length(j_tr))),
    `Código columna` = encabezado[c(j_act, j_tr)]
  )
  d <- expand_grid(filas, cols) |>
    mutate(Valor = num(m[cbind(i, j)]))

  # Control: la suma de cada fila de productos reproduce el total de INDEC
  col_total <- if (cuadro == "Oferta") "OPC" else "DEMANDA"
  total_indec <- tibble(i = i_prod, total = num(m[i_prod, match(col_total, encabezado)]))
  dif <- d |>
    filter(`Tipo fila` == "Producto") |>
    group_by(i) |>
    summarise(suma = sum(Valor, na.rm = TRUE), .groups = "drop") |>
    left_join(total_indec, by = "i") |>
    mutate(dif = abs(suma - total) / pmax(abs(total), 1))
  if (any(dif$dif > 1e-6)) {
    stop(sprintf("%s %d: %d filas no suman el total %s de INDEC", cuadro, anio, sum(dif$dif > 1e-6), col_total), call. = FALSE)
  }

  # Valor agregado bruto por actividad, en la hoja de utilización
  if (cuadro == "Utilización") {
    i_va <- match("Valor Agregado Bruto pb", etiqueta)
    stopifnot(!is.na(i_va))
    d <- bind_rows(
      d,
      tibble(
        `Tipo fila` = "Valor agregado", `Código fila` = "VAB",
        `Tipo columna` = "Actividad", `Código columna` = encabezado[j_act],
        Valor = num(m[i_va, j_act])
      )
    )
  }
  d |>
    select(-any_of(c("i", "j"))) |>
    mutate(Año = as.integer(anio), Cuadro = if_else(`Tipo fila` == "Valor agregado", "Valor Agregado", cuadro))
}

leer_anio <- function(k) {
  cfg <- config[k, ]
  message("Leyendo ", cfg$Año, " (", cfg$Archivo, ")")
  bind_rows(desdoblar(cfg, "Oferta"), desdoblar(cfg, "Utilización"))
}

# 3. Base de datos plana con la clasificación bioeconómica
# --------------------------------------------------------
attr_prod <- productos |>
  select(-Orden, -Desde, -Hasta) |>
  mutate(`Código fila` = `Código Nomenclatura Local de Productos`, `Grupo fila` = `CPC Grupos`)
attr_esp <- especiales |>
  transmute(`Código fila`, `Nomenclatura Local de Productos` = `Etiqueta INDEC`, `Grupo fila` = `Fila cuadro`)
attr_act <- actividades |>
  select(-Orden, -Desde, -Hasta, -`Alias INDEC`) |>
  mutate(`Código columna` = `Código Clasificación Actividades`)
attr_tr <- transacciones |>
  filter(Tipo == "Transacción") |>
  select(Cuadro, `Código columna`, Componente, `Código Transacciones`, Transacciones)

scn <- map_dfr(seq_len(nrow(config)), leer_anio) |>
  mutate(Valor = replace_na(Valor, 0)) |>
  filter(Valor != 0)

scn <- bind_rows(
  scn |> filter(`Tipo fila` == "Producto") |> left_join(attr_prod, by = "Código fila"),
  scn |> filter(`Tipo fila` != "Producto") |> left_join(attr_esp, by = "Código fila")
) |>
  left_join(attr_act, by = "Código columna") |>
  left_join(attr_tr, by = c("Cuadro", "Código columna")) |>
  mutate(
    Precios = "Corrientes",
    Unidades = "Millones de pesos",
    `No. Cuadro` = match(Cuadro, c("Oferta", "Utilización", "Valor Agregado")),
    Componente = case_when(
      Cuadro == "Valor Agregado" ~ "Valor agregado",
      `Tipo columna` == "Actividad" & Cuadro == "Oferta" ~ "Producción",
      `Tipo columna` == "Actividad" & Cuadro == "Utilización" ~ "Consumo intermedio",
      TRUE ~ Componente
    ),
    across(c(`Bioeconomía Actividades`, `Bioeconomía Productos`), \(x) {
      if_else(x == "-" | is.na(x), "No aplica", x)
    })
  ) |>
  select(
    Año, Precios, Unidades, `No. Cuadro`, Cuadro, Componente,
    `Tipo fila`, `Código fila`, `Grupo fila`,
    `Tipo columna`, `Código columna`, Valor,
    `Código Nomenclatura Local de Productos`,
    `Nomenclatura Local de Productos`,
    `Código serie`,
    `Bioeconomía Productos`,
    `CPC Grupos Código`, `CPC Grupos`,
    `Código Clasificación Actividades`,
    `Descripción Actividades`,
    `Bioeconomía Actividades`,
    `CIIU Secciones`, `CIIU Compacto`, `CIIU Compacto Descripción`, `Sector agregado`,
    `Código Transacciones`, Transacciones
  ) |>
  arrange(Año, `No. Cuadro`)

stopifnot(
  !anyNA(scn$Componente),
  !anyNA(scn$`Grupo fila`),
  all(!is.na(scn$`Sector agregado`[scn$`Tipo columna` == "Actividad"]))
)

# 4. Controles contables
# ----------------------
controles <- scn |>
  filter(Cuadro %in% c("Oferta", "Utilización")) |>
  group_by(Año, `Código fila`, Cuadro) |>
  summarise(v = sum(Valor), .groups = "drop") |>
  pivot_wider(names_from = Cuadro, values_from = v, values_fill = 0) |>
  mutate(dif = abs(Oferta - Utilización))

resumen_controles <- controles |>
  group_by(Año) |>
  summarise(
    oferta = sum(Oferta),
    utilizacion = sum(Utilización),
    max_dif_producto = max(dif),
    .groups = "drop"
  )
print(as.data.frame(resumen_controles))
stopifnot(all(abs(resumen_controles$oferta - resumen_controles$utilizacion) < 5))

# PIB por el método de la producción
pib <- scn |>
  group_by(Año) |>
  summarise(
    va = sum(Valor[Componente == "Valor agregado"]),
    imp = sum(Valor[Componente == "Impuestos menos subvenciones"]),
    pib = va + imp,
    .groups = "drop"
  )
print(as.data.frame(pib))

saveRDS(scn, salida, compress = "xz")
cat("Filas por año:\n")
print(count(scn, Año))
