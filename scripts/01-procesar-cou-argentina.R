# Procesamiento de los cuadros de oferta y utilización (COU) de Argentina
# =========================================================================
# Convierte los COU de INDEC (2018 a 2021) en una base de datos en formato
# plano, le agrega la clasificación bioeconómica y la guarda para su análisis.
#
# Adaptado del script original de A. Mondaini para el COU de 2018. Diferencias:
#   - Lee cada año con sus dos hojas (oferta y utilización) del .xls de INDEC.
#   - Asigna filas y columnas POR CÓDIGO y no por posición. INDEC cambió la
#     nomenclatura entre años:
#       * 2019+: producto nuevo "715 CSCD" y columna nueva "CSCD" (comisiones
#         por servicios de cambio de divisas).
#       * 2020+: "651" y "652" se publican juntos como "651/652".
#       * 2021:  desaparece el producto "841 L" (locutorios).
#   - Conserva solo los valores distintos de cero, para que el Excel sea liviano.
#
# Ejecutar desde la raíz del proyecto:
#   Rscript scripts/01-procesar-cou-argentina.R

library(readxl)
library(dplyr)
library(tidyr)
library(stringr)

anios <- 2018:2021
ruta_clasif <- "data/clasificaciones/ARG_Clasificaciones.xlsx"

limpiar <- function(x) str_squish(as.character(x))
parse_number_col <- function(x) as.numeric(str_extract(x, "\\d+$"))

# 1. Clasificaciones
# ------------------
columnas <- read_excel(ruta_clasif, sheet = "columnas") |>
  filter(!is.na(Columnas)) |>
  left_join(
    read.csv(
      "data/clasificaciones/ARG_equivalencias_columnas.csv",
      check.names = FALSE,
      encoding = "UTF-8"
    ),
    by = "Columnas"
  ) |>
  mutate(across(where(is.character), limpiar))

filas <- read_excel(ruta_clasif, sheet = "filas") |>
  filter(!is.na(Filas)) |>
  left_join(
    read.csv(
      "data/clasificaciones/ARG_equivalencias_filas.csv",
      check.names = FALSE,
      encoding = "UTF-8"
    ),
    by = "Filas"
  ) |>
  mutate(across(where(is.character), limpiar))

# Producto nuevo "715 CSCD": hereda los atributos de "711 COM/715" (servicios financieros, no bioeconomía).
nueva_fila <- function(prefijo, id) {
  filas |>
    filter(
      str_starts(Filas, prefijo),
      `Código Nomenclatura Local de Productos` == "711 COM/715"
    ) |>
    mutate(
      Filas = id,
      `Código Nomenclatura Local de Productos` = "715 CSCD",
      `Nomenclatura Local de Productos` =
        "715 - Otros servicios auxiliares de la intermediación financiera"
    )
}
filas <- bind_rows(
  filas,
  nueva_fila("ARGof", "ARGof227"),
  nueva_fila("ARGuf", "ARGuf227")
)

# Columna nueva "CSCD": se trata como un margen, igual que "Mg D".
columnas <- bind_rows(
  columnas,
  columnas |>
    filter(Columnas == "ARGoc113") |>
    mutate(
      Columnas = "ARGoc116",
      `Código Clasificación Actividades` = "CSCD",
      `Descripción Actividades` =
        "COMISIONES POR SERVICIOS DE CAMBIO DE DIVISAS"
    )
)

# Columnas de transacciones (después de las 107 actividades), por código.
col_oferta <- c(
  "IMPO" = "ARGoc109", "Ajuste CIF/FOB" = "ARGoc110", "DI" = "ARGoc111",
  "IP" = "ARGoc112", "Mg D" = "ARGoc113", "IVA" = "ARGoc114",
  "CSCD" = "ARGoc116"
)
col_util <- c(
  "CH" = "ARGuc109", "CP" = "ARGuc110", "EX" = "ARGuc111",
  "Fbc Fijo" = "ARGuc112", "OV" = "ARGuc113",
  "Productos terminados" = "ARGuc114", "Trabajos en curso" = "ARGuc115"
)
filas_ajuste <- c(
  "Ajuste CIF/FOB" = "224",
  "Compras de residentes en el mercado exterior" = "225",
  "Compras de no residentes en el mercado doméstico" = "226"
)

# 2. Lectura de un año
# --------------------
leer_hoja <- function(anio, hoja) {
  read_excel(
    sprintf("data/cou/COU-%d.xls", anio),
    sheet = hoja,
    col_names = FALSE,
    col_types = "text",
    .name_repair = "minimal"
  ) |>
    as.data.frame()
}

id_fila_producto <- function(codigo, prefijo) {
  # Los códigos fusionados se asignan al primero de sus componentes.
  codigo <- if_else(codigo == "651/652", "651", codigo)
  f <- filas[str_starts(filas$Filas, prefijo), ]
  f$Filas[match(codigo, f$`Código Nomenclatura Local de Productos`)]
}

desdoblar <- function(hoja, anio, columnas_trans, pref_fila, pref_col, cuadro) {
  etiqueta <- limpiar(hoja[[1]])
  codigo <- limpiar(hoja[[2]])
  encabezado <- limpiar(hoja[5, ])
  fin <- which(etiqueta == "Total")[1] - 1
  n_act <- which(encabezado %in% c("OPB", "UI"))[1] - 3
  stopifnot(n_act == 107)

  # Identificadores de columnas: actividades por posición y el resto por código.
  idx_act <- 3:(n_act + 2)
  id_act <- sprintf("ARG%sc%03d", pref_col, seq_len(n_act))
  idx_tr <- match(names(columnas_trans), encabezado)
  presentes <- !is.na(idx_tr)
  idx <- c(idx_act, idx_tr[presentes])
  ids <- c(id_act, unname(columnas_trans[presentes]))

  # Identificadores de filas: productos y filas de ajuste.
  r_prod <- 6:fin
  id_prod <- id_fila_producto(codigo[r_prod], paste0("ARG", pref_fila, "f"))
  stopifnot(!anyNA(id_prod))
  r_aj <- match(names(filas_ajuste), etiqueta)
  id_aj <- sprintf("ARG%sf%s", pref_fila, filas_ajuste)
  r <- c(r_prod, r_aj)
  id_fila <- c(id_prod, id_aj)

  m <- as.matrix(hoja[r, idx, drop = FALSE])
  m <- matrix(as.numeric(m), nrow = nrow(m))
  m[is.na(m)] <- 0

  salida <- expand_grid(
    i = seq_along(id_fila),
    j = seq_along(ids)
  ) |>
    mutate(
      Filas = id_fila[i],
      Columnas = ids[j],
      Valor = m[cbind(i, j)]
    ) |>
    select(-i, -j) |>
    mutate(Año = anio, Cuadro = cuadro)
  salida
}

leer_anio <- function(anio) {
  of <- leer_hoja(anio, 1)
  ut <- leer_hoja(anio, 2)
  oferta <- desdoblar(of, anio, col_oferta, "o", "o", "Oferta")
  utilizacion <- desdoblar(ut, anio, col_util, "u", "u", "Utilización")

  # Valor agregado bruto por actividad
  r_va <- which(limpiar(ut[[1]]) == "Valor Agregado Bruto pb")
  stopifnot(length(r_va) == 1)
  va <- tibble(
    Filas = "ARGvf001",
    Columnas = sprintf("ARGvc%03d", 1:107),
    Valor = as.numeric(unlist(ut[r_va, 3:109])),
    Año = anio,
    Cuadro = "Valor Agregado"
  )
  bind_rows(oferta, utilizacion, va)
}

# 3. Base de datos plana con la clasificación bioeconómica
# --------------------------------------------------------
scn <- bind_rows(lapply(anios, leer_anio)) |>
  mutate(Valor = replace_na(Valor, 0)) |>
  filter(Valor != 0) |>
  left_join(columnas, by = "Columnas") |>
  left_join(filas, by = "Filas") |>
  mutate(
    Precios = "Corrientes",
    Unidades = "Miles de pesos",
    `No. Cuadro` = match(Cuadro, c("Oferta", "Utilización", "Valor Agregado")),
    # Grupo de columnas del COU, con nombres uniformes para las tablas dinámicas
    Componente = case_when(
      Cuadro == "Valor Agregado" ~ "Valor agregado",
      str_detect(Columnas, "^ARGo") & parse_number_col(Columnas) <= 107 ~
        "Producción",
      str_detect(Columnas, "^ARGu") & parse_number_col(Columnas) <= 107 ~
        "Consumo intermedio",
      Columnas == "ARGoc109" ~ "Importaciones",
      Columnas == "ARGoc110" ~ "Ajuste CIF/FOB",
      Columnas %in% c("ARGoc111", "ARGoc112", "ARGoc114") ~
        "Impuestos menos subvenciones",
      Columnas %in% c("ARGoc113", "ARGoc116") ~ "Márgenes de distribución",
      Columnas %in% c("ARGuc109", "ARGuc110") ~ "Consumo final",
      Columnas == "ARGuc111" ~ "Exportaciones",
      Columnas %in% c("ARGuc112", "ARGuc113", "ARGuc114", "ARGuc115") ~
        "Formación bruta de capital"
    ),
    `Sector agregado` = case_when(
      `CIIU Compacto` %in% c("A", "B") ~ "A-B Sector primario",
      `CIIU Compacto` == "C" ~ "C Manufacturas",
      `CIIU Compacto` %in% c("D - E", "F", "G", "H - S") ~ "D-S Servicios"
    ),
    across(c(`Bioeconomía Actividades`, `Bioeconomía Productos`), \(x) {
      if_else(x == "-" | is.na(x), "No aplica", x)
    })
  ) |>
  select(
    Año, Precios, Unidades, `No. Cuadro`, Cuadro, Componente,
    Filas, Columnas, Valor,
    `Código Nomenclatura Local de Productos`,
    `Nomenclatura Local de Productos`,
    `Bioeconomía Productos`,
    `CPC Grupos Código`, `CPC Grupos`,
    `Código Clasificación Actividades`,
    `Descripción Actividades`,
    `Bioeconomía Actividades`,
    `CIIU Compacto`, `CIIU Compacto Descripción`, `Sector agregado`,
    `Transacciones`
  )

saveRDS(scn, "data/bd/arg_scn_2018_2021.rds", compress = "xz")

# 4. Controles contables
# ----------------------
controles <- scn |>
  filter(Cuadro %in% c("Oferta", "Utilización")) |>
  mutate(
    Producto = str_remove(Filas, "^ARG[ou]f")
  ) |>
  group_by(Año, Producto, Cuadro) |>
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
stopifnot(all(abs(resumen_controles$oferta - resumen_controles$utilizacion) < 5e3))

cat("Filas por año:\n")
print(count(scn, Año))
