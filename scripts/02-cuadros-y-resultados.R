# Cuadros, indicadores y multiplicadores de la cuenta de bioeconomía de Argentina
# ================================================================================
# Lee la base plana de 01-procesar-cou-argentina.R y produce:
#   data/salidas/resultados.rds                     insumo de presentaciones y documento
#   data/salidas/ARG_cuadros_bioeconomia_2021.xlsx  cuadros para descargar
#   data/bd/ARG_SCN_BD_2021.xlsx                    base con tabla dinámica
#
# Ejecutar desde la raíz del proyecto:
#   Rscript scripts/02-cuadros-y-resultados.R

library(dplyr)
library(tidyr)
library(stringr)
library(openxlsx2)

scn <- readRDS("data/bd/arg_scn_2018_2021.rds")
ANIO <- 2021L
ESCALA <- 1e6 # de miles de pesos a miles de millones de pesos

niveles_bio <- c("Bioeconomía", "Bioeconomía extendida", "No bioeconomía")
niveles_cpc <- c(
  "Productos agrícolas", "Productos mineros", "Manufacturas",
  "Servicios básicos", "Construcción", "Comercio", "Otros servicios"
)

# Rótulo de fila: grupo de producto o ajuste
fila_cou <- function(d) {
  d |>
    mutate(
      Fila = case_when(
        str_detect(Filas, "f224$") ~ "Ajuste CIF/FOB",
        str_detect(Filas, "f22[56]$") ~ "Compras directas",
        TRUE ~ `CPC Grupos`
      )
    )
}

# 1. Cuadro básico por sector (oferta, utilización, PIB)
# ------------------------------------------------------
cuadros_basicos <- function(a) {
  d <- scn |> filter(Año == a) |> fila_cou()
  ofe <- d |>
    filter(Cuadro == "Oferta") |>
    mutate(Col = if_else(Componente == "Producción", `Sector agregado`, Componente)) |>
    group_by(Fila, Col) |>
    summarise(v = sum(Valor) / ESCALA, .groups = "drop") |>
    pivot_wider(names_from = Col, values_from = v, values_fill = 0)
  uti <- d |>
    filter(Cuadro == "Utilización") |>
    mutate(Col = if_else(Componente == "Consumo intermedio", `Sector agregado`, Componente)) |>
    group_by(Fila, Col) |>
    summarise(v = sum(Valor) / ESCALA, .groups = "drop") |>
    pivot_wider(names_from = Col, values_from = v, values_fill = 0)
  orden <- c(niveles_cpc, "Ajuste CIF/FOB", "Compras directas")
  ord_col_o <- c(
    "A-B Sector primario", "C Manufacturas", "D-S Servicios",
    "Importaciones", "Ajuste CIF/FOB", "Impuestos menos subvenciones",
    "Márgenes de distribución"
  )
  ord_col_u <- c(
    "A-B Sector primario", "C Manufacturas", "D-S Servicios",
    "Exportaciones", "Consumo final", "Formación bruta de capital"
  )
  arreglar <- function(t, cols) {
    for (cc in setdiff(cols, names(t))) t[[cc]] <- 0
    t |>
      mutate(Fila = factor(Fila, levels = orden)) |>
      arrange(Fila) |>
      mutate(Fila = as.character(Fila)) |>
      select(Fila, all_of(cols)) |>
      mutate(Total = rowSums(across(all_of(cols))))
  }
  va <- d |>
    filter(Cuadro == "Valor Agregado") |>
    group_by(`Sector agregado`) |>
    summarise(v = sum(Valor) / ESCALA, .groups = "drop")
  list(oferta = arreglar(ofe, ord_col_o), utilizacion = arreglar(uti, ord_col_u), va = va)
}

# 2. Cuadros bioeconómicos
# ------------------------
col_bio <- function(bio, sector) {
  # Bio|Sector en una sola clave de columna
  paste(bio, sector, sep = " | ")
}

cuadro_bio <- function(a, cuadro, componente_act) {
  d <- scn |>
    filter(Año == a, Cuadro == cuadro) |>
    fila_cou() |>
    mutate(
      Bio = `Bioeconomía Productos`,
      Col = if_else(
        Componente == componente_act,
        col_bio(`Bioeconomía Actividades`, `Sector agregado`),
        Componente
      )
    )
  grupos <- d |>
    group_by(Bio, Fila, Col) |>
    summarise(v = sum(Valor) / ESCALA, .groups = "drop") |>
    pivot_wider(names_from = Col, values_from = v, values_fill = 0)
  grupos
}

ordenar_bio <- function(t, cols_final) {
  cols_act <- c(
    "Bioeconomía | A-B Sector primario", "Bioeconomía | C Manufacturas",
    "Bioeconomía extendida | C Manufacturas", "Bioeconomía extendida | D-S Servicios",
    "No bioeconomía | A-B Sector primario", "No bioeconomía | C Manufacturas",
    "No bioeconomía | D-S Servicios"
  )
  cols <- c(cols_act, cols_final)
  for (cc in setdiff(cols, names(t))) t[[cc]] <- 0
  # Ninguna columna de actividades puede quedar fuera del cuadro
  sobrantes <- setdiff(names(t), c("Bio", "Fila", cols))
  if (length(sobrantes)) stop("Columnas sin ubicar: ", paste(sobrantes, collapse = ", "))
  t |>
    select(Bio, Fila, all_of(cols)) |>
    mutate(Total = rowSums(across(all_of(cols)))) |>
    mutate(
      o1 = match(Bio, c(niveles_bio, "No aplica")),
      o2 = match(Fila, c(niveles_cpc, "Ajuste CIF/FOB", "Compras directas"))
    ) |>
    arrange(o1, o2) |>
    select(-o1, -o2)
}

cuadros_bio <- function(a) {
  of <- cuadro_bio(a, "Oferta", "Producción") |>
    ordenar_bio(c(
      "Importaciones", "Ajuste CIF/FOB", "Impuestos menos subvenciones",
      "Márgenes de distribución"
    ))
  ut <- cuadro_bio(a, "Utilización", "Consumo intermedio") |>
    ordenar_bio(c("Exportaciones", "Consumo final", "Formación bruta de capital"))
  va <- scn |>
    filter(Año == a, Cuadro == "Valor Agregado") |>
    mutate(Col = col_bio(`Bioeconomía Actividades`, `Sector agregado`)) |>
    group_by(Col) |>
    summarise(v = sum(Valor) / ESCALA, .groups = "drop")
  list(oferta = of, utilizacion = ut, va = va)
}

# 3. Resumen de valor agregado por actividad bioeconómica
# -------------------------------------------------------
va_actividades <- function(a) {
  d <- scn |> filter(Año == a, Componente %in% c("Producción", "Consumo intermedio", "Valor agregado"))
  d |>
    group_by(`Bioeconomía Actividades`, Componente) |>
    summarise(v = sum(Valor) / ESCALA, .groups = "drop") |>
    pivot_wider(names_from = Componente, values_from = v) |>
    mutate(pct_va = `Valor agregado` / sum(`Valor agregado`)) |>
    rename(Bio = `Bioeconomía Actividades`) |>
    mutate(o = match(Bio, niveles_bio)) |>
    arrange(o) |>
    select(-o)
}

# Valor agregado ajustado por el contenido bioeconómico del consumo intermedio.
# Sección a sección (7 secciones CIIU), como en Vargas, Mondaini y Rodríguez (2023).
va_via_ci <- function(a) {
  d <- scn |> filter(Año == a)
  ci <- d |>
    filter(Componente == "Consumo intermedio") |>
    group_by(Seccion = `CIIU Compacto`, Nombre = `CIIU Compacto Descripción`) |>
    summarise(
      ci = sum(Valor) / ESCALA,
      ci_bio = sum(Valor[`Bioeconomía Productos` == "Bioeconomía"]) / ESCALA,
      ci_ext = sum(Valor[`Bioeconomía Productos` == "Bioeconomía extendida"]) / ESCALA,
      .groups = "drop"
    )
  va <- d |>
    filter(Componente == "Valor agregado") |>
    group_by(Seccion = `CIIU Compacto`) |>
    summarise(va = sum(Valor) / ESCALA, .groups = "drop")
  ci |>
    left_join(va, by = "Seccion") |>
    mutate(
      pct_bio = ci_bio / ci,
      va_bio = pct_bio * va,
      o = match(Seccion, c("A", "B", "C", "D - E", "F", "G", "H - S"))
    ) |>
    arrange(o) |>
    select(-o)
}

# 4. Multiplicadores de uso de insumos bioeconómicos
# --------------------------------------------------
# Modelo de demanda con supuesto de tecnología de la industria:
#   B = U * inv(diag(x))   insumos por peso de producción de cada actividad
#   D = V * inv(diag(q))   participación de cada actividad en cada producto
#   L = inv(I - D * B)     requerimientos directos e indirectos entre actividades
# Multiplicador de la actividad j: b' L, donde b suma las filas de B de los
# productos bioeconómicos y bioeconómicos extendidos.
# Es una medida de uso total: el cuadro de INDEC está a precios de comprador y
# no separa insumos nacionales de importados.
multiplicadores <- function(a) {
  d <- scn |> filter(Año == a)
  act <- d |>
    filter(Componente == "Valor agregado") |>
    transmute(n = as.integer(str_extract(Columnas, "\\d+$")),
              Actividad = `Descripción Actividades`,
              `Código` = `Código Clasificación Actividades`,
              `Bioeconomía Actividades`,
              Seccion = `CIIU Compacto`,
              `Sección CIIU` = `CIIU Compacto Descripción`,
              va = Valor)
  n_act <- 107L
  prod <- sort(unique(str_extract(
    d$Filas[d$Componente %in% c("Producción", "Consumo intermedio")], "\\d+$"
  )))
  prod <- prod[as.integer(prod) <= 223 | prod == "227"]
  n_p <- length(prod)
  V <- matrix(0, n_act, n_p)
  U <- matrix(0, n_p, n_act)
  pr <- d |> filter(Componente == "Producción", str_detect(Filas, paste0("(", paste(prod, collapse = "|"), ")$")))
  V[cbind(as.integer(str_extract(pr$Columnas, "\\d+$")), match(str_extract(pr$Filas, "\\d+$"), prod))] <- pr$Valor
  ci <- d |> filter(Componente == "Consumo intermedio", str_extract(Filas, "\\d+$") %in% prod)
  U[cbind(match(str_extract(ci$Filas, "\\d+$"), prod), as.integer(str_extract(ci$Columnas, "\\d+$")))] <- ci$Valor

  x <- rowSums(V)
  q <- colSums(V)
  inv_seg <- function(v) ifelse(v > 0, 1 / v, 0)
  B <- sweep(U, 2, inv_seg(x), "*")
  Dm <- sweep(V, 2, inv_seg(q), "*")
  A <- Dm %*% B
  L <- solve(diag(n_act) - A)

  bio_p <- d |>
    filter(Componente == "Producción") |>
    distinct(id = str_extract(Filas, "\\d+$"), `Bioeconomía Productos`)
  clase <- bio_p$`Bioeconomía Productos`[match(prod, bio_p$id)]
  clase[is.na(clase)] <- "No bioeconomía"
  b_bio <- colSums(B[clase == "Bioeconomía", ])
  b_ext <- colSums(B[clase == "Bioeconomía extendida", ])
  m_bio <- as.vector(b_bio %*% L)
  m_ext <- as.vector(b_ext %*% L)

  res <- act |>
    arrange(n) |>
    mutate(
      directo = b_bio + b_ext,
      m_bio = m_bio,
      m_ext = m_ext,
      m_total = m_bio + m_ext,
      m_demanda = colSums(L),
      vbp = x / ESCALA,
      va = va / ESCALA
    )
  med <- median(res$m_total)
  dam <- median(abs(res$m_total - med))
  res <- res |>
    mutate(
      Grupo = case_when(
        m_total > med + 3 * dam ~ "1. Muy alta",
        m_total > med + 2 * dam ~ "2. Alta",
        m_total > med + 1 * dam ~ "3. Sobre la mediana",
        TRUE ~ "4. Mediana o abajo"
      )
    )
  attr(res, "mediana") <- med
  attr(res, "dam") <- dam
  res
}

# 5. Indicadores por año
# ----------------------
series <- function(a) {
  d <- scn |> filter(Año == a, `Bioeconomía Productos` %in% niveles_bio)
  comp <- c(
    "Producción", "Importaciones", "Impuestos menos subvenciones",
    "Consumo intermedio", "Exportaciones", "Consumo final",
    "Formación bruta de capital"
  )
  d |>
    filter(Componente %in% comp) |>
    group_by(Componente, Bio = `Bioeconomía Productos`) |>
    summarise(v = sum(Valor) / ESCALA, .groups = "drop") |>
    group_by(Componente) |>
    mutate(pct = v / sum(v), Año = a) |>
    ungroup()
}

tasa_implicita <- function(a) {
  d <- scn |> filter(Año == a, `Bioeconomía Productos` %in% niveles_bio)
  d |>
    filter(Componente %in% c("Producción", "Impuestos menos subvenciones")) |>
    group_by(Bio = `Bioeconomía Productos`, Componente) |>
    summarise(v = sum(Valor), .groups = "drop") |>
    pivot_wider(names_from = Componente, values_from = v) |>
    mutate(tasa = `Impuestos menos subvenciones` / Producción, Año = a)
}

medidas_va <- function(a) {
  r <- va_actividades(a)
  v <- va_via_ci(a)
  tot <- sum(r$`Valor agregado`)
  tibble(
    Año = a,
    va_total = tot,
    apriori_bio = r$`Valor agregado`[r$Bio == "Bioeconomía"] / tot,
    apriori_ext = r$`Valor agregado`[r$Bio == "Bioeconomía extendida"] / tot,
    via_ci = sum(v$va_bio) / tot
  )
}

# Detalle por producto en el año de análisis
detalle_productos <- function(a) {
  scn |>
    filter(Año == a, !str_detect(Filas, "f22[456]$"), Cuadro != "Valor Agregado") |>
    group_by(
      Código = `Código Nomenclatura Local de Productos`,
      Producto = `Nomenclatura Local de Productos`,
      Bio = `Bioeconomía Productos`,
      Grupo = `CPC Grupos`,
      Componente
    ) |>
    summarise(v = sum(Valor) / ESCALA, .groups = "drop") |>
    pivot_wider(names_from = Componente, values_from = v, values_fill = 0) |>
    mutate(
      Producto = str_squish(str_remove(Producto, "^[^ ]+( [A-Z]+)? - ")),
      # El nombre oficial empieza por el componente menor del grupo
      Producto = if_else(Código == "217/218", "Tortas oleaginosas y borras de algodón", Producto)
    )
}

# 6. Ensamblar resultados
# -----------------------
anios <- sort(unique(scn$Año))
resultados <- list(
  anio = ANIO,
  basicos = cuadros_basicos(ANIO),
  bio = cuadros_bio(ANIO),
  va_act = va_actividades(ANIO),
  va_ci = va_via_ci(ANIO),
  mult = multiplicadores(ANIO),
  series = bind_rows(lapply(anios, series)),
  tasas = bind_rows(lapply(anios, tasa_implicita)),
  medidas = bind_rows(lapply(anios, medidas_va)),
  productos = detalle_productos(ANIO),
  # Para validar contra el anexo publicado
  bio2018 = cuadros_bio(2018)
)
resultados$med_dam <- c(
  mediana = attr(resultados$mult, "mediana"),
  dam = attr(resultados$mult, "dam")
)
saveRDS(resultados, "data/salidas/resultados.rds")

# 7. Excel con cuadros para descargar
# -----------------------------------
wb <- wb_workbook(creator = "Renato Vargas")
agregar <- function(wb, nombre, titulo, df) {
  wb$add_worksheet(nombre)
  wb$add_data(sheet = nombre, x = titulo, dims = "A1")
  wb$add_font(sheet = nombre, dims = "A1", bold = TRUE, size = 13)
  wb$add_data(sheet = nombre, x = "Miles de millones de pesos corrientes. Fuente: elaboración propia con datos de INDEC.", dims = "A2")
  wb$add_data_table(sheet = nombre, x = df, dims = "A4", table_style = "TableStyleLight9", na.strings = "")
  wb$set_col_widths(sheet = nombre, cols = 1:ncol(df), widths = 16)
  num <- which(sapply(df, is.numeric))
  for (j in num) {
    wb$add_numfmt(
      sheet = nombre,
      dims = wb_dims(x = df, cols = j, from_row = 4, select = "data"),
      numfmt = "#,##0"
    )
  }
  wb
}
r <- resultados
wb <- agregar(wb, "Oferta", sprintf("Argentina: cuadro de oferta por sector, %d", ANIO), r$basicos$oferta)
wb <- agregar(wb, "Utilización", sprintf("Argentina: cuadro de utilización por sector, %d", ANIO), r$basicos$utilizacion)
wb <- agregar(wb, "Oferta bio", sprintf("Argentina: oferta bioeconómica, %d", ANIO), r$bio$oferta)
wb <- agregar(wb, "Utilización bio", sprintf("Argentina: utilización bioeconómica, %d", ANIO), r$bio$utilizacion)
wb <- agregar(wb, "VA vía CI", sprintf("Argentina: valor agregado vía consumo intermedio, %d", ANIO), r$va_ci)
wb$add_worksheet("Multiplicadores")
wb$add_data_table(
  sheet = "Multiplicadores",
  x = r$mult |>
    select(Código, Actividad, `Sección CIIU`, `Bioeconomía Actividades`, directo, m_bio, m_ext, m_total, m_demanda, Grupo, vbp, va) |>
    arrange(desc(m_total)) |>
    as.data.frame(),
  table_style = "TableStyleLight9"
)
wb$set_col_widths(sheet = "Multiplicadores", cols = 1:12, widths = 16)
wb$save("data/salidas/ARG_cuadros_bioeconomia_2021.xlsx")

# 8. Base de datos en Excel con tabla dinámica
# --------------------------------------------
bd <- scn |>
  filter(Año == ANIO) |>
  mutate(
    across(where(is.character), \(x) replace_na(x, "No aplica")),
    `Nomenclatura Local de Productos` = str_sub(`Nomenclatura Local de Productos`, 1, 120),
    `Descripción Actividades` = str_sub(`Descripción Actividades`, 1, 120)
  ) |>
  select(-Precios, -Transacciones) |>
  as.data.frame()

lee <- data.frame(
  Campo = c(
    "Cuadro", "Componente", "Valor", "Bioeconomía Productos", "CPC Grupos",
    "Nomenclatura Local de Productos", "Bioeconomía Actividades",
    "Sector agregado", "CIIU Compacto Descripción", "Descripción Actividades",
    "Filas / Columnas"
  ),
  Descripción = c(
    "Oferta, Utilización o Valor Agregado.",
    "Columna del COU con nombre uniforme: Producción, Importaciones, Impuestos menos subvenciones, Márgenes de distribución, Consumo intermedio, Exportaciones, Consumo final, Formación bruta de capital, Valor agregado.",
    "Valor en miles de pesos corrientes, igual que en el archivo de INDEC. Solo se incluyen celdas distintas de cero.",
    "Condición del producto (la fila del COU).",
    "Grupo de productos.",
    "Producto (222 en 2021).",
    "Condición de la actividad (la columna del COU).",
    "Sector primario, manufacturas o servicios.",
    "Sección de la CIIU compacta.",
    "Actividad económica (107).",
    "Identificadores internos de la fila y la columna."
  )
)

wbb <- wb_workbook(creator = "Renato Vargas")
wbb$add_worksheet("Léeme")
wbb$add_data(sheet = "Léeme", x = "Base de datos de la cuenta de bioeconomía de Argentina, 2021", dims = "A1")
wbb$add_font(sheet = "Léeme", dims = "A1", bold = TRUE, size = 14)
wbb$add_data(
  sheet = "Léeme",
  x = c(
    "La hoja BD contiene el COU de INDEC en formato plano: cada fila es una celda del cuadro original.",
    "La hoja TD contiene una tabla dinámica lista para usar. Si aparece vacía, seleccione Datos > Actualizar todo.",
    "Para crear otra: seleccione una celda de la tabla en BD, luego Insertar > Tabla dinámica.",
    "Pruebe filtrar por Cuadro y Componente; coloque Bioeconomía Productos en filas y Bioeconomía Actividades en columnas."
  ),
  dims = "A3"
)
wbb$add_data_table(sheet = "Léeme", x = lee, dims = "A8", table_style = "TableStyleLight9")
wbb$set_col_widths(sheet = "Léeme", cols = 1:2, widths = c(34, 120))
wbb$add_worksheet("BD")
wbb$add_data_table(sheet = "BD", x = bd, table_name = "BD_ARG", table_style = "TableStyleLight9")
wbb$set_col_widths(sheet = "BD", cols = seq_len(ncol(bd)), widths = 18)
wbb$add_worksheet("TD")
wbb$add_pivot_table(
  wb_data(wbb, sheet = "BD"),
  sheet = "TD",
  dims = "A5",
  filter = c("Cuadro"),
  rows = c("Bioeconomía Productos", "CPC Grupos"),
  cols = "Componente",
  data = "Valor",
  fun = "sum",
  pivot_table = "Tabla_Bioeconomia"
)
wbb$set_active_sheet("Léeme")
wbb$save("data/bd/ARG_SCN_BD_2021.xlsx")

cat("Listo\n")
