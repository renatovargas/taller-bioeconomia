# Funciones de apoyo para los cuadros y gráficos de presentaciones y documentos
# Convierte las tablas de data/salidas/resultados.rds en HTML y gráficos.

library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(knitr)

COL_BIO <- c(
  "Bioeconomía" = "#0F7B5F",
  "Bioeconomía extendida" = "#C27C0E",
  "No bioeconomía" = "#2F6DB5"
)
TINTA <- "#1F2A2E"
TINTA2 <- "#5B676B"
REJILLA <- "#E3E8E6"

opts_chunk$set(dev = "ragg_png", dpi = 200, dev.args = list(bg = "transparent"))

# Formato numérico en español: 1.234,5
num <- function(x, d = 0) {
  formatC(x, format = "f", digits = d, big.mark = ".", decimal.mark = ",")
}
pct <- function(x, d = 1) paste0(num(100 * x, d), " %")
celda <- function(x) {
  ifelse(is.na(x) | abs(x) < 0.5, "", num(x))
}

html_salida <- function(x) {
  asis_output(paste0("\n\n```{=html}\n", x, "\n```\n\n"))
}

# Cuadros de oferta y utilización por sector
# ------------------------------------------
cuadro_basico <- function(t, tipo = c("oferta", "utilizacion"), va = NULL) {
  tipo <- match.arg(tipo)
  sec <- c("A-B Sector primario", "C Manufacturas", "D-S Servicios")
  otras <- setdiff(names(t), c("Fila", sec, "Total"))
  grupo <- if (tipo == "oferta") "Producción" else "Consumo intermedio"
  cab1 <- paste0(
    "<tr><th rowspan='2' class='izq'>", grupo, " de</th>",
    "<th colspan='3' class='grupo-sec'>", grupo, "</th>",
    paste0("<th rowspan='2'>", otras, "</th>", collapse = ""),
    "<th rowspan='2'>Total</th></tr>"
  )
  cab2 <- "<tr><th>A-B<br>Sector primario</th><th>C<br>Manufacturas</th><th>D-S<br>Servicios</th></tr>"
  filas <- apply(t, 1, function(f) {
    v <- as.numeric(f[-1])
    paste0(
      "<tr><td class='izq'>", f[["Fila"]], "</td>",
      paste0("<td>", celda(v), "</td>", collapse = ""), "</tr>"
    )
  })
  tot <- colSums(t[, -1])
  total <- paste0(
    "<tr class='total'><td class='izq'>",
    if (tipo == "oferta") "Oferta total" else "Utilización total", "</td>",
    paste0("<td>", celda(tot), "</td>", collapse = ""), "</tr>"
  )
  extra <- ""
  if (!is.null(va)) {
    v <- va$v[match(sec, va$`Sector agregado`)]
    extra <- paste0(
      "<tr class='subtotal'><td class='izq'>Valor agregado bruto</td>",
      paste0("<td>", celda(v), "</td>", collapse = ""),
      paste0(rep("<td></td>", length(otras)), collapse = ""),
      "<td>", celda(sum(v)), "</td></tr>"
    )
  }
  html_salida(paste0(
    "<table class='cou'><thead>", cab1, cab2, "</thead><tbody>",
    paste(filas, collapse = ""), total, extra, "</tbody></table>"
  ))
}

cuadro_pib <- function(b) {
  sec <- c("A-B Sector primario", "C Manufacturas", "D-S Servicios")
  vbp <- colSums(b$oferta[, sec])
  ci <- colSums(b$utilizacion[, sec])
  va <- vbp - ci
  imp <- sum(b$oferta$`Impuestos menos subvenciones`)
  fila <- function(et, v, cl = "") {
    paste0(
      "<tr class='", cl, "'><td class='izq'>", et, "</td>",
      paste0("<td>", celda(v), "</td>", collapse = ""),
      "<td>", celda(sum(v)), "</td></tr>"
    )
  }
  html_salida(paste0(
    "<table class='cou grande'><thead><tr><th class='izq'></th>",
    "<th>A-B<br>Sector primario</th><th>C<br>Manufacturas</th><th>D-S<br>Servicios</th><th>Total</th></tr></thead><tbody>",
    fila("(+) Valor bruto de producción", vbp),
    fila("(-) Consumo intermedio", ci),
    fila("(=) Valor agregado bruto", va, "total"),
    "<tr><td class='izq'>(+) Impuestos menos subvenciones sobre los productos</td><td></td><td></td><td></td><td>", celda(imp), "</td></tr>",
    "<tr class='total'><td class='izq'>(=) Producto interno bruto a precios corrientes</td><td></td><td></td><td></td><td>", celda(sum(va) + imp), "</td></tr>",
    "</tbody></table>"
  ))
}

# Cuadros bioeconómicos
# ---------------------
COLS_ACT <- c(
  "Bioeconomía | A-B Sector primario", "Bioeconomía | C Manufacturas",
  "Bioeconomía extendida | C Manufacturas", "Bioeconomía extendida | D-S Servicios",
  "No bioeconomía | A-B Sector primario", "No bioeconomía | C Manufacturas",
  "No bioeconomía | D-S Servicios"
)
cuadro_bio_html <- function(t, va = NULL, marcar = FALSE) {
  otras <- setdiff(names(t), c("Bio", "Fila", COLS_ACT, "Total"))
  nums <- c(COLS_ACT, otras, "Total")
  etq <- str_replace(COLS_ACT, ".* \\| ", "") |>
    str_remove("^[A-D]-?[A-S]? ") |>
    str_replace("Sector primario", "Sector<br>primario") |>
    str_replace("Manufacturas", "Manu-<br>facturas")
  cab1 <- paste0(
    "<tr><th rowspan='2' class='izq'></th>",
    "<th colspan='2' class='h-bio'>Bioeconomía</th>",
    "<th colspan='2' class='h-ext'>Bioeconomía extendida</th>",
    "<th colspan='3' class='h-nob'>No bioeconomía</th>",
    paste0("<th rowspan='2' class='vert'>", otras, "</th>", collapse = ""),
    "<th rowspan='2'>Total</th></tr>"
  )
  clases <- c("h-bio", "h-bio", "h-ext", "h-ext", "h-nob", "h-nob", "h-nob")
  cab2 <- paste0(
    "<tr>",
    paste0("<th class='", clases, " sub'>", etq, "</th>", collapse = ""),
    "</tr>"
  )
  fila_html <- function(et, v, cl, bio_fila = NA) {
    cel <- mapply(function(val, n) {
      cc <- ""
      if (marcar && !is.na(bio_fila)) {
        es_act_bio <- str_detect(n, "^Bioeconomía")
        es_act_nob <- str_detect(n, "^No bioeconomía")
        if (bio_fila %in% c("Bioeconomía", "Bioeconomía extendida") && es_act_nob) cc <- "subest"
        if (bio_fila == "No bioeconomía" && es_act_bio) cc <- "sobrest"
      }
      paste0("<td class='", cc, "'>", celda(val), "</td>")
    }, v, nums)
    paste0("<tr class='", cl, "'><td class='izq'>", et, "</td>", paste(cel, collapse = ""), "</tr>")
  }
  cuerpo <- character()
  for (b in c("Bioeconomía", "Bioeconomía extendida", "No bioeconomía")) {
    g <- t[t$Bio == b, ]
    cuerpo <- c(
      cuerpo,
      fila_html(b, colSums(g[, nums]), paste0("subtotal s-", match(b, names(COL_BIO))), b)
    )
    for (i in seq_len(nrow(g))) {
      cuerpo <- c(
        cuerpo,
        fila_html(paste0("&ensp;", g$Fila[i]), as.numeric(g[i, nums]), "", b)
      )
    }
  }
  resto <- t[t$Bio == "No aplica", ]
  for (i in seq_len(nrow(resto))) {
    cuerpo <- c(cuerpo, fila_html(resto$Fila[i], as.numeric(resto[i, nums]), ""))
  }
  cuerpo <- c(
    cuerpo,
    fila_html(
      if (is.null(va)) "Oferta total" else "Utilización total",
      colSums(t[, nums]), "total"
    )
  )
  if (!is.null(va)) {
    v <- va$v[match(COLS_ACT, va$Col)]
    v[is.na(v)] <- 0
    vv <- c(v, rep(NA, length(otras)), sum(v))
    cuerpo <- c(cuerpo, fila_html("Valor agregado bruto", vv, "total"))
  }
  html_salida(paste0(
    "<table class='cou bio'><thead>", cab1, cab2, "</thead><tbody>",
    paste(cuerpo, collapse = ""), "</tbody></table>"
  ))
}

cuadro_va_act <- function(v) {
  o <- match(names(COL_BIO), v$Bio)
  v <- v[o, ]
  cols <- c("h-bio", "h-ext", "h-nob")
  fila <- function(et, x, cl = "", f = celda) {
    paste0(
      "<tr class='", cl, "'><td class='izq'>", et, "</td>",
      paste0("<td>", f(x), "</td>", collapse = ""), "<td>", f(sum(x)), "</td></tr>"
    )
  }
  html_salida(paste0(
    "<table class='cou grande'><thead><tr><th class='izq'>Actividades</th>",
    paste0("<th class='", cols, "'>", v$Bio, "</th>", collapse = ""),
    "<th>Total</th></tr></thead><tbody>",
    fila("(+) Valor bruto de producción", v$Producción),
    fila("(-) Consumo intermedio", v$`Consumo intermedio`),
    fila("(=) Valor agregado bruto", v$`Valor agregado`, "total"),
    fila("Porcentaje", v$pct_va, "subtotal", function(x) pct(x)),
    "</tbody></table>"
  ))
}

cuadro_va_ci <- function(t) {
  ind <- function(et, x, cl = "", f = celda, tot = sum(x)) {
    paste0(
      "<tr class='", cl, "'><td class='izq'>", et, "</td>",
      paste0("<td>", f(x), "</td>", collapse = ""), "<td>", f(tot), "</td></tr>"
    )
  }
  html_salida(paste0(
    "<table class='cou grande'><thead><tr><th class='izq'></th>",
    paste0("<th>", t$Seccion, "<br>", t$Nombre, "</th>", collapse = ""),
    "<th>Total</th></tr></thead><tbody>",
    ind("Consumo intermedio", t$ci),
    ind("de los cuales, insumos bioeconómicos", t$ci_bio),
    ind("% bioeconómico en el consumo intermedio", t$pct_bio, "subtotal", function(x) pct(x), sum(t$ci_bio) / sum(t$ci)),
    ind("Valor agregado bruto", t$va),
    ind("VA bioeconómico vía consumo intermedio", t$va_bio, "total"),
    ind("% del VA de la sección", t$va_bio / t$va, "", function(x) pct(x), sum(t$va_bio) / sum(t$va)),
    "</tbody></table>"
  ))
}

# Multiplicadores
# ---------------
nombre_actividad <- function(x, largo = 70) {
  y <- x |>
    str_replace_all("\\b\\d{2,5}\\s*-\\s*", "") |>
    str_squish()
  y <- str_split_fixed(y, "\\. ", 2)[, 1]
  str_trunc(y, largo, ellipsis = "...")
}

lista_mult <- function(m, grupos, desde = 1, hasta = Inf) {
  d <- m |> filter(Grupo %in% grupos) |> arrange(Grupo, desc(m_total))
  salida <- character()
  for (g in grupos) {
    dd <- d |> filter(Grupo == g)
    medio <- mean(dd$m_total)
    dd <- dd |> slice(desde:min(hasta, nrow(dd)))
    salida <- c(
      salida,
      paste0("<tr class='subtotal s-1'><td colspan='2' class='izq'>", g, "</td><td>", num(medio, 2), "</td></tr>"),
      paste0(
        "<tr><td class='sec'>", dd$Seccion, "</td><td class='izq'>", nombre_actividad(dd$Actividad),
        "</td><td>", num(dd$m_total, 2), "</td></tr>"
      )
    )
  }
  html_salida(paste0("<table class='cou lista'><tbody>", paste(salida, collapse = ""), "</tbody></table>"))
}

cuadro_grupos_va <- function(m) {
  g <- m |>
    group_by(Grupo) |>
    summarise(n = n(), va = sum(va), .groups = "drop") |>
    mutate(p = va / sum(va))
  filas <- paste0(
    "<tr><td class='izq'>", g$Grupo, "</td><td>", g$n, "</td><td>", celda(g$va), "</td><td>", pct(g$p), "</td></tr>",
    collapse = ""
  )
  html_salida(paste0(
    "<table class='cou grande'><thead><tr><th class='izq'>Grupo de uso de insumos</th><th>Actividades</th><th>Valor agregado bruto</th><th>Porcentaje</th></tr></thead><tbody>",
    filas,
    "<tr class='total'><td class='izq'>Total</td><td>", sum(g$n), "</td><td>", celda(sum(g$va)), "</td><td>100,0 %</td></tr></tbody></table>"
  ))
}

# Tablas simples
# --------------
tabla_simple <- function(d, clase = "cou", izq = 1) {
  cab <- paste0(
    "<th class='", ifelse(seq_along(d) <= izq, "izq", ""), "'>", names(d), "</th>",
    collapse = ""
  )
  cuerpo <- apply(as.matrix(d), 1, function(f) {
    paste0(
      "<tr>",
      paste0("<td class='", ifelse(seq_along(f) <= izq, "izq", ""), "'>", f, "</td>", collapse = ""),
      "</tr>"
    )
  })
  html_salida(paste0(
    "<table class='", clase, "'><thead><tr>", cab, "</tr></thead><tbody>",
    paste(cuerpo, collapse = ""), "</tbody></table>"
  ))
}

# Gráficos
# --------
tema_bio <- function(base = 15) {
  theme_minimal(base_size = base) +
    theme(
      text = element_text(color = TINTA),
      axis.text = element_text(color = TINTA2),
      axis.title = element_text(color = TINTA2, size = rel(0.85)),
      panel.grid.major.y = element_line(color = REJILLA, linewidth = 0.4),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      strip.text = element_text(face = "bold", color = TINTA, hjust = 0),
      legend.position = "top",
      legend.title = element_blank(),
      legend.justification = "left",
      plot.background = element_rect(fill = "transparent", color = NA),
      panel.background = element_rect(fill = "transparent", color = NA),
      plot.margin = margin(4, 8, 4, 4)
    )
}

orden_comp <- c(
  "Producción", "Consumo intermedio", "Importaciones", "Exportaciones",
  "Impuestos menos subvenciones", "Consumo final", "Formación bruta de capital"
)

# Años de las series
anios_de <- function(d) sort(unique(d$Año))

g_participacion <- function(series, a = max(series$Año), base = 15) {
  d <- series |>
    filter(Año == a) |>
    mutate(
      Componente = factor(Componente, levels = rev(orden_comp)),
      Bio = factor(Bio, levels = rev(names(COL_BIO)))
    )
  ggplot(d, aes(pct, Componente, fill = Bio)) +
    geom_col(width = 0.72, color = "white", linewidth = 0.6) +
    geom_text(
      aes(label = ifelse(pct > 0.035, pct(pct, 1), "")),
      position = position_stack(vjust = 0.5),
      color = "white", size = base * 0.26, fontface = "bold"
    ) +
    scale_fill_manual(values = COL_BIO, breaks = names(COL_BIO)) +
    scale_x_continuous(labels = NULL, expand = c(0, 0)) +
    labs(x = NULL, y = NULL) +
    tema_bio(base) +
    theme(panel.grid = element_blank(), axis.text.y = element_text(color = TINTA))
}

g_series <- function(series, base = 13) {
  d <- series |>
    filter(Componente != "Impuestos menos subvenciones") |>
    mutate(
      Componente = factor(Componente, levels = orden_comp),
      Bio = factor(Bio, levels = names(COL_BIO))
    )
  ggplot(d, aes(Año, pct, color = Bio)) +
    geom_line(linewidth = 1) +
    geom_point(size = 2.4) +
    ggrepel::geom_text_repel(
      data = \(x) filter(x, Año %in% range(x$Año)),
      aes(label = pct(pct, 1)),
      size = base * 0.23, show.legend = FALSE, color = TINTA,
      direction = "y", nudge_y = 0.06, min.segment.length = Inf,
      box.padding = 0.15, seed = 1
    ) +
    facet_wrap(~Componente, ncol = 3) +
    scale_color_manual(values = COL_BIO) +
    scale_x_continuous(breaks = anios_de(series), labels = \(x) paste0("'", substr(x, 3, 4)), expand = expansion(mult = 0.12)) +
    scale_y_continuous(
      labels = \(x) paste0(round(100 * x), " %"),
      limits = c(0, 1),
      breaks = c(0, 0.5, 1)
    ) +
    labs(x = NULL, y = NULL) +
    tema_bio(base)
}

g_tasas <- function(tasas, base = 15) {
  d <- tasas |> mutate(Bio = factor(Bio, levels = names(COL_BIO)))
  ggplot(d, aes(Año, tasa, color = Bio)) +
    geom_line(linewidth = 1.1) +
    geom_point(size = 3) +
    geom_text(aes(label = pct(tasa, 1)), vjust = -1, size = base * 0.26, show.legend = FALSE, color = TINTA) +
    scale_color_manual(values = COL_BIO) +
    scale_x_continuous(breaks = anios_de(tasas), expand = expansion(mult = 0.08)) +
    scale_y_continuous(labels = \(x) paste0(round(100 * x), " %"), limits = c(0, 0.22)) +
    labs(x = NULL, y = "Impuestos netos / valor bruto de producción") +
    tema_bio(base)
}

# Valor agregado bioeconómico por los dos métodos
g_medidas <- function(medidas, base = 15, tam = 4) {
  medidas |>
    select(Año, `Método de actividades` = apriori_bio, `Método del consumo intermedio` = via_ci) |>
    pivot_longer(-Año, names_to = "Medida", values_to = "v") |>
    ggplot(aes(Año, v, color = Medida)) +
    geom_line(linewidth = 1.1) +
    geom_point(size = 3) +
    geom_text(aes(label = pct(v), vjust = ifelse(v >= ave(v, Año, FUN = max), -1, 1.9)),
              size = tam, show.legend = FALSE, color = TINTA) +
    scale_color_manual(values = c("Método de actividades" = "#0F7B5F", "Método del consumo intermedio" = "#C27C0E")) +
    scale_x_continuous(breaks = anios_de(medidas), expand = expansion(mult = 0.08)) +
    scale_y_continuous(labels = \(x) paste0(round(100 * x), " %"), limits = c(0, 0.18)) +
    labs(x = NULL, y = "% del valor agregado") +
    tema_bio(base)
}

# Saldo comercial por condición del producto, en % del PIB
g_saldo <- function(comercio, base = 15, tam = 4.2) {
  d <- comercio |> mutate(Bio = factor(Bio, levels = names(COL_BIO)))
  ggplot(d, aes(Año, saldo_pib, color = Bio)) +
    geom_hline(yintercept = 0, color = TINTA2, linewidth = 0.5) +
    geom_line(linewidth = 1.1) +
    geom_point(size = 3) +
    geom_text(aes(label = pct(saldo_pib), vjust = ifelse(saldo_pib > 0, -1, 1.9)),
              size = tam, show.legend = FALSE, color = TINTA) +
    scale_color_manual(values = COL_BIO) +
    scale_x_continuous(breaks = anios_de(comercio), expand = expansion(mult = 0.08)) +
    scale_y_continuous(labels = \(x) paste0(round(100 * x), " %"), expand = expansion(mult = 0.15)) +
    labs(x = NULL, y = "Exportaciones menos importaciones / PIB") +
    tema_bio(base)
}

# Composición de las exportaciones bioeconómicas: los n productos que más
# exportan en el año de análisis y el resto
g_export_comp <- function(export_bio, n = 5, base = 15, tam = 4) {
  ult <- max(export_bio$Año)
  top <- export_bio |> filter(Año == ult, rango <= n) |> arrange(rango)
  nombres <- str_split_fixed(str_remove(top$Producto, "^[^ ]+( [A-Z]+)? - "), "\\. |; | / ", 2)[, 1] |>
    str_remove(" animales y vegetales$") |>
    str_trunc(34)
  nombres[top$Código == "217/218"] <- "Tortas oleaginosas"
  d <- export_bio |>
    mutate(Producto = if_else(Código %in% top$Código, nombres[match(Código, top$Código)], "Otros productos")) |>
    group_by(Año, Producto) |>
    summarise(p = sum(p), .groups = "drop") |>
    mutate(Producto = factor(Producto, levels = rev(c(nombres, "Otros productos"))))
  pal <- c(colorRampPalette(c("#0B4A3A", "#7FC4A8"))(n), "#B9C3BF")
  names(pal) <- c(nombres, "Otros productos")
  ggplot(d, aes(factor(Año), p, fill = Producto)) +
    geom_col(width = 0.72, color = "white", linewidth = 0.5) +
    geom_text(aes(label = ifelse(p > 0.045, pct(p, 0), "")),
              position = position_stack(vjust = 0.5), color = "white", size = tam, fontface = "bold") +
    scale_fill_manual(values = pal, breaks = names(pal)) +
    scale_y_continuous(labels = NULL, expand = c(0, 0)) +
    labs(x = NULL, y = NULL) +
    tema_bio(base) +
    theme(panel.grid = element_blank(), legend.position = "right", legend.justification = "center")
}

g_barras <- function(d, x, y, fill = "Bio", etq = NULL, base = 15, leyenda = TRUE) {
  etq <- etq %||% x
  ggplot(d, aes(.data[[x]], .data[[y]], fill = .data[[fill]])) +
    geom_col(width = 0.72) +
    geom_text(
      aes(label = .data[[etq]]),
      hjust = -0.08, size = base * 0.26, color = TINTA
    ) +
    scale_fill_manual(values = COL_BIO) +
    scale_x_continuous(labels = NULL, expand = expansion(mult = c(0, 0.2))) +
    labs(x = NULL, y = NULL) +
    tema_bio(base) +
    theme(
      panel.grid = element_blank(),
      axis.text.y = element_text(color = TINTA),
      legend.position = if (leyenda) "top" else "none"
    )
}

g_balanza <- function(prod, base = 15) {
  d <- prod |>
    group_by(Bio) |>
    summarise(Exportaciones = sum(Exportaciones), Importaciones = sum(Importaciones), .groups = "drop") |>
    mutate(Saldo = Exportaciones - Importaciones, Bio = factor(Bio, levels = rev(names(COL_BIO))))
  ggplot(d, aes(Saldo, Bio, fill = Bio)) +
    geom_col(width = 0.6) +
    geom_vline(xintercept = 0, color = TINTA2, linewidth = 0.5) +
    geom_text(
      aes(label = num(Saldo), hjust = ifelse(Saldo > 0, -0.1, 1.1)),
      size = base * 0.3, color = TINTA, fontface = "bold"
    ) +
    scale_fill_manual(values = COL_BIO) +
    scale_x_continuous(labels = NULL, expand = expansion(mult = 0.22)) +
    labs(x = NULL, y = NULL) +
    tema_bio(base) +
    theme(legend.position = "none", panel.grid = element_blank(), axis.text.y = element_text(color = TINTA))
}

g_insumos <- function(scn, a, codigos, base = 15) {
  d <- scn |>
    filter(
      Año == a, Componente == "Consumo intermedio",
      `Código Clasificación Actividades` %in% names(codigos),
      `Bioeconomía Productos` %in% names(COL_BIO)
    ) |>
    mutate(Actividad = unname(codigos[`Código Clasificación Actividades`])) |>
    group_by(Actividad, Bio = `Bioeconomía Productos`) |>
    summarise(v = sum(Valor), .groups = "drop") |>
    group_by(Actividad) |>
    mutate(p = v / sum(v)) |>
    ungroup()
  orden <- d |> filter(Bio == "Bioeconomía") |> arrange(p) |> pull(Actividad)
  d <- d |>
    mutate(
      Actividad = factor(Actividad, levels = orden),
      Bio = factor(Bio, levels = rev(names(COL_BIO)))
    )
  ggplot(d, aes(p, Actividad, fill = Bio)) +
    geom_col(width = 0.7, color = "white", linewidth = 0.6) +
    geom_text(
      aes(label = ifelse(p > 0.04, pct(p), "")),
      position = position_stack(vjust = 0.5),
      color = "white", fontface = "bold", size = base * 0.27
    ) +
    scale_fill_manual(values = COL_BIO, breaks = names(COL_BIO)) +
    scale_x_continuous(labels = NULL, expand = c(0, 0)) +
    labs(x = NULL, y = NULL) +
    tema_bio(base) +
    theme(panel.grid = element_blank(), axis.text.y = element_text(color = TINTA))
}

# Preguntas con respuestas que aparecen una por una
qa <- function(preguntas, respuestas, negativas = integer()) {
  filas <- vapply(seq_along(preguntas), function(i) {
    paste0(
      "<div class='q fragment'>", i, ". ", preguntas[i], "</div>",
      "<div class='a fragment", if (i %in% negativas) " neg", "'>", respuestas[i], "</div>"
    )
  }, "")
  html_salida(paste0(
    "<div class='qa'><div class='cab'>Pregunta</div><div class='cab'>Respuesta</div>",
    paste(filas, collapse = ""), "</div>"
  ))
}
