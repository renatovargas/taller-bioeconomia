# Categorías de la clasificación bioeconómica
# ===========================================
# Única fuente de los nombres y colores: la hoja "categorias" de
# data/config/arg_lookups.xlsx. Siguen la terminología del SCN 2025 para
# cuentas temáticas (capítulo 38):
#   1 Bioeconomía              dentro de la frontera de la actividad clave
#   2 Bioeconomía parcial      parcialmente dentro (contenido biológico y no
#                              biológico que el COU no permite separar)
#   3 Fuera de la bioeconomía  fuera de la frontera

categorias <- readxl::read_excel("data/config/arg_lookups.xlsx", sheet = "categorias", col_types = "text")
niveles_bio <- categorias$Categoría
B_BIO <- niveles_bio[1]
B_PAR <- niveles_bio[2]
B_FUE <- niveles_bio[3]
COL_BIO <- setNames(categorias$Color, niveles_bio)

# Columnas de actividades de los cuadros bioeconómicos: categoría | sector
COLS_ACT <- c(
  paste(B_BIO, c("A-B Sector primario", "C Manufacturas"), sep = " | "),
  paste(B_PAR, c("C Manufacturas", "D-S Servicios"), sep = " | "),
  paste(B_FUE, c("A-B Sector primario", "C Manufacturas", "D-S Servicios"), sep = " | ")
)
