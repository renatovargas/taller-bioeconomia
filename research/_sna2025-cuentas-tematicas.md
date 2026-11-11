# SCN 2025, capítulo 38: cuentas temáticas y extendidas

Resumen de investigación para decidir las denominaciones de la clasificación bioeconómica de Argentina. Fuente: *2025 SNA, Chapter 38: Thematic and Extended Accounts*, borrador V11, 13 páginas, párrafos 38.1 a 38.44 (<https://unstats.un.org/unsd/nationalaccount/SNAUpdate/2025/2025SNA_CH38_V11.pdf>). Copia local, fuera del repositorio: `refs/sna2025/2025SNA_CH38_V11.pdf`.

## Lo que cambia respecto del SCN 2008

- **Las cuentas satélite cambian de nombre.** "Thematic and extended accounts (formerly known as satellite accounts)" (38.2).
  - **Cuentas temáticas:** reordenan o desagregan lo que ya está en el marco central.
  - **Cuentas extendidas:** amplían o modifican la frontera de producción, de consumo o de activos (38.6 a 38.14).
  - Una cuenta de bioeconomía construida con el COU, sin cambiar fronteras, es una **cuenta temática**.
- **El concepto organizador es la "key activity" (actividad clave),** un grupo de industrias y productos de interés (38.21 y 38.22). Puede tomar industrias de distintas secciones de la CIIU y productos de distintas secciones de la CPC.
- **No hay categorías "característicos" o "conexos".** El capítulo no usa "characteristic products", "connected products" ni ninguna tercera categoría fija, y no menciona la bioeconomía. Esos términos vienen del SCN 2008 (capítulo 29) y de la cuenta satélite de turismo, donde "conexo" es un concepto de demanda.

## Cómo define el alcance

- **Hay que definir una frontera precisa** de la actividad clave antes de compilar (38.29).
- **Industrias (columnas):** "Identify the industry columns of the supply and use tables that are within the bounds, or partly within the bounds, of the key activity". Las columnas que "straddle the boundary" deben desagregarse para separar la parte que está dentro del ámbito (38.32 i).
- **Productos (filas):** se desagregan "the rows ... that cover the key products produced by, or used in, the key activity, including the relevant parts of rows that straddle the product boundary" (38.32 ii).
- **Producción secundaria:** "Special aggregations of products in a key activity thematic account may include relevant secondary or ancillary outputs of industries whose primary product is out-of-scope" (38.24). Esto respalda clasificar productos y actividades por separado, que es el centro del método de Vargas y otros (2022).
- **Cinco dimensiones posibles para las agregaciones alternativas:** propósito del gasto, características del producto, usuario, productor y características de la transacción (38.24). La bioeconomía usa la segunda, características del producto: el origen biológico.
- **Terminología:** debe distinguir con claridad los conceptos alternativos de los estándar del SCN (38.14).
- **Indicadores complementarios recomendados:** capital fijo, indicadores físicos, precios y volúmenes, empleo y emisiones (38.33 a 38.35).

## Correspondencia con nuestras tres categorías

| Categoría actual | Concepto del SCN 2025 | Observación |
|---|---|---|
| Bioeconomía | Dentro de la frontera de la actividad clave | Todo el contenido es de origen biológico |
| Bioeconomía extendida | Parcialmente dentro de la frontera ("partly within the bounds", "straddle the boundary") | El SCN 2025 pide desagregar estas filas y columnas. Con el detalle del COU de INDEC no se puede, por eso quedan como una categoría propia |
| No bioeconomía | Fuera de la frontera ("out-of-scope") | |

El SCN 2025 no fija nombres en español para estas categorías; los de la tabla son una traducción propia. Habría que verificar si existe una versión oficial en español del SCN 2025 antes de fijar la terminología.

## Decisión (2026-11-11)

No se encontró una traducción oficial al español del SCN 2025. Se adoptan estas denominaciones, con los mismos códigos 1, 2 y 3:

| Código | Categoría | Método 2022 (CEPAL) |
|---|---|---|
| 1 | Bioeconomía | Característicos de la bioeconomía |
| 2 | Bioeconomía parcial | De características extendidas |
| 3 | Fuera de la bioeconomía | No característicos |

Los nombres viven solo en la hoja `categorias` de `data/config/arg_lookups.xlsx`, y `scripts/categorias.R` los carga para todos los scripts. "Cuenta satélite" pasa a "cuenta temática" en todo el sitio, salvo en los títulos de publicaciones anteriores.
