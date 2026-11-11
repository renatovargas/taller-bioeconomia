# Auditoría de la clasificación de Argentina contra el archivo original

Fecha: 2026-11-11. Se buscaron corrimientos entre la clasificación actual (`data/config/arg_lookups.xlsx`) y el archivo original (`ARG_Clasificaciones.xlsx` más las equivalencias CSV de los commits 92f1f15 y 06dfa61, idénticos a la copia en `refs/data/arg-cou/`).

## Resultado

| Prueba | Resultado |
|---|---|
| Categoría y grupo CPC de cada producto: lookup vs original | Idénticos. Solo difieren los códigos nuevos de INDEC (715 CSCD desde 2019, 651/652 desde 2020) |
| Categoría y sección CIIU de cada actividad: lookup vs original (columnas de oferta, utilización y VA) | Idénticos. 29300 tenía la sección vacía ("-") solo en la columna de VA |
| Valores de 2018: base original de la CEPAL (`refs/data/arg-db/ARG_SCN_BD_descriptivo.xlsx`) vs base actual, por par de códigos | Coinciden las 7.954 celdas producto × actividad, las transacciones por producto y el VA por actividad (diferencia máxima 5e-7 miles de pesos) |
| Código de cada fila de la base original vs código de INDEC en esa posición del COU 2018 | Coinciden las 223 filas: no hay corrimiento posicional |
| Nombres del lookup vs etiquetas de INDEC, 2018 a 2023 | Coinciden, salvo los alias de utilización (15120, 15130, 15140, 15200) y "PÚBLICO" con tilde en 2020 |
| Sección CIIU vs rango del código CIIU Rev. 3 | Una anomalía: 050 Pesca |

## Corrección aplicada

**050 Pesca.** Estaba en la sección B ("Explotación de minas y canteras", denominación de la CIIU Rev. 4) y clasificada como No bioeconomía. En la CIIU Rev. 3 de INDEC, la sección B es Pesca, lo que sugiere que la letra se trasladó entre revisiones sin reasignar la actividad. El 100 % de su producción es el producto 041/042/049 (pescado), clasificado como Bioeconomía. Ahora está en la sección A y en Bioeconomía.

Efecto en el valor agregado bioeconómico:

| Año | Por actividades | Por consumo intermedio |
|---|---|---|
| 2018 | 10,9 % → 11,3 % | 12,3 % → 12,4 % |
| 2023 | 12,5 % → 12,8 % | 14,4 % → 14,5 % |

Con la corrección, el método de actividades supera al del consumo intermedio en 2020 y 2021. Antes, solo en 2021. Los textos que comparan los dos métodos se calculan ahora a partir de los datos.

## Casos pendientes de decisión (no son corrimientos)

Actividades cuya categoría difiere de la de su producto principal en 2023:

| Actividad | Categoría | Producto principal (participación) | Categoría del producto |
|---|---|---|---|
| 016 Construcciones agropecuarias | Bioeconomía | 540 CA Construcción (61 %) | Fuera |
| 222 Impresión | Fuera | 326 Libros de registro (34 %) | Parcial |
| 2411 Gases comprimidos | Fuera | 341 Químicos orgánicos básicos (33 %) | Parcial |
| 292 Maquinaria de uso especial | Fuera | 441 Maquinaria agrícola (81 %) | Parcial |
| 37/38 Reciclado, reparación | Parcial | 871/2/3 Mantenimiento (44 %) | Fuera |
| 41 Agua | Parcial | 692 H Distribución de agua (77 %) | Fuera |
| 552 Restaurantes | Parcial | 632 Suministro de comida (96 %) | Fuera |
| 900 Desechos | Parcial | 940 Alcantarillado (100 %) | Fuera |

Otros puntos:

- 692 H (distribución de agua a hogares) es Fuera de la bioeconomía y 692 R (al resto) es parcial.
- **Resuelto:** los productos de distribución de electricidad, gas y agua (6911 H, 6912 H, 6911R/6912R, 692 H, 692 R) pasaron del grupo CPC "Otros servicios" a "Servicios básicos", con 171 Energía eléctrica. 940 Alcantarillado sigue en "Otros servicios".
