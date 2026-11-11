# Taller de bioeconomía

Minisitio de Quarto con los materiales de un taller sobre cuentas satélite de bioeconomía, con el caso de Argentina.

## Estructura

- `slides/`: tres presentaciones de revealjs y su tema (`pstyles.scss`).
- `materiales/`: páginas que embeben las presentaciones, documento y descargas.
- `data/indec-cou/`: COU de INDEC tal como se publican, 2018 a 2023.
- `data/config/`: `arg_config.xlsx` (qué leer de cada año) y `arg_lookups.xlsx` (clasificación por código, con vigencia por año).
- `data/bd/arg_scn.rds`: base plana con todos los años, en millones de pesos, con su clasificación.
- `data/bd/` y `data/salidas/`: Excel para el taller y resultados del año más reciente.
- `scripts/`: procesamiento en R.
- `docs/`: sitio generado para GitHub Pages.
- `refs/`: referencias locales, fuera del repositorio.

## Reproducir

Desde la raíz del proyecto:

```bash
Rscript scripts/01-procesar-cou-argentina.R
Rscript scripts/02-cuadros-y-resultados.R
quarto render
```

Para agregar un año: copie el `.xls` de INDEC en `data/indec-cou/`, agregue su renglón en `arg_config.xlsx` y, si INDEC cambió códigos, ajuste `Desde` y `Hasta` en `arg_lookups.xlsx`. La rutina se detiene y dice qué código falta o sobra.

`quarto` puede usarse desde Positron: `/usr/share/positron/resources/app/quarto/bin/quarto`.
