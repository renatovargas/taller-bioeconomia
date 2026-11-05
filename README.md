# Taller de bioeconomía

Minisitio de Quarto con los materiales de un taller sobre cuentas satélite de bioeconomía, con el caso de Argentina.

## Estructura

- `slides/`: tres presentaciones de revealjs y su tema (`pstyles.scss`).
- `materiales/`: páginas que embeben las presentaciones, documento y descargas.
- `data/`: COU de INDEC, clasificaciones, base de datos y resultados.
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

`quarto` puede usarse desde Positron: `/usr/share/positron/resources/app/quarto/bin/quarto`.
