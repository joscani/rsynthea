# Convenciones del proyecto

Cada subcarpeta agrupa convenciones por tecnología. Un fichero por convención.

- `python/` — estilo, estructura, testing, tipado.
- `dbt/` — naming, materializaciones, tests, macros.
- `r/` — estilo, renv, tidymodels, reporting, arquitectura rsynthea.
- `sql/` — dialecto, formato, performance, ClickHouse.

### Ficheros en `r/`

- [`rsynthea-hot-path-performance.md`](r/rsynthea-hot-path-performance.md) — optimizaciones del hot-path; leer antes de tocar `simulation.R` / `logic.R`.
- [`gmf-module-execution.md`](r/gmf-module-execution.md) — cómo funciona un módulo GMF de principio a fin, con ejemplo completo de `contraceptives.json`.

## Cómo añadir una convención

Usa el comando `/create-doc` tras una conversación en la que el agente haya sido corregido sobre una práctica del equipo. Crea el fichero en la subcarpeta que corresponda y actualiza el índice de `AGENTS.md`.
