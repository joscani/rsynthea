# rsynthea

> Índice delgado para agentes de IA. Las convenciones detalladas viven en `docs/`. Las reglas globales (idioma, seguridad, stack general) están en `~/.claude/AGENTS.md`.

## Comandos útiles

```bash
# Tests R
Rscript -e 'devtools::load_all("."); testthat::test_dir("tests/testthat")'

# Benchmark rápido (10 pacientes, serial)
Rscript -e 'devtools::load_all("."); m <- load_all_modules(); system.time(generate_population(10, seed=1L, modules=m, end_date=as.POSIXct("2020-01-01")))'

# Con paralelismo (todos los cores)
Rscript -e 'devtools::load_all("."); m <- load_all_modules(); system.time(generate_population(10, seed=1L, modules=m, end_date=as.POSIXct("2020-01-01"), mc.cores=parallel::detectCores(logical=FALSE)))'

# Perfil rápido
Rscript -e 'devtools::load_all("."); m <- load_all_modules(); Rprof("/tmp/prof.out", interval=0.005); generate_population(3, seed=1L, modules=m, end_date=as.POSIXct("2020-01-01")); Rprof(NULL); print(head(summaryRprof("/tmp/prof.out")$by.self[order(-summaryRprof("/tmp/prof.out")$by.self$self.time),], 15))'
```

## Estructura

```
./
├── R/                # código del paquete
│   ├── simulation.R      # hot-path: simulate_life, advance_module
│   ├── state_flow.R      # process_state, state handlers de control
│   ├── state_clinical.R  # state handlers clínicos (encounter, condition…)
│   ├── state_observe.R   # observation, vital sign, submodule
│   ├── logic.R           # evaluate_condition, cond_*, .REC cache
│   ├── module.R          # GMFState, Module, load_all_modules
│   ├── generator.R       # generate_population (entrada principal)
│   ├── classes.R         # clase Person (S7)
│   ├── transition.R      # resolve_transition
│   └── export.R          # export_population → tibbles
├── tests/testthat/   # tests (testthat)
├── inst/extdata/modules/  # módulos GMF en JSON (242 módulos)
└── docs/             # convenciones detalladas
```

## Documentación

Convenciones detalladas en `docs/`. **Lee solo los ficheros relevantes para la tarea en curso**.

```
docs/
├── r/
│   └── rsynthea-hot-path-performance.md  ← LEER antes de tocar simulation.R / logic.R
└── superpowers/plans/
    └── 2026-06-03-rsynthea-package.md    # plan original
```

## Contexto del proyecto

- **Objetivo**: Port en R de py-synthea — simula historiales clínicos sintéticos de pacientes usando módulos de estado GMF (Generic Module Framework). Salida: tibbles (patients, encounters, conditions, medications…).
- **Stack R**: S7 para Person, entornos mutables para records clínicos, testthat para tests.
- **Rendimiento actual**: ~4.4s/10 pacientes serial; ~1.3s con 12 cores. Baseline original: ~300s.

## Cosas que evitar

- **No toques el orden de campos en `GMFState`** — están ordenados por frecuencia de acceso para minimizar búsqueda lineal en `[[name]]`.
- **No uses `person@.record` en state handlers** — usa `rec <- .REC$e` (ya cacheado).
- **No intentes `.RES` mutable para evitar `list(person, next_state)`** — falló (+220ms regresión, ver doc de rendimiento).
- **No uses `as.numeric(time)` en el hot-path** — usa `rec$.t_num`.
- **No uses `result$person` / `result$next_state`** — usa `result[[1L]]` / `result[[2L]]`.
