# rsynthea

> Índice delgado para agentes de IA. Las convenciones detalladas viven en `docs/`. Las reglas globales (idioma, seguridad, stack general) están en `~/.claude/AGENTS.md`.

## Comandos útiles

```bash
# Tests R
Rscript -e 'devtools::load_all("."); testthat::test_dir("tests/testthat")'

# Motor C++ — 2.7x más rápido que Java Synthea (RECOMENDADO)
Rscript -e '
devtools::load_all(".")
m <- load_all_modules()
cpp_m <- compile_all_modules(m)   # compila 243 módulos en ~0.2s, reusar entre llamadas
system.time(generate_population(20, seed=1L, modules=m,
  end_date=as.POSIXct("2020-01-01"), use_cpp=TRUE, cpp_modules=cpp_m))
'

# Motor C++ con parallelismo (6 cores)
Rscript -e '
devtools::load_all(".")
m <- load_all_modules(); cpp_m <- compile_all_modules(m)
system.time(generate_population(200, seed=1L, modules=m,
  end_date=as.POSIXct("2020-01-01"), use_cpp=TRUE, cpp_modules=cpp_m, mc.cores=6L))
'

# Motor R original (serial, fallback)
Rscript -e 'devtools::load_all("."); m <- load_all_modules(); system.time(generate_population(10, seed=1L, modules=m, end_date=as.POSIXct("2020-01-01")))'

# Motor R con paralelismo
Rscript -e 'devtools::load_all("."); m <- load_all_modules(); system.time(generate_population(10, seed=1L, modules=m, end_date=as.POSIXct("2020-01-01"), mc.cores=parallel::detectCores(logical=FALSE)))'

# Perfil rápido (motor R)
Rscript -e 'devtools::load_all("."); m <- load_all_modules(); Rprof("/tmp/prof.out", interval=0.005); generate_population(3, seed=1L, modules=m, end_date=as.POSIXct("2020-01-01")); Rprof(NULL); print(head(summaryRprof("/tmp/prof.out")$by.self[order(-summaryRprof("/tmp/prof.out")$by.self$self.time),], 15))'
```

## Estructura

```
./
├── R/                # código del paquete
│   ├── simulation.R      # motor R: simulate_life, advance_module (fallback)
│   ├── generator.R       # generate_population — parámetros: use_cpp, cpp_modules, mc.cores
│   ├── cpp_engine.R      # wrapper del motor C++ con barra de progreso
│   ├── RcppExports.R     # exportaciones Rcpp auto-generadas
│   ├── state_flow.R      # process_state, state handlers de control (motor R)
│   ├── state_clinical.R  # state handlers clínicos (motor R)
│   ├── logic.R           # evaluate_condition, .REC cache (motor R)
│   ├── module.R          # GMFState, Module, load_all_modules
│   ├── classes.R         # clase Person (S7)
│   ├── transition.R      # resolve_transition (motor R)
│   └── export.R          # export_population → tibbles (motor R)
├── src/              # motor C++ (Rcpp/C++17)
│   ├── rsynthea.h        # tipos base: AttrVal, CppCode
│   ├── person_record.h   # PersonRecord: estado completo de un paciente en C++
│   ├── conditions.h/cpp  # evaluador de condiciones GMF (19 tipos)
│   ├── transitions.h/cpp # resolutor de transiciones (5 tipos)
│   ├── module.h          # StateType enum, CppState, CppModule
│   ├── module_compiler.cpp # compile_all_modules(): JSON → structs C++
│   ├── condition_eval.h/cpp # evaluate_condition_cpp()
│   ├── simulation.h/cpp  # simulate_life_cpp(), advance_module_cpp()
│   ├── state_handlers.cpp # dispatch_state(): 30 tipos de estado
│   ├── simulate_patients.cpp # interfaz R→C++: simulate_patient_cpp()
│   ├── export_records.cpp # generate_and_export_cpp(): sim + export tibbles
│   └── Makevars          # CXX17, -O3
├── tests/testthat/   # tests (testthat)
├── inst/extdata/modules/  # módulos GMF en JSON (243 módulos)
└── docs/             # convenciones detalladas
```

## Motor C++ — API

```r
# 1. Compilar módulos (una vez por sesión, ~0.2s)
m       <- load_all_modules()
cpp_m   <- compile_all_modules(m)    # devuelve XPtr<vector<CppModule>>

# 2. Generar población (devuelve lista de tibbles)
tbls <- generate_population(
  n           = 200,
  seed        = 1L,
  modules     = m,
  end_date    = as.POSIXct("2020-01-01"),
  use_cpp     = TRUE,        # activa motor C++
  cpp_modules = cpp_m,       # módulos pre-compilados
  mc.cores    = 6L,          # paralelismo opcional
  min_age     = 30L,         # filtro de edad opcional
  max_age     = 70L
)

# tbls es una lista con: patients, encounters, conditions,
#   medications, procedures, observations, allergies, immunizations
```

## Rendimiento (Linux x86, seed=1, end_date=2020-01-01)

| Implementación | Modo | 20 pac | 200 pac |
|---|---|---|---|
| **rsynthea C++** | serial | **4.96s** | ~50s |
| **rsynthea C++** | 6 cores | ~1.5s | ~17s |
| Java Synthea | multihilo | 13.4s | ~2min |
| py-synthea | 1 hilo | 14.3s | ~2.5min |
| rsynthea R | serial | 55.6s | ~550s |

## Documentación

Convenciones detalladas en `docs/`. **Lee solo los ficheros relevantes para la tarea en curso**.

```
docs/
├── r/
│   └── rsynthea-hot-path-performance.md  ← LEER antes de tocar simulation.R / logic.R
├── superpowers/
│   ├── progress.md       ← log de la reingeniería C++ (iteraciones, benchmarks, bugs)
│   └── plans/
│       └── 2026-06-03-rsynthea-package.md
```

## Contexto del proyecto

- **Objetivo**: Port en R de py-synthea — simula historiales clínicos sintéticos de pacientes usando módulos de estado GMF (Generic Module Framework). Salida: tibbles (patients, encounters, conditions, medications…).
- **Stack**: S7 para Person, motor C++ Rcpp/C++17 (primary), motor R puro (fallback).
- **Rendimiento motor C++** (Linux x86): ~5s/20 pac serial = 2.7x Java, 2.9x Python.
- **Validación prevalencias** (500 pac 30-70 años): diabetes 23%, prediabetes 33%, hipertensión 52% — dentro de rangos EEUU.

## Cosas que evitar

### Motor C++
- **No llames `compile_all_modules()` en cada llamada a `generate_population()`** — cuesta ~0.2s, reusar entre llamadas.
- **No modifiques `parse_quantity()` para aplicar `unit_secs()` a VitalSign/Observation/Symptom** — esos valores son medidas crudas (%, bpm, etc.), no duraciones. Solo Delay y SetAttribute con distribución usan conversión de unidades.
- **No uses `module_current` map para estado de módulos principales** — usa `module_states_flat[midx]` (acceso O(1) sin hash).

### Motor R (si lo tocas)
- **No toques el orden de campos en `GMFState`** — están ordenados por frecuencia de acceso.
- **No uses `person@.record` en state handlers** — usa `rec <- .REC$e`.
- **No intentes `.RES` mutable** — falló (+220ms regresión).
- **No uses `as.numeric(time)` en el hot-path** — usa `rec$.t_num`.
- **No uses `result$person` / `result$next_state`** — usa `result[[1L]]` / `result[[2L]]`.
