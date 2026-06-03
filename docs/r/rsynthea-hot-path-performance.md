# Patrones de rendimiento para el hot-path de rsynthea

## ¿Qué?

Guía de las técnicas de optimización aplicadas al loop de simulación de `rsynthea`. El motor procesa ~944K llamadas a `process_state` por paciente de 34 años; las micro-optimizaciones en el hot-path tienen efecto acumulativo grande.

## ¿Por qué?

La simulación es interpretada en R puro sobre un state machine con 242 módulos × 1.820 timesteps semanales × N pacientes. Sin estas técnicas el baseline era ~300s/10 pacientes; con todas son ~4.4s serial y ~1.3s con 12 cores.

---

## Técnicas activas (qué hace el código y por qué)

### 1. Cache global `.REC$e` — evitar S7 dispatch en el hot-path

`person@.record` se guarda en `.REC$e` UNA vez al inicio de `simulate_life`. Todos los state handlers leen `rec <- .REC$e` en vez de `person@.record` en cada llamada.

```r
# simulate_life: una sola vez por paciente
rec <- person@.record
.REC$e <- rec

# state handlers: acceso directo sin dispatch S7
rec <- .REC$e
rec$encounters[[n]] <- enc_env   # O(1), sin copia de Person
```

`person@.record` activa el operador `@` de S7 (~500ns de dispatch). Con 944K accesos por paciente, eso son ~0.5s evitados.

---

### 2. Timestamp numérico cacheado `rec$.t_num`

`as.numeric(time)` se llama una vez por timestep en `simulate_life` y se guarda en `rec$.t_num`. Los state handlers lo reusan sin recalcular.

```r
# simulate_life: una vez por semana
rec$.t_num <- t_cur

# .state_delay, .cond_date, .cond_age: usan el cache
t_num <- rec$.t_num %||% as.numeric(time)  # fallback para tests directos
```

---

### 3. Cache de fechas en `.cond_date`

Las condiciones de tipo `Date` llaman `as.POSIXct(paste(y, m, d))` que internamente ejecuta `strptime` (parseo completo). Como hay solo ~20 fechas únicas pero se evalúan ~38K veces por paciente, se cachean en `.DATE_CACHE`.

```r
.DATE_CACHE <- new.env(parent = emptyenv(), hash = TRUE)

.cond_date <- function(cond, time) {
  key <- paste0(y, "-", m, "-", d)
  target <- .DATE_CACHE[[key]]
  if (is.null(target)) {
    target <- as.numeric(as.POSIXct(key))
    .DATE_CACHE[[key]] <- target
  }
  .compare(.REC$e$.t_num %||% as.numeric(time), cond[["operator"]] %||% "==", target)
}
```

Ahorro: ~684ms (la optimización más grande de la sesión).

---

### 4. Edad calculada numéricamente en `.cond_age`

La función `age_at` hace dos conversiones `as.POSIXlt` por llamada. `.cond_age` usa aritmética numérica directa y cachea `birth_num` (solo cambia entre pacientes, no entre timesteps).

```r
.cond_age <- function(cond, person, time) {
  rec <- .REC$e
  birth_num <- rec$.birth_num
  if (is.null(birth_num)) {
    birth_num <- as.numeric(person@attributes[["birth_date"]])
    rec$.birth_num <- birth_num
  }
  age <- (rec$.t_num %||% as.numeric(time) - birth_num) / (365.25 * 86400)
  ...
}
```

La aproximación (±1 día en fechas de cumpleaños) es irrelevante con timesteps semanales.

---

### 5. Pre-skip de módulos terminales en `simulate_life`

De 242 módulos × 1.820 timesteps = 440K llamadas a `advance_module` por paciente, la mayoría retornan inmediatamente al ver `__terminal__`. El check en `simulate_life` evita el overhead de llamada de función.

```r
for (module in modules) {
  if (identical(rec[[module$state_key]], "__terminal__")) next  # evita la llamada
  person <- advance_module(person, module, current_time, modules)
}
```

Ahorro: ~780ms (segunda optimización más grande).

---

### 6. Orden del `switch` en `process_state` por frecuencia de llamada

R's `switch()` con strings hace búsqueda lineal. Los tipos más frecuentes van primero:

```r
switch(state[["type"]],
  "Delay"          = ...,   # ~91K llamadas/paciente
  "Guard"          = ...,   # ~63K
  "Simple"         = ...,   # ~50K
  "Encounter"      = ...,   # ~20K
  "EncounterEnd"   = ...,   # ~20K
  "Initial"        = ...,   # ~10K
  ...
)
```

---

### 7. Acceso posicional a listas en el inner loop

`result$person` / `result$next_state` se reemplazan por `result[[1L]]` / `result[[2L]]` — evita la búsqueda por nombre (1.9M llamadas por paciente).

```r
result    <- process_state(state, person, time)
person    <- result[[1L]]    # no result$person
next_name <- result[[2L]]    # no result$next_state
```

**Invariante**: todos los retornos de state handlers tienen `person` en posición 1 y `next_state` en posición 2.

---

### 8. Bypass de wellness encounters en `advance_module`

Los módulos de wellness quedan en su estado `Encounter` mientras esperan el siguiente turno. Sin el bypass, cada timestep reentraría a `process_state → .state_encounter` (que devuelve inmediatamente). El bypass lo detecta antes de llamar a `process_state`.

```r
if (state[["type"]] == "Encounter" && state[["is_wellness"]]) {
  wt <- rec[[state[["wellness_key"]]]]
  if (!is.null(wt) && wt >= rec$.t_num) break  # ya visitado este timestep
}
```

---

### 9. States del módulo como entorno hash

`module$states` es un entorno (`list2env(..., hash=TRUE)`) en vez de una named list. `mod_states[[current_name]]` es O(1) en vez de O(N).

```r
# En load_module:
states_env <- list2env(states_list, parent = emptyenv(), hash = TRUE)
Module(name = name, states = states_env)
```

---

### 10. Paralelismo por paciente con `mc.cores`

Cada paciente es independiente. En Unix/macOS se usa `parallel::mclapply` (fork-based, sin serialización).

```r
generate_population(n = 100, seed = 1L, modules = modules,
                    end_date = as.POSIXct("2020-01-01"),
                    mc.cores = parallel::detectCores(logical = FALSE))
```

Escalado medido con 12 cores físicos:
- 10 pacientes: 3.6× speedup
- 50 pacientes: 6.5× speedup
- 100 pacientes: ~8-10× speedup (estimado)

**Nota sobre IDs**: con `mc.cores > 1`, el contador `.id_counter$n` se reinicia en cada proceso hijo — los IDs generados por `.new_id()` pueden colisionar entre pacientes distintos. Para producción con IDs únicos globales, añadir el seed del paciente como prefijo en export.

---

## Qué NO hacer (optimizaciones que fallaron)

### `.RES` — entorno mutable como return value

Intento de evitar 944K allocaciones de `list(person, next_state)` usando un entorno global compartido:

```r
# MALO: causó regresión de +220ms
.RES$person     <- person
.RES$next_state <- next_name
```

El overhead de `$<-` en un entorno global + lookup superó el coste de allocar la lista. Las listas pequeñas en R son muy baratas (~0.5ns por puntero).

### Pre-check de Terminal en `advance_module`

Antes de cada transición, se miraba si el estado destino era `Terminal`:

```r
# MALO: 2 hash lookups por transición para evitar 1 llamada extra
next_state_obj <- mod_states[[current_name]]
if (!is.null(next_state_obj) && next_state_obj[["type"]] == "Terminal") ...
```

La llamada extra a `process_state("Terminal", ...)` es O(50) veces por paciente; los 2 hash lookups por cada una de las ~500K transiciones son O(1M). Se eliminó.

---

## Ejemplos reales en el codebase

- `R/simulation.R` — loop principal, wellness bypass, terminal pre-skip
- `R/logic.R` — `.DATE_CACHE`, `.cond_age` numérico, `.REC$e`
- `R/state_flow.R` — orden del switch, acceso posicional `[[1L]]/[[2L]]`
- `R/state_clinical.R` — `.state_encounter` con env mutable para end_time
- `R/module.R` — `GMFState` con campos ordenados por frecuencia, `list2env` para states
- `R/generator.R` — `mc.cores` con `mclapply`

---

## Excepciones

- Los tests que llaman `process_state()` directamente (sin `simulate_life`) no tienen `rec$.t_num` ni `rec$.birth_num` inicializados → los fallbacks `%||% as.numeric(time)` son necesarios.
- El `.DATE_CACHE` es global al paquete. Para tests que necesiten fechas reproducibles, el cache está caliente entre tests (no afecta corrección, solo rendimiento).

## Relacionado

- `docs/superpowers/plans/2026-06-03-rsynthea-package.md` — plan original del paquete
