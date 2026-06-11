# GMF Module Execution — cómo funciona un módulo de principio a fin

Este documento explica cómo el motor de simulación ejecuta los módulos GMF (Generic Module
Framework), usando [`contraceptives.json`](../../inst/extdata/modules/contraceptives.json)
como ejemplo concreto. También sirve de referencia para entender cualquier otro módulo.

## Conceptos previos

### Módulos vs submódulos

| Tipo | Ubicación | Se ejecuta |
|------|-----------|-----------|
| Módulo principal | `inst/extdata/modules/*.json` (raíz) | Automáticamente en cada timestep para cada paciente |
| Submódulo | `inst/extdata/modules/<carpeta>/*.json` | Solo cuando un módulo padre lo invoca con `CallSubmodule` |

`contraceptives.json` es un módulo principal. Los ficheros en
[`inst/extdata/modules/contraceptives/`](../../inst/extdata/modules/contraceptives/)
(`oral_contraceptive.json`, `clear_contraceptive.json`, etc.) son sus submódulos.

### Timestep semanal

`simulate_life` avanza en pasos de 7 días desde el nacimiento hasta `end_date`. En cada
timestep llama a `advance_module` para cada módulo cargado. La mayoría de semanas el módulo
está esperando en un `Delay` o `Guard` y `advance_module` retorna en microsegundos.

### Tipos de transición

| Tipo | Carácter | Ejemplo en `contraceptives.json` |
|------|----------|----------------------------------|
| `direct_transition` | Determinista, siempre el mismo destino | `Reset_Contraceptive_Use → clear_contraceptive` |
| `conditional_transition` | Evalúa condiciones en orden, primera que cumple | `Female_Contraceptive_Use` (routing por edad) |
| `distributed_transition` | Sorteo `runif(1)` según probabilidades | `Young_Contraceptive_Use` (elección de método) |
| `complex_transition` | Primero condición, luego sorteo dentro de esa rama | `oral_contraceptive/Initial` (fecha + disponibilidad) |
| `Delay` | Bloquea el módulo N unidades de tiempo | `Route_To_Guard` (12 meses) |
| `Guard` | Bloquea hasta que una condición sea verdadera | `Pregnant_Guard` (espera a que termine el embarazo) |
| `CallSubmodule` | Pausa el módulo padre y ejecuta un hijo inline | `Using_Oral_Contraceptive` |

---

## Ejemplo paso a paso: 5 mujeres a través de `contraceptives.json`

### Fase 1 — Espera hasta los 14 años

```
Initial → Delay_Until_Reproductive_Age  (14 años exactos)
```

Durante 14 años el módulo no hace nada útil. Cada semana `advance_module` llega al estado
`Delay_Until_Reproductive_Age`, comprueba que el delay no ha expirado y retorna inmediatamente.
Al cumplir 14 años el delay expira y el módulo avanza.

### Fase 2 — Routing por edad (`conditional_transition`)

`Female_Contraceptive_Use` evalúa la edad del paciente. No hay aleatoriedad: la primera
condición que se cumple gana.

```json
// contraceptives.json — estado Female_Contraceptive_Use
"conditional_transition": [
  { "condition": { "condition_type": "Age", "operator": "<", "quantity": 25 },
    "transition": "Young_Contraceptive_Use" },
  { "condition": { "condition_type": "Age", "operator": "<", "quantity": 35 },
    "transition": "Mid_Contraceptive_Use" },
  { "condition": { "condition_type": "Age", "operator": "<", "quantity": 50 },
    "transition": "Mature_Contraceptive_Use" },
  { "transition": "Terminal" }
]
```

Las 5 mujeres tienen 14 años → todas van a `Young_Contraceptive_Use`.

### Fase 3 — Elección de método (`distributed_transition`)

`Young_Contraceptive_Use` sortea el método con `runif(1)`:

```json
// contraceptives.json — estado Young_Contraceptive_Use
"distributed_transition": [
  { "distribution": 0.41, "transition": "Using_Oral_Contraceptive" },
  { "distribution": 0.21, "transition": "Using_Condom_Only" },
  { "distribution": 0.20, "transition": "Using_Withdrawal" },
  { "distribution": 0.09, "transition": "Using_No_Contraceptive" },
  ...
]
```

Resultado del sorteo para cada mujer:

| Paciente | Nacida | runif | Destino |
|----------|--------|-------|---------|
| Ana      | 1955   | 0.21  | `Using_Oral_Contraceptive` |
| Bea      | 1975   | 0.67  | `Using_Withdrawal` |
| Carmen   | 1980   | 0.85  | `Using_No_Contraceptive` |
| Diana    | 1985   | 0.38  | `Using_Oral_Contraceptive` |
| Eva      | 1990   | 0.44  | `Using_Condom_Only` |

**Bea, Carmen y Eva** van a estados `SetAttribute` simples: fijan `contraceptive_type`
directamente y saltan a `Route_To_Guard`.

```
Bea    → contraceptive_type = "withdrawal"
Carmen → contraceptive_type = "none"
Eva    → contraceptive_type = "condom"
```

**Ana y Diana** van a `Using_Oral_Contraceptive`, que es un `CallSubmodule`.

### Fase 4 — CallSubmodule: el padre se pausa

```json
// contraceptives.json — estado Using_Oral_Contraceptive
{ "type": "CallSubmodule",
  "submodule": "contraceptives/oral_contraceptive",
  "direct_transition": "Contraceptive_Prescribed?" }
```

El handler `.state_call_submodule` (en `state_observe.R`) guarda el nombre del submódulo en
`rec` y devuelve `"Contraceptive_Prescribed?"` como next state. Pero antes de avanzar,
`advance_module` detecta el pending call y ejecuta el submódulo inline:

```r
# simulation.R — advance_module, dentro del bucle de estados
sub_name <- rec[[state[["call_key"]]]]
if (!is.null(sub_name)) {
  rec[[state[["call_key"]]]] <- NULL
  person <- advance_module(person, all_modules[[sub_name]], time, all_modules)
}
```

El submódulo [`contraceptives/oral_contraceptive.json`](../../inst/extdata/modules/contraceptives/oral_contraceptive.json)
usa una `complex_transition` para modelar la disponibilidad histórica de la píldora:

```json
// oral_contraceptive.json — Initial
"complex_transition": [
  { "condition": { "condition_type": "Date", "operator": "<", "year": 1960 },
    "distributions": [{ "distribution": 1.0, "transition": "Terminal" }] },
  { "condition": { "condition_type": "Date", "operator": "<", "year": 1970 },
    "distributions": [
      { "distribution": 0.5, "transition": "Prescribe_Oral_Contraceptive" },
      { "distribution": 0.5, "transition": "Terminal" }
    ]},
  ...
]
```

**Ana** (simulación en 1969): `Date < 1970` → sorteo 50/50 → Terminal sin prescripción.
`contraceptive_type` sigue siendo `nil`.

**Diana** (simulación en 1999): supera todas las guards de fecha → prescripción exitosa.
`contraceptive_type = "pill"`.

### Fase 5 — Comprobación del resultado (`conditional_transition`)

De vuelta en el módulo padre, `Contraceptive_Prescribed?` detecta si el submódulo falló:

```json
// contraceptives.json — estado Contraceptive_Prescribed?
"conditional_transition": [
  { "condition": {
      "condition_type": "And",
      "conditions": [
        { "attribute": "contraceptive",      "operator": "is nil" },
        { "attribute": "contraceptive_type", "operator": "is nil" }
      ]},
    "transition": "Historical_Contraceptive_Use" },
  { "transition": "Route_To_Guard" }
]
```

**Ana**: ambos atributos son `nil` → `Historical_Contraceptive_Use` → nuevo sorteo con
métodos pre-1970 (condón, retirada, ninguno) → `contraceptive_type = "withdrawal"`.

**Diana**: `contraceptive_type = "pill"` → directo a `Route_To_Guard`.

### Fase 6 — El módulo "duerme" un año (`Delay`)

Todas llegan a `Route_To_Guard`, un `Delay` de 12 meses:

```
Semanas 1–51:  advance_module llega a Route_To_Guard, delay no expirado → BREAK
Semana 52:     delay expirado → continúa
```

Mientras el módulo duerme, [`female_reproduction.json`](../../inst/extdata/modules/female_reproduction.json)
corre cada ~28 días y lee `contraceptive_type` para calcular la probabilidad mensual de
embarazo. Carmen (`"none"`) tiene un 19.3% de probabilidad por ciclo; Diana (`"pill"`) un 0.69%.

### Fase 7 — Reasignación anual

```
Route_To_Guard expirado
→ Reset_Contraceptive_Use        (SetAttribute: borra contraception_care_reason)
→ CallSubmodule: clear_contraceptive   ← cierra prescripción activa, borra contraceptive_type
→ Female_Contraceptive_Use             ← nuevo routing por edad + sorteo
→ ... (vuelve a Fase 2)
```

Si Carmen se quedó embarazada, `female_reproduction` activa `Pregnancy_Guard` (bloqueo hasta
`pregnant == false`). Al terminar el embarazo, el módulo de contracepción la redirige
directamente a `Female_Contraceptive_Use` sin esperar el año, para reasignar método.

---

## Diagrama de flujo simplificado

```
nacimiento
    │
    ▼
Delay 14 años
    │
    ▼
Female_Contraceptive_Use ◄──────────────────────────────────┐
    │ (conditional: edad)                                    │
    ├─ <25 → Young_Contraceptive_Use                        │
    ├─ <35 → Mid_Contraceptive_Use    (distributed: método) │
    └─ <50 → Mature_Contraceptive_Use                       │
                 │                                          │
                 ▼                                          │
    Using_X (SetAttribute)                                  │
    o CallSubmodule → submodule prescribe                   │
                 │                                          │
                 ▼                                          │
    Contraceptive_Prescribed?  ──nil──► Historical_Use      │
                 │ (ok)                      │              │
                 └──────────────────────────┘              │
                 │                                          │
                 ▼                                          │
    Route_To_Guard (Delay 12 meses)                         │
                 │                                          │
                 ▼                                          │
    Reset → clear_contraceptive (submodule) ────────────────┘
```

---

## Lectura relacionada

- [`docs/r/rsynthea-hot-path-performance.md`](rsynthea-hot-path-performance.md) — por qué
  los estados están ordenados por frecuencia de acceso en `GMFState`.
- [`R/simulation.R`](../../R/simulation.R) — `advance_module` y el bucle de estados.
- [`R/state_flow.R`](../../R/state_flow.R) — dispatcher de handlers por tipo de estado.
- [`R/transition.R`](../../R/transition.R) — implementación de cada tipo de transición.
