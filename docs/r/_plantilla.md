# Título de la convención

## ¿Qué?

Descripción breve de la regla en 1-2 frases.

## ¿Por qué?

Motivación, beneficios y problemas que evita.

## Ejemplo correcto

```r
# código que cumple la convención
library(tidyverse)

datos |>
  filter(ano == 2026) |>
  summarise(precio_medio = mean(precio))
```

## Ejemplo incorrecto

```r
# código que la incumple
```

## Ejemplos reales en el codebase

- `R/modulo.R:42`

## Excepciones

Situaciones donde la convención no aplica.

## Relacionado

- `docs/otra-convencion.md`
