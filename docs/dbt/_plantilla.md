# Título de la convención

## ¿Qué?

Descripción breve de la regla en 1-2 frases.

## ¿Por qué?

Motivación, beneficios y problemas que evita.

## Ejemplo correcto

```sql
-- models/staging/stg_ejemplo.sql
{{ config(materialized='view') }}

select …
from {{ source('fuente', 'tabla') }}
```

## Ejemplo incorrecto

```sql
-- qué NO hacer
```

## Ejemplos reales en el codebase

- `models/marts/fct_ejemplo.sql`

## Excepciones

Situaciones donde la convención no aplica.

## Relacionado

- `docs/otra-convencion.md`
