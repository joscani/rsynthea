# Título de la convención

## ¿Qué?

Descripción breve de la regla en 1-2 frases.

## ¿Por qué?

Motivación, beneficios y problemas que evita.

## Ejemplo correcto

```sql
select
    user_id,
    count(*) as n_eventos
from eventos
where fecha >= toDate('2026-01-01')
group by user_id
```

## Ejemplo incorrecto

```sql
-- qué NO hacer
```

## Ejemplos reales en el codebase

- `sql/consultas/ejemplo.sql`

## Excepciones

Situaciones donde la convención no aplica.

## Relacionado

- `docs/otra-convencion.md`
