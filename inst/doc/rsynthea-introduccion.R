## ----setup, include=FALSE-----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment  = "#>",
  fig.width = 7,
  fig.height = 4
)


## ----basico, eval=FALSE-------------------------------------------------
# library(rsynthea)
# 
# tbls <- generate_population(
#   n        = 200,
#   seed     = 42L,
#   end_date = as.POSIXct("2023-01-01"),
#   min_age  = 40L,
#   max_age  = 65L
# )
# 
# names(tbls)
# #> [1] "patients"      "encounters"    "conditions"    "medications"
# #> [4] "procedures"    "observations"  "allergies"     "immunizations"


## ----explorar, eval=FALSE-----------------------------------------------
# # Demografía
# dplyr::glimpse(tbls$patients)
# 
# # Condiciones más frecuentes
# tbls$conditions |>
#   dplyr::count(code, description, sort = TRUE) |>
#   dplyr::slice_head(n = 10)
# 
# # Encuentros por paciente
# tbls$encounters |>
#   dplyr::count(patient_id, name = "n_visitas") |>
#   summary()


## ----diabetes_setup, message=FALSE, warning=FALSE-----------------------
library(rsynthea)
library(dplyr)
library(ggplot2)


## ----generar_cohort, cache=TRUE, message=FALSE--------------------------
tbls <- generate_population(
  n        = 200,
  seed     = 1L,
  end_date = as.POSIXct("2023-01-01"),
  min_age  = 30L,
  max_age  = 75L
)

cat(sprintf("Pacientes: %d  |  Encuentros: %d  |  Condiciones: %d  |  Medicaciones: %d\n",
    nrow(tbls$patients), nrow(tbls$encounters),
    nrow(tbls$conditions), nrow(tbls$medications)))


## ----prevalencia--------------------------------------------------------
prevalencia <- tbls$conditions |>
  filter(code %in% c("44054006", "714628002", "59621000", "195967001")) |>
  group_by(code, description) |>
  summarise(n_pacientes = n_distinct(patient_id), .groups = "drop") |>
  mutate(
    prevalencia = n_pacientes / nrow(tbls$patients) * 100,
    descripcion = case_when(
      code == "44054006" ~ "Diabetes tipo 2",
      code == "714628002" ~ "Prediabetes",
      code == "59621000" ~ "Hipertensión",
      code == "195967001" ~ "Asma",
      TRUE ~ description
    )
  )

ggplot(prevalencia, aes(x = reorder(descripcion, prevalencia), y = prevalencia)) +
  geom_col(fill = "#2196F3", alpha = 0.85) +
  geom_text(aes(label = sprintf("%.1f%%", prevalencia)), hjust = -0.1, size = 3.5) +
  coord_flip() +
  scale_y_continuous(limits = c(0, 70), labels = function(x) paste0(x, "%")) +
  labs(
    title    = "Prevalencia de condiciones crónicas",
    subtitle = "Cohorte sintética de 200 adultos (30-75 años)",
    x = NULL, y = "% pacientes"
  ) +
  theme_minimal(base_size = 12)


## ----comorbilidades-----------------------------------------------------
dm_ids <- tbls$conditions |>
  filter(code == "44054006") |>
  pull(patient_id) |>
  unique()

cat(sprintf("Pacientes con diabetes tipo 2: %d / %d (%.1f%%)\n",
    length(dm_ids), nrow(tbls$patients),
    100 * length(dm_ids) / nrow(tbls$patients)))

comorb_codes <- c(
  "59621000"  = "Hipertensión",
  "714628002" = "Prediabetes",
  "195967001" = "Asma"
)

comorb <- tbls$conditions |>
  filter(code %in% names(comorb_codes)) |>
  distinct(patient_id, code) |>
  mutate(
    es_diabetico = patient_id %in% dm_ids,
    condicion    = comorb_codes[code]
  ) |>
  group_by(condicion, es_diabetico) |>
  summarise(n = n(), .groups = "drop") |>
  mutate(
    total = ifelse(es_diabetico, length(dm_ids), nrow(tbls$patients) - length(dm_ids)),
    pct   = n / total * 100,
    grupo = ifelse(es_diabetico, "Diabético", "No diabético")
  )

ggplot(comorb, aes(x = condicion, y = pct, fill = grupo)) +
  geom_col(position = "dodge", alpha = 0.85) +
  geom_text(aes(label = sprintf("%.0f%%", pct)),
            position = position_dodge(0.9), vjust = -0.4, size = 3.2) +
  scale_fill_manual(values = c("Diabético" = "#E53935", "No diabético" = "#43A047")) +
  scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(0, 80)) +
  labs(
    title    = "Comorbilidades: pacientes diabéticos vs no diabéticos",
    subtitle = "Mayor carga de hipertensión y prediabetes en diabéticos",
    x = NULL, y = "% con la condición", fill = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "top")


## ----genero-------------------------------------------------------------
tbls$patients |>
  mutate(
    edad = as.numeric(as.POSIXct("2023-01-01") - birth_date, units = "days") / 365.25,
    grupo_edad = cut(edad, breaks = c(30, 45, 55, 65, 75),
                     labels = c("30-44", "45-54", "55-64", "65-75"))
  ) |>
  filter(!is.na(grupo_edad)) |>
  count(grupo_edad, gender) |>
  mutate(gender = ifelse(gender == "F", "Mujer", "Hombre")) |>
  ggplot(aes(x = grupo_edad, y = n, fill = gender)) +
  geom_col(position = "dodge", alpha = 0.85) +
  scale_fill_manual(values = c("Mujer" = "#E91E63", "Hombre" = "#1565C0")) +
  labs(
    title = "Distribución de la cohorte por edad y género",
    x = "Grupo de edad", y = "Nº pacientes", fill = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "top")


## ----longitudinal, fig.height=4.5---------------------------------------
ha1c <- tbls$observations |>
  filter(code == "4548-4") |>
  inner_join(
    tbls$conditions |> filter(code == "44054006") |> distinct(patient_id),
    by = "patient_id"
  ) |>
  mutate(
    anyo       = as.integer(format(time, "%Y")),
    ha1c_valor = as.numeric(value)
  ) |>
  filter(!is.na(ha1c_valor), ha1c_valor > 4, ha1c_valor < 15) |>
  group_by(anyo) |>
  summarise(
    media = mean(ha1c_valor),
    p25   = quantile(ha1c_valor, 0.25),
    p75   = quantile(ha1c_valor, 0.75),
    n     = n(),
    .groups = "drop"
  )

cat(sprintf("Observaciones HbA1c en diabéticos: %d  |  Años: %d–%d\n",
    sum(ha1c$n), min(ha1c$anyo), max(ha1c$anyo)))

ggplot(ha1c, aes(x = anyo)) +
  geom_ribbon(aes(ymin = p25, ymax = p75), alpha = 0.2, fill = "#E53935") +
  geom_line(aes(y = media), color = "#E53935", linewidth = 1) +
  geom_hline(yintercept = 6.5, linetype = "dashed", color = "grey40") +
  annotate("text", x = min(ha1c$anyo), y = 6.65,
           label = "Umbral diagnóstico (6.5%)", hjust = 0, size = 3) +
  scale_x_continuous(breaks = scales::breaks_pretty(n = 8)) +
  labs(
    title    = "HbA1c media en pacientes diabéticos a lo largo del tiempo",
    subtitle = "Banda: IQR (p25–p75)",
    x = NULL, y = "HbA1c (%)"
  ) +
  theme_minimal(base_size = 12)

