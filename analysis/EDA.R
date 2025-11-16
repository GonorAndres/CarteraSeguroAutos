# ==============================================================================
# ANÁLISIS EXPLORATORIO DE DATOS - SINIESTRALIDAD AUTO
# Proyecto: Dashboard Siniestralidad
# Autor: Andrés González Ortega
# Descripción: Análisis comprensivo de pólizas y siniestros
# ==============================================================================

# Librerías
library(tidyverse)
library(DBI)
library(RSQLite)
library(lubridate)
library(scales)

# Configuración de gráficos
theme_set(theme_minimal(base_size = 12))

# ==============================================================================
# 1. CARGA DE DATOS
# ==============================================================================

cat("Conectando a base de datos...\n")

# Conexión a SQLite
con <- dbConnect(SQLite(), "data/siniestralidad.db")

# Cargar tablas
polizas <- dbGetQuery(con, "SELECT * FROM polizas")
siniestros <- dbGetQuery(con, "SELECT * FROM siniestros")

# Convertir fechas
polizas <- polizas %>%
  mutate(across(starts_with("fecha"), as.Date))

siniestros <- siniestros %>%
  mutate(across(starts_with("fecha"), as.Date))

dbDisconnect(con)

cat(sprintf("Datos cargados: %d pólizas, %d siniestros\n",
            nrow(polizas), nrow(siniestros)))

# ==============================================================================
# 2. ESTADÍSTICAS DESCRIPTIVAS BÁSICAS
# ==============================================================================

cat("\n=== RESUMEN DE PÓLIZAS ===\n")

# Resumen pólizas
cat(sprintf("Total pólizas: %s\n", format(nrow(polizas), big.mark = ",")))
cat(sprintf("Suma asegurada total: $%s MXN\n",
            format(sum(polizas$suma_asegurada), big.mark = ",", scientific = FALSE)))
cat(sprintf("Prima neta total: $%s MXN\n",
            format(sum(polizas$prima_neta), big.mark = ",", scientific = FALSE)))
cat(sprintf("Edad conductor promedio: %.1f años\n", mean(polizas$edad_conductor)))
cat(sprintf("Score crediticio promedio: %.0f\n", mean(polizas$score_crediticio, na.rm = TRUE)))

cat("\n=== RESUMEN DE SINIESTROS ===\n")

# Resumen siniestros
cat(sprintf("Total siniestros: %s\n", format(nrow(siniestros), big.mark = ",")))
cat(sprintf("Monto total siniestros: $%s MXN\n",
            format(sum(siniestros$monto_siniestro), big.mark = ",", scientific = FALSE)))
cat(sprintf("Monto promedio: $%s MXN\n",
            format(mean(siniestros$monto_siniestro), big.mark = ",", scientific = FALSE)))
cat(sprintf("Severidad mediana: $%s MXN\n",
            format(median(siniestros$monto_siniestro), big.mark = ",", scientific = FALSE)))

# ==============================================================================
# 3. ANÁLISIS DE LOSS RATIO
# ==============================================================================

cat("\n=== ANÁLISIS LOSS RATIO ===\n")

# Unir datos para análisis
datos_completos <- polizas %>%
  left_join(siniestros, by = "poliza_id")

# Loss ratio global
prima_total <- sum(polizas$prima_neta)
siniestros_total <- sum(siniestros$monto_pagado)
loss_ratio_global <- siniestros_total / prima_total

cat(sprintf("Loss Ratio Global: %.2f%%\n", loss_ratio_global * 100))

# Loss ratio por canal de venta
lr_canal <- polizas %>%
  group_by(canal_venta) %>%
  summarise(
    n_polizas = n(),
    prima_total = sum(prima_neta),
    .groups = "drop"
  ) %>%
  left_join(
    siniestros %>%
      left_join(polizas %>% select(poliza_id, canal_venta), by = "poliza_id") %>%
      group_by(canal_venta) %>%
      summarise(siniestros_total = sum(monto_pagado), .groups = "drop"),
    by = "canal_venta"
  ) %>%
  mutate(
    siniestros_total = replace_na(siniestros_total, 0),
    loss_ratio = siniestros_total / prima_total
  ) %>%
  arrange(desc(loss_ratio))

cat("\nLoss Ratio por Canal de Venta:\n")
print(lr_canal)

# Visualización Loss Ratio por Canal
p1 <- ggplot(lr_canal, aes(x = reorder(canal_venta, loss_ratio), y = loss_ratio)) +
  geom_col(fill = "#2E86AB") +
  geom_hline(yintercept = loss_ratio_global, linetype = "dashed", color = "red") +
  scale_y_continuous(labels = percent_format()) +
  coord_flip() +
  labs(
    title = "Loss Ratio por Canal de Venta",
    subtitle = "Línea roja = Loss Ratio Global",
    x = "Canal",
    y = "Loss Ratio"
  )

print(p1)

# Loss ratio por tipo de vehículo
lr_vehiculo <- polizas %>%
  group_by(tipo_vehiculo) %>%
  summarise(
    n_polizas = n(),
    prima_total = sum(prima_neta),
    .groups = "drop"
  ) %>%
  left_join(
    siniestros %>%
      left_join(polizas %>% select(poliza_id, tipo_vehiculo), by = "poliza_id") %>%
      group_by(tipo_vehiculo) %>%
      summarise(siniestros_total = sum(monto_pagado), .groups = "drop"),
    by = "tipo_vehiculo"
  ) %>%
  mutate(
    siniestros_total = replace_na(siniestros_total, 0),
    loss_ratio = siniestros_total / prima_total
  ) %>%
  arrange(desc(loss_ratio))

cat("\nLoss Ratio por Tipo de Vehículo:\n")
print(lr_vehiculo)

# Visualización Loss Ratio por Tipo Vehículo
p2 <- ggplot(lr_vehiculo, aes(x = reorder(tipo_vehiculo, loss_ratio), y = loss_ratio)) +
  geom_col(fill = "#A23B72") +
  geom_hline(yintercept = loss_ratio_global, linetype = "dashed", color = "red") +
  scale_y_continuous(labels = percent_format()) +
  coord_flip() +
  labs(
    title = "Loss Ratio por Tipo de Vehículo",
    subtitle = "Línea roja = Loss Ratio Global",
    x = "Tipo de Vehículo",
    y = "Loss Ratio"
  )

print(p2)

# ==============================================================================
# 4. ANÁLISIS DE FRECUENCIA
# ==============================================================================

cat("\n=== ANÁLISIS DE FRECUENCIA ===\n")

# Frecuencia global
frecuencia_global <- nrow(siniestros) / nrow(polizas)
cat(sprintf("Frecuencia Global: %.2f%%\n", frecuencia_global * 100))

# Frecuencia por tipo de vehículo
freq_vehiculo <- polizas %>%
  group_by(tipo_vehiculo) %>%
  summarise(n_polizas = n(), .groups = "drop") %>%
  left_join(
    siniestros %>%
      left_join(polizas %>% select(poliza_id, tipo_vehiculo), by = "poliza_id") %>%
      group_by(tipo_vehiculo) %>%
      summarise(n_siniestros = n(), .groups = "drop"),
    by = "tipo_vehiculo"
  ) %>%
  mutate(
    n_siniestros = replace_na(n_siniestros, 0),
    frecuencia = n_siniestros / n_polizas
  ) %>%
  arrange(desc(frecuencia))

cat("\nFrecuencia por Tipo de Vehículo:\n")
print(freq_vehiculo)

# Frecuencia por género
freq_genero <- polizas %>%
  group_by(genero) %>%
  summarise(n_polizas = n(), .groups = "drop") %>%
  left_join(
    siniestros %>%
      left_join(polizas %>% select(poliza_id, genero), by = "poliza_id") %>%
      group_by(genero) %>%
      summarise(n_siniestros = n(), .groups = "drop"),
    by = "genero"
  ) %>%
  mutate(
    n_siniestros = replace_na(n_siniestros, 0),
    frecuencia = n_siniestros / n_polizas
  )

cat("\nFrecuencia por Género:\n")
print(freq_genero)

# Frecuencia por rango de edad
polizas_edad <- polizas %>%
  mutate(
    rango_edad = cut(edad_conductor,
                     breaks = c(0, 25, 35, 45, 55, 100),
                     labels = c("18-25", "26-35", "36-45", "46-55", "56+"))
  )

freq_edad <- polizas_edad %>%
  group_by(rango_edad) %>%
  summarise(n_polizas = n(), .groups = "drop") %>%
  left_join(
    siniestros %>%
      left_join(polizas_edad %>% select(poliza_id, rango_edad), by = "poliza_id") %>%
      group_by(rango_edad) %>%
      summarise(n_siniestros = n(), .groups = "drop"),
    by = "rango_edad"
  ) %>%
  mutate(
    n_siniestros = replace_na(n_siniestros, 0),
    frecuencia = n_siniestros / n_polizas
  )

cat("\nFrecuencia por Rango de Edad:\n")
print(freq_edad)

# Visualización frecuencia por edad
p3 <- ggplot(freq_edad, aes(x = rango_edad, y = frecuencia)) +
  geom_col(fill = "#F18F01") +
  geom_hline(yintercept = frecuencia_global, linetype = "dashed", color = "red") +
  scale_y_continuous(labels = percent_format()) +
  labs(
    title = "Frecuencia de Siniestros por Rango de Edad",
    subtitle = "Línea roja = Frecuencia Global",
    x = "Rango de Edad",
    y = "Frecuencia"
  )

print(p3)

# ==============================================================================
# 5. ANÁLISIS DE SEVERIDAD
# ==============================================================================

cat("\n=== ANÁLISIS DE SEVERIDAD ===\n")

# Severidad por tipo de siniestro
sev_tipo <- siniestros %>%
  group_by(tipo_siniestro) %>%
  summarise(
    n_siniestros = n(),
    severidad_promedio = mean(monto_siniestro),
    severidad_mediana = median(monto_siniestro),
    .groups = "drop"
  ) %>%
  arrange(desc(severidad_promedio))

cat("\nSeveridad por Tipo de Siniestro:\n")
print(sev_tipo)

# Visualización severidad por tipo
p4 <- ggplot(sev_tipo, aes(x = reorder(tipo_siniestro, severidad_promedio),
                            y = severidad_promedio)) +
  geom_col(fill = "#C73E1D") +
  scale_y_continuous(labels = dollar_format(prefix = "$", big.mark = ",")) +
  coord_flip() +
  labs(
    title = "Severidad Promedio por Tipo de Siniestro",
    x = "Tipo de Siniestro",
    y = "Severidad Promedio (MXN)"
  )

print(p4)

# Distribución de severidad
p5 <- ggplot(siniestros, aes(x = monto_siniestro)) +
  geom_histogram(bins = 50, fill = "#2E86AB", alpha = 0.7) +
  scale_x_continuous(labels = dollar_format(prefix = "$", big.mark = ",")) +
  labs(
    title = "Distribución de Severidad de Siniestros",
    x = "Monto del Siniestro (MXN)",
    y = "Frecuencia"
  )

print(p5)

# ==============================================================================
# 6. RESUMEN EJECUTIVO
# ==============================================================================

cat("\n\n")
cat("====================================================================\n")
cat("                    RESUMEN EJECUTIVO - KPIs                       \n")
cat("====================================================================\n\n")

cat(sprintf("Total Pólizas:           %s\n", format(nrow(polizas), big.mark = ",")))
cat(sprintf("Total Siniestros:        %s\n", format(nrow(siniestros), big.mark = ",")))
cat(sprintf("Loss Ratio Global:       %.2f%%\n", loss_ratio_global * 100))
cat(sprintf("Frecuencia Global:       %.2f%%\n", frecuencia_global * 100))
cat(sprintf("Severidad Promedio:      $%s MXN\n",
            format(mean(siniestros$monto_siniestro), big.mark = ",", digits = 2)))
cat(sprintf("Prima Total:             $%s MXN\n",
            format(prima_total, big.mark = ",", scientific = FALSE)))
cat(sprintf("Siniestros Pagados:      $%s MXN\n",
            format(siniestros_total, big.mark = ",", scientific = FALSE)))

cat("\n")
cat("Canal con peor Loss Ratio:     ", lr_canal$canal_venta[1],
    sprintf(" (%.2f%%)\n", lr_canal$loss_ratio[1] * 100))
cat("Vehículo con peor Loss Ratio:  ", lr_vehiculo$tipo_vehiculo[1],
    sprintf(" (%.2f%%)\n", lr_vehiculo$loss_ratio[1] * 100))
cat("Tipo siniestro más costoso:    ", sev_tipo$tipo_siniestro[1],
    sprintf(" ($%s)\n", format(sev_tipo$severidad_promedio[1], big.mark = ",")))

cat("\n====================================================================\n")
cat("Análisis completado exitosamente.\n")
cat("====================================================================\n")
