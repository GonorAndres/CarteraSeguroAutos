# ==============================================================================
# ANÁLISIS GEOGRÁFICO Y SEGMENTACIÓN DE RIESGO
# Proyecto: Dashboard Siniestralidad
# Autor: Andrés González Ortega
# Descripción: Análisis de patrones geográficos y perfiles de riesgo
# ==============================================================================

# Librerías
library(tidyverse)
library(DBI)
library(RSQLite)
library(scales)

# Configuración
theme_set(theme_minimal(base_size = 12))

# ==============================================================================
# 1. CARGA DE DATOS
# ==============================================================================

cat("Cargando datos desde SQLite...\n")

con <- dbConnect(SQLite(), "data/siniestralidad.db")
polizas <- dbGetQuery(con, "SELECT * FROM polizas")
siniestros <- dbGetQuery(con, "SELECT * FROM siniestros")
dbDisconnect(con)

cat(sprintf("Datos cargados: %d pólizas, %d siniestros\n\n",
            nrow(polizas), nrow(siniestros)))

# ==============================================================================
# 2. ANÁLISIS GEOGRÁFICO POR ESTADO
# ==============================================================================

cat("=== ANÁLISIS GEOGRÁFICO ===\n\n")

# Métricas por estado
geo_estado <- polizas %>%
  group_by(estado) %>%
  summarise(
    n_polizas = n(),
    prima_total = sum(prima_neta),
    suma_asegurada_total = sum(suma_asegurada),
    .groups = "drop"
  ) %>%
  left_join(
    siniestros %>%
      left_join(polizas %>% select(poliza_id, estado), by = "poliza_id") %>%
      group_by(estado) %>%
      summarise(
        n_siniestros = n(),
        monto_siniestros = sum(monto_pagado),
        .groups = "drop"
      ),
    by = "estado"
  ) %>%
  mutate(
    n_siniestros = replace_na(n_siniestros, 0),
    monto_siniestros = replace_na(monto_siniestros, 0),
    frecuencia = n_siniestros / n_polizas,
    loss_ratio = monto_siniestros / prima_total,
    penetracion = n_polizas / sum(n_polizas)
  ) %>%
  arrange(desc(loss_ratio))

cat("Top 10 Estados por Loss Ratio:\n")
print(head(geo_estado, 10))

# Visualización Loss Ratio por estado
p1 <- ggplot(geo_estado, aes(x = reorder(estado, loss_ratio), y = loss_ratio)) +
  geom_col(fill = "#2E86AB") +
  geom_hline(yintercept = mean(geo_estado$loss_ratio),
             linetype = "dashed", color = "red") +
  scale_y_continuous(labels = percent_format()) +
  coord_flip() +
  labs(
    title = "Loss Ratio por Estado",
    subtitle = "Línea roja = Promedio Nacional",
    x = "Estado",
    y = "Loss Ratio"
  )

print(p1)

# Frecuencia vs Severidad por estado
sev_estado <- siniestros %>%
  left_join(polizas %>% select(poliza_id, estado), by = "poliza_id") %>%
  group_by(estado) %>%
  summarise(severidad_promedio = mean(monto_siniestro), .groups = "drop")

geo_completo <- geo_estado %>%
  left_join(sev_estado, by = "estado")

p2 <- ggplot(geo_completo, aes(x = frecuencia, y = severidad_promedio)) +
  geom_point(aes(size = n_polizas, color = loss_ratio), alpha = 0.7) +
  geom_text(aes(label = estado), vjust = -1, size = 3) +
  scale_x_continuous(labels = percent_format()) +
  scale_y_continuous(labels = dollar_format(prefix = "$", big.mark = ",")) +
  scale_color_gradient(low = "green", high = "red", labels = percent_format()) +
  scale_size_continuous(labels = comma_format()) +
  labs(
    title = "Mapa de Frecuencia vs Severidad por Estado",
    subtitle = "Tamaño = Número de pólizas, Color = Loss Ratio",
    x = "Frecuencia",
    y = "Severidad Promedio (MXN)",
    color = "Loss Ratio",
    size = "Pólizas"
  )

print(p2)

# ==============================================================================
# 3. ANÁLISIS DE PENETRACIÓN DE MERCADO
# ==============================================================================

cat("\n=== PENETRACIÓN DE MERCADO POR ESTADO ===\n\n")

penetracion <- geo_estado %>%
  select(estado, n_polizas, penetracion) %>%
  arrange(desc(penetracion))

cat("Top 5 Estados por Penetración:\n")
print(head(penetracion, 5))

# Visualización penetración
p3 <- ggplot(geo_estado, aes(x = reorder(estado, penetracion), y = penetracion)) +
  geom_col(fill = "#F18F01") +
  scale_y_continuous(labels = percent_format()) +
  coord_flip() +
  labs(
    title = "Penetración de Mercado por Estado",
    subtitle = "Porcentaje del total de pólizas",
    x = "Estado",
    y = "Penetración"
  )

print(p3)

# ==============================================================================
# 4. SEGMENTACIÓN POR MARCA Y MODELO
# ==============================================================================

cat("\n=== ANÁLISIS POR MARCA DE VEHÍCULO ===\n\n")

# Performance por marca
perf_marca <- polizas %>%
  group_by(marca_vehiculo) %>%
  summarise(
    n_polizas = n(),
    prima_total = sum(prima_neta),
    .groups = "drop"
  ) %>%
  left_join(
    siniestros %>%
      left_join(polizas %>% select(poliza_id, marca_vehiculo), by = "poliza_id") %>%
      group_by(marca_vehiculo) %>%
      summarise(
        n_siniestros = n(),
        monto_siniestros = sum(monto_pagado),
        severidad_promedio = mean(monto_siniestro),
        .groups = "drop"
      ),
    by = "marca_vehiculo"
  ) %>%
  mutate(
    n_siniestros = replace_na(n_siniestros, 0),
    monto_siniestros = replace_na(monto_siniestros, 0),
    frecuencia = n_siniestros / n_polizas,
    loss_ratio = monto_siniestros / prima_total
  ) %>%
  arrange(desc(loss_ratio))

cat("Ranking de Marcas por Loss Ratio:\n")
print(perf_marca)

# Visualización marcas
p4 <- ggplot(perf_marca, aes(x = reorder(marca_vehiculo, loss_ratio), y = loss_ratio)) +
  geom_col(fill = "#C73E1D") +
  scale_y_continuous(labels = percent_format()) +
  coord_flip() +
  labs(
    title = "Loss Ratio por Marca de Vehículo",
    x = "Marca",
    y = "Loss Ratio"
  )

print(p4)

# ==============================================================================
# 5. SEGMENTACIÓN POR PERFIL DE CONDUCTOR
# ==============================================================================

cat("\n=== SEGMENTACIÓN POR PERFIL DE CONDUCTOR ===\n\n")

# Crear segmentos de edad
polizas_segmentadas <- polizas %>%
  mutate(
    segmento_edad = case_when(
      edad_conductor < 25 ~ "Joven (<25)",
      edad_conductor < 35 ~ "Adulto Joven (25-34)",
      edad_conductor < 50 ~ "Adulto (35-49)",
      TRUE ~ "Senior (50+)"
    ),
    segmento_score = case_when(
      is.na(score_crediticio) ~ "Sin Score",
      score_crediticio < 550 ~ "Bajo (<550)",
      score_crediticio < 650 ~ "Medio (550-649)",
      TRUE ~ "Alto (650+)"
    )
  )

# Performance por segmento de edad
perf_edad <- polizas_segmentadas %>%
  group_by(segmento_edad) %>%
  summarise(
    n_polizas = n(),
    prima_total = sum(prima_neta),
    .groups = "drop"
  ) %>%
  left_join(
    siniestros %>%
      left_join(polizas_segmentadas %>% select(poliza_id, segmento_edad),
                by = "poliza_id") %>%
      group_by(segmento_edad) %>%
      summarise(
        n_siniestros = n(),
        monto_siniestros = sum(monto_pagado),
        .groups = "drop"
      ),
    by = "segmento_edad"
  ) %>%
  mutate(
    n_siniestros = replace_na(n_siniestros, 0),
    monto_siniestros = replace_na(monto_siniestros, 0),
    frecuencia = n_siniestros / n_polizas,
    loss_ratio = monto_siniestros / prima_total
  )

cat("Performance por Segmento de Edad:\n")
print(perf_edad)

# Performance por score crediticio
perf_score <- polizas_segmentadas %>%
  group_by(segmento_score) %>%
  summarise(
    n_polizas = n(),
    prima_total = sum(prima_neta),
    .groups = "drop"
  ) %>%
  left_join(
    siniestros %>%
      left_join(polizas_segmentadas %>% select(poliza_id, segmento_score),
                by = "poliza_id") %>%
      group_by(segmento_score) %>%
      summarise(
        n_siniestros = n(),
        monto_siniestros = sum(monto_pagado),
        .groups = "drop"
      ),
    by = "segmento_score"
  ) %>%
  mutate(
    n_siniestros = replace_na(n_siniestros, 0),
    monto_siniestros = replace_na(monto_siniestros, 0),
    frecuencia = n_siniestros / n_polizas,
    loss_ratio = monto_siniestros / prima_total
  )

cat("\nPerformance por Score Crediticio:\n")
print(perf_score)

# Visualización segmentos
p5 <- ggplot(perf_edad, aes(x = segmento_edad, y = loss_ratio)) +
  geom_col(fill = "#A23B72") +
  scale_y_continuous(labels = percent_format()) +
  labs(
    title = "Loss Ratio por Segmento de Edad",
    x = "Segmento",
    y = "Loss Ratio"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(p5)

p6 <- ggplot(perf_score %>% filter(segmento_score != "Sin Score"),
             aes(x = segmento_score, y = frecuencia)) +
  geom_col(fill = "#2E86AB") +
  scale_y_continuous(labels = percent_format()) +
  labs(
    title = "Frecuencia por Score Crediticio",
    x = "Segmento Score",
    y = "Frecuencia"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(p6)

# ==============================================================================
# 6. MATRIZ DE RIESGO
# ==============================================================================

cat("\n=== MATRIZ DE RIESGO ===\n\n")

# Crear matriz combinando múltiples factores
matriz_riesgo <- polizas_segmentadas %>%
  group_by(segmento_edad, tipo_vehiculo) %>%
  summarise(
    n_polizas = n(),
    prima_total = sum(prima_neta),
    .groups = "drop"
  ) %>%
  left_join(
    siniestros %>%
      left_join(polizas_segmentadas %>%
                  select(poliza_id, segmento_edad, tipo_vehiculo),
                by = "poliza_id") %>%
      group_by(segmento_edad, tipo_vehiculo) %>%
      summarise(
        n_siniestros = n(),
        monto_siniestros = sum(monto_pagado),
        .groups = "drop"
      ),
    by = c("segmento_edad", "tipo_vehiculo")
  ) %>%
  mutate(
    n_siniestros = replace_na(n_siniestros, 0),
    monto_siniestros = replace_na(monto_siniestros, 0),
    frecuencia = n_siniestros / n_polizas,
    loss_ratio = monto_siniestros / prima_total
  ) %>%
  arrange(desc(loss_ratio))

cat("Top 10 Combinaciones de Mayor Riesgo (Edad x Tipo Vehículo):\n")
print(head(matriz_riesgo, 10))

# Heatmap de loss ratio
p7 <- ggplot(matriz_riesgo, aes(x = tipo_vehiculo, y = segmento_edad,
                                 fill = loss_ratio)) +
  geom_tile(color = "white") +
  geom_text(aes(label = sprintf("%.1f%%", loss_ratio * 100)), color = "white") +
  scale_fill_gradient2(low = "green", mid = "yellow", high = "red",
                       midpoint = 0.75, labels = percent_format()) +
  labs(
    title = "Matriz de Riesgo: Loss Ratio por Edad y Tipo de Vehículo",
    x = "Tipo de Vehículo",
    y = "Segmento de Edad",
    fill = "Loss Ratio"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(p7)

# ==============================================================================
# 7. RESUMEN DE SEGMENTACIÓN
# ==============================================================================

cat("\n")
cat("====================================================================\n")
cat("              RESUMEN DE ANÁLISIS GEOGRÁFICO Y RIESGO             \n")
cat("====================================================================\n\n")

# Estados
estado_peor <- geo_estado[1, ]
estado_mejor <- geo_estado[nrow(geo_estado), ]

cat("ANÁLISIS GEOGRÁFICO:\n")
cat(sprintf("  Estado con peor LR:      %s (%.2f%%)\n",
            estado_peor$estado, estado_peor$loss_ratio * 100))
cat(sprintf("  Estado con mejor LR:     %s (%.2f%%)\n",
            estado_mejor$estado, estado_mejor$loss_ratio * 100))
cat(sprintf("  Estado más grande:       %s (%s pólizas)\n",
            penetracion$estado[1], format(penetracion$n_polizas[1], big.mark = ",")))

# Marcas
marca_peor <- perf_marca[1, ]
cat("\nSEGMENTACIÓN VEHÍCULOS:\n")
cat(sprintf("  Marca con peor LR:       %s (%.2f%%)\n",
            marca_peor$marca_vehiculo, marca_peor$loss_ratio * 100))

# Perfiles
perfil_peor <- perf_edad %>% filter(loss_ratio == max(loss_ratio))
cat("\nPERFILES DE CONDUCTOR:\n")
cat(sprintf("  Segmento con peor LR:    %s (%.2f%%)\n",
            perfil_peor$segmento_edad, perfil_peor$loss_ratio * 100))

# Riesgo
combo_peor <- matriz_riesgo[1, ]
cat("\nCOMBINACIÓN MÁS RIESGOSA:\n")
cat(sprintf("  %s + %s: LR = %.2f%%\n",
            combo_peor$segmento_edad, combo_peor$tipo_vehiculo,
            combo_peor$loss_ratio * 100))

cat("\n====================================================================\n")
cat("Análisis geográfico y de riesgo completado.\n")
cat("====================================================================\n")
