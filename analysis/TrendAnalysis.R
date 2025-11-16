# ==============================================================================
# ANÁLISIS DE TENDENCIAS TEMPORALES - SINIESTRALIDAD AUTO
# Proyecto: Dashboard Siniestralidad
# Autor: Andrés González Ortega
# Descripción: Análisis de patrones temporales en frecuencia y severidad
# ==============================================================================

# Librerías
library(tidyverse)
library(DBI)
library(RSQLite)
library(lubridate)
library(scales)

# Configuración
theme_set(theme_minimal(base_size = 12))

# ==============================================================================
# 1. CARGA DE DATOS
# ==============================================================================

cat("Cargando datos desde SQLite...\n")

con <- dbConnect(SQLite(), "data/siniestralidad.db")
polizas <- dbGetQuery(con, "SELECT * FROM polizas") %>%
  mutate(across(starts_with("fecha"), as.Date))
siniestros <- dbGetQuery(con, "SELECT * FROM siniestros") %>%
  mutate(across(starts_with("fecha"), as.Date))
dbDisconnect(con)

cat(sprintf("Datos cargados: %d pólizas, %d siniestros\n\n",
            nrow(polizas), nrow(siniestros)))

# ==============================================================================
# 2. ANÁLISIS MENSUAL DE SINIESTROS
# ==============================================================================

cat("=== ANÁLISIS TEMPORAL MENSUAL ===\n\n")

# Siniestros por mes
siniestros_mes <- siniestros %>%
  mutate(
    mes = month(fecha_siniestro, label = TRUE, abbr = FALSE),
    mes_num = month(fecha_siniestro)
  ) %>%
  group_by(mes, mes_num) %>%
  summarise(
    n_siniestros = n(),
    monto_total = sum(monto_siniestro),
    severidad_promedio = mean(monto_siniestro),
    .groups = "drop"
  ) %>%
  arrange(mes_num)

cat("Siniestros por Mes:\n")
print(siniestros_mes)

# Visualización siniestros por mes
p1 <- ggplot(siniestros_mes, aes(x = mes, y = n_siniestros)) +
  geom_col(fill = "#2E86AB") +
  geom_text(aes(label = n_siniestros), vjust = -0.5, size = 3) +
  labs(
    title = "Distribución de Siniestros por Mes",
    subtitle = "Análisis de estacionalidad",
    x = "Mes",
    y = "Número de Siniestros"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(p1)

# Severidad por mes
p2 <- ggplot(siniestros_mes, aes(x = mes, y = severidad_promedio)) +
  geom_line(group = 1, color = "#C73E1D", size = 1) +
  geom_point(color = "#C73E1D", size = 3) +
  scale_y_continuous(labels = dollar_format(prefix = "$", big.mark = ",")) +
  labs(
    title = "Severidad Promedio por Mes",
    subtitle = "Variación mensual del costo promedio",
    x = "Mes",
    y = "Severidad Promedio (MXN)"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(p2)

# ==============================================================================
# 3. ANÁLISIS DE FRECUENCIA POR PERÍODO
# ==============================================================================

cat("\n=== ANÁLISIS DE FRECUENCIA POR PERÍODO ===\n\n")

# Crear exposición mensual de pólizas
polizas_mes <- polizas %>%
  mutate(
    mes_inicio = floor_date(fecha_inicio, "month"),
    mes_fin = floor_date(fecha_vencimiento, "month")
  )

# Calcular frecuencia mensual (aproximada)
meses <- unique(siniestros_mes$mes_num)
frecuencia_mes <- siniestros_mes %>%
  mutate(
    # Aproximación: asumimos exposición constante
    polizas_expuestas = nrow(polizas) / 12,
    frecuencia = n_siniestros / polizas_expuestas
  )

cat("Frecuencia Mensual (aproximada):\n")
print(frecuencia_mes %>% select(mes, n_siniestros, frecuencia))

# Visualización frecuencia mensual
p3 <- ggplot(frecuencia_mes, aes(x = mes, y = frecuencia)) +
  geom_col(fill = "#F18F01") +
  scale_y_continuous(labels = percent_format()) +
  labs(
    title = "Frecuencia de Siniestros por Mes",
    subtitle = "Porcentaje de pólizas con siniestros",
    x = "Mes",
    y = "Frecuencia"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(p3)

# ==============================================================================
# 4. ANÁLISIS POR TIPO DE SINIESTRO EN EL TIEMPO
# ==============================================================================

cat("\n=== TENDENCIAS POR TIPO DE SINIESTRO ===\n\n")

# Siniestros por tipo y mes
tipo_mes <- siniestros %>%
  mutate(mes = month(fecha_siniestro, label = TRUE)) %>%
  group_by(mes, tipo_siniestro) %>%
  summarise(n_siniestros = n(), .groups = "drop")

cat("Top 5 combinaciones Mes-Tipo:\n")
print(head(tipo_mes %>% arrange(desc(n_siniestros)), 10))

# Visualización stack por tipo
p4 <- ggplot(tipo_mes, aes(x = mes, y = n_siniestros, fill = tipo_siniestro)) +
  geom_col(position = "stack") +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "Composición de Siniestros por Mes",
    subtitle = "Distribución por tipo de siniestro",
    x = "Mes",
    y = "Número de Siniestros",
    fill = "Tipo de Siniestro"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(p4)

# ==============================================================================
# 5. TIEMPO DE REPORTE
# ==============================================================================

cat("\n=== ANÁLISIS DE TIEMPO DE REPORTE ===\n\n")

# Calcular días entre ocurrencia y reporte
siniestros_reporte <- siniestros %>%
  mutate(dias_reporte = as.numeric(fecha_reporte - fecha_siniestro))

cat(sprintf("Tiempo promedio de reporte: %.1f días\n",
            mean(siniestros_reporte$dias_reporte)))
cat(sprintf("Tiempo mediano de reporte: %.0f días\n",
            median(siniestros_reporte$dias_reporte)))

# Distribución tiempo de reporte
p5 <- ggplot(siniestros_reporte, aes(x = dias_reporte)) +
  geom_histogram(bins = 30, fill = "#A23B72", alpha = 0.7) +
  labs(
    title = "Distribución del Tiempo de Reporte",
    subtitle = "Días entre ocurrencia y reporte del siniestro",
    x = "Días de Reporte",
    y = "Frecuencia"
  )

print(p5)

# Tiempo de reporte por tipo de siniestro
reporte_tipo <- siniestros_reporte %>%
  group_by(tipo_siniestro) %>%
  summarise(
    dias_promedio = mean(dias_reporte),
    dias_mediana = median(dias_reporte),
    .groups = "drop"
  ) %>%
  arrange(desc(dias_promedio))

cat("\nTiempo de Reporte por Tipo:\n")
print(reporte_tipo)

# ==============================================================================
# 6. ESTADO DE SINIESTROS
# ==============================================================================

cat("\n=== ESTADO DE SINIESTROS ===\n\n")

# Distribución por estado
estado_dist <- siniestros %>%
  group_by(estado_siniestro) %>%
  summarise(
    n_siniestros = n(),
    monto_total = sum(monto_siniestro),
    porcentaje = n() / nrow(siniestros)
  ) %>%
  arrange(desc(n_siniestros))

cat("Distribución por Estado del Siniestro:\n")
print(estado_dist)

# Visualización estado
p6 <- ggplot(estado_dist, aes(x = "", y = porcentaje, fill = estado_siniestro)) +
  geom_col(width = 1) +
  coord_polar("y", start = 0) +
  scale_fill_brewer(palette = "Set1") +
  scale_y_continuous(labels = percent_format()) +
  labs(
    title = "Distribución por Estado del Siniestro",
    fill = "Estado"
  ) +
  theme_void() +
  theme(legend.position = "right")

print(p6)

# ==============================================================================
# 7. RESUMEN DE TENDENCIAS
# ==============================================================================

cat("\n")
cat("====================================================================\n")
cat("                    RESUMEN DE TENDENCIAS                          \n")
cat("====================================================================\n\n")

mes_max <- siniestros_mes %>% filter(n_siniestros == max(n_siniestros))
mes_min <- siniestros_mes %>% filter(n_siniestros == min(n_siniestros))
mes_max_sev <- siniestros_mes %>% filter(severidad_promedio == max(severidad_promedio))

cat(sprintf("Mes con más siniestros:        %s (%d siniestros)\n",
            mes_max$mes, mes_max$n_siniestros))
cat(sprintf("Mes con menos siniestros:      %s (%d siniestros)\n",
            mes_min$mes, mes_min$n_siniestros))
cat(sprintf("Mes con mayor severidad:       %s ($%s)\n",
            mes_max_sev$mes,
            format(mes_max_sev$severidad_promedio, big.mark = ",")))
cat(sprintf("Tiempo promedio de reporte:    %.1f días\n",
            mean(siniestros_reporte$dias_reporte)))
cat(sprintf("Tasa de pago:                  %.1f%%\n",
            (estado_dist %>% filter(estado_siniestro == "Paid") %>%
               pull(porcentaje)) * 100))

cat("\n====================================================================\n")
cat("Análisis de tendencias completado.\n")
cat("====================================================================\n")
