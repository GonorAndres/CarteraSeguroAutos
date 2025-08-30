# ==============================================================================
# GENERACIÓN DE DATOS SINTÉTICOS - SEGUROS AUTO MÉXICO
# Proyecto: Dashboard Siniestralidad 
# Autor: Andrés González Ortega
# Fecha: Agosto 2025
# ==============================================================================

# Configuración inicial
library(tidyverse)
library(lubridate)
library(DBI)
library(RMySQL)

set.seed(1971)

# Parámetros calibrados con mercado mexicano
params_mercado <- list(
  n_policies = 52000,
  lambda_freq = 0.08,          # 8% siniestralidad real CONDUSEF
  avg_severity = 24000,        # $24k promedio real AMIS
  target_loss_ratio = 0.75,    # 75% objetivo sectorial
  missing_rate = 0.06
)

# Configuración MySQL
db_config <- list(
  host = "localhost",
  port = 3306,
  user = " root",
  password = "Andres77",               # Tu password aquí
  dbname = "siniestralidad_db"
)

print("Configuración cargada")
print(params_mercado)

# Funciones generación datos - Autocontenidas

# Catálogos integrados
estados_mexico <- data.frame(
  estado = c("Estado de México", "Jalisco", "Ciudad de México", "Nuevo León", 
             "Sonora", "Chihuahua", "Michoacán", "Guanajuato", "Puebla", 
             "Veracruz", "Baja California", "Tamaulipas", "Coahuila"),
  cp_base = c(50000, 44000, 01000, 64000, 83000, 31000, 58000, 36000, 
              72000, 91000, 21000, 87000, 25000),
  concentracion = c(0.12, 0.08, 0.06, 0.05, 0.04, 0.04, 0.04, 0.04, 
                    0.04, 0.04, 0.04, 0.04, 0.37)
)

vehiculos_catalogo <- data.frame(
  marca = c("Nissan", "Volkswagen", "Chevrolet", "Toyota", "Ford", "Honda", 
            "Hyundai", "Mazda", "Kia", "Seat", "Suzuki", "Renault"),
  tipo = c("Sedan", "Sedan", "SUV", "Sedan", "SUV", "Sedan", 
           "Sedan", "SUV", "SUV", "Sedan", "Hatchback", "Sedan"),
  valor_promedio = c(280000, 320000, 350000, 300000, 380000, 290000,
                     250000, 330000, 270000, 310000, 240000, 260000),
  participacion_mercado = c(0.18, 0.15, 0.12, 0.10, 0.08, 0.07,
                            0.06, 0.05, 0.05, 0.04, 0.05, 0.05)
)

# Función demografía (ya funciona)
generar_conductores <- function(n) {
  edad <- round(rnorm(n, mean = 35, sd = 8))
  edad <- pmax(pmin(edad, 75), 18)
  
  experiencia <- pmax(0, edad - 18 + rnorm(n, 0, 2))
  experiencia <- round(pmin(experiencia, edad - 18))
  
  score <- round(rnorm(n, mean = 650, sd = 60))
  score <- pmax(pmin(score, 754), 413)
  
  data.frame(
    edad_conductor = edad,
    experiencia_anos = experiencia,
    score_crediticio = score,
    genero = sample(c("M", "F"), n, prob = c(0.52, 0.48), replace = TRUE)
  )
}

# Función vehículos y ubicación (corregida)
generar_vehiculos_ubicacion <- function(n) {
  # Estados según concentración
  estados_sel <- sample(estados_mexico$estado, n, 
                        prob = estados_mexico$concentracion, replace = TRUE)
  
  # CPs realistas
  cps <- sapply(estados_sel, function(estado) {
    cp_base <- estados_mexico$cp_base[estados_mexico$estado == estado]
    sprintf("%05d", cp_base + sample(0:9999, 1))
  })
  
  # Vehículos según mercado
  vehiculos_idx <- sample(1:nrow(vehiculos_catalogo), n,
                          prob = vehiculos_catalogo$participacion_mercado, 
                          replace = TRUE)
  
  # Años (parque promedio 8 años)
  año_actual <- year(today())
  años <- año_actual - rpois(n, lambda = 8)
  años <- pmax(años, año_actual - 20)
  
  data.frame(
    estado = estados_sel,
    codigo_postal = cps,
    marca_vehiculo = vehiculos_catalogo$marca[vehiculos_idx],
    tipo_vehiculo = vehiculos_catalogo$tipo[vehiculos_idx],
    año_vehiculo = años,
    valor_comercial = vehiculos_catalogo$valor_promedio[vehiculos_idx] * 
      exp(-0.12 * (año_actual - años)),
    canal_venta = sample(c("Agente", "Directo", "Banco", "Digital"), n,
                         prob = c(0.45, 0.25, 0.20, 0.10), replace = TRUE)
  )
}

# Test
demo_test <- generar_conductores(100)
vehiculos_test <- generar_vehiculos_ubicacion(100)

cat("Demografía:\n")
summary(demo_test)

cat("\nVehículos:\n")  
summary(vehiculos_test)

# Función fechas y exposición
generar_fechas_exposicion <- function(n) {
  fecha_inicio <- today() - days(sample(1:365, n, replace = TRUE))
  fecha_vencimiento <- fecha_inicio + years(1)
  exposicion_dias <- as.numeric(pmin(today() - fecha_inicio, 365))
  
  data.frame(
    fecha_inicio = fecha_inicio,
    fecha_vencimiento = fecha_vencimiento,
    exposicion = exposicion_dias / 365
  )
}

# Función cálculo prima
calcular_prima <- function(freq_esperada, sev_esperada, suma_aseg) {
  prima_pura <- freq_esperada * sev_esperada
  recargos <- 0.35
  prima_comercial <- prima_pura * (1 + recargos)
  
  # Ajuste por suma asegurada
  factor_suma <- log(suma_aseg / 250000) * 0.1 + 1
  prima_comercial * factor_suma
}

# Generar dataset completo de pólizas
generar_dataset_polizas <- function(n_policies) {
  # Combinar todas las características
  polizas <- data.frame(poliza_id = 1:n_policies) %>%
    bind_cols(generar_conductores(n_policies)) %>%
    bind_cols(generar_vehiculos_ubicacion(n_policies)) %>%
    bind_cols(generar_fechas_exposicion(n_policies)) %>%
    mutate(
      suma_asegurada = round(valor_comercial * runif(n_policies, 0.8, 1.2)),
      
      # Factores de riesgo para tarificación
      factor_edad = case_when(
        edad_conductor < 25 ~ 1.4,
        edad_conductor < 35 ~ 1.1,
        edad_conductor < 50 ~ 1.0,
        TRUE ~ 1.2
      ),
      
      factor_vehiculo = case_when(
        tipo_vehiculo == "SUV" ~ 1.15,
        tipo_vehiculo == "Sedan" ~ 1.0,
        TRUE ~ 1.0
      ),
      
      factor_zona = case_when(
        estado %in% c("Ciudad de México", "Estado de México") ~ 1.3,
        estado %in% c("Jalisco", "Nuevo León") ~ 1.1,
        TRUE ~ 1.0
      ),
      
      # Frecuencia y severidad esperadas
      freq_esperada = params_mercado$lambda_freq * factor_edad * 
        factor_vehiculo * factor_zona * exposicion,
      
      sev_esperada = params_mercado$avg_severity * 
        (1 + (suma_asegurada - 250000) / 1000000) *
        case_when(
          tipo_vehiculo == "SUV" ~ 1.25,
          tipo_vehiculo == "Sedan" ~ 1.0,
          TRUE ~ 0.95
        ),
      
      # Prima técnica
      prima_neta = calcular_prima(freq_esperada, sev_esperada, suma_asegurada)
    ) %>%
    select(-factor_edad, -factor_vehiculo, -factor_zona, 
           -freq_esperada, -sev_esperada, -valor_comercial)
  
  return(polizas)
}

# Generar dataset principal del proyecto
dataset_polizas <- generar_dataset_polizas(params_mercado$n_policies)

# Validación rápida
glimpse(dataset_polizas)
summary(dataset_polizas)
sum(duplicated(dataset_polizas$poliza_id))
sum(is.na(dataset_polizas))

# Generar siniestros basados en pólizas
generar_siniestros <- function(polizas_df) {
  # Número de siniestros por póliza (Poisson)
  siniestros_por_poliza <- rpois(nrow(polizas_df), 
                                 polizas_df$freq_esperada)
  
  # Crear dataset de siniestros
  siniestros_list <- map2_dfr(polizas_df$poliza_id, siniestros_por_poliza, 
                              function(poliza_id, n_siniestros) {
                                if (n_siniestros == 0) return(NULL)
                                
                                poliza_info <- polizas_df[polizas_df$poliza_id == poliza_id, ]
                                
                                tibble(
                                  poliza_id = poliza_id,
                                  fecha_siniestro = poliza_info$fecha_inicio + 
                                    days(sample(1:as.numeric(poliza_info$exposicion * 365), 
                                                n_siniestros, replace = TRUE)),
                                  tipo_siniestro = sample(c("Colisión", "Robo Total", "Robo Parcial", 
                                                            "Daños", "Incendio"), 
                                                          n_siniestros, 
                                                          prob = c(0.75, 0.05, 0.05, 0.10, 0.05), 
                                                          replace = TRUE)
                                ) %>%
                                  mutate(
                                    # Severidad con distribución Gamma ajustada
                                    monto_base = rgamma(n_siniestros, 
                                                        shape = 2.0, 
                                                        scale = 9000),
                                    
                                    # Ajuste por tipo de siniestro
                                    monto_siniestro = case_when(
                                      tipo_siniestro == "Robo Total" ~ poliza_info$suma_asegurada * 
                                        runif(n_siniestros, 0.85, 1.0),
                                      tipo_siniestro == "Colisión" ~ monto_base * 
                                        runif(n_siniestros, 0.7, 1.5),
                                      tipo_siniestro == "Incendio" ~ poliza_info$suma_asegurada * 
                                        runif(n_siniestros, 0.70, 0.95),
                                      TRUE ~ monto_base
                                    ),
                                    
                                    # Fechas y estados
                                    fecha_reporte = fecha_siniestro + days(sample(0:15, n_siniestros, 
                                                                                  replace = TRUE)),
                                    estado_siniestro = sample(c("Pagado", "En proceso", "Rechazado"), 
                                                              n_siniestros, 
                                                              prob = c(0.85, 0.12, 0.03), 
                                                              replace = TRUE),
                                    
                                    # Deducibles y pagos
                                    deducible = case_when(
                                      tipo_siniestro == "Robo Total" ~ 0,
                                      TRUE ~ pmax(monto_siniestro * 0.1, 1500)
                                    ),
                                    
                                    monto_pagado = case_when(
                                      estado_siniestro == "Pagado" ~ pmax(0, monto_siniestro - deducible),
                                      TRUE ~ 0
                                    ),
                                    
                                    # Estacionalidad
                                    mes_siniestro = month(fecha_siniestro),
                                    factor_estacional = case_when(
                                      mes_siniestro %in% 6:10 ~ 1.3,
                                      mes_siniestro %in% c(1,2) ~ 0.8,
                                      TRUE ~ 1.0
                                    )
                                  ) %>%
                                  mutate(
                                    monto_siniestro = monto_siniestro * 
                                      rnorm(n_siniestros, factor_estacional, 0.1),
                                    monto_pagado = case_when(
                                      estado_siniestro == "Pagado" ~ pmax(0, monto_siniestro - deducible),
                                      TRUE ~ 0
                                    )
                                  ) %>%
                                  select(-monto_base, -factor_estacional)
                              })
  
  # Asignar IDs secuenciales
  if(nrow(siniestros_list) > 0) {
    siniestros_list$siniestro_id <- 1:nrow(siniestros_list)
  }
  
  return(siniestros_list)
}

# Calcular frecuencia esperada para dataset_polizas
dataset_polizas <- dataset_polizas %>%
  mutate(
    factor_edad = case_when(
      edad_conductor < 25 ~ 1.4,
      edad_conductor < 35 ~ 1.1, 
      edad_conductor < 50 ~ 1.0,
      TRUE ~ 1.2
    ),
    factor_vehiculo = case_when(
      tipo_vehiculo == "SUV" ~ 1.15,
      tipo_vehiculo == "Sedan" ~ 1.0,
      TRUE ~ 0.95
    ),
    factor_zona = case_when(
      estado %in% c("Ciudad de México", "Estado de México") ~ 1.3,
      estado %in% c("Jalisco", "Nuevo León") ~ 1.1,
      TRUE ~ 0.9
    ),
    freq_esperada = params_mercado$lambda_freq * factor_edad * 
      factor_vehiculo * factor_zona * exposicion
  )

# Generar siniestros
dataset_siniestros <- generar_siniestros(dataset_polizas)

# Introducir missing values realistas
introducir_missing <- function(polizas_df) {
  polizas_df %>%
    mutate(
      score_crediticio = ifelse(runif(n()) < params_mercado$missing_rate, 
                                NA, score_crediticio),
      experiencia_anos = ifelse(edad_conductor < 25 & runif(n()) < 0.08, 
                                NA, experiencia_anos)
    )
}

# Validar KPIs
validar_kpis <- function(polizas_df, siniestros_df) {
  loss_ratio <- sum(siniestros_df$monto_pagado, na.rm = TRUE) / 
    sum(polizas_df$prima_neta, na.rm = TRUE)
  
  freq_promedio <- nrow(siniestros_df) / sum(polizas_df$exposicion, na.rm = TRUE)
  
  sev_promedio <- mean(siniestros_df$monto_siniestro, na.rm = TRUE)
  
  list(
    loss_ratio = loss_ratio,
    frecuencia = freq_promedio,
    severidad = sev_promedio,
    n_polizas = nrow(polizas_df),
    n_siniestros = nrow(siniestros_df),
    target_achieved = loss_ratio >= 0.65 & loss_ratio <= 0.85
  )
}

# Preparar datos finales
dataset_polizas_final <- introducir_missing(dataset_polizas) %>%
  select(-factor_edad, -factor_vehiculo, -factor_zona, -freq_esperada)

dataset_siniestros_final <- dataset_siniestros %>%
  select(siniestro_id, poliza_id, fecha_siniestro, fecha_reporte,
         monto_siniestro, tipo_siniestro, estado_siniestro, 
         deducible, monto_pagado, mes_siniestro)

# Validación final
kpis_validacion <- validar_kpis(dataset_polizas_final, dataset_siniestros_final)

# Crear directorios
dir.create("data", showWarnings = FALSE)
dir.create("data/processed", showWarnings = FALSE)

# Exportar como CSV
write_csv(dataset_polizas_final, "data/processed/polizas_sinteticas.csv")
write_csv(dataset_siniestros_final, "data/processed/siniestros_sinteticos.csv")

# Guardar resumen
resumen_generacion <- list(
  fecha_generacion = today(),
  parametros = params_mercado,
  kpis = kpis_validacion
)

jsonlite::write_json(resumen_generacion, "data/processed/resumen_generacion.json", 
                     pretty = TRUE, auto_unbox = TRUE)