# ==============================================================================
# CAPA DE ACCESO A DATOS
# Carga, enriquecimiento y filtrado de datos
# ==============================================================================

#' Cargar datos desde SQLite
#' @param db_path ruta a la base de datos
#' @return list(polizas, siniestros) con fechas parseadas
load_data <- function(db_path = "data/siniestralidad.db") {
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  polizas <- DBI::dbGetQuery(con, "SELECT * FROM polizas") %>%
    tibble::as_tibble() %>%
    mutate(across(starts_with("fecha"), as.Date))

  siniestros <- DBI::dbGetQuery(con, "SELECT * FROM siniestros") %>%
    tibble::as_tibble() %>%
    mutate(across(starts_with("fecha"), as.Date))

  list(polizas = polizas, siniestros = siniestros)
}

#' Enriquecer polizas con columnas calculadas
#' @param polizas tibble de polizas
#' @return polizas con columnas adicionales para segmentacion
enrich_polizas <- function(polizas) {
  polizas %>%
    mutate(
      rango_edad = cut(
        edad_conductor,
        breaks = c(0, 25, 35, 45, 55, 100),
        labels = c("18-25", "26-35", "36-45", "46-55", "56+"),
        right = TRUE
      ),
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
      ),
      zona_riesgo = case_when(
        estado %in% c("Ciudad de Mexico", "Estado de Mexico") ~ "Zona Alta",
        estado %in% c("Jalisco", "Nuevo Leon") ~ "Zona Media",
        TRUE ~ "Zona Baja"
      )
    )
}

#' Aplicar filtros del sidebar a los datos
#' @param data list(polizas, siniestros)
#' @param filters list con valores de filtros del sidebar
#' @return list(polizas, siniestros) filtrados
filter_data <- function(data, filters) {
  polizas_f <- data$polizas

  # Filtro por rango de fechas
  if (!is.null(filters$date_range)) {
    polizas_f <- polizas_f %>%
      filter(
        fecha_inicio >= filters$date_range[1],
        fecha_inicio <= filters$date_range[2]
      )
  }

  # Filtro por estado
  if (!is.null(filters$estado) && length(filters$estado) > 0) {
    polizas_f <- polizas_f %>% filter(estado %in% filters$estado)
  }

  # Filtro por tipo de vehiculo
  if (!is.null(filters$tipo_vehiculo) && length(filters$tipo_vehiculo) > 0) {
    polizas_f <- polizas_f %>% filter(tipo_vehiculo %in% filters$tipo_vehiculo)
  }

  # Filtro por canal de venta
  if (!is.null(filters$canal_venta) && length(filters$canal_venta) > 0) {
    polizas_f <- polizas_f %>% filter(canal_venta %in% filters$canal_venta)
  }

  # Filtro por rango de edad
  if (!is.null(filters$edad_range)) {
    polizas_f <- polizas_f %>%
      filter(
        edad_conductor >= filters$edad_range[1],
        edad_conductor <= filters$edad_range[2]
      )
  }

  # Filtro por anio de suscripcion
  if (!is.null(filters$anio_suscripcion) && length(filters$anio_suscripcion) > 0) {
    polizas_f <- polizas_f %>%
      filter(anio_suscripcion %in% as.integer(filters$anio_suscripcion))
  }

  # Filtrar siniestros por polizas filtradas
  siniestros_f <- data$siniestros %>%
    filter(poliza_id %in% polizas_f$poliza_id)

  list(polizas = polizas_f, siniestros = siniestros_f)
}

#' Obtener opciones unicas para filtros del sidebar
#' @param data list(polizas, siniestros)
#' @return named list con valores unicos por dimension
get_filter_choices <- function(data) {
  p <- data$polizas
  if (nrow(p) == 0) {
    return(list(
      estados = character(0), tipos_vehiculo = character(0),
      canales = character(0), anios = integer(0),
      edad_min = 18L, edad_max = 75L,
      fecha_min = Sys.Date() - 365, fecha_max = Sys.Date()
    ))
  }
  list(
    estados = sort(unique(p$estado)),
    tipos_vehiculo = sort(unique(p$tipo_vehiculo)),
    canales = sort(unique(p$canal_venta)),
    anios = sort(unique(p$anio_suscripcion)),
    edad_min = min(p$edad_conductor, na.rm = TRUE),
    edad_max = max(p$edad_conductor, na.rm = TRUE),
    fecha_min = min(p$fecha_inicio, na.rm = TRUE),
    fecha_max = max(p$fecha_inicio, na.rm = TRUE)
  )
}
