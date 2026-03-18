# ==============================================================================
# MODULO: Filtros Globales del Sidebar
# ==============================================================================

sidebarFiltersUI <- function(id) {
  ns <- NS(id)
  tagList(
    dateRangeInput(
      ns("date_range"), "Rango de Fechas",
      start = NULL, end = NULL,
      language = "es", separator = "a"
    ),
    pickerInput(
      ns("anio"), "Año de Suscripcion",
      choices = NULL, multiple = TRUE,
      options = pickerOptions(
        actionsBox = TRUE, liveSearch = FALSE,
        noneSelectedText = "Todos"
      )
    ),
    pickerInput(
      ns("estado"), "Estado",
      choices = NULL, multiple = TRUE,
      options = pickerOptions(
        actionsBox = TRUE, liveSearch = TRUE,
        noneSelectedText = "Todos"
      )
    ),
    pickerInput(
      ns("tipo_vehiculo"), "Tipo Vehiculo",
      choices = NULL, multiple = TRUE,
      options = pickerOptions(
        actionsBox = TRUE,
        noneSelectedText = "Todos"
      )
    ),
    pickerInput(
      ns("canal_venta"), "Canal de Venta",
      choices = NULL, multiple = TRUE,
      options = pickerOptions(
        actionsBox = TRUE,
        noneSelectedText = "Todos"
      )
    ),
    sliderInput(
      ns("edad_range"), "Edad Conductor",
      min = 18, max = 75, value = c(18, 75), step = 1
    ),
    hr(),
    htmlOutput(ns("filter_badge")),
    actionButton(ns("reset"), "Reiniciar Filtros",
                 class = "btn-outline-secondary btn-sm w-100 mt-2")
  )
}

sidebarFiltersServer <- function(id, all_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Inicializar opciones de filtros
    observe({
      choices <- get_filter_choices(all_data)
      updateDateRangeInput(session, "date_range",
                           start = choices$fecha_min,
                           end = choices$fecha_max)
      updatePickerInput(session, "anio", choices = as.character(choices$anios))
      updatePickerInput(session, "estado", choices = choices$estados)
      updatePickerInput(session, "tipo_vehiculo", choices = choices$tipos_vehiculo)
      updatePickerInput(session, "canal_venta", choices = choices$canales)
      updateSliderInput(session, "edad_range",
                        min = choices$edad_min, max = choices$edad_max,
                        value = c(choices$edad_min, choices$edad_max))
    }) |> bindEvent(all_data$polizas, once = TRUE)

    # Reset
    observeEvent(input$reset, {
      choices <- get_filter_choices(all_data)
      updateDateRangeInput(session, "date_range",
                           start = choices$fecha_min, end = choices$fecha_max)
      updatePickerInput(session, "anio", selected = character(0))
      updatePickerInput(session, "estado", selected = character(0))
      updatePickerInput(session, "tipo_vehiculo", selected = character(0))
      updatePickerInput(session, "canal_venta", selected = character(0))
      updateSliderInput(session, "edad_range",
                        value = c(choices$edad_min, choices$edad_max))
    })

    # Datos filtrados
    filtered <- reactive({
      filters <- list(
        date_range       = input$date_range,
        estado           = input$estado,
        tipo_vehiculo    = input$tipo_vehiculo,
        canal_venta      = input$canal_venta,
        edad_range       = input$edad_range,
        anio_suscripcion = input$anio
      )
      result <- filter_data(all_data, filters)
      # Mantener pagos
      result$pagos <- all_data$pagos
      if (nrow(result$siniestros) > 0) {
        result$pagos <- all_data$pagos %>%
          filter(siniestro_id %in% result$siniestros$siniestro_id)
      }
      result
    })

    # Badge
    output$filter_badge <- renderUI({
      n_filtered <- nrow(filtered()$polizas)
      n_total <- nrow(all_data$polizas)
      pct <- round(n_filtered / n_total * 100)
      tags$div(
        class = "text-muted small text-center",
        tags$strong(format(n_filtered, big.mark = ",")),
        " de ",
        format(n_total, big.mark = ","),
        " polizas ",
        tags$span(class = "badge bg-primary", paste0(pct, "%"))
      )
    })

    filtered
  })
}
