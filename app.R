# ==============================================================================
# DASHBOARD SINIESTRALIDAD - SEGUROS AUTO MÉXICO
# Proyecto: Dashboard Siniestralidad
# Autor: Andrés González Ortega
# Descripción: Dashboard interactivo para análisis de siniestralidad
# ==============================================================================

library(shiny)
library(shinydashboard)
library(tidyverse)
library(DBI)
library(RSQLite)
library(lubridate)
library(scales)
library(plotly)
library(DT)

# ==============================================================================
# FUNCIONES AUXILIARES
# ==============================================================================

# Cargar datos
load_data <- function() {
  con <- dbConnect(SQLite(), "data/siniestralidad.db")

  polizas <- dbGetQuery(con, "SELECT * FROM polizas") %>%
    mutate(across(starts_with("fecha"), as.Date))

  siniestros <- dbGetQuery(con, "SELECT * FROM siniestros") %>%
    mutate(across(starts_with("fecha"), as.Date))

  dbDisconnect(con)

  list(polizas = polizas, siniestros = siniestros)
}

# Calcular KPIs
calculate_kpis <- function(polizas, siniestros) {
  list(
    n_polizas = nrow(polizas),
    n_siniestros = nrow(siniestros),
    prima_total = sum(polizas$prima_neta),
    siniestros_total = sum(siniestros$monto_pagado),
    loss_ratio = sum(siniestros$monto_pagado) / sum(polizas$prima_neta),
    frecuencia = nrow(siniestros) / nrow(polizas),
    severidad = mean(siniestros$monto_siniestro)
  )
}

# ==============================================================================
# UI - INTERFAZ DE USUARIO
# ==============================================================================

ui <- dashboardPage(
  skin = "blue",

  # Header
  dashboardHeader(title = "Dashboard Siniestralidad"),

  # Sidebar
  dashboardSidebar(
    sidebarMenu(
      menuItem("Resumen Ejecutivo", tabName = "resumen", icon = icon("dashboard")),
      menuItem("Loss Ratio", tabName = "lossratio", icon = icon("chart-line")),
      menuItem("Frecuencia", tabName = "frecuencia", icon = icon("chart-bar")),
      menuItem("Severidad", tabName = "severidad", icon = icon("dollar-sign")),
      menuItem("Análisis Temporal", tabName = "temporal", icon = icon("calendar")),
      menuItem("Análisis Geográfico", tabName = "geografico", icon = icon("map")),
      menuItem("Segmentación", tabName = "segmentacion", icon = icon("users")),
      menuItem("Datos", tabName = "datos", icon = icon("table"))
    )
  ),

  # Body
  dashboardBody(
    tabItems(
      # ========================================================================
      # TAB 1: RESUMEN EJECUTIVO
      # ========================================================================
      tabItem(
        tabName = "resumen",
        h2("Resumen Ejecutivo - KPIs Principales"),

        fluidRow(
          valueBoxOutput("box_polizas"),
          valueBoxOutput("box_siniestros"),
          valueBoxOutput("box_lossratio")
        ),

        fluidRow(
          valueBoxOutput("box_frecuencia"),
          valueBoxOutput("box_severidad"),
          valueBoxOutput("box_prima")
        ),

        fluidRow(
          box(
            title = "Loss Ratio por Canal de Venta",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("plot_lr_canal", height = 300)
          ),
          box(
            title = "Distribución de Siniestros por Tipo",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("plot_dist_tipo", height = 300)
          )
        ),

        fluidRow(
          box(
            title = "Tendencia Mensual de Siniestros",
            status = "info",
            solidHeader = TRUE,
            width = 12,
            plotlyOutput("plot_tendencia_mes", height = 300)
          )
        )
      ),

      # ========================================================================
      # TAB 2: LOSS RATIO
      # ========================================================================
      tabItem(
        tabName = "lossratio",
        h2("Análisis de Loss Ratio"),

        fluidRow(
          box(
            title = "Loss Ratio por Canal de Venta",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("plot_lr_canal_detail", height = 350)
          ),
          box(
            title = "Loss Ratio por Tipo de Vehículo",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("plot_lr_vehiculo", height = 350)
          )
        ),

        fluidRow(
          box(
            title = "Loss Ratio por Marca",
            status = "warning",
            solidHeader = TRUE,
            width = 12,
            plotlyOutput("plot_lr_marca", height = 400)
          )
        ),

        fluidRow(
          box(
            title = "Tabla: Loss Ratio Detallado",
            status = "info",
            solidHeader = TRUE,
            width = 12,
            DTOutput("table_lr")
          )
        )
      ),

      # ========================================================================
      # TAB 3: FRECUENCIA
      # ========================================================================
      tabItem(
        tabName = "frecuencia",
        h2("Análisis de Frecuencia"),

        fluidRow(
          box(
            title = "Frecuencia por Tipo de Vehículo",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("plot_freq_vehiculo", height = 350)
          ),
          box(
            title = "Frecuencia por Rango de Edad",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("plot_freq_edad", height = 350)
          )
        ),

        fluidRow(
          box(
            title = "Frecuencia por Género",
            status = "warning",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("plot_freq_genero", height = 350)
          ),
          box(
            title = "Tabla: Frecuencia por Segmento",
            status = "info",
            solidHeader = TRUE,
            width = 6,
            DTOutput("table_frecuencia")
          )
        )
      ),

      # ========================================================================
      # TAB 4: SEVERIDAD
      # ========================================================================
      tabItem(
        tabName = "severidad",
        h2("Análisis de Severidad"),

        fluidRow(
          box(
            title = "Severidad por Tipo de Siniestro",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("plot_sev_tipo", height = 350)
          ),
          box(
            title = "Distribución de Severidad",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("plot_sev_dist", height = 350)
          )
        ),

        fluidRow(
          box(
            title = "Estadísticas de Severidad",
            status = "info",
            solidHeader = TRUE,
            width = 12,
            DTOutput("table_severidad")
          )
        )
      ),

      # ========================================================================
      # TAB 5: ANÁLISIS TEMPORAL
      # ========================================================================
      tabItem(
        tabName = "temporal",
        h2("Análisis Temporal"),

        fluidRow(
          box(
            title = "Siniestros por Mes",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("plot_sin_mes", height = 350)
          ),
          box(
            title = "Severidad por Mes",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("plot_sev_mes", height = 350)
          )
        ),

        fluidRow(
          box(
            title = "Composición por Tipo de Siniestro",
            status = "warning",
            solidHeader = TRUE,
            width = 12,
            plotlyOutput("plot_tipo_mes", height = 400)
          )
        )
      ),

      # ========================================================================
      # TAB 6: ANÁLISIS GEOGRÁFICO
      # ========================================================================
      tabItem(
        tabName = "geografico",
        h2("Análisis Geográfico"),

        fluidRow(
          box(
            title = "Loss Ratio por Estado",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("plot_lr_estado", height = 400)
          ),
          box(
            title = "Penetración de Mercado",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("plot_penetracion", height = 400)
          )
        ),

        fluidRow(
          box(
            title = "Frecuencia vs Severidad por Estado",
            status = "warning",
            solidHeader = TRUE,
            width = 12,
            plotlyOutput("plot_freq_sev_estado", height = 400)
          )
        )
      ),

      # ========================================================================
      # TAB 7: SEGMENTACIÓN
      # ========================================================================
      tabItem(
        tabName = "segmentacion",
        h2("Segmentación de Riesgo"),

        fluidRow(
          box(
            title = "Loss Ratio por Segmento de Edad",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("plot_lr_seg_edad", height = 350)
          ),
          box(
            title = "Frecuencia por Score Crediticio",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("plot_freq_score", height = 350)
          )
        ),

        fluidRow(
          box(
            title = "Matriz de Riesgo: Edad x Tipo Vehículo",
            status = "danger",
            solidHeader = TRUE,
            width = 12,
            plotlyOutput("plot_matriz_riesgo", height = 400)
          )
        )
      ),

      # ========================================================================
      # TAB 8: DATOS
      # ========================================================================
      tabItem(
        tabName = "datos",
        h2("Exploración de Datos"),

        fluidRow(
          box(
            title = "Pólizas",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            DTOutput("table_polizas")
          )
        ),

        fluidRow(
          box(
            title = "Siniestros",
            status = "warning",
            solidHeader = TRUE,
            width = 12,
            DTOutput("table_siniestros")
          )
        )
      )
    )
  )
)

# ==============================================================================
# SERVER - LÓGICA DEL SERVIDOR
# ==============================================================================

server <- function(input, output, session) {

  # Cargar datos reactivamente
  datos <- reactive({
    load_data()
  })

  # Calcular KPIs reactivamente
  kpis <- reactive({
    d <- datos()
    calculate_kpis(d$polizas, d$siniestros)
  })

  # ==========================================================================
  # VALUE BOXES - RESUMEN
  # ==========================================================================

  output$box_polizas <- renderValueBox({
    valueBox(
      format(kpis()$n_polizas, big.mark = ","),
      "Total Pólizas",
      icon = icon("file-contract"),
      color = "aqua"
    )
  })

  output$box_siniestros <- renderValueBox({
    valueBox(
      format(kpis()$n_siniestros, big.mark = ","),
      "Total Siniestros",
      icon = icon("car-crash"),
      color = "red"
    )
  })

  output$box_lossratio <- renderValueBox({
    valueBox(
      sprintf("%.2f%%", kpis()$loss_ratio * 100),
      "Loss Ratio",
      icon = icon("percent"),
      color = if (kpis()$loss_ratio > 0.75) "red" else "green"
    )
  })

  output$box_frecuencia <- renderValueBox({
    valueBox(
      sprintf("%.2f%%", kpis()$frecuencia * 100),
      "Frecuencia",
      icon = icon("chart-line"),
      color = "yellow"
    )
  })

  output$box_severidad <- renderValueBox({
    valueBox(
      paste0("$", format(round(kpis()$severidad), big.mark = ",")),
      "Severidad Promedio",
      icon = icon("dollar-sign"),
      color = "orange"
    )
  })

  output$box_prima <- renderValueBox({
    valueBox(
      paste0("$", format(round(kpis()$prima_total / 1e6, 1), big.mark = ","), "M"),
      "Prima Total (MXN)",
      icon = icon("money-bill-wave"),
      color = "green"
    )
  })

  # ==========================================================================
  # PLOTS - RESUMEN
  # ==========================================================================

  output$plot_lr_canal <- renderPlotly({
    d <- datos()

    lr_canal <- d$polizas %>%
      group_by(canal_venta) %>%
      summarise(prima_total = sum(prima_neta), .groups = "drop") %>%
      left_join(
        d$siniestros %>%
          left_join(d$polizas %>% select(poliza_id, canal_venta), by = "poliza_id") %>%
          group_by(canal_venta) %>%
          summarise(siniestros_total = sum(monto_pagado), .groups = "drop"),
        by = "canal_venta"
      ) %>%
      mutate(
        siniestros_total = replace_na(siniestros_total, 0),
        loss_ratio = siniestros_total / prima_total
      )

    plot_ly(lr_canal, x = ~canal_venta, y = ~loss_ratio, type = "bar",
            marker = list(color = "#2E86AB")) %>%
      layout(xaxis = list(title = "Canal"),
             yaxis = list(title = "Loss Ratio", tickformat = ".1%"))
  })

  output$plot_dist_tipo <- renderPlotly({
    d <- datos()

    dist_tipo <- d$siniestros %>%
      group_by(tipo_siniestro) %>%
      summarise(n = n(), .groups = "drop")

    plot_ly(dist_tipo, labels = ~tipo_siniestro, values = ~n, type = "pie",
            marker = list(colors = RColorBrewer::brewer.pal(5, "Set2"))) %>%
      layout(showlegend = TRUE)
  })

  output$plot_tendencia_mes <- renderPlotly({
    d <- datos()

    sin_mes <- d$siniestros %>%
      mutate(mes = month(fecha_siniestro, label = TRUE)) %>%
      group_by(mes) %>%
      summarise(n_siniestros = n(), .groups = "drop")

    plot_ly(sin_mes, x = ~mes, y = ~n_siniestros, type = "scatter", mode = "lines+markers",
            line = list(color = "#F18F01", width = 3),
            marker = list(size = 8, color = "#F18F01")) %>%
      layout(xaxis = list(title = "Mes"),
             yaxis = list(title = "Número de Siniestros"))
  })

  # ==========================================================================
  # PLOTS - LOSS RATIO
  # ==========================================================================

  output$plot_lr_canal_detail <- renderPlotly({
    d <- datos()

    lr_canal <- d$polizas %>%
      group_by(canal_venta) %>%
      summarise(prima_total = sum(prima_neta), .groups = "drop") %>%
      left_join(
        d$siniestros %>%
          left_join(d$polizas %>% select(poliza_id, canal_venta), by = "poliza_id") %>%
          group_by(canal_venta) %>%
          summarise(siniestros_total = sum(monto_pagado), .groups = "drop"),
        by = "canal_venta"
      ) %>%
      mutate(
        siniestros_total = replace_na(siniestros_total, 0),
        loss_ratio = siniestros_total / prima_total
      ) %>%
      arrange(desc(loss_ratio))

    plot_ly(lr_canal, y = ~reorder(canal_venta, loss_ratio), x = ~loss_ratio,
            type = "bar", orientation = "h",
            marker = list(color = "#2E86AB")) %>%
      layout(xaxis = list(title = "Loss Ratio", tickformat = ".1%"),
             yaxis = list(title = ""))
  })

  output$plot_lr_vehiculo <- renderPlotly({
    d <- datos()

    lr_veh <- d$polizas %>%
      group_by(tipo_vehiculo) %>%
      summarise(prima_total = sum(prima_neta), .groups = "drop") %>%
      left_join(
        d$siniestros %>%
          left_join(d$polizas %>% select(poliza_id, tipo_vehiculo), by = "poliza_id") %>%
          group_by(tipo_vehiculo) %>%
          summarise(siniestros_total = sum(monto_pagado), .groups = "drop"),
        by = "tipo_vehiculo"
      ) %>%
      mutate(
        siniestros_total = replace_na(siniestros_total, 0),
        loss_ratio = siniestros_total / prima_total
      ) %>%
      arrange(desc(loss_ratio))

    plot_ly(lr_veh, y = ~reorder(tipo_vehiculo, loss_ratio), x = ~loss_ratio,
            type = "bar", orientation = "h",
            marker = list(color = "#A23B72")) %>%
      layout(xaxis = list(title = "Loss Ratio", tickformat = ".1%"),
             yaxis = list(title = ""))
  })

  output$plot_lr_marca <- renderPlotly({
    d <- datos()

    lr_marca <- d$polizas %>%
      group_by(marca_vehiculo) %>%
      summarise(prima_total = sum(prima_neta), .groups = "drop") %>%
      left_join(
        d$siniestros %>%
          left_join(d$polizas %>% select(poliza_id, marca_vehiculo), by = "poliza_id") %>%
          group_by(marca_vehiculo) %>%
          summarise(siniestros_total = sum(monto_pagado), .groups = "drop"),
        by = "marca_vehiculo"
      ) %>%
      mutate(
        siniestros_total = replace_na(siniestros_total, 0),
        loss_ratio = siniestros_total / prima_total
      ) %>%
      arrange(desc(loss_ratio))

    plot_ly(lr_marca, y = ~reorder(marca_vehiculo, loss_ratio), x = ~loss_ratio,
            type = "bar", orientation = "h",
            marker = list(color = "#C73E1D")) %>%
      layout(xaxis = list(title = "Loss Ratio", tickformat = ".1%"),
             yaxis = list(title = ""))
  })

  # Continuará en la siguiente sección...

  output$table_lr <- renderDT({
    d <- datos()

    lr_canal <- d$polizas %>%
      group_by(canal_venta) %>%
      summarise(n_polizas = n(), prima_total = sum(prima_neta), .groups = "drop") %>%
      left_join(
        d$siniestros %>%
          left_join(d$polizas %>% select(poliza_id, canal_venta), by = "poliza_id") %>%
          group_by(canal_venta) %>%
          summarise(n_siniestros = n(), siniestros_total = sum(monto_pagado), .groups = "drop"),
        by = "canal_venta"
      ) %>%
      mutate(
        n_siniestros = replace_na(n_siniestros, 0),
        siniestros_total = replace_na(siniestros_total, 0),
        loss_ratio = siniestros_total / prima_total
      ) %>%
      arrange(desc(loss_ratio))

    datatable(lr_canal,
              options = list(pageLength = 10),
              colnames = c("Canal", "Pólizas", "Prima Total", "Siniestros", "Monto Pagado", "Loss Ratio")) %>%
      formatCurrency(c("prima_total", "siniestros_total"), currency = "$", digits = 0) %>%
      formatPercentage("loss_ratio", digits = 2)
  })

  # ==========================================================================
  # PLOTS - FRECUENCIA
  # ==========================================================================

  output$plot_freq_vehiculo <- renderPlotly({
    d <- datos()

    freq_veh <- d$polizas %>%
      group_by(tipo_vehiculo) %>%
      summarise(n_polizas = n(), .groups = "drop") %>%
      left_join(
        d$siniestros %>%
          left_join(d$polizas %>% select(poliza_id, tipo_vehiculo), by = "poliza_id") %>%
          group_by(tipo_vehiculo) %>%
          summarise(n_siniestros = n(), .groups = "drop"),
        by = "tipo_vehiculo"
      ) %>%
      mutate(
        n_siniestros = replace_na(n_siniestros, 0),
        frecuencia = n_siniestros / n_polizas
      )

    plot_ly(freq_veh, x = ~tipo_vehiculo, y = ~frecuencia, type = "bar",
            marker = list(color = "#F18F01")) %>%
      layout(xaxis = list(title = "Tipo de Vehículo"),
             yaxis = list(title = "Frecuencia", tickformat = ".1%"))
  })

  output$plot_freq_edad <- renderPlotly({
    d <- datos()

    polizas_edad <- d$polizas %>%
      mutate(rango_edad = cut(edad_conductor,
                              breaks = c(0, 25, 35, 45, 55, 100),
                              labels = c("18-25", "26-35", "36-45", "46-55", "56+")))

    freq_edad <- polizas_edad %>%
      group_by(rango_edad) %>%
      summarise(n_polizas = n(), .groups = "drop") %>%
      left_join(
        d$siniestros %>%
          left_join(polizas_edad %>% select(poliza_id, rango_edad), by = "poliza_id") %>%
          group_by(rango_edad) %>%
          summarise(n_siniestros = n(), .groups = "drop"),
        by = "rango_edad"
      ) %>%
      mutate(
        n_siniestros = replace_na(n_siniestros, 0),
        frecuencia = n_siniestros / n_polizas
      )

    plot_ly(freq_edad, x = ~rango_edad, y = ~frecuencia, type = "bar",
            marker = list(color = "#2E86AB")) %>%
      layout(xaxis = list(title = "Rango de Edad"),
             yaxis = list(title = "Frecuencia", tickformat = ".1%"))
  })

  output$plot_freq_genero <- renderPlotly({
    d <- datos()

    freq_gen <- d$polizas %>%
      group_by(genero) %>%
      summarise(n_polizas = n(), .groups = "drop") %>%
      left_join(
        d$siniestros %>%
          left_join(d$polizas %>% select(poliza_id, genero), by = "poliza_id") %>%
          group_by(genero) %>%
          summarise(n_siniestros = n(), .groups = "drop"),
        by = "genero"
      ) %>%
      mutate(
        n_siniestros = replace_na(n_siniestros, 0),
        frecuencia = n_siniestros / n_polizas
      )

    plot_ly(freq_gen, x = ~genero, y = ~frecuencia, type = "bar",
            marker = list(color = "#A23B72")) %>%
      layout(xaxis = list(title = "Género"),
             yaxis = list(title = "Frecuencia", tickformat = ".1%"))
  })

  output$table_frecuencia <- renderDT({
    d <- datos()

    polizas_edad <- d$polizas %>%
      mutate(rango_edad = cut(edad_conductor,
                              breaks = c(0, 25, 35, 45, 55, 100),
                              labels = c("18-25", "26-35", "36-45", "46-55", "56+")))

    freq_edad <- polizas_edad %>%
      group_by(rango_edad) %>%
      summarise(n_polizas = n(), .groups = "drop") %>%
      left_join(
        d$siniestros %>%
          left_join(polizas_edad %>% select(poliza_id, rango_edad), by = "poliza_id") %>%
          group_by(rango_edad) %>%
          summarise(n_siniestros = n(), .groups = "drop"),
        by = "rango_edad"
      ) %>%
      mutate(
        n_siniestros = replace_na(n_siniestros, 0),
        frecuencia = n_siniestros / n_polizas
      ) %>%
      arrange(desc(frecuencia))

    datatable(freq_edad,
              options = list(pageLength = 10),
              colnames = c("Rango Edad", "Pólizas", "Siniestros", "Frecuencia")) %>%
      formatPercentage("frecuencia", digits = 2)
  })

  # ==========================================================================
  # PLOTS - SEVERIDAD
  # ==========================================================================

  output$plot_sev_tipo <- renderPlotly({
    d <- datos()

    sev_tipo <- d$siniestros %>%
      group_by(tipo_siniestro) %>%
      summarise(severidad_promedio = mean(monto_siniestro), .groups = "drop") %>%
      arrange(desc(severidad_promedio))

    plot_ly(sev_tipo, y = ~reorder(tipo_siniestro, severidad_promedio),
            x = ~severidad_promedio, type = "bar", orientation = "h",
            marker = list(color = "#C73E1D")) %>%
      layout(xaxis = list(title = "Severidad Promedio (MXN)"),
             yaxis = list(title = ""))
  })

  output$plot_sev_dist <- renderPlotly({
    d <- datos()

    plot_ly(d$siniestros, x = ~monto_siniestro, type = "histogram",
            marker = list(color = "#2E86AB")) %>%
      layout(xaxis = list(title = "Monto del Siniestro (MXN)"),
             yaxis = list(title = "Frecuencia"))
  })

  output$table_severidad <- renderDT({
    d <- datos()

    sev_tipo <- d$siniestros %>%
      group_by(tipo_siniestro) %>%
      summarise(
        n_siniestros = n(),
        severidad_promedio = mean(monto_siniestro),
        severidad_mediana = median(monto_siniestro),
        severidad_max = max(monto_siniestro),
        .groups = "drop"
      ) %>%
      arrange(desc(severidad_promedio))

    datatable(sev_tipo,
              options = list(pageLength = 10),
              colnames = c("Tipo", "N Siniestros", "Promedio", "Mediana", "Máximo")) %>%
      formatCurrency(c("severidad_promedio", "severidad_mediana", "severidad_max"),
                     currency = "$", digits = 0)
  })

  # ==========================================================================
  # PLOTS - TEMPORAL
  # ==========================================================================

  output$plot_sin_mes <- renderPlotly({
    d <- datos()

    sin_mes <- d$siniestros %>%
      mutate(mes = month(fecha_siniestro, label = TRUE)) %>%
      group_by(mes) %>%
      summarise(n_siniestros = n(), .groups = "drop")

    plot_ly(sin_mes, x = ~mes, y = ~n_siniestros, type = "bar",
            marker = list(color = "#2E86AB")) %>%
      layout(xaxis = list(title = "Mes"),
             yaxis = list(title = "Número de Siniestros"))
  })

  output$plot_sev_mes <- renderPlotly({
    d <- datos()

    sev_mes <- d$siniestros %>%
      mutate(mes = month(fecha_siniestro, label = TRUE)) %>%
      group_by(mes) %>%
      summarise(severidad_promedio = mean(monto_siniestro), .groups = "drop")

    plot_ly(sev_mes, x = ~mes, y = ~severidad_promedio,
            type = "scatter", mode = "lines+markers",
            line = list(color = "#C73E1D", width = 3),
            marker = list(size = 8, color = "#C73E1D")) %>%
      layout(xaxis = list(title = "Mes"),
             yaxis = list(title = "Severidad Promedio (MXN)"))
  })

  output$plot_tipo_mes <- renderPlotly({
    d <- datos()

    tipo_mes <- d$siniestros %>%
      mutate(mes = month(fecha_siniestro, label = TRUE)) %>%
      group_by(mes, tipo_siniestro) %>%
      summarise(n_siniestros = n(), .groups = "drop")

    plot_ly(tipo_mes, x = ~mes, y = ~n_siniestros, color = ~tipo_siniestro,
            type = "bar") %>%
      layout(barmode = "stack",
             xaxis = list(title = "Mes"),
             yaxis = list(title = "Número de Siniestros"))
  })

  # ==========================================================================
  # PLOTS - GEOGRÁFICO
  # ==========================================================================

  output$plot_lr_estado <- renderPlotly({
    d <- datos()

    lr_estado <- d$polizas %>%
      group_by(estado) %>%
      summarise(prima_total = sum(prima_neta), .groups = "drop") %>%
      left_join(
        d$siniestros %>%
          left_join(d$polizas %>% select(poliza_id, estado), by = "poliza_id") %>%
          group_by(estado) %>%
          summarise(siniestros_total = sum(monto_pagado), .groups = "drop"),
        by = "estado"
      ) %>%
      mutate(
        siniestros_total = replace_na(siniestros_total, 0),
        loss_ratio = siniestros_total / prima_total
      ) %>%
      arrange(desc(loss_ratio))

    plot_ly(lr_estado, y = ~reorder(estado, loss_ratio), x = ~loss_ratio,
            type = "bar", orientation = "h",
            marker = list(color = "#2E86AB")) %>%
      layout(xaxis = list(title = "Loss Ratio", tickformat = ".1%"),
             yaxis = list(title = ""))
  })

  output$plot_penetracion <- renderPlotly({
    d <- datos()

    penetracion <- d$polizas %>%
      group_by(estado) %>%
      summarise(n_polizas = n(), .groups = "drop") %>%
      mutate(penetracion = n_polizas / sum(n_polizas)) %>%
      arrange(desc(penetracion))

    plot_ly(penetracion, y = ~reorder(estado, penetracion), x = ~penetracion,
            type = "bar", orientation = "h",
            marker = list(color = "#F18F01")) %>%
      layout(xaxis = list(title = "Penetración", tickformat = ".1%"),
             yaxis = list(title = ""))
  })

  output$plot_freq_sev_estado <- renderPlotly({
    d <- datos()

    freq_estado <- d$polizas %>%
      group_by(estado) %>%
      summarise(n_polizas = n(), .groups = "drop") %>%
      left_join(
        d$siniestros %>%
          left_join(d$polizas %>% select(poliza_id, estado), by = "poliza_id") %>%
          group_by(estado) %>%
          summarise(n_siniestros = n(), severidad = mean(monto_siniestro), .groups = "drop"),
        by = "estado"
      ) %>%
      mutate(
        n_siniestros = replace_na(n_siniestros, 0),
        frecuencia = n_siniestros / n_polizas
      )

    plot_ly(freq_estado, x = ~frecuencia, y = ~severidad,
            type = "scatter", mode = "markers+text",
            text = ~estado, textposition = "top center",
            marker = list(size = 12, color = "#A23B72")) %>%
      layout(xaxis = list(title = "Frecuencia", tickformat = ".1%"),
             yaxis = list(title = "Severidad Promedio (MXN)"))
  })

  # ==========================================================================
  # PLOTS - SEGMENTACIÓN
  # ==========================================================================

  output$plot_lr_seg_edad <- renderPlotly({
    d <- datos()

    polizas_seg <- d$polizas %>%
      mutate(segmento_edad = case_when(
        edad_conductor < 25 ~ "Joven (<25)",
        edad_conductor < 35 ~ "Adulto Joven (25-34)",
        edad_conductor < 50 ~ "Adulto (35-49)",
        TRUE ~ "Senior (50+)"
      ))

    lr_seg <- polizas_seg %>%
      group_by(segmento_edad) %>%
      summarise(prima_total = sum(prima_neta), .groups = "drop") %>%
      left_join(
        d$siniestros %>%
          left_join(polizas_seg %>% select(poliza_id, segmento_edad), by = "poliza_id") %>%
          group_by(segmento_edad) %>%
          summarise(siniestros_total = sum(monto_pagado), .groups = "drop"),
        by = "segmento_edad"
      ) %>%
      mutate(
        siniestros_total = replace_na(siniestros_total, 0),
        loss_ratio = siniestros_total / prima_total
      )

    plot_ly(lr_seg, x = ~segmento_edad, y = ~loss_ratio, type = "bar",
            marker = list(color = "#A23B72")) %>%
      layout(xaxis = list(title = "Segmento de Edad"),
             yaxis = list(title = "Loss Ratio", tickformat = ".1%"))
  })

  output$plot_freq_score <- renderPlotly({
    d <- datos()

    polizas_seg <- d$polizas %>%
      mutate(segmento_score = case_when(
        is.na(score_crediticio) ~ "Sin Score",
        score_crediticio < 550 ~ "Bajo (<550)",
        score_crediticio < 650 ~ "Medio (550-649)",
        TRUE ~ "Alto (650+)"
      ))

    freq_score <- polizas_seg %>%
      filter(segmento_score != "Sin Score") %>%
      group_by(segmento_score) %>%
      summarise(n_polizas = n(), .groups = "drop") %>%
      left_join(
        d$siniestros %>%
          left_join(polizas_seg %>% select(poliza_id, segmento_score), by = "poliza_id") %>%
          filter(segmento_score != "Sin Score") %>%
          group_by(segmento_score) %>%
          summarise(n_siniestros = n(), .groups = "drop"),
        by = "segmento_score"
      ) %>%
      mutate(
        n_siniestros = replace_na(n_siniestros, 0),
        frecuencia = n_siniestros / n_polizas
      )

    plot_ly(freq_score, x = ~segmento_score, y = ~frecuencia, type = "bar",
            marker = list(color = "#2E86AB")) %>%
      layout(xaxis = list(title = "Segmento Score Crediticio"),
             yaxis = list(title = "Frecuencia", tickformat = ".1%"))
  })

  output$plot_matriz_riesgo <- renderPlotly({
    d <- datos()

    polizas_seg <- d$polizas %>%
      mutate(segmento_edad = case_when(
        edad_conductor < 25 ~ "Joven (<25)",
        edad_conductor < 35 ~ "Adulto Joven (25-34)",
        edad_conductor < 50 ~ "Adulto (35-49)",
        TRUE ~ "Senior (50+)"
      ))

    matriz <- polizas_seg %>%
      group_by(segmento_edad, tipo_vehiculo) %>%
      summarise(prima_total = sum(prima_neta), .groups = "drop") %>%
      left_join(
        d$siniestros %>%
          left_join(polizas_seg %>% select(poliza_id, segmento_edad, tipo_vehiculo),
                    by = "poliza_id") %>%
          group_by(segmento_edad, tipo_vehiculo) %>%
          summarise(siniestros_total = sum(monto_pagado), .groups = "drop"),
        by = c("segmento_edad", "tipo_vehiculo")
      ) %>%
      mutate(
        siniestros_total = replace_na(siniestros_total, 0),
        loss_ratio = siniestros_total / prima_total
      )

    plot_ly(matriz, x = ~tipo_vehiculo, y = ~segmento_edad, z = ~loss_ratio,
            type = "heatmap", colors = colorRamp(c("green", "yellow", "red")),
            text = ~paste0(sprintf("%.1f%%", loss_ratio * 100)),
            texttemplate = "%{text}", textfont = list(size = 14)) %>%
      layout(xaxis = list(title = "Tipo de Vehículo"),
             yaxis = list(title = "Segmento de Edad"))
  })

  # ==========================================================================
  # TABLAS - DATOS
  # ==========================================================================

  output$table_polizas <- renderDT({
    d <- datos()
    datatable(head(d$polizas, 1000),
              options = list(pageLength = 25, scrollX = TRUE))
  })

  output$table_siniestros <- renderDT({
    d <- datos()
    datatable(head(d$siniestros, 1000),
              options = list(pageLength = 25, scrollX = TRUE))
  })
}

# ==============================================================================
# RUN APP
# ==============================================================================

shinyApp(ui = ui, server = server)
