# ==============================================================================
# MODULO: Explorador de Datos
# ==============================================================================

datosUI <- function(id) {
  ns <- NS(id)
  tagList(
    h3("Explorador de Datos"),

    # --- Context banner ---
    card(
      class = "border-start border-primary border-4 mb-3",
      card_body(
        class = "py-2",
        layout_columns(
          col_widths = breakpoints(sm = 12, md = c(7, 5)),

          # Left: source & methodology
          tagList(
            tags$h5(class = "text-primary mb-2", icon("database"), " Datos Sinteticos - Mercado Mexicano"),
            tags$p(class = "mb-1",
              "Dataset sintetico generado con distribuciones estadisticas calibradas a parametros del mercado asegurador mexicano.",
              tags$strong("No contiene datos reales de asegurados.")
            ),
            tags$p(class = "mb-1 text-muted small",
              tags$strong("Fuentes de calibracion: "),
              "CONDUSEF (frecuencia de siniestralidad), AMIS (severidad promedio y loss ratio sectorial)."
            ),
            tags$p(class = "mb-0 text-muted small",
              tags$strong("Distribuciones: "),
              "Frecuencia ~ Poisson (lambda=0.085), Severidad ~ Gamma (shape=2, scale=8,000), ",
              "Inflacion 4% anual, Retencion 82% base, Missing data 6%."
            ),
            tags$p(class = "mb-0 text-muted small",
              tags$strong("Tipo siniestro: "),
              "Colision 65%, Danos 20%, Robo Parcial 10%, Robo Total 4%, Incendio 1%."
            )
          ),

          # Right: volume summary + KPIs
          tagList(
            tags$div(class = "bg-light rounded p-2",
              tags$h6(class = "mb-2", icon("chart-pie"), " Volumen y KPIs"),
              tags$table(class = "table table-sm table-borderless mb-0",
                tags$tbody(
                  tags$tr(tags$td(class = "text-muted", "Periodo:"), tags$td(tags$strong("2020 - 2024 (5 anos)"))),
                  tags$tr(tags$td(class = "text-muted", "Polizas:"), tags$td(tags$strong(format(nrow(APP_DATA$polizas), big.mark = ",")))),
                  tags$tr(tags$td(class = "text-muted", "Siniestros:"), tags$td(tags$strong(format(nrow(APP_DATA$siniestros), big.mark = ",")))),
                  tags$tr(tags$td(class = "text-muted", "Pagos desarrollo:"), tags$td(tags$strong(format(nrow(APP_DATA$pagos), big.mark = ",")))),
                  tags$tr(tags$td(class = "text-muted", "Estados:"), tags$td(paste(length(unique(APP_DATA$polizas$estado)), "entidades federativas"))),
                  tags$tr(tags$td(class = "text-muted", "Vehiculos:"), tags$td(paste(length(unique(APP_DATA$polizas$modelo_vehiculo)), "modelos,", length(unique(APP_DATA$polizas$marca_vehiculo)), "marcas"))),
                  tags$tr(tags$td(class = "text-muted", "Canales:"), tags$td(paste(sort(unique(APP_DATA$polizas$canal_venta)), collapse = "/")))
                )
              ),
              tags$hr(class = "my-1"),
              tags$div(class = "d-flex justify-content-between small", {
                kpis <- calc_kpis(APP_DATA$polizas, APP_DATA$siniestros)
                tagList(
                  tags$span("LR objetivo: ", tags$strong("75%"), paste0(" (logrado: ", format_pct(kpis$loss_ratio), ")")),
                  tags$span("Freq: ", tags$strong("8.5%"), paste0(" (", format_pct(kpis$frecuencia), ")")),
                  tags$span("Sev: ", tags$strong("$24K"), paste0(" (", format_currency_mxn(kpis$severidad_media), ")"))
                )
              })
            )
          )
        ),

        # Data dictionary (collapsible)
        tags$details(class = "mt-2",
          tags$summary(class = "text-muted small", style = "cursor:pointer;",
                       icon("book"), " Diccionario de Datos (click para expandir)"),
          tags$div(class = "mt-2 small",
            layout_columns(
              col_widths = breakpoints(sm = 12, md = c(4, 4, 4)),
              tags$div(
                tags$h6("Polizas"),
                tags$ul(class = "list-unstyled mb-0",
                  tags$li(tags$code("poliza_id"), " - ID unico"),
                  tags$li(tags$code("anio_suscripcion"), " - Ano de emision (2020-2024)"),
                  tags$li(tags$code("poliza_status"), " - Vigente/Renovada/No Renovada"),
                  tags$li(tags$code("edad_conductor"), " - Edad (18-75)"),
                  tags$li(tags$code("estado"), " - Entidad federativa"),
                  tags$li(tags$code("tipo_vehiculo"), " - Sedan/Hatchback/SUV"),
                  tags$li(tags$code("canal_venta"), " - Agente/Directo/Banco/Digital"),
                  tags$li(tags$code("prima_neta"), " - Prima cobrada (MXN)"),
                  tags$li(tags$code("suma_asegurada"), " - Valor asegurado (MXN)")
                )
              ),
              tags$div(
                tags$h6("Siniestros"),
                tags$ul(class = "list-unstyled mb-0",
                  tags$li(tags$code("siniestro_id"), " - ID unico del siniestro"),
                  tags$li(tags$code("poliza_id"), " - FK a poliza"),
                  tags$li(tags$code("tipo_siniestro"), " - Colision/Robo/Danos/Incendio"),
                  tags$li(tags$code("monto_siniestro"), " - Monto bruto (MXN)"),
                  tags$li(tags$code("deducible"), " - Deducible aplicado"),
                  tags$li(tags$code("monto_pagado"), " - Monto neto pagado"),
                  tags$li(tags$code("estado_siniestro"), " - Pagado/En proceso/Rechazado"),
                  tags$li(tags$code("monto_reserva"), " - Reserva (siniestros abiertos)")
                )
              ),
              tags$div(
                tags$h6("Pagos Desarrollo"),
                tags$ul(class = "list-unstyled mb-0",
                  tags$li(tags$code("anio_ocurrencia"), " - Ano del siniestro"),
                  tags$li(tags$code("anio_desarrollo"), " - Periodo de desarrollo (0-4)"),
                  tags$li(tags$code("monto_pago"), " - Pago incremental"),
                  tags$li(tags$code("monto_acumulado"), " - Pago acumulado"),
                  tags$li("Patron: 60%/85%/95%/99%/100%")
                )
              )
            )
          )
        )
      )
    ),

    navset_card_tab(
      nav_panel(
        "Polizas",
        layout_columns(
          col_widths = breakpoints(sm = 12, md = 6),
          downloadButton(ns("dl_polizas_csv"), "Descargar CSV", class = "btn-sm btn-outline-primary mt-2"),
          downloadButton(ns("dl_polizas_xlsx"), "Descargar Excel", class = "btn-sm btn-outline-success mt-2")
        ),
        DTOutput(ns("tabla_polizas"))
      ),
      nav_panel(
        "Siniestros",
        layout_columns(
          col_widths = breakpoints(sm = 12, md = 6),
          downloadButton(ns("dl_siniestros_csv"), "Descargar CSV", class = "btn-sm btn-outline-primary mt-2"),
          downloadButton(ns("dl_siniestros_xlsx"), "Descargar Excel", class = "btn-sm btn-outline-success mt-2")
        ),
        DTOutput(ns("tabla_siniestros"))
      ),
      nav_panel(
        "Pagos Desarrollo",
        DTOutput(ns("tabla_pagos"))
      )
    )
  )
}

datosServer <- function(id, filtered_data) {
  moduleServer(id, function(input, output, session) {

    output$tabla_polizas <- renderDT({
      d <- filtered_data()
      datatable(head(d$polizas, 5000),
                options = list(pageLength = 25, scrollX = TRUE),
                rownames = FALSE,
                caption = sprintf("Mostrando primeras 5,000 de %s polizas", format(nrow(d$polizas), big.mark = ","))) %>%
        formatCurrency(c("suma_asegurada", "prima_neta"), currency = "$", digits = 0)
    })

    output$tabla_siniestros <- renderDT({
      d <- filtered_data()
      datatable(d$siniestros,
                options = list(pageLength = 25, scrollX = TRUE),
                rownames = FALSE) %>%
        formatCurrency(c("monto_siniestro", "deducible", "monto_neto", "monto_pagado",
                         "monto_reserva"), currency = "$", digits = 0)
    })

    output$tabla_pagos <- renderDT({
      d <- filtered_data()
      if (is.null(d$pagos) || nrow(d$pagos) == 0) return(NULL)
      datatable(d$pagos,
                options = list(pageLength = 25, scrollX = TRUE),
                rownames = FALSE) %>%
        formatCurrency(c("monto_pago", "monto_acumulado"), currency = "$", digits = 0)
    })

    output$dl_polizas_csv <- download_csv_handler(
      reactive(filtered_data()$polizas), "polizas"
    )
    output$dl_polizas_xlsx <- download_excel_handler(
      reactive(filtered_data()$polizas), "polizas"
    )
    output$dl_siniestros_csv <- download_csv_handler(
      reactive(filtered_data()$siniestros), "siniestros"
    )
    output$dl_siniestros_xlsx <- download_excel_handler(
      reactive(filtered_data()$siniestros), "siniestros"
    )
  })
}
