# ==============================================================================
# MODULO: Explorador de Datos
# ==============================================================================

datosUI <- function(id) {
  ns <- NS(id)
  tagList(
    navset_card_tab(
      nav_panel(
        "Polizas",
        layout_columns(
          col_widths = c(6, 6),
          downloadButton(ns("dl_polizas_csv"), "Descargar CSV", class = "btn-sm btn-outline-primary mt-2"),
          downloadButton(ns("dl_polizas_xlsx"), "Descargar Excel", class = "btn-sm btn-outline-success mt-2")
        ),
        DTOutput(ns("tabla_polizas"))
      ),
      nav_panel(
        "Siniestros",
        layout_columns(
          col_widths = c(6, 6),
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
      datatable(d$polizas,
                options = list(pageLength = 25, scrollX = TRUE, scrollY = "500px"),
                rownames = FALSE, filter = "top") %>%
        formatCurrency(c("suma_asegurada", "prima_neta"), currency = "$", digits = 0)
    })

    output$tabla_siniestros <- renderDT({
      d <- filtered_data()
      datatable(d$siniestros,
                options = list(pageLength = 25, scrollX = TRUE, scrollY = "500px"),
                rownames = FALSE, filter = "top") %>%
        formatCurrency(c("monto_siniestro", "deducible", "monto_neto", "monto_pagado",
                         "monto_reserva"), currency = "$", digits = 0)
    })

    output$tabla_pagos <- renderDT({
      d <- filtered_data()
      if (is.null(d$pagos) || nrow(d$pagos) == 0) return(NULL)
      datatable(d$pagos,
                options = list(pageLength = 25, scrollX = TRUE, scrollY = "500px"),
                rownames = FALSE, filter = "top") %>%
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
