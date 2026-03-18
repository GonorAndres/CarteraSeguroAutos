# ==============================================================================
# FUNCIONES DE EXPORTACION
# Handlers para descarga de datos en CSV y Excel
# ==============================================================================

#' Handler para descarga CSV
#' @param data reactive o data.frame a exportar
#' @param filename nombre del archivo
#' @return downloadHandler de shiny
download_csv_handler <- function(data, filename = "datos.csv") {
  shiny::downloadHandler(
    filename = function() {
      paste0(tools::file_path_sans_ext(filename), "_",
             format(Sys.Date(), "%Y%m%d"), ".csv")
    },
    content = function(file) {
      d <- if (is.reactive(data)) data() else data
      readr::write_csv(d, file)
    }
  )
}

#' Handler para descarga Excel
#' @param data reactive o data.frame a exportar
#' @param filename nombre del archivo
#' @return downloadHandler de shiny
download_excel_handler <- function(data, filename = "datos.xlsx") {
  shiny::downloadHandler(
    filename = function() {
      paste0(tools::file_path_sans_ext(filename), "_",
             format(Sys.Date(), "%Y%m%d"), ".xlsx")
    },
    content = function(file) {
      d <- if (is.reactive(data)) data() else data
      writexl::write_xlsx(d, file)
    }
  )
}

#' Handler para descarga de multiples hojas Excel
#' @param data_list named list de data.frames
#' @param filename nombre del archivo
#' @return downloadHandler de shiny
download_excel_multi_handler <- function(data_list, filename = "reporte.xlsx") {
  shiny::downloadHandler(
    filename = function() {
      paste0(tools::file_path_sans_ext(filename), "_",
             format(Sys.Date(), "%Y%m%d"), ".xlsx")
    },
    content = function(file) {
      sheets <- if (is.reactive(data_list)) data_list() else data_list
      writexl::write_xlsx(sheets, file)
    }
  )
}
