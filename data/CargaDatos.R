# ==============================================================================
# CARGA DE DATOS A SQLITE
# Lee CSVs generados y crea base de datos con indices
# ==============================================================================

library(DBI)
library(RSQLite)
library(readr)

cat("=== Carga de datos a SQLite ===\n")

# Verificar archivos
archivos <- c(
  "data/processed/polizas_sinteticas.csv",
  "data/processed/siniestros_sinteticos.csv",
  "data/processed/pagos_desarrollo.csv"
)

for (f in archivos) {
  if (!file.exists(f)) stop(paste("Archivo no encontrado:", f))
}

# Leer CSVs
polizas <- read_csv(archivos[1], show_col_types = FALSE)
siniestros <- read_csv(archivos[2], show_col_types = FALSE)
pagos <- read_csv(archivos[3], show_col_types = FALSE)

cat(sprintf("  Polizas: %s registros\n", format(nrow(polizas), big.mark = ",")))
cat(sprintf("  Siniestros: %s registros\n", format(nrow(siniestros), big.mark = ",")))
cat(sprintf("  Pagos desarrollo: %s registros\n", format(nrow(pagos), big.mark = ",")))

# Conectar a SQLite
db_path <- "data/siniestralidad.db"
if (file.exists(db_path)) file.remove(db_path)
con <- dbConnect(SQLite(), db_path)

# Escribir tablas
dbWriteTable(con, "polizas", polizas, overwrite = TRUE)
dbWriteTable(con, "siniestros", siniestros, overwrite = TRUE)
dbWriteTable(con, "pagos_desarrollo", pagos, overwrite = TRUE)

# Crear indices
cat("\nCreando indices...\n")
dbExecute(con, "CREATE INDEX idx_polizas_anio ON polizas(anio_suscripcion)")
dbExecute(con, "CREATE INDEX idx_polizas_estado ON polizas(estado)")
dbExecute(con, "CREATE INDEX idx_polizas_status ON polizas(poliza_status)")
dbExecute(con, "CREATE INDEX idx_siniestros_poliza ON siniestros(poliza_id)")
dbExecute(con, "CREATE INDEX idx_siniestros_anio ON siniestros(anio_ocurrencia)")
dbExecute(con, "CREATE INDEX idx_siniestros_tipo ON siniestros(tipo_siniestro)")
dbExecute(con, "CREATE INDEX idx_pagos_siniestro ON pagos_desarrollo(siniestro_id)")
dbExecute(con, "CREATE INDEX idx_pagos_anio_dev ON pagos_desarrollo(anio_ocurrencia, anio_desarrollo)")

# Validacion
cat("\nValidacion:\n")
tablas <- dbListTables(con)
for (t in tablas) {
  n <- dbGetQuery(con, sprintf("SELECT COUNT(*) AS n FROM %s", t))$n
  cat(sprintf("  %s: %s registros\n", t, format(n, big.mark = ",")))
}

dbDisconnect(con)
cat(sprintf("\nBase de datos creada: %s\n", db_path))
cat("Carga completada exitosamente.\n")
