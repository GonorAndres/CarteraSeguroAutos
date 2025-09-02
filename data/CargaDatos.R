# ==============================================================================
# CARGA DE DATOS SINTÉTICOS - SEGUROS AUTO MÉXICO
# Proyecto: Dashboard Siniestralidad 
# Autor: Andrés González Ortega
# Fecha: 02 Septiembre 2025
# ==============================================================================

# Verificar archivos CSV
file.exists("data/processed/polizas_sinteticas.csv")
file.exists("data/processed/siniestros_sinteticos.csv")


library(DBI)
library(RSQLite)
library(readr)
library(dplyr)

# Cargar datos CSV
polizas <- read_csv("data/processed/polizas_sinteticas.csv")
siniestros <- read_csv("data/processed/siniestros_sinteticos.csv")

# Crear conexión SQLite
con <- dbConnect(SQLite(), "data/siniestralidad.db")

# Cargar tablas
dbWriteTable(con, "polizas", polizas, overwrite = TRUE)
dbWriteTable(con, "siniestros", siniestros, overwrite = TRUE)

# Verificar carga
print(paste("Tablas creadas:", paste(dbListTables(con), collapse = ", ")))
print(paste("Polizas en BD:", dbGetQuery(con, "SELECT COUNT(*) as count FROM polizas")$count))
print(paste("Siniestros en BD:", dbGetQuery(con, "SELECT COUNT(*) as count FROM siniestros")$count))

# Cerrar conexión
dbDisconnect(con)
cat("CARGA COMPLETADA! Base de datos creada en: data/siniestralidad.db\n")
