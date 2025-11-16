# CarteraSeguroAutos

Sistema de generación de datos sintéticos para análisis de siniestralidad en seguros de autos para el mercado mexicano.

## Descripción

Proyecto de ciencia de datos enfocado en crear un dataset sintético realista de pólizas y siniestros de seguros automotrices. Los datos están calibrados con parámetros del mercado mexicano (CONDUSEF y AMIS) para análisis actuarial y desarrollo de dashboards de siniestralidad.

## Estructura del Proyecto

```
CarteraSeguroAutos/
├── data/
│   ├── GeneracionDatos.R          # Script de generación de datos sintéticos
│   ├── CargaDatos.R               # Carga de datos a SQLite
│   ├── siniestralidad.db          # Base de datos SQLite (5MB)
│   └── processed/
│       ├── polizas_sinteticas.csv     # 52,000 pólizas
│       ├── siniestros_sinteticos.csv  # 2,332 siniestros
│       └── resumen_generacion.json    # Metadata y KPIs
├── LICENSE                         # Licencia MIT
└── README.md
```

## Instalación y Configuración

### Requisitos

- R >= 4.0
- RStudio (recomendado)

### Configuración de Entorno

**Este proyecto usa `renv` para gestión reproducible de dependencias.**

Para configuración completa del entorno, ver [SETUP.md](SETUP.md) que incluye:
- Instalación de renv
- Comando renv::restore() para instalar dependencias
- Uso diario y mejores prácticas

### Paquetes Requeridos (instalación manual alternativa)

```r
install.packages(c(
  "tidyverse",
  "lubridate",
  "DBI",
  "RSQLite",
  "readr",
  "dplyr",
  "jsonlite"
))
```

### Uso

1. Clonar el repositorio:
```bash
git clone https://github.com/GonorAndres/CarteraSeguroAutos.git
cd CarteraSeguroAutos
```

2. Abrir el proyecto en RStudio (archivo .Rproj)

3. Ejecutar scripts en orden:
```r
# Generar nuevos datos sintéticos
source("data/GeneracionDatos.R")

# Cargar datos a SQLite
source("data/CargaDatos.R")
```

## Diccionario de Datos

### Tabla: polizas (52,000 registros)

| Campo | Tipo | Descripción | Rango/Valores |
|-------|------|-------------|---------------|
| poliza_id | Integer | Identificador único de póliza | 1-52000 |
| edad_conductor | Integer | Edad del conductor | 18-75 años |
| experiencia_anos | Integer | Años de experiencia conduciendo | 0-57 años |
| score_crediticio | Integer | Score crediticio | 413-754 (~6% missing) |
| genero | Character | Género del conductor | M/F |
| estado | Character | Estado de la República | 13 estados |
| codigo_postal | Character | Código postal | 5 dígitos |
| marca_vehiculo | Character | Marca del vehículo | 9 marcas |
| modelo_vehiculo | Character | Modelo del vehículo | 18 modelos |
| tipo_vehiculo | Character | Tipo de vehículo | Sedan/Hatchback/SUV |
| año_vehiculo | Integer | Año del modelo | 2005-2025 |
| canal_venta | Character | Canal de venta | Agent/Direct/Bank/Digital |
| fecha_inicio | Date | Fecha de inicio de vigencia | - |
| fecha_vencimiento | Date | Fecha de vencimiento | - |
| exposicion | Numeric | Periodo de exposición | 0-1 año |
| suma_asegurada | Numeric | Monto asegurado | MXN |
| prima_neta | Numeric | Prima neta | MXN |

### Tabla: siniestros (2,332 registros)

| Campo | Tipo | Descripción | Rango/Valores |
|-------|------|-------------|---------------|
| siniestro_id | Integer | Identificador único de siniestro | 1-2332 |
| poliza_id | Integer | FK a tabla pólizas | 1-52000 |
| fecha_siniestro | Date | Fecha de ocurrencia | - |
| fecha_reporte | Date | Fecha de reporte | 0-15 días después |
| monto_siniestro | Numeric | Monto del siniestro | MXN |
| tipo_siniestro | Character | Tipo de siniestro | Collision/Damage/Partial_Theft/Total_Theft/Fire |
| estado_siniestro | Character | Estado del siniestro | Paid/In_Process/Rejected |
| deducible | Numeric | Deducible aplicado | MXN (15% mínimo) |
| monto_pagado | Numeric | Monto pagado neto | MXN |
| mes_siniestro | Integer | Mes de ocurrencia | 1-12 |

## Parámetros de Calibración

Los datos sintéticos están calibrados con el mercado mexicano:

- **Frecuencia objetivo**: 8.5% (basado en CONDUSEF)
- **Severidad promedio**: $24,000 MXN (basado en AMIS)
- **Loss Ratio objetivo**: 75%
- **Tasa de datos faltantes**: 6%

### Distribuciones Estadísticas

- Frecuencia de siniestros: Distribución Poisson (λ=0.085)
- Severidad: Distribución Gamma
- Ajustes estacionales: Factores mensuales aplicados

## KPIs del Dataset Generado

Resultados de la generación actual (2025-09-02):

- Loss Ratio: 74.28% (target: 75%)
- Frecuencia: 8.95% (target: 8.5%)
- Severidad promedio: $27,499.57 MXN
- Total pólizas: 52,000
- Total siniestros: 2,332
- Objetivo alcanzado: TRUE

### Distribución de Siniestros por Tipo

- Colisión: 65%
- Daño: 20%
- Robo parcial: 10%
- Robo total: 4%
- Incendio: 1%

### Distribución de Canales de Venta

- Agente: 45%
- Directo: 25%
- Banco: 20%
- Digital: 10%

## Características del Dataset

- Datos geográficamente distribuidos en 13 estados mexicanos
- 18 modelos de vehículos populares en México
- Modelado de depreciación por marca y antigüedad
- Ajustes estacionales en siniestralidad
- Cálculo de factores de riesgo (edad, tipo vehículo, zona)
- Missing data realista (~6%)
- Sin duplicados en IDs

## Próximos Pasos

Este proyecto está en fase de fundación. Próximos desarrollos planeados:

1. Dashboard interactivo con Shiny
2. Análisis exploratorio de datos (EDA)
3. Modelado predictivo de frecuencia/severidad
4. Análisis de segmentación de riesgo
5. Reportes automatizados

## Autor

Andrés González Ortega

## Licencia

MIT License - ver archivo LICENSE para detalles
