# CarteraSeguroAutos

Dashboard interactivo de analisis actuarial para un portafolio sintetico de seguros de autos, calibrado al mercado mexicano (CONDUSEF/AMIS).

## Descripcion

Plataforma R Shiny con arquitectura modular bslib que cubre el pipeline completo de P&C: pricing GLM, reservas IBNR, stress testing Monte Carlo y deteccion de fraude. Los datos sinteticos simulan 5 anos de operacion (2020-2024) con renovaciones, inflacion y patrones de desarrollo.

## Estructura del Proyecto

```
CarteraSeguroAutos/
├── app.R                          # Entrada principal Shiny
├── global.R                       # Carga de paquetes, datos y modulos
├── R/
│   ├── mod_resumen.R              # Resumen ejecutivo con KPIs
│   ├── mod_loss_ratio.R           # Analisis de loss ratio por segmentos
│   ├── mod_frecuencia.R           # Frecuencia de siniestros
│   ├── mod_severidad.R            # Severidad de siniestros
│   ├── mod_temporal.R             # Tendencias temporales
│   ├── mod_geografico.R           # Analisis geografico (leaflet)
│   ├── mod_segmentacion.R         # Segmentacion de riesgo (heatmap)
│   ├── mod_pricing_glm.R          # Pricing GLM (Poisson + Gamma)
│   ├── mod_ibnr.R                 # Reservas IBNR (Chain Ladder + BF)
│   ├── mod_scenario.R             # Stress testing (VaR/TVaR Monte Carlo)
│   ├── mod_fraud.R                # Deteccion de fraude (Mahalanobis + reglas)
│   ├── mod_sidebar_filters.R      # Filtros globales del sidebar
│   ├── mod_datos.R                # Explorador de datos y diccionario
│   ├── utils_metrics.R            # Funciones puras de metricas actuariales
│   ├── utils_data.R               # Carga, enriquecimiento y filtrado de datos
│   ├── utils_theme.R              # Tema centralizado y formateo
│   └── utils_export.R             # Handlers de exportacion CSV/Excel
├── data/
│   ├── GeneracionDatos.R          # Generacion de datos sinteticos (5 anos)
│   ├── CargaDatos.R               # Carga a SQLite
│   ├── siniestralidad.db          # Base de datos SQLite
│   └── processed/                 # CSVs generados y metadata
├── tests/testthat/                # Suite de tests (testthat)
├── Dockerfile                     # Contenedorizacion para Cloud Run
├── .github/workflows/ci.yml       # CI/CD (test + deploy)
├── renv.lock                      # Dependencias reproducibles
├── InformeEjecutivo.Rmd           # Reporte ejecutivo RMarkdown
└── SETUP.md                       # Guia de configuracion renv
```

## Instalacion

### Requisitos

- R >= 4.3
- renv (gestion de dependencias)

### Configuracion

```bash
git clone https://github.com/GonorAndres/CarteraSeguroAutos.git
cd CarteraSeguroAutos
```

```r
# Restaurar dependencias
renv::restore()

# Lanzar dashboard
shiny::runApp()
```

Para configuracion detallada de renv, ver [SETUP.md](SETUP.md).

### Docker

```bash
docker build -t cartera-autos .
docker run -p 8080:8080 cartera-autos
```

## Dataset

### Volumenes

| Tabla | Registros | Descripcion |
|-------|-----------|-------------|
| polizas | 140,346 | 12K nuevas/ano + renovaciones (2020-2024) |
| siniestros | 11,714 | Colision, Robo, Danos, Incendio |
| pagos_desarrollo | 28,264 | Pagos incrementales (dev 0-4) |

### Calibracion (CONDUSEF/AMIS)

| Parametro | Target | Logrado |
|-----------|--------|---------|
| Frecuencia | 8.5% | 8.4% |
| Severidad media | $24,000 MXN | $27,477 MXN |
| Loss Ratio | 75% | 69.6% |
| Retencion | 82% | 82% |
| Inflacion severidad | 4% anual | 4% |

### Distribuciones

- Frecuencia: Poisson (lambda = 0.085 * factores de riesgo)
- Severidad: Gamma (shape=2, scale=8000) con ajustes por tipo, inflacion, estacionalidad
- Factores de riesgo: edad (1.0-1.35), vehiculo (1.0-1.15), zona (0.95-1.30)
- Tipos de siniestro: Colision 65%, Danos 20%, Robo Parcial 10%, Robo Total 4%, Incendio 1%
- Canales: Agente 45%, Directo 25%, Banco 20%, Digital 10%

## Modulos del Dashboard

### Analisis Basico (7 tabs)
- **Resumen** - KPIs principales, loss ratio por canal, tendencia mensual
- **Loss Ratio** - Segmentacion por canal, vehiculo, marca, ano
- **Frecuencia** - Por vehiculo, edad, genero, canal
- **Severidad** - Estadisticos por tipo, histograma, tendencia
- **Temporal** - Tendencias mensuales, estacionalidad, lag de reporte
- **Geografico** - Mapa interactivo (leaflet), ranking de estados
- **Segmentacion** - Heatmap loss ratio edad x vehiculo

### Modulos Actuariales Avanzados (4 tabs)
- **Pricing GLM** - Poisson (frecuencia) + Gamma (severidad), relatividades, cotizador interactivo
- **Reservas IBNR** - Chain Ladder + Bornhuetter-Ferguson, triangulo de desarrollo, errores estandar Mack
- **Escenarios** - Modelo colectivo de riesgo, Monte Carlo (1K-50K sims), VaR/TVaR 95/99/99.5%
- **Fraude** - Mahalanobis + 5 flags basados en reglas, score compuesto

## CI/CD

- **Test**: R 4.3.3 + renv + testthat en GitHub Actions
- **Deploy**: Docker -> Google Artifact Registry -> Cloud Run (2 vCPU, 2 GiB)
- **Auth**: Workload Identity Federation (sin service account keys)

## Tests

```r
testthat::test_dir("tests/testthat")
```

Cobertura: metricas actuariales, carga/filtrado de datos, triangulo IBNR, formateo, integridad de datos.

## Autor

Andres Gonzalez Ortega

## Licencia

MIT License - ver archivo LICENSE para detalles
