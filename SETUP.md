# Configuración del Entorno Reproducible

Este proyecto usa `renv` para gestión de dependencias de R.

## Primera Vez - Configuración Inicial (Paso a Paso)

### 1. Abrir el proyecto en RStudio

Doble clic en `CarteraSeguroAutos.Rproj` para abrir el proyecto en RStudio.

### 2. Instalar renv (si no lo tienes)

En la consola de RStudio, ejecuta:

```r
install.packages("renv")
```

Espera a que termine la instalación (1-2 minutos).

**Nota:** Si ya tienes renv instalado, puedes saltar este paso.

### 3. Restaurar las dependencias del proyecto

En la consola de RStudio, ejecuta:

```r
renv::restore()
```

**IMPORTANTE:** Te pueden aparecer diferentes mensajes dependiendo del estado de tu proyecto:

#### Escenario A: Proyecto no activado

Si ves este mensaje:
```
It looks like you've called renv::restore() in a project that hasn't been activated yet.
How would you like to proceed?

1: Activate the project and use the project library.
2: Do not activate the project and use the current library paths.
3: Cancel and resolve the situation another way.

Selection:
```

**Respuesta:** Escribe `1` y presiona Enter.

Esto activará renv para el proyecto. RStudio se reiniciará automáticamente.

#### Escenario B: Después de la activación (o si ya estaba activado)

Después del reinicio, ejecuta nuevamente:

```r
renv::restore()
```

Ahora verás un mensaje como:
```
The following package(s) will be installed:
- tidyverse [2.0.0]
- shiny [1.9.1]
- plotly [4.10.4]
...

Do you want to proceed? [y/N]:
```

**Respuesta:** Escribe `y` y presiona Enter.

### 4. Esperar la instalación

renv descargará e instalará todos los paquetes necesarios:
- tidyverse
- lubridate
- DBI
- RSQLite
- shiny
- shinydashboard
- plotly
- DT
- knitr
- rmarkdown
- kableExtra
- scales

**Tiempo estimado:** 5-10 minutos dependiendo de tu conexión a internet.

Verás mensajes de progreso en la consola. Cuando termine, verás algo como:
```
✔ The library has been restored.
```

### 5. Verificar la instalación

Una vez completado, verifica que todo funciona:

```r
library(tidyverse)
library(shiny)
```

Si no hay errores, la instalación fue exitosa.

---

## Solución de Problemas Comunes

### Error: "no hay paquete llamado 'renv'"

**Causa:** renv no está instalado en tu sistema.

**Solución:**
```r
install.packages("renv")
```

Luego ejecuta `renv::restore()` nuevamente.

### RStudio se reinicia automáticamente

**Esto es normal.** Cuando activas renv por primera vez, RStudio reinicia la sesión de R para cargar el entorno del proyecto. Simplemente ejecuta `renv::restore()` nuevamente después del reinicio y confirma con `y`.

### Error de compilación de paquetes

Si algún paquete falla al compilar:

**En Windows:**
- Instala Rtools desde https://cran.r-project.org/bin/windows/Rtools/
- Reinicia RStudio
- Ejecuta `renv::restore()` nuevamente

**En Mac:**
- Abre Terminal
- Ejecuta `xcode-select --install`
- Reinicia RStudio
- Ejecuta `renv::restore()` nuevamente

**En Linux:**
- Instala dependencias de desarrollo: `sudo apt-get install build-essential libcurl4-openssl-dev libssl-dev libxml2-dev`

### Instalación muy lenta

**Esto es normal** la primera vez. renv descarga e instala versiones específicas de cada paquete para garantizar reproducibilidad. La instalación puede tomar 5-15 minutos dependiendo de:
- Velocidad de tu conexión a internet
- Velocidad de tu procesador
- Cantidad de paquetes que necesitan compilarse

Las instalaciones futuras serán mucho más rápidas porque renv reutiliza paquetes ya descargados.

### Error: "lockfile was generated with R 4.0.0"

Este es solo un aviso informativo, no un error. El proyecto funciona con versiones de R 4.0 o superiores. Puedes ignorar este mensaje de forma segura.

---

## Uso Diario

Una vez completada la configuración inicial, cuando abras el proyecto en RStudio, renv se activará automáticamente gracias al archivo `.Rprofile`.

### Agregar Nuevos Paquetes

Si necesitas agregar un paquete nuevo al proyecto:

```r
# Instalar paquete
install.packages("nuevo_paquete")

# Actualizar el lockfile para compartir con otros
renv::snapshot()
```

### Actualizar Paquetes Existentes

```r
# Actualizar todos los paquetes
renv::update()

# Actualizar paquete específico
renv::update("nombre_paquete")

# Guardar cambios
renv::snapshot()
```

### Restaurar Dependencias (otro colaborador)

Si otro desarrollador clona el proyecto o si cambias de computadora:

```r
renv::restore()
```

## Verificación del Entorno

Para verificar el estado de tus paquetes:

```r
# Ver estado del proyecto
renv::status()

# Ver paquetes instalados
renv::dependencies()

# Ver qué paquetes están desactualizados
renv::update(check = TRUE)
```

---

## Archivos de renv

### Archivos que SÍ debes commitear a Git:

- `.Rprofile` - Activa renv automáticamente al abrir el proyecto
- `renv.lock` - Lista completa de paquetes y versiones exactas
- `renv/activate.R` - Script de activación de renv
- `renv/settings.json` - Configuración del proyecto

### Archivos que NO debes commitear a Git:

- `renv/library/` - Paquetes instalados (ya está en .gitignore)
- `renv/local/` - Cache local (ya está en .gitignore)
- `renv/staging/` - Área temporal (ya está en .gitignore)

**Importante:** Cada colaborador tendrá su propia copia local de los paquetes en `renv/library/`. El archivo `renv.lock` garantiza que todos usen las mismas versiones.

---

## Comandos Útiles de renv

```r
# Ver ayuda general
?renv

# Ver estado actual
renv::status()

# Restaurar paquetes desde lockfile
renv::restore()

# Actualizar lockfile con paquetes actuales
renv::snapshot()

# Limpiar paquetes no usados
renv::clean()

# Ver historial de snapshots
renv::history()

# Reparar instalación si algo salió mal
renv::repair()
```

---

## Preguntas Frecuentes

**¿Por qué usar renv en lugar de instalar paquetes normalmente?**

renv garantiza que todos los colaboradores usen exactamente las mismas versiones de paquetes, evitando el problema de "en mi máquina funciona". También permite tener diferentes versiones de paquetes para diferentes proyectos sin conflictos.

**¿Cuánto espacio ocupa renv?**

Este proyecto requiere aproximadamente 500MB-1GB de espacio para todos los paquetes. renv tiene un cache global que reutiliza paquetes entre proyectos para ahorrar espacio.

**¿Puedo usar el proyecto sin renv?**

Técnicamente sí, pero no es recomendado. Tendrías que instalar manualmente todos los paquetes con las versiones correctas listadas en README.md. Con renv, todo es automático.

**¿Necesito internet para usar renv después de la primera instalación?**

No. Una vez que los paquetes están instalados con `renv::restore()`, puedes trabajar offline sin problemas.
