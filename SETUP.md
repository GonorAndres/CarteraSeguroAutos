# Configuración del Entorno Reproducible

Este proyecto usa `renv` para gestión de dependencias de R.

## Primera Vez - Configuración Inicial

1. Abrir el proyecto en RStudio (doble clic en `CarteraSeguroAutos.Rproj`)

2. Instalar renv si no lo tienes:
```r
install.packages("renv")
```

3. Inicializar renv (primera vez):
```r
renv::init()
```

4. Instalar todas las dependencias del proyecto:
```r
renv::restore()
```

## Uso Diario

Cuando abras el proyecto en RStudio, renv se activará automáticamente gracias al archivo `.Rprofile`.

### Agregar Nuevos Paquetes

```r
# Instalar paquete
install.packages("nuevo_paquete")

# Actualizar el lockfile
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

### Restaurar Dependencias

Si otro desarrollador clona el proyecto:

```r
renv::restore()
```

## Verificación del Entorno

```r
# Ver estado del proyecto
renv::status()

# Ver paquetes instalados
renv::dependencies()
```

## Archivos de renv

- `.Rprofile`: Activa renv automáticamente
- `renv.lock`: Lista de paquetes y versiones (commitear a git)
- `renv/`: Biblioteca local de paquetes (NO commitear a git)

## Notas

- El archivo `renv.lock` debe commitearse a git para compartir el entorno
- La carpeta `renv/` está en `.gitignore` y no debe commitearse
- Cada colaborador tendrá su propia copia local de los paquetes
