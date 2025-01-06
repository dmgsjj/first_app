# Dashboard de Caracterización de la Población Colombiana y Venezolana - GEIH 2024

Este proyecto es una aplicación interactiva desarrollada por **Daniel Molina** utilizando `Shiny` y `shinydashboard`, basada en los datos de la Gran Encuesta Integrada de Hogares (GEIH) 2024 del DANE. La herramienta permite caracterizar a la población colombiana y venezolana en el país.

👉 **[Ver la aplicación en vivo](https://jsidte-daniel-molina.shinyapps.io/shiny-app/)**

## Funcionalidades principales

- **Módulos interactivos**: 
  - Demografía
  - Educación
  - Mercado laboral
  - Vivienda
  - Salud
  - Motivos de migración
- **Enfoque en migración venezolana**: Caracterización específica de la población venezolana en Colombia.
- **Visualizaciones avanzadas**: 
  - Pirámides poblacionales
  - Gráficos circulares y de barras
  - Gráficos interactivos con `plotly`
- **Filtros dinámicos**: Selección por nivel (nacional o departamental) y por condición migratoria.
- **Exportación de datos**: Descarga de datos procesados en formatos CSV y Excel.

## Estructura del proyecto

El desarrollo de este proyecto se basa en un proceso estructurado que consolida y prepara los datos de la Gran Encuesta Integrada de Hogares (GEIH) 2024 para su análisis interactivo.

### `app.R`
Contiene el código principal de la aplicación Shiny, definiendo la interfaz de usuario (UI) y la lógica del servidor (server). Aquí se gestionan las visualizaciones interactivas, los filtros dinámicos y las opciones de exportación.

### `preparacion/`
Carpeta que agrupa los scripts para el procesamiento de los datos a diferentes niveles:
- **`caracterizacion_nacional.R`**: Procesa los datos consolidados para la caracterización a nivel nacional. Genera estadísticas como pirámides poblacionales, distribuciones de género y análisis educativos.
- **`caracterizacion_departamento.R`**: Se encarga del procesamiento para análisis a nivel departamental, ofreciendo resultados específicos para cada uno de los departamentos de Colombia.

### `data/`
Contiene los archivos mensuales de la GEIH, tal como se obtuvieron inicialmente. Estos datos sirvieron como insumo para el proceso de consolidación, pero no son utilizados directamente por la aplicación.

### `geih_complete.csv`
Este archivo representa la GEIH consolidada y limpia, creada a partir de los datos mensuales. Es el principal insumo para los scripts de preparación ubicados en la carpeta `preparacion/`.  
La consolidación inicial y la limpieza de los datos se realizaron utilizando un proyecto previo, disponible en [este repositorio](https://github.com/Alicbm/data-exploration). Dicho proyecto permitió unificar y depurar los datos mensuales en un único dataset.

### `www/`
Carpeta que contiene recursos estáticos como hojas de estilo personalizadas, capturas de pantalla y scripts adicionales para mejorar la experiencia del usuario en la aplicación.

---

## Ejemplos de visualizaciones

- **Pirámide Poblacional**
- **Distribución por Género**
- **Estado Civil de Migrantes Venezolanos**

## Créditos

- **Desarrollador**: Daniel Molina  
- **Datos fuente**: DANE - Gran Encuesta Integrada de Hogares (GEIH) 2024

