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

- **`app.R`**: Código principal de la aplicación.
- **`preparacion/`**: Scripts para el procesamiento y preparación de datos.
  - `caracterizacion_nacional.R`: Procesamiento de datos a nivel nacional.
  - `caracterizacion_departamento.R`: Procesamiento de datos a nivel departamental.
- **`data/`**: Archivos de entrada con datos de caracterización.
- **`www/`**: Recursos estáticos como hojas de estilo y capturas de pantalla.


## Ejemplos de visualizaciones

- **Pirámide Poblacional**
- **Distribución por Género**
- **Estado Civil de Migrantes Venezolanos**

## Créditos

- **Desarrollador**: Daniel Molina  
- **Datos fuente**: DANE - Gran Encuesta Integrada de Hogares (GEIH) 2024

