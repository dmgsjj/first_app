library(shiny)
library(plotly)
library(bit64)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(reshape2)
library(tidyr)
library(RColorBrewer)
library(viridis)
library(data.table)
library(paletteer)
library(openxlsx)
library(DT)


# Leer el archivo CSV asegurando que se mantengan los formatos correctos
geih <- fread("geih_complete.csv")


migrant_variables <- c(
  # Variables identificadoras
  "DIRECTORIO", "SECUENCIA_P", "ORDEN", "HOGAR", "FEX_C18", "DPTO", "MES", "PERIODO", "PER", "AREA", "REGIS",
  # Variables de Migración
  "P3373S3", "P3374S1", "P3374S2", "P3374S3", "P3373S3A1",
  # Variables Demográficas
  "P6040", "P3271", "P6070",
  # Variables Educativas
  "P3042", "P6170",
  # Variables Laborales
  "OCI", "DSI", "INGLABO", "P6430", "P6800", "PT", "FT", "PET",
  # Variables de Vivienda y Hogar
  "P4000", "P4030S1", "P4030S2", "P4030S3", "P4030S5", "P6008", "P70", "P5090",
  # Variables de Salud
  "P6090", "P6100",
  # Variables de Motivos de Migración
  "P3386", "P3373S3", "P3374S1"
)


# Filtrar los datos de migrantes que cumplan ciertas condiciones
Venezuelan_Migrants <- geih[P3373S3 == 862 & P3374S1 == 862, migrant_variables, with = FALSE]

source('preparacion/caracterizacion_nacional.R')
source('preparacion/caracterizacion_departamento.R')



# Lista de departamentos de Colombia
departamentos_colombia <- c(
  "Amazonas", "Antioquia", "Arauca", "Atlántico", "Bolívar", 
  "Boyacá", "Bogotá", "Caldas", "Caquetá", "Casanare", "Cauca", 
  "Cesar", "Chocó", "Córdoba", "Cundinamarca", "Guainía", 
  "Guaviare", "Huila", "La Guajira", "Magdalena", "Meta", 
  "Nariño", "Norte de Santander", "Putumayo", "Quindío", 
  "Risaralda", "San Andrés y Providencia", "Santander", 
  "Sucre", "Tolima", "Valle del Cauca", "Vaupés", "Vichada"
)

# Definir la interfaz de usuario
ui <- dashboardPage(
  dashboardHeader(title = "Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",  # Asegurarse de tener un ID para las pestañas
      menuItem("Demografía", tabName = "demographics", icon = icon("users")),
      menuItem("Educación", tabName = "education", icon = icon("graduation-cap")),
      menuItem("Mercado Laboral", tabName = "labor_market", icon = icon("briefcase")),
      menuItem("Vivienda", tabName = "housing", icon = icon("home")),
      menuItem("Salud", tabName = "health", icon = icon("medkit")),
      menuItem("Datos", tabName = "datos", icon = icon("database"))
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$link(
        rel = "stylesheet", 
        type = "text/css", 
        href = "radar_style.css")),
    tags$script(src = 'id_generate.js'),
    
    fluidRow(
      # Filtros visibles en todas las pestañas excepto "Datos"
      conditionalPanel(
        condition = "input.tabs != 'datos'",  # Filtrar por las pestañas activas
        box(title = "Seleccionar Nivel", status = "primary", solidHeader = TRUE, width = 6,
            selectInput("level_selection", "Nivel:", choices = c("Nacional", "Departamental")),
            conditionalPanel(
              condition = "input.level_selection == 'Departamental'",
              selectInput("department_selection", "Seleccionar Departamento:", choices = departamentos_colombia)
            )
        ),
        box(title = "Migración Venezolana", status = "primary", solidHeader = TRUE, width = 6,
            selectInput("migration_filter", "Migración Venezolana:", choices = c("Todos", "Solo Migración Venezolana"))
        )
      )
    ),
    
    tabItems(
      tabItem(tabName = "demographics",
              fluidRow(
                column(width = 6,
                       box(title = "Pirámide Poblacional", status = "primary", solidHeader = TRUE, width = NULL,
                           plotlyOutput("pyramidPlot", height = "400px"))
                ),
                column(width = 6,
                       box(title = "Distribución de Género", status = "primary", solidHeader = TRUE, width = NULL,
                           plotlyOutput("genderPiePlot", height = "400px"))
                )
              ),
              fluidRow(
                box(title = "Estado Civil", status = "primary", solidHeader = TRUE, width = 12,
                    plotlyOutput("maritalStatusBarPlot", height = "400px"))
              )
      ),
      tabItem(tabName = "education",
              fluidRow(
                column(width = 6,
                       box(title = "Nivel Educativo Alcanzado", status = "primary", solidHeader = TRUE, width = NULL,
                           plotlyOutput("educationBarPlot", height = "600px"))
                ),
                column(width = 6,
                       box(title = "Ingreso por Nivel de Escolaridad", status = "primary", solidHeader = TRUE, width = NULL,
                           plotlyOutput("incomeBarPlot", height = "600px"))
                )
              )
      ),
      tabItem(tabName = "labor_market",
              fluidRow(
                column(width = 6,
                       box(title = "Desocupado y Ocupación por Género", status = "primary", solidHeader = TRUE, width = NULL,
                           plotlyOutput("laborMarketBarPlot", height = "400px"))
                ),
                column(width = 6,
                       box(title = "Tasa de Desempleo y Ocupación por Género", status = "primary", solidHeader = TRUE, width = NULL,
                           plotlyOutput("laborMarketBarPlot2", height = "400px"))
                )
              ),
              fluidRow(
                box(title = "Tipo de Trabajo", status = "primary", solidHeader = TRUE, width = 12,
                    plotlyOutput("jobTypeBarPlot", height = "400px"))
              )
      ),
      tabItem(tabName = "housing",
              fluidRow(
                column(width = 6,
                       box(title = "Tipo de Vivienda", status = "primary", solidHeader = TRUE, width = NULL,
                           plotlyOutput("housingTypeBarPlot", height = "600px"))
                ),
                column(width = 6,
                       box(title = "Condiciones del Hogar", status = "primary", solidHeader = TRUE, width = NULL,
                           plotlyOutput("homeConditionsBarPlot", height = "600px"))
                )
              )
      ),
      tabItem(tabName = "health",
              fluidRow(
                column(width = 6,
                       box(title = "Acceso a Salud", status = "primary", solidHeader = TRUE, width = NULL,
                           plotlyOutput("healthCoverageBarPlot", height = "600px"))
                ),
                column(width = 6,
                       box(title = "Tipo de Afiliación al Sistema de Salud", status = "primary", solidHeader = TRUE, width = NULL,
                           plotlyOutput("healthAffiliationBarPlot", height = "600px"))
                )
              )
      ),
      tabItem(tabName = "datos",
              fluidRow(
                box(title = "Seleccionar Nivel de Datos", status = "primary", solidHeader = TRUE, width = 12,
                    selectInput("level_selection", "Selecciona el Nivel de Datos:",
                                choices = c("Nacional", "Migrante"), selected = "Nacional")
                )
              ),
              fluidRow(
                box(title = "Seleccionar Variable", status = "primary", solidHeader = TRUE, width = 12,
                    uiOutput("sheet_selector")  # Selector de hojas
                )
              ),
              fluidRow(
                box(
                  title = "Visualización de Datos", status = "primary", solidHeader = TRUE, width = 12,
                  p("Tabla interactiva con los datos descargados:"),
                  DT::DTOutput("tableData"),
                  downloadButton("exportCSV", "Exportar como CSV"),
                  downloadButton("exportExcel", "Exportar como Excel")
                )
              )
      )
    )
  )
)


# Definir el servidor
server <- function(input, output, session) {
  
  # Rutas de los archivos
  nacional_file <- 'data.frame/caracterizacion_poblacional.xlsx'
  migrante_file <- 'data.frame/caracterizacion_vene_dep.xlsx'
  
  
  colors <- c("#B4D4DAFF", "#A9D2DCFF", "#9ECFDDFF", "#93CDDFFF", "#86CAE1FF",
              "#7AC7E2FF", "#76C1DFFF", "#72BCDCFF", "#6EB6D9FF", "#6AB1D6FF",
              "#64AAD2FF", "#5BA2CCFF", "#529AC6FF", "#4993C0FF", "#3F8BBAFF",
              "#3885B6FF", "#3281B5FF", "#2D7DB4FF", "#2678B3FF", "#1F74B1FF",
              "#1C6FAEFF", "#1C6AA8FF", "#1C65A3FF", "#1C5F9EFF", "#1C5A99FF")
  
  
  colors_gender <- c(colors[10], colors[20]) 
  
  
  
  # Función reactiva para filtrar los datos según el nivel (Nacional o Departamental)
  datos_filtrados <- reactive({
    data <- if (input$migration_filter == "Todos") geih else Venezuelan_Migrants
    
    resultados <- if (input$level_selection == "Nacional") {
      list(
        pyramid_population_result = pyramid_population_nacional(data),
        marital_status_result = marital_status_nacional(data),
        sex_result = sex_nacional(data),
        education_achieved_result = education_achieved_nacional(data),
        income_by_education_result = income_by_education_nacional(data),
        group_variables_result = group_variables_nacional(data),
        job_type_result = job_type_nacional(data),
        calcular_tipo_vivienda_result = calcular_tipo_vivienda_nacional(data),
        home_conditions_result = calcular_condiciones_hogar_nacional(data),
        calcular_acceso_salud_result = calcular_acceso_salud_nacional(data),
        calcular_afiliacion_salud_result = calcular_afiliacion_salud_nacional(data)
      )
    } else {
      req(input$department_selection)
      list(
        pyramid_population_result = piramide_poblaciona_dep(data, input$department_selection),
        marital_status_result = marital_status_dep(data, input$department_selection),
        sex_result = sexo_departamento_dep(data, input$department_selection),
        education_achieved_result = nivel_educacion_dep(data, input$department_selection),
        income_by_education_result = ingreso_por_educacion_dep(data, input$department_selection),
        group_variables_result = estadisticas_laborales_dep(data, input$department_selection),
        job_type_result = tipo_trabajo_dep(data, input$department_selection),
        calcular_tipo_vivienda_result = tipo_vivienda_dep(data, input$department_selection),
        home_conditions_result = calcular_condiciones_hogar_dep(data, input$department_selection),
        calcular_acceso_salud_result = cobertura_salud_dep(data, input$department_selection),
        calcular_afiliacion_salud_result = afiliacion_salud_dep(data, input$department_selection)
      )
    }
    
    return(resultados)
  })
  
  
  
  output$pyramidPlot <- renderPlotly({
    resultados <- datos_filtrados()
    pyramid_population_result <- resultados$pyramid_population_result
    
    
    
    # Asegurar que el rango de edad esté ordenado de menor a mayor
    pyramid_population_result[, age_group := factor(age_group, 
                                                    levels = c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", 
                                                               "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", 
                                                               "60-64", "65-69", "70-74", "75-79", "80-84", "85+"))]
    
    # Crear gráfico de pirámide poblacional con plotly
    pyramid_plotly <- plot_ly(pyramid_population_result, 
                              x = ~Mujeres_pct, 
                              y = ~age_group, 
                              type = 'bar', 
                              name = 'Mujeres', 
                              orientation = 'h', 
                              marker = list(color = colors[10]),  # Color para Mujeres
                              hoverinfo = 'text',
                              text = ~paste0("<b>", round(abs(Mujeres_pct), 1), "%</b>"),  # Valores en blanco y negrita dentro del gráfico
                              textfont = list(color = 'white')) %>%  # Texto en blanco
      
      add_trace(x = ~Hombres_pct, 
                y = ~age_group, 
                type = 'bar', 
                name = 'Hombres', 
                orientation = 'h', 
                marker = list(color = colors[20]),  # Color para Hombres
                hoverinfo = 'text',
                text = ~paste0("<b>", round(abs(Hombres_pct), 1), "%</b>"),  # Valores en blanco y negrita dentro del gráfico
                textfont = list(color = 'white')) %>%  # Texto en blanco
      
      layout(
        barmode = 'overlay',
        xaxis = list(title = 'Porcentaje de Población',
                     tickformat = '.0%',  # Sin decimales
                     range = c(-5, 5),  # Ajustar rango de los ejes para mostrar de 0 a 5 en ambos lados
                     tickvals = seq(-5, 5, by = 1),  # Marcas en el eje x
                     ticktext = paste0(abs(seq(-5, 5, by = 1)), "%"),  # Etiquetas en porcentaje desde 0 a 5
                     showgrid = TRUE, 
                     zeroline = FALSE,
                     titlefont = list(family = "bold", color = 'white'),  # Fuente bold para el título del eje X
                     tickfont = list(family = "bold", color = 'white')),  # Color blanco para el eje X
        yaxis = list(title = 'Grupos de Edad', 
                     categoryorder = "array", 
                     categoryarray = levels(pyramid_population_result$age_group), 
                     titlefont = list(family = "bold", color = 'white'),  # Fuente bold para el título del eje Y
                     tickfont = list(family = "bold", color = 'white')),  # Color blanco para las etiquetas del eje Y
        legend = list(title = list(text = 'Sexo', font = list(family = "bold", color = 'white')), 
                      font = list(family = "bold", color = 'white')),  # Color blanco y negrita para la leyenda
        hovermode = "compare",
        
        # Ajustar el tamaño del gráfico, color de fondo y márgenes
        plot_bgcolor = '#013B63',  # Color de fondo
        paper_bgcolor = '#013B63',  # Color del área total del gráfico
        margin = list(l = 100, r = 30, t = 10, b = 10)  # Reducir márgenes para aprovechar el espacio
      )
    
    # Mostrar el gráfico interactivo con los valores en blanco y negrita
    pyramid_plotly
    
  })
  
  
  output$genderPiePlot <- renderPlotly ({
    
    resultados <- datos_filtrados()
    sex_result <- resultados$sex_result
    
    # Crear el gráfico circular con plotly
    Sexo_r <- plot_ly(sex_result, 
                      labels = ~P3271, 
                      values = ~Percentage, 
                      type = 'pie',
                      text = ~paste0(format(personas, big.mark = ",", scientific = FALSE), 
                                     "\n", round(Percentage, 1), "%"),
                      textposition = 'inside',  # Colocar las etiquetas dentro de las porciones
                      textinfo = 'text',  # Mostrar solo el texto personalizado
                      insidetextfont = list(color = 'white',  # Texto blanco dentro de las porciones
                                            size = 14, 
                                            family = 'Arial'),  # Estilo del texto
                      hoverinfo = 'label+value+percent',  # Información al pasar el cursor
                      marker = list(colors = colors_gender))  # Usar los mismos colores para hombres y mujeres
    
    # Layout del gráfico
    Sexo_r <- Sexo_r %>%
      layout(  # Título en blanco y negrita
        legend = list(title = list(text = "Género", font = list(color = 'white', face = "bold")),  # Título de la leyenda en blanco
                      font = list(color = 'white', face = "bold"),  # Texto de la leyenda en blanco
                      x = 0.9,  # Posición de la leyenda
                      y = 0.5), 
        plot_bgcolor = '#013B63',  # Color de fondo
        paper_bgcolor = '#013B63',  # Color del área total del gráfico
        margin = list(l = 100, r = 30, t = 10, b = 10))
    
    # Mostrar el gráfico interactivo
    Sexo_r
    
  })
  
  output$maritalStatusBarPlot <- renderPlotly({
    resultados <- datos_filtrados()
    
    marital_status_result <- resultados$marital_status_result
    
    marital_status_result <- marital_status_result[order(-personas)]
    
    # Crear el gráfico con plotly
    grafico3 <- plot_ly(
      marital_status_result, 
      x = ~personas,  # El número de personas va en el eje X
      y = ~reorder(as.factor(P6070), personas),  # El estado civil va en el eje Y, reordenado
      type = 'bar', 
      orientation = 'h',  # Barras horizontales
      color = ~as.factor(P6070), 
      colors = colors,  # Usar la paleta de colores definida manualmente
      text = ~paste0(format(personas, big.mark = ",", scientific = FALSE)),  # Etiquetas de texto
      textposition = 'auto',  # Posicionar automáticamente las etiquetas dentro o fuera de la barra
      textfont = list(color = 'white', size = 14, family = "bold"),  # Texto en blanco y negrita
      hoverinfo = 'text',  # Mostrar solo las etiquetas al pasar el cursor
      marker = list(line = list(color = 'black', width = 1))  # Borde de las barras
    )
    
    # Configurar el diseño del gráfico
    grafico3 <- grafico3 %>%
      layout(
        # Título en blanco y negrita
        xaxis = list(title = 'Número de Personas',
                     tickformat = ',',  # Formato de los números
                     tickfont = list(size = 12, color = 'white', family = "bold"),  # Etiquetas del eje X en blanco y negrita
                     titlefont = list(size = 14, color = 'white', family = "bold")),  # Título del eje X en blanco y negrita
        yaxis = list(title = '',
                     tickfont = list(size = 12, color = 'white', family = "bold"),  # Etiquetas del eje Y en blanco y negrita
                     titlefont = list(size = 14),
                     automargin = TRUE,
                     ticklabelposition = "outside",  # Posicionar las etiquetas fuera del eje
                     ticklen = 8,  # Ajustar longitud de las etiquetas
                     tickwidth = 1,  # Ajustar grosor de las líneas de las etiquetas
                     tickcolor = '#013B63'),  # Aumentar el margen automáticamente
        legend = list(title = list(text = '', font = list(color = 'white', face = "bold")),  # Eliminar el título de la leyenda
                      font = list(color = 'white', face = "bold")),  # Texto de la leyenda en blanco y negrita
        plot_bgcolor = '#013B63',  # Fondo del gráfico en azul oscuro
        paper_bgcolor = '#013B63',  # Fondo del área total del gráfico en azul oscuro
        margin = list(l = 100, r = 30, t = 30, b = 30)  # Ajustar márgenes
      )
    
    # Mostrar el gráfico interactivo
    grafico3
    
  })
  
  output$educationBarPlot <- renderPlotly({
    resultados <- datos_filtrados()
    education_achieved_result <- resultados$education_achieved_result
    
    grafico4 <- plot_ly(
      education_achieved_result,
      x = ~personas,  # El número de personas en el eje X
      y = ~reorder(as.factor(P3042), personas),  # El nivel educativo en el eje Y, reordenado
      type = 'bar', 
      orientation = 'h',  # Barras horizontales
      color = ~as.factor(P3042), 
      colors = colors,  # Usar paleta de colores viridis
      text = ~format(round(personas, 0), big.mark = ","),  # Etiquetas con separadores de miles con coma
      textposition = 'auto',  # Posicionar automáticamente las etiquetas
      textfont = list(color = 'white', size = 14, family = "bold"),  # Texto en blanco y negrita
      hoverinfo = 'text',  # Mostrar solo las etiquetas al pasar el cursor
      marker = list(line = list(color = 'black', width = 1))  # Borde de las barras
    )
    
    # Configurar el diseño del gráfico
    grafico4 <- grafico4 %>%
      layout(
        # Título en blanco y negrita
        xaxis = list(title = 'Número de Personas',
                     tickformat = ',',  # Formato de los números
                     tickfont = list(size = 12, color = 'white', family = "bold"),  # Etiquetas del eje X en blanco y negrita
                     titlefont = list(size = 14, color = 'white', family = "bold")),  # Título del eje X en blanco y negrita
        yaxis = list(title = '',
                     tickfont = list(size = 12, color = 'white', family = "bold"),  # Etiquetas del eje Y en blanco y negrita
                     titlefont = list(size = 14),
                     automargin = TRUE,
                     ticklabelposition = "outside",  # Posicionar las etiquetas fuera del eje
                     ticklen = 8,  # Ajustar longitud de las etiquetas
                     tickwidth = 1,  # Ajustar grosor de las líneas de las etiquetas
                     tickcolor = '#013B63'),  # Aumentar el margen automáticamente
        legend = list(title = list(text = '', font = list(color = 'white', face = "bold")),  # Eliminar el título de la leyenda
                      font = list(color = 'white', face = "bold")),  # Texto de la leyenda en blanco y negrita
        plot_bgcolor = '#013B63',  # Fondo del gráfico en azul oscuro
        paper_bgcolor = '#013B63',  # Fondo del área total del gráfico en azul oscuro
        margin = list(l = 100, r = 30, t = 30, b = 30)  # Ajustar márgenes
      )
    
    grafico4
  })
  
  output$incomeBarPlot <- renderPlotly({
    
    
    resultados <- datos_filtrados()
    income_by_education_result <- resultados$income_by_education_result
    
    grafico5 <- plot_ly(
      income_by_education_result,
      x = ~ ingreso,  # El número de personas en el eje X
      y = ~ reorder(as.factor(P3042), ingreso),  # El nivel educativo en el eje Y, reordenado
      type = 'bar', 
      orientation = 'h',  # Barras horizontales
      color = ~as.factor(P3042), 
      colors = colors,  # Usar paleta de colores viridis
      text = ~format(round(ingreso, 0), big.mark = ","),  # Etiquetas con separadores de miles con coma
      insidetextfont = list(color = 'white', size = 14, family = "bold"),  # Etiquetas con separadores de miles con coma
      textposition = 'auto',  # Posicionar automáticamente las etiquetas
      textfont = list(color = 'white', size = 14, family = "bold"),  # Texto en blanco y negrita
      hoverinfo = 'text',  # Mostrar solo las etiquetas al pasar el cursor
      marker = list(line = list(color = 'black', width = 1))  # Borde de las barras
    )
    
    # Configurar el diseño del gráfico
    grafico5 <- grafico5 %>%
      
      layout(
        # Título en blanco y negrita
        xaxis = list(title = 'Ingreso Promedio',
                     tickformat = ',',  # Formato de los números
                     tickfont = list(size = 12, color = 'white', family = "bold"),  # Etiquetas del eje X en blanco y negrita
                     titlefont = list(size = 14, color = 'white', family = "bold")),  # Título del eje X en blanco y negrita
        yaxis = list(title = '',
                     tickfont = list(size = 12, color = 'white', family = "bold"),  # Etiquetas del eje Y en blanco y negrita
                     titlefont = list(size = 14),
                     automargin = TRUE,
                     ticklabelposition = "outside",  # Posicionar las etiquetas fuera del eje
                     ticklen = 8,  # Ajustar longitud de las etiquetas
                     tickwidth = 1,  # Ajustar grosor de las líneas de las etiquetas
                     tickcolor = '#013B63'),  # Aumentar el margen automáticamente
        legend = list(title = list(text = '', font = list(color = 'white', face = "bold")),  # Eliminar el título de la leyenda
                      font = list(color = 'white', face = "bold")),  # Texto de la leyenda en blanco y negrita
        plot_bgcolor = '#013B63',  # Fondo del gráfico en azul oscuro
        paper_bgcolor = '#013B63',  # Fondo del área total del gráfico en azul oscuro
        margin = list(l = 150, r = 50, t = 50, b = 50)  # Ajustar márgenes
      )
    
    # Mostrar el gráfico interactivo
    grafico5
    
  })
  
  # Mercado Laboral
  output$laborMarketBarPlot <- renderPlotly({
    
    resultados <- datos_filtrados()
    
    group_variables_result <- resultados$group_variables_result
    # Convertir a formato largo y calcular el total por variable y género
    labor_data_long <- group_variables_result %>%
      pivot_longer(
        cols = c("ocupados", "desocupados", "fuerza_trabajo"),
        names_to = "variable",
        values_to = "valor"
      ) %>%
      group_by(P3271, variable) %>%
      summarize(total = sum(valor), .groups = 'drop') %>%
      # Reordenar las variables en función del total
      mutate(variable = factor(variable, levels = unique(variable[order(-total, decreasing = TRUE)])))
    
    # Crear el gráfico con plotly
    grafico6 <- plot_ly(
      labor_data_long,
      x = ~total,  # El número de personas en el eje X
      y = ~variable,  # La variable en el eje Y
      color = ~P3271,  # Diferenciar por género
      colors = colors,  # Usar paleta de colores viridis
      type = 'bar', 
      orientation = 'h',  # Barras horizontales
      text = ~format(round(total, 0), big.mark = ","),  # Etiquetas con separadores de miles con coma
      insidetextfont = list(color = 'white', size = 14, family = "bold"),  # Etiquetas con separadores de miles con coma
      textposition = 'auto',  # Posicionar automáticamente las etiquetas
      textfont = list(color = 'white', size = 14, family = "bold"),  # Texto en blanco y negrita
      hoverinfo = 'text',  # Mostrar solo las etiquetas al pasar el cursor
      marker = list(line = list(color = 'black', width = 1))  # Borde de las barras
    )
    
    # Configurar el diseño del gráfico
    grafico6 <- grafico6 %>%
      layout(
        
        xaxis = list(
          title = 'Número de Personas',
          tickformat = ',',
          tickfont = list(size = 12, color = 'white', family = "bold"),  # Etiquetas en fuente Arial
          titlefont = list(size = 14, color ='white',  family = "bold")
        ),
        yaxis = list(
          tickfont = list(size = 12, color ='white',family = "bold"),  # Etiquetas del eje Y en fuente Arial
          titlefont = list(size = 14, color ='white', family = "bold"),
          ticklabelposition = "outside",  # Posicionar las etiquetas fuera del eje
          ticklen = 8,  # Ajustar longitud de las etiquetas
          tickwidth = 1,  # Ajustar grosor de las líneas de las etiquetas
          tickcolor = '#013B63'),  # Aumentar el margen automáticamente
        legend = list(title = list(text = '', font = list(color = 'white', face = "bold")),  # Eliminar el título de la leyenda
                      font = list(color = 'white', face = "bold")),  # Texto de la leyenda en blanco y negrita
        plot_bgcolor = '#013B63',  # Fondo del gráfico en azul oscuro
        paper_bgcolor = '#013B63',  # Fondo del área total del gráfico en azul oscuro
        margin = list(l = 150, r = 50, t = 50, b = 50)  # Ajustar márgenes
      )
    
    # Mostrar el gráfico interactivo
    grafico6
  })
  #tasa de desempleo
  
  output$laborMarketBarPlot2 <- renderPlotly({
    
    resultados <- datos_filtrados()
    
    group_variables_result <- resultados$group_variables_result
    
    # Convertir a formato largo y calcular el total por variable y género
    labor_data_long <- group_variables_result %>%
      pivot_longer(
        cols = c("tasa_desempleo", "tasa_ocupacion"),
        names_to = "variable",
        values_to = "valor"
      ) %>%
      group_by(P3271, variable) %>%
      summarize(total = sum(valor), .groups = 'drop') %>%
      # Reordenar las variables en función del total
      mutate(variable = factor(variable, levels = unique(variable[order(-total, decreasing = TRUE)])))
    
    # Crear el gráfico con plotly
    grafico7 <- plot_ly(
      labor_data_long,
      x = ~total,  # El número de personas en el eje X
      y = ~variable,  # La variable en el eje Y
      color = ~P3271,  # Diferenciar por género
      colors = colors,  # Usar paleta de colores viridis
      type = 'bar', 
      orientation = 'h',  # Barras horizontales
      text = ~paste0(format(round(total, 1), big.mark = ","), "%"),  # Etiquetas con separadores de miles con coma
      insidetextfont = list(color = 'white', size = 14, family = "bold"),  # Etiquetas con separadores de miles con coma
      textposition = 'auto',  # Posicionar automáticamente las etiquetas
      textfont = list(color = 'white', size = 14, family = "bold"),  # Texto en blanco y negrita
      hoverinfo = 'text',  # Mostrar solo las etiquetas al pasar el cursor
      marker = list(line = list(color = '#013B63', width = 3))  # Borde de las barras
    )
    
    # Configurar el diseño del gráfico
    grafico7 <- grafico7 %>%
      layout(
        
        xaxis = list(
          title = 'Porcentaje',
          tickformat = ',',
          tickfont = list(size = 12,color = 'white', family = "bold"),  # Etiquetas en fuente Arial
          titlefont = list(size = 14,color = 'white', family = "bold"),
          range = c(0, 80)  # Ajustar el rango del eje X al 100%
        ),
        yaxis = list(
          tickfont = list(size = 12, color = 'white', family = "bold"),  # Etiquetas del eje Y en fuente Arial
          titlefont = list(size = 14, color = 'white', family = "bold"),
          ticklabelposition = "outside",  # Posicionar las etiquetas fuera del eje
          ticklen = 8,  # Ajustar longitud de las etiquetas
          tickwidth = 1,  # Ajustar grosor de las líneas de las etiquetas
          tickcolor = '#013B63'),  # Aumentar el margen automáticamente
        legend = list(title = list(text = '', font = list(color = 'white', face = "bold")),  # Eliminar el título de la leyenda
                      font = list(color = 'white', face = "bold")),  # Texto de la leyenda en blanco y negrita
        plot_bgcolor = '#013B63',  # Fondo del gráfico en azul oscuro
        paper_bgcolor = '#013B63',  # Fondo del área total del gráfico en azul oscuro
        margin = list(l = 150, r = 50, t = 50, b = 50)  # Ajustar márgenes
      )
    
    # Mostrar el gráfico interactivo
    grafico7
  })
  
  output$jobTypeBarPlot <- renderPlotly({
    resultados <- datos_filtrados()
    job_type_result <- resultados$job_type_result
    
    grafico8 <- plot_ly(
      job_type_result,
      x = ~ personas,  # El número de personas en el eje X
      y = ~ reorder(as.factor(P6430), personas),  # El nivel educativo en el eje Y, reordenado
      type = 'bar', 
      orientation = 'h',  # Barras horizontales
      color = ~as.factor(P6430), 
      colors = colors,  # Usar paleta de colores viridis
      text = ~format(round(personas, 0), big.mark = ","),  # Etiquetas con separadores de miles con coma
      insidetextfont = list(color = 'white', size = 14, family = "bold"),  # Etiquetas con separadores de miles con coma
      textposition = 'auto',  # Posicionar automáticamente las etiquetas
      textfont = list(color = 'white', size = 14, family = "bold"),  # Texto en blanco y negrita
      hoverinfo = 'text',  # Mostrar solo las etiquetas al pasar el cursor
      marker = list(line = list(color = '#013B63', width = 3))  # Borde de las barras
    )
    
    
    # Configurar el diseño del gráfico
    grafico8 <- grafico8 %>%
      layout(
        title = list( 
          x = 0.5,  # Centrar el título
          font = list(size = 16, face = "bold")),
        xaxis = list(title = 'Número de Personas',
                     tickformat = ',',
                     tickfont = list(size = 12, color = 'white', family = "bold"),  # Etiquetas en negrita
                     titlefont = list(size = 14,color = 'white', family = "bold")),
        yaxis = list(
          tickfont = list(size = 12, color = 'white', family = "bold"),  # Etiquetas del eje Y en negrita
          titlefont = list(size = 14,color = 'white',  family = "bold"),
          ticklabelposition = "outside",  # Posicionar las etiquetas fuera del eje
          ticklen = 8,  # Ajustar longitud de las etiquetas
          tickwidth = 1,  # Ajustar grosor de las líneas de las etiquetas
          tickcolor = '#013B63'),  # Aumentar el margen automáticamente
        legend = list(title = list(text = '', font = list(color = 'white', face = "bold")),  # Eliminar el título de la leyenda
                      font = list(color = 'white', face = "bold")),  # Texto de la leyenda en blanco y negrita
        plot_bgcolor = '#013B63',  # Fondo del gráfico en azul oscuro
        paper_bgcolor = '#013B63',  # Fondo del área total del gráfico en azul oscuro
        margin = list(l = 150, r = 50, t = 50, b = 50)  # Ajustar márgenes
      )
    
    # Mostrar el gráfico interactivo
    grafico8
  })
  
  
  ####### Vivienda ######
  
  output$housingTypeBarPlot <- renderPlotly({
    resultados <- datos_filtrados()
    calcular_tipo_vivienda_result <- resultados$calcular_tipo_vivienda_result
    
    
    grafico9 <- plot_ly(
      calcular_tipo_vivienda_result,
      x = ~ personas,  # El número de personas en el eje X
      y = ~ reorder(as.factor(P5090), personas),  # El nivel educativo en el eje Y, reordenado
      type = 'bar', 
      orientation = 'h',  # Barras horizontales
      color = ~as.factor(P5090), 
      colors = colors,  # Usar paleta de colores viridis
      text = ~format(round(personas, 0), big.mark = ","),  # Etiquetas con separadores de miles con coma
      insidetextfont = list(color = 'white', size = 14, family = "bold"),  # Etiquetas con separadores de miles con coma
      textposition = 'auto',  # Posicionar automáticamente las etiquetas
      textfont = list(color = 'white', size = 14, family = "bold"),  # Texto en blanco y negrita
      hoverinfo = 'text',  # Mostrar solo las etiquetas al pasar el cursor
      marker = list(line = list(color = 'black', width = 1))  # Borde de las barras
    )
    
    # Configurar el diseño del gráfico
    grafico9 <- grafico9 %>%
      layout(
        title = list( 
          x = 0.5,  # Centrar el título
          font = list(size = 16, face = "bold")),
        xaxis = list(title = 'Número de Personas',
                     tickformat = ',',
                     tickfont = list(size = 12, color = 'white', family = "bold"),  # Etiquetas en negrita
                     titlefont = list(size = 14,color = 'white', family = "bold")),
        yaxis = list(
          tickfont = list(size = 12,color = 'white', family = "bold"),  # Etiquetas del eje Y en negrita
          titlefont = list(size = 14,color = 'white', family = "bold"),
          ticklabelposition = "outside",  # Posicionar las etiquetas fuera del eje
          ticklen = 8,  # Ajustar longitud de las etiquetas
          tickwidth = 1,  # Ajustar grosor de las líneas de las etiquetas
          tickcolor = '#013B63'),  # Aumentar el margen automáticamente
        legend = list(title = list(text = '', font = list(color = 'white', face = "bold")),  # Eliminar el título de la leyenda
                      font = list(color = 'white', face = "bold")),  # Texto de la leyenda en blanco y negrita
        plot_bgcolor = '#013B63',  # Fondo del gráfico en azul oscuro
        paper_bgcolor = '#013B63',  # Fondo del área total del gráfico en azul oscuro
        margin = list(l = 150, r = 50, t = 50, b = 50)  # Ajustar márgenes
      )
    
    # Mostrar el gráfico interactivo
    grafico9
  })
  
  output$homeConditionsBarPlot <- renderPlotly({
    # Obtener los resultados
    
    resultados <- datos_filtrados()
    
    home_conditions_result <- resultados$home_conditions_result
    
    # Convertir a formato largo y calcular el total por variable y género
    home_conditions_long <- 
      pivot_longer(home_conditions_result, 
                   cols = c("porcentaje_electri", "porcentaje_gas", 
                            "porcentaje_alcan", "porcentaje_acue"),
                   names_to = "variable",
                   values_to = "valor"
      ) %>%
      group_by(acceso, variable) %>%
      summarize(total = sum(valor), .groups = 'drop') %>%
      # Reordenar las variables en función del total
      mutate(variable = factor(variable, levels = unique(variable[order(-total, decreasing = TRUE)])))
    
    # Crear el gráfico con plotly
    grafico10 <- plot_ly(
      home_conditions_long,
      x = ~total,  # El número de personas en el eje X
      y = ~variable,  # La variable en el eje Y
      color = ~acceso,  # Diferenciar por género
      colors = colors,  # Usar paleta de colores viridis
      type = 'bar', 
      orientation = 'h',  # Barras horizontales
      text = ~paste0(format(round(total, 1), big.mark = ","), "%"),  # Etiquetas con separadores de miles con coma
      insidetextfont = list(color = 'white', size = 14, family = "bold"),  # Etiquetas con separadores de miles con coma
      textposition = 'auto',  # Posicionar automáticamente las etiquetas
      textfont = list(color = 'white', size = 14, family = "bold"),  # Texto en blanco y negrita
      hoverinfo = 'text',  # Mostrar solo las etiquetas al pasar el cursor
      marker = list(line = list(color = 'black', width = 1))  # Borde de las barras
    )
    
    # Configurar el diseño del gráfico
    grafico10 <- grafico10 %>%
      layout(
        xaxis = list(
          title = 'Porcentaje',
          tickformat = ',',
          tickfont = list(size = 12, color = 'white', family = "bold"),  # Etiquetas en fuente Arial
          titlefont = list(size = 14,color = 'white', family = "bold"),
          range = c(0, 100)  # Ajustar el rango del eje X al 100%
        ),
        yaxis = list(
          tickfont = list(size = 12,color = 'white', family = "bold"),  # Etiquetas del eje Y en fuente Arial
          titlefont = list(size = 14,color = 'white', family = "bold"),
          ticklabelposition = "outside",  # Posicionar las etiquetas fuera del eje
          ticklen = 8,  # Ajustar longitud de las etiquetas
          tickwidth = 1,  # Ajustar grosor de las líneas de las etiquetas
          tickcolor = '#013B63'),  # Aumentar el margen automáticamente
        legend = list(title = list(text = '', font = list(color = 'white', face = "bold")),  # Eliminar el título de la leyenda
                      font = list(color = 'white', face = "bold")),  # Texto de la leyenda en blanco y negrita
        plot_bgcolor = '#013B63',  # Fondo del gráfico en azul oscuro
        paper_bgcolor = '#013B63',  # Fondo del área total del gráfico en azul oscuro
        margin = list(l = 150, r = 50, t = 50, b = 50)  # Ajustar márgenes
      )
    
    # Mostrar el gráfico interactivo
    grafico10
    
  })
  
  # Acceso a Salud
  output$healthCoverageBarPlot <- renderPlotly({
    resultados <- datos_filtrados()
    calcular_acceso_salud_result <- resultados$calcular_acceso_salud_result
    
    grafico11 <- plot_ly(
      calcular_acceso_salud_result,
      x = ~ personas,  # El número de personas en el eje X
      y = ~ reorder(as.factor(P6090),personas),  # El nivel educativo en el eje Y, reordenado
      type = 'bar', 
      orientation = 'h',  # Barras horizontales
      color = ~as.factor(P6090), 
      colors = colors,  # Usar paleta de colores viridis
      text = ~format(round(personas, 0), big.mark = ","),  # Etiquetas con separadores de miles con coma
      insidetextfont = list(color = 'white', size = 14, family = "bold"),  # Etiquetas con separadores de miles con coma
      textposition = 'auto',  # Posicionar automáticamente las etiquetas
      textfont = list(color = 'white', size = 14, family = "bold"),  # Texto en blanco y negrita
      hoverinfo = 'text',  # Mostrar solo las etiquetas al pasar el cursor
      marker = list(line = list(color = 'black', width = 1))  # Borde de las barras
    )
    
    # Configurar el diseño del gráfico
    grafico11 <- grafico11 %>%
      layout(
        title = list( 
          x = 0.5,  # Centrar el título
          font = list(size = 16, face = "bold")),
        xaxis = list(title = 'Número de Personas',
                     tickformat = ',',
                     tickfont = list(size = 12, color = 'white', family = "bold"),  # Etiquetas en negrita
                     titlefont = list(size = 14,color = 'white', family = "bold")),
        yaxis = list(
          tickfont = list(size = 12, color = 'white', family = "bold"),  # Etiquetas del eje Y en negrita
          titlefont = list(size = 14,color = 'white',  family = "bold"),
          ticklabelposition = "outside",  # Posicionar las etiquetas fuera del eje
          ticklen = 8,  # Ajustar longitud de las etiquetas
          tickwidth = 1,  # Ajustar grosor de las líneas de las etiquetas
          tickcolor = '#013B63'),  # Aumentar el margen automáticamente
        legend = list(title = list(text = '', font = list(color = 'white', face = "bold")),  # Eliminar el título de la leyenda
                      font = list(color = 'white', face = "bold")),  # Texto de la leyenda en blanco y negrita
        plot_bgcolor = '#013B63',  # Fondo del gráfico en azul oscuro
        paper_bgcolor = '#013B63',  # Fondo del área total del gráfico en azul oscuro
        margin = list(l = 150, r = 50, t = 50, b = 50)  # Ajustar márgenes
      )
    
    # Mostrar el gráfico interactivo
    grafico11
    
  })
  
  # Tipo de Afiliación al Sistema de Salud
  output$healthAffiliationBarPlot <- renderPlotly({
    
    
    resultados <- datos_filtrados()
    
    calcular_afiliacion_salud_result <- resultados$calcular_afiliacion_salud_result
    
    grafico12 <- plot_ly(
      calcular_afiliacion_salud_result,
      x = ~ personas,  # El número de personas en el eje X
      y = ~ reorder(as.factor(P6100), personas),  # El nivel educativo en el eje Y, reordenado
      type = 'bar', 
      orientation = 'h',  # Barras horizontales
      color = ~as.factor(P6100), 
      colors = colors,  # Usar paleta de colores viridis
      text = ~format(round(personas, 0), big.mark = ","),  # Etiquetas con separadores de miles con coma
      insidetextfont = list(color = 'white', size = 14, family = "bold"),  # Etiquetas con separadores de miles con coma
      textposition = 'auto',  # Posicionar automáticamente las etiquetas
      textfont = list(color = 'white', size = 14, family = "bold"),  # Texto en blanco y negrita
      hoverinfo = 'text',  # Mostrar solo las etiquetas al pasar el cursor
      marker = list(line = list(color = 'black', width = 1))  # Borde de las barras
    )
    
    # Configurar el diseño del gráfico
    grafico12 <- grafico12 %>%
      layout(
        title = list( 
          x = 0.5,  # Centrar el título
          font = list(size = 16, face = "bold")),
        xaxis = list(title = 'Número de Personas',
                     tickformat = ',',
                     tickfont = list(size = 12,color = 'white', family = "bold"),  # Etiquetas en negrita
                     titlefont = list(size = 14,color = 'white', family = "bold")),
        yaxis = list(
          tickfont = list(size = 12, color = 'white', family = "bold"),  # Etiquetas del eje Y en negrita
          titlefont = list(size = 14,color = 'white', family = "bold"),
          ticklabelposition = "outside",  # Posicionar las etiquetas fuera del eje
          ticklen = 8,  # Ajustar longitud de las etiquetas
          tickwidth = 1,  # Ajustar grosor de las líneas de las etiquetas
          tickcolor = '#013B63'),  # Aumentar el margen automáticamente
        legend = list(title = list(text = '', font = list(color = 'white', face = "bold")),  # Eliminar el título de la leyenda
                      font = list(color = 'white', face = "bold")),  # Texto de la leyenda en blanco y negrita
        plot_bgcolor = '#013B63',  # Fondo del gráfico en azul oscuro
        paper_bgcolor = '#013B63',  # Fondo del área total del gráfico en azul oscuro
        margin = list(l = 150, r = 50, t = 50, b = 50)  # Ajustar márgenes
      )
    
    # Mostrar el gráfico interactivo
    grafico12
  })
  
  # Cargar los nombres de las hojas cuando el archivo es seleccionado
  observeEvent(input$level_selection, {
    if (input$level_selection == "Nacional") {
      sheets <- getSheetNames(nacional_file)  # Obtener nombres de hojas directamente
    } else {
      sheets <- getSheetNames(migrante_file)  # Obtener nombres de hojas directamente
    }
    
    # Mensaje de depuración para ver las hojas disponibles
    print(paste("Hojas disponibles en el archivo:", paste(sheets, collapse = ", ")))
    
    # Crea un selector de hojas dinámico
    output$sheet_selector <- renderUI({
      selectInput("sheet_choice", "Selecciona Variable:", choices = sheets)
    })
  })
  
  # Cargar los datos de la hoja seleccionada y mostrarlos en la tabla
  output$tableData <- DT::renderDT({  # Cambiado a DT::renderDT
    req(input$sheet_choice)  # Asegúrate de que haya una hoja seleccionada
    
    # Leer la hoja seleccionada y devolver los datos
    if (input$level_selection == "Nacional") {
      data <- read.xlsx(nacional_file, sheet = input$sheet_choice)
    } else {
      data <- read.xlsx(migrante_file, sheet = input$sheet_choice)
    }
    
    # Verifica que los datos se hayan cargado
    print(head(data))  # Mensaje de depuración para verificar los datos
    # Redondear todas las columnas numéricas a 2 decimales
    data <- data %>%
      mutate_if(is.numeric, round, 0)  # Redondea a 2 decimales
    
    datatable(data, options = list(pageLength = 10))
  })
  
  # Lógica para exportar los datos a CSV
  output$exportCSV <- downloadHandler(
    filename = function() {
      paste("datos_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      if (input$level_selection == "Nacional") {
        data <- read.xlsx(nacional_file, sheet = input$sheet_choice)
      } else {
        data <- read.xlsx(migrante_file, sheet = input$sheet_choice)
      }
      write.csv(data, file, row.names = FALSE)
    }
  )
  
  # Lógica para exportar los datos a Excel
  output$exportExcel <- downloadHandler(
    filename = function() {
      paste("datos_", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      if (input$level_selection == "Nacional") {
        data <- read.xlsx(nacional_file, sheet = input$sheet_choice)
      } else {
        data <- read.xlsx(migrante_file, sheet = input$sheet_choice)
      }
      write.xlsx(data, file)
    }
  )
}

# Correr la aplicación Shiny
shinyApp(ui, server)

