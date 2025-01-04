# Cargar paquetes necesarios
library(data.table)
library(scales)
data <- geih_completed()
geih <- data
#REEMPLAZAR CODIGO DE LOS DEPARTAMENTOS POR SU NOMBRE RESPECTIVO
geih[, DPTO := fcase(
  DPTO == "5", "Antioquia",
  DPTO == "8", "Atlántico",
  DPTO == "11", "Bogotá",
  DPTO == "13", "Bolívar",
  DPTO == "15", "Boyacá",
  DPTO == "17", "Caldas",
  DPTO == "18", "Caquetá",
  DPTO == "19", "Cauca",
  DPTO == "20", "Cesar",
  DPTO == "23", "Córdoba",
  DPTO == "25", "Cundinamarca",
  DPTO == "27", "Chocó",
  DPTO == "41", "Huila",
  DPTO == "44", "La Guajira",
  DPTO == "47", "Magdalena",
  DPTO == "50", "Meta",
  DPTO == "52", "Nariño",
  DPTO == "54", "Norte de Santander",
  DPTO == "63", "Quindío",
  DPTO == "66", "Risaralda",
  DPTO == "68", "Santander",
  DPTO == "70", "Sucre",
  DPTO == "73", "Tolima",
  DPTO == "76", "Valle del Cauca",
  DPTO == "81", "Arauca",
  DPTO == "85", "Casanare",
  DPTO == "86", "Putumayo",
  DPTO == "88", "San Andrés y Providencia",
  DPTO == "91", "Amazonas",
  DPTO == "94", "Guainía",
  DPTO == "95", "Guaviare",
  DPTO == "97", "Vaupés",
  DPTO == "99", "Vichada",
  default = NA_character_
)]

# 1. Estado civil

# Mapear valores de variables con códigos a descripciones legibles
# Ejemplo: Asignación de etiquetas legibles para "estado civil"
replacement_map_estado_civil <- c(
  "1" = "Pareja con menos de 2 años", "2" = "Pareja con mayor 2 años", "3" = "Casado(a)",
  "4" = "Separado o Divorciado", "5" = "Viudo(a)", "6" = "Soltero(a)"
)
geih[, P6070 := replacement_map_estado_civil[as.character(P6070)]]

# 2. Nivel de educación alcanzado

# Mapear los niveles educativos
education_map_short <- c(
  "1" = "Ninguno", "2" = "Preescolar", "3" = "Primaria",
  "4" = "Secundaria", "5" = "Media Académica", "6" = "Media Técnica",
  "7" = "Normalista", "8" = "Técnica Prof.", "9" = "Tecnológica",
  "10" = "Universitaria", "11" = "Especialización", "12" = "Maestría",
  "13" = "Doctorado", "99" = "No sabe"
)
geih[, P3042 := education_map_short[as.character(P3042)]]

# 3. Acceso a salud

# Mapear acceso a salud
acceso_health <- c(
  "9" = "No informa", "2" = "No", "1" = "Sí"
)
geih[, P6090 := acceso_health[as.character(P6090)]]

# 4. Afiliación al sistema de salud
replacement_map_afiliacion <- c(
  "1" = "Contributivo", "2" = "Especial", "3" = "Subsidiado", "9" = "No informa"
)

geih[, P6100 := replacement_map_afiliacion[as.character(P6100)]]

# 5. Tipo de trabajo

# Mapear tipos de trabajo
ocupacion_map_short <- c(
  "1" = "Empleado de empresa particular", "2" = "Empleado del gobierno", "3" = "Empleado doméstico",
  "4" = "Cuenta propia", "5" = "Empleador", "6" = "Trabajador familiar sin pago",
  "7" = "Trabajador sin pago", "8" = "Jornalero", "9" = "Otro"
)
geih[, P6430 := ocupacion_map_short[as.character(P6430)]]

# 6. La vivienda ocupada por este hogar es:

# Mapear tipos de vivienda
propiedad_map_short <- c(
  "1" = "Propia, pagada", "2" = "Propia, la están pagando",
  "3" = "En arriendo/subarriendo", "4" = "En usufructo", "5" = "Posesión sin título",
  "6" = "Propiedad colectiva", "7" = "Otra"
)
geih[, P5090 := propiedad_map_short[as.character(P5090)]]

# 7. Variables de Motivos de Migración

# Motivos de migración
replacement_map_p3386 <- c(
  "1" = "Trabajo", "2" = "Estudio", "3" = "Salud", "4" = "Conflicto armado",
  "5" = "Violencia", "6" = "Desastres", "7" = "Nuevo hogar",
  "8" = "Acompañar hogar", "9" = "Motivos culturales", "10" = "Vivienda propia",
  "12" = "Otro"
)
geih[, P3386 := replacement_map_p3386[as.character(P3386)]]

# 8. Sexo de la población
replacement_sexo <- c(
  "1"= "Hombres", "2" = "Mujeres"
)
geih[, P3271 := replacement_sexo[as.character(P3271)]]

# Condiciones del Hogar 

replacement_con <- c(
  "1" = "si",  "2" = "no"
)
geih[, P4030S1 := replacement_con[as.character(P4030S1)] ]
geih[, P4030S2 := replacement_con[as.character(P4030S2)] ]
geih[, P4030S3 := replacement_con[as.character(P4030S3)] ]
geih[, P4030S5 := replacement_con[as.character(P4030S5)] ]

# Definir las variables de interés para el análisis de migrantes
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

###################### DATA.TABLE, FILTRADA PARA LOS VENEZOLANOS #############
geih1 <- geih[, migrant_variables, with = FALSE]

# Guardar archivo CSV con delimitador tabulador (\t)
fwrite(geih1, file = "geih_complete.csv", sep = "\t")

# Filtrar los datos de migrantes que cumplan ciertas condiciones
Venezuelan_Migrants <- geih[P3373S3 == 862 & P3374S1 == 862, migrant_variables, with = FALSE]
