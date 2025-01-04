library(data.table)

# Pirámide poblacional a nivel nacional
pyramid_population_nacional <- function(geih) {
  geih[, age_group := cut(
    P6040,
    breaks = c(0, 4, 9, 14, 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79, 84, Inf),
    labels = c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85+"),
    right = FALSE
  )]
  
  pyramid_nacional <- geih[, .(personas = sum(FEX_C18, na.rm = TRUE) / 7), by = .(age_group, P3271)]
  
  pyramid_nacional_wide <- as.data.table(dcast(pyramid_nacional, age_group ~ P3271, value.var = "personas", fun.aggregate = sum, fill = 0))
  pyramid_nacional_wide[, total_poblacion := sum(abs(Hombres)) + sum(Mujeres)]
  pyramid_nacional_wide[, Hombres_pct := (abs(Hombres) / total_poblacion) * 100]
  pyramid_nacional_wide[, Mujeres_pct := (Mujeres / total_poblacion) * 100]
  pyramid_nacional_wide[, Mujeres_pct := -Mujeres_pct] # Invertir para pirámide
  
  return(pyramid_nacional_wide)
}


# Estado civil a nivel nacional
marital_status_nacional <- function(geih) {
  geih[!is.na(P6070) & P6070 != "", .(personas = sum(FEX_C18, na.rm = TRUE) / 7), by = .(P6070)]
}


# Sexo de la población a nivel nacional
sex_nacional <- function(geih) {
  sex <- geih[, .(personas = sum(FEX_C18, na.rm = TRUE) / 7), by = .(P3271)]
  sex[, Percentage := (personas / sum(personas)) * 100]
  
  return(sex)
}



########## Educación ########

# Nivel de educación alcanzado a nivel nacional
education_achieved_nacional <- function(geih) {
  geih[!is.na(P3042) & P3042 != "", .(personas = sum(FEX_C18, na.rm = TRUE) / 7), by = .(P3042)]
}

# Ingreso por nivel educativo a nivel nacional
income_by_education_nacional <- function(geih) {
  geih[!is.na(P3042) & !is.na(INGLABO), .(ingreso = mean(INGLABO, na.rm = TRUE)), by = .(P3042)]
}

######### Mercado Laboral #####

# Función para calcular variables laborales a nivel nacional
group_variables_nacional <- function(geih) {
  factor_expansion <- geih[!is.na(P3271), .(factor_expansion = sum(FEX_C18) / 7), by = P3271]
  
  ocupados <- geih[!is.na(P3271), .(ocupados = sum(OCI * FEX_C18, na.rm = TRUE) / 7), by = P3271]
  
  desocupados <- geih[!is.na(P3271), .(desocupados = sum(DSI * FEX_C18, na.rm = TRUE) / 7), by = P3271]
  
  poblacion_edad_trabajar <- geih[P6040 >= 15, .(poblacion_edad_trabajar = sum(FEX_C18, na.rm = TRUE) / 7), by = P3271]
  
  combined <- factor_expansion[ocupados, on = "P3271"][desocupados, on = "P3271"][poblacion_edad_trabajar, on = "P3271"]
  
  combined[, fuerza_trabajo := (ocupados + desocupados)]
  combined[, tasa_desempleo := (desocupados / fuerza_trabajo) * 100]
  combined[, tasa_ocupacion := (ocupados / poblacion_edad_trabajar) * 100]
  
  return(combined)
}

# Tipo de trabajo a nivel nacional
job_type_nacional <- function(geih) {
  geih[!is.na(P6430) & P6430 != "", .(personas = sum(FEX_C18, na.rm = TRUE) / 7), by = .(P6430)]
}

######## Vivienda #####

# Tipo de vivienda a nivel nacional
calcular_tipo_vivienda_nacional <- function(geih) {
  housing_type_nacional <- geih[!is.na(P5090), .(personas = sum(FEX_C18, na.rm = TRUE) / 7), by = .(P5090)]
  return(housing_type_nacional)
}

# Condiciones del hogar por servicios públicos a nivel nacional

# Energía eléctrica a nivel nacional
calcular_condiciones_hogar_nacional  <- function(geih) {
  electrical_energy_nacional <- geih[, .(energia_electrica = sum(PT * FEX_C18, na.rm = TRUE) / 7), by = .(P4030S1)]
  electrical_energy_nacional[, total_energia := sum(energia_electrica)]
  electrical_energy_nacional[, porcentaje_electri := round(energia_electrica / total_energia * 100, 2)]
  setnames(electrical_energy_nacional, "P4030S1", "acceso")
  
  natural_gas_nacional <- geih[!is.na(P4030S2), .(gas_natural = sum(PT * FEX_C18, na.rm = TRUE) / 7), by = .(P4030S2)]
  natural_gas_nacional[, total_gas := sum(gas_natural)]
  natural_gas_nacional[, porcentaje_gas := round(gas_natural / total_gas * 100, 2)]
  setnames(natural_gas_nacional, "P4030S2", "acceso")
  
  sewer_nacional <- geih[!is.na(P4030S3), .(alcantarillado = sum(PT * FEX_C18, na.rm = TRUE) / 7), by = .(P4030S3)]
  sewer_nacional[, total_alc := sum(alcantarillado)]
  sewer_nacional[, porcentaje_alcan := round(alcantarillado / total_alc * 100, 2)]
  setnames(sewer_nacional, "P4030S3", "acceso")
  
  aqueduct_nacional <- geih[!is.na(P4030S5), .(acueducto = sum(PT * FEX_C18, na.rm = TRUE) / 7), by = .(P4030S5)]
  aqueduct_nacional[, total_acue := sum(acueducto)]
  aqueduct_nacional[, porcentaje_acue := round(acueducto / total_acue * 100, 2)]
  setnames(aqueduct_nacional, "P4030S5", "acceso")
  
  combined <- electrical_energy_nacional[natural_gas_nacional, on = "acceso"][sewer_nacional, on = "acceso"][aqueduct_nacional, on ="acceso"]
  
  return(combined)
}

############# Salud ########

# Acceso a salud a nivel nacional
calcular_acceso_salud_nacional <- function(geih) {
  health_coverage_nacional <- geih[, .(personas = sum(FEX_C18, na.rm = TRUE) / 7), by = .(P6090)]
  return(health_coverage_nacional)
}

# Afiliación al sistema de salud a nivel nacional
calcular_afiliacion_salud_nacional <- function(geih) {
  health_affiliation_nacional <- geih[!is.na(P6100)  & P6100 != "" , .(personas = sum(FEX_C18, na.rm = TRUE) / 7), by = .(P6100)]
  return(health_affiliation_nacional)
}
