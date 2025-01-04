library(data.table)


# Pirámide de población por departamento
piramide_poblaciona_dep <- function(geih, department_selection) {
  geih[, age_group := cut(
    P6040,
    breaks = c(0, 4, 9, 14, 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79, 84, Inf),
    labels = c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", 
               "60-64", "65-69", "70-74", "75-79", "80-84", "85+"),
    right = FALSE
  )]
  
  # Calcular población por grupo de edad y sexo
  pyramid_dept_m <- geih[DPTO == department_selection, .(personas = sum(FEX_C18, na.rm = TRUE) / 7), by = .(DPTO, age_group, P3271)]
  print(pyramid_dept_m)
  pyramid_dept_wide_m <- as.data.table(dcast(pyramid_dept_m, DPTO + age_group ~ P3271, value.var = "personas", fill = 0))
  pyramid_dept_wide_m[, total_poblacion := sum(abs(Hombres)) + sum(Mujeres)]
  pyramid_dept_wide_m[, Hombres_pct := (abs(Hombres) / total_poblacion) * 100]
  pyramid_dept_wide_m[, Mujeres_pct := (Mujeres / total_poblacion) * 100]
  pyramid_dept_wide_m[, Mujeres_pct := -Mujeres_pct]
  
  return(pyramid_dept_wide_m)
}

# Estado civil por departamento 
marital_status_dep <- function(geih, department_selection) {
  geih[DPTO == department_selection & !is.na(P6070) & P6070 != "", .(personas = sum(FEX_C18, na.rm = TRUE) / 7), by = .(DPTO, P6070)]
}


# Sexo por departamento
sexo_departamento_dep <- function(geih, department_selection) {
  sex_dept_m <- geih[DPTO == department_selection, .(personas = sum(FEX_C18, na.rm = TRUE) / 7), by = .(DPTO, P3271)]
  sex_dept_m[, Percentage := (personas / sum(personas)) * 100]
  return(sex_dept_m)
}

# 2. EDUCACIÓN

# Nivel de educación alcanzado por departamento
nivel_educacion_dep <- function(geih, department_selection) {
  education_achieved_dept_m <- geih[DPTO == department_selection & !is.na(P3042) & P3042 != "", .(personas = sum(FEX_C18, na.rm = TRUE) / 7), by = .(DPTO, P3042)]
  return(education_achieved_dept_m)
}

# Nivel de ingreso por nivel educativo
ingreso_por_educacion_dep <- function(geih, department_selection) {
  income_by_education_dept_m <- geih[DPTO == department_selection & !is.na(P3042) & !is.na(INGLABO), .(ingreso = mean(INGLABO, na.rm = TRUE)), by = .(DPTO, P3042)]
  return(income_by_education_dept_m)
}

# 3. MERCADO LABORAL

# Función para calcular estadísticas laborales
estadisticas_laborales_dep <- function(geih, department_selection) {
  factor_expansion <- geih[DPTO == department_selection & !is.na(P3271), .(factor_expansion = sum(FEX_C18) / 7), by = .(DPTO, P3271)]
  ocupados <- geih[DPTO == department_selection & !is.na(P3271), .(ocupados = sum(OCI * FEX_C18, na.rm = TRUE) / 7), by = .(DPTO, P3271)]
  desocupados <- geih[DPTO == department_selection & !is.na(P3271), .(desocupados = sum(DSI * FEX_C18, na.rm = TRUE) / 7), by = .(DPTO, P3271)]
  poblacion_edad_trabajar <- geih[DPTO == department_selection & P6040 >= 15, .(poblacion_edad_trabajar = sum(FEX_C18, na.rm = TRUE) / 7), by = .(DPTO, P3271)]
  
  combined <- factor_expansion[ocupados, on = c("DPTO", "P3271")][desocupados, on = c("DPTO", "P3271")][poblacion_edad_trabajar, on = c("DPTO", "P3271")]
  combined[, fuerza_trabajo := (ocupados + desocupados)]
  combined[, tasa_desempleo := (desocupados / fuerza_trabajo) * 100]
  combined[, tasa_ocupacion := (ocupados / poblacion_edad_trabajar) * 100]
  
  return(combined)
}

# Tipo de trabajo por departamento
tipo_trabajo_dep <- function(geih, department_selection) {
  t_job_dep_m <- geih[DPTO == department_selection & !is.na(P6430) & P6430 != "", .(personas = sum(FEX_C18, na.rm = TRUE) / 7), by = .(DPTO, P6430)]
  return(t_job_dep_m)
}

# 4. VIVIENDA

# Tipo de vivienda por departamento
tipo_vivienda_dep <- function(geih, department_selection) {
  housing_type_dept_m <- geih[DPTO == department_selection & !is.na(P5090) & P5090 != "", .(personas = sum(FEX_C18, na.rm = TRUE) / 7), by = .(DPTO, P5090)]
  return(housing_type_dept_m)
}


# Condiciones del hogar por servicios públicos a nivel nacional

# Energía eléctrica a nivel nacional
calcular_condiciones_hogar_dep <- function(geih, department_selection) {
  electrical_energy_nacional <- geih[DPTO == department_selection & !is.na(P4030S1), .(energia_electrica = sum(PT * FEX_C18, na.rm = TRUE) / 7), by = .(DPTO, P4030S1)]
  electrical_energy_nacional[, total_energia := sum(energia_electrica)]
  electrical_energy_nacional[, porcentaje_electri := round(energia_electrica / total_energia * 100, 2)]
  setnames(electrical_energy_nacional, "P4030S1", "acceso")
  
  natural_gas_nacional <- geih[DPTO == department_selection & !is.na(P4030S2), .(gas_natural = sum(PT * FEX_C18, na.rm = TRUE) / 7), by = .(DPTO, P4030S2)]
  natural_gas_nacional[, total_gas := sum(gas_natural)]
  natural_gas_nacional[, porcentaje_gas := round(gas_natural / total_gas * 100, 2)]
  setnames(natural_gas_nacional, "P4030S2", "acceso")
  
  sewer_nacional <- geih[DPTO == department_selection & !is.na(P4030S3), .(alcantarillado = sum(PT * FEX_C18, na.rm = TRUE) / 7), by = .(DPTO, P4030S3)]
  sewer_nacional[, total_alc := sum(alcantarillado)]
  sewer_nacional[, porcentaje_alcan := round(alcantarillado / total_alc * 100, 2)]
  setnames(sewer_nacional, "P4030S3", "acceso")
  
  aqueduct_nacional <- geih[DPTO == department_selection & !is.na(P4030S5), .(acueducto = sum(PT * FEX_C18, na.rm = TRUE) / 7), by = .(DPTO, P4030S5)]
  aqueduct_nacional[, total_acue := sum(acueducto)]
  aqueduct_nacional[, porcentaje_acue := round(acueducto / total_acue * 100, 2)]
  setnames(aqueduct_nacional, "P4030S5", "acceso")
  
  combined <- electrical_energy_nacional[natural_gas_nacional, on = c("acceso", "DPTO")][sewer_nacional, on = c("acceso", "DPTO")][aqueduct_nacional, on = c("acceso", "DPTO")]
  
  return(combined)
}

# 5. SALUD

# Cobertura de salud por departamento
cobertura_salud_dep <- function(geih, department_selection) {
  health_coverage_dep_m <- geih[DPTO == department_selection, .(personas = sum(FEX_C18, na.rm = TRUE) / 7), by = .(DPTO, P6090)]
  return(health_coverage_dep_m)
}

# Afiliación al sistema de salud
afiliacion_salud_dep <- function(geih, department_selection) {
  health_affiliation_dept_m <- geih[DPTO == department_selection & !is.na(P6100) & P6100 != "", .(personas = sum(FEX_C18, na.rm = TRUE) / 7), by = .(DPTO, P6100)]
  return(health_affiliation_dept_m)
}




