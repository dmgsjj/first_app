# Cargar la librería openxlsx
library(openxlsx)

# Crear un workbook
wb <- createWorkbook()

# Agregar una hoja para cada data frame y escribir los datos

# 1. Pirámide poblacional por departamento
addWorksheet(wb, "Pyramid Dept")
writeData(wb, "Pyramid Dept", pyramid_dept_wide)

# 2. Estado civil por departamento
addWorksheet(wb, "Marital Status Dept")
writeData(wb, "Marital Status Dept", marital_status_dept)

# 3. Sexo de la población por departamento
addWorksheet(wb, "Sex Dept")
writeData(wb, "Sex Dept", sex_dept)

# 4. Nivel de educación alcanzado por departamento
addWorksheet(wb, "Education Achieved Dept")
writeData(wb, "Education Achieved Dept", education_achieved_dept)

# 5. Ingreso por nivel educativo por departamento
addWorksheet(wb, "Income by Education Dept")
writeData(wb, "Income by Education Dept", income_by_education_dept)

# 6. Variables laborales por departamento
addWorksheet(wb, "Labor Variables Dept")
writeData(wb, "Labor Variables Dept", table_variables_dep)

# 7. Tipo de trabajo por departamento
addWorksheet(wb, "Job Type Dept")
writeData(wb, "Job Type Dept", t_job_dep)

# 8. Tipo de vivienda por departamento
addWorksheet(wb, "Housing Type Dept")
writeData(wb, "Housing Type Dept", housing_type_dept)

# 9. Condiciones del hogar (energía eléctrica, gas, alcantarillado, acueducto)
addWorksheet(wb, "Electrical Energy")
writeData(wb, "Electrical Energy", electrical_energy)

addWorksheet(wb, "Natural Gas")
writeData(wb, "Natural Gas", natural_gas)

addWorksheet(wb, "Sewer")
writeData(wb, "Sewer", sewer)

addWorksheet(wb, "Aqueduct")
writeData(wb, "Aqueduct", aqueduct)

# 10. Acceso a salud por departamento
addWorksheet(wb, "Health Coverage Dept")
writeData(wb, "Health Coverage Dept", health_coverage_dep)

# 11. Afiliación al sistema de salud por departamento
addWorksheet(wb, "Health Affiliation Dept")
writeData(wb, "Health Affiliation Dept", health_affiliation_dept)

# Guardar el archivo Excel
saveWorkbook(wb, "C:/caracterizacion_poblacional/caracterizacion_poblacional.xlsx", overwrite = TRUE)






############# ARCHICOS DE MIGRACIÓN #######
install.packages("writexl")
library(writexl)

# Define los data.frames que quieres guardar
data_frames <- list(
  "Pyramid_Department" = pyramid_dept_wide_m,
  "Marital_Status_Department" = marital_status_dept_m,
  "Sex_Department" = sex_dept_m,
  "Education_Achieved_Department" = education_achieved_dept_m,
  "Income_By_Education_Department" = income_by_education_dept_m,
  "Labor_Market" = table_variables_dep_m,
  "Job_Type_Department" = t_job_dep_m,
  "Housing_Type_Department" = housing_type_dept_m,
  "Electrical_Energy_Department" = electrical_energy_m,
  "Natural_Gas_Department" = natural_gas_m,
  "Sewer_Department" = sewer_m,
  "Aqueduct_Department" = aqueduct_m,
  "Health_Coverage_Department" = health_coverage_dep_m,
  "Health_Affiliation_Department" = health_affiliation_dept_m
)

# Define la ruta del archivo
file_path <- "C:/caracterizacion_poblacional/data_frames.xlsx"

# Guarda todos los data.frames en el archivo Excel
write_xlsx(data_frames, path = file_path)
