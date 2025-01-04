# Paso 1: Instalar rsconnect (solo la primera vez)
install.packages('rsconnect')

# Paso 2: Autorizar la cuenta
rsconnect::setAccountInfo(name='jsidte-daniel-molina',
			  token='25F013590629D54B9F68FF42C75B831A',
			  secret='7GBGlp8hYGmcYeUax9h+Dw1qJvB8jPpYon48Dld4')

# Paso 3: Desplegar la aplicación
library(rsconnect)
rsconnect::deployApp('C:/Users/ESTUDIANTE/OneDrive - Universidad del Magdalena/proyectos_con_R/shiny-app')

