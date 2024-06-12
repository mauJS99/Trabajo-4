## Procesamiento de datos ##
##    José Norambuena     ##
##       Trabajo 2        ##
############################


#### 1. Instalación de paquetes ####
install.packages("pacman")

# 1.1. Cargar paquetes
pacman::p_load(dplyr, sjmisc, car, sjlabelled, stargazer, haven)

#### 2. Cargar base de datos ####
# 2.1 Pasos previos 
rm(list=ls())       # borrar todos los objetos en el espacio de trabajo
options(scipen=999) # valores sin notación científica

# 2.2 Cargar la base de datos desde internet
CASEN1 <- read_sav("input/CASEN.sav")

# 2.3 Filtrar por edad (mayores de 18 años)
CASEN <- CASEN1 %>% dplyr::filter(edad>=18)
CASEN1$e8
#### 3. Selección de variables ####
# r3 --> pueblos originarios
#e6a --> ¿Cuál es el nivel educacional al que asiste o el más alto al cual asistió?
#e8 --> ¿En qué tipo de institución realizó su educación superior?
#e16 --> En el año escolar 2022, ¿paga por la carrera que estudia?
#e18 --> En el año escolar 2022, ¿recibe créditos universitarios?
#o1 --> La semana pasada, ¿trabajó al menos 1 hora, sin considerar quehaceres hogar?
#y1 --> Ingresos
#sexo

# 3.1 Seleccionar variable de interés y crear nueva base de datos
CASEN <- CASEN %>% select(r3, # Pertenencia a pueblos originarios
                          e6a, # Nivel educativo más alto asistiendo o asistido
                          e8, # Tipo de institución de Educación Superior
                          e16,# Pago por la carrera que estudia
                          e18,# Créditos universitarios recibidos
                          o1,# Trabajo
                          y1) # Ingresos

# Comprobar
names(CASEN)

# 3.2 Atributo de las variables
sjlabelled::get_label(CASEN)

#### 4. Procesamiento de variables ####
## 4.1 Renombrar y re-etiquetar variables ##
CASEN <- CASEN %>% rename("pueb_or"=r3, # Pertenencia a pueblos originarios
                          "educ"=e6a, # Nivel educativo más alto alcanzado
                          "est_educ_sup"=e8, # Tipo de establecimiento de educación superior
                          "gratuidad" = e16, # Posee gratuidad (estudiante 2022)
                          "cred_univ" = e18, #Créditos universitarios recibidos
                          "trabajo" = o1, #Trabajo
                          "ingresos" = y1) #Ingresos
# Comprobar cambio de nombre
names(CASEN)

## 4.1.1 Re-etiquetar variables 
# Pueblos originarios
CASEN$pueb_or <- set_label(x = CASEN$pueb_or,label = "Pertenencia a pueblos originarios")
get_label(CASEN$pueb_or)
# Nivel educativo
CASEN$educ <- set_label(x = CASEN$educ,label = "Máximo nivel educativo alcanzado")
get_label(CASEN$educ)
# Tipo de institución de educación superior
CASEN$est_educ_sup <- set_label(x = CASEN$est_educ_sup,label = "Tipo de establecimiento de educación superior")
get_label(CASEN$est_educ_sup)
# Gratuidad
CASEN$gratuidad <- set_label(x = CASEN$gratuidad,label = "Posee gratuidad (estudiante 2022)")
get_label(CASEN$gratuidad)
# Crédito universitario
CASEN$cred_univ <- set_label(x = CASEN$cred_univ,label = "Posee algún crédito (estudiante 2022)")
get_label(CASEN$cred_univ)
#Trabajo
CASEN$trabajo <- set_label(x = CASEN$trabajo,label = "Trabajo")
get_label(CASEN$trabajo)
#Ingresos
CASEN$ingresos <- set_label(x = CASEN$ingresos,label = "Ingresos del mes pasado")
get_label(CASEN$ingresos)

## 4.2 Recodificación de casos perdidos a NA 
# Pertenencia a pueblos originarios
frq(CASEN$pueb_or) #No hay que recodificar
# Nivel educativo
frq(CASEN$educ) #No hay que recodificar
# Tipo de institucion de educación superior
frq(CASEN$est_educ_sup)
CASEN$est_educ_sup <- recode(CASEN$est_educ_sup, "c(-99,-88)=NA")
# Posee gratuidad
frq(CASEN$gratuidad)
CASEN$gratuidad <- recode(CASEN$gratuidad, "c(-88)=NA")
# Crédito universitario
frq(CASEN$cred_univ) #No hay que recodificar
# Trabajo
frq(CASEN$trabajo) #No hay que recodificar
# Ingresos
frq(CASEN$ingresos)
CASEN$ingresos <- recode(CASEN$ingresos, "c(-88)=NA")

## 4.3 Recodificación de casos perdidos para toda la base
CASEN <- CASEN %>% set_na(., na = c(-66, -88, -99))

## 4.4 Reordenar categorías de algunas variables 
# Pertenencia a Pueblos Originarios
frq(CASEN$pueb_or)
CASEN$pueb_or <- recode(CASEN$pueb_or, "1:10=1; 11=0")
CASEN$pueb_or <- set_labels(CASEN$pueb_or,
                            labels=c( "Sí"=1,
                                      "No"=0))

CASEN$pueb_or <- factor(CASEN$pueb_or,
             labels = c("No pertenece a algún pueblo originario", "Sí pertenece a algún pueblo originario"),
             levels = c(0,1))


# Nivel educativo
frq(CASEN$educ)
CASEN$educ <- recode(CASEN$educ, "1=0; 2:5=NA; 6:7=1; 8:11=2; 12:13=3; 14:15=4")
CASEN$educ <- set_labels(CASEN$educ,
                         labels=c( "No asistió"=0,
                                   "Básica"=1,
                                   "Media"=2,
                                   "Superior"=3,
                                   "Postgrados"=4))

CASEN$educ <- factor(CASEN$educ,
          labels = c("No asístió", "Básica", "Media", "Superior", "Postgrados"),
          levels = c(0,1,2,3,4))

# Tipo de institución de educación superior
frq(CASEN$est_educ_sup)
CASEN$est_educ_sup <- recode(CASEN$est_educ_sup, "NA=0; 1=1; 2=2; 3:5=3; 6=4; 7=3")
CASEN$est_educ_sup <- set_labels(CASEN$est_educ_sup,
                         labels=c( "No asistió"=0,
                                   "Centro de Formación Técnica"=1,
                                   "Instituto Profesional"=2,
                                   "Universidad"=3,
                                   "Establecimiento de las Fuerzas Armadas"=4))

CASEN$est_educ_sup <- factor(CASEN$est_educ_sup,
                  labels = c("No asístió", "C.F.T.", "I.P.", "Universidad", "Establecimiento de las Fuerzas Armadas"),
                  levels = c(0,1,2,3,4))

# Estudiantes en el año 2022 que cuentan con gratuidad
frq(CASEN$gratuidad)
CASEN$gratuidad <- recode(CASEN$gratuidad, "1=1; 2:3=0")
CASEN$gratuidad <- set_labels(CASEN$gratuidad,
                   labels = c("Sí" = 1,
                              "No" = 0))

CASEN$gratuidad <- factor(CASEN$gratuidad,
               labels = c("No posee gratuidad", "Sí posee gratuidad"),
               levels = c(0,1))

# Estudiantes en el año 2022 que cuentan con algún crédito
frq(CASEN$cred_univ)
CASEN$cred_univ <- recode(CASEN$cred_univ, "1=1; 3=2; 4=3; 6=3; 2=4; 5=4; 7=4; 8=0")
CASEN$cred_univ <- set_labels(CASEN$cred_univ,
                    labels= c("Crédito universitario o fondo solidario" = 1,
                              "Crédito con aval del Estado" = 2,
                              "Crédito de institución" = 3,
                              "Otro crédito" = 4,
                              "No cuenta con crédito" = 0))

CASEN$cred_univ <- factor(CASEN$cred_univ,
               labels = c("No cuenta con crédito", "Crédito universitario o fondo solidario", "Crédito con aval del Estado", "Crédito de institución", "Otro crédito"),
               levels = c(0,1,2,3,4))
# Trabaja
frq(CASEN$trabajo)
CASEN$trabajo <- recode(CASEN$trabajo, "1=1; 2=0")
CASEN$trabajo <- set_labels(CASEN$trabajo,
                 labels = c("Sí" = 1,
                            "No" = 0))

CASEN$trabajo <- factor(CASEN$trabajo,
              labels = c("No tiene trabajo", "Sí tiene trabajo"),
              levels = c(0,1))

# Ingresos
CASEN$ingresos <- recode(CASEN$ingresos, "0:469999=1; 470000:999999=2; 1000000:1999999=3;2000000:25000000=4")
CASEN$ingresos <- set_labels(CASEN$ingresos,
                  labels = c("Menos del sueldo mínimo" = 1,
                             "Sueldo promedio" = 2,
                             "Sueldo alto" = 3,
                             "Sueldo muy alto" = 4))

CASEN$ingresos <- factor(CASEN$ingresos,
              labels = c("Menos del sueldo mínimo", "Sueldo promedio", "Sueldo alto", "Sueldo muy alto"),
              levels = c(1,2,3,4))
frq(CASEN$ingresos)
#### 5. Generar base de datos procesada para el análisis ####
## 5.1 Reformatear objeto (CASEN) a BBDD
CASEN <-as.data.frame(CASEN)

## 5.2 Generar tabla descriptiva general
stargazer(CASEN, type="text")

view(dfSummary(CASEN, headings=FALSE))

## 5.3 Guardar base de datos en una ruta particular
save(CASEN, file ="input/CASEN.rdata")

