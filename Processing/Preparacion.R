#### SCRIPT PREPARACION
### Preparación trabajo "pobreza multidimensional rural 8va region"

# 0. Ajustes iniciales:
rm()
options(scipen=999)

setwd("D:/UNIVERSIDARKSSS/SEXTO AÑO/PROF.R/Trabajo-4")

# 1. Paquetes:
install.packages("pacman")
pacman::p_load(haven,
               sjlabelled,
               dplyr, #Manipulacion de datos
               stargazer, #Tablas
               sjmisc, # Tablas
               summarytools, # Tablas
               kableExtra, #Tablas
               sjPlot, #Tablas y gráficos
               corrplot, # Correlaciones
               sessioninfo, # Información de la sesión de trabajo
               ggplot2,  # Para la mayoría de los gráficos
               mice,   #Para imputar NA's
               fastDummies, 
               ggeffects,
               texreg)


# 2. Cargamos Base de datos
CASEN2022<- read_dta("input/Casen_2022.dta") #para bases de datos de STATA
summary(CASEN2022)

load(file = "Input/Proc_data.RData")

# 3. Seleccionamos y manipulamos variables
Proc_data <- CASEN2022 %>% 
  select(edad,
         sexo,
         nse,
         region,
         area,
         e6a, #Educacion
         oficio1_08, #Trabajo u oficio 
         s13, #Salud
         r6, #Red de apoyo
         v1) #Tipo de vivienda 

# 3.1 Exploramos las variables
summary(Proc_data)
names(Proc_data)
sjlabelled::get_label(Proc_data)

# 3.2 Filtramos para trabajar con la 8va región
Proc_data <- Proc_data %>%
  filter(region==8, #Region del Biobio.
         area==2) #Area rural

colSums(is.na(Proc_data))

# 3.3 Tratamos los casos perdidos
Proc_data <- Proc_data %>% set_na(., na = c(-88, -99, -66))
Proc_data <- na.omit(Proc_data)

###### CON LOS FILTROS Y EL TRATAMIENTO DE LOS CASOS PERDIDOS, QUEDAMOS EN 1419 OBSERVACIONES Y 10 VARIABLES POR EL MOMENTO

# 3.4 Renombramos las variables
Proc_data <- Proc_data %>% rename("Nivel_educacional"=e6a, 
                                  "Trabajo_oficio"=oficio1_08, 
                                  "Sistema_salud"=s13,
                                  "Red_apoyo"=r6,
                                  "Tipo_vivienda"=v1)
sjlabelled::get_label(Proc_data)

# 3.5 Recodificación de variables:
### Nivel Socioeconómico:
Proc_data$nse <- car::recode(Proc_data$nse, "1=1; 4=1; 2=2; 5=2; 6=2; 3=3; 7=3")
Proc_data$nse <- factor(Proc_data$nse,
                              labels = c("NSE Bajo", "NSE Medio", "NSE Alto"),
                              levels = c(1, 2, 3))
### Nivel educacional:
Proc_data$Nivel_educacional <- car::recode(Proc_data$Nivel_educacional, "1=1; 2:7=2; 8:11=3; 12:15=4")
Proc_data$Nivel_educacional <- factor(Proc_data$Nivel_educacional,
                                      labels = c("No asistió", "Educ. Básica", "Educ. Media", "Educ. Superior"),
                                      levels = c(1, 2, 3, 4))
### Sistema de Salud al que pertenece:
Proc_data$Sistema_salud <- car::recode(Proc_data$Sistema_salud, "5=0; 1=1; 2=2; 3=3; 4=4;")
Proc_data$Sistema_salud <- factor(Proc_data$Sistema_salud,
                                      labels = c("Otro sistema", "FONASA", "Isapre", "FF.AA.", "Particular"),
                                      levels = c(0, 1, 2, 3, 4))
### Tipo de ocupación o trabajo:
Proc_data$Trabajo_oficio <- car::recode(Proc_data$Trabajo_oficio, "1=1; 2=1; 3=1; 4=2; 5=2; 6=3; 7=3; 8=3; 0=4; 9=4")
Proc_data$Trabajo_oficio <- factor(Proc_data$Trabajo_oficio,
                                   labels = c("Gestión y Profesionales", "Administración y Servicios", "Ocupaciones Manuales y Técnicas", "Ocupaciones generales y Militares" ),
                                   levels = c(1, 2, 3, 4))
### Participación en Organizaciones de Red de Apoyo
Proc_data$Red_apoyo <- car::recode(Proc_data$Red_apoyo,"1=1; 2=1; 13=1; 4=2; 5=2; 3=3; 11=3; 6=4; 7=4; 8=4; 9=5; 10=5; 12=6; 14=7; 15=0" )
Proc_data$Red_apoyo <- factor(Proc_data$Red_apoyo,
                              labels = c("No participa", "Organizaciones Comunitarias y territoriales", "Organizaciones culturales y de Identidad", "Organizaciones Religiosas e Ideológicas", "Organizaciones de Grupos etáreos y género", "Organizaciones de Voluntariado y Apoyo", "Organizaicones corportativas y profesionales", "Otros"),
                              levels = c(0, 1, 2, 3, 4, 5, 6, 7))

# 3.6 Removemos base anterior y guardamos la nueva base
remove(CASEN2022)

save(Proc_data,file = "input/Proc_data.RData")

# 3.7 Comprobamos
names(Proc_data)

view_df(Proc_data)

frq(Proc_data$edad)
frq(Proc_data$sexo)
frq(Proc_data$region)
frq(Proc_data$area)
frq(Proc_data$nse)
frq(Proc_data$Nivel_educacional)
frq(Proc_data$Trabajo_oficio)
frq(Proc_data$Sistema_salud)
frq(Proc_data$Red_apoyo)
frq(Proc_data$Tipo_vivienda)

# 4. Descriptivos:
summarytools::dfSummary(Proc_data, plain.ascii = FALSE)
view(dfSummary(Proc_data, headings=FALSE))

# 4.1Graficos
graf_nse <- ggplot(Proc_data, aes(x = nse)) +
  geom_bar(fill = "coral") +
  labs (title = "Nivel socioeconómico")

graf_educ <- ggplot(Proc_data, aes(x = Nivel_educacional))+
  geom_bar(fill = "coral") +
  labs(title = "Nivel educativo")

graf_Trabajo <- ggplot(Proc_data, aes(x = Trabajo_oficio))+
  geom_bar(fill = "coral") +
  labs(title = "Oficio u ocupación")

graf_Salud <- ggplot(Proc_data, aes(x = Sistema_salud))+
  geom_bar(fill = "coral") +
  labs(title = "Sistema de previsional de salud")

graf_Apoyo <- ggplot(Proc_data, aes(x = Red_apoyo))+
  geom_bar(fill = "coral") +
  labs (title = "Red de apoyo")

graf_Viv <- ggplot(Proc_data, aes(x = Tipo_vivienda))+
  geom_bar(fill = "coral") +
  labs(title = "Tipo de Vivienda")

# 4.2 Guardamos los gráficos
ggsave(graf_nse , file="output/graf_nse.png")
ggsave(graf_Apoyo, file="output/graf_Apoyo.png")
ggsave(graf_Viv, file="output/graf_Viv.png")
ggsave(graf_educ, file="output/graf_educ.png")
ggsave(graf_Salud, file="output/graf_Salud.png")
ggsave(graf_Trabajo, file="output/graf_Trabajo.png")

#5. Asociación de variables: 
dim(Proc_data)
sjmisc::descr(Proc_data,
              show = c("label","range", "mean", "sd", "NA.prc", "n")) %>%
  kable(.,"markdown")

# 5.1 Correlación de variables: Eliminamos la región y el area, para ver como se comportan las variables que explican el fenómeno
Proc_data_cor <- Proc_data %>%
  select(nse,
         Nivel_educacional,
         Trabajo_oficio,
         Sistema_salud,
         Red_apoyo,
         Tipo_vivienda)

Proc_data_cor <- mutate_all(Proc_data_cor, as.numeric)

cor_proc_data <- cor(Proc_data_cor,
                     use = "complete.obs")
cor_proc_data

sjPlot::tab_corr(Proc_data_cor, 
                 triangle = "lower")

corrplot.mixed(cor_proc_data)

# 5.2 Confiabilidad del conjunto:

## Alpha de Cronbach:
psych::alpha(Proc_data_cor, check.keys = TRUE)

psych::alpha(dplyr::select(Proc_data_cor, nse,Nivel_educacional, Trabajo_oficio, Sistema_salud),check.keys = TRUE)

# 5.3 Construcción de índice:
Proc_data <- mutate_all(Proc_data, as.numeric)

Proc_data <- Proc_data %>% 
  rowwise() %>% 
  mutate(indice_pobreza_rural = sum(nse, Nivel_educacional,Trabajo_oficio, Sistema_salud, Red_apoyo))
summary(Proc_data$indice_pobreza_rural)


graf_IPC <- ggplot(Proc_data, aes(x = indice_pobreza_rural)) +
  geom_histogram(binwidth = 0.6, colour = "black", fill = "yellow") +  
  theme_bw() +
  xlab("Indices de pobreza multidimensional Rural en la 8va región") +
  ylab("Frecuencia")
ggsave(graf_IPC, file="Output/graf_IPC.png")

####### CORRECCION ENTREGA 3###########

# 6. Regresión Lineal

# 6.1 Recodificamos para la reegresión:

Proc_data <- Proc_data %>%
  mutate(sexo = case_when(sexo == 1 ~ 1, #Para Hombres
                          sexo == 2 ~ 0)) #Para Mujeres
#Modelos:
fit01 <- lm(indice_pobreza_rural ~ sexo, data = Proc_data)
fit02 <- lm(indice_pobreza_rural ~ sexo + edad, data = Proc_data)
fit03 <- lm(indice_pobreza_rural ~ sexo + edad + Tipo_vivienda, data = Proc_data)

labs01 <- c("Intercepto","Sexo (mujer)","Edad", "Tipo de vivienda")

screenreg(list(fit01,fit02,fit03),custom.coef.names = labs01)


# htmlreg para que se vea en el sitio web
knitreg(list(fit01,fit02,fit03),
        custom.model.names = c("Modelo 1","Modelo 2","Modelo 3"),
        custom.coef.names = c("Intercepto",
                              "Sexo (mujer)",
                              "Edad",
                              "Tipo de vivienda"))

###################################

# Función auxiliar para obtener coeficientes adecuados
get_coef_names <- function(fit, labels) {
  coef_names <- names(coef(fit))
  return(labels[1:length(coef_names)])
}

# Aplicar esta función a cada modelo
custom_labels <- lapply(list(fit01, fit02, fit03, fit04), get_coef_names, labels = labs01[[4]])

# Mostrar los modelos en la pantalla con screenreg
screenreg(list(fit01, fit02, fit03, fit04), 
          custom.model.names = c("Modelo 1", "Modelo 2", "Modelo 3", "Modelo 4"),
          custom.coef.names = custom_labels)

# htmlreg para que se vea en el sitio web
knitreg(list(fit01, fit02, fit03, fit04),
        custom.model.names = c("Modelo 1", "Modelo 2", "Modelo 3", "Modelo 4"),
        custom.coef.names = custom_labels)

