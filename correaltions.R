setwd("/")
setwd("Users/pc/unah/I P 2020/seminario/repositorio-git/encuesta-Seminario/")

#install.packages("dplyr") #instalamos la libreria dplyr
#install.packages("ggplot2")
#install.packages("corrplot")
#install.packages("factoextra")
#install.packages("caret")

survey <- read.csv("survey_cleaned_v2.csv", sep = ",", header = T)

library(dplyr)
library(ggplot2)
library(corrplot)
library(factoextra)
library(caret)

#---------------------------------------------------------------------------------------------------------------------------------------

#correlacion entre variables rendimiento_academico y rechazo_en_entrevista
#Con esta correlacion se pretende saber si el rendimiento academico influye en el rechazo o aceptacion para la practica
prop.table(table(survey$rendimiento_academico, survey$rechazo_en_entrevista), 1)


ggplot(survey) +
  aes(x = rendimiento_academico, fill = factor(rechazo_en_entrevista)) +
  geom_bar(position = "stack") +
  theme(axis.text.x = element_text(angle = 45))

ggplot(survey) +
  aes(x = rendimiento_academico, fill = factor(rechazo_en_entrevista)) +
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 45))



chisq.test(table(survey$rendimiento_academico,survey$rechazo_en_entrevista))

#Ho: variables independientes
#Ha: variables dependientes 


#segun el p-value=0.3895 obtenido de nuestra prueba de chisq se rechza la hipotesis nula.  
  #conclusion: el rendimiento academico si influye en cuanto al rechazo d estudiantes en su entrevista laboral, por lo tanto 
    #las variables son dependientes.

#---------------------------------------------------------------------------------------------------------------------------------------

#correlacion entre variables entrevistas_previas y rechazo_en_entrevista
#Con esta correlacion se pretende saber si tener experiencia previa en entrevistas influye en el rechazo o aceptacion para la practica

prop.table(table(survey$entrevistas_previas, survey$rechazo_en_entrevista), 2)


ggplot(survey) +
  aes(x = entrevistas_previas, fill = factor(rechazo_en_entrevista)) +
  geom_bar(position = "stack") +
  theme(axis.text.x = element_text(angle = 45))

ggplot(survey) +
  aes(x = entrevistas_previas, fill = factor(rechazo_en_entrevista)) +
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 45))

chisq.test(table(survey$entrevistas_previas,survey$rechazo_en_entrevista))

#Ho: variables independientes
#Ha: variables dependientes 
#segun el p-value=0.03152 obtenido en esta correlacion se acepta la hipotesis nula
  #conlcusion: como se acepta la hipotesis nula, esto significa que las variables son independientes
    #por lo tanto la experiencia en entrevistas previas no influye sobre el rechazo que pueda tener un estudiante

#---------------------------------------------------------------------------------------------------------------------------------------

#correlacion entre variables area_sistema_dificulta y rechazo_en_entrevista
#Con esta correlacion se pretende saber si el estudiante que tiene dificultad en alguna area de estudio de la carrera
  #influye en el rechazo o aceptacion para la practica

prop.table(table(survey$area_sistema_dificulta, survey$rechazo_en_entrevista), 2)


ggplot(survey) +
  aes(x = area_sistema_dificulta, fill = factor(rechazo_en_entrevista)) +
  geom_bar(position = "stack") +
  theme(axis.text.x = element_text(angle = 45))

ggplot(survey) +
  aes(x = area_sistema_dificulta, fill = factor(rechazo_en_entrevista)) +
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 45))

chisq.test(table(survey$area_sistema_dificulta,survey$rechazo_en_entrevista))

#Ho: variables independientes
#Ha: variables dependientes 
#segun el p-value=0.807 obtenido en esta correlacion se rechaza la hipotesis nula
#conlcusion: como se rechaza la hipotesis nula, esto significa que las variables son dependientes
#por lo tanto que el estudiante tenga dificultad en determinadas areas si influye sobre el rechazo que pueda tener un estudiante

#---------------------------------------------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------------------------------------

