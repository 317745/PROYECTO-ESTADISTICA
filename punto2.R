# Directorio de trabajo
setwd("C:\\Users\\ASUS\\Downloads\\Rtrabajos")

library(readxl)
Datos_MP1 <- read_excel("Datos_MP1.xlsx", 
                        sheet = "Datos_2")
View(Datos_MP1)

install.packages("openxlsx")
library(openxlsx)

install.packages ("ggplot2")
library(ggplot2) 


head(Datos_MP1)
dim(Datos_MP1)
summary(Datos_MP1)
table(Datos_MP1$programa)



png("Figuras/fig.png")
barplot(table(Datos_MP1$programa),
        main = "1",
        xlab = "Programa",
        ylab = "Frecuencia")
dev.off()

png("Figuras/fig2.png")
boxplot(puntaje_mat ~ programa,
        data = Datos_MP1,
        main = "2",
        xlab = "Programa",
        ylab = "Puntaje")
dev.off()

png("Figuras/fig3.png")
par(mfrow = c(2, 1))
#Histograma puntaje en matemáticas
hist(Datos_MP1$puntaje_mat,
     main = "3",
     xlab = "Puntaje en matemáticas",
     ylab = "Frecuencia")

#Histograma número de premios 
hist(Datos_MP1$num_premios,
     main = "4",
     xlab = "Número de premios",
     ylab = "Frecuencia")
dev.off()

#Aqui reemplazamos los datos nulos por la mediana de puntaje
mediana1 = median(Datos_MP1$puntaje_mat, na.rm = TRUE)

Datos_MP1$puntaje_mat[is.na(Datos_MP1$puntaje_mat)] = mediana1

#Tomamos una muestra aleatoriadel 48% del total de datos
muestra = Datos_MP1[sample(nrow(Datos_MP1), 96), ]
write.xlsx(muestra, "muestra.xlsx")

library(MASS)
library(dplyr)



modelo_poisson = glm(num_premios ~ puntaje_mat + programa,
                      data = muestra,
                      family = poisson(link = "log"))
summary(modelo_poisson)

modelo_binon = glm.nb(num_premios ~ puntaje_mat + programa,
                   data = muestra)

summary(modelo_binon)


