# Directorio de trabajo
setwd("C:/Users/ASUS/Downloads/Rtrabajos")
library(readxl)
Datos_MP1 <- read_excel("Datos_MP1.xlsx", 
                        sheet = "Datos_2")
View(Datos_MP1)

#Tomar la muestra
set.seed(256)  
idx = sample(nrow(Datos_MP1), 121)
muestra = Datos_MP1[idx, ]

library(openxlsx)
write.xlsx(muestra, "muestra_121.xlsx")

#imputaciones
mediana_m = median(muestra$puntaje_mat, na.rm = TRUE)
media_m   = mean(muestra$puntaje_mat,   na.rm = TRUE)

#Tipos de muestra
muestra_mediana = muestra
muestra_media   = muestra
muestra_mediana$puntaje_mat[is.na(muestra_mediana$puntaje_mat)] = mediana_m
muestra_media$puntaje_mat[is.na(muestra_media$puntaje_mat)]     = media_m

#Summary
summary(muestra_mediana)
sd(muestra_mediana$puntaje_mat)
sd(muestra_mediana$num_premios)


#EDA GRAFICAS

png("Figuras/fig0.png")

# densidades
graf_orig    = density(muestra$puntaje_mat, na.rm = TRUE)
graf_mediana = density(muestra_mediana$puntaje_mat)
graf_media   = density(muestra_media$puntaje_mat)


plot(graf_orig,
     main = "Densidad puntaje",
     xlab = "Puntaje en matemáticas",
     ylab = "Densidad",
     lwd  = 2,
     ylim = c(0, max(graf_orig$y, graf_mediana$y, graf_media$y) + 0.005))

lines(graf_mediana, lwd = 3, lty = 2, col = "gray30") 
lines(graf_media,   lwd = 3, lty = 3, col = "red")

legend("topright",
       legend = c("Sin imputar", "Mediana", "Media"),
       lty = 1:3, lwd = 2, bty = "n", cex = 1.05)
dev.off()


# Gráfico 1: Frecuencia por programa en la muestra imputada con mediana
png("Figuras/fig.png")
barplot(table(muestra_mediana$programa),
        main = "1",
        xlab = "Programa",
        ylab = "Frecuencia")
dev.off()

# Gráfico 2: Boxplot de puntaje imputado con mediana por programa
png("Figuras/fig2.png")
boxplot(puntaje_mat ~ programa,
        data = muestra_mediana,
        main = "2",
        xlab = "Programa",
        ylab = "Puntaje")
dev.off()

# Gráfico 3: Histogramas de puntaje y premios en la muestra imputada
png("Figuras/fig3.png")
par(mfrow = c(2, 1))

hist(muestra_mediana$puntaje_mat,
     main = "3",
     xlab = "Puntaje en matemáticas",
     ylab = "Frecuencia")

hist(muestra_mediana$num_premios,
     main = "4",
     xlab = "Número de premios",
     ylab = "Frecuencia")
dev.off()


#Modelos de regresion
library(MASS)
library(dplyr)

modelo_poisson = glm(num_premios ~ puntaje_mat + programa,
                     data = muestra_mediana,
                     family = poisson(link = "log"))
summary(modelo_poisson)

modelo_binon = glm.nb(num_premios ~ puntaje_mat + programa,
                      data = muestra_mediana)
summary(modelo_binon)

png("Figuras/fig4.png")

muestra_mediana$programa = factor(muestra_mediana$programa,
                                  levels = c("General", "Academico", "Vocacional"))
# Secuencia de puntajes
x = seq(min(muestra_mediana$puntaje_mat),
        max(muestra_mediana$puntaje_mat),
        length.out = 100)

# Gráfico base
plot(NA, xlim = range(x), ylim = c(0, 6),
     xlab = "Puntaje en matemáticas",
     ylab = "Número esperado de premios",
     main = "Regresion Poisson")

# Curvas por programa
for (p in levels(muestra_mediana$programa)) {
  newdata = data.frame(puntaje_mat = x, programa = factor(p, levels = levels(muestra_mediana$programa)))
  pred = predict(modelo_poisson, newdata = newdata, type = "response")
  lines(x, pred, lwd = 2, lty = which(levels(muestra_mediana$programa) == p))
}

legend("topleft", legend = levels(muestra_mediana$programa),
       lty = 1:3, lwd = 2, bty = "n")

dev.off()

png("Figuras/fig5.png")
plot(NA, xlim = range(x), ylim = c(0, 6),
     xlab = "Puntaje en matemáticas",
     ylab = "Número esperado de premios",
     main = "Regresión Binomial Negativa")

for (p in levels(muestra_mediana$programa)) {
  newdata = data.frame(puntaje_mat = x,
                       programa = factor(p, levels = levels(muestra_mediana$programa)))
  y_pred = predict(modelo_binon, newdata = newdata, type = "response")
  lines(x, y_pred, lty = which(levels(muestra_mediana$programa) == p), lwd = 2)
}

legend("topleft", legend = levels(muestra_mediana$programa),
       lty = 1:3, lwd = 2, bty = "n")

dev.off()

#Evaluacion unica a puntaje matematico

modelo_binon_simple = glm.nb(num_premios ~ puntaje_mat,
                             data = muestra_mediana)

# Resumen del modelo
summary(modelo_binon_simple)

x = seq(min(muestra_mediana$puntaje_mat),
        max(muestra_mediana$puntaje_mat),
        length.out = 100)

# Crear dataframe de predicción
newdata = data.frame(puntaje_mat = x)

# Predicción del modelo binomial negativa
y_pred = predict(modelo_binon_simple, newdata = newdata, type = "response")

# Gráfica con puntos reales y predicción
plot(x, y_pred,
     type = "l",
     lwd = 2,
     col = "blue",
     ylim = c(0, max(muestra_mediana$num_premios)),
     xlab = "Puntaje en matemáticas",
     ylab = "Número esperado de premios",
     main = "Regresión Binomial Negativa")

points(muestra_mediana$puntaje_mat,
       muestra_mediana$num_premios,
       pch = 16, col = "black")

legend("topleft",
       legend = c("Predicción", "Datos reales"),
       col = c("blue", "black"),
       lty = c(1, NA),
       pch = c(NA, 16),
       bty = "n")
