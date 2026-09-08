#Leer base de datos
Base_datos <- read.csv("TB4.csv")
library(ggplot2)


# Vector para almacenar las columnas donde aparece el primer valor > 0 por fila
capturas <- c()

# Vector donde se acumula
Vec_Datos <- c()

# Conjunto para almacenar columnas vistas
columnas_vistas <- c()

# Contador de nuevas capturas
contador_actual <- 0  

# Conjunto de especies ya vistas
especies_vistas <- c(1,2,3,4,5,length(Base_datos[1,]))  #para no considerar como especies las primeras columnas de la base y ultimas

#Creo esta base de datos para que se obtengan los numeros de nuevas especies desde 
#el tiempo más en el pasado, hacia el presente.
REV_Base_datos <- Base_datos[order(Base_datos$profundidad,decreasing=TRUE),]

# Recorrer cada fila
for (i in 1:nrow(REV_Base_datos)) {
  especies_presentes <- which(REV_Base_datos[i, ] > 0)  # Buscar especies con > 0 en la fila
  
  # Verificar si hay nuevas especies que no estaban antes
  nuevas_especies <- setdiff(especies_presentes, especies_vistas)
  
  if (length(nuevas_especies) > 0) {
    contador_actual <- contador_actual + length(nuevas_especies)  # Sumar nuevas especies
    especies_vistas <- c(especies_vistas, nuevas_especies)  # Agregar nuevas especies vistas
  }
  
  Vec_Datos[i] <- contador_actual  # Guardar el valor acumulativo en el vector
}

df <- data.frame(prof = c(332,REV_Base_datos$profundidad), time = c(15251,REV_Base_datos$edad_mean), conteos = c(0,Vec_Datos))

Dias_prof <- 332 - df$prof
Vec_Datos <- df$conteos
saveRDS(Dias_prof,file = "Datos_Dias_prof.RDS")
saveRDS(Vec_Datos,"Datos_obs.RDS")
saveRDS(df$time,"Time_Graphs.RDS")



plot(-df$prof,df$conteos)  #pongo el menos en el eje x para que ordene del pasado al presente

ggplot(data = df, aes(x = -prof, y = conteos)) +  
  geom_point(shape = 21, fill = "green", color = "darkgreen", size = 2) +
  labs(x = "Depth",y = "Taxa") +
  theme_minimal(base_size = 15) +
  scale_x_continuous(breaks = -c(300,200,100,0) , #Negativo para que ponga en orden
                     labels = c(331,231,131,31)) +  #Estos van de la mayor a la menor prof. sin signo negativo.
  theme(panel.grid.minor = element_blank())   



#Ahora con el tiempo

ggplot(data = df, aes(x = -time, y = conteos)) +  
  geom_point(shape = 21, fill = "green", color = "darkgreen", size = 2) +
  labs(x = "Time",y = "Species") +
  theme_minimal(base_size = 15) +
  scale_x_continuous(breaks = -c(15000, 10000, 5000,0) , #Negativo para que ponga en orden
                     labels = c(15000, 10000, 5000,0)) + #Estos van de la mayor a la menor time sin signo negativo.
  theme(panel.grid.minor = element_blank())   



plot(df$prof,df$time)

###

plot(df$time,df$prof,xlim=c(0,16000),ylim=c(0,350))
xx<-df$time
yy<-df$prof
reg <- lm(yy~poly(xx, degfree=5))
lines(df$time,reg$fitted.values,col="red")

new_times <- data.frame(xx = seq(500,15500,by=1000))
tiempo_ticks <- predict(reg, new_times) #profundidades predichas
lines(new_times$xx,tiempo_ticks,col="red",t="p",pch=19)
abline(h=280)
abline(v=11000)
Base_datos[82,]

###


par(oma=c(2,2,3,1))
plot(x = -df$prof, y = df$conteos, 
     pch = 21, 
     col = "black",
     bg = "gray",
     cex = 1.2,
     xlab = "Depth (cm)", 
     ylab = "Pollen taxa",
     cex.lab = 1.5,
     frame=FALSE,
     #xaxp = c(0,340,10))
     axes = FALSE)
#grid(col=gray(0.2))

y_ticks = seq(0,90,by = 10)
abline(h = 90, col = "lightgray", lty = "dotted")
axis(side = 2, at = y_ticks)
abline(h = y_ticks, col = gray(0.2), lty = "dotted")
axis(side = 2, at = y_ticks)

prof_ticks = c(seq(-331, 0, by = 50),0)
abline(v = prof_ticks, col = gray(0.2), lty = "dotted")
axis(side = 1, at = prof_ticks, labels = -prof_ticks)


mtext("Time (years)", side = 3, line = 4, cex=1.5)
axis(side = 3, at = -tiempo_ticks ,labels = new_times$xx,las=2)
abline(v = tiempo_ticks, col = rgb(0,0.5,1,0.5), lty = "aa",lwd=.3)


