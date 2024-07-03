
setwd("/Users/imac/Desktop/Prueba 1 experimento")

# casa

setwd("/Users/guru/Downloads/rejudithiniciodatos")

data <- read.csv("jud.Table")

str(data)

# ===================== Ranquin ====================

# Selección de los textos identificados correctamente



orales <- subset(data, tipo =="o")
leidos <- subset(data, tipo =="l")

orales_acierto <- subset(orales, RespuestaCorrecta == 1)
leidos_acierto <- subset(leidos, RespuestaCorrecta == 1)

orales_acierto_seguridad <- subset(orales_acierto, goodness==5 | goodness==4)
leidos_acierto_seguridad <- subset(leidos_acierto, goodness==5 | goodness==4)

orales_acierto_seguridad_tiempo <- subset(orales_acierto_seguridad, reactionTime < 10)
leidos_acierto_seguridad_tiempo <- subset(leidos_acierto_seguridad, reactionTime < 10)

orales_acierto_seguridad_tiempo
leidos_acierto_seguridad_tiempo

media_de_tiempo_de_respuesta = mean(data[,5])
range(data[,5])
desviacion = sd(data[,5])


media_de_tiempo_de_respuesta
desviacion

boxplot(data[,5])
grid()


hist(orales_acierto[,5])
hist(leidos_acierto[,5])

hist(orales_acierto_seguridad[,5])
hist(leidos_acierto_seguridad[,5])


order(orales_acierto_tiempo[,5])

# orales_acierto_tiempo

order(orales_acierto_tiempo[,5])
[1] 3 2 1 7 5 6 4

order(leidos_acierto_tiempo[,5])
leidos_acierto_tiempo
o1f2 
o3f3 
o1m3 


leidos_acierto
l2f2 
l2f2 
l2f2
l2f2 
 l1f3 
 l1f3
 
 Es decir, l2f2  y l1f3 


#z = rank(data, na.last = TRUE, ties.method = c("average", "first", "last", "random", "max", "min"))






# ============================

tipos <- c("Textos orales", "Textos leídos")
subtipos <- c("orales L. hombres", "orales L. mujeres", "leídos L. mujeres", "leídos L. hombres")
sexos <- c("lector hombre", "lector mujer")



orales
leidos

orales_acierto <- subset(orales, RespuestaCorrecta == 1)
leidos_acierto <- subset(leidos, RespuestaCorrecta == 1)

orales_acierto
leidos_acierto 



# hacer el ránquin de los resultados

textos <- c("l1m1", "l2m3", "l3m2", "l3f1", "l2f2", "l1f3", "o1m3", "o1f2", "o2f1", "o2m2", "o3m1", "o3f3")

x <- data

x <- x*-1






orales_f <- subset(orales, sexoLector =="f")
orales_m <- subset(orales, sexoLector =="m")



leidos_f <- subset(leidos, sexoLector =="f")
leidos_m <- subset(leidos, sexoLector =="m")

mean(orales[,5])
mean(leidos[,5])


medias <- c(mean(orales_f[,5]), mean(orales_m[,5]), mean(leidos_f[,5]), mean(leidos_m[,5]))




# aciertos

# leídos considerados como leídos
leidos_identificados <- subset(leidos, response == "l" )
leidos_no_identificados <- subset(leidos, response == "e" )

orales_identificados <- subset(orales, response == "e" )
orales_no_identificados <- subset(orales, response == "l" )
# orales considerados como orales




n_leidos_iden <-nrow(leidos_identificados)
n_leidos_no_iden  <-nrow(leidos_no_identificados)
n_orales_iden <-nrow(orales_identificados)
n_orales_no_iden <-nrow(orales_no_identificados)

total_respuestas <- c(n_leidos_iden,n_orales_iden,n_leidos_no_iden,n_orales_no_iden)


identificados <- c("leídos +", "orales +", "leídos -", "orales -")


tiempo_leidos_Si <- (leidos_identificados[,5])
tiempo_leidos_No <- (leidos_no_identificados[,5])
tiempo_leidos_No
tiempo_orales_Si <- (orales_identificados[,5])
tiempo_orales_No <- (orales_no_identificados[,5])
tiempo_orales_Si
tiempo_orales_No
xtiempo_leidos_Si <- mean(leidos_identificados[,5])
xtiempo_leidos_No <- mean(leidos_no_identificados[,5])

xtiempo_orales_Si <- mean(orales_identificados[,5])
xtiempo_orales_No <- mean(orales_no_identificados[,5])
x_tiempos_respuestas <- c(xtiempo_leidos_Si, xtiempo_orales_Si, xtiempo_leidos_No, xtiempo_orales_No)


tiempo_diferenciados <- c(tiempo_leidos_Si, tiempo_leidos_No, tiempo_orales_Si, tiempo_orales_No)







####### Gráficos ############


boxplot(orales[,5], leidos[,5], col = c("red", "blue"), axes = F, main="Diagrama de cajas del tiempo de respuesta")
axis(1, at =c(1, 2), labels = tipos)
box()
axis(2)
grid()
# Observación
# El tiempo de respuesta es un poco mayor en textos orales que leídos. Los textos leídos se identifican más rápidamente.







boxplot(orales_f[,5], orales_m[,5], leidos_f[,5], leidos_m[,5], col = c("brown", "brown1", "cadetblue","cadetblue1" ), axes=F, main ="Diagrama de cajas del tiempo de respuesta según sexo del lector")
axis(1, at =c(1, 2, 3, 4), labels = subtipos)
box()
axis(2)
grid()
# El tiempo de respuesta es más homogéneo en los textos leídos por hombres y tiene la mayor dispersión en los textos orales emitidos por hombres.



plot(medias, type="b", xlab="", axes=F, pch=c(18,23,2,17), col = "red", ylim=c(12,20), main= "Medias del tiempo de respuesta por sexo y tipo")
axis(1, at =c(1, 2, 3, 4), labels = subtipos)
box()
axis(2)
grid()
# El promedio del tiempo de respuesta es mayor para los textos orales de lector mujer y es menor para los textos leídos por hombres.



par(mfrow=c(1,2))
plot(total_respuestas,type="b", xlab="", axes=F, pch=c(18,23,2,17), col = "blue", ylim=c(2,30), ylab="Ene de respuestas", main ="Número de respuestas correctas e incorrectas")
axis(1, at =c(1, 2, 3, 4), labels = identificados)
box()
axis(2)
grid()

# los textos leídos son bien identificados muchas más veces que los que no son bien identificados
# Lo mismo ocurreo con los textos orales


# Tiempos de respuesta para leídos + orales + leidos - orales -


plot(tiempos_respuestas,type="b", xlab="", axes=F, ylab="tiempo de respuesta", col = "red" )
axis(1, at =c(1, 2, 3, 4), labels = identificados)
box()
axis(2)
grid()



boxplot(tiempo_leidos_Si, tiempo_leidos_No, tiempo_orales_Si, tiempo_orales_No,xlab="", axes=F, col = c("brown", "brown1", "cadetblue","cadetblue1" ))
axis(1, at =c(1, 2, 3, 4), labels = identificados)
box()
axis(2)
grid()

