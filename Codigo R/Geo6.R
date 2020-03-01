library(geoR)
library(readr)
b <- read_csv("C:/Users/Machine Learning/Documents/Quinto Avance/bd5.csv")
base <- rbind(b[,c("LATITUD", "LONGITUD","DESCRIPCION_CONDUCTA")])
#b <- read.table("C:/Users/Machine Learning/Documents/Quinto Avance/Base.xlsX")
base <- base[!is.na(base$LONGITUD),]
base <- base[!is.na(base$LATITUD),]
base <- unique(base)
lon_min <-  -74.039257 # minimum longitude of the analized city
lon_max <- -73.068602 # maximum longitude of the analized city
lat_min <-  3.840272 # minimum latitude of the analized city
lat_max <-  4.461413 # maximum latitude of the analized city
base <- base[as.logical((base$LONGITUD > lon_min)*(base$LONGITUD < lon_max)*
                          (base$LATITUD > lat_min)*(base$LATITUD < lat_max)),]#convert  1 and 0 in true or false

base$DESCRIPCION_CONDUCTA <- as.factor(base$DESCRIPCION_CONDUCTA)
summary(base)
basedf<-as.data.frame(base)
basedf<-as.data.frame(b)
#bdgeo<-read.geodata(basedf,header=TRUE, coord.col=1:2, data.col=3)
#bdgeo <- read.geodata('bdgeouno.txt', header = FALSE, coords.col = 1:2, data.col = 3)
#bdgeor <-read.geodata(basedf, header = FALSE, coords.col = c(1,2), data.col = 3,
#             data.names = c(dia,mes,delito))
#################################################
#funciona
################################################
datageo<-as.geodata(basedf, header = TRUE, coords.col = c(1,2), data.col = 3,
                    data.names = NULL)
summary(datageo)
oldpar <- par(mfrow=c(1,2)) 
variog(datageo, coords =datageo$coords, data = datageo$data, uvec = "default", breaks = "default", trend = "cte", lambda = 1, option = c("bin", "cloud", "smooth"), estimator.type = c("classical", "modulus"), nugget.tolerance=0.00179662, max.dist=0.00179662, pairs.min = 2, bin.cloud = FALSE, direction = "omnidirectional", tolerance = pi/8, unit.angle = c("radians","degrees"), angles = FALSE, messages=TRUE)
##################################################
#No funciona
##################################################
x=as.geodata(basedf, header = FALSE, coords.col = c(1,2), data.col = 3,
             data.names = NULL)
oldpar <- par(mfrow=c(1,2)) 
plot(variog(basedf))
coordenadas <- data.frame(x = basedf$LATITUD, y = basedf$LONGITUD)
datos<- data.frame(desc=basedf$DESCRIPCION_CONDUCTA)
variog(basedf, coords =coordenadas, data =datos , uvec = "default", breaks = "default", trend = "cte", lambda = 1, option = c("bin", "cloud", "smooth"), estimator.type = c("classical", "modulus"), nugget.tolerance, max.dist, pairs.min = 2, bin.cloud = FALSE, direction = "omnidirectional", tolerance = pi/8, unit.angle = c("radians","degrees"), angles = FALSE, messages=TRUE) 
#variog(x)
###################################################
# Funciona ok
###################################################
plot(variog(datageo,coords=datageo$coords))

plot(variog(datageo, max.dist = 0.00179662))
par(oldpar)
vario <- variog(datageo, max.dist = 0.00179662)
# variog: computing omnidirectional variogram
names(vario)
str(vario)
vario.b <- variog(datageo, max.dist = 0.00179662)
vario.c <- variog(datageo, max.dist=0.00179662, op="cloud")
vario.bc <- variog(datageo, max.dist=0.00179662, bin.cloud=TRUE)
vario.s <- variog(datageo, max.dist=0.00179662, op="sm", band=0.2) 
oldpar<-par(mfrow=c(2,2)) # Preparar para 4 gráficos por ventana
plot(vario.b, main="Variograma empírico")
plot(vario.c, main="Nube de puntos variograma")
plot(vario.bc, bin.cloud=TRUE, main="Graficos de cajas")
title("Gráficos de cajas") # Corregir fallo del comando anterior
plot(vario.s, main="Variograma suavizado")
par(oldpar)
varior.b <- variog(datageo, estimator.type = "modulus", max.dist=0.00179662)
varior.bc <- variog(datageo, estimator.type = "modulus", max.dist=0.00179662, bin.cloud=TRUE)
oldpar<-par(mfrow=c(2,2)) #Preparar para 4 gráficos por ventana
plot(vario.b, main="Estimador clásico")
plot(varior.b, main="Estimador robusto")
plot(vario.bc, bin.cloud=TRUE)
plot(varior.bc, bin.cloud=TRUE)
par(oldpar)
vario.60 <- variog(datageo, max.dist = 0.00179662, direction = pi/3) #variograma en la dirección de 60 grados
vario.4 <- variog4(datageo, max.dist = 0.00179662)
oldpar <- par(mfrow=c(1,2))
plot(vario.60)
title(main = expression(paste("direccional, angulo = ", 60 * degree)))
plot(vario.4, lwd = 2)
par(oldpar)
vario.b <- variog(datageo, max.dist=0.00179662) #discretizado
vario.s <- variog(datageo, max.dist=0.00179662,option = "smooth", kernel = "normal", band = 0.2)  #suavizado
plot(vario.b)
lines(vario.s, type = "l", lty = 2)

lines.variomodel(cov.model = "exp", cov.pars = c(1,0.3), nugget = 0, max.dist = 0.00179662, lwd = 3)
legend(0.3, 0.3, c("empirico", "suavizado", "modelo exponencial"), lty = c(1, 2, 1), lwd = c(1, 1, 3))

plot(vario.b)
lines.variomodel(cov.model = "exp", cov.pars = c(0.9,0.3), nug = 0.1, max.dist = 0.00179662)
lines.variomodel(cov.model = "mat", cov.pars = c(0.85,0.2), nug = 0.1, kappa = 1, max.dist = 0.00179662,lty = 2)
lines.variomodel(cov.model = "sph", cov.pars = c(0.8,0.8), nug = 0.1, max.dist = 0.00179662, lwd = 2)
eyefit(vario.b)
##############################
#Revisar vario.ols y vario.wls valor para phi es muy alto y para sigmaq + nugget es muy bajo
##############################
vario.ols <- variofit(vario.b, ini = c(1, 0.5), weights = "equal")  #ordinarios
vario.wls <- variofit(vario.b, ini = c(1, 0.5), weights = "cressie")  #ponderados
vario.wls
summary(vario.wls)
##############################
vario.ml <- likfit(datageo, ini = c(1, 0.5)) #Modelo exponencial con par ini umbral y escala (1/3 rango)
vario.ml
summary(vario.ml)
vario.reml <- likfit(datageo, ini = c(1, 0.5), lik.method = "RML")
summary(vario.reml)
plot(vario.b, main = "Estimador empírico y modelos ajustados")
lines(vario.ml, max.dist = 0.00179662)
lines(vario.reml, lwd = 2, max.dist = 0.00179662)
lines(vario.ols, lty = 2, max.dist = 0.00179662)
lines(vario.wls, lty = 2, lwd = 2, max.dist = 0.00179662)
legend(0.3, 0.3, legend = c("ML", "REML", "OLS", "WLS"), lty = c(1, 1, 2, 2), lwd = c(1, 2,1, 2)) 
env.indep <- variog.mc.env(datageo, obj.var = vario.b)
env.model <- variog.model.env(datageo, obj.var = vario.b, model = vario.wls)
oldpar <- par(mfrow = c(1, 2))
plot(vario.b, envelope = env.indep)
plot(vario.b, envelope = env.model)
lines(vario.wls, lty = 2, lwd = 2, max.dist = 0.00179662)
par(oldpar) 
library(sm)
sm.variogram(datageo$coords, datageo$data, model = "independent")
xv.wls <- xvalid(datageo, model = vario.wls)
summary(xv.wls)
xv.reml <- xvalid(datageo, model = vario.reml)
summary(xv.reml)
oldpar <- par(mfrow = c(2, 5))
###Se necesitan valore finitos de xlim*
plot(xv.wls, ask = FALSE)
par(oldpar)
oldpar <- par(mfrow=c(1,2)) 
plot(variog(datageo, max.dist = 0.00179662)) # Supone que el proceso es estacionario
plot(variog(datageo, trend = ~coords, max.dist = 0.00179662)) # Asume una tendencia lineal en las coordenadas
par(oldpar)
# Rejilla regular 51x51 en cuadrado unidad
xx <- seq(0, 1, l = 51)
yy <- seq(0, 1, l = 51)
pred.grid <- expand.grid(x = xx, y = yy) 
plot(datageo$coords, pch = 20)
points(pred.grid, pch = 3, cex = 0.2)
ko.wls <- krige.conv(datageo, loc = pred.grid, krige = krige.control(obj.m = vario.wls))
names(ko.wls)
oldpar <- par(mfrow = c(1, 2))
image(ko.wls) #superficie de predicción
title("Predicciones")
points(datageo$coords, pch=20) #añadir posiciones datos
contour(ko.wls,add=T) #añadir gráfico de contorno

image(ko.wls, val = ko.wls$krige.var) #superficie de varianzas
title("Superficie de varianzas")
points(datageo$coords, pch=20)
contour(ko.wls,val=sqrt(ko.wls$krige.var),add=T)
par(oldpar)
contour(ko.wls,filled = TRUE)
fcol <- topo.colors(10)[cut(matrix(ko.wls$pred,nrow=51,ncol=51)[-1,-1],10,include.lowest=TRUE)]
persp(ko.wls, theta=-60, phi=40, col=fcol)

