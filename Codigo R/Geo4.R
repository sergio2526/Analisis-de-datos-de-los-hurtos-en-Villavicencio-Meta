library(geoR)
data(s100) # Cargar datos estacionarios
summary(s100)
oldpar <- par(mfrow=c(1,2)) 
plot(variog(s100))
plot(variog(s100, max.dist = 0.6))
par(oldpar)
vario <- variog(s100, max.dist = 0.6)
# variog: computing omnidirectional variogram
names(vario)
str(vario)
vario.b <- variog(s100, max.dist = 0.6)
vario.c <- variog(s100, max.dist=0.6, op="cloud")
vario.bc <- variog(s100, max.dist=0.6, bin.cloud=TRUE)
vario.s <- variog(s100, max.dist=0.6, op="sm", band=0.2) 
oldpar<-par(mfrow=c(2,2)) # Preparar para 4 gráficos por ventana
plot(vario.b, main="Variograma empírico")
plot(vario.c, main="Nube de puntos variograma")
plot(vario.bc, bin.cloud=TRUE, main="Graficos de cajas")
title("Gráficos de cajas") # Corregir fallo del comando anterior
plot(vario.s, main="Variograma suavizado")
par(oldpar)
varior.b <- variog(s100, estimator.type = "modulus", max.dist=0.6)
varior.bc <- variog(s100, estimator.type = "modulus", max.dist=0.6, bin.cloud=TRUE)
oldpar<-par(mfrow=c(2,2)) #Preparar para 4 gráficos por ventana
plot(vario.b, main="Estimador clásico")
plot(varior.b, main="Estimador robusto")
plot(vario.bc, bin.cloud=TRUE)
plot(varior.bc, bin.cloud=TRUE)
par(oldpar)
vario.60 <- variog(s100, max.dist = 0.6, direction = pi/3) #variograma en la dirección de 60 grados
vario.4 <- variog4(s100, max.dist = 0.6)
oldpar <- par(mfrow=c(1,2))
plot(vario.60)
title(main = expression(paste("direccional, angulo = ", 60 * degree)))
plot(vario.4, lwd = 2)
par(oldpar)
vario.b <- variog(s100, max.dist=0.6) #discretizado
vario.s <- variog(s100, max.dist=0.6,option = "smooth", kernel = "normal", band = 0.2)  #suavizado
plot(vario.b)
lines(vario.s, type = "l", lty = 2)

lines.variomodel(cov.model = "exp", cov.pars = c(1,0.3), nugget = 0, max.dist = 0.6, lwd = 3)
legend(0.3, 0.3, c("empirico", "suavizado", "modelo exponencial"), lty = c(1, 2, 1), lwd = c(1, 1, 3))

plot(vario.b)
lines.variomodel(cov.model = "exp", cov.pars = c(0.9,0.3), nug = 0.1, max.dist = 0.6)
lines.variomodel(cov.model = "mat", cov.pars = c(0.85,0.2), nug = 0.1, kappa = 1, max.dist = 0.6,lty = 2)
lines.variomodel(cov.model = "sph", cov.pars = c(0.8,0.8), nug = 0.1, max.dist = 0.6, lwd = 2)
eyefit(vario.b)
vario.ols <- variofit(vario.b, ini = c(1, 0.5), weights = "equal")  #ordinarios
vario.wls <- variofit(vario.b, ini = c(1, 0.5), weights = "cressie")  #ponderados
vario.wls
summary(vario.wls)
vario.ml <- likfit(s100, ini = c(1, 0.5)) #Modelo exponencial con par ini umbral y escala (1/3 rango)
vario.ml
summary(vario.ml)
vario.reml <- likfit(s100, ini = c(1, 0.5), lik.method = "RML")
summary(vario.reml)
plot(vario.b, main = "Estimador empírico y modelos ajustados")
lines(vario.ml, max.dist = 0.6)
lines(vario.reml, lwd = 2, max.dist = 0.6)
lines(vario.ols, lty = 2, max.dist = 0.6)
lines(vario.wls, lty = 2, lwd = 2, max.dist = 0.6)
legend(0.3, 0.3, legend = c("ML", "REML", "OLS", "WLS"), lty = c(1, 1, 2, 2), lwd = c(1, 2,1, 2)) 
env.indep <- variog.mc.env(s100, obj.var = vario.b)
env.model <- variog.model.env(s100, obj.var = vario.b, model = vario.wls)
oldpar <- par(mfrow = c(1, 2))
plot(vario.b, envelope = env.indep)
plot(vario.b, envelope = env.model)
lines(vario.wls, lty = 2, lwd = 2, max.dist = 0.6)
par(oldpar) 
library(sm)
sm.variogram(s100$coords, s100$data, model = "independent")
xv.wls <- xvalid(s100, model = vario.wls)
summary(xv.wls)
xv.reml <- xvalid(s100, model = vario.reml)
summary(xv.reml)
oldpar <- par(mfrow = c(2, 5))
plot(xv.wls, ask = FALSE)
par(oldpar)
oldpar <- par(mfrow=c(1,2)) 
plot(variog(wolfcamp, max.dist = 200)) # Supone que el proceso es estacionario
plot(variog(wolfcamp, trend = ~coords, max.dist = 200)) # Asume una tendencia lineal en las coordenadas
par(oldpar)
# Rejilla regular 51x51 en cuadrado unidad
xx <- seq(0, 1, l = 51)
yy <- seq(0, 1, l = 51)
pred.grid <- expand.grid(x = xx, y = yy) 
plot(s100$coords, pch = 20)
points(pred.grid, pch = 3, cex = 0.2)
ko.wls <- krige.conv(s100, loc = pred.grid, krige = krige.control(obj.m = vario.wls))
names(ko.wls)
oldpar <- par(mfrow = c(1, 2))
image(ko.wls) #superficie de predicción
title("Predicciones")
points(s100$coords, pch=20) #añadir posiciones datos
contour(ko.wls,add=T) #añadir gráfico de contorno

image(ko.wls, val = ko.wls$krige.var) #superficie de varianzas
title("Superficie de varianzas")
points(s100$coords, pch=20)
contour(ko.wls,val=sqrt(ko.wls$krige.var),add=T)
par(oldpar)
contour(ko.wls,filled = TRUE)
fcol <- topo.colors(10)[cut(matrix(ko.wls$pred,nrow=51,ncol=51)[-1,-1],10,include.lowest=TRUE)]
persp(ko.wls, theta=-60, phi=40, col=fcol)
