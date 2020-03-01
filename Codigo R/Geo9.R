library(geoR)
library(readr)
b <- read_csv("C:/Users/Machine Learning/Documents/Sexto Avance/Fiscalia11072019/bd8.csv")
base <- rbind(b[,c("LATITUD", "LONGITUD","DIA_SEMANA","MES_LARGO","DESCRIPCION_CONDUCTA")])
base$DIA_SEMANA <- as.factor(base$DIA_SEMANA)
base$MES_LARGO <- as.factor(base$MES_LARGO)
base$DESCRIPCION_CONDUCTA <- as.factor(base$DESCRIPCION_CONDUCTA)
summary(base)
basedf<-as.data.frame(b)
x<-c(3.840272 , -74.039257)
y<-c(4.461413,-73.068602)
bordes<-as.data.frame(rbind(x,y))

coordenadas <- data.frame(x = basedf$LATITUD, y = basedf$LONGITUD , z = basedf$DIA_SEMANA, f = basedf$MES_LARGO , e = basedf$DESCRIPCION_CONDUCTA )

datos<- data.frame(desc=basedf$DESCRIPCION_CONDUCTA)

datageo<-as.geodata(coordenadas, header = TRUE, coords.col = c(1,2), data.col = 3:4,
                    data.names =NULL)

summary(datageo)
oldpar <- par(mfrow=c(1,2)) 
variog(datageo, coords =datageo$coords, data = datageo$data, uvec = "default", breaks = "default", trend = "cte", lambda = 1, option = c("bin", "cloud", "smooth"), estimator.type = c("classical", "modulus"), nugget.tolerance=0, max.dist=0.2, pairs.min = 2, bin.cloud = FALSE, direction = "omnidirectional", tolerance = pi/8, unit.angle = c("radians","degrees"), angles = FALSE, messages=TRUE)
vario<-variog(datageo, coords =datageo$coords, data = datageo$data, uvec = "default", breaks = "default", trend = "cte", lambda = 1, option = c("bin", "cloud", "smooth"), estimator.type = c("classical", "modulus"), nugget.tolerance=0, max.dist=0.2, pairs.min = 2, bin.cloud = FALSE, direction = "omnidirectional", tolerance = pi/8, unit.angle = c("radians","degrees"), angles = FALSE, messages=TRUE)
plot(vario)

plot(variog(datageo,coords=datageo$coords))

plot(variog(datageo, max.dist = 0.2))
par(oldpar)
vario <- variog(datageo, max.dist = 0.2)
names(vario)
str(vario)

vario.b <- variog(datageo, max.dist = 0.2)

vario.c <- variog(datageo, max.dist=0.2, op="cloud")

vario.bc <- variog(datageo, max.dist=0.2)


oldpar<-par(mfrow=c(1,2))
plot(vario.b, main="Variograma empírico")
plot(vario.c, main="Nube de puntos variograma")
par(oldpar)
varior.b <- variog(datageo, estimator.type = "modulus", max.dist=0.2)
oldpar<-par(mfrow=c(1,2)) #Preparar para 4 gráficos por ventana
plot(vario.b, main="Estimador clásico")
plot(varior.b, main="Estimador robusto")
par(oldpar)



vario.60 <- variog(datageo1, max.dist = 0.00179662, direction = pi/3) #variograma en la dirección de 60 grados
vario.4 <- variog4(datageo1, max.dist = 0.00179662)
oldpar <- par(mfrow=c(1,2))
plot(vario.60)

title(main = expression(paste("direccional, angulo = ", 60 * degree)))
plot(vario.4, lwd = 2)
par(oldpar)
vario.b <- variog(datageo, max.dist=0.2) #discretizado
plot(vario.b)
lines.variomodel(cov.model = "exp", cov.pars = c(1,0.3), nugget = 0.1, max.dist = 0.2, lwd = 3)
legend(0.3, 0.3, c("empirico", "suavizado", "modelo exponencial"), lty = c(1, 2, 1), lwd = c(1, 1, 3))

plot(vario.b)
lines.variomodel(cov.model = "exp", cov.pars = c(0.9,0.3), nug = 0.1, max.dist = 0.2)
lines.variomodel(cov.model = "mat", cov.pars = c(0.85,0.2), nug = 0.1, kappa = 1, max.dist = 0.2,lty = 2)
lines.variomodel(cov.model = "sph", cov.pars = c(0.8,0.8), nug = 0.1, max.dist = 0.2, lwd = 2)
eyefit(vario.b)
vario.ols <- variofit(vario, ini.cov.pars, cov.model, fix.nugget = FALSE, nugget = 0, fix.kappa = TRUE, kappa = 0.5, simul.number = NULL, max.dist = vario$max.dist, weights, minimisation.function, limits = pars.limits(), messages=TRUE)
vario.ols <- variofit(vario.b, ini = c(1, 0.5), weights = "equal")  #ordinarios
vario.wls <- variofit(vario.b, ini = c( 0.5,0.5),weights = "cressie")  #ponderados
vario.wls <- variofit(vario.b, ini = c(0.5, 0.5), fix.nugget=TRUE)  #ponderados
vario.wls
summary(vario.wls)
vario.ml <- likfit(datageo, coords=datageo$coords, data=datageo$data, trend = "cte",ini.cov.pars=FALSE, fix.nugget = FALSE, nugget = 0, fix.kappa = TRUE, kappa = 0.5, fix.lambda = TRUE, lambda = 1, fix.psiA = TRUE, psiA = 0, fix.psiR = TRUE, psiR = 1, lik.method = "ML", components = TRUE, nospatial = TRUE, limits = pars.limits(), print.pars = FALSE, messages=TRUE)
                   
vario.ml <- likfit(datageo, ini = c(0.5, 0.5),fix.nug = TRUE) #Modelo exponencial con par ini umbral y escala (1/3 rango)
vario.ml
summary(vario.ml)
vario.reml <- likfit(datageo, ini = c(1, 0.5), lik.method = "RML")
summary(vario.reml)
plot(vario.b, main = "Estimador empírico y modelos ajustados")
lines(vario.ml, max.dist = 0.2)
lines(vario.reml, lwd = 2, max.dist = 0.2)
lines(vario.ols, lty = 2, max.dist = 0.2)
lines(vario.wls, lty = 2, lwd = 2, max.dist = 0.2)
legend(0.3, 0.3, legend = c("ML", "REML", "OLS", "WLS"), lty = c(1, 1, 2, 2), lwd = c(1, 2,1, 2)) 
env.indep <- variog.mc.env(datageo, obj.var = vario.b)
env.model <- variog.model.env(datageo, obj.var = vario.b, model = vario.wls)
oldpar <- par(mfrow = c(1, 2))
plot(vario.b, envelope = env.indep)
plot(vario.b, envelope = env.model)
lines(vario.wls, lty = 2, lwd = 2, max.dist = 0.2)
par(oldpar) 

library(sm)
xv.wls <- xvalid(datageo, model = vario.wls)
summary(xv.wls)
xv.reml <- xvalid(datageo, model = vario.reml)
summary(xv.reml)
oldpar <- par(mfrow = c(2, 5))
plot(xv.wls, ask = FALSE)
par(oldpar)
oldpar <- par(mfrow=c(1,2)) 
plot(variog(datageo, max.dist = 0.2))
plot(variog(datageo, trend = ~coords, max.dist = 0.2)) 
par(oldpar)


xx <- seq(0, 1, l = 51)
yy <- seq(0, 1, l = 51)
pred.grid <- expand.grid(x = xx, y = yy) 
plot(datageo$coords, pch = 20)
points(pred.grid, pch = 3, cex = 0.2)

ko.wls <- krige.conv(s100, loc = pred.grid, krige = krige.control(obj.m = vario.wls))
names(ko.wls)


# GRAFICANDO SOLO LOS DIAS Y LA SEMANAS 

oldpar <- par(mfrow = c(1, 2))
image(ko.wls) #superficie de varianzas}xx <- seq(3.9, 4.28, l = 51)
xx <- seq(0,10, l = 51)
yy <- seq(0, 10, l = 51)
pred.grid <- expand.grid(x = xx, y = yy) 
title("Predicciones")
points(datageo$data , pch=20)
contour(ko.wls,add=T)


image(ko.wls , val  = ko.wls$krige.var) #superficie de varianzas
title("Superficie de varianzas")
xx <- seq(0, 30, l = 51)
yy <- seq(0, 30, l = 51)
pred.grid <- expand.grid(x = xx, y = yy) 
points(datageo$data, pch=20)
contour(ko.wls,val=sqrt(ko.wls$krige.var),add=T)

# COORDENADAS


oldpar <- par(mfrow = c(1, 2))
image(ko.wls) #superficie de varianzas
xx <- seq(3.9, 4.28, l = 51)
yy <- seq(-73.43, -73.83, l = 51)
pred.grid <- expand.grid(x = xx, y = yy) 
title("Predicciones")
points(datageo$coords , pch=20)
contour(ko.wls,add=T)


image(ko.wls , var = ko.wls$krige.var) #superficie de varianzas
title("Superficie de varianzas")
xx <- seq(3.9, 4.28, l = 51)
yy <- seq(-73.43, -73.83, l = 51)
pred.grid <- expand.grid(x = xx, y = yy) 
points(datageo$coords, pch=20)
contour(ko.wls,val=sqrt(ko.wls$krige.var),add=T)



