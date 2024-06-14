
#----------------------------------------------------------------------
par(mfrow=c(2,3), mar = c(3, 3, 0.5, 0.5), mgp=c(2, 0.8, 0))
#----------------------------------------------------------------------

# Exemplo 1: crescimento de planta
plantas <- data.frame(x = 1:7, y = c(5,13,16,23,33,38,40))
plot(y~x, data=plantas, xlab = 'Idade da planta (semanas)',
     ylab = 'Altura (cm)', pch = 20, cex = 1.4, ylim = c(0,45),
     col = 'grey60')

x.pred <- data.frame(x = seq(0, 8, 0.05))

## Regressão linear simples
abline(lm(y~x, data=plantas), lwd = 2)

## Regressão ponimial de grau 3
y3 <- lm(y ~ poly(x, 3), data = plantas)
x.pred$x3 <- predict(y3, newdata = x.pred)
with(x.pred, lines(x, x3, col = 'blue', lwd = 2))

## Regressão ponimial de grau 6
y6 <- lm(y ~ poly(x, 6), data = plantas)
x.pred$x6 <- predict(y6, newdata = x.pred)
with(x.pred, lines(x, x6, col = 'red', lwd = 2))

## Loess
yloess <- loess(y~x, data = plantas)
x.pred$xloess <- predict(yloess, newdata = x.pred)
with(x.pred, lines(x, xloess, col = '#008000', lwd = 2))

## Spline
library(splines)
yspline <- lm(y~bs(x), data = plantas)
x.pred$xspline <- predict(yspline, newdata = x.pred)
with(x.pred, lines(x, xspline, col = '#4B0082', lwd = 2))

## Segmentada
library(segmented)
fit <- lm(y ~ x, data=plantas)
segmented.fit <- segmented(fit, seg.Z = ~x)
plot(segmented.fit, add=T, col = '#D2691E', lwd = 2)

#----------------------------------------------------------------------

# Exemplo 2: corrosão para diferentes teores de ferro 
require(faraway)
data(corrosion)
plot(loss~Fe, data=corrosion, pch = 20, cex = 1.4,col = 'grey60')

Fe.pred <- data.frame(Fe = seq(0, 2, 0.05))

## Regressão linear simples
abline(lm(loss~Fe, data=corrosion), lwd = 2)

## Regressão ponimial de grau 3
Fe3 <- lm(loss ~ poly(Fe, 3), data = corrosion)
Fe.pred$Fe3 <- predict(Fe3, newdata = Fe.pred)
with(Fe.pred, lines(Fe, Fe3, col = 'blue', lwd = 2))

## Regressão ponimial de grau 6
Fe6 <- lm(loss ~ poly(Fe, 6), data = corrosion)
Fe.pred$Fe6 <- predict(Fe6, newdata = Fe.pred)
with(Fe.pred, lines(Fe, Fe6, col = 'red', lwd = 2))

## Loess
lossloess <- loess(loss~Fe, data = corrosion)
Fe.pred$Feloess <- predict(lossloess, newdata = Fe.pred)
with(Fe.pred, lines(Fe, Feloess, col = '#008000', lwd = 2))

## Spline
library(splines)
lossspline <- lm(loss~bs(Fe), data = corrosion)
Fe.pred$Fespline <- predict(lossspline, newdata = Fe.pred)
with(Fe.pred, lines(Fe, Fespline, col = '#4B0082', lwd = 2))

## Segmentada
fit <- lm(loss ~ Fe, data=corrosion)
segmented.fit <- segmented(fit, seg.Z = ~Fe)
plot(segmented.fit, add=T, col = '#D2691E', lwd = 2)

#----------------------------------------------------------------------

## Exemplo 3: peso corporal e do coração de gatos
require(MASS)
data(cats) 
plot(Hwt ~ Bwt, data = cats, xlab = 'Peso corporal (kg)',
     ylab = 'Peso do coração (g)', pch = 20, cex = 1.4, col = 'grey60')

Bwt.pred <- data.frame(Bwt = seq(1, 5, 0.05))

## Regressão linear simples
abline(lm(Hwt~Bwt, data=cats), lwd = 2)

## Regressão ponimial de grau 3
Hwt3 <- lm(Hwt ~ poly(Bwt, 3), data = cats)
Bwt.pred$Bwt3 <- predict(Hwt3, newdata = Bwt.pred)
with(Bwt.pred, lines(Bwt, Bwt3, col = 'blue', lwd = 2))

## Regressão ponimial de grau 6
Hwt6 <- lm(Hwt ~ poly(Bwt, 6), data = cats)
Bwt.pred$Bwt6 <- predict(Hwt6, newdata = Bwt.pred)
with(Bwt.pred, lines(Bwt, Bwt6, col = 'red', lwd = 2))

## Loess
Hwtloess <- loess(Hwt~Bwt, data = cats)
Bwt.pred$Bwtloess <- predict(Hwtloess, newdata = Bwt.pred)
with(Bwt.pred, lines(Bwt, Bwtloess, col = '#008000', lwd = 2))

## Spline
library(splines)
Hwtspline <- lm(Hwt~bs(Bwt), data = cats)
Bwt.pred$Bwtspline <- predict(Hwtspline, newdata = Bwt.pred)
with(Bwt.pred, lines(Bwt, Bwtspline, col = '#4B0082', lwd = 2))

## Segmentada
fit <- lm(Hwt ~ Bwt, data=cats)
segmented.fit <- segmented(fit, seg.Z = ~Bwt, npsi = 2)
plot(segmented.fit, add=T, col = '#D2691E', lwd = 2)

#----------------------------------------------------------------------

# Exemplo 4: tempo de erupção vs intervalo de tempo entre erupções
plot(eruptions ~ waiting, data = faithful, pch = 20, cex = 1.4, col = 'grey60',
     xlab = 'Tempo desde a última erupção',
     ylab = 'Duração da erupção')

waiting.pred <- data.frame(waiting = seq(40, 100, 0.05))

## Regressão linear simples
abline(lm(eruptions~waiting, data=faithful), lwd = 2)

## Regressão ponimial de grau 3
eruptions3 <- lm(eruptions ~ poly(waiting, 3), data = faithful)
waiting.pred$waiting3 <- predict(eruptions3, newdata = waiting.pred)
with(waiting.pred, lines(waiting, waiting3, col = 'blue', lwd = 2))

## Regressão ponimial de grau 6
eruptions6 <- lm(eruptions ~ poly(waiting, 6), data = faithful)
waiting.pred$waiting6 <- predict(eruptions6, newdata = waiting.pred)
with(waiting.pred, lines(waiting, waiting6, col = 'red', lwd = 2))

## Loess
eruptionsloess <- loess(eruptions~waiting, data = faithful)
waiting.pred$waitingloess <- predict(eruptionsloess, newdata = waiting.pred)
with(waiting.pred, lines(waiting, waitingloess, col = '#008000', lwd = 2))

# Splines
library(splines)
library(splines)
eruptionsspline <- lm(eruptions~bs(waiting), data = faithful)
waiting.pred$waitingspline <- predict(eruptionsspline, newdata = waiting.pred)
with(waiting.pred, lines(waiting, waitingspline, col = '#4B0082', lwd = 2))

## Segmentada
fit <- lm(eruptions ~ waiting, data=faithful)
segmented.fit <- segmented(fit, seg.Z = ~waiting, npsi = 2)
plot(segmented.fit, add=T, col = '#D2691E', lwd = 2)

#----------------------------------------------------------------------

## Exemplo 5: escore está relacionado com a renda?
require(AER)
data("CASchools")
CASchools$lincome <- log(CASchools$income)
plot(math~lincome, data=CASchools,
     xlab = 'Renda média do distrito (em log(U$1.000))',
     ylab = 'Escore médio (Matemática)',
     pch = 20, col = 'grey60', cex = 1.4)

lincome.pred <- data.frame(lincome = seq(1.6, 4.1, 0.05))

## Regressão linear simples
abline(lm(math~lincome, data=CASchools), lwd = 2)

## Regressão ponimial de grau 3
math3 <- lm(math ~ poly(lincome, 3), data = CASchools)
lincome.pred$lincome3 <- predict(math3, newdata = lincome.pred)
with(lincome.pred, lines(lincome, lincome3, col = 'blue', lwd = 2))

## Regressão ponimial de grau 6
math6 <- lm(math ~ poly(lincome, 6), data = CASchools)
lincome.pred$lincome6 <- predict(math6, newdata = lincome.pred)
with(lincome.pred, lines(lincome, lincome6, col = 'red', lwd = 2))

## Loess
mathloess <- loess(math~lincome, data = CASchools)
lincome.pred$lincomeloess <- predict(mathloess, newdata = lincome.pred)
with(lincome.pred, lines(lincome, lincomeloess, col = '#008000', lwd = 2))

## Spline
library(splines)
mathspline <- lm(math~bs(lincome), data = CASchools)
lincome.pred$xspline <- predict(mathspline, newdata = lincome.pred)
with(lincome.pred, lines(lincome, xspline, col = '#4B0082', lwd = 2))

## Segmentada
fit <- lm(math ~ lincome, data=CASchools)
segmented.fit <- segmented(fit, seg.Z = ~lincome, npsi = 3)
plot(segmented.fit, add=T, col = '#D2691E', lwd = 2)

#----------------------------------------------------------------------

# Exemplo 6: energia gerada para diferentes velocidades de vento
eolica <- data.frame(
  wind = c(5.00, 6.00, 3.40, 2.70, 10.0, 9.70, 9.55, 3.05, 8.15, 6.20, 2.90, 6.35, 4.60, 5.80, 7.40, 3.60, 7.85, 8.80, 7.00, 5.45, 9.10, 10.2, 4.10, 3.95, 2.45),
  energy = c(1.582, 1.822, 1.057, 0.500, 2.236, 2.386, 2.294, 0.558, 2.166, 1.866, 0.653, 1.930, 1.562, 1.737, 2.088, 1.137, 2.179, 2.112, 1.800, 1.501, 2.303, 2.310, 1.194, 1.144, 0.123)#^2
)
plot(energy ~ wind, data = eolica,
     pch = 20, cex = 1.4, las = 1, col = 'grey60')

wind.pred <- data.frame(wind = seq(2, 11, 0.05))

## Regressão linear simples
abline(lm(energy~wind, data=eolica), lwd = 2)

## Regressão ponimial de grau 3
energy3 <- lm(energy ~ poly(wind, 3), data = eolica)
wind.pred$wind3 <- predict(energy3, newdata = wind.pred)
with(wind.pred, lines(wind, wind3, col = 'blue', lwd = 2))

## Regressão ponimial de grau 6
energy6 <- lm(energy ~ poly(wind, 6), data = eolica)
wind.pred$wind6 <- predict(energy6, newdata = wind.pred)
with(wind.pred, lines(wind, wind6, col = 'red', lwd = 2))

## Loess
energyloess <- loess(energy~wind, data = eolica)
wind.pred$windloess <- predict(energyloess, newdata = wind.pred)
with(wind.pred, lines(wind, windloess, col = '#008000', lwd = 2))

## Splines
library(splines)
energyspline <- lm(energy~bs(wind), data = eolica)
wind.pred$windspline <- predict(energyspline, newdata = wind.pred)
with(wind.pred, lines(wind, windspline, col = '#4B0082', lwd = 2))

## Segmentada
fit <- lm(energy ~ wind, data=eolica)
segmented.fit <- segmented(fit, seg.Z = ~wind, npsi = 2)
plot(segmented.fit, add=T, col = '#D2691E', lwd = 2)

#----------------------------------------------------------------------
