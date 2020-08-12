#Tutorial here: https://rspatial.org/raster/sdm/6_sdm_methods.html
library(dismo)
library(maptools)
data(wrld_simpl)
predictors <- stack(list.files(file.path(system.file(package="dismo"), 'ex'), pattern='grd$', full.names=TRUE ))
file <- file.path(system.file(package="dismo"), "ex/bradypus.csv")
presvals <- extract(predictors, data1)
set.seed(0)
backgr <- randomPoints(predictors, 500)
absvals <- extract(predictors, backgr)
pb <- c(rep(1, nrow(presvals)), rep(0, nrow(absvals)))
sdmdata <- data.frame(cbind(pb, rbind(presvals, absvals)))
sdmdata[,'biome'] <- as.factor(sdmdata[,'biome'])
pred_nf <- dropLayer(predictors, 'biome')

set.seed(0)
group <- kfold(data1, 5)
pres_train <- data1[group != 1, ]
pres_test <- data1[group == 1, ]

ext <- extent(-90, -32, -33, 23)
set.seed(10)
backg <- randomPoints(pred_nf, n=1000, ext=ext, extf = 1.25)
colnames(backg) = c('lon', 'lat')
group <- kfold(backg, 5)
backg_train <- backg[group != 1, ]
backg_test <- backg[group == 1, ]

r <- raster(pred_nf, 1)
plot(!is.na(r), col=c('white', 'light grey'), legend=FALSE) #Plot a map
plot(ext, add=TRUE, col='red', lwd=2)
points(backg_train, pch='-', cex=0.5, col='yellow')
points(backg_test, pch='-',  cex=0.5, col='black')
points(pres_train, pch= '+', col='green')
points(pres_test, pch='+', col='blue')

sdata3<-sdmData(species~., data3, predictors=biom,bg=backg_test(n=10000)) #specify species (the name of the column) against everything else



bc <- bioclim(pred_nf, pres_train)
e <- evaluate(pres_test, backg_test, bc, pred_nf)
e

tr <- threshold(e, 'spec_sens')
pb <- predict(pred_nf, bc, ext=ext, progress='')
par(mfrow=c(1,2))
plot(pb, main='Bioclim, raw values')
plot(wrld_simpl, add=TRUE, border='dark grey')
plot(pb > tr, main='presence/absence')
plot(wrld_simpl, add=TRUE, border='dark grey')
points(pres_train, pch='+')