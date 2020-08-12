##SDM tutorial, for more info go here: https://rspatial.org/raster/sdm/2_sdm_occdata.html 
###Install and load packages required for the SDM
install.packages(c('raster', 'rgdal', 'dismo', 'rJava'))
install.packages("raster")
install.packages("rgeos")
install.packages("maptools")
install.packages("usdm")
install.packages("rlang")
install.packages("mapview")
install.packages("sdm")
installAll() #install all the dependent packages to the previously installed package, in this case sdm
library('raster')
library("rgdal")
library("dismo")
library("raster")
library("rgeos")
library("maptools")
library("usdm")
library("mapview")
library("rlang")
library("sdm")
library("devtools")

##DATA PREPARATION
#Load the data
acaule <- gbif("solanum", "acaule*", geo=FALSE)  #extracting data from gbif
#in the paranthesis put the species names required
#"geo=FALSE"- gets all records, geo=T will get only records with geographical data
lynx<-gbif("lynx","rufus*",geo=FALSE)# OR lynx example
data(acaule) # load the acaule data
colnames(acaule) #check column names
acgeo <- subset(acaule, !is.na(lon) & !is.na(lat)) #get data that has coordinates
dim(acgeo)#check how much data with coordinates you have
acgeo[1:4, c(1:5,7:10)] #to see column 1-4 and rows 1-5, 7-1

##Other places you can get data from
#http://portal.vertnet.org/search

###Create a map
data(wrld_simpl) #load world map
plot(wrld_simpl, xlim=c(-80,70), ylim=c(-85,90), axes=TRUE, col="light yellow") #for world coordinates
plot(wrld_simpl, xlim=c(-63,-55), ylim=c(-28,-19), axes=TRUE, col="light yellow")#for Paraguay coordinates
plot(wrld_simpl, xlim=c(-100,0), ylim=c(-70,20), axes=TRUE, col="light yellow")#for south america coordinates

box() # restore the box around the map
points(acgeo$lon, acgeo$lat, col='orange', pch=20, cex=0.75) #add data coordinates
points(acgeo$lon, acgeo$lat, col='green', cex=0.75) #give colour to the border
?getData #you can upload different country boundaries, better quality

###CORRECT COORDINATES
lonzero = subset(acgeo, lon==0) #find all the data with longitude of zero
dups <- duplicated(lonzero[, 1:10]) #find duplicates of the longitude zero 
lonzero  <-lonzero[dups, ] #remove all the duplicates from the lonzero data
dups2 <- duplicated(acgeo[, c('lon', 'lat')]) #detect all the duplicates (i.e in same coordinates) in the whole data
sum(dups2) #check how many points you have left
acg <- acgeo[!dups2, ] #keep the non duplicated records
# this data should only have coordinates in southern Peru, Bolivia and northern Argentina
# coordinates (latitude-NS -80 to -57, longitude WE -22 to -0)
#Therefore, any positive coordinates should be changed to negative
i <- acg$lon > 0 & acg$lat > 0 #creates a data where both coordinates are more than 0
acg$lon[i] <- -1 * acg$lon[i] #changes the longitude of the coordinates that are more than 0 to negative numbers
acg$lat[i] <- -1 * acg$lat[i] #same with latitude (This gets rid of wrong coordinates in Parkistan)
#With these removed now we want to get rid of coordinates in Antarctica and Brazil
acg <- acg[acg$lon < -50 & acg$lat > -50, ]
Occurences.PY<-acgeo[acgeo$country=="Paraguay",] #this chooses only points found in Paraguay
#Note: some coordinates may not have a country name, if there are any without country name
which(is.na(acgeo$country))

#Alternatively to remove duplicates
dups.1<-duplicated(lonzero[c("lat","lon")])
sum(dups.1) #to see how many duplicates you've got
Occurences<-Occurences[!dups,] #update your data to not have duplicates

#Remove fossil and captive data (if there is one), check for any other things
Occurences<-Occurrences[Occurences$isfossil==o]

#Double check coordinates with the coordinating countries. This allows to see spatial query of polygons
library(sp)
coordinates(acg) <- ~lon+lat #set your columns as coordinates
crs(acg) <- crs(wrld_simpl) #crs - coordinate reference system
class(acg) #prints the vector of names of classes "acg" inherits from
class(wrld_simpl)
ovr <- over(acg, wrld_simpl) #This spatialy overlays the data acg with the world simple data
head(ovr) #to see the result
cntr <- ovr$NAME ## To get variable ‘NAME’ in the data.frame of wrld_simpl
t <- which(is.na(cntr)) # find data with missing country names (the one's that have NA for the country column)
t #it will tell you how many you have, in this case the integer will be 0 because we have the data
j <- which(cntr != acg$country) #find points where coordinates and country do not match, "!=" means are not equal to
cbind(cntr, acg$country)[j,] # for the mismatches, bind the country names of the polygons and points
#Note one of the common mistakes is mistmaching points on the border of countries, due to wrld_simpl not being very precise
plot(acg) #plot the coordinates with all the cleaning up you've done
plot(wrld_simpl, add=T, border='blue', lwd=2) #"add=T" - this adds the map to the existing plot (i.e the points), "lwd" - line width
points(acg[j, ], col='red', pch=20, cex=2) #add the j coordinates to the map
#"cex"-size of the point, "pch"  - type of character http://coleoguy.blogspot.com/2016/06/symbols-and-colors-in-r-pch-argument.html

##To create coordiantes for data with country records with no coordinates, use georeferencing
georef <- subset(acaule, (is.na(lon) | is.na(lat)) & ! is.na(locality) ) # "subset"- selects only parts of the data you need, "is.na"- find data with no coordinates, but where locality is present
dim(georef) #returns dimentions of the georef, gives you number of records and columns
# to do georeferencing you need to get Google API_KEY

#To subsample your samples (to not be biased towards certain samples)
r <- raster(acg) #create a RasterLayer with the extent of acgeo (creates squares on the map)
res(r) <- 1 #set the resolution of the cells to (for example) 1 degree
r <- extend(r, extent(r)+1) ## expand (extend) the extent of the RasterLayer a little
acsel <- gridSample(acg, r, n=1) # to sample
p <- rasterToPolygons(r) # to see the results and illustrate the method
plot(p, border='gray')
points(acg)
points(acsel, cex=1, col='red', pch='x')

#Alternatively to thin the data use spThin package
thin( loc.data, lat.col = "LAT", long.col = "LONG", spec.col = "SPEC",
      thin.par, reps, locs.thinned.list.return = FALSE, write.files = TRUE,
      max.files = 5, out.dir, out.base = "thinned_data", write.log.file = TRUE, log.file = "spatial_thin_log.txt", verbose = TRUE )

##Make sure that coordinate system are the same
Occur.NAD27a<-Occurrences[Occurrences$geodeticdatum=="NAD27",]
Occur.NAD27b<-Occurrences[Occurrences$geodeticdatum=="North American Datum",]
Occur.NAD27<-rbind(Occur.NAD27a,Occur.NAD27b) #bird the observations together
coordinates(Occur.NAD27)<--dechimallongitude+decimallatitude #state that these are coordinates
projection(Occur.NAD27)<-crs('+proj=longlat +datum=NAD27') #give those coordinates a projection
NAD27.converted<-spTransform(Occur.NAD27,crs('+proj=longlat +datum=WGS84')) #convert the coordinates to WGS84

#Once you clean up all the data you can have data.frame with needed columns
acgeo$species1<-1 #add a new column, make sure that the name of the column is not already taken, make every value in that column 1
acgeo<-acgeo[,c('lon','lat','species1')] #choose the columns you want to keep
coordinates(acgeo)<-~lon+lat  #to convert this data to spatial dataframe, introduce the columns that has the geodata
class(acgeo) #double-check that it worked, it should say now "SpatialPointsDataFrame", also acgeo should have only one column now

### ABSENCE-PRESENCE
##this allows us to choose random points which would eliminate oversampling or undersampling error)
files <- list.files(path=paste(system.file(package="dismo"), '/ex',
                               sep=''),  pattern='grd',  full.names=TRUE ) #get the file names, "pattern=grd" - read files ending with .grd
# "sep'' " - reads a file in a table format and creates a data format for it
mask <- raster(files[1]) # create raster layer
set.seed(1963) #this makes sure that the examples are chosen at random, will produce 1963 random examples
bg <- randomPoints(mask, 500 ) #select 500 random points

###Plot a map (to inspect the results)
par(mfrow=c(1,1)) #set the parameters (i.e size of the map)
plot(!is.na(mask), legend=FALSE) #exclude all the points in the "mask" that have NA, "legend=FALSE" - don't show legend
points(bg, cex=0.5) #puts the points on the map
#Note that when your map space is too small the map and points will appear displaced

e <- extent(-80, -53, -39, -22) #this creates limits of the points, creating a red square of results(xmin=-80, xmax=-53, ymin=-39, ymax=-22)
bg2 <- randomPoints(mask, 50, ext=e) # selects 50 random points, using the limits we've set
plot(!is.na(mask), legend=FALSE)  #plot mask exluding NA data
plot(e, add=T, col='red') #add new limits (i.e the red box), "add=T" - adds map to the existing one
points(bg2, cex=0.5) #add points chosen will be the random one's we've assigned. To see current parameters write this "par()", "cex" - size of dots or text

##Example
file <- paste(system.file(package="dismo"), '/ex/acaule.csv', sep='') #Load acaule example
ac <- read.csv(file)
coordinates(ac) <- ~lon+lat #create coordinates
projection(ac) <- CRS('+proj=longlat +datum=WGS84') #change ac data from ac.frame to SpatialPointsDataFrame
#make these coordinates readable, createing coordinate reference system with projects of longitude and latitude, using goereferencing datum of WGS84

#Create diameter of locations
x <- circles(ac, d=50000, lonlat=TRUE) #"d" - radius/diameter, in this case 50km
pol <- polygons(x) #instead of having points for your data, you will not have  i.e you'll encase data within 50km of your points
samp1 <- spsample(pol, 250, type='random', iter=25) #randomly sample the circles
cells <- cellFromXY(mask, samp1) #get unique cells
length(cells) #check the number of the cells
cells <- unique(cells) #check number of unique cells
xy <- xyFromCell(mask, cells) #"xyFromCell"-get coordinates of the center of raster cells for a row, column
plot(pol, axes=TRUE)
points(xy, cex=0.75, pch=20, col='blue')

#Select only those cells that have their centers within the circles, using the overlay function
spxy <- SpatialPoints(xy, proj4string=CRS('+proj=longlat +datum=WGS84'))
o <- over(spxy, geometry(x))
xyInside <- xy[!is.na(o), ]

v <- extract(mask, x@polygons, cellnumbers=T)
v <- do.call(rbind, v) # use rbind to combine the elements in list v
v <- unique(v[,1]) # get unique cell numbers from which you could sample
head(v)
m <- mask
m[] <- NA #this tells you to not select NA subsets? I think
m[v] <- 1
plot(m, ext=extent(x@polygons)+1)
plot(x@polygons, add=T)

##ENVIRONMENTAL DATA - pull up maps
##Note: make sure that spatial extent, resolution, origin, and projection are the same
#For more go to dismo package: https://cran.r-project.org/web/packages/dismo/dismo.pdf
# OR how it is calculates: https://pubs.usgs.gov/ds/691/ds691.pdf
path <- file.path(system.file(package="dismo"), 'ex') #get the file "ex"
files <- list.files(path, pattern="grd$", full.names=TRUE ) #"choose files that end with grd ($ means ends with)", "full.names" - returns the full path names
predictors <- stack(files) #Stack merges all the files 
#This will load predictable values for environmental changes
predictors
names(predictors)
plot(predictors)

###PLOT species environmental data
data(wrld_simpl)
file <- paste(system.file(package="dismo"), "/ex/bradypus.csv", sep="") #pull up bradypus data and dismo package has bioclimatic data
bradypus <- read.table(file,  header=TRUE,  sep=',') 
bradypus  <- bradypus[,-1] #get rid of the first column with bradypus species names
plot(predictors,1) #1 is the number of the biological data you want to load, in this case temperature (i.e bio1)
plot(wrld_simpl, add=TRUE)
points(bradypus, col='red') #plug the bradypus numbers
#For more datasets or higher resolution go to WWF:https://www.worldwildlife.org/pages/conservation-science-data-and-tools
#Or here for climatic data: https://www.worldclim.org/data/index.html

#Creating predictors for boclimatic variables
climate<-getData('worldclim',download=T,var='bio',res=2.5) #get climate data, var - variables, res-resolution, try putting 10
plot(climate)
obsclim<-extract(climate,Occur) #extract climate data for each one of the observation points (of the Occurence data)
#Create species distribution model using bioclim
bioclim.model<-bioclim(obsclim)
pairs(bioclim.model,pa='p')
#Predictors - creates a map of suitable areas, taking into account the biomes you want it to include
predictors<-stack(climate$bio1,climate$bio2,climate$bio3) #add ass many as you want
predictions<-predict(predictors,bioclim.model)
plot(predictions,xlim=c(-63,-55), ylim=c(-28,-19),axes=T)

##Extract environemntal values from rasters
presvals <- extract(predictors, bradypus) #predictors (predictable environemntal data) and bradypus data
set.seed(0) #setting a random value
backgr <- randomPoints(predictors, 500) #select 500 random points
absvals <- extract(predictors, backgr) #extract numbers for the background points
pb <- c(rep(1, nrow(presvals)), rep(0, nrow(absvals)))
sdmdata <- data.frame(cbind(pb, rbind(presvals, absvals))) #first column variable is "pb", this indicates whether its presence or a backgrond point (i.e bradypus present or not)
sdmdata[,'biome'] = as.factor(sdmdata[,'biome'])
head(sdmdata) #see the data
tail(sdmdata) #see the data
pairs(sdmdata[,2:5], cex=0.1) #to see colinearity within the environmental data
#Save data for the next steps
saveRDS(sdmdata, "sdm.Rds")
saveRDS(presvals, "pvals.Rds")

##CREATE A MAP (+IMPROVE IT)

mapview(acgeo)
#if the projection doesn't work i.e it's not showing where its supposed to be
proj4string(acgeo) #to check what the projection is 
proj4string(acgeo)<-projection(raster()) 
#raster function without any argument creates an empty raster with a Geographic coordinate system
#projection(x) reads the coordinate reference system of the object x
#projection(raster()) resutns the coordinate reference system for the object created by raster()which is the one 
#we needed for or sp object i.e the definition for the geographic coordinate reference system
mapview(acgeo)

## MODEL FITTING, PREDICTION, AND EVALUATION
#Calculate variance inflation factor (VIF) for a set of variables and exclude the highly correlated variables
#When bio data is corelated e.g high humidity and rain, they can have an excesively high effect on the data
#whilst in reality they are almost the same thing (kind of)
bio<-raster::getData('worldclim',var='bio',res=10) #extract biome data (new way) or use previous methods

#Reduce collinearity
v1<-vifstep(bio) #you can choose either this or the vifcor one below. Both are good. 
v2<-vifcor(bio,th=0.9) #tests correlation, if greater than 0.9 - need to remove
v1
v2 #to check results. Tells you how many variables from the input are collinearity problem
biom<-exclude(bio,v2) #then you choose to exlude those variables, in dimensions the number before the bracket says how many variables it has
#Plot to check (the species data to see the distribution)
plot(biom[[1]]) #plot biom data only bio1 figure
points(acgeo,cex=0.5,pch=16)

#Load saved data
sdmdata <- readRDS("sdm.Rds")
presvals <- readRDS("pvals.Rds")

#model fitting
m1 <- glm(pb ~ bio1 + bio5 + bio12, data=sdmdata) #how does the presence data correlate with the bio data (1,5, and 12)
#for meanings for the biomes go here:
class(m1) #prints/checks the vector of names of classes an object has, in this case generalised linear model and linear model
summary(m1)
m2 = glm(pb ~ ., data=sdmdata) #create general linear model  between the presence and other data?
m2 #see data

#bioclim only takes presence data, so presvals is used instead of "sdmdata"
bc <- bioclim(presvals[,c('bio1', 'bio5', 'bio12')]) 
#extracted presvals (bradypus data on where it is present and environmental data), but only columns bio1, bio5, bio12
bc #show data
pairs(bc) #show the plot matrix 

#Model prediction
bio1 = c(40, 150, 200)
bio5 = c(60, 115, 290)
bio12 = c(600, 1600, 1700)
pd = data.frame(cbind(bio1, bio5, bio12))
pd
predict(m1, pd) #m1 - glm model object
predict(bc, pd) #bc - bioclimatic model
response(bc) #creates response plots for each variable

#Create a map of predictors
predictors <- stack(list.files(file.path(system.file(package="dismo"), 'ex'), pattern='grd$', full.names=TRUE ))
#This creates predictable model for the bio data
p <- predict(predictors, m1) #predict presence and bio data
plot(p) #map of suitability scores, the darkest means more chances of having presence of bradypus?
 
#Model evaluation
###Test quality of the prediction
p <- rnorm(50, mean=0.7, sd=0.3) #rnorm - random numbers from normal distribution
#presence has higher values, shows prediction for 50 known cases/locations where the species is present
a <- rnorm(50, mean=0.4, sd=0.4)
#same as presence, but for when the species is absent
par(mfrow=c(1, 2)) #set parameters, 1 row, 2 columns (for the plot)
plot(sort(p), col='red', pch=21) #sort a vector into accending/descending order
points(sort(a), col='blue', pch=24)
legend(1, 0.95 * max(a,p), c('presence', 'absence'),
       pch=c(21,24), col=c('red', 'blue')) #creates a legend
comb <- c(p,a) #combine both data
group <- c(rep('presence', length(p)), rep('absence', length(a))) #length - sets the length of a vector 
boxplot(comb~group, co=c('blue', 'red')) #combined data vs grouped length
##Compute the correlation coefficient and the AUC
group = c(rep(1, length(p)), rep(0, length(a)))
cor.test(comb, group)$estimate
mv <- wilcox.test(p,a)
auc <- as.numeric(mv$statistic) / (length(p) * length(a))
auc

#Another example of testing prediction
e <- evaluate(p=p, a=a)
class(e)
e
par(mfrow=c(1, 2))
density(e)
boxplot(e, col=c('blue', 'red'))

###Bioclimatic example
samp <- sample(nrow(sdmdata), round(0.75 * nrow(sdmdata))) #
traindata <- sdmdata[samp,]
traindata <- traindata[traindata[,1] == 1, 2:9]
testdata <- sdmdata[-samp,]
bc <- bioclim(traindata)
e2 <- evaluate(testdata[testdata==1,], testdata[testdata==0,], bc)
e2
plot(e, 'ROC')

pres <- sdmdata[sdmdata[,1] == 1, 2:9]
back <- sdmdata[sdmdata[,1] == 0, 2:9]
k <- 5
group <- kfold(pres, k) #partition the data into 5 groups
group[1:10]
unique(group)

##fit and test our model five times.
##In each run, the records corresponding to one of the five groups is only used to evaluate the model,
##while the other four groups are only used to fit the model. 
e <- list() #results will be stored in a list called ‘e’.
for (i in 1:k) {
  train <- pres[group != i,]
  test <- pres[group == i,]
  bc <- bioclim(train)
  e[[i]] <- evaluate(p=test, a=back, bc)
}
## Extract AUC values and the “maximum of the sum of the sensitivity (true positive rate) and specificity (true negative rate)” threshold “spec_sens”
##(this is sometimes uses as a threshold for setting cells to presence or absence).
auc <- sapply(e, function(x){x@auc})
auc
mean(auc)
sapply( e, function(x){ threshold(x)['spec_sens'] } )

##
file <- file.path(system.file(package="dismo"), "ex/bradypus.csv")
bradypus <- read.table(file,  header=TRUE,  sep=',') #read the data
bradypus <- bradypus[,-1] #get rid of unessesary columns
presvals <- extract(predictors, bradypus) #
set.seed(0)
backgr <- randomPoints(predictors, 500)
nr <- nrow(bradypus)
s <- sample(nr, 0.25 * nr)
pres_train <- bradypus[-s, ]
pres_test <- bradypus[s, ]
nr <- nrow(backgr)
set.seed(9)
s <- sample(nr, 0.25 * nr)
back_train <- backgr[-s, ]
back_test <- backgr[s, ]
sb <- ssb(pres_test, back_test, pres_train)
sb[,1] / sb[,2] #sb[,1] / sb[,2] is an indicator of spatial sorting bias (SSB).
#If there is no SSB this value should be 1, in these data it is close to zero, indicating that SSB is very strong.

#Example of when SSB is removed
i <- pwdSample(pres_test, back_test, pres_train, n=1, tr=0.1)
pres_test_pwd <- pres_test[!is.na(i[,1]), ]
back_test_pwd <- back_test[na.omit(as.vector(i)), ]
sb2 <- ssb(pres_test_pwd, back_test_pwd, pres_train)
sb2[1]/ sb2[2]
bc <- bioclim(predictors, pres_train)
evaluate(bc, p=pres_test, a=back_test, x=predictors)
evaluate(bc, p=pres_test_pwd, a=back_test_pwd, x=predictors) #notice how AUC dropped

##MODELING METHODS

##Method 1 of modelling for PRESENCE ONLY DATA
head(acgeo) #double check that your data only has species column
d<-sdmData(species1~.,acgeo, predictors=biom) #specify species (the name of the colum) against everything else
#predictors would be the biom
d #this will show you that this is presence only data

#Method 2 of modelling for Presence-Absence data
head(acgeo) #double check that your data only has species column
d<-sdmData(species1~., acgeo, predictors=biom,bg=list(n=1000)) #specify species (the name of the colum) against everything else
#predictors would be the biom, "bg=" - background creates absence data
#where 1000 is the number of pseudoabsences, usually generate twice the number of existing presence number
d #now you can see that it's presence-background data
#Note that you might lose some values due to this formula getting rid of duplications or any missing values
m<-sdm(species1~., d, methods = c('glm','svm','rf','brt','mars'),
       replication=c('boot'),n=2)
#choose the column/dataset name -species1
#we use four different methods to fit the models
#for boostrapping the minimum number of samples is 400 to get p value of 0.05, for 0.01 15000
m #to see the results
#Look at AUC values. the higher they are the higher theprobability that the fit model will score a randomly drawn positive sample
#Note that the higher you put your pseudoabsences doesn't make the result more reliable since those are not real absences

#Prediction building
#First option
p<-predict(m,biom,'predictions.img', mean =T) 
#predictions - file name in which your data is saved in your working directory
#"mean=T", if you have several rounds for each replication
#(e.g for example we have 1000 for bg - you will get an average over all rounds for each technique)
#Second option
en<-ensemble(m,biom,'ens.img',setting=list(method='weights',stat="AUC"))
#for this function you need to specify some setting. 
#Method used. Choices: 1.unweighted - taken averages over all the methods,
#2. weighted - gives higher weights to the methods that have higehr accuracy - for this you need to specify statistics e.g AUC
#AUC is threshold independent
#OR
en<-ensemble(m,biom,'ens.img',setting=list(method='weights',stat="TSS",opt=2,overwrite=TRUE))
#use TSS
gui(m) #look at the output of the model

#To download variables for the future time
biof<-raster::getData('CMIP5',var='bio',res=10,rcp=85,year=70,model='AC')
#to get data from wordclim you need to use CMIPS as the name of the dataset
#res-resolution, rcp scenario, year =70
names(biof)<-names(bio) #becase if you check the names of biof they will be changed to more complicated ones
?getData #to get more info on variations

#Plot that model of the potential current distribution of the species
plot(p[[1]]) #the number stands for the model that you are trying
#in this case 1=glm, 5-mars
plot(p[[c(1:5)]])

#Plot the map for the future
pf<-predict(m,biof,'predictionsf.img')
enf<-ensemble(m,biof,'ensf.img',setting=list(method='weights',stat="TSS",opt=2))
plot(stack(en,enf)) #stack current and future to compare
plot (enf) #to see the future
mapview(stack(en,enf)) #you stack the probability for the current time and the future time
#on the map (its interactive), you can see the layer 1 - current, 2 future

#Calculate the changes/difference in probability
changes<-enf-en
plot(changes) 
#the plus values (over zero) - are the ones that gain suitability
#under 0 are the ones that lost suitability, 0 is no change

##FOR MAP OF PRESENCE-ABSENCE (e.g only 1 and 0 numbers)
#this one is for the current
ev<-getEvaluation(m,stat=c('AUC','TCC','threshold'),opt=2) #Get statistics that you want
#opt - specifies which optimisation value you want to use e.g 2
mean(ev$threshold) #gives result of the mean of the threshold e.g 0.5
pa<-raster(en) #create an empty raster 
pa[]<-ifelse(en[] >=0.5,1,0) #0.5 is the value of the mean threshold see the top, change depending on your value
#if the value of en is greater of equal to that number than the answer will be 1, otherwise 0
plot (pa)
#For future time
paf<-raster(enf) #create an empty raster 
paf[]<-ifelse(enf[] >=0.05,1,0) #if the values of en, 0.5 the value of the mean threshold
#if the value is greater of equal to that number than the answer will be 1, otherwise 0
plot (paf)
#Compare those two
pac<-paf-pa
cl<-colorRampPalette(c('red','gray','darkgreen')) #set the pallete: red -extinction, gray-no change, darkgreen- colonisation
plot(pac,col=cl(3)) #the results is either 1,0,-1, which means colonisation, didn't change, lost respectively

##Method 3 of modelling
##Types of organisms
##Create data
data(wrld_simpl)
predictors <- stack(list.files(file.path(system.file(package="dismo"), 'ex'), pattern='grd$', full.names=TRUE ))
file <- file.path(system.file(package="dismo"), "ex/bradypus.csv")
bradypus <- read.table(file,  header=TRUE,  sep=',')
bradypus <- bradypus[,-1]
presvals <- extract(predictors, bradypus)
set.seed(0)
backgr <- randomPoints(predictors, 500)
absvals <- extract(predictors, backgr)
pb <- c(rep(1, nrow(presvals)), rep(0, nrow(absvals)))
sdmdata <- data.frame(cbind(pb, rbind(presvals, absvals)))
sdmdata[,'biome'] <- as.factor(sdmdata[,'biome'])

#Responce of species to each variable (bios)
rcurve(m,id=9) #plot that shows responce curve - shows at which range the species would increase/descrease, range of survival
#id - is the number of the model used. Note the curve does NOT show importance of the variables itself
plot(sdm::getVarImp(m,9)) #to see how important different variables(i.e bios) are

#Pre-model data clean up
pred_nf <- dropLayer(predictors, 'biome') #drop categorical values
set.seed(0)
group <- kfold(bradypus, 5) #kfold - partitioning of a data set for model testing purposes, 5 - number of groups
pres_train <- bradypus[group != 1, ] # group is not equal to 1 (as in absent?)
pres_test <- bradypus[group == 1, ]# group is equal to 1 (as in present?)
ext <- extent(-90, -32, -33, 23) #restrict the area coordinates

#The first layer in the RasterStack is used as a ‘mask’.
#That ensures that random points only occur within the spatial extent of the rasters,
# and within cells that are not NA, and that there is only a single absence point per cell. 
set.seed(10)
backg <- randomPoints(pred_nf, n=1000, ext=ext, extf = 1.25)
#restrict the background points to be within 12.5% of our specified extent ‘ext’
colnames(backg) = c('lon', 'lat')
group <- kfold(backg, 5)
backg_train <- backg[group != 1, ]
backg_test <- backg[group == 1, ]

r <- raster(pred_nf, 1)
plot(!is.na(r), col=c('white', 'light grey'), legend=FALSE)  #create a map with white background, gray continents
plot(ext, add=TRUE, col='red', lwd=2) #create limits for the restricted coordinates
points(backg_train, pch='-', cex=0.5, col='yellow') #points for baground train
points(backg_test, pch='-',  cex=0.5, col='black')#points for baground test
points(pres_train, pch= '+', col='green') #bradypus absent?
points(pres_test, pch='+', col='blue') #bradypus present? 

##Profile methods:
#Bioclimatic method - used extensively in sdm, great model, not as great for the climate change, easiest to use
bc <- bioclim(pred_nf, pres_train) #we fit a bioclim model simply using the predictors and the occurrence points
plot(bc, a=1, b=2, p=0.85)
e <- evaluate(pres_test, backg_test, bc, pred_nf) #evaluate the model
e
tr <- threshold(e, 'spec_sens') #find a threshold
tr
pb <- predict(pred_nf, bc, ext=ext, progress='') #RasterStack with predictor variables to make a prediction to a RasterLayer
pb
#Plot
par(mfrow=c(1,2)) #2 graphs
plot(pb, main='Bioclim, raw values') #bioclimate data
plot(wrld_simpl, add=TRUE, border='dark grey')
plot(pb > tr, main='presence/absence') #map w/presence and absence
plot(wrld_simpl, add=TRUE, border='dark grey') #create country borders
points(pres_train, pch='+') #present data bradypus coordinates

#Domain method - commonly used, bad in terms of climate change, poor in model comparison
