##BRADYPUS EXAMPLE

###1.Install and load packages required for the SDM
install.packages(c('raster', 'rgdal', 'dismo', 'rJava'))
install.packages("raster")
install.packages("rgeos")
install.packages("maptools")
install.packages("usdm")
install.packages("rlang")
install.packages("mapview")
install.packages("sdm")
install.packages("jsonlite")
install.packages("rgbif")
install.packages("rJava")
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
library("rgbif")

#Note replace data1 with the name of your data
###2.LOAD THE DATA
#note when downloading from gbif, make sure to write these geo=TRUE, sp=TRUE
data1<-gbif("Blastocerus", "dichotomus*", geo=FALSE)
#2.1  Double check that they all have the same coordinate system (in the column), if not change it (see SDM tutorial)

###3.PREP THE DATA (SEE SDM TUTORIAL)
#3.1 choose only data with coordinates
data1 <- subset(data1, !is.na(lon) & !is.na(lat))
#3.2 Delete coordinates that are in the wrong place - or reposition them
#3.3 Delete duplicates
#3.4 Check that coordinate system is the same for all of your coordiantes
#3.5 Load the map to see the coordinates
data(wrld_simpl) #load world map
#Check that there are no coordinates where they are not supposed to be with plotting
#If they are go to SDM tutorials-> CORRECT COORDINATES to correct it
plot(wrld_simpl, xlim=c(-80,70), ylim=c(-85,90), axes=TRUE, col="light yellow") #for world coordinates
plot(wrld_simpl, xlim=c(-80,70), ylim=c(-60,10), axes=TRUE, col="light yellow") #for south america coordiantes
points(data1$lon, data1$lat, col='orange', pch=20, cex=0.75) #plot your data

# 3.6 Get rid of duplicates
dups <- duplicated(data1[, c('lon', 'lat')]) #detect all the duplicates (i.e same coordinates) in the whole data
sum(dups) #check how many points you have left
data1<- data1[!dups, ] #keep the non duplicated records

#3.7 Choose columns you need
data1$species<-1 #add a new column, make sure that the name of the column is not already taken, make every value in that column 1
data1<-data1[,c('lon','lat','species')] #choose the columns you want to keep
data3<-data1 #save this data for the thinning before you convert it to spatial coordinates
# 3.8 Convert data to spatial dataframe - to be able to read coordinates
coordinates(data1)<-~lon+lat  #to convert this data to spatial dataframe, introduce the columns that has the geodata
class(data1) #double-check that it worked, it should say now "SpatialPointsDataFrame", also acgeo should have only one column now
# 3.9 Map the data again to double check all is working (see point 4)
head(data1) #double check that your data only has species column


#3.10 Reduce data collection bias
#Subsample - to reduce sampling bias
#To subsample your samples (to not be biased towards certain samples)
r <- raster(data1) #create a RasterLayer with the extent of acgeo (creates squares on the map)
res(r) <- 1 #set the resolution of the cells to (for example) 1 degree
r <- extend(r, extent(r)+1) ## expand (extend) the extent of the RasterLayer a little
data2 <- gridSample(data1, r, n=1) # to sample
p <- rasterToPolygons(r) # to see the results and illustrate the method
plot(p, border='gray')
points(data1)
points(data2, cex=1, col='red', pch='x')

#OR data thinning using spthin
#Example here: https://cran.csiro.au/web/packages/spThin/vignettes/spThin_vignette.html
install.packages("spThin")
library("spThin")
thindataset2<-thin( loc.data= data3, lat.col = "lat", long.col = "lon", spec.col = "species", thin.par=10, reps=100, locs.thinned.list.return = TRUE,
      write.files = FALSE, write.log.file = FALSE)

### 4. Prep environmental data
#4.1 Get bio data
bio<-raster::getData('worldclim',var='bio',res=10) #extract biome data
#4.2 Reduce colinearity within bio data
v2<-vifcor(bio,th=0.9) #tests correlation, if greater than 0.9 - need to remove
v2 #to check results. Tells you how many variables from the input are collinearity problem
biom<-exclude(bio,v2) # to exlude all the data that's under th=0.9
#4.3 Double check bio data and species data
plot(biom[[1]]) #plot biom data only bio1 figure
points(data1,cex=0.5,pch=16)
mapview(data1) #if the map doesn't show than follow the two steps below
proj4string(data1) #if the projection is NA then you projection is not defined
proj4string(data1)<-projection(raster()) #if the map doesn't show the coordinates use this

##5.Build the model
#5.1 Build the model
ext <- extent(-90, -32, -33, 23) #Limit coordinates to a specific area (e.g continent)
s<-sdmData(species~., data1, predictors=biom,bg=list(n=10000), method='gRandom',ext=ext) #specify species (the name of the column) against everything else
#sdmData - Creates a sdmdata objects that holds species (single or multiple) and explanatory variants
#predictors would be the biom, "bg=" - background creates absence data
#where 10000 is the number of pseudoabsences, most papers use this number
#"method= gRandom" creates a better result because it randomly chooses points all around geographical area
#Try eRandom - new method developed by Dr.Babak Naimi
#You can also eliminate method altogether, in that case it selects random points over the study area
#Note: you can get an error: In proj4string(train) : CRS object has comment, which is lost in output, Reinstalling sp version 1.4-1 mutes the warning, or just leave it
s #now you can see that it's presence-background data
plot(s)
#Note that you might lose some values due to this formula getting rid of duplications or any missing values
m<-sdm(species~., s, methods = c('glm','svm','rf','brt','mars','maxent'),replication=c('boot'),n=2)
#sdm - fits and evaluates species distribution models
m #to see the results
#Look at AUC values. the higher they are the higher the probability that the fit model will score a randomly drawn positive sample
#Note that the higher you put your pseudoabsences doesn't make the result more reliable since those are not real absences

# 6. Create current predictions
p<-predict(m,biom,'predictions1.img', mean =T,overwrite=TRUE) 
#You can choose any other ones and compare
en<-ensemble(m,biom,'enc1.img',setting=list(method='weights',stat="AUC"),overwrite=TRUE)
gui(m) #to see graphical and numeral results

#6.1 Plot predictions for current distribution
plot(p[[c(1:5)]]) #the number stands for the models (e.g 1-glm, 5-mars) see your input

##7. Future predictions

#7.1 To get the data for the future (climatic)
#To download variables for the future time
biof<-raster::getData('CMIP5',var='bio',res=10,rcp=85,year=70,model='AC')
#to get data from wordclim you need to use CMIPS as the name of the dataset
#res-resolution, rcp scenario, year =70
names(biof)<-names(bio) #because if you check the names of biof they will be changed to more complicated ones
?getData #to get more info on variations

#7.2 Create the model for the future predictions
pf<-predict(m,biof,'predictionsfu.img',overwrite=TRUE) #overwrite=TRUE - if you want to overwrite the name "predictionsf"
enf<-calc(pf,mean)
enf<-ensemble(m,biof,'ensfu.img',setting=list(method='weights',stat="TSS",opt=2),overwrite=TRUE)

#7.3 Plot future predictions
plot(stack(en,enf)) #stack current and future to compare
plot (en) #to see the future
mapview(stack(en,enf)) #you stack the probability for the current time and the future time
#on the map (its interactive), you can see the layer 1 - current, 2 future

#8 Plot to compare the changes over time
changes<-enf-en
plot(changes) 
#the plus values (over zero) - are the ones that gain suitability
#under 0 are the ones that lost suitability, 0 is no change

#9 To see correlations between the climate and distribution
rcurve(m) # it will show results for each one of the biomes, shows the prefered range
rcurve(m,id=9) #plots for specific models, in this case model 9
plot(sdm::getVarImp(m,9))#look at the importance of the variables for specific model 9
