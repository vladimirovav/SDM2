
#1. Modify the dataset to be workable
data3$species<-1 #add a new column, make sure that the name of the column is not already taken, make every value in that column 1
coordinates(data3)<-~lon+lat  #to convert this data to spatial dataframe, introduce the columns that has the geodata
class(data3) #double-check that it worked, it should say now "SpatialPointsDataFrame", also acgeo should have only one column now
# 1.2 Map the data again to double check all is working (see point 4)
head(data3) #double check that your data only has species column
mapview(data3) #if the map doesn't show than follow the two steps below
proj4string(data3) #if the projection is NA then you projection is not defined
proj4string(data3)<-projection(raster()) #if the map doesn't show the coordinates use this

##2.Build the model
#2.1 Build the model
sdata3<-sdmData(species~., data3, predictors=biom,bg=list(n=10000), method='gRandom') #specify species (the name of the column) against everything else
#sdmData - Creates a sdmdata objects that holds species (single or multiple) and explanatory variants
#predictors would be the biom, "bg=" - background creates absence data
#where 10000 is the number of pseudoabsences, most papers use this number
#"method= gRandom" creates a better result because it randomly chooses points all around geographical area
#Try eRandom - new method developed by Dr.Babak Naimi
#You can also eliminate method altogether, in that case it selects random points over the study area
#Note: you can get an error: In proj4string(train) : CRS object has comment, which is lost in output, Reinstalling sp version 1.4-1 mutes the warning, or just leave it
sdata3 #now you can see that it's presence-background data
#Note that you might lose some values due to this formula getting rid of duplications or any missing values
mdata3<-sdm(species~., sdata3, methods = c('glm','svm','rf','brt','mars','maxent'),
            replication=c('boot'),n=2)
#sdm - fits and evaluates species distribution models
mdata3 #to see the results
#Look at AUC values. the higher they are the higher the probability that the fit model will score a randomly drawn positive sample
#Note that the higher you put your pseudoabsences doesn't make the result more reliable since those are not real absences

# 6. Create current predictions
pdata3<-predict(mdata3,biom,'predictionsdata3.img', mean =T,overwrite=TRUE) 
#You can choose any other ones and compare
endata3<-ensemble(mdata3,biom,'endata3.img',setting=list(method='weights',stat="AUC"),overwrite=TRUE)
gui(mdata3) #to see graphical and numeral results

#6.1 Plot predictions for current distribution
plot(pdata3[[c(1:5)]]) #the number stands for the models (e.g 1=glm, 5-mars) see your input

##7. Future predictions

#7.2 Create the model for the future predictions
pfdata3<-predict(mdata3,biof,'predictionsfdata3.img',overwrite=TRUE)
enfdata3<-calc(pfdata3,mean)
enfdata3<-ensemble(mdata3,biof,'ensfdata3.img',setting=list(method='weights',stat="TSS",opt=2),overwrite=TRUE)

#7.3 Plot future predictions
plot(stack(endata3,enfdata3)) #stack current and future to compare
plot (endata3) #to see the future
mapview(stack(endata3,enfdata3)) #you stack the probability for the current time and the future time
#on the map (its interactive), you can see the layer 1 - current, 2 future

#8 Plot to compare the changes over time
changesdata3<-enfdata3-endata3
plot(changesdata3) 
#the plus values (over zero) - are the ones that gain suitability
#under 0 are the ones that lost suitability, 0 is no change

#9 To see correlations between the climate and distribution
rcurve(mdata3) # it will show results for each one of the biomes, shows the prefered range
rcurve(mdata3,id=9) #plots for specific models, in this case model 9
plot(sdm::getVarImp(mdata3,9))#look at the importance of the variables for specific model 9