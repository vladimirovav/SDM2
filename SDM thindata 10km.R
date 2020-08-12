
#1. Thindataset (10km distance)
datasetthin<-thin( loc.data= data1, lat.col = "lat", long.col = "lon", spec.col = "species", thin.par=10, reps=100, locs.thinned.list.return = TRUE,
                   write.files = FALSE, write.log.file = FALSE)
#2. Choose one of the datasets from the subsets by pressing on one of the datasets and saving it
datasetthin<-thindataset2[[11]]

#3. Modify the dataset to be workable
datasetthin$species<-1 #add a new column, make sure that the name of the column is not already taken, make every value in that column 1
coordinates(datasetthin)<-~Longitude+Latitude  #to convert this data to spatial dataframe, introduce the columns that has the geodata
class(datasetthin) #double-check that it worked, it should say now "SpatialPointsDataFrame", also acgeo should have only one column now
# 3.8 Map the data again to double check all is working (see point 4)
head(datasetthin) #double check that your data only has species column
mapview(datasetthin) #if the map doesn't show than follow the two steps below
proj4string(datasetthin) #if the projection is NA then you projection is not defined
proj4string(datasetthin)<-projection(raster()) #if the map doesn't show the coordinates use this

##5.Build the model
#5.1 Build the model
sthin1<-sdmData(species~., datasetthin, predictors=biom,bg=list(n=10000), method='gRandom',ext=ext) #specify species (the name of the column) against everything else
#sdmData - Creates a sdmdata objects that holds species (single or multiple) and explanatory variants
#predictors would be the biom, "bg=" - background creates absence data
#where 10000 is the number of pseudoabsences, most papers use this number
#"method= gRandom" creates a better result because it randomly chooses points all around geographical area
#Try eRandom - new method developed by Dr.Babak Naimi
#You can also eliminate method altogether, in that case it selects random points over the study area
#Note: you can get an error: In proj4string(train) : CRS object has comment, which is lost in output, Reinstalling sp version 1.4-1 mutes the warning, or just leave it
sthin1 #now you can see that it's presence-background data
#Note that you might lose some values due to this formula getting rid of duplications or any missing values
mthin1<-sdm(species~., sthin1, methods = c('glm','svm','rf','brt','mars','maxent'),
       replication=c('boot'),n=2)
#sdm - fits and evaluates species distribution models
mthin1 #to see the results
#Look at AUC values. the higher they are the higher the probability that the fit model will score a randomly drawn positive sample
#Note that the higher you put your pseudoabsences doesn't make the result more reliable since those are not real absences

# 6. Create current predictions
pthin1<-predict(mthin1,biom,'predictionsthin1.img', mean =T,overwrite=TRUE) 
#You can choose any other ones and compare
enthin1<-ensemble(mthin1,biom,'encthin1.img',setting=list(method='weights',stat="AUC"),overwrite=TRUE)
gui(mthin1) #to see graphical and numeral results

#6.1 Plot predictions for current distribution
plot(pthin1[[c(1:5)]]) #the number stands for the models (e.g 1=glm, 5-mars) see your input

##7. Future predictions

#7.2 Create the model for the future predictions
pfthin1<-predict(mthin1,biof,'predictionsfthin1.img',overwrite=TRUE) #overwrite=TRUE - if you want to overwrite the name "predictionsf"
enfthin1<-calc(pfthin1,mean)
enfthin1<-ensemble(m,biof,'ensfthin1.img',setting=list(method='weights',stat="TSS",opt=2),overwrite=TRUE)

#7.3 Plot future predictions
plot(stack(enthin1,enfthin1)) #stack current and future to compare
plot (enthin1) #to see the future
mapview(stack(enthin1,enfthin1)) #you stack the probability for the current time and the future time
#on the map (its interactive), you can see the layer 1 - current, 2 future

#8 Plot to compare the changes over time
changesthin1<-enfthin1-enthin1
plot(changesthin1) 
#the plus values (over zero) - are the ones that gain suitability
#under 0 are the ones that lost suitability, 0 is no change

#9 To see correlations between the climate and distribution
rcurve(mthin1) # it will show results for each one of the biomes, shows the prefered range
rcurve(mthin1,id=9) #plots for specific models, in this case model 9
plot(sdm::getVarImp(mthin1,9))#look at the importance of the variables for specific model 9