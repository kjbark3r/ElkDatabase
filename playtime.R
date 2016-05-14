###########################
########playtime###########
###########################

##############
##can you use mutate to append new column based on diff df?

View(iris)
#just make sure you can use mitate
mutate(iris, testcol = "yup")
#make new dataframe of 150 random numbers
##try to add that to iris
testdf <- sample(10, 150, replace = TRUE)
testdf <- as.data.frame(testdf)
mutate(iris, testcol2 = testdf[,1])

##YES :)
  #but don't forget it doesn't save unless you say to


##########
##add column using ifelse?
rm(test)

test <- iris
test$type <- NA


for(i in 1:nrow(test)){
  if(test$Species[i] =="versicolor"){
        test$type[i] <- "your dad"
  }else{test$type[i] <- "yo mama"}
}


#
#below = yep (early steps)
for(i in 1:nrow(test)){
  test$type[test$Species=="versicolor"] <- "yourdad"
}

test$type[test$Species=="setosa"] <- "yourmom"

for(i in 1:ntest){
  if(test$Species=="setosa"){
    cat("Baby step #2...\n")
  }else{cat("Getting there")}
}   

if(test$Species=="fdhjsetosa"){
  cat("Baby step #2...\n")
}else{cat("Getting there")}

if(test$Sepal.Length<0){
  cat("Baby step #2...\n")
}else{cat("Getting there")}

if(is.numeric(test$Sepal.Length)){
  cat("Baby steps...\n")
}

test$type <- NA

testdf <- iris %>%
  group_by(Species) %>%
  mutate(type=sample(10, 50, replace=TRUE))

testdf <- iris %>%
  group_by(Species) 

mutate(iris, type="hm")



#################################
##ADD ANIMAL IDS TO LOCATION DATA

########
#STEP 1: populate AnimalID based on Date

#this does work, but it's slow af
test <- alllocs
for(i in 1:nrow(test)){
  if(alllocs$Date[i] < "2015-01-23"){
    alllocs$AnimalID[i] <- "2014"
  }else{
    alllocs$AnimalID[i] <- "2015"
  }
}

#also works, but would be better to pull datetime out first
test <- alllocs
test$DateTime <- as.character(test$DateTime)
test$AnimalID <- ifelse(test$Date < "2015-01-23", 2014, 2015)

#this works best
test <- subset(alllocs, select = -c(DateTime, AnimalID))
test <- test %>%
  mutate(AnimalID = ifelse(test$Date < "2015-01-23", 2014, 2015))

########
#STEP 2: populate AnimalID based on Date AND DeviceID

#subset capture data to remove 3300s we don't have data for
#to order both collarids and capture data by DeviceID
#so we can index all the same
cap14 <- allcap14[ ! allcap14$DeviceID %in% c(2496, 3521),]
cap15 <- allcap15[ ! allcap15$DeviceID %in% c(2496, 3521),]
collarids <- arrange(collarids, DeviceID)
cap14 <- arrange(cap14, DeviceID)
cap15 <- arrange(cap15, DeviceID)

#just because i'm paranoid about screwing up real data
#change test to alllocs for real version
test <- subset(alllocs, select = -DateTime)

#..AAAAAAND THE PITCH!
#fuuuck yeah, this works
#because i'm a super genius
#but it would be best to store all in one df
newdf <- data.frame(matrix(ncol = 9, nrow = 0)) #create df wo NAs
colnames(newdf) <- c("DeviceID", "AnimalID", "Date", "Time", 
                       "Lat", "Long", "FixStatus", "DOP", "TempC")

for(i in 1:nrow(collarids)){
  indiv = collarids[i,] 
  id <- subset(test, test$DeviceID == indiv) #for each individual,
  id$AnimalID <- ifelse(id$Date < "2015-01-23",  #assign animalid
                          cap14$AnimalID[i], cap15$AnimalID[i])
  newdf <- bind_rows(newdf, id)
}


#to remove bad pitches...
rm(test, indiv, i, id, locs, newdf, indivname)


#######################
#doesn't work

#below works, but it would be better to store all in one df
for(i in 1:nrow(collarids)){
  indiv = collarids[i,] 
  id <- subset(alllocs, alllocs$DeviceID == indiv) #for each individual,
  id$AnimalID <- ifelse(id$Date < "2015-01-23",  #assign animalid
                        cap14$AnimalID[i], cap15$AnimalID[i])
  indivname <- paste("locs", indiv, sep = "") #create name for df
  blankdf <- data.frame(matrix(ncol = 9, nrow = nrow(id))) #create df
  colnames(blankdf) <- c("DeviceID", "AnimalID", "Date", "Time", 
                         "Lat", "Long", "FixStatus", "DOP", "TempC")
  blankdf <- id
  assign(indivname, blankdf) #fill df with indiv data
}

#close - creates df properly but doesn't store the data in it
for(i in 1:nrow(collarids)){
  indiv = collarids[i,] 
  id <- subset(test, test$DeviceID == indiv) #for each individual,
  id$AnimalID <- ifelse(id$Date < "2015-01-23",  #assign animalid
                        cap14$AnimalID[i], cap15$AnimalID[i])
  indivname <- paste(indiv, "locs", sep = "") #create name for df
  blankdf <- data.frame(matrix(ncol = 9, nrow = nrow(id))) #create df
  colnames(blankdf) <- c("DeviceID", "AnimalID", "Date", "Time", 
                         "Lat", "Long", "FixStatus", "DOP", "TempC")
  assign(indivname, blankdf) #fill df with indiv data
}
for(i in 1:nrow(collarids)){
  indiv = collarids[i,] 
  id <- subset(test, test$DeviceID == indiv) #for each individual,
  id$AnimalID <- ifelse(id$Date < "2015-01-23", 
                        cap14$AnimalID[i], cap15$AnimalID[i])
  locs[i] <- id #store ea subset as new df?
}

#also, same as above but
locs <- id
locs <- id[i]
paste(id, "locs", sep = "")
write.csv(id, file="indiv.csv", append=TRUE)

test <- subset(alllocs, select = -c(DateTime, AnimalID))
test <- test %>%
  mutate(AnimalID = ifelse(test$Date[test$Device] < "2015-01-23", 
                           cap14$AnimalID[cap14$DeviceID], 2015))

for(i in 1:nrow(test)){
  for(i in 1:nrow(collarids)){
    
  }
  if(alllocs$Date[i] < "2015-01-23"){
    alllocs$AnimalID[i] <- cap14$AnimalID[cap14$DeviceID]
  }else{
    alllocs$AnimalID[i] <- cap15$AnimalID[cap15$DeviceID]
  }
}

###############
#Stand back...
##I'm going to try functions
###it would be cool if this would spit out a csv too...
#[below is all jacked up. Don't run it.] 
indiv <- function(locs, deploy1, deploy2){
  for(i in 1:nrow(alllocs)){
    if(alllocs$Date[i] < "2015-01-23"){
      alllocs$AnimalID[i] <- "2014"
    }else{
      alllocs$AnimalID[i] <- "2015"
    }
  }  
}

alllocs$AnimalID <- indiv(alllocs, cap14, cap15)
#just kidding i'm not

########
#remove pre-deployment locations
##2014 capture start date = 02/26/2014
##2015 captures start date = 01/24/2015

#step 1: conditional removal of pre-2014 deployment
newtestdf <- newdf[!(newdf$Date < "2014-02-26"),]

#step 2: remove pre-2015 deployment only for redeploys
newtestdf <- newtestdf[!(newtestdf$CaptureYr == 2015 & 
                      newtestdf$Date < "2015-01-24"),]

##don't work
#doesn't remove any data
newtestdf <- ifelse(newtestdf$CaptureYr == 2015, 
                    newtestdf[!(newtestdf$Date < "2015-01-24"),], 
                    newtestdf)

#freezes r - hangs after first line
newtestdf <- ifelse(newtestdf$CaptureYr = 2015, 
                    newtestdf[!(newtestdf$Date < "2015-01-24"),], 
                    newtestdf)

########
#remove post-harvest/collar drop locations
#PRELIMINARY THOUGHTS
###note probably want to read in these data towards the top of your code
###with the rest of the read-ins
##read above code first to use testdf rather than real one

transend <- read.csv("rawdata/transend.csv", as.is=T) #treat dates as chars
newtestdf <- left_join(newtestdf, transend, by = "AnimalID")

for(i in 1:nrow(collarids)){
  indiv = collarids[i,] #treat each collarid as an individual
  id <- subset(newtestdf, newtestdf$DeviceID == indiv) #for each individual,

#ponder the below: need to make datetime column, not treat separately
  newtestdf <- ifelse(id$Date < id$EndDate & ,  #assign animalid
                        cap14$AnimalID[i], cap15$AnimalID[i])
  newdf <- bind_rows(newdf, id) #and add data to master df
}
