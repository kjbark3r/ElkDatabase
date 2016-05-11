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
test <- subset(alllocs, select = -DateTime)

#..AAAAAAND THE PITCH!
#currently keeps rewriting over the data each time
#need to save per indiv
for(i in 1:nrow(collarids)){
  indiv = collarids[i,] 
  id <- subset(test, test$DeviceID == indiv) #for each individual,
  id$AnimalID <- ifelse(id$Date < "2015-01-23", 
                          cap14$AnimalID[i], cap15$AnimalID[i])
 # write.csv(id, file="indiv.csv", append=TRUE)
    }

#to remove bad pitches...
rm(test, indiv, i, id, kickass)

#if you need a df to store that stuff in for some reason
#kickass <- data.frame(matrix(ncol = 9, nrow = 520479))
#colnames(kickass) <- c("DeviceID", "AnimalID", "Date", "Time", 
#                       "Lat", "Long", "FixStatus", "DOP", "TempC")


#######################
#doesn't work
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