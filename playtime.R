###########################
########playtime###########
###########################

##BETTER WD CODE
wd_workcomp <- "C:\\Users\\kristin.barker\\Documents\\GitHub\\NSERP_DB"
wd_laptop <- "C:\\Users\\kjbark3r\\Documents\\GitHub\\NSERP_DB"
if (file.exists(wd_workcomp)){
  setwd("C:\\Users\\kristin.barker\\Documents\\GitHub\\NSERP_DB\\")
} else {
  setwd("C:\\Users\\kjbark3r\\Documents\\GitHub\\NSERP_DB\\")
}
rm(wd_workcomp, wd_laptop)

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
##create list of animalids unique to each capture year
#need to keep deviceid in these to sort by it
cap14ids <- as.data.frame(unique(cap14))

cap14ids <- as.data.frame(c(unique(cap14$DeviceID), cap14$AnimalID))
  names(cap14ids)[1] <- "DeviceID"
cap15ids <- as.data.frame(unique(cap15$DeviceID))
  names(cap15ids)[1] <- "DeviceID"

collarids <- arrange(collarids, DeviceID)
cap14ids <- arrange(cap14ids, DeviceID)
cap15ids <- arrange(cap15ids, DeviceID)

newdf <- data.frame(matrix(ncol = 10, nrow = 0)) #create df wo NAs
colnames(newdf) <- c("DeviceID", "AnimalID", "DateTime", "Date", "Time", 
                     "Lat", "Long", "FixStatus", "DOP", "TempC")

for(i in 1:nrow(collarids)){
  indiv <- collarids[i,] #treat each collarid as an individual
  id <- subset(alllocs, alllocs$DeviceID == indiv) #for each individual,
  id$AnimalID <- ifelse(id$Date < "2015-01-23",  #for locns b4 2015 capture,
                        cap14ids$AnimalID[i], cap15ids$AnimalID[i]) #animalid=2014
  newdf <- as.data.frame(bind_rows(newdf, id)) #and add data to master df
}   

####
previous attempts
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

#this was original
for(i in 1:nrow(collarids)){
  indiv <- collarids[i,] #treat each collarid as an individual
  id <- subset(alllocs, alllocs$DeviceID == indiv) #for each individual,
  id$AnimalID <- ifelse(id$Date < "2015-01-23",  #for locns b4 2015 capture,
                        cap14$AnimalID[i], cap15$AnimalID[i]) #animalid=2014
  newdf <- as.data.frame(bind_rows(newdf, id)) #and add data to master df
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
#REOMVE POST-HARVEST/DROP LOCATIONS
#
#preliminary thoughts
newtestdf <- left_join(newtestdf, transend, by = "AnimalID")

for(i in 1:nrow(collarids)){
  indiv = collarids[i,] #treat each collarid as an individual
  id <- subset(newtestdf, newtestdf$DeviceID == indiv) #for each individual,

#ponder the below: need to make datetime column, not treat separately
  newtestdf <- ifelse(id$DateTime <- id$Date & ,  #assign animalid
                        cap14$AnimalID[i], cap15$AnimalID[i])
  newdf <- bind_rows(newdf, id) #and add data to master df
}

#actually don't need per individual, just per location
##first subset transend: remove deviceid, 
newtestdf <- newdf
transend.sub <- subset(transend, select = -DeviceID)
newtestdf <- left_join(newtestdf, transend.sub, by = "AnimalID")
elklocs <- newtestdf[!(newtestdf$DateTime >= newtestdf$EndDateTime),]

#don't forget NAs are still included
elklocs.nona <- elklocs[!is.na(elklocs$Lat),]

#nope
elklocs.nona <- as.data.frame(na.omit(elklocs$Lat))
elklocs.nona <- elklocs[!(elklocs$Lat = "NA"),]
elklocs.nona <- na.omit(elklocs[(elklocs$Lat),])

########
#MISC SPECIAL CASES

######
#DECIMAL PLACES  (currently rounds longitude to 4, dumb)

#1. does it make a difference if I use read.csv vs read.delim? NO
df140007 <- read.csv("rawdata/140007-3529.csv")
df140007[4,7]
print(df140007[4,7], digits=10)
rm(df140007)
  #all decimal places included
df140007 <- read.delim("rawdata/140007-3529.txt", header = TRUE, sep = "\t")
df140007[4,7]
print(df140007[4,7], digits=10)
rm(df140007)
  #all decimal places included
  #raw 3300 data are all good
dfiridium <- read.delim("rawdata/iridium-all.txt", header = TRUE, sep = "\t")
dfiridium[4,7]
print(dfiridium[4,7], digits=10)
rm(dfiridium)
  #raw iridium good too
allcap14 <- read.csv("rawdata/capture14.csv")
allcap14[4,9]
print(allcap14[14,9], digits=10)
print(allcap15[14,9], digits=10)
  #raw capture data good
print(cap14[14,9], digits=10)
print(cap15[14,9], digits=10)
  #subsetted capture data good
print(transend[14,6], digits=10)
  #transmission end data good
print(alllocs[134,7], digits=10)
  #data after rows bound NOT GOOD, WTF

#2. what's the difference between bound data and raw data?
str(dfiridium)
str(alllocs)
  #actually the below is a way better way to check this
sapply(dfiridium, class)
sapply(df140007, class)
sapply(alllocs, class)
  #ok, need to make classes the same before binding. 
alllocs[4,3]

##nope
df140057 <- read.delim("rawdata/140057-2204.txt", header = TRUE, 
                       as.is = FALSE, numerals = "no.loss", sep = "\t")
df140057 <- read.delim("rawdata/140057-2204.txt", header = TRUE, 
                       as.is = TRUE, numerals = "no.loss", sep = "\t")
df140057 <- read.delim("rawdata/140057-2204.txt", header = TRUE, 
                       numerals = "no.loss", sep = "\t")
df140057 <- read.delim("rawdata/140057-2204.txt", header = TRUE, 
                       numerals = "allow.loss", sep = "\t")
df140057 <- read.delim("rawdata/140057-2204.txt", header = TRUE, 
                       as.is = TRUE, sep = "\t")
df140057 <- read.delim("rawdata/140057-2204.txt", header = TRUE, 
                       as.is = FALSE, numerals = "no.loss", sep = "\t")
df140057[4,7]
rm(df140057)

############
#DATE TIME FORMATTING, UGH

#yes
alllocs$DateTime <- as.POSIXct(alllocs$DateTime, format = "%m/%d/%Y  %H:%M")
alllocs$Date <- as.POSIXct(alllocs$DateTime, format = "%m/%d/%Y  %H:%M")
#format(alllocs$Time[j], format="%H:%M:%S")
format(alllocs$Time, format="%H:%M:%S")
class(alllocs$Time)

#no
alllocs$Time <- as.POSIXct(alllocs$Time, format = "%H:%M")  
  #forces date into same column
alllocs$Time <- strftime(alllocs$Time, format = "%H:%M")
  #error

#MORE MISC
#checking out 34908 to see when last location was
##this is the one GPS stopped working on
locs34908 <- subset(newdf, DeviceID==34908); View(locs34908)

#34909 - can't zoom far enough in to lotek map to get 1st cluster pt
locs34909 <- subset(newdf, DeviceID==34909); View(locs34909)
locs34908.nona <- locs34908[complete.cases(locs34908),]

#motherfucking daylight savings

#figuring out and/or operators

(endd <- as.Date("2011-01-02", format = "%Y-%m-%d"))
(endt <- as.character("03:00"))

rm(fuckthis)
rm(newfuckthis)

fuckthis <- data.frame(matrix(ncol = 2, nrow = 4))
colnames(fuckthis) <- c("Date", "Time")
fuckthis[1,1] <- "2011-01-02"
fuckthis[1,2] <- as.character("02:00")
fuckthis[2,1] <- "2011-01-02"
fuckthis[2,2] <- as.character("04:00")
fuckthis[3,1] <- "2011-01-04"
fuckthis[3,2] <- as.character("02:00")
fuckthis[4,1] <- "2011-01-04"
fuckthis[4,2] <- as.character("04:00")
fuckthis$Date <- as.Date(fuckthis$Date, format = "%Y-%m-%d")

newfuckthis <- fuckthis[!(fuckthis$Date >= endd & fuckthis$Time >= endt),]
newfuckthis <- newfuckthis[!(newfuckthis$Date > endd),]
  #that works, now can you make it all one line?
newfuckthis <- fuckthis[!(fuckthis$Date >= endd & fuckthis$Time >= endt
                        |fuckthis$Date > endd),]
  #yup

#now apply it to your actual data

endd2 <- as.Date("2015-01-02", format = "%Y-%m-%d")
endt2 <- as.character("03:00")
test <- newdf[!(newdf$Date >= endd2 & newdf$Time > endt2),]
test2 <- newdf[!(newdf$Date > endd2),]
test3 <- newdf[!(newdf$Date >= endd2 & newdf$Time > endt2
                 | newdf$Date > endd2),]
#it works when end date and time are fixed, external entities
#note don't change those operators or shit fails for some reason
test4 <- newdf[!(newdf$Date >= newdf$EndDate),]

newdf$EndDate <- as.Date(newdf$EndDate, format = "%m/%d/%Y")

test5 <- newdf[!(newdf$Date >= newdf$EndDate & newdf$Time > newdf$EndTime
                 | newdf$Date > newdf$EndDate),]
#make sure it worked
test5sub <- subset(test5, test5$DeviceID == "34926")

elklocs <- newdf[!(newdf$Date = newdf$EndDate & newdf$Time > newdf$EndTime
                   |newdf$Date > newdf$EndDate),]

newfuckthis <- fuckthis[!(fuckthis$Date >= endd & fuckthis$Time >= endt),]
newfuckthis <- newfuckthis[!(newfuckthis$Date > endd),]
#all together now
newfuckthis <- fuckthis[!(fuckthis$Date >= endd & fuckthis$Time >= endt
                          |fuckthis$Date > endd),]



################
#this section is original animalid for loop 
#but didn't allow me to keep DateTime
#and truncated longitude decimal places

#create blank df to store results
newdf <- data.frame(matrix(ncol = 9, nrow = 0)) #create df wo NAs
colnames(newdf) <- c("DeviceID", "AnimalID", "Date", "Time", 
                     "Lat", "Long", "FixStatus", "DOP", "TempC")

#have to remove DateTime or else R explodes for some reason :(
nodatetime <- subset(alllocs, select = -DateTime)

#create list of elk 
collarids <- as.data.frame(unique(nodatetime$DeviceID))
names(collarids)[1] <- "DeviceID"

#assign animalid based on device id and capture year
for(i in 1:nrow(collarids)){
  indiv = collarids[i,] #treat each collarid as an individual
  id <- subset(nodatetime, nodatetime$DeviceID == indiv) #for each individual,
  id$AnimalID <- ifelse(id$Date < "2015-01-23",  #assign animalid
                        cap14$AnimalID[i], cap15$AnimalID[i])
  newdf <- bind_rows(newdf, id) #and add data to master df
}

#add sex and capture year
newdf <- newdf %>%
  mutate(Sex = ifelse(between(DeviceID, 34942, 34963), "Male", "Female")) %>%
  mutate(CaptureYr = ifelse(AnimalID < 149999, 2014, 2015 ))
################

###Why the fuck are some DateTimes reading in as NAs?
wtf140560 <- df140560[is.na(df140560$DateTime),]
wtf140910 <- df140910[is.na(df140910$DateTime),]
wtf141490 <- df141490[is.na(df141490$DateTime),]
wtfiridium <- dfiridium[is.na(dfiridium$DateTime),]

holyshitdidthatwork <- elklocs[is.na(elklocs$Date),]
#yassssssssss

#MOTHERFUCKING DAYLIGHT SAVINGS TIME

#force R to use those DateTimes anyway?? Maybe diff format? 
#they read in OK as factors, but that's not ideal

#does character work?
df140560$DateTime <- as.character(df140560$DateTime)
test <- df140560[!(df140560$DateTime >= "02/20/2014 20:00"),] #line80
#no

#numeric?
rm(df140560, test)
df140560 <- read.delim("rawdata/140560-2204.txt", header = TRUE, sep = "\t")
df140560$DateTime <- as.numeric(df140560$DateTime)
test <- df140560[!(df140560$DateTime >= "02/20/2014 20:00"),] #line80
#no. kill me.

################
#why tf did it keep some locs after the properly specified endtime?
#anid 141300 - check some others
 test <- elklocs.nona
test <- subset(elklocs.nona, AnimalID == 141300)
View(test)
test <- subset(elklocs.nona, AnimalID == 151110)
View(test)
#feck.
#endtime also 0800
#try 1400 endtime
test <- subset(elklocs.nona, AnimalID == 141110)
View(test)
#one additional loc
#ok, something's up. . .

#reran with slightly different code
test <- subset(elklocs, AnimalID == 141300)
View(test)

#ok try removing extra points in stages
testdf <- elklocs[!(newdf$Date > newdf$EndDate),] # first remove post-end dates
testdf <- testdf[!(elklocs$Date >= elklocs$EndDate & elklocs$Time > elklocs$EndTime),]

test <- subset(testdf, AnimalID == 141300)
View(test)

#ok i think i have to give in and use datetime
testdf <- newdf
testdf$DateTime <- as.POSIXct(paste(testdf$Date, testdf$Time, sep = " "), 
                              format = "%Y-%m-%d %H:%M")
testdf$EndDateTime <- as.POSIXct(paste(testdf$EndDate, testdf$EndTime, sep = " "), 
                              format = "%Y-%m-%d %H:%M")
testdf <- testdf[(testdf$DateTime <= testdf$EndDateTime),]

#
test <- subset(elklocs, AnimalID == 151110)
View(test)
                   
##############################
###GENERALLY HELPFUL STUFF####
##############################

####CHECK CLASSES AND FORMATTING
str(dfiridium)
str(df140057)

print(df140007[240,7], digits=10)
print(dfiridium[240,7], digits=10)
print(alllocs[409,7], digits=10)
print(newdf[409,7], digits=10)

str(alllocs)

sapply(dfiridium, class)
sapply(df140007, class)
sapply(alllocs, class)

##########################
## NUKE IT ###############
rm(list=ls())
