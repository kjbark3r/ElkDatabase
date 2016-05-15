###################################################
## Formatting elk collar data for final database ##
#######  NSERP - Kristin Barker - May 2016  #######
###################################################

##SET WD
wd_workcomp <- "C:\\Users\\kristin.barker\\Documents\\GitHub\\NSERP_DB"
wd_laptop <- "C:\\Users\\kjbark3r\\Documents\\GitHub\\NSERP_DB"
if (file.exists(wd_workcomp)){
  inpath="C:\\Users\\kristin.barker\\Documents\\GitHub\\NSERP_DB\\"
  } else {
  inpath="C:\\Users\\kjbark3r\\Documents\\GitHub\\NSERP_DB\\"
}
setwd(inpath)

##LOAD LIBRARIES
library(dplyr)
library(tidyr)

##READ IN RAW DATA
df140057 <- read.delim("rawdata/140057-2204.txt", header = TRUE, sep = "\t")
df140016 <- read.delim("rawdata/140016-3518.txt", header = TRUE, sep = "\t")
df140007 <- read.delim("rawdata/140007-3529.txt", header = TRUE, sep = "\t")
dfiridium <- read.delim("rawdata/iridium-all.txt", header = TRUE, sep = "\t")
  #none above have truncated longitude - just printing shorter. to verify:
  #print(df140007[4,7], digits=10)
  #but below ARE truncated >:(
  #print(allcap14[4,9], digits=10)
  #leaving it for now because capture long's aren't in final database
allcap14 <- read.csv("rawdata/capture14.csv")
allcap15 <- read.csv("rawdata/capture15.csv")
  #subset capture data to remove 3300s we don't have data for
  cap14 <- allcap14[ ! allcap14$DeviceID %in% c(2496, 3521),]
  cap15 <- allcap15[ ! allcap15$DeviceID %in% c(2496, 3521),]

##SET UP LOCN DATAFRAME
  #THIS is where longitudes get truncated - WHYYYYYY?
  #print(alllocs[4,7], digits=10)
  #may need to use a base r command instead of bind_rows
alllocs <- bind_rows(df140057, df140016, df140007, dfiridium)
  alllocs$DateTime <- strptime(alllocs$DateTime, format = "%m/%d/%Y  %H:%M")
  alllocs$Date <- as.Date(alllocs$Date, format = "%m/%d/%Y")
  #alllocs$Time <- strptime(alllocs$Time, format = "%H:%M")  #needs date too

##CLEAN UP WORKSPACE
rm(df140057, df140016, df140007, dfiridium, allcap14, allcap15)
  
##ADD ANIMAL ID, SEX, CAPTURE YEAR############################

#order collarids and capture data by DeviceID to index all the same
collarids <- arrange(collarids, DeviceID)
cap14 <- arrange(cap14, DeviceID)
cap15 <- arrange(cap15, DeviceID)

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

##REMOVE EXTRANEOUS LOCATIONS####################################

#pre-deployment removal
newdf <- newdf[!(newdf$Date < "2014-02-26"),]
newdf <- newdf[!(newdf$CaptureYr == 2015 & newdf$Date < "2015-01-24"),]

#post-mortality or post-collar drop removal