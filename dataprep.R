###################################################
## Formatting elk collar data for final database ##
#######  NSERP - Kristin Barker - May 2016  #######
###################################################

##SET WD
wd_workcomp <- "C:\\Users\\kristin.barker\\Documents\\GitHub\\NSERP_DB"
wd_laptop <- "C:\\Users\\kjbark3r\\Documents\\GitHub\\NSERP_DB"
if (file.exists(wd_workcomp)){
  setwd("C:\\Users\\kristin.barker\\Documents\\GitHub\\NSERP_DB\\")
} else {
  setwd("C:\\Users\\kjbark3r\\Documents\\GitHub\\NSERP_DB\\")
}
rm(wd_workcomp, wd_laptop)

##LOAD LIBRARIES
library(dplyr)
library(tidyr)

##PREP DATA

#collar locations
df140560 <- read.delim("rawdata/140560-2204.txt", header = TRUE, sep = "\t")
  df140560$Date <- as.Date(df140560$Date, format = "%m/%d/%Y")
  df140560$Time <- as.character(df140560$Time)
  df140560$FixStatus <- as.character(df140560$FixStatus)
df140910 <- read.delim("rawdata/140910-3518.txt", header = TRUE, sep = "\t")
  df140910$Date <- as.Date(df140910$Date, format = "%m/%d/%Y")
  df140910$Time <- as.character(df140910$Time)
  df140910$FixStatus <- as.character(df140910$FixStatus)
df141490 <- read.delim("rawdata/141490-3529.txt", header = TRUE, sep = "\t")
  df141490$Date <- as.Date(df141490$Date, format = "%m/%d/%Y")
  df141490$Time <- as.character(df141490$Time)
  df141490$FixStatus <- as.character(df141490$FixStatus)
dfiridium <- read.delim("rawdata/iridium-all.txt", header = TRUE, sep = "\t")
  dfiridium$AnimalID <- as.integer(dfiridium$AnimalID)
  dfiridium$Date <- as.Date(dfiridium$Date, format = "%m/%d/%Y")
  dfiridium$Time <- as.character(dfiridium$Time)
  dfiridium$TempC <- as.numeric(dfiridium$TempC)
  dfiridium$FixStatus <- as.character(dfiridium$FixStatus)
alllocs <- as.data.frame(bind_rows(df140560, df140910, df141490, dfiridium))
  alllocs <- subset(alllocs, select = -DateTime)
 
#capture information
  #remove 3300s we have no data for
  #note these data aren't perfectly classified (eg date/time)
allcap14 <- read.csv("rawdata/capture14.csv")
  cap14 <- allcap14[ ! allcap14$DeviceID %in% c(2496, 3521),]
allcap15 <- read.csv("rawdata/capture15.csv", as.is = TRUE)
  cap15 <- allcap15[ ! allcap15$DeviceID %in% c(2496, 3521),]

##transmission end information
transend <- read.csv("rawdata/transend.csv", as.is = TRUE)
  transend$EndDate <- as.Date(transend$EndDate, format = "%m/%d/%Y")
  transend <- transend[,c("DeviceID", "AnimalID", "EndDate", "EndTime", 
                     "EndLat", "EndLong", "StatusFeb16", "EndCause1", "EndCause2")] 

#clean up workspace
rm(df140560, df140910, df141490, dfiridium, allcap14, allcap15)
  
########ADD ANIMAL ID, SEX, CAPTURE YEAR############################

#create list of individual elk 
collarids <- as.data.frame(unique(alllocs$DeviceID))
  names(collarids)[1] <- "DeviceID"

#order collarids and capture data by DeviceID to index all the same
collarids <- arrange(collarids, DeviceID)
cap14 <- arrange(cap14, DeviceID)
cap15 <- arrange(cap15, DeviceID)

#create blank df to store results in
newdf <- data.frame(matrix(ncol = 9, nrow = 0)) #create df wo NAs
colnames(newdf) <- c("DeviceID", "AnimalID", "Date", "Time", 
                     "Lat", "Long", "FixStatus", "DOP", "TempC")

#assign animalid based on device id and capture year
for(i in 1:nrow(collarids)){
  indiv <- collarids[i,] #treat each collarid as an individual
  id <- subset(alllocs, alllocs$DeviceID == indiv) #for each individual,
  id$AnimalID <- ifelse(id$Date < "2015-01-23",  #for locns b4 2015 capture,
                        cap14$AnimalID[i], cap15$AnimalID[i]) #animalid=2014
  newdf <- as.data.frame(bind_rows(newdf, id)) #and add data to master df
}                                              #df keeps all decimal places

#add sex and capture year
newdf <- newdf %>%
  mutate(Sex = ifelse(between(DeviceID, 34942, 34963), "Male", "Female")) %>%
  mutate(CaptureYr = ifelse(AnimalID < 149999, 2014, 2015 ))

##REMOVE EXTRANEOUS LOCATIONS####################################

#pre-deployment removal
newdf <- newdf[!(newdf$Date < "2014-02-26"),]
newdf <- newdf[!(newdf$CaptureYr == 2015 & newdf$Date < "2015-01-24"),]

#post-mortality or post-collar drop removal
transend.sub <- subset(transend, select = -DeviceID) #remove duplicate column
newdf <- as.data.frame(left_join(newdf, transend.sub, by = "AnimalID")) #add transend data

elklocs <- newdf[!(newdf$Date >= newdf$EndDate & newdf$Time > newdf$EndTime
                 | newdf$Date > newdf$EndDate),]

#na removal
elklocs.nona <- elklocs[!is.na(elklocs$Lat),]

#export data
write.csv(elklocs, file = "allcollardata.csv")
write.csv(elklocs.nona, file = "collardata_locsonly.csv")