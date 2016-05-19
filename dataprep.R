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
df140057 <- read.delim("rawdata/140057-2204.txt", header = TRUE, sep = "\t")
  df140057$DateTime <- as.POSIXct(df140057$DateTime, format = "%m/%d/%Y  %H:%M:%S")
  df140057$Date <- as.Date(df140057$Date, format = "%m/%d/%Y")
  df140057$Time <- as.character(df140057$Time)
  df140057$FixStatus <- as.character(df140057$FixStatus)
df140016 <- read.delim("rawdata/140016-3518.txt", header = TRUE, sep = "\t")
  df140016$DateTime <- as.POSIXct(df140016$DateTime, format = "%m/%d/%Y  %H:%M:%S")
  df140016$Date <- as.Date(df140016$Date, format = "%m/%d/%Y")
  df140016$Time <- as.character(df140016$Time)
  df140016$FixStatus <- as.character(df140016$FixStatus)
df140007 <- read.delim("rawdata/140007-3529.txt", header = TRUE, sep = "\t")
  df140007$DateTime <- as.POSIXct(df140007$DateTime, format = "%m/%d/%Y  %H:%M:%S")
  df140007$Date <- as.Date(df140007$Date, format = "%m/%d/%Y")
  df140007$Time <- as.character(df140007$Time)
  df140007$FixStatus <- as.character(df140007$FixStatus)
dfiridium <- read.delim("rawdata/iridium-all.txt", header = TRUE, sep = "\t")
  dfiridium$AnimalID <- as.integer(dfiridium$AnimalID)
  dfiridium$DateTime <- as.POSIXct(dfiridium$DateTime, format = "%m/%d/%Y  %H:%M:%S")
  dfiridium$Date <- as.Date(dfiridium$Date, format = "%m/%d/%Y")
  dfiridium$Time <- as.character(dfiridium$Time)
  dfiridium$TempC <- as.numeric(dfiridium$TempC)
  dfiridium$FixStatus <- as.character(dfiridium$FixStatus)
alllocs <- as.data.frame(bind_rows(df140057, df140016, df140007, dfiridium))
  
#capture information
  #note these data aren't perfectly classified (eg date/time)
allcap14 <- read.csv("rawdata/capture14.csv")
  cap14 <- allcap14[ ! allcap14$DeviceID %in% c(2496, 3521),]
allcap15 <- read.csv("rawdata/capture15.csv")
  cap15 <- allcap15[ ! allcap15$DeviceID %in% c(2496, 3521),]

##transmission end information
transend <- read.csv("rawdata/transend.csv", as.is = TRUE)
  transend$EndDateTime <- as.POSIXct(paste(transend$EndDate, transend$EndTime), 
                              format = "%m/%d/%Y  %H:%M")
  transend <- transend[,c("DeviceID", "AnimalID", "EndDateTime", "EndDate", "EndTime", 
                     "Lat", "Long", "StatusFeb16", "Cause1", "Cause2")] 

#clean up workspace
rm(df140057, df140016, df140007, dfiridium, allcap14, allcap15)
  
########ADD ANIMAL ID, SEX, CAPTURE YEAR############################

#create list of individual elk 
collarids <- as.data.frame(unique(alllocs$DeviceID))
names(collarids)[1] <- "DeviceID"

#order collarids and capture data by DeviceID to index all the same
collarids <- arrange(collarids, DeviceID)
cap14 <- arrange(cap14, DeviceID)
cap15 <- arrange(cap15, DeviceID)
  
#create blank df to store results in
newdf <- data.frame(matrix(ncol = 10, nrow = 0)) #create df wo NAs
colnames(newdf) <- c("DeviceID", "AnimalID", "DateTime", "Date", "Time", 
                     "Lat", "Long", "FixStatus", "DOP", "TempC")

#assign animalid based on device id and capture year
for(i in 1:nrow(collarids)){
  indiv <- collarids[i,] #treat each collarid as an individual
  id <- subset(alllocs, alllocs$DeviceID == indiv) #for each individual,
  id$AnimalID <- ifelse(id$Date < "2015-01-23",  #assign animalid
                        cap14$AnimalID[i], cap15$AnimalID[i])
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
