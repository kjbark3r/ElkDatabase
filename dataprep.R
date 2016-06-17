###################################################
## Formatting elk collar data for final database ##
#######  NSERP - Kristin Barker - May 2016  #######
###################################################

##SET WORKING DIRECTORY
###GitHub repository on Work computer or personal laptop

wd_workcomp <- "C:\\Users\\kristin.barker\\Documents\\GitHub\\NSERP_DB"
wd_laptop <- "C:\\Users\\kjbark3r\\Documents\\GitHub\\NSERP_DB"

if (file.exists(wd_workcomp)) {
  setwd(wd_workcomp)
} else {
  if(file.exists(wd_laptop)) {
    setwd(wd_laptop)
  } else {
      cat("Are you SURE you got that file path right?\n")
  }
}
rm(wd_workcomp, wd_laptop)

##LOAD LIBRARIES
library(dplyr)
library(tidyr)

##PREP DATA

#collar locations - format and combine
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
  alllocs <- subset(alllocs, select = -DateTime) #avoid daylight savings NAs
 
#transmission start information (capture dates/times)
  #remove malfunctioned collars we have no data for
allcap14 <- read.csv("rawdata/capture14.csv")
  cap14 <- allcap14[ ! allcap14$DeviceID %in% c(2496, 3521),]
allcap15 <- read.csv("rawdata/capture15.csv", as.is = TRUE)
  cap15 <- allcap15[ ! allcap15$DeviceID %in% c(2496, 3521),]

##transmission end information (mortality/malfunction/collar drop dates/times)
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

#create blank dataframe to store results of for loop in
newdf <- data.frame(matrix(ncol = 9, nrow = 0)) #create df wo NAs
colnames(newdf) <- c("DeviceID", "AnimalID", "Date", "Time", 
                     "Lat", "Long", "FixStatus", "DOP", "TempC")

#assign animalid based on device id and capture year
##because some collars were redeployed on new individuals in 2015 
for(i in 1:nrow(collarids)){
  indiv <- collarids[i,] #treat each collarid as an individual
  id <- subset(alllocs, alllocs$DeviceID == indiv) #for each individual,
  id$AnimalID <- ifelse(id$Date < "2015-01-23",  #for locns b4 2015 capture,
                        cap14$AnimalID[i], cap15$AnimalID[i]) #animalid=2014
  newdf <- as.data.frame(bind_rows(newdf, id))  #after 2015 capture, id=2015
}          #dataframe doesn't truncate lat/longs - keeps all decimal places

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
newdf <- as.data.frame(left_join(newdf, transend.sub, by = "AnimalID")) #transend data

elklocs <- newdf[!(newdf$Date >= newdf$EndDate & newdf$Time > newdf$EndTime
                 | newdf$Date > newdf$EndDate),] #only keep locs b4 transend date/times


##ADD MISC HELPFUL INFO####################################

#season
elklocs$Month <- as.numeric(format(as.POSIXlt(elklocs$Date), "%m"))
elklocs$Season <- ifelse(between(elklocs$Month, 03, 05), "Spring", 
                         ifelse(between(elklocs$Month, 06, 08), "Summer", 
                                ifelse(between(elklocs$Month, 09, 11), "Fall", "Winter")
                                 )
                          )
elklocs <- subset(elklocs, select = -Month)              

##EXPORT DATA####################################

#without location NAs
elklocs.nona <- elklocs[!is.na(elklocs$Lat),]

#with odd hour locations removed from 3300s
#to match bihourly locations from iridiums
is.odd <- function(x) { x %% 2 == 1}

elklocs.eq <- elklocs.nona 
elklocs.eq$Hour <- as.POSIXlt(elklocs.eq$Time, format = "%H:%M")
elklocs.eq <- subset(elklocs.eq, !(AnimalID == 140560 & is.odd(Hour$hour)) &
                                 !(AnimalID == 140910 & is.odd(Hour$hour)) &
                                 !(AnimalID == 141490 & is.odd(Hour$hour)))
elklocs.eq <- subset(elklocs.eq, select = -Hour)
  
#export data
write.csv(elklocs, file = "allcollardata-seasons.csv")
write.csv(elklocs.nona, file = "collardata_locsonly-seasons.csv")
write.csv(elklocs.eq, file = "collardata-locsonly-equalsampling.csv")
