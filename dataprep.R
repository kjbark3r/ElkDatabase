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
allcap14 <- read.csv("rawdata/capture14.csv")
allcap15 <- read.csv("rawdata/capture15.csv")

##SET UP LOCN DATAFRAME
alllocs <- bind_rows(df140057, df140016, df140007, dfiridium)
  alllocs$DateTime <- strptime(alllocs$DateTime, format = "%m/%d/%Y  %H:%M")
  alllocs$Date <- as.Date(alllocs$Date, format = "%m/%d/%Y")
  #alllocs$Time <- strptime(alllocs$Time, format = "%H:%M")  #needs date too

##[possibly unnecessary] CREATE LIST OF ELK 
collarids <- as.data.frame(unique(alllocs$DeviceID))
names(collarids)[1] <- "DeviceID"
ncollars <- nrow(collarids)

##ADD ANIMAL IDS
#See #..AAAAAAND THE PITCH! section of playtime.R
#You're getting close
    