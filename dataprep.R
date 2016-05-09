###################################################
#Formatting 3300 collar data for location database#
#########NSERP - Kristin Barker - May 2016#########
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
df140057 <- read.delim("rawdata/140057-2204.txt", header = TRUE,
                       sep = "\t")
df140016 <- read.delim("rawdata/140016-3518.txt", header = TRUE,
                       sep = "\t")
df140007 <- read.delim("rawdata/140007-3529.txt", header = TRUE,
                       sep = "\t")
dfiridium <- read.delim("rawdata/iridium-all.txt", header = TRUE,
                        sep = "\t")
