#########################################################
# SharkRiverMovementCode_AppendxxxxDownloads.R
# Date Last Updated: 9/7/2021
# Author: J. Massie
# Purpose: Script takes raw VUE movement data from database offload and merges with a table to add distance
#           from river mouth (station distances in river km), screens for Rehage Lab tags, 
#           filters detections by species, and plots timerseries of movement tracks
#
#         8/27/2019 Update
#         Re-runs the VUE databases with updates discovered during QA where station distances or 
#         station names were missing (from VUE). These corrections have now been incorporated into the 
#         source VUE files
#
#         7/16/2020 Update
#         Code now creates a table subsetting and plotting only "live" tags, or those fish that were actively detected
#         In the system over the last 12 months.
#
#         9/7/2021 Update
#         Modified to incorporate downloads through AUG 2021
##########################################################

setwd("D:/DataProcessing/AcousticArray/")

# Create dataframe from Legacy VUE database, containing all download data pre-Summer2018
# This can be combined with new records to create comprehensive detection histories
#OldVUE <- read.csv("SRDB_Pre032018_Offload08262019.csv")
# Save this as a .rds to save import time in future...
#saveRDS(OldVUE, file = "RDS Files/OldVUEdatabase.rds")
# Read Back in table to resume
OldVUE <- readRDS("RDS Files/OldVUEdatabase.rds")


# Bring in new detections from July 2018 receiver downloads
NewVUE <- read.csv("VUE_ExportCSV/VUE_Export_AUG2021Updates.csv")
# Save this as a .rds to save import time in future...
saveRDS(NewVUE, file = "RDS Files/NewVUEdatabase_AUG2021.rds")
# Read Back in table to resume
#NewVUE <- readRDS("RDS Files/NewVUEdatabase_AUG2021.rds")


# Look at headers to make sure merge will take
head(OldVUE)
head(NewVUE)

## Note 9/7/2021
## After VUE update, there are now 2 new columns for transmitter type and sensor precision in database output .csv, neet to resolve to append new data
NewVUE1 <- subset(NewVUE, select = -c(Transmitter.Type, Sensor.Precision))

# Combine old and new VUE databases
AllVUE <- rbind(OldVUE, NewVUE1)
# Save this as a .rds to save import time in future...
saveRDS(AllVUE, file = "RDS Files/AllVUEdatabase_082021.rds")


#####
# 04022021 Determine the first detection dates for each receiver (hacking processing script)
# Read Back in table to resume
AllVUE <- readRDS("RDS Files/AllVUEdatabase_082021.rds")

# Simplify Header Name
names(AllVUE)[names(AllVUE)=="ï..Date.and.Time..UTC."] <- "Datetime_UTC"

# Look at headers
head(AllVUE)
class(AllVUE$Datetime_UTC)
# need to convert datetime to POSIX

# Convert datetime to Posix to determine first detection date
AllVUE$Datetime_UTC <- as.POSIXct(AllVUE$Datetime_UTC, format="%Y-%m-%d %H:%M:%S", tz = "UTC")
head(AllVUE)
class(AllVUE$Datetime_UTC)

# Now aggregate receivers to determine first detection (min datetime)
FirstDetect <- aggregate(Datetime_UTC~Receiver, AllVUE, min)


# Parse out datetime into date:time
library(splitstackshape) ###package to use cSplit and do text to column like excel
library(chron)

FirstDetect$Date.Time2 <- FirstDetect$Datetime_UTC

FirstDetect<-cSplit(FirstDetect, "Date.Time2", sep = " ", type.convert = FALSE)
head(FirstDetect)
colnames(FirstDetect)[3:4]<-c("Date", "Time")
head(FirstDetect)

# Parse out Receiver into VR2w:ReceiverNumber
FirstDetect$Receiver2 <- FirstDetect$Receiver

FirstDetect<-cSplit(FirstDetect, "Receiver2", sep = "-", type.convert = FALSE)
head(FirstDetect)
colnames(FirstDetect)[5:6]<-c("ReceiverModel", "ReceiverID")
head(FirstDetect)

# convert ReceiverID from character to integer
FirstDetect$ReceiverID <- as.integer(FirstDetect$ReceiverID)
class(FirstDetect$ReceiverID)

# Merge with list of current receivers to look at first detect date
CurrentReceivers <- read.csv("CurrentSRreceivers_2020on.csv")
class(CurrentReceivers$ReceiverID)


ReceiverFirstDeployed <- merge(FirstDetect, CurrentReceivers)

#write.csv(ReceiverFirstDeployed, "ReceiverFirstDeployed_04022021calc.csv", row.names=F)


##
## END OF RECEIVER NEW DATE CODE
##
#####



# Remove lat lon columns to simplify merger of new columns with updated coordinates
RawVUE1 <- subset(AllVUE, select = -c(Longitude, Latitude))

head(RawVUE1)

# Create dataframe from .csv table with a list of Rehage lab SRS tags and species ID
TagList <- read.csv("Acoustic Tags Master List_07152021update.csv")

head(TagList)

# Combine with movement data to select for only our fish
RehageVUE <- merge(RawVUE1, TagList)
# Save this as a .rds to save import time in future...
saveRDS(RehageVUE, file = "RDS Files/RehageVUEdatabase_09072021.rds")
# Read Back in table to resume
#RehageVUE <- readRDS("RDS Files/RehageVUEdatabase_09072021.rds")

head(RehageVUE)
summary(RehageVUE)

# Note 7/14/2020 takes ~ 3min30sec on Jordan's home computer to run the preceeding code from beginning to here with .csv imports

# Import table containing river distances from mouth with station names matching VUE output
StationData <- read.csv("Station_Distance_Updated07142020.csv")
# Rename default VUE station names column to allow merger with distances
names(StationData)[names(StationData)=="VUE_Name"] <- "Station.Name"
head(StationData)

# Add distances to  reduced movement data
RehageVUE_Distance <- merge(RehageVUE, StationData)
head(RehageVUE_Distance)

# Save this as a .rds to save import time in future...
saveRDS(RehageVUE_Distance, file = "RDS Files/RehageVUE_WithDistance_09072021.rds")
# Read Back in table to resume
#RehageVUE_Distance <- readRDS("RDS Files/RehageVUE_WithDistance_09072021.rds")
#names(RehageVUE_Distance)[names(RehageVUE_Distance)=="ï..Date.and.Time..UTC."] <- "Datetime_UTC"


# QA THESE DATA!!!
# As of 9/7/2021 all Rehage fish records have been QA/QC'd (notes on RBHW below)
# As of 8/27/2019 all Rehage fish records have been QA/QC'd
# Only 1 record remains after adding distances, and this is for a station assigned a name of "FFF" in July 2014
# No known location for this station, data are OK to continue in analyses
TestMissingData <- merge(RehageVUE, StationData, all=T)
WhatsMissing <- subset(TestMissingData, is.na(TestMissingData$Distance))
# Looks like some stations are missing on the NOV 2020 dowload round as follows
# VR2W-109553 (8908 detections) VR2W-128968 (1428 detections) VR2W-128958 (184 detections) VR2W-101313 (1 detection)
# the 1 detection is FFF like before, but I now see that the RBHW stations were not assigned for the other 3
# (RBHW1 didnt have any detections, which is why there are only 3 not 4 problem .vrl files)
summary(WhatsMissing$Receiver)


# Note 7/14/2020 takes ~ 6 minutes on Jordan's home computer to run the preceeding code from beginning to here with .csv imports
# ... and eats up about 16.5 GB of RAM at the peak

# Example Below for Manually Assigning Stations/Distances post hoc
#ThisReceiver <- subset(WhatsMissing, WhatsMissing$Receiver == "VR2W-128958")
#OtherReceivers <- subset(WhatsMissing, WhatsMissing$Receiver != "VR2W-128958")
##### !!!
##  NOTE: 1 records are lost after merging with station table.  Some of these are missing station names, and one record has station = FFF
##### !!!
# Correct the error for the missing station on receiver VR2W-128958
# Records from the post-Jan 2019 download indicate that this was the unit (VR2W-128958) 
# taken from VUE Station "Otter Creek OT1" and accounts for 10807 of the missing station names
# Looks like still some QA to do from the 2016 missing stations though...
#ThisReceiver$Station.Name <- "Otter Creek OT1"
#ThisReceiver$Distance <- 27.08
# Bring back in with other data
#RehageVUE_Distance2 <- rbind(RehageVUE_Distance, ThisReceiver)


library(scales)

# Convert datetime to Posix for subsetting and plotting
RehageVUE_Distance$Datetime_UTC <- as.POSIXct(RehageVUE_Distance$Datetime_UTC, format="%Y-%m-%d %H:%M:%S", tz = "UTC")
head(RehageVUE_Distance)
class(RehageVUE_Distance$Datetime_UTC)
summary(RehageVUE_Distance$Datetime_UTC)

# Save table for later use to save processing time
saveRDS(RehageVUE_Distance, file = "RDS Files/RehageVUE_Distances_Datetime_CombinedOldNew_NewTagList09072021.rds")
# Read Back in table to resume
#RehageVUE <- readRDS("RDS Files/RehageVUE_Distances_Datetime_CombinedOldNew_NewTagList09072021.rds")


# Compute a sample size (detection count) for each tag number.  Distance is only used as a proxy in order to count the number of rows occuring
# for each transmitter, and the "length" argument simply returns a count of those rows
PingCount <-aggregate(Distance~Transmitter, RehageVUE_Distance, length)
names(PingCount)[names(PingCount)=="Distance"] <- "n"

# Merge reduced VUE data with the sample size screener to get just the fish of interest
RehageVUE_Distance_n <- merge(RehageVUE_Distance, PingCount)

# Save table for later use to save processing time
saveRDS(RehageVUE_Distance_n, file ="RDS Files/RehageVUE_Distances_Datetime_n_09072021.rds")



#Data for FCE submission 11/2021
# Read Back in table to resume
RehageVUE_Distance_n <- readRDS("RDS Files/RehageVUE_Distances_Datetime_n_09072021.rds")

FCEdata <- subset(RehageVUE_Distance_n, select = c(Transmitter, Species, Datetime_UTC, Latitude, Longitude))

FCEdata2 <- subset(FCEdata, FCEdata$Datetime_UTC >= "2012-02-01 00:00:00")

summary(FCEdata2)

class(FCEdata2$Species)

FCEdata2$Species <- as.character(FCEdata2$Species)

FCEdata2$Species[FCEdata2$Species=="Snook"] <- "Common Snook (Centropomus undecimalis)"
FCEdata2$Species[FCEdata2$Species=="Bass"] <- "Largemouth Bass (Micropterus salmoides)"
FCEdata2$Species[FCEdata2$Species=="Redfish"] <- "Red Drum (Sciaenops ocellatus)"
FCEdata2$Species[FCEdata2$Species=="Tarpon"] <- "Atlantic Tarpon (Megalops atlanticus)"

FCEdata2$Species <- as.factor(FCEdata2$Species)

summary(FCEdata2)


write.csv(FCEdata2, "RehageAcousticDetections_FCEsubmission_112021.csv", row.names = F)

  
  
  
  
  # Remove fish with very small detection records (false detections?)
GoodRecords <- subset(RehageVUE_Distance_n, RehageVUE_Distance_n$n >= 5)
# Whose the culprit for the singele bad record on 7/14/2020?
BadRecords <- subset(RehageVUE_Distance_n, RehageVUE_Distance_n$n < 5)
# Looks like tag A69-1601-35758 was only detected once, at RBE2 on 5/17/2016

AllSnook <- subset(GoodRecords, GoodRecords$Species == "Snook")
AllBass <- subset(GoodRecords, GoodRecords$Species == "Bass")
AllRedfish <- subset(GoodRecords, GoodRecords$Species == "Redfish")

# Look at number of individuals in each species
Snook <- as.data.frame(unique(AllSnook$Transmitter))
Bass <- as.data.frame(unique(AllBass$Transmitter))
Redfish <- as.data.frame(unique(AllRedfish$Transmitter))

Snook
Bass
Redfish

saveRDS(AllSnook, file = "RDS Files/AllSnook_PeriodOfRecord_09072021.rds")
# Read Back in table to resume
#AllSnook <- readRDS("RDS Files/AllSnook_PeriodOfRecord_09072021.rds")

saveRDS(AllBass, file = "RDS Files/AllBass_PeriodOfRecord_09072021.rds")
# Read Back in table to resume
#AllBass <- readRDS("RDS Files/AllBass_PeriodOfRecord_09072021.rds")

saveRDS(AllRedfish, file = "RDS Files/AllRedfish_PeriodOfRecord_09072021.rds")
# Read Back in table to resume
#AllRedfish <- readRDS("RDS Files/AllRedfish_PeriodOfRecord_09072021.rds")


########### 7/16/2020 #############
# Create dataframes of current tags using a detection in last 12mos as screening threshold
RehageVUE_Distance_n <- readRDS("RDS Files/RehageVUE_Distances_Datetime_n_09072021.rds")

# Look at detections occuring over the last year
RecentDetections <- subset(RehageVUE_Distance_n, RehageVUE_Distance_n$Datetime_UTC >= "2020-08-01 00:00:00")

# create a list of unique tag numbers from last year of detections
RecentTags <- as.data.frame(unique(RecentDetections$Transmitter))
head(RecentTags)
# rename header as "Transmitter" to match data
names(RecentTags)[names(RecentTags)=="unique(RecentDetections$Transmitter)"] <- "Transmitter"

# Subset all detections using only tags detected in last year
RecentTransmitterTracks <- merge(RehageVUE_Distance_n, RecentTags)

# Subset by species for plotting
RecentSnook <- subset(RecentTransmitterTracks, RecentTransmitterTracks$Species == "Snook")
RecentBass <- subset(RecentTransmitterTracks, RecentTransmitterTracks$Species == "Bass")
RecentRedfish <- subset(RecentTransmitterTracks, RecentTransmitterTracks$Species == "Redfish")

# Look at range in detection dates for plotting purposes
summary(RecentTransmitterTracks$Datetime_UTC)
summary(RecentSnook$Datetime_UTC)
summary(RecentBass$Datetime_UTC)
summary(RecentRedfish$Datetime_UTC)

# Look at number of individuals in each species
Snook_Yr <- as.data.frame(unique(RecentSnook$Transmitter))
Bass_Yr <- as.data.frame(unique(RecentBass$Transmitter))
Redfish_Yr <- as.data.frame(unique(RecentRedfish$Transmitter))

#################
# For additional insight, repeat above steps, but for fish detected in last 6 months
# Look at detections occuring over the last 6 months
LiveDetections <- subset(RehageVUE_Distance_n, RehageVUE_Distance_n$Datetime_UTC >= "2021-02-01 00:00:00")

# create a list of unique tag numbers from last 6 months of detections
LiveTags <- as.data.frame(unique(LiveDetections$Transmitter))
head(LiveTags)
# rename header as "Transmitter" to match data
names(LiveTags)[names(LiveTags)=="unique(LiveDetections$Transmitter)"] <- "Transmitter"

# Subset all detections using only tags detected in last 6 months
LiveTransmitterTracks <- merge(RehageVUE_Distance_n, LiveTags)

# Subset by species for plotting
LiveSnook <- subset(LiveTransmitterTracks, LiveTransmitterTracks$Species == "Snook")
LiveBass <- subset(LiveTransmitterTracks, LiveTransmitterTracks$Species == "Bass")
LiveRedfish <- subset(LiveTransmitterTracks, LiveTransmitterTracks$Species == "Redfish")

# Look at number of individuals in each species
Snook_6mos <- as.data.frame(unique(LiveSnook$Transmitter))
Bass_6mos <- as.data.frame(unique(LiveBass$Transmitter))
Redfish_6mos <- as.data.frame(unique(LiveRedfish$Transmitter))


# END OF SCREENING CODE
############################################################################################

# PLOTTING:

# load required package if not already loaded
library(ggplot2)
library(scales)

###
### Snook Plotting (Code for all plots replicated for each species)
###

### PERIOD OF RECORD PLOTTING - Snook

# Specify the number of plots-per-page to display in the final .pdf output
noPlots <- 3

# Save all variables in a seperate vector to select in for-loop
allVars <- unique(AllSnook$Transmitter)
noVars <- length(allVars)

# Indices for plotting variables
plotSequence <- c(seq(0, noVars-1, by = noPlots), noVars)

# Create a new .pdf file to save plotted output for each site to a single document
# Can add arguments to adjust page size, layout etc. if desired
pdf("Plots/RehageSnook09072021.pdf")

# Loop to plot movement for each transimitter (tag) in the dataframe
for(ii in 2:length(plotSequence)){
  
  # Select start and end of variables to plot
  start <- plotSequence[ii-1] + 1
  end <- plotSequence[ii]
  
  # Subset the variables and save new temporary data.frame
  tmp <- subset(AllSnook, Transmitter %in% allVars[start:end])
  cat(unique(tmp$Transmitter), "\n")
  
  # Generate plots with lines/points showing transmitter detections (as river km points at time)
  p <- ggplot(tmp,aes(Datetime_UTC, Distance, group = 1)) + geom_line(color="black") + geom_point(color="black") +
    # Add vertical line showing when hurricane Irma made landfall at Marco Island (UTC time)
    #geom_vline(xintercept=as.numeric(as.POSIXct("2017-09-10 19:35:00")), linetype=4, color = "red") +
    # Display output as a wrap of multiple plots on each page and input the desired number of columns to display
    # NOTE: The argument 'scale = "free"' results in axis labels for each plot rather than the default of only
    # labeling the far right and bottom axis on a page of wrapped plots (scale = "fixed")
    facet_wrap(~Transmitter, ncol = 1, scale = "fixed") +
    # Indicate break intervals for x axis major grid display. Options are  "sec", "min", "hour", "day", "week", 
    # "month", "year". Can be by an integer and a space, or followed by "s"
    # NOTE: data must be in POSIXct format for this breaks function, hence the earlier conversion.
    # Limits allow you to adjust the period to plot, warnings will indicate the records outside this window that have been removed
    scale_x_datetime(breaks = date_breaks("2 months"), limits = c(
      as.POSIXct("2012-01-01 00:00:00"),
      as.POSIXct("2021-09-01 00:00:00")
    )) +
    # Label axis with prefered text
    labs(x = "Date",  y = "Distance from Mouth (river km)") +
    # If desired, set output display limits for the y axis (temperature)
    ylim(0, 35) +
    # Change the angle and text size of x-axis tick labels to make more readable in final plots
    theme(axis.text.x=element_text(angle= 90, size=8))
  print(p)
}

# Close and finalize .pdf file containing all output plots
dev.off()




###
### POR PLOTTING FOR FISH WITH RECENT DETECTIONS (w/in last 12 months) - Snook

# 09142021
# Look at the 3 Eta movers
EtaTags <- read.csv("EtaFish_09142021.csv")
RecentSnookEta <- merge(RecentSnook, EtaTags)


# Specify the number of plots-per-page to display in the final .pdf output
noPlots <- 3

# Save all variables in a seperate vector to select in for-loop
allVars <- unique(RecentSnookEta$Transmitter)
noVars <- length(allVars)

# Indices for plotting variables
plotSequence <- c(seq(0, noVars-1, by = noPlots), noVars)

# Create a new .pdf file to save plotted output for each site to a single document
# Can add arguments to adjust page size, layout etc. if desired
pdf("Plots/Snook_RecentDetections_09142021_EtaMoves.pdf")

# Loop to plot movement for each transimitter (tag) in the dataframe
for(ii in 2:length(plotSequence)){
  
  # Select start and end of variables to plot
  start <- plotSequence[ii-1] + 1
  end <- plotSequence[ii]
  
  # Subset the variables and save new temporary data.frame
  tmp <- subset(RecentSnookEta, Transmitter %in% allVars[start:end])
  cat(unique(tmp$Transmitter), "\n")
  
  # Generate plots with lines/points showing transmitter detections (as river km points at time)
  p <- ggplot(tmp,aes(Datetime_UTC, Distance, group = 1)) + geom_line(color="black") + geom_point(color="black") +
    # Add vertical line showing when hurricane Irma made landfall at Marco Island (UTC time)
    geom_vline(xintercept=as.numeric(as.POSIXct("2020-11-01 00:00:00")), linetype=4, color = "black") +
    geom_vline(xintercept=as.numeric(as.POSIXct("2020-11-09 00:00:00")), linetype=4, color = "red") +
    geom_vline(xintercept=as.numeric(as.POSIXct("2020-11-15 00:00:00")), linetype=4, color = "black") +
    # Display output as a wrap of multiple plots on each page and input the desired number of columns to display
    # NOTE: The argument 'scale = "free"' results in axis labels for each plot rather than the default of only
    # labeling the far right and bottom axis on a page of wrapped plots (scale = "fixed")
    facet_wrap(~Transmitter, ncol = 1, scale = "fixed") +
    # Indicate break intervals for x axis major grid display. Options are  "sec", "min", "hour", "day", "week", 
    # "month", "year". Can be by an integer and a space, or followed by "s"
    # NOTE: data must be in POSIXct format for this breaks function, hence the earlier conversion.
    # Limits allow you to adjust the period to plot, warnings will indicate the records outside this window that have been removed
    scale_x_datetime(breaks = date_breaks("1 month"), limits = c(
      as.POSIXct("2020-01-01 00:00:00"),
      as.POSIXct("2020-12-31 00:00:00")
    )) +
    # Label axis with prefered text
    labs(x = "Date",  y = "Distance from Mouth (river km)") +
    # If desired, set output display limits for the y axis (temperature)
    ylim(0, 35) +
    # Change the angle and text size of x-axis tick labels to make more readable in final plots
    theme(axis.text.x=element_text(angle= 90, size=8))
  print(p)
}

# Close and finalize .pdf file containing all output plots
dev.off()


# Only Snook Run for full Plotting + 12 months on 9/7/2021
# Repeat below and change files/dates if want to do 12 month plots for other species

### PERIOD OF RECORD PLOTTING - Bass

# Specify the number of plots-per-page to display in the final .pdf output
noPlots <- 3

# Save all variables in a seperate vector to select in for-loop
allVars <- unique(AllBass$Transmitter)
noVars <- length(allVars)

# Indices for plotting variables
plotSequence <- c(seq(0, noVars-1, by = noPlots), noVars)

# Create a new .pdf file to save plotted output for each site to a single document
# Can add arguments to adjust page size, layout etc. if desired
pdf("Plots/RehageBass09072021.pdf")

# Loop to plot movement for each transimitter (tag) in the dataframe
for(ii in 2:length(plotSequence)){
  
  # Select start and end of variables to plot
  start <- plotSequence[ii-1] + 1
  end <- plotSequence[ii]
  
  # Subset the variables and save new temporary data.frame
  tmp <- subset(AllBass, Transmitter %in% allVars[start:end])
  cat(unique(tmp$Transmitter), "\n")
  
  # Generate plots with lines/points showing transmitter detections (as river km points at time)
  p <- ggplot(tmp,aes(Datetime_UTC, Distance, group = 1)) + geom_line(color="black") + geom_point(color="black") +
    # Add vertical line showing when hurricane Irma made landfall at Marco Island (UTC time)
    #geom_vline(xintercept=as.numeric(as.POSIXct("2017-09-10 19:35:00")), linetype=4, color = "red") +
    # Display output as a wrap of multiple plots on each page and input the desired number of columns to display
    # NOTE: The argument 'scale = "free"' results in axis labels for each plot rather than the default of only
    # labeling the far right and bottom axis on a page of wrapped plots (scale = "fixed")
    facet_wrap(~Transmitter, ncol = 1, scale = "fixed") +
    # Indicate break intervals for x axis major grid display. Options are  "sec", "min", "hour", "day", "week", 
    # "month", "year". Can be by an integer and a space, or followed by "s"
    # NOTE: data must be in POSIXct format for this breaks function, hence the earlier conversion.
    # Limits allow you to adjust the period to plot, warnings will indicate the records outside this window that have been removed
    scale_x_datetime(breaks = date_breaks("2 months"), limits = c(
      as.POSIXct("2012-01-01 00:00:00"),
      as.POSIXct("2021-09-01 00:00:00")
    )) +
    # Label axis with prefered text
    labs(x = "Date",  y = "Distance from Mouth (river km)") +
    # If desired, set output display limits for the y axis (temperature)
    ylim(0, 35) +
    # Change the angle and text size of x-axis tick labels to make more readable in final plots
    theme(axis.text.x=element_text(angle= 90, size=8))
  print(p)
}

# Close and finalize .pdf file containing all output plots
dev.off()




#####
### POR PLOTTING FOR FISH WITH RECENT DETECTIONS (w/in last 12 months) - Bass

# Specify the number of plots-per-page to display in the final .pdf output
noPlots <- 3

# Save all variables in a seperate vector to select in for-loop
allVars <- unique(RecentBass$Transmitter)
noVars <- length(allVars)

# Indices for plotting variables
plotSequence <- c(seq(0, noVars-1, by = noPlots), noVars)

# Create a new .pdf file to save plotted output for each site to a single document
# Can add arguments to adjust page size, layout etc. if desired
pdf("Plots/Bass_RecentDetections_07162020.pdf")

# Loop to plot movement for each transimitter (tag) in the dataframe
for(ii in 2:length(plotSequence)){
  
  # Select start and end of variables to plot
  start <- plotSequence[ii-1] + 1
  end <- plotSequence[ii]
  
  # Subset the variables and save new temporary data.frame
  tmp <- subset(RecentBass, Transmitter %in% allVars[start:end])
  cat(unique(tmp$Transmitter), "\n")
  
  # Generate plots with lines/points showing transmitter detections (as river km points at time)
  p <- ggplot(tmp,aes(Datetime_UTC, Distance, group = 1)) + geom_line(color="black") + geom_point(color="black") +
    # Add vertical line showing when hurricane Irma made landfall at Marco Island (UTC time)
    #geom_vline(xintercept=as.numeric(as.POSIXct("2017-09-10 19:35:00")), linetype=4, color = "red") +
    # Display output as a wrap of multiple plots on each page and input the desired number of columns to display
    # NOTE: The argument 'scale = "free"' results in axis labels for each plot rather than the default of only
    # labeling the far right and bottom axis on a page of wrapped plots (scale = "fixed")
    facet_wrap(~Transmitter, ncol = 1, scale = "fixed") +
    # Indicate break intervals for x axis major grid display. Options are  "sec", "min", "hour", "day", "week", 
    # "month", "year". Can be by an integer and a space, or followed by "s"
    # NOTE: data must be in POSIXct format for this breaks function, hence the earlier conversion.
    # Limits allow you to adjust the period to plot, warnings will indicate the records outside this window that have been removed
    scale_x_datetime(breaks = date_breaks("1 month"), limits = c(
      as.POSIXct("2017-01-01 00:00:00"),
      as.POSIXct("2020-07-01 00:00:00")
    )) +
    # Label axis with prefered text
    labs(x = "Date",  y = "Distance from Mouth (river km)") +
    # If desired, set output display limits for the y axis (temperature)
    ylim(0, 35) +
    # Change the angle and text size of x-axis tick labels to make more readable in final plots
    theme(axis.text.x=element_text(angle= 90, size=8))
  print(p)
}

# Close and finalize .pdf file containing all output plots
dev.off()

#####
###

### PERIOD OF RECORD PLOTTING - Redfish

# Specify the number of plots-per-page to display in the final .pdf output
noPlots <- 3

# Save all variables in a seperate vector to select in for-loop
allVars <- unique(AllRedfish$Transmitter)
noVars <- length(allVars)

# Indices for plotting variables
plotSequence <- c(seq(0, noVars-1, by = noPlots), noVars)

# Create a new .pdf file to save plotted output for each site to a single document
# Can add arguments to adjust page size, layout etc. if desired
pdf("Plots/RehageRedfish09072021.pdf")

# Loop to plot movement for each transimitter (tag) in the dataframe
for(ii in 2:length(plotSequence)){
  
  # Select start and end of variables to plot
  start <- plotSequence[ii-1] + 1
  end <- plotSequence[ii]
  
  # Subset the variables and save new temporary data.frame
  tmp <- subset(AllRedfish, Transmitter %in% allVars[start:end])
  cat(unique(tmp$Transmitter), "\n")
  
  # Generate plots with lines/points showing transmitter detections (as river km points at time)
  p <- ggplot(tmp,aes(Datetime_UTC, Distance, group = 1)) + geom_line(color="black") + geom_point(color="black") +
    # Add vertical line showing when hurricane Irma made landfall at Marco Island (UTC time)
    #geom_vline(xintercept=as.numeric(as.POSIXct("2017-09-10 19:35:00")), linetype=4, color = "red") +
    # Display output as a wrap of multiple plots on each page and input the desired number of columns to display
    # NOTE: The argument 'scale = "free"' results in axis labels for each plot rather than the default of only
    # labeling the far right and bottom axis on a page of wrapped plots (scale = "fixed")
    facet_wrap(~Transmitter, ncol = 1, scale = "fixed") +
    # Indicate break intervals for x axis major grid display. Options are  "sec", "min", "hour", "day", "week", 
    # "month", "year". Can be by an integer and a space, or followed by "s"
    # NOTE: data must be in POSIXct format for this breaks function, hence the earlier conversion.
    # Limits allow you to adjust the period to plot, warnings will indicate the records outside this window that have been removed
    scale_x_datetime(breaks = date_breaks("2 months"), limits = c(
      as.POSIXct("2012-01-01 00:00:00"),
      as.POSIXct("2021-09-01 00:00:00")
    )) +
    # Label axis with prefered text
    labs(x = "Date",  y = "Distance from Mouth (river km)") +
    # If desired, set output display limits for the y axis (temperature)
    ylim(0, 35) +
    # Change the angle and text size of x-axis tick labels to make more readable in final plots
    theme(axis.text.x=element_text(angle= 90, size=8))
  print(p)
}

# Close and finalize .pdf file containing all output plots
dev.off()




#####
### POR PLOTTING FOR FISH WITH RECENT DETECTIONS (w/in last 12 months) - Redfish

# Specify the number of plots-per-page to display in the final .pdf output
noPlots <- 3

# Save all variables in a seperate vector to select in for-loop
allVars <- unique(RecentRedfish$Transmitter)
noVars <- length(allVars)

# Indices for plotting variables
plotSequence <- c(seq(0, noVars-1, by = noPlots), noVars)

# Create a new .pdf file to save plotted output for each site to a single document
# Can add arguments to adjust page size, layout etc. if desired
pdf("Plots/Redfish_RecentDetections_07162020.pdf")

# Loop to plot movement for each transimitter (tag) in the dataframe
for(ii in 2:length(plotSequence)){
  
  # Select start and end of variables to plot
  start <- plotSequence[ii-1] + 1
  end <- plotSequence[ii]
  
  # Subset the variables and save new temporary data.frame
  tmp <- subset(RecentRedfish, Transmitter %in% allVars[start:end])
  cat(unique(tmp$Transmitter), "\n")
  
  # Generate plots with lines/points showing transmitter detections (as river km points at time)
  p <- ggplot(tmp,aes(Datetime_UTC, Distance, group = 1)) + geom_line(color="black") + geom_point(color="black") +
    # Add vertical line showing when hurricane Irma made landfall at Marco Island (UTC time)
    #geom_vline(xintercept=as.numeric(as.POSIXct("2017-09-10 19:35:00")), linetype=4, color = "red") +
    # Display output as a wrap of multiple plots on each page and input the desired number of columns to display
    # NOTE: The argument 'scale = "free"' results in axis labels for each plot rather than the default of only
    # labeling the far right and bottom axis on a page of wrapped plots (scale = "fixed")
    facet_wrap(~Transmitter, ncol = 1, scale = "fixed") +
    # Indicate break intervals for x axis major grid display. Options are  "sec", "min", "hour", "day", "week", 
    # "month", "year". Can be by an integer and a space, or followed by "s"
    # NOTE: data must be in POSIXct format for this breaks function, hence the earlier conversion.
    # Limits allow you to adjust the period to plot, warnings will indicate the records outside this window that have been removed
    scale_x_datetime(breaks = date_breaks("1 month"), limits = c(
      as.POSIXct("2017-01-01 00:00:00"),
      as.POSIXct("2020-07-01 00:00:00")
    )) +
    # Label axis with prefered text
    labs(x = "Date",  y = "Distance from Mouth (river km)") +
    # If desired, set output display limits for the y axis (temperature)
    ylim(0, 35) +
    # Change the angle and text size of x-axis tick labels to make more readable in final plots
    theme(axis.text.x=element_text(angle= 90, size=8))
  print(p)
}

# Close and finalize .pdf file containing all output plots
dev.off()
#####




#####################################
## END OF FUNCTIONAL CODE
#####################################



