###Loading Libraries and Opening Data
rm(list=ls()) #clears the global environment
#Load libraries and install packages
packages.wanted <- c("conflicted", "finalfit", "leafgl","mapview","data.table","s2","sf","r5r","GGally","MASS", "robustbase","mvoutlier","RColorBrewer","rgdal","ggplot2","Hmisc", "dplyr","purrr","foreign","stargazer","tidyr")
#for (package in packages.wanted) install.packages(package,character.only=TRUE)
for (package in packages.wanted) require(package,character.only=TRUE)

conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")
conflict_prefer("summarize", "dplyr")

#Set the working directory
setwd("~/Documents/GitHub/LODES-ACS-commuting-flows/LODES-ACS-commuting-flows/Data")


# Calculate network distance between centroids using walking distance

# Define a  file path to GTFS data
# GTFS data downloaded from: http://transitfeeds.com/p/chicago-transit-authority/165
# Date: 11 November 2021 - you must run the r5r query for the same date
data_path <- "~/Documents/GitHub/LODES-ACS-commuting-flows/LODES-ACS-commuting-flows/Data/chicagopoa"
list.files(data_path)

# Open and show points of interest (i.e., census tract centroids in this case) from Chicago 
# poi <- fread(file.path(data_path, "poi.csv"))
# head(poi)
poi2 <- fread(file.path(data_path, "poi2.csv"))
# head(poi2)

# poi$toId <- as.character(poi$id)
# poi$fromId <- as.character(poi$id)

poi2$fromId <- as.character(poi2$id)
poi2$toId <-  as.character(poi2$id)

#Increase working memory available for Java
# options(java.parameters = "-Xmx10G")


# # Build a routable transport network with setup_R5
# 
# # Indicate the path where OSM and GTFS data are stored
# r5r_core <- setup_r5(data_path = data_path, verbose = FALSE)
# 
# # Now that we have created the requisite files, we can use the travel_time_matrix() function
# # to calculate transit travel times from all origins to destinations based on the supplied parameters.
# 
# mode <- c("WALK")
# max_walk_dist <- 1000000
# max_trip_duration <- 180000
# departure_datetime <- as.POSIXct("11-11-2021 01:00:00",
#                                  format = "%d-%m-%Y %H:%M:%S")
# 
# # Calculate a travel time matrix
# dist <- travel_time_matrix(r5r_core = r5r_core,
#                            origins = poi2,
#                            destinations = poi2,
#                            mode = mode,
#                            departure_datetime = departure_datetime,
#                            time_window = 1,
#                            breakdown = FALSE,
#                            max_walk_dist = max_walk_dist,
#                            max_trip_duration = max_trip_duration,
#                            walk_speed = 5,
#                            bike_speed = 18,
#                            verbose = FALSE)
# 
# stop_r5(r5r_core)
# rJava::.jgc(R.gc = TRUE)
# 
# #1740494
# 
# write.csv(dist, file = "dist7.csv")
# 
#Or save time and read in pre-calculated distances
dist <- read.csv("dist7.csv")

dist$fromId <- as.character(dist$fromId)
dist$toId <- as.character(dist$toId)

dist_m1 <- merge(poi2,dist,by="fromId", all.x=TRUE)
dist_m1 <- dist_m1 %>%
  select(fromId, toId.y, GEOID, travel_time)
dist_m1 <- plyr::rename(dist_m1, c("GEOID" = "origTR", "toId.y" = "toId"))

dist_m2 <- merge(dist_m1, poi2,by="toId", all.x=TRUE)
dist_m2 <- dist_m2 %>%
  select(fromId.x, toId,origTR, GEOID, travel_time)
dist_m2 <- plyr::rename(dist_m2, c("GEOID" = "destTR", "fromId.x" = "fromId"))

dist_m2$uniqueID <- paste0(as.character(dist_m2$fromId),", ", as.character(dist_m2$toId))

dist_m2$origCT <- substr(dist_m2$origTR, 1, 5)
dist_m2$destCT <- substr(dist_m2$destTR, 1, 5)

#Create a unique identifier for origins and destinations
dist_m2$TRIP_ID <- paste0(as.character(dist_m2$origTR),", ", as.character(dist_m2$destTR))

#Ensure only properly-calculated distances (i.e., "complete cases" or non-NA values, are retained)
# dist_m2 <- dist_m2[complete.cases(dist_m2[ , 5]),]

dist_m2$origTR <- as.numeric(dist_m2$origTR)
dist_m2$destTR <- as.numeric(dist_m2$destTR)

#Load ACS data
ACS <- read.csv("ACS_2013_2017_Mode.csv")
ACS <- as.data.frame(ACS)
ACS <- plyr::rename(ACS, c("GEOID10" = "origTR"))

MergeACS <- merge(dist_m2,ACS,by="origTR", all.x=TRUE)

#Rename origin-based ycoord and xcoord for "h_lat" and "h_long"
MergeChi <- plyr::rename(MergeACS, c("lat" = "h_lat", "lon" = "h_long"))

#Re-join ACS data, this time by destTR
ACSdest <- read.csv("ACS_2013_2017_Mode.csv")
ACSdest <- as.data.frame(ACSdest)
ACSdest <- plyr::rename(ACSdest, c("GEOID10" = "destTR"))
ACSdest <- plyr::rename(ACSdest, c("lat" = "w_lat", "lon" = "w_long"))
ACSdest <- ACSdest %>%
  select(destTR, w_lat, w_long)

########Create O-D Trips#######################

#Load OD matrix
OD1 <- read.csv("il_od_main_JT00_2015.csv")

#Convert csv data into a "data frame", a specific (tabular) data structure that R can manipulate
OD <- as.data.frame(OD1)

##Data Preparation

#Rename total job count column
OD <- plyr::rename(OD, c("S000" = "JTOT"))

#Turn origin and destination variables into factors (e.g., strings)
OD$orig <- factor(OD$h_geocode)
OD$dest <- factor(OD$w_geocode)

#Census Block FIPS Codes
# AABBBCCCCCCDEEE
# A = State (2 digit FIPS code)
# B = County (3 digit FIPS code)
# C = Tract (6 digit FIPS code)
# D = Block Group (1 digit FIPS code)
# E = Block (3 digit FIPS code)

#Select left 5 characters of origin/destination FIPS code, i.e., County FIPS
OD$origCT <- substr(OD$orig, 1, 5)
OD$destCT <- substr(OD$dest, 1, 5)

#Select left 11 characters of origin/destination FIPS code, i.e., Tract FIPS
OD$origTR <- substr(OD$orig, 1, 11)
OD$destTR <- substr(OD$dest, 1, 11)

#Filter by Cook County (FIPS code 17031) and aggregate to tract
ODTR <- OD %>%
  filter(origCT=="17031" & destCT=="17031") %>%
  group_by(origTR, destTR) %>%
  summarize(JTOT = sum(JTOT, na.rm = T))

# ODTR1 <- OD %>%
#   filter(origCT=="17031") %>%
#   group_by(origTR, destTR) %>%
#   summarize(EstTOT = sum(JTOT, na.rm = T))

#Create a unique identifier for origins and destinations
ODTR$TRIP_ID <- paste0(as.character(ODTR$origTR),", ", as.character(ODTR$destTR))
# ODTR1$TRIP_ID <- paste0(as.character(ODTR1$origTR),", ", as.character(ODTR1$destTR))

ODTR <- ODTR %>%
  ungroup() %>%
  select(-(1:2))

# ODTR1 <- ODTR1 %>%
#   ungroup() %>%
#   select(-(1:2))

# #Find off-diagonal values (true commuting)
# ODTR_offdiag <- ODTR[ODTR$origTR!=ODTR$destTR, ]





###Merge with Origin and Destination Coordinates and Find Commuting by Mode

#Merge destination-based coordinates with commuting flows by mode
MergeChi2 <- merge(MergeChi,ACSdest,by="destTR",all.x=TRUE)

MergeChi2 <- merge(MergeChi2,ODTR,by="TRIP_ID",all.x=TRUE)
# MergeChi2 <- merge(MergeChi2,ODTR1,by="TRIP_ID",all.x=TRUE)


# Make links w/ no flows 0
MergeChi2$JTOT[is.na(MergeChi2$JTOT)]<-0
# MergeChi2$EstTOT[is.na(MergeChi2$EstTOT)]<-0

#Estimate expected number of commuters by mode for low and high margins of error
MergeChi2$JTOTOTH <- MergeChi2$JTOT * (MergeChi2$OTH/MergeChi2$WORK16)
MergeChi2$JTOTAUT_L <- MergeChi2$JTOT * (MergeChi2$AUTOL/MergeChi2$WORK16L)
MergeChi2$JTOTAUT <- MergeChi2$JTOT * (MergeChi2$AUTO/MergeChi2$WORK16)
MergeChi2$JTOTAUT_H <- MergeChi2$JTOT * (MergeChi2$AUTOH/MergeChi2$WORK16H)
MergeChi2$JTOTTRN_L <- MergeChi2$JTOT * (MergeChi2$TRANSL/MergeChi2$WORK16L)
MergeChi2$JTOTTRN <- MergeChi2$JTOT * (MergeChi2$TRANSIT/MergeChi2$WORK16)
MergeChi2$JTOTTRN_H <- MergeChi2$JTOT * (MergeChi2$TRANSH/MergeChi2$WORK16H)
MergeChi2$JTOTBIK_L <- MergeChi2$JTOT * (MergeChi2$BIKEL/MergeChi2$WORK16L)
MergeChi2$JTOTBIK <- MergeChi2$JTOT * (MergeChi2$BIKE/MergeChi2$WORK16)
MergeChi2$JTOTBIK_H <- MergeChi2$JTOT * (MergeChi2$BIKEH/MergeChi2$WORK16H)
MergeChi2$JTOTWLK_L <- MergeChi2$JTOT * (MergeChi2$WALKL/MergeChi2$WORK16L)
MergeChi2$JTOTWLK <- MergeChi2$JTOT * (MergeChi2$WALK/MergeChi2$WORK16)
MergeChi2$JTOTWLK_H <- MergeChi2$JTOT * (MergeChi2$WALKH/MergeChi2$WORK16H)
MergeChi2$JTOTWFH_L <- MergeChi2$JTOT * (MergeChi2$WFHL/MergeChi2$WORK16L)
MergeChi2$JTOTWFH <- MergeChi2$JTOT * (MergeChi2$WFH/MergeChi2$WORK16)
MergeChi2$JTOTWFH_H <- MergeChi2$JTOT * (MergeChi2$WFHH/MergeChi2$WORK16H)

MergeChi2$JTOTOTH[is.nan(MergeChi2$JTOTOTH)]<-0
MergeChi2$JTOTAUT_L[is.nan(MergeChi2$JTOTAUT_L)]<-0
MergeChi2$JTOTAUT[is.nan(MergeChi2$JTOTAUT)]<-0
MergeChi2$JTOTAUT_H[is.nan(MergeChi2$JTOTAUT_H)]<-0
MergeChi2$JTOTTRN_L[is.nan(MergeChi2$JTOTTRN_L)]<-0
MergeChi2$JTOTTRN[is.nan(MergeChi2$JTOTTRN)]<-0
MergeChi2$JTOTTRN_H[is.nan(MergeChi2$JTOTTRN_H)]<-0
MergeChi2$JTOTBIK_L[is.nan(MergeChi2$JTOTBIK_L)]<-0
MergeChi2$JTOTBIK[is.nan(MergeChi2$JTOTBIK)]<-0
MergeChi2$JTOTBIK_H[is.nan(MergeChi2$JTOTBIK_H)]<-0
MergeChi2$JTOTWLK_L[is.nan(MergeChi2$JTOTWLK_L)]<-0
MergeChi2$JTOTWLK[is.nan(MergeChi2$JTOTWLK)]<-0
MergeChi2$JTOTWLK_H[is.nan(MergeChi2$JTOTWLK_H)]<-0
MergeChi2$JTOTWFH_L[is.nan(MergeChi2$JTOTWFH_L)]<-0
MergeChi2$JTOTWFH[is.nan(MergeChi2$JTOTWFH)]<-0
MergeChi2$JTOTWFH_H[is.nan(MergeChi2$JTOTWFH_H)]<-0

MergeChi2 <- MergeChi2 %>%
  select(-(14:38))

MergeChi2$DIST <- (MergeChi2$travel_time/60)*5


#Calculate Euclidean distance between tracts
# Convert degrees to radians
deg2rad <- function(deg) return(deg*pi/180)

MergeChi2$long1 <- deg2rad(MergeChi2$h_long)
MergeChi2$lat1 <- deg2rad(MergeChi2$h_lat)
MergeChi2$long2 <- deg2rad(MergeChi2$w_long)
MergeChi2$lat2 <- deg2rad(MergeChi2$w_lat)

# Calculates the geodesic distance between two points specified by radian latitude/longitude using the
# Spherical Law of Cosines (slc)
gcd.slc <- function(long1, lat1, long2, lat2) {
  R <- 6371 # Earth mean radius [km]
  d <- acos(sin(lat1)*sin(lat2) + cos(lat1)*cos(lat2) * cos(long2-long1)) * R
  return(d) # Distance in km
}

#Change R options to not show scientific notation
options(scipen = 999)

#Run the function using newly-calculated coordinates in radians.
MergeChi2$DIST_EUC <- gcd.slc(MergeChi2$long1, MergeChi2$lat1, MergeChi2$long2, MergeChi2$lat2)

MergeChi2$DIST_EUC[is.na(MergeChi2$DIST_EUC)]<-0

# # Routes cannot be found to "GEOID10" = 17031980000 OR "GEOID10" = 17031830005 after 1+ hours; substitute Euclidean distance in these cases
# MergeChi2$DIST[is.na(MergeChi2$DIST)]<-MergeChi2$DIST_EUC

MergeChi2$DIST_EUC <- MergeChi2$DIST_EUC+.05
MergeChi2$DIST <- MergeChi2$DIST+.05

#Filter columns to contain only origin/destination coordinates and flows by mode
MergeChi2 <- MergeChi2 %>%
  select(TRIP_ID, origCT, destCT, origTR, destTR, h_lat, h_long, w_lat, w_long, fromId, toId, JTOT, JTOTOTH, JTOTAUT_L, JTOTAUT, JTOTAUT_H, JTOTTRN_L, JTOTTRN, JTOTTRN_H, JTOTBIK_L, JTOTBIK, JTOTBIK_H, JTOTWLK_L, JTOTWLK, JTOTWLK_H, JTOTWFH_L, JTOTWFH, JTOTWFH_H, DIST, DIST_EUC)

# ff_glimpse(MergeChi2)

# Correlation is 0.993!
cor(MergeChi2$DIST,MergeChi2$DIST_EUC)

#Calculate the weighted number of walking trips based on 2009 NHTS-derived distance decay factor
MergeChi2$W_JWALK <- ifelse(MergeChi2$DIST>3.5,0,
                            ifelse(MergeChi2$DIST<1,MergeChi2$JTOTWLK,(MergeChi2$JTOTWLK*MergeChi2$DIST^-0.714)))
# MergeChi2$W_JWALK <- ifelse(MergeChi2$DIST>3.5,0,MergeChi2$JTOTWLK)

MergeChi2$W_JWALK_L <- ifelse(MergeChi2$DIST>3.5,0,
                            ifelse(MergeChi2$DIST<1,MergeChi2$JTOTWLK_L,(MergeChi2$JTOTWLK_L*MergeChi2$DIST^-0.714)))

MergeChi2$W_JWALK_H <- ifelse(MergeChi2$DIST>3.5,0,
                            ifelse(MergeChi2$DIST<1,MergeChi2$JTOTWLK_H,(MergeChi2$JTOTWLK_H*MergeChi2$DIST^-0.714)))

#Find the remainder (excess) walking trips, i.e., the difference between weighted and raw predicted trips
MergeChi2$W_REM <- MergeChi2$JTOTWLK - MergeChi2$W_JWALK

MergeChi2$W_REM_L <- MergeChi2$JTOTWLK_L - MergeChi2$W_JWALK_L

MergeChi2$W_REM_H <- MergeChi2$JTOTWLK_H - MergeChi2$W_JWALK_H

#Filter the data by links within a given re-absorption threshold (3.5km); count the number of destination tracts within this distance for each origin
Neighbors <- MergeChi2 %>%
  filter(DIST<=3.5) %>% 
  add_count(origTR, name = "N_CLOSE") %>%
  select(TRIP_ID, N_CLOSE)

#Merge these counts back into the full dataset
MergeChi3 <- merge(MergeChi2,Neighbors,by="TRIP_ID",all.x=TRUE)

#Filter the data by links within a given re-absorption threshold (3.5km); find total number of commuters in destination tracts linked to each origin
Neighbors2 <- MergeChi2 %>%
  filter(DIST<=3.5) %>% 
  group_by(origTR) %>%
  summarize(TTOT = sum(JTOT, na.rm=T))

#Merge
MergeChi4 <- merge(MergeChi3,Neighbors2,by="origTR",all.x=TRUE)

#Find the total amount of excess trips for each origin id
Apportion <- MergeChi4 %>%
  group_by(origTR) %>%
  summarize(T_WREM = sum(W_REM, na.rm = T), T_WREM_L = sum(W_REM_L, na.rm = T), T_WREM_H = sum(W_REM_H, na.rm = T))

#Merge back to full dataset
MergeChi5 <- merge(MergeChi4,Apportion,by="origTR",all.x=TRUE)

#Divide T_WREM among all nearby tracts evenly
MergeChi5$EVEN <- MergeChi5$T_WREM/MergeChi5$N_CLOSE
# MergeChi5$N_WALK <- ifelse(is.na(MergeChi5$EVEN), MergeChi5$W_JWALK, (MergeChi5$W_JWALK + MergeChi5$EVEN))
MergeChi5$EVEN_L <- MergeChi5$T_WREM_L/MergeChi5$N_CLOSE
MergeChi5$EVEN_H <- MergeChi5$T_WREM_H/MergeChi5$N_CLOSE

#Divide T_WREM among all nearby tracts based on ratio of individual link commuters to total number of commuters from that origin to nearby destinations
MergeChi5$ADD_WALK <- ifelse(!is.na(MergeChi5$EVEN), MergeChi5$T_WREM*(MergeChi5$JTOT/MergeChi5$TTOT), 0)
MergeChi5$N_WALK2 <- MergeChi5$ADD_WALK + MergeChi5$W_JWALK

MergeChi5$ADD_WALK_L <- ifelse(!is.na(MergeChi5$EVEN_L), MergeChi5$T_WREM_L*(MergeChi5$JTOT/MergeChi5$TTOT), 0)
MergeChi5$N_WALK2_L <- MergeChi5$ADD_WALK_L + MergeChi5$W_JWALK_L

MergeChi5$ADD_WALK_H <- ifelse(!is.na(MergeChi5$EVEN_H), MergeChi5$T_WREM_H*(MergeChi5$JTOT/MergeChi5$TTOT), 0)
MergeChi5$N_WALK2_H <- MergeChi5$ADD_WALK_H + MergeChi5$W_JWALK_H

# hist(MergeChi5$N_WALK, breaks =seq(0,2000, by=20), xlab="Mode Share %", main="Estimated Walk Percent", col="lightgreen", ylim=c(0, 1200))
# hist(MergeChi5$N_WALK2, breaks =seq(0,2000, by=20), xlab="Mode Share %", main="Estimated Walk2 Percent", col="lightgreen", ylim=c(0, 1200))

##Bike Re-Apportionment based on 2009 NHTS-derived distance decay factor
MergeChi5$W_JBIKE <- ifelse(MergeChi5$DIST>6.8,0,
                            ifelse(MergeChi5$DIST<1,MergeChi5$JTOTBIK,(MergeChi5$JTOTBIK*MergeChi5$DIST^-0.329)))
# MergeChi5$W_JBIKE <- ifelse(MergeChi5$DIST>6.8,0,MergeChi5$JTOTBIK)

MergeChi5$W_JBIKE_L <- ifelse(MergeChi5$DIST>6.8,0,
                            ifelse(MergeChi5$DIST<1,MergeChi5$JTOTBIK_L,(MergeChi5$JTOTBIK_L*MergeChi5$DIST^-0.329)))

MergeChi5$W_JBIKE_H <- ifelse(MergeChi5$DIST>6.8,0,
                            ifelse(MergeChi5$DIST<1,MergeChi5$JTOTBIK_H,(MergeChi5$JTOTBIK_H*MergeChi5$DIST^-0.329)))

MergeChi5$B_REM <- MergeChi5$JTOTBIK - MergeChi5$W_JBIKE

MergeChi5$B_REM_L <- MergeChi5$JTOTBIK_L - MergeChi5$W_JBIKE_L

MergeChi5$B_REM_H <- MergeChi5$JTOTBIK_H - MergeChi5$W_JBIKE_H

NeighborsB <- MergeChi5 %>%
  filter(DIST<=6.8) %>% 
  add_count(origTR, name = "B_CLOSE") %>%
  select(TRIP_ID, B_CLOSE)

MergeChi6 <- merge(MergeChi5,NeighborsB,by="TRIP_ID",all.x=TRUE)

NeighborsB2 <- MergeChi5 %>%
  filter(DIST<=6.8) %>% 
  group_by(origTR) %>%
  summarize(TTOTB = sum(JTOT, na.rm=T))

MergeChi7 <- merge(MergeChi6,NeighborsB2,by="origTR",all.x=TRUE)

ApportionB <- MergeChi7 %>%
  group_by(origTR) %>%
  summarize(T_BREM = sum(B_REM, na.rm = T), T_BREM_L = sum(B_REM_L, na.rm = T), T_BREM_H = sum(B_REM_H, na.rm = T))

MergeChi8 <- merge(MergeChi7,ApportionB,by="origTR",all.x=TRUE)

MergeChi8$EVENB <- MergeChi8$T_BREM/MergeChi8$B_CLOSE
# MergeChi8$N_BIKE <- ifelse(is.na(MergeChi8$EVENB), MergeChi8$W_JBIKE, (MergeChi8$W_JBIKE + MergeChi8$EVENB))
MergeChi8$EVENB_L <- MergeChi8$T_BREM_L/MergeChi8$B_CLOSE
MergeChi8$EVENB_H <- MergeChi8$T_BREM_H/MergeChi8$B_CLOSE

MergeChi8$ADD_BIKE <- ifelse(!is.na(MergeChi8$EVENB), MergeChi8$T_BREM*(MergeChi8$JTOT/MergeChi8$TTOTB), 0)
MergeChi8$ADD_BIKE_L <- ifelse(!is.na(MergeChi8$EVENB_L), MergeChi8$T_BREM_L*(MergeChi8$JTOT/MergeChi8$TTOTB), 0)
MergeChi8$ADD_BIKE_H <- ifelse(!is.na(MergeChi8$EVENB_H), MergeChi8$T_BREM_H*(MergeChi8$JTOT/MergeChi8$TTOTB), 0)

MergeChi8$N_BIKE2 <- MergeChi8$ADD_BIKE + MergeChi8$W_JBIKE
MergeChi8$N_BIKE2_L <- MergeChi8$ADD_BIKE_L + MergeChi8$W_JBIKE_L
MergeChi8$N_BIKE2_H <- MergeChi8$ADD_BIKE_H + MergeChi8$W_JBIKE_H

##Transit Re-Apportionment using r5r

# # Build a routable transport network with setup_R5
# 
# # Indicate the path where OSM and GTFS data are stored
# r5r_core <- setup_r5(data_path = data_path, verbose = FALSE)
# 
# mode <- c("TRANSIT")
# max_walk_dist <- 1000
# max_trip_duration <- 180
# departure_datetime <- as.POSIXct("11-11-2021 09:00:00",
#                                  format = "%d-%m-%Y %H:%M:%S")
# 
# # Calculate a travel time matrix
# ttmt <- travel_time_matrix(r5r_core = r5r_core,
#                            origins = poi2,
#                            destinations = poi2,
#                            mode = mode,
#                            departure_datetime = departure_datetime,
#                            time_window = 20,
#                            breakdown = TRUE,
#                            breakdown_stat = 'mean',
#                            max_walk_dist = max_walk_dist,
#                            max_trip_duration = max_trip_duration,
#                            walk_speed = 5,
#                            bike_speed = 18,
#                            max_rides = 15,
#                            verbose = FALSE)
# 
# 
# stop_r5(r5r_core)
# rJava::.jgc(R.gc = TRUE)
# 
# write.csv(ttmt, file = "ttmt.csv")
# 
#Or save time and read in pre-calculated distances
ttmt <- read.csv("ttmt.csv")

ttmt_m1 <- merge(ttmt,poi2,by="fromId", all.x=TRUE)
ttmt_m1 <- ttmt_m1 %>%
  select(fromId, toId.x, GEOID, total_time, n_rides)
ttmt_m1 <- plyr::rename(ttmt_m1, c("GEOID" = "origTR", "toId.x" = "toId"))

ttmt_m2 <- merge(ttmt_m1,poi2,by="toId", all.x=TRUE)
ttmt_m2 <- ttmt_m2 %>%
  select(fromId.x, toId,origTR, GEOID, total_time, n_rides)
ttmt_m2 <- plyr::rename(ttmt_m2, c("GEOID" = "destTR", "fromId.x" = "fromId"))

# ttmto_m1 <- merge(poi2,ttmto,by="fromId", all.x=TRUE)
# ttmto_m1 <- ttmto_m1 %>%
#   select(fromId, toId.y, GEOID, travel_time, n_rides)
# ttmto_m1 <- plyr::rename(ttmto_m1, c("GEOID" = "origTR", "toId.y" = "toId"))
# 
# ttmto_m2 <- merge(poi2,ttmto_m1,by="toId", all.x=TRUE)
# ttmto_m2 <- ttmto_m2 %>%
#   select(fromId.y, toId,origTR, GEOID, travel_time, n_rides)
# ttmto_m2 <- plyr::rename(ttmto_m2, c("GEOID" = "destTR", "fromId.y" = "fromId"))

#Create a unique identifier for origins and destinations
ttmt_m2$TRIP_ID <- paste0(as.character(ttmt_m2$origTR),", ", as.character(ttmt_m2$destTR))
# ttmto_m2$TRIP_ID <- paste0(as.character(ttmto_m2$origTR),", ", as.character(ttmto_m2$destTR))

ttmt_m2$origCT <- substr(ttmt_m2$origTR, 1, 5)
ttmt_m2$destCT <- substr(ttmt_m2$destTR, 1, 5)

# ttmto_m2$origCT <- substr(ttmto_m2$origTR, 1, 5)
# ttmto_m2$destCT <- substr(ttmto_m2$destTR, 1, 5)

# ttmt_m2$n_rides <- ifelse((ttmt_m2$origTR==ttmt_m2$destTR),1,ttmt_m2$n_rides)

#n_rides all eithe intra-tract or to very close tracts (where you wouldn't take transit)
ttmt_m2 %>%
  count(destCT,n_rides)

# ttmto_m2 %>%
#   count(destCT,n_rides)

ttmt_m2 <- ttmt_m2 %>%
  select(TRIP_ID,n_rides)

# ttmto_m2 <- ttmto_m2 %>%
#   select(TRIP_ID,n_rides)
# ttmto_m2 <- plyr::rename(ttmto_m2, c("n_rides" = "n_rideso"))

MergeChi9 <- merge(MergeChi8,ttmt_m2,by="TRIP_ID", all.x=TRUE)
# MergeChi9 <- merge(MergeChi9,ttmto_m2,by="TRIP_ID", all.x=TRUE)

# ff_glimpse(MergeChi9)

MergeChi9$n_rides[is.na(MergeChi9$n_rides)] <- 0
# MergeChi9$n_rideso[is.na(MergeChi9$n_rideso)] <- 0

# MergeChi9$n_rides <- ifelse((MergeChi9$origTR==MergeChi9$destTR),1,MergeChi9$n_rides)


# MergeChi9$tdif <- MergeChi9$n_rideso - MergeChi9$n_rides
 
# sum(MergeChi9$tdif)

MergeChi9$W_JTRAN <- ifelse((MergeChi9$n_rides>3 | MergeChi9$n_rides<1),0,MergeChi9$JTOTTRN)
MergeChi9$W_JTRAN_L <- ifelse((MergeChi9$n_rides>3 | MergeChi9$n_rides<1),0,MergeChi9$JTOTTRN_L)
MergeChi9$W_JTRAN_H <- ifelse((MergeChi9$n_rides>3 | MergeChi9$n_rides<1),0,MergeChi9$JTOTTRN_H)

# MergeChi9$W_JTRAN <- ifelse(MergeChi9$n_rides==0,0,MergeChi9$JTOTTRN)

MergeChi9$T_REM <- MergeChi9$JTOTTRN - MergeChi9$W_JTRAN
MergeChi9$T_REM_L <- MergeChi9$JTOTTRN_L - MergeChi9$W_JTRAN_L
MergeChi9$T_REM_H <- MergeChi9$JTOTTRN_H - MergeChi9$W_JTRAN_H

#Allow tracts to be neighbors of themselves (as is the case throughout)
NeighborsT <- MergeChi9 %>%
  filter(n_rides<=3) %>% 
  add_count(origTR, name = "T_CLOSE") %>%
  select(TRIP_ID, T_CLOSE)

MergeChi10 <- merge(MergeChi9,NeighborsT,by="TRIP_ID",all.x=TRUE)

NeighborsT2 <- MergeChi9 %>%
  filter(n_rides<=3) %>% 
  group_by(origTR) %>%
  summarize(TTOTT = sum(JTOT, na.rm=F))

MergeChi11 <- merge(MergeChi10,NeighborsT2,by="origTR",all.x=TRUE)

ApportionT <- MergeChi11 %>%
  group_by(origTR) %>%
  summarize(T_TREM = sum(T_REM, na.rm = T), T_TREM_L = sum(T_REM_L, na.rm = T), T_TREM_H = sum(T_REM_H, na.rm = T))

MergeChi12 <- merge(MergeChi11,ApportionT,by="origTR",all.x=TRUE)

MergeChi12$EVENT <- MergeChi12$T_TREM/MergeChi12$T_CLOSE
# MergeChi12$N_TRAN <- ifelse(is.na(MergeChi12$EVENT), MergeChi12$W_JTRAN, (MergeChi12$W_JTRAN + MergeChi12$EVENT))
MergeChi12$EVENT_L <- MergeChi12$T_TREM_L/MergeChi12$T_CLOSE
MergeChi12$EVENT_H <- MergeChi12$T_TREM_H/MergeChi12$T_CLOSE

MergeChi12$ADD_TRAN <- ifelse(!is.na(MergeChi12$EVENT), MergeChi12$T_TREM*(MergeChi12$JTOT/MergeChi12$TTOTT), 0)
MergeChi12$ADD_TRAN_L <- ifelse(!is.na(MergeChi12$EVENT_L), MergeChi12$T_TREM_L*(MergeChi12$JTOT/MergeChi12$TTOTT), 0)
MergeChi12$ADD_TRAN_H <- ifelse(!is.na(MergeChi12$EVENT_H), MergeChi12$T_TREM_H*(MergeChi12$JTOT/MergeChi12$TTOTT), 0)

MergeChi12$N_TRAN2 <- MergeChi12$ADD_TRAN + MergeChi12$W_JTRAN
MergeChi12$N_TRAN2_L <- MergeChi12$ADD_TRAN_L + MergeChi12$W_JTRAN_L
MergeChi12$N_TRAN2_H <- MergeChi12$ADD_TRAN_H + MergeChi12$W_JTRAN_H

MergeChi12$N_TRAN2[is.na(MergeChi12$N_TRAN2)] <- 0
MergeChi12$N_TRAN2_L[is.na(MergeChi12$N_TRAN2_L)] <- 0
MergeChi12$N_TRAN2_H[is.na(MergeChi12$N_TRAN2_H)] <- 0

# Check!
sum(MergeChi12$JTOTTRN) - sum(MergeChi12$N_TRAN2) 


MergeChi12 <- MergeChi12[MergeChi12$JTOT>0]







####VALIDATION

#Calculate "Original" totals by mode
MergeChi12$O_TOT <- MergeChi12$JTOTWLK + MergeChi12$JTOTAUT + MergeChi12$JTOTTRN + MergeChi12$JTOTBIK + MergeChi12$JTOTWFH + MergeChi12$JTOTOTH
MergeChi12$O_TOT_L <- MergeChi12$JTOTWLK_L + MergeChi12$JTOTAUT_L + MergeChi12$JTOTTRN_L + MergeChi12$JTOTBIK_L + MergeChi12$JTOTWFH_L
MergeChi12$O_TOT_H <- MergeChi12$JTOTWLK_H + MergeChi12$JTOTAUT_H + MergeChi12$JTOTTRN_H + MergeChi12$JTOTBIK_H + MergeChi12$JTOTWFH_H

#Calculate "New" totals by mode
MergeChi12$N_TOT <- MergeChi12$N_WALK2 + MergeChi12$JTOTAUT + MergeChi12$N_TRAN2 + MergeChi12$N_BIKE2 + MergeChi12$JTOTWFH + MergeChi12$JTOTOTH
MergeChi12$N_TOT_L <- MergeChi12$N_WALK2_L + MergeChi12$JTOTAUT_L + MergeChi12$N_TRAN2_L + MergeChi12$N_BIKE2_L + MergeChi12$JTOTWFH_L
MergeChi12$N_TOT_H <- MergeChi12$N_WALK2_H + MergeChi12$JTOTAUT_H + MergeChi12$N_TRAN2_H + MergeChi12$N_BIKE2_H + MergeChi12$JTOTWFH_H

#Check the aggregate values for N_WALK2, NNWALK2, N_TOT, O_TOT, etc.
Test <- MergeChi12 %>%
  summarize(sum(JTOTWLK, na.rm=T), sum(N_WALK2, na.rm=T),
            sum(JTOTBIK, na.rm=T), sum(N_BIKE2, na.rm=T), 
            sum(JTOTAUT, na.rm=T),
            sum(JTOTTRN, na.rm=T), sum(N_TRAN2, na.rm=T),
            sum(JTOTWFH, na.rm=T),
            sum(JTOTOTH, na.rm=T),
            sum(N_TOT, na.rm=T), sum(O_TOT, na.rm=T), sum(JTOT, na.rm=T))

Test_L <- MergeChi12 %>%
  summarize(sum(JTOTWLK_L, na.rm=T), sum(N_WALK2_L, na.rm=T),
            sum(JTOTBIK_L, na.rm=T), sum(N_BIKE2_L, na.rm=T), 
            sum(JTOTAUT_L, na.rm=T), 
            sum(JTOTTRN_L, na.rm=T), sum(N_TRAN2_L, na.rm=T), 
            sum(JTOTWFH_L, na.rm=T),
            sum(N_TOT_L, na.rm=T), sum(O_TOT_L, na.rm=T), sum(JTOT, na.rm=T))

Test_H <- MergeChi12 %>%
  summarize(sum(JTOTWLK_H, na.rm=T), sum(N_WALK2_H, na.rm=T),
            sum(JTOTBIK_H, na.rm=T), sum(N_BIKE2_H, na.rm=T), 
            sum(JTOTAUT_H, na.rm=T), 
            sum(JTOTTRN_H, na.rm=T), sum(N_TRAN2_H, na.rm=T), 
            sum(JTOTWFH_H, na.rm=T),
            sum(N_TOT_H, na.rm=T), sum(O_TOT_H, na.rm=T), sum(JTOT, na.rm=T))


#Quick check on totals
MergeChi12$C <- ifelse((MergeChi12$JTOTOTH + MergeChi12$JTOTAUT + MergeChi12$JTOTTRN + MergeChi12$JTOTBIK + MergeChi12$JTOTWLK + MergeChi12$JTOTWFH - MergeChi12$JTOT)<=-1,1,0)

MergeChicheck <- subset(MergeChi12,MergeChi12$C==1) #0 workers in ACS

#Compare original ACS mode share by origin to new estimates
Mode <- MergeChi12 %>%
  group_by(origTR) %>%
  summarize(JTOT = sum(JTOT, na.rm=T), TOTAUTO = sum(JTOTAUT, na.rm=T), TOTTRAN = sum(N_TRAN2, na.rm=T), TOTBIKE = sum(N_BIKE2, na.rm=T), TOTWALK = sum(N_WALK2, na.rm=T), TOTWFH = sum(JTOTWFH, na.rm=T))

Mode$AUTOP1 <- Mode$TOTAUTO/Mode$JTOT
Mode$TRANS1 <- Mode$TOTTRAN/Mode$JTOT
Mode$BIKE1 <- Mode$TOTBIKE/Mode$JTOT
Mode$WALK1 <- Mode$TOTWALK/Mode$JTOT
Mode$WFH1 <- Mode$TOTWFH/Mode$JTOT

MergeMode <- merge(Mode,ACS,by="origTR", all.x=TRUE)

MergeMode$AUTOP <- MergeMode$AUTO/MergeMode$WORK16
MergeMode$TRANSITP <- MergeMode$TRANSIT/MergeMode$WORK16
MergeMode$BIKEP <- MergeMode$BIKE/MergeMode$WORK16
MergeMode$WALKP <- MergeMode$WALK/MergeMode$WORK16
MergeMode$WFHP <- MergeMode$WFH/MergeMode$WORK16

######################Test whether or not we correctly matched our estimates of commuting by mode at the origin to the original ACS data
hist(MergeMode$AUTOP1, breaks =seq(0,1, by=.02), xlab="Mode Share %", main="A) Estimated Auto Percent", col="lightblue", ylim=c(0, 100))
hist(MergeMode$AUTOP, breaks =seq(0,1, by=.02), xlab="Mode Share %", main="B) ACS Auto Percent", col="lightblue", ylim=c(0, 100))
hist(MergeMode$TRANS1, breaks =seq(0,1, by=.02), xlab="Mode Share %", main="C) Estimated Transit Percent", col="lightblue", ylim=c(0, 160))
hist(MergeMode$TRANSITP, breaks =seq(0,1, by=.02), xlab="Mode Share %", main="D) ACS Transit Percent", col="lightblue", ylim=c(0, 160))
hist(MergeMode$BIKE1, breaks =seq(0,1, by=.02), xlab="Mode Share %", main="E) Estimated Cycling Percent", col="lightblue", ylim=c(0, 1200))
hist(MergeMode$BIKEP, breaks =seq(0,1, by=.02), xlab="Mode Share %", main="F) ACS Cycling Percent", col="lightblue", ylim=c(0, 1200))
hist(MergeMode$WALK1, breaks =seq(0,1, by=.02), xlab="Mode Share %", main="G) Estimated Walking Percent", col="lightblue", ylim=c(0, 1200))
hist(MergeMode$WALKP, breaks =seq(0,1, by=.02), xlab="Mode Share %", main="H) ACS Walking Percent", col="lightblue", ylim=c(0, 1200))
hist(MergeMode$WFH1, breaks =seq(0,1, by=.02), xlab="Mode Share %", main="I) Estimated Work From Home Percent", col="lightblue", ylim=c(0, 1200))
hist(MergeMode$WFHP, breaks =seq(0,1, by=.02), xlab="Mode Share %", main="J) ACS Work From Home Percent", col="lightblue", ylim=c(0, 1200))
######################


MergeMode$AUTOP - MergeMode$AUTOP1 #very slight discrepancies due to 
#difference in rounding between Excel and R in % calculation - histograms in paper use pre-calculated ACS %s. which match exactly


MergeChi12$N_WALK2_R <- (MergeChi12$N_WALK2_H - MergeChi12$N_WALK2_L)/(MergeChi12$N_WALK2+1)
MergeChi12$N_BIKE2_R <- (MergeChi12$N_BIKE2_H - MergeChi12$N_BIKE2_L)/(MergeChi12$N_BIKE2+1)
MergeChi12$N_TRAN2_R <- (MergeChi12$N_TRAN2_H - MergeChi12$N_TRAN2_L)/(MergeChi12$N_TRAN2+1)
MergeChi12$JTOTAUT_R <- (MergeChi12$JTOTAUT_H - MergeChi12$JTOTAUT_L)/(MergeChi12$JTOTAUT+1)
MergeChi12$JTOTWFH_R <- (MergeChi12$JTOTWFH_H - MergeChi12$JTOTWFH_L)/(MergeChi12$JTOTWFH+1)

MergeChi2$N_WALK2_R[is.nan(MergeChi2$N_WALK2_R)]<-0
MergeChi2$N_BIKE2_R[is.nan(MergeChi2$N_BIKE2_R)]<-0
MergeChi2$N_TRAN2_R[is.nan(MergeChi2$N_TRAN2_R)]<-0
MergeChi2$N_JTOTAUT_R[is.nan(MergeChi2$N_JTOTAUT_R)]<-0
MergeChi2$N_JTOTWFH_R[is.nan(MergeChi2$N_JTOTWFH_R)]<-0

Mode_Flows <- MergeChi12 %>%
  select(TRIP_ID, origTR, destTR, h_lat, h_long, w_lat, w_long, JTOT, JTOTOTH, JTOTAUT, JTOTTRN, JTOTBIK, JTOTWLK, JTOTWFH,
         N_TRAN2, N_BIKE2, N_WALK2, JTOTAUT_R, N_TRAN2_R, N_BIKE2_R, N_WALK2_R, JTOTWFH_R,
         ADD_TRAN, ADD_BIKE, ADD_WALK, DIST, DIST_EUC)

#Export data as a CSV
write.csv(Mode_Flows, file = "Mode_Flows.csv")


##Find number of total trips by distance band
#0 - 1km
DIST1 <- Mode_Flows  %>%
  filter(DIST<=1) %>%
  group_by(DIST) %>%
  summarize(JTOT = sum(JTOT, na.rm = T))
DIST1 <- DIST1 %>%
  summarize(JTOT = sum(JTOT, na.rm = T))
#1 - 2 km
DIST2 <- Mode_Flows  %>%
  filter(DIST>1 & DIST<=2) %>%
  group_by(DIST) %>%
  summarize(JTOT = sum(JTOT, na.rm = T))
DIST2 <- DIST2 %>%
  summarize(JTOT = sum(JTOT, na.rm = T))
#2 - 5km
DIST5 <- Mode_Flows  %>%
  filter(DIST>2 & DIST<=5) %>%
  group_by(DIST) %>%
  summarize(JTOT = sum(JTOT, na.rm = T))
DIST5 <- DIST5 %>%
  summarize(JTOT = sum(JTOT, na.rm = T))
#5 - 10km
DIST10 <- Mode_Flows  %>%
  filter(DIST>5 & DIST<=10) %>%
  group_by(DIST) %>%
  summarize(JTOT = sum(JTOT, na.rm = T))
DIST10 <- DIST10 %>%
  summarize(JTOT = sum(JTOT, na.rm = T))
#10 - 20km
DIST20 <- Mode_Flows  %>%
  filter(DIST>10 & DIST<=20) %>%
  group_by(DIST) %>%
  summarize(JTOT = sum(JTOT, na.rm = T))
DIST20 <- DIST20 %>%
  summarize(JTOT = sum(JTOT, na.rm = T))
#20 - 30km
DIST30 <- Mode_Flows  %>%
  filter(DIST>20 & DIST<=30) %>%
  group_by(DIST) %>%
  summarize(JTOT = sum(JTOT, na.rm = T))
DIST30 <- DIST30 %>%
  summarize(JTOT = sum(JTOT, na.rm = T))
#30 + km
DISTP <- Mode_Flows  %>%
  filter(DIST>30) %>%
  group_by(DIST) %>%
  summarize(JTOT = sum(JTOT, na.rm = T))
DISTP <- DISTP %>%
  summarize(JTOT = sum(JTOT, na.rm = T))

##Find number of walk trips by distance band
#0 - 1km
DIST1N_WALK2 <- Mode_Flows  %>%
  filter(DIST<=1) %>%
  group_by(DIST) %>%
  summarize(N_WALK2 = sum(N_WALK2, na.rm = T))
DIST1N_WALK2 <- DIST1N_WALK2 %>%
  summarize(N_WALK2 = sum(N_WALK2, na.rm = T))
#1 - 2 km
DIST2N_WALK2 <- Mode_Flows  %>%
  filter(DIST>1 & DIST<=2) %>%
  group_by(DIST) %>%
  summarize(N_WALK2 = sum(N_WALK2, na.rm = T))
DIST2N_WALK2 <- DIST2N_WALK2 %>%
  summarize(N_WALK2 = sum(N_WALK2, na.rm = T))
#2 - 5km
DIST5N_WALK2 <- Mode_Flows  %>%
  filter(DIST>2 & DIST<=5) %>%
  group_by(DIST) %>%
  summarize(N_WALK2 = sum(N_WALK2, na.rm = T))
DIST5N_WALK2 <- DIST5N_WALK2 %>%
  summarize(N_WALK2 = sum(N_WALK2, na.rm = T))
#5 - 10km
DIST10N_WALK2 <- Mode_Flows  %>%
  filter(DIST>5 & DIST<=10) %>%
  group_by(DIST) %>%
  summarize(N_WALK2 = sum(N_WALK2, na.rm = T))
DIST10N_WALK2 <- DIST10N_WALK2 %>%
  summarize(N_WALK2 = sum(N_WALK2, na.rm = T))
#10 - 20km
DIST20N_WALK2 <- Mode_Flows  %>%
  filter(DIST>10 & DIST<=20) %>%
  group_by(DIST) %>%
  summarize(N_WALK2 = sum(N_WALK2, na.rm = T))
DIST20N_WALK2 <- DIST20N_WALK2 %>%
  summarize(N_WALK2 = sum(N_WALK2, na.rm = T))
#20 - 30km
DIST30N_WALK2 <- Mode_Flows  %>%
  filter(DIST>20 & DIST<=30) %>%
  group_by(DIST) %>%
  summarize(N_WALK2 = sum(N_WALK2, na.rm = T))
DIST30N_WALK2 <- DIST30N_WALK2 %>%
  summarize(N_WALK2 = sum(N_WALK2, na.rm = T))
#30 + km
DISTPN_WALK2 <- Mode_Flows  %>%
  filter(DIST>30) %>%
  group_by(DIST) %>%
  summarize(N_WALK2 = sum(N_WALK2, na.rm = T))
DISTPN_WALK2 <- DISTPN_WALK2 %>%
  summarize(N_WALK2 = sum(N_WALK2, na.rm = T))

##Find number of transit trips by distance band
#0 - 1km
DIST1N_TRAN2 <- Mode_Flows  %>%
  filter(DIST<=1) %>%
  group_by(DIST) %>%
  summarize(N_TRAN2 = sum(N_TRAN2, na.rm = T))
DIST1N_TRAN2 <- DIST1N_TRAN2 %>%
  summarize(N_TRAN2 = sum(N_TRAN2, na.rm = T))
#1 - 2 km
DIST2N_TRAN2 <- Mode_Flows  %>%
  filter(DIST>1 & DIST<=2) %>%
  group_by(DIST) %>%
  summarize(N_TRAN2 = sum(N_TRAN2, na.rm = T))
DIST2N_TRAN2 <- DIST2N_TRAN2 %>%
  summarize(N_TRAN2 = sum(N_TRAN2, na.rm = T))
#2 - 5km
DIST5N_TRAN2 <- Mode_Flows  %>%
  filter(DIST>2 & DIST<=5) %>%
  group_by(DIST) %>%
  summarize(N_TRAN2 = sum(N_TRAN2, na.rm = T))
DIST5N_TRAN2 <- DIST5N_TRAN2 %>%
  summarize(N_TRAN2 = sum(N_TRAN2, na.rm = T))
#5 - 10km
DIST10N_TRAN2 <- Mode_Flows  %>%
  filter(DIST>5 & DIST<=10) %>%
  group_by(DIST) %>%
  summarize(N_TRAN2 = sum(N_TRAN2, na.rm = T))
DIST10N_TRAN2 <- DIST10N_TRAN2 %>%
  summarize(N_TRAN2 = sum(N_TRAN2, na.rm = T))
#10 - 20km
DIST20N_TRAN2 <- Mode_Flows  %>%
  filter(DIST>10 & DIST<=20) %>%
  group_by(DIST) %>%
  summarize(N_TRAN2 = sum(N_TRAN2, na.rm = T))
DIST20N_TRAN2 <- DIST20N_TRAN2 %>%
  summarize(N_TRAN2 = sum(N_TRAN2, na.rm = T))
#20 - 30km
DIST30N_TRAN2 <- Mode_Flows  %>%
  filter(DIST>20 & DIST<=30) %>%
  group_by(DIST) %>%
  summarize(N_TRAN2 = sum(N_TRAN2, na.rm = T))
DIST30N_TRAN2 <- DIST30N_TRAN2 %>%
  summarize(N_TRAN2 = sum(N_TRAN2, na.rm = T))
#30 + km
DISTPN_TRAN2 <- Mode_Flows  %>%
  filter(DIST>30) %>%
  group_by(DIST) %>%
  summarize(N_TRAN2 = sum(N_TRAN2, na.rm = T))
DISTPN_TRAN2 <- DISTPN_TRAN2 %>%
  summarize(N_TRAN2 = sum(N_TRAN2, na.rm = T))

##Find number of auto trips by distance band
#0 - 1km
DIST1JTOTAUT <- Mode_Flows  %>%
  filter(DIST<=1) %>%
  group_by(DIST) %>%
  summarize(JTOTAUT = sum(JTOTAUT, na.rm = T))
DIST1JTOTAUT <- DIST1JTOTAUT %>%
  summarize(JTOTAUT = sum(JTOTAUT, na.rm = T))
#1 - 2 km
DIST2JTOTAUT <- Mode_Flows  %>%
  filter(DIST>1 & DIST<=2) %>%
  group_by(DIST) %>%
  summarize(JTOTAUT = sum(JTOTAUT, na.rm = T))
DIST2JTOTAUT <- DIST2JTOTAUT %>%
  summarize(JTOTAUT = sum(JTOTAUT, na.rm = T))
#2 - 5km
DIST5JTOTAUT <- Mode_Flows  %>%
  filter(DIST>2 & DIST<=5) %>%
  group_by(DIST) %>%
  summarize(JTOTAUT = sum(JTOTAUT, na.rm = T))
DIST5JTOTAUT <- DIST5JTOTAUT %>%
  summarize(JTOTAUT = sum(JTOTAUT, na.rm = T))
#5 - 10km
DIST10JTOTAUT <- Mode_Flows  %>%
  filter(DIST>5 & DIST<=10) %>%
  group_by(DIST) %>%
  summarize(JTOTAUT = sum(JTOTAUT, na.rm = T))
DIST10JTOTAUT <- DIST10JTOTAUT %>%
  summarize(JTOTAUT = sum(JTOTAUT, na.rm = T))
#10 - 20km
DIST20JTOTAUT <- Mode_Flows  %>%
  filter(DIST>10 & DIST<=20) %>%
  group_by(DIST) %>%
  summarize(JTOTAUT = sum(JTOTAUT, na.rm = T))
DIST20JTOTAUT <- DIST20JTOTAUT %>%
  summarize(JTOTAUT = sum(JTOTAUT, na.rm = T))
#20 - 30km
DIST30JTOTAUT <- Mode_Flows  %>%
  filter(DIST>20 & DIST<=30) %>%
  group_by(DIST) %>%
  summarize(JTOTAUT = sum(JTOTAUT, na.rm = T))
DIST30JTOTAUT <- DIST30JTOTAUT %>%
  summarize(JTOTAUT = sum(JTOTAUT, na.rm = T))
#30 + km
DISTPJTOTAUT <- Mode_Flows  %>%
  filter(DIST>30) %>%
  group_by(DIST) %>%
  summarize(JTOTAUT = sum(JTOTAUT, na.rm = T))
DISTPJTOTAUT <- DISTPJTOTAUT %>%
  summarize(JTOTAUT = sum(JTOTAUT, na.rm = T))

##Find number of bike trips by distance band
#0 - 1km
DIST1N_BIKE2 <- Mode_Flows  %>%
  filter(DIST<=1) %>%
  group_by(DIST) %>%
  summarize(N_BIKE2 = sum(N_BIKE2, na.rm = T))
DIST1N_BIKE2 <- DIST1N_BIKE2 %>%
  summarize(N_BIKE2 = sum(N_BIKE2, na.rm = T))
#1 - 2 km
DIST2N_BIKE2 <- Mode_Flows  %>%
  filter(DIST>1 & DIST<=2) %>%
  group_by(DIST) %>%
  summarize(N_BIKE2 = sum(N_BIKE2, na.rm = T))
DIST2N_BIKE2 <- DIST2N_BIKE2 %>%
  summarize(N_BIKE2 = sum(N_BIKE2, na.rm = T))
#2 - 5km
DIST5N_BIKE2 <- Mode_Flows  %>%
  filter(DIST>2 & DIST<=5) %>%
  group_by(DIST) %>%
  summarize(N_BIKE2 = sum(N_BIKE2, na.rm = T))
DIST5N_BIKE2 <- DIST5N_BIKE2 %>%
  summarize(N_BIKE2 = sum(N_BIKE2, na.rm = T))
#5 - 10km
DIST10N_BIKE2 <- Mode_Flows  %>%
  filter(DIST>5 & DIST<=10) %>%
  group_by(DIST) %>%
  summarize(N_BIKE2 = sum(N_BIKE2, na.rm = T))
DIST10N_BIKE2 <- DIST10N_BIKE2 %>%
  summarize(N_BIKE2 = sum(N_BIKE2, na.rm = T))
#10 - 20km
DIST20N_BIKE2 <- Mode_Flows  %>%
  filter(DIST>10 & DIST<=20) %>%
  group_by(DIST) %>%
  summarize(N_BIKE2 = sum(N_BIKE2, na.rm = T))
DIST20N_BIKE2 <- DIST20N_BIKE2 %>%
  summarize(N_BIKE2 = sum(N_BIKE2, na.rm = T))
#20 - 30km
DIST30N_BIKE2 <- Mode_Flows  %>%
  filter(DIST>20 & DIST<=30) %>%
  group_by(DIST) %>%
  summarize(N_BIKE2 = sum(N_BIKE2, na.rm = T))
DIST30N_BIKE2 <- DIST30N_BIKE2 %>%
  summarize(N_BIKE2 = sum(N_BIKE2, na.rm = T))
#30 + km
DISTPN_BIKE2 <- Mode_Flows  %>%
  filter(DIST>30) %>%
  group_by(DIST) %>%
  summarize(N_BIKE2 = sum(N_BIKE2, na.rm = T))
DISTPN_BIKE2 <- DISTPN_BIKE2 %>%
  summarize(N_BIKE2 = sum(N_BIKE2, na.rm = T))


##Find number of WFH trips by distance band
#0 - 1km
DIST1JTOTWFH <- Mode_Flows  %>%
  filter(DIST<=1) %>%
  group_by(DIST) %>%
  summarize(JTOTWFH = sum(JTOTWFH, na.rm = T))
DIST1JTOTWFH <- DIST1JTOTWFH %>%
  summarize(JTOTWFH = sum(JTOTWFH, na.rm = T))
#1 - 2 km
DIST2JTOTWFH <- Mode_Flows  %>%
  filter(DIST>1 & DIST<=2) %>%
  group_by(DIST) %>%
  summarize(JTOTWFH = sum(JTOTWFH, na.rm = T))
DIST2JTOTWFH <- DIST2JTOTWFH %>%
  summarize(JTOTWFH = sum(JTOTWFH, na.rm = T))
#2 - 5km
DIST5JTOTWFH <- Mode_Flows  %>%
  filter(DIST>2 & DIST<=5) %>%
  group_by(DIST) %>%
  summarize(JTOTWFH = sum(JTOTWFH, na.rm = T))
DIST5JTOTWFH <- DIST5JTOTWFH %>%
  summarize(JTOTWFH = sum(JTOTWFH, na.rm = T))
#5 - 10km
DIST10JTOTWFH <- Mode_Flows  %>%
  filter(DIST>5 & DIST<=10) %>%
  group_by(DIST) %>%
  summarize(JTOTWFH = sum(JTOTWFH, na.rm = T))
DIST10JTOTWFH <- DIST10JTOTWFH %>%
  summarize(JTOTWFH = sum(JTOTWFH, na.rm = T))
#10 - 20km
DIST20JTOTWFH <- Mode_Flows  %>%
  filter(DIST>10 & DIST<=20) %>%
  group_by(DIST) %>%
  summarize(JTOTWFH = sum(JTOTWFH, na.rm = T))
DIST20JTOTWFH <- DIST20JTOTWFH %>%
  summarize(JTOTWFH = sum(JTOTWFH, na.rm = T))
#20 - 30km
DIST30JTOTWFH <- Mode_Flows  %>%
  filter(DIST>20 & DIST<=30) %>%
  group_by(DIST) %>%
  summarize(JTOTWFH = sum(JTOTWFH, na.rm = T))
DIST30JTOTWFH <- DIST30JTOTWFH %>%
  summarize(JTOTWFH = sum(JTOTWFH, na.rm = T))
#30 + km
DISTPJTOTWFH <- Mode_Flows  %>%
  filter(DIST>30) %>%
  group_by(DIST) %>%
  summarize(JTOTWFH = sum(JTOTWFH, na.rm = T))
DISTPJTOTWFH <- DISTPJTOTWFH %>%
  summarize(JTOTWFH = sum(JTOTWFH, na.rm = T))


##Find number of OTH trips by distance band
#0 - 1km
DIST1JTOTOTH <- Mode_Flows  %>%
  filter(DIST<=1) %>%
  group_by(DIST) %>%
  summarize(JTOTOTH = sum(JTOTOTH, na.rm = T))
DIST1JTOTOTH <- DIST1JTOTOTH %>%
  summarize(JTOTOTH = sum(JTOTOTH, na.rm = T))
#1 - 2 km
DIST2JTOTOTH <- Mode_Flows  %>%
  filter(DIST>1 & DIST<=2) %>%
  group_by(DIST) %>%
  summarize(JTOTOTH = sum(JTOTOTH, na.rm = T))
DIST2JTOTOTH <- DIST2JTOTOTH %>%
  summarize(JTOTOTH = sum(JTOTOTH, na.rm = T))
#2 - 5km
DIST5JTOTOTH <- Mode_Flows  %>%
  filter(DIST>2 & DIST<=5) %>%
  group_by(DIST) %>%
  summarize(JTOTOTH = sum(JTOTOTH, na.rm = T))
DIST5JTOTOTH <- DIST5JTOTOTH %>%
  summarize(JTOTOTH = sum(JTOTOTH, na.rm = T))
#5 - 10km
DIST10JTOTOTH <- Mode_Flows  %>%
  filter(DIST>5 & DIST<=10) %>%
  group_by(DIST) %>%
  summarize(JTOTOTH = sum(JTOTOTH, na.rm = T))
DIST10JTOTOTH <- DIST10JTOTOTH %>%
  summarize(JTOTOTH = sum(JTOTOTH, na.rm = T))
#10 - 20km
DIST20JTOTOTH <- Mode_Flows  %>%
  filter(DIST>10 & DIST<=20) %>%
  group_by(DIST) %>%
  summarize(JTOTOTH = sum(JTOTOTH, na.rm = T))
DIST20JTOTOTH <- DIST20JTOTOTH %>%
  summarize(JTOTOTH = sum(JTOTOTH, na.rm = T))
#20 - 30km
DIST30JTOTOTH <- Mode_Flows  %>%
  filter(DIST>20 & DIST<=30) %>%
  group_by(DIST) %>%
  summarize(JTOTOTH = sum(JTOTOTH, na.rm = T))
DIST30JTOTOTH <- DIST30JTOTOTH %>%
  summarize(JTOTOTH = sum(JTOTOTH, na.rm = T))
#30 + km
DISTPJTOTOTH <- Mode_Flows  %>%
  filter(DIST>30) %>%
  group_by(DIST) %>%
  summarize(JTOTOTH = sum(JTOTOTH, na.rm = T))
DISTPJTOTOTH <- DISTPJTOTOTH %>%
  summarize(JTOTOTH = sum(JTOTOTH, na.rm = T))

#Change R options to not show scientific notation
options(scipen = 999)

#Merge results together
DISTALL <- data.frame("DIST" = c('A) 0-1km','B) 1-2km','C) 2-5km','D) 5-10km','E) 10-20km','F) 20-30km','G) 30+km'),
                      "JTOT" = rbind(DIST1,DIST2,DIST5,DIST10,DIST20,DIST30,DISTP),
                      "WALK" = rbind(DIST1N_WALK2,DIST2N_WALK2,DIST5N_WALK2,DIST10N_WALK2,DIST20N_WALK2,DIST30N_WALK2,DISTPN_WALK2),
                      "TRANSIT" = rbind(DIST1N_TRAN2,DIST2N_TRAN2,DIST5N_TRAN2,DIST10N_TRAN2,DIST20N_TRAN2,DIST30N_TRAN2,DISTPN_TRAN2),
                      "AUTO" = rbind(DIST1JTOTAUT,DIST2JTOTAUT,DIST5JTOTAUT,DIST10JTOTAUT,DIST20JTOTAUT,DIST30JTOTAUT,DISTPJTOTAUT),
                      "BIKE" = rbind(DIST1N_BIKE2,DIST2N_BIKE2,DIST5N_BIKE2,DIST10N_BIKE2,DIST20N_BIKE2,DIST30N_BIKE2,DISTPN_BIKE2),
                      "WFH" = rbind(DIST1JTOTWFH,DIST2JTOTWFH,DIST5JTOTWFH,DIST10JTOTWFH,DIST20JTOTWFH,DIST30JTOTWFH,DISTPJTOTWFH),
                      "OTH" = rbind(DIST1JTOTOTH,DIST2JTOTOTH,DIST5JTOTOTH,DIST10JTOTOTH,DIST20JTOTOTH,DIST30JTOTOTH,DISTPJTOTOTH))

DISTALL

#Stacked bar chart
# First we have to reshape to "long" format
long <- DISTALL %>% gather(Mode, Flows, c(N_WALK2, N_BIKE2, N_TRAN2, JTOTAUT, JTOTWFH))
# Stacked bar chart
ggplot(long, aes(fill=Mode, y=Flows, x=DIST)) +
  geom_bar(position="stack", stat="identity") +
  labs(title = "Number of Flows by Mode and Distance", x = "Distance bands", y = "# Flows") +
  scale_fill_brewer(palette = "BuPu", labels = c("Auto", "WFH", "Cycle", "Transit", "Walk"))




####TRACT CHARACTERISTICS


#Aggregate all by origin
ODTR_orig <- Mode_Flows %>%
  group_by(origTR) %>%
  summarize(JTOTO = sum(JTOT, na.rm = T), JTOTATO = sum(JTOTAUT, na.rm = T), JTOTTNO = sum(JTOTTRN, na.rm = T), JTOTBKO = sum(JTOTBIK, na.rm = T), JTOTWFHO = sum(JTOTWFH, na.rm = T),
            JTOTWKO = sum(JTOTWLK, na.rm = T), NTRANO = sum(N_TRAN2, na.rm = T), NBIKEO = sum(N_BIKE2, na.rm = T), NWALKO = sum(N_WALK2, na.rm = T))

sum(ODTR_orig$JTOTO)

#Aggregate all by destination
ODTR_dest <- Mode_Flows %>%
  group_by(destTR) %>%
  summarize(JTOTD = sum(JTOT, na.rm = T), JTOTATD = sum(JTOTAUT, na.rm = T), JTOTTND = sum(JTOTTRN, na.rm = T), JTOTBKD = sum(JTOTBIK, na.rm = T), JTOTWFHD = sum(JTOTWFH, na.rm = T),
            JTOTWKD = sum(JTOTWLK, na.rm = T), NTRAND = sum(N_TRAN2, na.rm = T), NBIKED = sum(N_BIKE2, na.rm = T), NWALKD = sum(N_WALK2, na.rm = T))

sum(ODTR_dest$JTOTD)

#Find on-diagonal values
ODTR_ondiag <- Mode_Flows[Mode_Flows$origTR==Mode_Flows$destTR, ]

#Merge total and off-diagonal files together
ODTR_orig <- plyr::rename(ODTR_orig, c("origTR" = "TR"))
ODTR_dest <- plyr::rename(ODTR_dest, c("destTR" = "TR"))
ODTR_ondiag <- plyr::rename(ODTR_ondiag, c("origTR" = "TR"))

Merge1 <- merge(ODTR_orig,ODTR_dest,by="TR")
Merge2 <- merge(Merge1,ODTR_ondiag,by="TR", all.x=TRUE)
poi2$GEOID <- as.character(poi2$GEOID)
Merge2 <- merge(Merge2,poi2,by.x="TR",by.y="GEOID", all.x=TRUE)

#Calculate difference between naive and weighted estimates by mode
# Merge2$NBIKEDFO <- Merge2$NBIKEO - Merge2$JTOTBKO
# Merge2$NWALKDFO <- Merge2$NWALKO - Merge2$JTOTWKO
# Merge2$NTRANDFO <- Merge2$NTRANO - Merge2$JTOTTNO

Merge2$NBIKEDFD <- Merge2$NBIKED - Merge2$JTOTBKD
Merge2$NWALKDFD <- Merge2$NWALKD - Merge2$JTOTWKD
Merge2$NTRANDFD <- Merge2$NTRAND - Merge2$JTOTTND

# Merge2$NBIKEPDO <- Merge2$NBIKEDFO / Merge2$JTOTO
# Merge2$NWALKPDO <- Merge2$NWALKDFO / Merge2$JTOTO
# Merge2$NTRANPDO <- Merge2$NTRANDFO / Merge2$JTOTO

Merge2$NBIKEDIF <- Merge2$NBIKEDFD / Merge2$JTOTD
Merge2$NWALKDIF <- Merge2$NWALKDFD / Merge2$JTOTD
Merge2$NTRANDIF <- Merge2$NTRANDFD / Merge2$JTOTD

#Calculate percentages of Self-Containment for weighted estimates by mode
Merge2$JTOTS <- Merge2$JTOT / Merge2$JTOTO
Merge2$JTOTATS <- Merge2$JTOTAUT / Merge2$JTOTATO
Merge2$NTRANS <- Merge2$N_TRAN2 / Merge2$NTRANO 
Merge2$NBIKES <- Merge2$N_BIKE2 / Merge2$NBIKEO
Merge2$NWALKS <- Merge2$N_WALK2 / Merge2$NWALKO
Merge2$JTOTWFHS <- Merge2$JTOTWFH / Merge2$JTOTWFHO

#Calculate D/O Ratio for weighted estimates by mode
Merge2$JTOTDOR <- Merge2$JTOTD / Merge2$JTOTO
Merge2$AUTODOR <- Merge2$JTOTATD / Merge2$JTOTATO
Merge2$TRANDOR <- Merge2$NTRAND / Merge2$NTRANO
Merge2$BIKEDOR <- Merge2$NBIKED / Merge2$NBIKEO
Merge2$WALKDOR <- Merge2$NWALKD / Merge2$NWALKO
Merge2$WFHDOR <- Merge2$JTOTWFHD / Merge2$JTOTWFHO

Merge2$JTOTAL <- Merge2$JTOTD + Merge2$JTOTO

#Filter some columns
Tract_Characteristics <- Merge2 %>%
  select(GISJOIN, TR, JTOTAL, JTOT, JTOTO, JTOTD, NTRANDFD, NBIKEDFD, NWALKDFD, NTRANDIF, NBIKEDIF, NWALKDIF, JTOTS, JTOTATS, NTRANS, NBIKES, NWALKS, JTOTWFHS, 
         JTOTDOR, AUTODOR, TRANDOR, BIKEDOR, WALKDOR, WFHDOR, JTOTATO, NTRANO, NBIKEO, JTOTATD, NTRAND, NBIKED)

Tract_Characteristics[sapply(Tract_Characteristics, is.infinite)] <- 0
Tract_Characteristics[sapply(Tract_Characteristics, is.na)] <- 0

#Export as a CSV
write.csv(Tract_Characteristics, file = "Tract_Characteristics.csv")


#############EXTERNAL VALIDATION#############

#Load Divvy data
Divvy <- read.csv("Divvy_Trips.csv")
Divvy <- as.data.frame(Divvy)

library(stringr)

Divvy$Morning <- str_sub(Divvy$START.TIME, -2, -1)

Divvy$Hour <- as.numeric(str_sub(Divvy$START.TIME, -11, -10))

Divvy <- subset(Divvy, (Divvy$Morning=="AM" & Divvy$Hour>=8 & Divvy$Hour <=10))

write.csv(Divvy, file = "Divvy.csv")

DivvyOD <- read.csv("Divvy_Origins_Dest.csv")

DivvyOD$origTR <- as.factor(DivvyOD$origTR)
DivvyOD$destTR <- as.factor(DivvyOD$destTR)

#Filter and aggregate
DivvyOD_T <- DivvyOD %>%
  group_by(origTR, destTR) %>%
  summarize(NBIKE = n())

write.csv(DivvyOD_T, file = "DivvyOD.csv")

#TNP
TNPOD <- read.csv("TNP_Origins_Dest.csv")

TNPOD$origTR <- as.factor(TNPOD$origTR)
TNPOD$destTR <- as.factor(TNPOD$destTR)

#Filter and aggregate
TNPOD_T <- TNPOD %>%
  group_by(origTR, destTR) %>%
  summarize(NAUTO = n())

write.csv(TNPOD_T, file = "TNPOD.csv")




