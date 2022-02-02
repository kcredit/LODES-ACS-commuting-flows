###Loading Libraries and Opening LODES Data

#Load libraries and install packages
packages.wanted <- c("conflicted", "finalfit", "leafgl","mapview","data.table","s2","sf","r5r","GGally","MASS", "robustbase","mvoutlier","RColorBrewer","rgdal","ggplot2","Hmisc", "dplyr","purrr","foreign","stargazer","tidyr")
#for (package in packages.wanted) install.packages(package,character.only=TRUE)
for (package in packages.wanted) require(package,character.only=TRUE)

# install.packages("r5r")

conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")
conflict_prefer("summarize", "dplyr")

#Set the working directory
setwd("~/Dropbox/Courses/Transportation Geography 2020/Lab 2/Data")

options(digits=8)

########Derive Beta, Mu, and Alpha coefficients#######################

# trip <- read.csv("trippub.csv")
# 
# tripchi <- subset(trip, (trip$CDIVMSAR==31 & trip$TRIPPURP=="HBW"))
# 
# tripchi2 <- tripchi %>%
#   select(TDCASEID, TRPMILES, TRVLCMIN, TRPTRANS)
# 
# tripchi2$DIST <- tripchi2$TRPMILES*1.60934
# 
# tripchiwalk <- subset(tripchi2, tripchi2$TRPTRANS==01)
# tripchibike <- subset(tripchi2, tripchi2$TRPTRANS==02)
# tripchitran <- subset(tripchi2, ((tripchi2$TRPTRANS==11)|(tripchi2$TRPTRANS==15)|(tripchi2$TRPTRANS==16)))
# 
# write.csv(tripchiwalk, file = "tripchiwalk.csv")
# write.csv(tripchibike, file = "tripchibike.csv")
# write.csv(tripchitran, file = "tripchitran.csv")

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

#Filter by Cook County (FIPS code 17031), aggregate values to tract
ODTR <- OD %>%
  filter(origCT=="17031" & destCT=="17031") %>%
  group_by(origTR, destTR) %>%
  summarize(JTOT = sum(JTOT, na.rm = T))


###1.3 Find Commuting by Mode and Merge with Origin and Destination Coordinates 

#Find off-diagonal values (true commuting)
ODTR_offdiag <- ODTR[ODTR$origTR!=ODTR$destTR, ]

#Load ACS data
ACS <- read.csv("ACS_2013_2017_Mode.csv")
ACS <- as.data.frame(ACS)
ACS <- plyr::rename(ACS, c("GEOID10" = "origTR"))

#Merge ACS data to LODES data by origin ID
# MergeACS <- merge(ODTR_offdiag,ACS,by="origTR", all.x=TRUE)

MergeACS <- merge(ODTR,ACS,by="origTR", all.x=TRUE)

#Estimate expected number of commuters by mode
MergeACS$JTOTAUT <- MergeACS$JTOT * (MergeACS$AUTO/MergeACS$WORK16)
MergeACS$JTOTTRN <- MergeACS$JTOT * (MergeACS$TRANSIT/MergeACS$WORK16)
MergeACS$JTOTBIK <- MergeACS$JTOT * (MergeACS$BIKE/MergeACS$WORK16)
MergeACS$JTOTWLK <- MergeACS$JTOT * (MergeACS$WALK/MergeACS$WORK16)
MergeACS$JTOTOTH <- MergeACS$JTOT * (MergeACS$OTHER/MergeACS$WORK16)
MergeACS$KTOT <- MergeACS$JTOTAUT + MergeACS$JTOTTRN + MergeACS$JTOTBIK + MergeACS$JTOTWLK + MergeACS$JTOTOTH
MergeACS$WTF <- MergeACS$JTOT - MergeACS$KTOT

#Rename origin-based ycoord and xcoord for "h_lat" and "h_long"
MergeChi <- plyr::rename(MergeACS, c("ycoord" = "h_lat", "xcoord" = "h_long"))

#Ensure only properly-calculated distances (i.e., "complete cases" or non-NA values, are retained)
MergeChi <- MergeChi[complete.cases(MergeChi[ , 4]),]

#Re-join ACS data, this time by destTR
ACSdest <- read.csv("ACS_2013_2017_Mode.csv")
ACSdest <- as.data.frame(ACSdest)
ACSdest <- plyr::rename(ACSdest, c("GEOID10" = "destTR"))
ACSdest <- plyr::rename(ACSdest, c("ycoord" = "w_lat", "xcoord" = "w_long"))

#Merge destination-based coordinates with commuting flows by mode
MergeChi2 <- merge(MergeChi,ACSdest,by="destTR",all.x=TRUE)

#Filter columns to contain only origin/destination coordinates and flows by mode
MergeChi2 <- MergeChi2 %>%
  select(origTR, destTR, h_lat, h_long, w_lat, w_long, JTOT, JTOTAUT, JTOTTRN, JTOTBIK, JTOTWLK, JTOTOTH, KTOT, WTF)

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
MergeChi2$DIST <- gcd.slc(MergeChi2$long1, MergeChi2$lat1, MergeChi2$long2, MergeChi2$lat2)

#Solve NAN problem for on-diagonal commutes
MergeChi2$DIST[is.nan(MergeChi2$DIST)]<-0
MergeChi2$DIST[is.na(MergeChi2$DIST)]<-0

MergeChi2$DIST <- MergeChi2$DIST+.05

#Calculate the weighted number of walking trips

#MergeChi2$W_JWALK <- ifelse(MergeChi2$DIST<2,MergeChi2$JTOTWLK,(MergeChi2$JTOTWLK*MergeChi2$DIST^-4))
#Weights calculated throughout, not only at boundary of min. threshold
#MergeChi2$W_JWALK <- ifelse(MergeChi2$DIST>5,0,
#                            ifelse(MergeChi2$DIST<2,MergeChi2$JTOTWLK,(MergeChi2$JTOTWLK*MergeChi2$DIST^-4)))

# MergeChi2$W_JWALK <- ifelse(MergeChi2$DIST>3.5,0,
#                             ifelse(MergeChi2$DIST<1,MergeChi2$JTOTWLK,(MergeChi2$JTOTWLK*MergeChi2$DIST^-0.714)))

MergeChi2$W_JWALK <- ifelse(MergeChi2$DIST>3.5,0,
                            ifelse(MergeChi2$DIST<1,MergeChi2$JTOTWLK,(MergeChi2$JTOTWLK*MergeChi2$DIST^-0.714)))


# MergeChi2$W_JWALK <- ifelse(MergeChi2$DIST>3.5,0,MergeChi2$JTOTWLK)


#Find the remainder (excess) walking trips, i.e., the difference between weighted and raw predicted trips
MergeChi2$W_REM <- MergeChi2$JTOTWLK - MergeChi2$W_JWALK

#Adds a unique ID value for each row for joins
MergeChi2$ID <- seq.int(nrow(MergeChi2))

#Filter the data by links within a given re-absorption threshold (1km); count the number of destination tracts within this distance for each origin
Neighbors <- MergeChi2 %>%
  filter(DIST<=3.5) %>% 
  add_count(origTR, name = "N_CLOSE") %>%
  select(ID, origTR, destTR, N_CLOSE)

#Merge these counts back into the full dataset
MergeChi3 <- merge(MergeChi2,Neighbors,by="ID",all.x=TRUE)

#Filter the data by links within a given re-absorption threshold (3.5km); find total number of commuters in destination tracts linked to each origin
Neighbors2 <- MergeChi2 %>%
  filter(DIST<=3.5) %>% 
  group_by(origTR) %>%
  summarize(TTOT = sum(JTOT, na.rm=T))

#Rename origin tract id for joining to full data; merge
Neighbors2$origTR.y <- Neighbors2$origTR
MergeChi4 <- merge(MergeChi3,Neighbors2,by="origTR.y",all.x=TRUE)

#Find the total amount of excess trips for each origin id
Apportion <- MergeChi4 %>%
  group_by(origTR.x) %>%
  summarize(T_WREM = sum(W_REM, na.rm = T))

#Merge back to full dataset
MergeChi5 <- merge(MergeChi4,Apportion,by="origTR.x",all.x=TRUE)

#Divide T_WREM among all nearby tracts evenly
MergeChi5$EVEN <- MergeChi5$T_WREM/MergeChi5$N_CLOSE
MergeChi5$N_WALK <- ifelse(is.na(MergeChi5$EVEN), MergeChi5$W_JWALK, (MergeChi5$W_JWALK + MergeChi5$EVEN))

#Divide T_WREM among all nearby tracts based on ratio of individual link commuters to total number of commuters from that origin to nearby destinations
MergeChi5$ADD_WALK <- ifelse(!is.na(MergeChi5$EVEN), MergeChi5$T_WREM*(MergeChi5$JTOT/MergeChi5$TTOT), 0)
MergeChi5$N_WALK2 <- MergeChi5$ADD_WALK + MergeChi5$W_JWALK

hist(MergeChi5$N_WALK, breaks =seq(0,2000, by=20), xlab="Mode Share %", main="Estimated Walk Percent", col="lightgreen", ylim=c(0, 1200))
hist(MergeChi5$N_WALK2, breaks =seq(0,2000, by=20), xlab="Mode Share %", main="Estimated Walk2 Percent", col="lightgreen", ylim=c(0, 1200))


##Bike Re-Apportionment
#MergeChi5$W_JBIKE <- ifelse(MergeChi5$DIST<3,MergeChi5$JTOTBIK,(MergeChi5$JTOTBIK*MergeChi5$DIST^-3.5))
# MergeChi5$W_JBIKE <- ifelse(MergeChi5$DIST>6.8,0,
#                             ifelse(MergeChi5$DIST<2.6,MergeChi5$JTOTBIK,(MergeChi5$JTOTBIK*(MergeChi5$DIST-1.6)^-0.329)))

MergeChi5$W_JBIKE <- ifelse(MergeChi5$DIST>6.8,0,(MergeChi5$JTOTBIK*MergeChi5$DIST^-0.329))

# MergeChi5$W_JBIKE <- ifelse(MergeChi5$DIST>6.8,0,MergeChi5$JTOTBIK)

MergeChi5$B_REM <- MergeChi5$JTOTBIK - MergeChi5$W_JBIKE

NeighborsB <- MergeChi5 %>%
  filter(DIST<=6.8) %>% 
  add_count(origTR.x, name = "B_CLOSE") %>%
  select(ID, origTR.x, destTR.x, B_CLOSE)

MergeChi6 <- merge(MergeChi5,NeighborsB,by="ID",all.x=TRUE)

NeighborsB2 <- MergeChi5 %>%
  filter(DIST<=6.8) %>% 
  group_by(origTR.x) %>%
  summarize(TTOTB = sum(JTOT, na.rm=T))

NeighborsB2$origTR.x.x <- NeighborsB2$origTR.x

MergeChi7 <- merge(MergeChi6,NeighborsB2,by="origTR.x.x",all.x=TRUE)

ApportionB <- MergeChi7 %>%
  group_by(origTR.x.x) %>%
  summarize(T_BREM = sum(B_REM, na.rm = T))

MergeChi8 <- merge(MergeChi7,ApportionB,by="origTR.x.x",all.x=TRUE)

MergeChi8$EVENB <- MergeChi8$T_BREM/MergeChi8$B_CLOSE

MergeChi8$N_BIKE <- ifelse(is.na(MergeChi8$EVENB), MergeChi8$W_JBIKE, (MergeChi8$W_JBIKE + MergeChi8$EVENB))

MergeChi8$ADD_BIKE <- ifelse(!is.na(MergeChi8$EVENB), MergeChi8$T_BREM*(MergeChi8$JTOT/MergeChi8$TTOTB), 0)

MergeChi8$N_BIKE2 <- MergeChi8$ADD_BIKE + MergeChi8$W_JBIKE

##Transit Re-Apportionment
# install.packages('bit64')

#Increase working memory available for Java
options(java.parameters = "-Xmx10G")

# Load packages necessary for vignette


# define file path to data (~Library/Frameworks/R.framework/Versions/4.0/Resources/library/r5r)
# GTFS data from: http://transitfeeds.com/p/chicago-transit-authority/165 "11 November 2021" MUST USE SAME DATE
data_path <- "~/Dropbox/Research Projects/LODES modal commuting/chicagopoa"
list.files(data_path)

# open and show points of interest from Chicago 
poi <- fread(file.path(data_path, "poa_tracts2.csv"))
head(poi)

# Build a routable transport network with setup_R5

# Indicate the path where OSM and GTFS data are stored. Note: this step took me upwards of 4 
# minutes to finish computing  
r5r_core <- setup_r5(data_path = data_path, verbose = FALSE)

# Now that we have created the requisite files, we give examples of R5R's core functions: 
# travel_time_matrix() and detailed_itineraries(). See the vignette for an overview of their
# necessary arguments. 

# Step 7: Calculate an example travel time matrix

# set inputs
mode <- c("TRANSIT") 
max_walk_dist <- 15000
max_trip_duration <- 720 #Think about varying distance based on location (i.e., force trips to Loop)
departure_datetime <- as.POSIXct("11-11-2021 15:00:00",
                                 format = "%d-%m-%Y %H:%M:%S")
# calculate a travel time matrix
ttmt <- travel_time_matrix(r5r_core = r5r_core,
                           origins = poi,
                           destinations = poi,
                           mode = mode,
                           departure_datetime = departure_datetime,
                           time_window = 5,
                           breakdown = TRUE,
                           max_walk_dist = max_walk_dist,
                           max_trip_duration = max_trip_duration,
                           walk_speed = 5,
                           bike_speed = 18,
                           verbose = FALSE)
# stop_r5(r5r_core)
# rJava::.jgc(R.gc = TRUE)
# r5r_core <- setup_r5(data_path = data_path, verbose = FALSE)

# mode2 <- c("WALK") 
# ttmw <- travel_time_matrix(r5r_core = r5r_core,
#                            origins = poi,
#                            destinations = poi,
#                            mode = mode2,
#                            departure_datetime = departure_datetime,
#                            max_walk_dist = max_walk_dist,
#                            max_trip_duration = max_trip_duration,
#                            verbose = FALSE)
stop_r5(r5r_core)
rJava::.jgc(R.gc = TRUE)

#################No longer needed with "breakdown" option
# ttmw <- plyr::rename(ttmw, c("travel_time" = "walk"))
# ttmw$uniqueID <- paste0(as.character(ttmw$fromId),", ", as.character(ttmw$toId))
# 
# ttmw2 <- ttmw %>%
#   select(uniqueID, walk)
 
poi$toId <- as.character(poi$id)
poi$fromId <- as.character(poi$id)

ttmt_m1 <- merge(ttmt,poi,by="fromId", all.x=TRUE)
ttmt_m1 <- ttmt_m1 %>%
  select(fromId, toId.x, GEOID, lat, lon, n_rides)
ttmt_m1 <- plyr::rename(ttmt_m1, c("GEOID" = "origTR", "lat" = "h_lat", "lon" = "h_long", "toId.x" = "toId"))

ttmt_m2 <- merge(ttmt_m1,poi,by="toId", all.x=TRUE)
ttmt_m2 <- ttmt_m2 %>%
  select(fromId.x, toId,origTR, GEOID, h_lat, h_long, lat, lon, n_rides)
ttmt_m2 <- plyr::rename(ttmt_m2, c("GEOID" = "destTR", "lat" = "w_lat", "lon" = "w_long", "fromId.x" = "fromId"))

ttmt_m2$uniqueID <- paste0(as.character(ttmt_m2$fromId),", ", as.character(ttmt_m2$toId))

#Create a unique identifier for origins and destinations
MergeChi8$TRIP_ID <- paste0(as.character(MergeChi8$origTR.x.x),", ", as.character(MergeChi8$destTR.x.x))
ttmt_m2$TRIP_ID <- paste0(as.character(ttmt_m2$origTR),", ", as.character(ttmt_m2$destTR))

MergeChi9 <- merge(MergeChi8,ttmt_m2,by="TRIP_ID", all.x=TRUE)

#Filter columns
MergeChi9  <- MergeChi9  %>%
  select(TRIP_ID, ID, origTR.x.x, destTR.x.x, h_lat.x, h_long.x, w_lat.x, w_long.x, DIST, JTOT, JTOTAUT, JTOTTRN, JTOTBIK, JTOTWLK, JTOTOTH, KTOT, WTF, N_BIKE2, N_WALK2,
         n_rides)

#Re-weight transit trips
#Fill walk NA values (over 1500 meters or 150 minutes) with 150
# MergeChi9$walk[is.na(MergeChi9$walk)] <- 301
# MergeChi9$transit[is.na(MergeChi9$transit)] <- 301

#Fix 3 tract-to-tract trips that didn't return a transit or walk time for some reason
# MergeChi9$walk[MergeChi9$TRIP_ID=="17031804405, 17031804405"] <- 0
# MergeChi9$transit[MergeChi9$TRIP_ID=="17031804405, 17031804405"] <- 0
# 
# MergeChi9$walk[MergeChi9$TRIP_ID=="17031804406, 17031804406"] <- 0
# MergeChi9$transit[MergeChi9$TRIP_ID=="17031804406, 17031804406"] <- 0
# 
# MergeChi9$walk[MergeChi9$TRIP_ID=="17031804404, 17031804404"] <- 0
# MergeChi9$transit[MergeChi9$TRIP_ID=="17031804404, 17031804404"] <- 0

# MergeChi9$ratio <- MergeChi9$transit/MergeChi9$walk

# MergeChi9$gap <- MergeChi9$walk-MergeChi9$transit

MergeChi9$n_rides[is.na(MergeChi9$n_rides)] <- 0

# MergeChi9$ratio[(MergeChi9$origTR.x.x==MergeChi9$destTR.x.x)] <- .05 

# gap_value <- 0 #set a gap value - big hinge point on gap; at 10 minutes there is a significant proportion of tracts whose 
# only transit travel is intra-tract (based on the way we defined transit "neighbors"); at 0, no tracts that only intra-tract 
# transit travel. Changes make almost no differenc eon large-scale patterns. 

# MergeChi9$n_rides[(MergeChi9$origTR.x.x==MergeChi9$destTR.x.x)] <- 1 #this solves the problem of tracts not having neighbors in NeighborsT2 due to using gap or ratio (using a DIST threshold,
# all tracts have neighbors), except for two tracts: 17031830400 and 17031826902, which have 0 intra-tract flows. This results in a small mismatch between "new" and "old" 
# transit flows because the raw estimated transit flows out of these tracts are simply lost. This amounts to 186 out of 353,711 total raw transit flows.

# ff_glimpse(MergeChi9) #Also have some issues with negative N_WALK2 (I think the issues are all associated with distance decay calcs)

# MergeChi9$W_JTRAN <- ifelse((MergeChi9$ratio>.75|MergeChi9$transit>150),0,
#                             ifelse(MergeChi9$transit<85,MergeChi9$JTOTTRN,(MergeChi9$JTOTTRN*(MergeChi9$transit-84)^-0.008)))

# MergeChi9$W_JTRAN <- ifelse(MergeChi9$ratio>.75,0,MergeChi9$JTOTTRN)

MergeChi9$W_JTRAN <- ifelse(MergeChi9$n_rides==0,0,MergeChi9$JTOTTRN)

MergeChi9$T_REM <- MergeChi9$JTOTTRN - MergeChi9$W_JTRAN

NeighborsT <- MergeChi9 %>%
  filter(n_rides>=1) %>% #DIST=35.5 not a good solution here, but it maintains 100% count. We should be using ratio (or transit at the very least), but we miss some observations when we do that.
  add_count(origTR.x.x, name = "T_CLOSE") %>%
  select(ID, origTR.x.x, destTR.x.x, T_CLOSE)

MergeChi10 <- merge(MergeChi9,NeighborsT,by="ID",all.x=TRUE)

NeighborsT2 <- MergeChi9 %>%
  filter(n_rides>=1) %>% 
  group_by(origTR.x.x) %>%
  summarize(TTOTT = sum(JTOT, na.rm=F))

NeighborsT2$origTR.x.x.x <- NeighborsT2$origTR.x.x

MergeChi11 <- merge(MergeChi10,NeighborsT2,by="origTR.x.x.x",all.x=TRUE)

# MergeChi11$TTOTT[is.na(MergeChi11$TTOTT)] <- 0

ApportionT <- MergeChi11 %>%
  group_by(origTR.x.x.x) %>%
  summarize(T_TREM = sum(T_REM, na.rm = T))

MergeChi12 <- merge(MergeChi11,ApportionT,by="origTR.x.x.x",all.x=TRUE)

MergeChi12$EVENT <- MergeChi12$T_TREM/MergeChi12$T_CLOSE

# MergeChi12$EVENT[sapply(MergeChi12$EVENT, is.infinite)] <- 0

MergeChi12$N_TRAN <- ifelse(is.na(MergeChi12$EVENT), MergeChi12$W_JTRAN, (MergeChi12$W_JTRAN + MergeChi12$EVENT))

MergeChi12$ADD_TRAN <- ifelse(!is.na(MergeChi12$EVENT), MergeChi12$T_TREM*(MergeChi12$JTOT/MergeChi12$TTOTT), 0)

MergeChi12$N_TRAN2 <- MergeChi12$ADD_TRAN + MergeChi12$W_JTRAN
MergeChi12$N_TRAN2[is.na(MergeChi12$N_TRAN2)] <- 0

# #Calculate "Original" totals by mode
MergeChi12$O_TOT <- MergeChi12$JTOTWLK + MergeChi12$JTOTAUT + MergeChi12$JTOTTRN + MergeChi12$JTOTBIK + MergeChi12$JTOTOTH
# 
# #Calculate "New" totals by mode
MergeChi12$N_TOT <- MergeChi12$N_WALK2 + MergeChi12$JTOTAUT + MergeChi12$N_TRAN2 + MergeChi12$N_BIKE2 +  MergeChi12$JTOTOTH
# 
# #Find percentage of newly-adjusted trips by mode
# MergeChi8$NW2PCT <- MergeChi8$N_WALK2/MergeChi8$N_TOT
# MergeChi8$NAPCT <- MergeChi8$JTOTAUT/MergeChi8$N_TOT
# MergeChi8$NTRPCT <- MergeChi8$JTOTTRN/MergeChi8$N_TOT
# MergeChi8$NB2CT<- MergeChi8$N_BIKE2/MergeChi8$N_TOT
# 
# #Remove NA values to solve the 0 numerator/denominator problem by creating a new column and 
# MergeChi8$NW2PCT2 <- ifelse(is.na(MergeChi8$NW2PCT), 0, MergeChi8$NW2PCT)
# MergeChi8$NAPCT2 <- ifelse(is.na(MergeChi8$NAPCT), 0, MergeChi8$NAPCT)
# MergeChi8$NTRPCT2 <- ifelse(is.na(MergeChi8$NTRPCT), 0, MergeChi8$NTRPCT)
# MergeChi8$NB2CT2 <- ifelse(is.na(MergeChi8$NB2CT), 0, MergeChi8$NB2CT)
# 
# #Multiply percentage of newly-adjusted trips by mode with original total number of trips to preserve totals
# MergeChi8$NNWALK <- MergeChi8$NW2PCT2 * MergeChi8$O_TOT
# MergeChi8$NNAUTO <- MergeChi8$NAPCT2 * MergeChi8$O_TOT
# MergeChi8$NNTRAN <- MergeChi8$NTRPCT2 * MergeChi8$O_TOT
# MergeChi8$NNBIKE<- MergeChi8$NB2CT2 * MergeChi8$O_TOT

#Check the aggregate values for N_WALK2, NNWALK2, N_TOT, O_TOT, etc.
Test <- MergeChi12 %>%
  summarize(sum(JTOTWLK, na.rm=T), sum(N_WALK2, na.rm=T), #sum(NNWALK, na.rm=T),
            sum(JTOTBIK, na.rm=T), sum(N_BIKE2, na.rm=T), #sum(NNBIKE, na.rm=T), 
            sum(JTOTAUT, na.rm=T), #sum(NNAUTO, na.rm=T),
            sum(JTOTTRN, na.rm=T), sum(N_TRAN2, na.rm=T), #sum(NNTRAN, na.rm=T),
            sum(JTOTOTH, na.rm=T),
            sum(N_TOT, na.rm=T), sum(O_TOT, na.rm=T), sum(JTOT, na.rm=T), sum(KTOT, na.rm=T), sum(WTF, na.rm=T))

#Export test as a CSV
write.csv(Test, file = "Test.csv")

new_DF <- MergeChi12[is.na(MergeChi12$JTOTAUT),] %>%
  summarize(sum(JTOT, na.rm=T))


#Compare original ACS mode share by origin to new estimates
Mode <- MergeChi12 %>%
  group_by(origTR.x.x.x) %>%
  summarize(JTOT = sum(JTOT, na.rm=T), TOTAUTO = sum(JTOTAUT, na.rm=T), TOTTRAN = sum(N_TRAN2, na.rm=T), TOTBIKE = sum(N_BIKE2, na.rm=T), TOTWALK = sum(N_WALK2, na.rm=T))

Mode$AUTOP1 <- Mode$TOTAUTO/Mode$JTOT
Mode$TRANS1 <- Mode$TOTTRAN/Mode$JTOT
Mode$BIKE1 <- Mode$TOTBIKE/Mode$JTOT
Mode$WALK1 <- Mode$TOTWALK/Mode$JTOT

Mode <- plyr::rename(Mode, c("origTR.x.x.x" = "origTR"))
MergeMode <- merge(Mode,ACS,by="origTR", all.x=TRUE)


######################Test whether or not we correctly matched our estimates of commuting by mode at the origin to the original ACS data
hist(MergeMode$AUTOP1, breaks =seq(0,1, by=.02), xlab="Mode Share %", main="A) Estimated Auto Percent", col="lightblue", ylim=c(0, 100))
hist(MergeMode$AUTOP, breaks =seq(0,1, by=.02), xlab="Mode Share %", main="B) ACS Auto Percent", col="lightblue", ylim=c(0, 100))
hist(MergeMode$TRANS1, breaks =seq(0,1, by=.02), xlab="Mode Share %", main="C) Estimated Transit Percent", col="lightblue", ylim=c(0, 160))
hist(MergeMode$TRANSITP, breaks =seq(0,1, by=.02), xlab="Mode Share %", main="D) ACS Transit Percent", col="lightblue", ylim=c(0, 160))
hist(MergeMode$BIKE1, breaks =seq(0,1, by=.02), xlab="Mode Share %", main="E) Estimated Cycling Percent", col="lightblue", ylim=c(0, 1200))
hist(MergeMode$BIKEP, breaks =seq(0,1, by=.02), xlab="Mode Share %", main="F) ACS Cycling Percent", col="lightblue", ylim=c(0, 1200))
hist(MergeMode$WALK1, breaks =seq(0,1, by=.02), xlab="Mode Share %", main="G) Estimated Walking Percent", col="lightblue", ylim=c(0, 1200))
hist(MergeMode$WALKP, breaks =seq(0,1, by=.02), xlab="Mode Share %", main="H) ACS Walking Percent", col="lightblue", ylim=c(0, 1200))
######################

MergeMode$ACSTOTP <- MergeMode$AUTOP + MergeMode$TRANSITP + MergeMode$BIKEP + MergeMode$WALKP

mean(MergeMode$ACSTOTP, na.rm=TRUE)

####Rename and Filter the data
MergeChi12 <- plyr::rename(MergeChi12, c("h_lat.x" = "h_lat", "h_long.x" = "h_long", "w_lat.x" = "w_lat", "w_long.x" = "w_long"))
Mode_Flows <- MergeChi12 %>%
  select(h_lat, h_long, w_lat, w_long, origTR.x.x.x, destTR.x.x.x, DIST, n_rides, JTOT, JTOTAUT, JTOTTRN, JTOTBIK, JTOTWLK, JTOTOTH, N_TRAN2, N_BIKE2, N_WALK2)

Mode_Flows <- Mode_Flows %>% mutate(id = row_number())

Mode_Flows <- plyr::rename(Mode_Flows, c("origTR.x.x.x" = "origTR"))
Mode_Flows <- plyr::rename(Mode_Flows, c("destTR.x.x.x" = "destTR"))

summary(Mode_Flows)

#Export all data as a CSV
write.csv(Mode_Flows, file = "Mode_Flows.csv")

























# Create distance bins to quantify over-estimation of local flows
Mode_Flows$Bin <- ifelse(Mode_Flows$DIST<1,1,ifelse(Mode_Flows$DIST<2,2,ifelse(Mode_Flows$DIST<5,3,ifelse(Mode_Flows$DIST<10,4,
                                                                                                          ifelse(Mode_Flows$DIST<20,5,ifelse(Mode_Flows$DIST<30,6,7))))))
Bins <- Mode_Flows %>%
  group_by(Bin) %>%
  summarize(JTOT = sum(JTOT, na.rm = T), AUT = sum(JTOTAUT, na.rm = T), TRN = sum(N_TRAN2, na.rm = T), BIKE = sum(N_BIKE2, na.rm = T), WALK = sum(N_WALK2, na.rm = T), OTH = sum(JTOTOTH, na.rm = T))

Bins$SUM_NEW <- Bins$AUT + Bins$TRN + Bins$BIKE + Bins$WALK + Bins$OTH
Bins$DIF <- Bins$SUM_NEW - Bins$JTOT
Bins$PerDIF <- Bins$DIF/(sum(Bins$JTOT, na.rm=TRUE))

# #Under-estimated remainder equals  the % remainder between O_TOT and JTOT and % workers not commuting by these 4 modes in ACS
# Est <- sum(Bins$PerDIF, na.rm=TRUE)
# 
# NONMode <- (sum(ACS$WORK16) - (sum(ACS$AUTO) + sum(ACS$TRANSIT) + sum(ACS$BIKE) + sum(ACS$WALK))) / (sum(ACS$WORK16))
# 
# Mode <- (sum(ACS$AUTO) + sum(ACS$TRANSIT) + sum(ACS$BIKE) + sum(ACS$WALK)) / sum(ACS$WORK16)
# Mode
# 
# Est+NONMode

#Export bins to a CSV
write.csv(Bins, file = "Bins.csv")




















###Characterizing neighborhood areas
#Aggregate all by origin
ODTR_orig <- Mode_Flows %>%
  group_by(origTR) %>%
  summarize(JTOTO = sum(JTOT, na.rm = T), JTOTATO = sum(JTOTAUT, na.rm = T), JTOTTNO = sum(JTOTTRN, na.rm = T), JTOTBKO = sum(JTOTBIK, na.rm = T),
            JTOTWKO = sum(JTOTWLK, na.rm = T), NTRANO = sum(N_TRAN2, na.rm = T), NBIKEO = sum(N_BIKE2, na.rm = T), NWALKO = sum(N_WALK2, na.rm = T))

sum(ODTR_orig$JTOTO)

#Aggregate all by destination
ODTR_dest <- Mode_Flows %>%
  group_by(destTR) %>%
  summarize(JTOTD = sum(JTOT, na.rm = T), JTOTATD = sum(JTOTAUT, na.rm = T), JTOTTND = sum(JTOTTRN, na.rm = T), JTOTBKD = sum(JTOTBIK, na.rm = T),
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
poi$GEOID <- as.character(poi$GEOID)
Merge2 <- merge(Merge2,poi,by.x="TR",by.y="GEOID", all.x=TRUE)

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

#Combined percent difference (O/D) - positive values = more representation in weighted estimates; negative values = more representation in naive estimates
# Merge2$NBIKEDIF <- Merge2$NBIKEPDO + Merge2$NBIKEPDD
# Merge2$NWALKDIF <- Merge2$NWALKPDO + Merge2$NWALKPDD #Destination-oriented only??
# Merge2$NTRANDIF <- Merge2$NTRANPDO + Merge2$NTRANPDD

#Calculate percentages of Self-Containment for weighted estimates by mode
Merge2$JTOTS <- Merge2$JTOT / Merge2$JTOTO
Merge2$JTOTATS <- Merge2$JTOTAUT / Merge2$JTOTATO
Merge2$NTRANS <- Merge2$N_TRAN2 / Merge2$NTRANO ####### THIS IS A PROBLEM in the re-circulation of transit flows
Merge2$NBIKES <- Merge2$N_BIKE2 / Merge2$NBIKEO
Merge2$NWALKS <- Merge2$N_WALK2 / Merge2$NWALKO

#Calculate D/O Ratio for weighted estimates by mode
Merge2$JTOTDOR <- Merge2$JTOTD / Merge2$JTOTO
Merge2$AUTODOR <- Merge2$JTOTATD / Merge2$JTOTATO
Merge2$TRANDOR <- Merge2$NTRAND / Merge2$NTRANO
Merge2$BIKEDOR <- Merge2$NBIKED / Merge2$NBIKEO
Merge2$WALKDOR <- Merge2$NWALKD / Merge2$NWALKO

Merge2$JTOTAL <- Merge2$JTOTD + Merge2$JTOTO

#Filter some columns
Tract_Characteristics <- Merge2 %>%
  select(GISJOIN, TR, JTOTAL, JTOT, JTOTO, JTOTD, NTRANDIF, NBIKEDIF, NWALKDIF, JTOTS, JTOTATS, NTRANS, NBIKES, NWALKS, JTOTDOR, AUTODOR, TRANDOR, BIKEDOR, WALKDOR)

Tract_Characteristics[sapply(Tract_Characteristics, is.infinite)] <- 0
Tract_Characteristics[sapply(Tract_Characteristics, is.na)] <- 0

#Export as a CSV
write.csv(Tract_Characteristics, file = "Tract_Characteristics.csv")






