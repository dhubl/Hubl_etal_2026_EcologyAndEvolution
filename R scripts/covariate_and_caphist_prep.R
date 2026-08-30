#This script generates all sites and stations for NIMBLE model.
#Detection Covariates are attached to each survey
#Run this before running any NIMBLE models to have Capture Histories and Occupancy & Detection Covariates
#############
#D Hubl
#2026
#############

################################################################################
#dependencies
library(nimble)
library(MCMCvis)
library(coda)
library(tidyr)

######################################################################

stations <- read.csv("site_stations_with_detection_covariates.csv")

#Site_ID is the unique site
#Hex_ID, Rep_ID, Station_ID, Rep, Station_num and Visit_num are the old fisher station designations
#site_rep: which skunk sampling season the record is from (1 or 2)
#site_station_num: which station within the site the record is from most will be 1 as most sites only had 1 station
#site_st_visit_num: survey occasion for the station

######################################################################
######################################################################
#season 1 data
Rep1 <- stations[stations$site_rep==1,]

Rep1 <- Rep1 %>%          #make all sites have an equal number of stations and visits to stations and fill in with NA
  complete(Site_ID,site_station_num,site_st_visit_num, fill = list(n=NA))
Rep1 <- as.data.frame(Rep1) #getting out of tibble
Rep1 <- Rep1 %>% fill(site_rep,Year) #fill in site_rep and Year for surveys we just augmented

#get capture (detection) history for spotted skunk
caphist.1.s <- Rep1[,c(1:3,44)] #just columns site_ID, site_station, Site_st_visit_num, and whether skunk was detected on camera or by hair 
caphist.1.s <- caphist.1.s %>%  
  pivot_wider(
    names_from = c(site_station_num, site_st_visit_num),
    values_from = combined_spottedskunk,
    names_glue = "St.{site_station_num}_V{site_st_visit_num}"
  )


#get cap hist for coyote, no coyotes were detected via DNA
caphist.1.c <- Rep1[,c(1:3,39)]
caphist.1.c <- caphist.1.c %>% 
  pivot_wider(
    names_from = c(site_station_num, site_st_visit_num),
    values_from = Coyote,
    names_glue = "St.{site_station_num}_V{site_st_visit_num}"
  )


#get cap hist for bobcat, no bobcats were detected via DNA
caphist.1.b <- Rep1[,c(1:3,37)]
caphist.1.b <- caphist.1.b %>% 
  pivot_wider(
    names_from = c(site_station_num, site_st_visit_num),
    values_from = Bobcat,
    names_glue = "St.{site_station_num}_V{site_st_visit_num}"
  )

###################################################################
###################################################################
#create community state detection history
caphist.1.all <- matrix(NA, nrow = nrow(caphist.1.s), ncol = ncol(caphist.1.s))
caphist.1.all[,1] <- as.matrix(caphist.1.s[,1]) #siteID
caphist.1.all[caphist.1.s==0 & caphist.1.c==0 & caphist.1.b==0] <- 1 #no detections
caphist.1.all[caphist.1.s==1 & caphist.1.c==0 & caphist.1.b==0] <- 2 #skunks only
caphist.1.all[caphist.1.s==0 & caphist.1.c==1 & caphist.1.b==0] <- 3 #coyotes only
caphist.1.all[caphist.1.s==0 & caphist.1.c==0 & caphist.1.b==1] <- 4 #bobcats only
caphist.1.all[caphist.1.s==1 & caphist.1.c==1 & caphist.1.b==0] <- 5 #skunks and coyotes
caphist.1.all[caphist.1.s==1 & caphist.1.c==0 & caphist.1.b==1] <- 6 #skunk and bobcat
caphist.1.all[caphist.1.s==0 & caphist.1.c==1 & caphist.1.b==1] <- 7 #coyote and bobcat
caphist.1.all[caphist.1.s==1 & caphist.1.c==1 & caphist.1.b==1] <- 8 # all species detected


###################
#do the same for the detection covariates
det.1.moon <- Rep1[,c(1:3,48)]                  #moon illuminance
det.1.moon <- det.1.moon %>% 
  pivot_wider(
    names_from = c(site_station_num, site_st_visit_num),
    values_from = moon.scaled,
    names_glue = "St.{site_station_num}_V{site_st_visit_num}"
  )
det.1.moon <- as.matrix(det.1.moon)


det.1.cam <- Rep1[,c(1:3,49)]                   #camera days
det.1.cam <- det.1.cam %>% 
  pivot_wider(
    names_from = c(site_station_num, site_st_visit_num),
    values_from = camera.days.scaled,
    names_glue = "St.{site_station_num}_V{site_st_visit_num}"
  )
det.1.cam <- as.matrix(det.1.cam)

det.1.bait <- Rep1[,c(1:3,50)]                #bait days (we don't end up using this)
det.1.bait <- det.1.bait %>% 
  pivot_wider(
    names_from = c(site_station_num, site_st_visit_num),
    values_from = bait.days.scaled,
    names_glue = "St.{site_station_num}_V{site_st_visit_num}"
  )
det.1.bait <- as.matrix(det.1.bait)
#####################################################################################
#Repeat for season 2 data

Rep2 <- stations[stations$site_rep==2,]


Rep2 <- Rep2 %>%          #make all sites have the same number of stations and visits and fill in with NA
  complete(Site_ID,site_station_num,site_st_visit_num, fill = list(n=NA))
Rep2 <- as.data.frame(Rep2) #getting out of tibble
Rep2 <- Rep2 %>% fill(site_rep,Year)

#get cap hist for spotted skunk
caphist.2.s <- Rep2[,c(1:3,44)]
caphist.2.s <- caphist.2.s %>% 
  pivot_wider(
    names_from = c(site_station_num, site_st_visit_num),
    values_from = combined_spottedskunk,
    names_glue = "St.{site_station_num}_V{site_st_visit_num}"
  )


#get cap hist for coyote
caphist.2.c <- Rep2[,c(1:3,39)]
caphist.2.c <- caphist.2.c %>% 
  pivot_wider(
    names_from = c(site_station_num, site_st_visit_num),
    values_from = Coyote,
    names_glue = "St.{site_station_num}_V{site_st_visit_num}"
  )


#get cap hist for bobcat
caphist.2.b <- Rep2[,c(1:3,37)]
caphist.2.b <- caphist.2.b %>% 
  pivot_wider(
    names_from = c(site_station_num, site_st_visit_num),
    values_from = Bobcat,
    names_glue = "St.{site_station_num}_V{site_st_visit_num}"
  )

#####################################################################
#####################################################################

#caphist with skunk, coyote, and bobcat
caphist.2.all <- matrix(NA, nrow = nrow(caphist.2.s), ncol = ncol(caphist.2.s))
caphist.2.all[,1]<- as.matrix(caphist.2.s[,1])
caphist.2.all[caphist.2.s==0 & caphist.2.c==0 & caphist.2.b==0] <- 1 #no detections
caphist.2.all[caphist.2.s==1 & caphist.2.c==0 & caphist.2.b==0] <- 2 #skunks only
caphist.2.all[caphist.2.s==0 & caphist.2.c==1 & caphist.2.b==0] <- 3 #coyotes only
caphist.2.all[caphist.2.s==0 & caphist.2.c==0 & caphist.2.b==1] <- 4 #bobcats only
caphist.2.all[caphist.2.s==1 & caphist.2.c==1 & caphist.2.b==0] <- 5 #skunks and coyotes
caphist.2.all[caphist.2.s==1 & caphist.2.c==0 & caphist.2.b==1] <- 6 #skunk and bobcat
caphist.2.all[caphist.2.s==0 & caphist.2.c==1 & caphist.2.b==1] <- 7 #coyote and bobcat
caphist.2.all[caphist.2.s==1 & caphist.2.c==1 & caphist.2.b==1] <- 8 # all species detected


###################
#do the same for the detection covariates
det.2.moon <- Rep2[,c(1:3,48)]
det.2.moon <- det.2.moon %>% 
  pivot_wider(
    names_from = c(site_station_num, site_st_visit_num),
    values_from = moon.scaled,
    names_glue = "St.{site_station_num}_V{site_st_visit_num}"
  )
det.2.moon <- as.matrix(det.2.moon)


det.2.cam <- Rep2[,c(1:3,49)]
det.2.cam <- det.2.cam %>% 
  pivot_wider(
    names_from = c(site_station_num, site_st_visit_num),
    values_from = camera.days.scaled,
    names_glue = "St.{site_station_num}_V{site_st_visit_num}"
  )
det.2.cam <- as.matrix(det.2.cam)

det.2.bait <- Rep2[,c(1:3,50)]
det.2.bait <- det.2.bait %>% 
  pivot_wider(
    names_from = c(site_station_num, site_st_visit_num),
    values_from = bait.days.scaled,
    names_glue = "St.{site_station_num}_V{site_st_visit_num}"
  )
det.2.bait <- as.matrix(det.2.bait)

#####################################################################################
#order the sites in each season so they are the same AND put the sites that were re-sampled in Rep2 at the top
caphist.1.all[,1] #order of sites in REP1
caphist.2.all[,1] #order of sites in REP2
which(caphist.1.all[,1] %in% caphist.2.all[,1]) #which REP1 sites are in REP2 (what sites were resampled)
which(caphist.1.all[,1] %in% caphist.2.all[,1]==FALSE) #which REP1 sites are NOT in REP2
caphist.1.all<- caphist.1.all[c(which(caphist.1.all[,1] %in% caphist.2.all[,1]),which(caphist.1.all[,1] %in% caphist.2.all[,1]==FALSE)),]  #Re-order rows 
identical(caphist.1.all[1:239,1],caphist.2.all[,1]) #if this is true than they are in the same order and all of the REP1 sites that don't have a REP2 are at the bottom of the matrix

caphist.2.all <- rbind(caphist.2.all,caphist.1.all[240:712,]) #add sites that were not resampled to the end of REP2
caphist.2.all[240:712,2:9] <- NA                              #input NA for all those added sites
identical(caphist.2.all[,1],caphist.1.all[,1]) #again are the sites in the same order

#make capture history array. page 1 is season 1, page 2 is season 2
caphist.full.all <- array(c(as.numeric(caphist.1.all[,-1]),as.numeric(caphist.2.all[,-1])),dim=c(712,8,2))
#Rep1 has 712 sites with surveys, Rep2 has 239 and the rest are filler.
#####################################################################################
#do the same so the covariates' orders match the capture histories
tmp <- match(caphist.1.all[,1],det.1.moon[,1])
det.1.moon <- det.1.moon[tmp,]
det.1.cam <- det.1.cam[tmp,]
det.1.bait <- det.1.bait[tmp,]

identical(caphist.1.all[,1],det.1.moon[,1]) #these should all be true if they are the same order
identical(caphist.1.all[,1],det.1.cam[,1])
identical(caphist.1.all[,1],det.1.bait[,1])

identical(caphist.1.all[1:239,1],det.2.bait[,1])#check to see if rep2 order matches the start of rep 1

det.2.moon <- rbind(det.2.moon,det.1.moon[240:712,]) #filling in dummy surveys, does not matter what values are inputted as the model uses a effort matrix to ensure detection probability is zero for dummy surveys 
det.2.cam <- rbind(det.2.cam,det.1.cam[240:712,])
det.2.bait <- rbind(det.2.bait,det.1.bait[240:712,])

identical(caphist.1.all[,1],det.2.moon[,1]) #all should be true
identical(caphist.1.all[,1],det.2.cam[,1])
identical(caphist.1.all[,1],det.2.bait[,1])

det.moon.full <- array(c(as.numeric(det.1.moon[,-1]),as.numeric(det.2.moon[,-1])),dim=c(712,8,2))
det.cam.full <- array(c(as.numeric(det.1.cam[,-1]),as.numeric(det.2.cam[,-1])),dim=c(712,8,2))
det.bait.full <- array(c(as.numeric(det.1.bait[,-1]),as.numeric(det.2.bait[,-1])),dim=c(712,8,2))
############################################################
############################################################
#make the array with Rep as the rows, sites in columns and each replicate its own matrix
#this is the caphistory used in the model. 

CH <- array(NA,dim = c(2,712,8))
for (i in 1:2) {
  for (j in 1:712) {
    for(k in 1:8){
      CH[i,j,k] <- caphist.full.all[j,k,i]
    }
  }
}

CH.effort <- CH      #make an effort array
CH.effort[CH.effort>0] <- 1   #latent categories 1-8 are just a 1 in effort matrix
CH.effort[is.na(CH.effort)] <- 0  #surveys that were not conducted become 0 in effort matrix
CH[is.na(CH)] <- 1                #surveys that were not conducted become 1 (neither skunk, coyote, nor detected) in caphist matrix

######DO the same for the covariates
moon.NIMBLE.det <- array(NA,dim = c(2,712,8))
camera.NIMBLE.det <- array(NA,dim = c(2,712,8))
bait.NIMBLE.det <- array(NA,dim = c(2,712,8))
for (i in 1:2) {
  for (j in 1:712) {
    for(k in 1:8){
      moon.NIMBLE.det[i,j,k] <- det.moon.full[j,k,i]
      camera.NIMBLE.det[i,j,k] <- det.cam.full[j,k,i]
      bait.NIMBLE.det[i,j,k] <- det.bait.full[j,k,i]
    }
  }
}
moon.NIMBLE.det[is.na(moon.NIMBLE.det)] <- 0     #input 0 for covariates of surveys that never happened so NIMBLE doesn't try to estimate them. Any number could be put here
camera.NIMBLE.det[is.na(camera.NIMBLE.det)] <- 0
bait.NIMBLE.det[is.na(bait.NIMBLE.det)] <- 0

###########################################################################
###########################################################################
#################### OCCUPANCY COVARS #####################################
#covariate values calculated in ArcGIS
############################################################################
#Area of buffers around sites. Max buffer area is 16.38km^2 with a radius of 2.2834km
#buffers were clipped by Peninsula shoreline and standing water (as categorized by
#LEMMA and confirmed using satellite imagery) was erased from buffers
area <- read.csv("Covariate_files/Site_Buffer_Areas.csv")

tmp <- match(caphist.1.all[,1],area[,1]) #match order of sites 
area <- area[tmp,]
identical(caphist.1.all[,1],area[,1]) #check the orders match

area$Shape_Area / 1000000 #from square meter to square kilometer
###################################################

##########      ELEVATION   #######################

#elevation at the survey site center in meters
elevation <- read.csv("Covariate_files/Elevation_sites_m.csv")

#put sites in the same order as they enter the  NIMBLE model
tmp <- match(caphist.1.all[,1],elevation$site_ID)

elevation <- elevation[tmp,]
identical(elevation[,"site_ID"],caphist.1.all[,1]) #check to make sure it's good

#create Occupancy matrix that can hold all site covariates
Occ.mat <- data.frame(site=caphist.1.all[,1],elevation=elevation[,"Elevation_m"])

####################################################

###########   PRECIPITATION  ########################

#PRISM value at survey site center in mm
PRISM <- read.csv("Covariate_files/PRISM_sites_mm.csv")

#order the sites
tmp <- match(caphist.1.all[,1],PRISM$site_ID)
PRISM <- PRISM[tmp,]
identical(caphist.1.all[,1],PRISM[,"site_ID"]) #check

#add to dataframe
Occ.mat <- cbind(Occ.mat,PRISM$precip_mm)
colnames(Occ.mat)[3] <- "precip_mm"

#####################################################

##########  LENGTH OF EDGE HABITAT  #################

#measurement of cumulative length of edges of polygons that were created from the
#LEMMA data. Open habitats consisted of LEMMA Structural Condition Groups: 0,1,2
#Forested habitats consisted of LEMMA Structural Condition Groups: 3,4,5,6
#All shorelines and standing water was removed from Open habitats to ensure that 
#no transitions from forested habitat to open water was considered an edge.

edge <- read.csv("Covariate_files/Length_of_edge_m.csv")

#each site has multiple records that need to be summed
edge.df <- data.frame(sites=caphist.1.all[,1],distance=rep(NA,712),div.area=rep(NA,712))

for (i in 1:length(caphist.1.all[,1])) {
  trgt <- caphist.1.all[,1][i] #each individual site
  edge.df[i,"distance"] <- sum(edge[edge$site_ID==trgt,"Edge_Length_m"])
  edge.df[i,"div.area"] <- edge.df[i,"distance"] / (area[i,"Shape_Area"] / 1000000)
}

identical(caphist.1.all[,1],edge.df[,1])

#add to matrix
Occ.mat <- cbind(Occ.mat,edge.df[,3])
colnames(Occ.mat)[4] <- "edge.length"

##############################################################

############      % FORESTED      ############################
#uses the same forested habitat categories that was used for edge length
tree <- read.csv("Covariate_files/Tree_Density.csv")
#COUNT: number of 30x30m pixels that were forest, AREA: COUNT x 900m^2, MEAN: average forest density of forested pixels

#rearrange rows to match
tmp <- match(caphist.1.all[,1],tree[,1])

tree <- tree[tmp,]
identical(caphist.1.all[,1],tree[,1])

tree$percent.forest <- tree$AREA / area$Shape_Area

Occ.mat <- cbind(Occ.mat,tree$percent.forest)
colnames(Occ.mat)[5] <- "percent.forest"

##########################################################################

############      TREE DENSITY    ############################
#average tree density in Forested Habitat within each buffer. Forested Habitat was
#categorized by the LEMMA data Structural Conditions 3,4,5,6. Density is trees/ha


Occ.mat <- cbind(Occ.mat,tree$MEAN)
colnames(Occ.mat)[6] <- "tree.density"

###########################################################################

###########           % DEVELOPED LAND               ##########################################
#NLCD 2013 data. combined developed open spaces, low/medium/and high intensity developement
#going to remove roads using the NLCD Impervious Descriptor data to prevent overlap with road covariates

Devel <- read.csv("Covariate_files/NLDC_Developed_cells_bySite.csv")
Devel <- Devel[,-c(13:19)]

#some sites had no developement in them need to ID which and add them back in
tmp <- setdiff(caphist.1.all[,1],Devel$site_ID) 
tmp.df <- data.frame(site_ID=tmp,ZONE_CODE=NA,COUNT=0,AREA=0,MIN=NA,MAX=NA,RANGE=NA,MEAN=NA,
                     STD=NA,SUM=NA,VARIETY=NA,MAJORITY=NA)
Devel <- rbind(Devel,tmp.df)

tmp <- match(caphist.1.all[,1],Devel$site_ID) #rearrange records to match sites
Devel <- Devel[tmp,]
identical(caphist.1.all[,1],Devel[,"site_ID"])

#need road cell counts to remove count of road cells from each site
road <- read.csv("Covariate_files/Road_cells_bySite.csv")
road <- road[,-c(13:19)]

#some sites have no roads need to ID and add back in
tmp <- setdiff(caphist.1.all[,1],road$site_ID) 
tmp.df <- data.frame(site_ID=tmp,ZONE_CODE=NA,COUNT=0,AREA=0,MIN=NA,MAX=NA,RANGE=NA,MEAN=NA,
                     STD=NA,SUM=NA,VARIETY=NA,MAJORITY=NA)
road <- rbind(road,tmp.df)

tmp <- match(caphist.1.all[,1],road$site_ID) #rearrange records to match sites order
road <- road[tmp,]
identical(caphist.1.all[,1],road[,1]) #check to match sites
identical(road[,1],Devel[,1]) #check to match to make sure subtraction is aligned

#subtract area of road cells from area of developed land
Devel$remove.roads <- (Devel$AREA - road$AREA) / area$Shape_Area
range(Devel$remove.roads)

Occ.mat <- cbind(Occ.mat,Devel$remove.roads)
colnames(Occ.mat)[7] <- "percent.developed"

#################################################################################

####################      AMOUNT OF ROAD     ####################################
#using count of raster cells occupied by road rather than area of road.
road$percent.road <- road$COUNT / (area$Shape_Area / 1000000) 

Occ.mat <- cbind(Occ.mat,road$percent.road)
colnames(Occ.mat)[8] <- "road"

################################################################################

##################    % AGRICULTURAL LAND       ################################

Agri <- read.csv("Covariate_files/NLDC_Agri_cells_bySite.csv")
Agri <- Agri[,-c(13:19)]

#lot of sites do not have agriculture and need to ID and add them back in
tmp <- setdiff(caphist.1.all[,1],Agri$site_ID) 
tmp.df <- data.frame(site_ID=tmp,ZONE_CODE=NA,COUNT=0,AREA=0,MIN=NA,MAX=NA,RANGE=NA,MEAN=NA,
                     STD=NA,SUM=NA,VARIETY=NA,MAJORITY=NA)
Agri <- rbind(Agri,tmp.df)

tmp <- match(caphist.1.all[,1],Agri$site_ID) #reorder to match sites
Agri <- Agri[tmp,]

identical(caphist.1.all[,1],Agri$site_ID)

Agri$percent.agri <- Agri$AREA / area$Shape_Area

Occ.mat <- cbind(Occ.mat,Agri$percent.agri)
colnames(Occ.mat)[9] <- "percent.agri"

################################################################################
################################################################################
#Scale Covariates

Occ.scaled.mat <- Occ.mat
for (i in 2:ncol(Occ.mat)) {
  Occ.scaled.mat[,i] <- scale(Occ.mat[i])
}

#check for correlations
cor(Occ.scaled.mat[,2:9])
#percent forest and percent agriculture have high correlations with other covariates

#rearranging database to retain the correlated covariates but move them out of the way
Occ.scaled.mat <- Occ.scaled.mat[,c(2:4,6:9,5)]

#drop the last two covariates as they are not used
Occ.scaled.mat <- Occ.scaled.mat[,1:6]

#######################################################################################
#species matrix is used in the autoregressive function. columns are species: skunk, coyote, bobcat
# and rows are the community states. Indicates which species are present in each community state.
species.mat <- matrix(data=c(0,0,0,
                             1,0,0,
                             0,1,0,
                             0,0,1,
                             1,1,0,
                             1,0,1,
                             0,1,1,
                             1,1,1),
                      byrow=TRUE, nrow=8)



