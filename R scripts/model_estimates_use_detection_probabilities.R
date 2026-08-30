#Calculate naive use and model estimated marginal use and average detection probability of each species
#run "covariate_and_caphist_prep.R" first
###############################################################################
#D Hubl
#2026

###############################################################################

#Season 1 detection history of each site 
detect.mat <- matrix(as.numeric(caphist.full.all[,,1]),nrow=712,ncol=8)

#which community states have each species
skunk <- c(2,5,6,8)
coyote <- c(3,5,7,8)
bobcat <- c(4,6,7,8)

#empty vectors for loop below
s.sites <- rep(0,712)
c.sites <- rep(0,712)
b.sites <- rep(0,712)
##################################################################
#Naive use
#Look at every survey at the site and determine what species were detected at the site
for (i in 1:nrow(detect.mat)) {
  tmp <- detect.mat[i,]
  ifelse(sum(tmp %in% skunk)>0, #test
         s.sites[i] <- 1,       #if true
         s.sites[i] <- 0)       #if false
  
  ifelse(sum(tmp %in% coyote)>0,#test
         c.sites[i] <- 1,       #if true
         c.sites[i] <- 0)       #if false
  
  ifelse(sum(tmp %in% bobcat)>0,#test
         b.sites[i] <- 1,       #if true
         b.sites[i] <- 0)       #if false
}

sum(s.sites) / 712  #skunk naive occupancy 
sum(c.sites) / 712  #coyote naive occupancy 
sum(b.sites) / 712  #bobcate naive occupancy 

#############################################################################
#model estimated marginal use

#null model from NIMBLE
none <- readRDS("null_Interactions.rds")
#look at z estimates

length(colnames(none$samples$chain1))          #how many parameters were monitored
range(grep("z",colnames(none$samples$chain1))) #which parameters were the "z" latent state
cn <- colnames(none$samples$chain1)[34:1457] #names of z parameters

#chain 1
z.mat <- matrix(none$samples$chain1[,34:1457], nrow=90000, ncol=length(cn),)
colnames(z.mat) <- cn

#chain 2
z.mat2 <- matrix(none$samples$chain2[,34:1457], nrow=90000, ncol=length(cn),)
colnames(z.mat2) <- cn

#chain 3
z.mat3 <- matrix(none$samples$chain3[,34:1457], nrow=90000, ncol=length(cn),)
colnames(z.mat3) <- cn


#columns are sites, rows are mcmc iterations
#extract just season 1 z estimates
z.mat1 <- z.mat[,seq(from=1,to=1423,by=2)]
z.mat2 <- z.mat2[,seq(from=1,to=1423,by=2)]
z.mat3 <- z.mat3[,seq(from=1,to=1423,by=2)]

z1.mat <- rbind(z.mat1,z.mat2,z.mat3)
dim(z1.mat)
####################################################
####################################################
#this chunk will get the marginal use prob for each species for each of 
#the 270,000 MCMC iterations then can use quantiles to get mean and 95% CI

#every row of z1.mat is an MCMC iteration
#make dataframe to accept each species marginal occupancy estimate
mo.df <- matrix(NA,nrow=3,ncol=270000)

for (i in 1:ncol(mo.df)) {
  mo.df[1,i] <- length(which(z1.mat[i,] %in% skunk)) / 712
  mo.df[2,i] <- length(which(z1.mat[i,] %in% coyote)) / 712
  mo.df[3,i] <- length(which(z1.mat[i,] %in% bobcat)) / 712
}

#skunk marginal occupancy
quantile(mo.df[1,],c(0.025,0.5,0.975)) 

#coyote marginal occupancy
quantile(mo.df[2,],c(0.025,0.5,0.975)) 

#bobcat marginal occupancy
quantile(mo.df[3,],c(0.025,0.5,0.975)) 
################################################################################
###############################################################################
##########  REPEAT FOR SEASON 2
#Season 2 detection history of each site 
detect.mat <- matrix(as.numeric(caphist.full.all[,,2]),nrow=712,ncol=8)
#only 239 sites were actually surveyed in season 2
detect.mat <- detect.mat[1:239,]

#empty vectors for loop below
s.sites <- rep(0,239)
c.sites <- rep(0,239)
b.sites <- rep(0,239)
##################################################################
#Naive use
#Look at every survey at the site and determine what species were detected at the site
for (i in 1:nrow(detect.mat)) {
  tmp <- detect.mat[i,]
  ifelse(sum(tmp %in% skunk)>0, #test
         s.sites[i] <- 1,       #if true
         s.sites[i] <- 0)       #if false
  
  ifelse(sum(tmp %in% coyote)>0,#test
         c.sites[i] <- 1,       #if true
         c.sites[i] <- 0)       #if false
  
  ifelse(sum(tmp %in% bobcat)>0,#test
         b.sites[i] <- 1,       #if true
         b.sites[i] <- 0)       #if false
}

sum(s.sites) / 239  #skunk naive occupancy for Season 2 sites
sum(c.sites) / 239  #coyote naive occupancy for Season 2 sites
sum(b.sites) / 239  #bobcate naive occupancy for Season 2 sites

#############################################################################
#model estimated marginal use

#take just the season 2 z estimates
#chain 1
z.mat <- matrix(none$samples$chain1[,seq(from=35,to=1457,by=2)], nrow=90000, ncol=712)
colnames(z.mat) <- cn[seq(from=2,to=1424,by=2)]

#chain 2
z.mat2 <- matrix(none$samples$chain2[,seq(from=35,to=1457,by=2)], nrow=90000, ncol=712)
colnames(z.mat2) <- cn[seq(from=2,to=1424,by=2)]

#chain 3
z.mat3 <- matrix(none$samples$chain3[,seq(from=35,to=1457,by=2)], nrow=90000, ncol=712)
colnames(z.mat3) <- cn[seq(from=2,to=1424,by=2)]

#only want the first 239 sites for this season
#columns are sites, rows are mcmc iterations
#extract just season 1 z estimates
z.mat1 <- z.mat[,1:239]
z.mat2 <- z.mat2[,1:239]
z.mat3 <- z.mat3[,1:239]

z2.mat <- rbind(z.mat1,z.mat2,z.mat3)
dim(z2.mat)
####################################################
####################################################
#this chunk will get the marginal use prob for each species for each of 
#the 270,000 MCMC iterations then can use quantiles to get mean and 95% CI

#every row of z2.mat is an MCMC iteration
#make df to accept each species marginal occupancy estimate
mo.df <- matrix(NA,nrow=3,ncol=270000)

for (i in 1:ncol(mo.df)) {
  mo.df[1,i] <- length(which(z2.mat[i,] %in% skunk)) / 239
  mo.df[2,i] <- length(which(z2.mat[i,] %in% coyote)) / 239
  mo.df[3,i] <- length(which(z2.mat[i,] %in% bobcat)) / 239
}

#skunk marginal occupancy
quantile(mo.df[1,],c(0.025,0.5,0.975)) 

#coyote marginal occupancy
quantile(mo.df[2,],c(0.025,0.5,0.975)) 

#bobcat marginal occupancy
quantile(mo.df[3,],c(0.025,0.5,0.975)) 

######################################################################
######################################################################
########## Average Detection Probability #############################

#### just uses the intercept of the detection for each species. Assumes average 
#number of camera days and average moon illuminance

#create data frames to hold MCMC chain values
d.bobcat <- data.frame(pb0=rep(NA,270000))

d.coyote <- data.frame(pc0=rep(NA,270000))

d.skunk <- data.frame(ps0=rep(NA,270000))

#get all the mcmc values into dataframes

##### BOBCAT #####

#intercept
colnames(none$samples$chain1)[17]
d.bobcat$pb0 <- c(none$samples$chain1[,17],
                  none$samples$chain2[,17],
                  none$samples$chain3[,17])

######### COYOTE #############

#intercept
colnames(none$samples$chain1)[20]
d.coyote$pc0 <- c(none$samples$chain1[,20],
                  none$samples$chain2[,20],
                  none$samples$chain3[,20])

############# SKUNK #################

#intercept
colnames(none$samples$chain1)[26]
d.skunk$ps0 <- c(none$samples$chain1[,26],
                 none$samples$chain2[,26],
                 none$samples$chain3[,26])


##### Bobcat detection probability #####
#just the intercept (average conditions for both covariates)
#reported in the paper as average bobcat detection probability
b.int <- plogis(d.bobcat$pb0)
quantile(b.int,c(0.025,.5,0.975)) #point est and 95% CI



##### Coyote detection prediction #####
#just the intercept (average conditions for both covariates)
#reported in paper as average detection probability of coyotes
c.int <- plogis(d.coyote$pc0)
quantile(c.int,c(0.025,.5,0.975)) #point est and 95% CI


##### Skunk detection prediction #####
#just the intercept (average conditions for covariates)
#reported in paper as average detection probability of skunks
s.int <- plogis(d.skunk$ps0)
quantile(s.int,c(0.025,.5,0.975)) #point est and 95% CI

###############################################################################
#average number days cameras were active vs full survey period
stations

tmp <- which(stations$Camera_Days_Good == stations$Interval)
stations[tmp,]

length(tmp) / nrow(stations)


mean(stations$Camera_Days_Good)

#how many surveys detected skunks via hair but not cam
nrow(stations[stations$Spotted_skunk == 0 & stations$skunk_hair ==1,])

#in how many sites were skunks only detected via hair
skunk.hair <- stations[stations$Spotted_skunk == 0 & stations$skunk_hair ==1,]
unique(skunk.hair$Site_ID)

skunk.df <- data.frame(site = unique(skunk.hair$Site_ID),
                       camera = rep(NA, length(unique(skunk.hair$Site_ID))),
                       hair = rep(NA, length(unique(skunk.hair$Site_ID))))

for (i in 1:nrow(skunk.df)) {
  trgt = skunk.df[i,1]
  
  tmp = stations[stations$Site_ID == trgt,]
  skunk.df[i,"camera"] <- sum(tmp$Spotted_skunk)
  skunk.df[i,"hair"] <- sum(tmp$skunk_hair)
}

length(which(skunk.df$camera < 1 & skunk.df$hair >= 1))


########################################################################
#autoregressive function values
#create data frames to hold MCMC chain values
d.bobcat <- data.frame(b.auto=rep(NA,270000))

d.coyote <- data.frame(c.auto=rep(NA,270000))

d.skunk <- data.frame(s.auto=rep(NA,270000))

#get all the mcmc values into dataframes

##### BOBCAT #####

#intercept
colnames(none$samples$chain1)[21]
d.bobcat$b.auto <- c(none$samples$chain1[,21],
                  none$samples$chain2[,21],
                  none$samples$chain3[,21])

######### COYOTE #############

#intercept
colnames(none$samples$chain1)[22]
d.coyote$c.auto <- c(none$samples$chain1[,22],
                  none$samples$chain2[,22],
                  none$samples$chain3[,22])

############# SKUNK #################

#intercept
colnames(none$samples$chain1)[23]
d.skunk$s.auto <- c(none$samples$chain1[,23],
                 none$samples$chain2[,23],
                 none$samples$chain3[,23])

quantile(d.skunk$s.auto,c(0.025,.5,0.975)) #point est and 95% CI
quantile(d.coyote$c.auto,c(0.025,.5,0.975)) #point est and 95% CI
quantile(d.bobcat$b.auto,c(0.025,.5,0.975)) #point est and 95% CI
