#Generate activity patterns and estimate extent of overlap between each pairing of species.
#####################
#D Hubl
#2026
#####################
#dependencies
library(lubridate)
library(circular)
library(ggplot2)
library(overlap)
################################################################################
#data are the detection events of coyotes, bobcats, and spotted skunks at sites in this study

detections <- read.csv("Detection_event_times.csv")
detections <- detections[,-1] #removing un-needed column

#####################################################################################################################3
#######################################################################################################################
#removing detections that are within 30min of each to ensure independence of detections

#amount of time (minutes) between skunk detections
#a diff of 0 is the first detection at a unique hex, rep, station
skunk <- detections[detections$Species=="Spotted_skunk",]
skunk <- skunk[order(skunk$Hex_ID, skunk$Station_num,skunk$Date_Time),]

skunk$diff <- NA
skunk <- skunk[,c(1:8,27,9:26)]
HexID <- unique(skunk$Hex_ID)
for (i in 1:length(HexID)) {                                 #in each unique sample unit
  trgt <- HexID[i]
  n.rep <- length(unique(skunk[skunk$Hex_ID==trgt,"Rep"]))   #how many seasons was the sample unit sampled
  rep_num <- unique(skunk[skunk$Hex_ID==trgt,"Rep"])         
  for (r in 1:n.rep) {                                            #in each season 
    n.station <- length(unique(skunk[skunk$Hex_ID==trgt & skunk$Rep==rep_num[r],"Station_num"])) #how many stations were sampled
    hex.st <- unique(skunk[skunk$Hex_ID==trgt & skunk$Rep==rep_num[r],"Station_num"])
    for (k in 1:n.station) {                                              #in each station
      n.rec <- nrow(skunk[skunk$Hex_ID==trgt & skunk$Rep==rep_num[r] & skunk$Station_num==hex.st[k],]) #number of detections in that hex, in season r at station k
      row.rec <- which(skunk$Hex_ID==trgt & skunk$Rep==rep_num[r] & skunk$Station_num==hex.st[k])      #which records are they
      for (j in 1:n.rec) {
        if (j==1) { 
          skunk[row.rec[j],"diff"] <- 0    #the first record at the station in the season gets a time diff of 0
          
        }else{skunk[row.rec[j],"diff"] <- difftime(skunk[row.rec[j],"Date_Time"],skunk[row.rec[j-1],"Date_Time"], tz="UTC", units="mins") #number of minutes from the previous detection of the species at that station in that season
        
        } 
      } 
    }
  }  
}


#remove any record where diff is less than 30min
tmp <- which(skunk$diff>0 & skunk$diff<30) #12 records
skunk <- skunk[-tmp,]

#amount of time (minutes) between coyote detections
#a diff of 0 is the first detection at a unique hex, rep, station
coyote <- detections[detections$Species=="Coyote",]
coyote <- coyote[order(coyote$Hex_ID, coyote$Station_num,coyote$Date_Time),]
nrow(coyote) #416 coyote detection events
coyote$diff <- NA
coyote <- coyote[,c(1:8,27,9:26)]
HexID <- unique(coyote$Hex_ID)
for (i in 1:length(HexID)) {
  trgt <- HexID[i]
  n.rep <- length(unique(coyote[coyote$Hex_ID==trgt,"Rep"]))
  rep_num <- unique(coyote[coyote$Hex_ID==trgt,"Rep"])
  for (r in 1:n.rep) {
    n.station <- length(unique(coyote[coyote$Hex_ID==trgt & coyote$Rep==rep_num[r],"Station_num"]))
    hex.st <- unique(coyote[coyote$Hex_ID==trgt & coyote$Rep==rep_num[r],"Station_num"])
    for (k in 1:n.station) {
      n.rec <- nrow(coyote[coyote$Hex_ID==trgt & coyote$Rep==rep_num[r] & coyote$Station_num==hex.st[k],])
      row.rec <- which(coyote$Hex_ID==trgt & coyote$Rep==rep_num[r] & coyote$Station_num==hex.st[k])
      for (j in 1:n.rec) {
        if (j==1) { 
          coyote[row.rec[j],"diff"] <- 0
          
        }else{coyote[row.rec[j],"diff"] <- difftime(coyote[row.rec[j],"Date_Time"],coyote[row.rec[j-1],"Date_Time"], tz="UTC", units="mins")
        
        } 
      } 
    }
  }  
}

#no records to remove


#amount of time (minutes) between bobcat detections
#a diff of 0 is the first detection at a unique hex, rep, station
bobcat <- detections[detections$Species=="Bobcat",]
bobcat <- bobcat[order(bobcat$Hex_ID, bobcat$Station_num,bobcat$Date_Time),]
nrow(bobcat) #221 bobcat detection events
bobcat$diff <- NA
bobcat <- bobcat[,c(1:8,27,9:26)]
HexID <- unique(bobcat$Hex_ID)
for (i in 1:length(HexID)) {
  trgt <- HexID[i]
  n.rep <- length(unique(bobcat[bobcat$Hex_ID==trgt,"Rep"]))
  rep_num <- unique(bobcat[bobcat$Hex_ID==trgt,"Rep"])
  for (r in 1:n.rep) {
    n.station <- length(unique(bobcat[bobcat$Hex_ID==trgt & bobcat$Rep==rep_num[r],"Station_num"]))
    hex.st <- unique(bobcat[bobcat$Hex_ID==trgt & bobcat$Rep==rep_num[r],"Station_num"])
    for (k in 1:n.station) {
      n.rec <- nrow(bobcat[bobcat$Hex_ID==trgt & bobcat$Rep==rep_num[r] & bobcat$Station_num==hex.st[k],])
      row.rec <- which(bobcat$Hex_ID==trgt & bobcat$Rep==rep_num[r] & bobcat$Station_num==hex.st[k])
      for (j in 1:n.rec) {
        if (j==1) { 
          bobcat[row.rec[j],"diff"] <- 0
          
        }else{bobcat[row.rec[j],"diff"] <- difftime(bobcat[row.rec[j],"Date_Time"],bobcat[row.rec[j-1],"Date_Time"], tz="UTC", units="mins")
        
        } 
      } 
    }
  }  
}

#no records to remove


############################################################################################################
####################  Overlap Package ####################################
#convert time to radians to plot circular data
skunk$times_in_decimal = lubridate::hour(skunk$Date_Time) + lubridate::minute(skunk$Date_Time) / 60
skunk$times_in_radians = 2 * pi * (skunk$times_in_decimal / 24)

coyote$times_in_decimal = lubridate::hour(coyote$Date_Time) + lubridate::minute(coyote$Date_Time) / 60
coyote$times_in_radians = 2 * pi * (coyote$times_in_decimal / 24)

bobcat$times_in_decimal = lubridate::hour(bobcat$Date_Time) + lubridate::minute(bobcat$Date_Time) / 60
bobcat$times_in_radians = 2 * pi * (bobcat$times_in_decimal / 24)

skunk.rad <- skunk$times_in_radians    #4471 skunk observations
coyote.rad <- coyote$times_in_radians  #416 coyote observations
bobcat.rad <- bobcat$times_in_radians  #221 bobcat observations

#help manual says Dhat4 is the best to use because the smallest sample has over 50 observations
dhat.sc <- overlapEst(skunk.rad,coyote.rad)
dhat.sb <- overlapEst(skunk.rad,bobcat.rad)
dhat.cb <- overlapEst(coyote.rad,bobcat.rad)

#bootstrap the 95% CI for Dhat4 values. Results recorded in chunk below
bs1 <- bootstrap(skunk.rad,coyote.rad,nb=500,type="Dhat4",cores = 1)
bs2 <- bootstrap(skunk.rad,bobcat.rad,nb=500,type="Dhat4",cores=1)
bs3 <- bootstrap(coyote.rad,bobcat.rad,nb=500,type="Dhat4",cores=1)

bootCI(dhat.sc[2],bs1)[2,] #  skunk & Coyote  0.635 (0.592, 0.678)
bootCI(dhat.sb[2],bs2)[2,] #  skunk & bobcat  0.588 (0.529, 0.648)
bootCI(dhat.cb[2],bs3)[2,] #  coyote & bobcat 0.883 (0.823, 0.943)

##########################################################################
##########################################################################
##################     PLOT                         ######################
##################                                 #######################
##########################################################################

s_c <- overlapPlot(skunk.rad,coyote.rad,xcenter = "midnight")   #skunk and coyote overlap
s_b <- overlapPlot(skunk.rad,bobcat.rad,xcenter = "midnight")   #skunk and bobcat overlap

#pdf("Figures/Temporal Overlap_midnight.pdf", height = 6, width = 8)
png("Figures/Temporal_Overlap_midnight_title.png",units = "in", height = 6, width = 8,res = 300)
par(family="serif")
plot(x=s_c$x, y=s_c$densityA, type="l",col="blue",xlab="",ylab = "",xaxt="n",cex.axis=1.25,family="serif")        #skunk
polygon(c(s_c$x,rev(s_c$x)),c(s_c$densityA,rev(rep(0,length(s_c$x)))),
        col= adjustcolor("cornflowerblue", 0.3),lty=0)
axis(side = 1, at=c(-12,-6,0,6,12),labels = c("12:00","18:00","00:00","06:00","12:00"),
     cex.axis=1.25,family="serif")
mtext("Density of Activity",side=2,line=2.4,cex=1.5,family="serif")
mtext("Time",side=1,line=2.4,cex=1.5,family="serif")

lines(x=s_c$x, y=s_c$densityB,col="red")       #coyote
polygon(c(s_c$x,rev(s_c$x)),c(s_c$densityB,rev(rep(0,length(s_c$x)))),
        col= adjustcolor("firebrick1", 0.3),lty=0)

lines(x=s_c$x, y=s_b$densityB,col="darkgreen") #bobcat
polygon(c(s_c$x,rev(s_c$x)),c(s_b$densityB,rev(rep(0,length(s_c$x)))),
        col= adjustcolor("darkolivegreen", 0.3),lty=0)
legend("topleft",inset=c(.0125,.02),legend = c("Western Spotted Skunk","Coyote","Bobcat"),
       lwd=2,col = c("blue","red","darkgreen"),cex=1.15)

dev.off()
##########################################################################
##########################################################################
#######     just look at the main skunk activity period           ########
##########################################################################
##########################################################################
#closest skunk x axis values to 18:00
s_c$x[32]  
s_c$x[33]
#what was density at those times
s_c$densityA[32]
s_c$densityA[33]

#closest approximation to that density on the other side of the peak
s_c$densityA[104]
s_c$densityA[105]
#occurs at
s_c$x[104] #7.464567
s_c$x[105] #7.653543

#look only at photos taken between 18:00 and 07:30

##########################################################################
#read back in data to start fresh

detections <- read.csv("Detection_event_times.csv")
detections <- detections[,-1] #removing un-needed column

detections$Date_Time <- as.POSIXct(paste0(detections$Start_Date," ",detections$Start_Time), format = "%m/%d/%Y %I:%M:%S %p", tz = "UTC")

########################################
#remove day time photos
a <- "07:30:00 AM"
b <- "06:00:00 PM"
detections$time.a <- as.POSIXct(paste0(detections$Start_Date," ",a), format = "%m/%d/%Y %I:%M:%S %p", tz = "UTC")
detections$time.b <- as.POSIXct(paste0(detections$Start_Date," ",b), format = "%m/%d/%Y %I:%M:%S %p", tz = "UTC") 
detections$time.int <- interval(detections$time.a,detections$time.b)

tmp <- which(detections$Date_Time %within% detections$time.int)

detections <- detections[-tmp,]

#####################################################################################################################3
#######################################################################################################################
#removing detections that are within 30min of each to ensure independence of detections

#amount of time (minutes) between skunk detections
#a diff of 0 is the first detection at a unique hex, rep, station
skunk <- detections[detections$Species=="Spotted_skunk",]
skunk <- skunk[order(skunk$Hex_ID, skunk$Station_num,skunk$Date_Time),]

skunk$diff <- NA
skunk <- skunk[,c(1:8,27,9:26)]
HexID <- unique(skunk$Hex_ID)
for (i in 1:length(HexID)) {
  trgt <- HexID[i]
  n.rep <- length(unique(skunk[skunk$Hex_ID==trgt,"Rep"]))
  rep_num <- unique(skunk[skunk$Hex_ID==trgt,"Rep"])
  for (r in 1:n.rep) {
    n.station <- length(unique(skunk[skunk$Hex_ID==trgt & skunk$Rep==rep_num[r],"Station_num"]))
    hex.st <- unique(skunk[skunk$Hex_ID==trgt & skunk$Rep==rep_num[r],"Station_num"])
    for (k in 1:n.station) {
      n.rec <- nrow(skunk[skunk$Hex_ID==trgt & skunk$Rep==rep_num[r] & skunk$Station_num==hex.st[k],])
      row.rec <- which(skunk$Hex_ID==trgt & skunk$Rep==rep_num[r] & skunk$Station_num==hex.st[k])
      for (j in 1:n.rec) {
        if (j==1) { 
          skunk[row.rec[j],"diff"] <- 0
          
        }else{skunk[row.rec[j],"diff"] <- difftime(skunk[row.rec[j],"Date_Time"],skunk[row.rec[j-1],"Date_Time"], tz="UTC", units="mins")
        
        } 
      } 
    }
  }  
}


#remove any record where diff is less than 30min
table(skunk$diff)
tmp <- which(skunk$diff>0 & skunk$diff<30) #12 records
skunk <- skunk[-tmp,]  #4416 nocturnal WSS detection events

#amount of time (minutes) between coyote detections
#a diff of 0 is the first detection at a unique hex, rep, station
coyote <- detections[detections$Species=="Coyote",]
coyote <- coyote[order(coyote$Hex_ID, coyote$Station_num,coyote$Date_Time),]
nrow(coyote) #284 nocturnal coyote detection events
coyote$diff <- NA
coyote <- coyote[,c(1:8,27,9:26)]
HexID <- unique(coyote$Hex_ID)
for (i in 1:length(HexID)) {
  trgt <- HexID[i]
  n.rep <- length(unique(coyote[coyote$Hex_ID==trgt,"Rep"]))
  rep_num <- unique(coyote[coyote$Hex_ID==trgt,"Rep"])
  for (r in 1:n.rep) {
    n.station <- length(unique(coyote[coyote$Hex_ID==trgt & coyote$Rep==rep_num[r],"Station_num"]))
    hex.st <- unique(coyote[coyote$Hex_ID==trgt & coyote$Rep==rep_num[r],"Station_num"])
    for (k in 1:n.station) {
      n.rec <- nrow(coyote[coyote$Hex_ID==trgt & coyote$Rep==rep_num[r] & coyote$Station_num==hex.st[k],])
      row.rec <- which(coyote$Hex_ID==trgt & coyote$Rep==rep_num[r] & coyote$Station_num==hex.st[k])
      for (j in 1:n.rec) {
        if (j==1) { 
          coyote[row.rec[j],"diff"] <- 0
          
        }else{coyote[row.rec[j],"diff"] <- difftime(coyote[row.rec[j],"Date_Time"],coyote[row.rec[j-1],"Date_Time"], tz="UTC", units="mins")
        
        } 
      } 
    }
  }  
}

#no records to remove


#amount of time (minutes) between bobcat detections
#a diff of 0 is the first detection at a unique hex, rep, station
bobcat <- detections[detections$Species=="Bobcat",]
bobcat <- bobcat[order(bobcat$Hex_ID, bobcat$Station_num,bobcat$Date_Time),]
nrow(bobcat) #142 bobcat detection events
bobcat$diff <- NA
bobcat <- bobcat[,c(1:8,27,9:26)]
HexID <- unique(bobcat$Hex_ID)
for (i in 1:length(HexID)) {
  trgt <- HexID[i]
  n.rep <- length(unique(bobcat[bobcat$Hex_ID==trgt,"Rep"]))
  rep_num <- unique(bobcat[bobcat$Hex_ID==trgt,"Rep"])
  for (r in 1:n.rep) {
    n.station <- length(unique(bobcat[bobcat$Hex_ID==trgt & bobcat$Rep==rep_num[r],"Station_num"]))
    hex.st <- unique(bobcat[bobcat$Hex_ID==trgt & bobcat$Rep==rep_num[r],"Station_num"])
    for (k in 1:n.station) {
      n.rec <- nrow(bobcat[bobcat$Hex_ID==trgt & bobcat$Rep==rep_num[r] & bobcat$Station_num==hex.st[k],])
      row.rec <- which(bobcat$Hex_ID==trgt & bobcat$Rep==rep_num[r] & bobcat$Station_num==hex.st[k])
      for (j in 1:n.rec) {
        if (j==1) { 
          bobcat[row.rec[j],"diff"] <- 0
          
        }else{bobcat[row.rec[j],"diff"] <- difftime(bobcat[row.rec[j],"Date_Time"],bobcat[row.rec[j-1],"Date_Time"], tz="UTC", units="mins")
        
        } 
      } 
    }
  }  
}

#no records to remove

####################  Overlap Package ####################################
#convert time to radians to plot circular data
skunk$times_in_decimal = lubridate::hour(skunk$Date_Time) + lubridate::minute(skunk$Date_Time) / 60
skunk$times_in_radians = 2 * pi * (skunk$times_in_decimal / 24)

coyote$times_in_decimal = lubridate::hour(coyote$Date_Time) + lubridate::minute(coyote$Date_Time) / 60
coyote$times_in_radians = 2 * pi * (coyote$times_in_decimal / 24)

bobcat$times_in_decimal = lubridate::hour(bobcat$Date_Time) + lubridate::minute(bobcat$Date_Time) / 60
bobcat$times_in_radians = 2 * pi * (bobcat$times_in_decimal / 24)

skunk.rad <- skunk$times_in_radians    #4416 skunk observations
coyote.rad <- coyote$times_in_radians  #284 coyote observations
bobcat.rad <- bobcat$times_in_radians  #142 bobcat observations

#help manual says Dhat4 is the best to use because the smallest sample has over 50 observations
dhat.sc <- overlapEst(skunk.rad,coyote.rad)
dhat.sb <- overlapEst(skunk.rad,bobcat.rad)
dhat.cb <- overlapEst(coyote.rad,bobcat.rad)

#bootstrap the 95% CI for Dhat4 values. Results recorded in chunk below
bs1 <- bootstrap(skunk.rad,coyote.rad,nb=500,type="Dhat4",cores = 1)
bs2 <- bootstrap(skunk.rad,bobcat.rad,nb=500,type="Dhat4",cores=1)
bs3 <- bootstrap(coyote.rad,bobcat.rad,nb=500,type="Dhat4",cores=1)

bootCI(dhat.sc[2],bs1)[2,] #  skunk & Coyote  0.866 (0.822, 0.909)
bootCI(dhat.sb[2],bs2)[2,] #  skunk & bobcat  0.798 (0.726, 0.871)


##########################################################################
##########################################################################
##################     PLOT                         ######################
##################                                 #######################
##########################################################################

s_c <- overlapPlot(skunk.rad,coyote.rad,xcenter = "midnight")  
s_b <- overlapPlot(skunk.rad,bobcat.rad,xcenter = "midnight")

identical(s_c$x,s_b$x)

#pdf("Figures/Temporal Overlap_midnight.pdf", height = 6, width = 8)
#png("Figures/Temporal_Overlap_midnight.png",units = "in", height = 6, width = 8,res = 300)
par(family="serif")
plot(x=s_c$x, y=s_c$densityA, type="l",col="blue",xlab="",ylab = "",xaxt="n",cex.axis=1.25,family="serif")        #skunk
polygon(c(s_c$x,rev(s_c$x)),c(s_c$densityA,rev(rep(0,length(s_c$x)))),
        col= adjustcolor("cornflowerblue", 0.3),lty=0)
axis(side = 1, at=c(-12,-6,0,6,12),labels = c("12:00","18:00","00:00","06:00","12:00"),
     cex.axis=1.25,family="serif")
mtext("Density of Activity",side=2,line=2.4,cex=1.5,family="serif")
mtext("Time",side=1,line=2.4,cex=1.5,family="serif")

lines(x=s_c$x, y=s_c$densityB,col="red")       #coyote
polygon(c(s_c$x,rev(s_c$x)),c(s_c$densityB,rev(rep(0,length(s_c$x)))),
        col= adjustcolor("firebrick1", 0.3),lty=0)

lines(x=s_c$x, y=s_b$densityB,col="darkgreen") #bobcat
polygon(c(s_c$x,rev(s_c$x)),c(s_b$densityB,rev(rep(0,length(s_c$x)))),
        col= adjustcolor("darkolivegreen", 0.3),lty=0)
legend("topleft",inset=c(.0125,.015),legend = c("Spotted Skunk","Coyote","Bobcat"),
       lwd=2,col = c("blue","red","darkgreen"),cex=1.2)


dev.off()
######################################################3####################

