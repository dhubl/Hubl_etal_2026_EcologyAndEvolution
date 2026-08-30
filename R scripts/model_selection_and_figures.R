#model selection and Results from top model
#this script creates the figures for marginal use and detection probabilities
#run "covariate_and_caphist_prep.R" first
#must have NIMBLE model runs saved as RDS files
###########################################
#D Hubl
#2026
###########################################
#read in model runs
full <- readRDS("Full_Interactions.rds")
flat <- readRDS("flat_Interactions.rds")
none <- readRDS("null_Interactions.rds")

full$WAIC 
flat$WAIC 
none$WAIC 
###########################################
###########################################

#remove unnecessary items from environment
rm(list=setdiff(ls(), c("camera.NIMBLE.det","moon.NIMBLE.det","Occ.scaled.mat","Occ.mat","none")))

#################################################################################
#################################################################################
#Creating figures showing how each species' marginal use probability is affected by the site covariates
#and how detection probabilities are affected by detection covariates

#following coding framework were used: 
#Larson, R. N., H. A. Sander, M. Fidino, J. L. Angstmann, S. Hayes Hursh, 
  #S. B. Magle, K. Moore, C. M. Salsbury, T. Stankowich, K. Tombs, L. Barczak, 
  #A. M. Davidge, D. Drake, L. Hartley, P. Reed Sanchez, A. Robey, T. Snyder, 
  #J. Williamson, and A. J. Zellmer. 2024. Patterns in tree squirrel co-occurrence 
  #vary with responses to local land cover in US cities. Urban Ecosystems. 
  #https://doi.org/10.1007/s11252-024-01581-7.

#Code example available in their supplemental material.

##############################
##############################
#create dataframe for each species that will hold the coefficient estimates
#for each covariate. Each chain in the model holds 90,000 estimates and there are 3 chains. 

bobcat <- data.frame(b0=rep(NA,270000),
                     dense=rep(NA,270000),
                     devel=rep(NA,270000),
                     edge=rep(NA,270000),
                     elev=rep(NA,270000),
                     precip=rep(NA,270000),
                     road=rep(NA,270000),
                     phi.b=rep(NA,270000))

coyote <- data.frame(c0=rep(NA,270000),
                     dense=rep(NA,270000),
                     devel=rep(NA,270000),
                     edge=rep(NA,270000),
                     elev=rep(NA,270000),
                     precip=rep(NA,270000),
                     road=rep(NA,270000),
                     phi.c=rep(NA,270000))

skunk <- data.frame(s0=rep(NA,270000),
                    dense=rep(NA,270000),
                    devel=rep(NA,270000),
                    edge=rep(NA,270000),
                    elev=rep(NA,270000),
                    precip=rep(NA,270000),
                    road=rep(NA,270000),
                    phi.s=rep(NA,270000))
##############################################################
#pulling chain values and put them in corresponding dataframe

#####  Bobcat  #####
#b0
colnames(none$sample$chain1)[7] #first line is checking to make sure I am pulling the right column of mcmc samples
bobcat$b0 <- c(none$samples$chain1[,7],
               none$samples$chain2[,7],
               none$samples$chain3[,7])


#dense
colnames(none$sample$chain1)[1]
bobcat$dense <- c(none$samples$chain1[,1],
                  none$samples$chain2[,1],
                  none$samples$chain3[,1])


#developed
colnames(none$sample$chain1)[2]
bobcat$devel <- c(none$samples$chain1[,2],
                  none$samples$chain2[,2],
                  none$samples$chain3[,2])

#edge
colnames(none$sample$chain1)[3]
bobcat$edge <- c(none$samples$chain1[,3],
                 none$samples$chain2[,3],
                 none$samples$chain3[,3])

#elev
colnames(none$sample$chain1)[4]
bobcat$elev <- c(none$samples$chain1[,4],
                 none$samples$chain2[,4],
                 none$samples$chain3[,4])

#precipitation
colnames(none$sample$chain1)[5]
bobcat$precip <-c(none$samples$chain1[,5],
                  none$samples$chain2[,5],
                  none$samples$chain3[,5])
#road
colnames(none$sample$chain1)[6]
bobcat$road <- c(none$samples$chain1[,6],
                 none$samples$chain2[,6],
                 none$samples$chain3[,6])

#autoregressive
colnames(none$sample$chain1)[21]
bobcat$phi.b <- c(none$samples$chain1[,21],
                  none$samples$chain2[,21],
                  none$samples$chain3[,21])

##### Coyote #####
#c0
colnames(none$sample$chain1)[14]
coyote$c0 <-c(none$samples$chain1[,14],
              none$samples$chain2[,14],
              none$samples$chain3[,14])

#dense
colnames(none$sample$chain1)[8]
coyote$dense <- c(none$samples$chain1[,8],
                  none$samples$chain2[,8],
                  none$samples$chain3[,8])

#developed
colnames(none$sample$chain1)[9]
coyote$devel <- c(none$samples$chain1[,9],
                  none$samples$chain2[,9],
                  none$samples$chain3[,9])

#edge
colnames(none$sample$chain1)[10]
coyote$edge <- c(none$samples$chain1[,10],
                 none$samples$chain2[,10],
                 none$samples$chain3[,10])

#elevation
colnames(none$sample$chain1)[11]
coyote$elev <- c(none$samples$chain1[,11],
                 none$samples$chain2[,11],
                 none$samples$chain3[,11])

#precipitation
colnames(none$sample$chain1)[12]
coyote$precip <- c(none$samples$chain1[,12],
                   none$samples$chain2[,12],
                   none$samples$chain3[,12])

#road
colnames(none$sample$chain1)[13]
coyote$road <- c(none$samples$chain1[,13],
                 none$samples$chain2[,13],
                 none$samples$chain3[,13])

#autoregressive
colnames(none$sample$chain1)[22]
coyote$phi.c <- c(none$samples$chain1[,22],
                  none$samples$chain2[,22],
                  none$samples$chain3[,22])

#####  Skunk  #####
#s0
colnames(none$sample$chain1)[33]
skunk$s0 <- c(none$samples$chain1[,33],
              none$samples$chain2[,33],
              none$samples$chain3[,33])

#density
colnames(none$sample$chain1)[27]
skunk$dense <- c(none$samples$chain1[,27],
                 none$samples$chain2[,27],
                 none$samples$chain3[,27])

#developed
colnames(none$sample$chain1)[28]
skunk$devel <- c(none$samples$chain1[,28],
                 none$samples$chain2[,28],
                 none$samples$chain3[,28])

#edge
colnames(none$sample$chain1)[29]
skunk$edge <- c(none$samples$chain1[,29],
                none$samples$chain2[,29],
                none$samples$chain3[,29])

#elevation
colnames(none$sample$chain1)[30]
skunk$elev <- c(none$samples$chain1[,30],
                none$samples$chain2[,30],
                none$samples$chain3[,30])
#precipitation
colnames(none$sample$chain1)[31]
skunk$precip <- c(none$samples$chain1[,31],
                  none$samples$chain2[,31],
                  none$samples$chain3[,31])

#road
colnames(none$sample$chain1)[32]
skunk$road <- c(none$samples$chain1[,32],
                none$samples$chain2[,32],
                none$samples$chain3[,32])

#autoregressive
colnames(none$sample$chain1)[23]
skunk$phi.s <- c(none$samples$chain1[,23],
                 none$samples$chain2[,23],
                 none$samples$chain3[,23])

################################################################################
#get the ranges of each covariate that was actually sampled
#evenly divide that range of scaled values to use for plotting
#I am dividing into 200 points

########## Tree Density ###########
den.mean <- mean(Occ.mat$tree.density) #trees/ha
den.sd <- sd(Occ.mat$tree.density)
tmp <- range(Occ.scaled.mat$tree.density)


cov.dense <- seq(from=tmp[1],to=tmp[2],length=200)      #range of scaled values that were sampled in the study 
label.dense <- (cov.dense * den.sd) + den.mean   #converting those scaled values back to real world values for x axis labels on graphs

######### Developed land % #############
devel.mean <- mean(Occ.mat$percent.developed) 
devel.sd <- sd(Occ.mat$percent.developed)     
tmp <- range(Occ.scaled.mat$percent.developed)


cov.devel <- seq(from=tmp[1],to=tmp[2],length=200)
label.devel <- (cov.devel * devel.sd) + devel.mean

########### Edge ###############
edge.mean <- mean(Occ.mat$edge.length)  
edge.sd <- sd(Occ.mat$edge.length)      
tmp <- range(Occ.scaled.mat$edge.length)

cov.edge <- seq(from=tmp[1],to=tmp[2],length=200)
label.edge <- (cov.edge * edge.sd) + edge.mean

############# elevation ############
elev.mean <- mean(Occ.mat$elevation)
elev.sd <- sd(Occ.mat$elevation)
tmp <- range(Occ.scaled.mat$elevation)

cov.elev <- seq(from=tmp[1],to=tmp[2],length=200)
label.elev <- (cov.elev * elev.sd) + elev.mean

#Precipitation
precip.mean <- mean(Occ.mat$precip_mm)
precip.sd <- sd(Occ.mat$precip_mm)
tmp <- range(Occ.scaled.mat$precip_mm)

cov.precip <- seq(from=tmp[1],to=tmp[2],length=200)
label.precip <- (cov.precip * precip.sd) + precip.mean

########## road ##################
road.mean <- mean(Occ.mat$road)
road.sd <- sd(Occ.mat$road)
tmp <- range(Occ.scaled.mat$road)

cov.road <- seq(from=tmp[1],to=tmp[2],length=200)
label.road <- (cov.road * road.sd) + road.mean

#intercept and Autoregressive
intercept <- rep(1,200)
autoreg <- rep(1,200)

covs <- cbind.data.frame(intercept, cov.dense, cov.devel, cov.edge, cov.elev, cov.precip, cov.road, autoreg)

#################################################################################
#################################################################################
############                                                          ###########
############     Occupancy Probabilities across Covariate Gradients   ###########
############                                                          ###########
#################################################################################
#################################################################################

#################### Tree Density ###############################

# create a matrix of log odds for each latent state 
# with each mcmc iteration's estimated coefficient and the range of scaled covariates 
# these first matrices are the numerator portions of the multinomial logit
# Holding all other covariates at their Mean (which is 0 when scaled)

#this process is repeated for each covariate

# state 1: no species present
# just a matrix of 1s
state.1 <- matrix(1,nrow=270000,ncol=200)

# State 2: just skunk
state.2 <- matrix(NA,nrow=270000,ncol=200)
state.2 <- exp(cbind(skunk$s0,skunk$dense) %*% t(cbind(covs$intercept,covs$cov.dense)))

# State 3: just coyote
state.3 <- matrix(NA,nrow=270000,ncol=200)
state.3 <- exp(cbind(coyote$c0,coyote$dense) %*% t(cbind(covs$intercept,covs$cov.dense)))

# State 4: just bobcat
state.4 <- matrix(NA,nrow=270000,ncol=200)
state.4 <- exp(cbind(bobcat$b0,bobcat$dense) %*% t(cbind(covs$intercept,covs$cov.dense)))

# State 5: skunk & coyote
state.5 <- matrix(NA,nrow=270000,ncol=200)
state.5 <- exp((cbind(skunk$s0,skunk$dense) %*% t(cbind(covs$intercept,covs$cov.dense))) +
                 (cbind(coyote$c0,coyote$dense) %*% t(cbind(covs$intercept,covs$cov.dense))))

# State 6: skunk & bobcat
state.6 <- matrix(NA,nrow=270000,ncol=200)
state.6 <- exp((cbind(skunk$s0,skunk$dense) %*% t(cbind(covs$intercept,covs$cov.dense))) +
                 (cbind(bobcat$b0,bobcat$dense) %*% t(cbind(covs$intercept,covs$cov.dense))))

# State 7: coyote & bobcat
state.7 <- matrix(NA,nrow=270000,ncol=200)
state.7 <- exp((cbind(coyote$c0,coyote$dense) %*% t(cbind(covs$intercept,covs$cov.dense))) + 
                 (cbind(bobcat$b0,bobcat$dense) %*% t(cbind(covs$intercept,covs$cov.dense))))

# State 8: skunk, coyote & bobcat
state.8 <- matrix(NA,nrow=270000,ncol=200)
state.8 <- exp((cbind(skunk$s0,skunk$dense) %*% t(cbind(covs$intercept,covs$cov.dense))) +
                 (cbind(coyote$c0,coyote$dense) %*% t(cbind(covs$intercept,covs$cov.dense))) +
                 (cbind(bobcat$b0,bobcat$dense) %*% t(cbind(covs$intercept,covs$cov.dense))))

density.lo.array <- array(c(state.1, state.2, state.3, state.4, state.5, state.6, state.7, state.8),
                          dim = c(270000, 200, 8))

#now use multinomial logit to transition from log odds to probabilities
density.prob.array <- array(NA, dim=c(270000,200,8))

for(i in 1:nrow(covs)){
  density.prob.array[,i,] <- sweep(
    density.lo.array[,i,],
    1,
    rowSums(density.lo.array[,i,]),
    FUN = "/"                        #divide each position in a row by the sum of its row
  )
}

#example of what the above for loop is doing

head(density.lo.array[,1,])        
#the first column of each latent state matrix
#each row are the numerators of the multinomial logit link, that are produced using that column's covariate gradient value and the row's mcmc iteration's coefficient estimates

head(rowSums(density.lo.array[,1,]))    
#the denominators of each row's multinomial logit

density.lo.array[1,1,] / rowSums(density.lo.array[,1,])[1] #showing how the loop above works, each item in a row is divided by the same denominator, and probabilities are generated
density.prob.array[1,1,]                                   #should be the same
sum(density.prob.array[1,1,])                              #probabilities of any particular cell in all pages of array should sum to 1


##### Now need to repeat but include the autoregressive coefficient in the equations

# state 1: no species present
# just a matrix of 1s
state.1 <- matrix(1,nrow=270000,ncol=200)

# State 2: just skunk
state.2 <- matrix(NA,nrow=270000,ncol=200)
state.2 <- exp(cbind(skunk$s0,skunk$dense,skunk$phi.s) %*% t(cbind(covs$intercept,covs$cov.dense,covs$autoreg)))

# State 3: just coyote
state.3 <- matrix(NA,nrow=270000,ncol=200)
state.3 <- exp(cbind(coyote$c0,coyote$dense,coyote$phi.c) %*% t(cbind(covs$intercept,covs$cov.dense,covs$autoreg)))

# State 4: just bobcat
state.4 <- matrix(NA,nrow=270000,ncol=200)
state.4 <- exp(cbind(bobcat$b0,bobcat$dense,bobcat$phi.b) %*% t(cbind(covs$intercept,covs$cov.dense,covs$autoreg)))

# State 5: skunk & coyote
state.5 <- matrix(NA,nrow=270000,ncol=200)
state.5 <- exp((cbind(skunk$s0,skunk$dense,skunk$phi.s) %*% t(cbind(covs$intercept,covs$cov.dense,covs$autoreg))) +
                 (cbind(coyote$c0,coyote$dense,coyote$phi.c) %*% t(cbind(covs$intercept,covs$cov.dense,covs$autoreg))))

# State 6: skunk & bobcat
state.6 <- matrix(NA,nrow=270000,ncol=200)
state.6 <- exp((cbind(skunk$s0,skunk$dense,skunk$phi.s) %*% t(cbind(covs$intercept,covs$cov.dense,covs$autoreg))) +
                 (cbind(bobcat$b0,bobcat$dense,bobcat$phi.b) %*% t(cbind(covs$intercept,covs$cov.dense,covs$autoreg))))

# State 7: coyote & bobcat
state.7 <- matrix(NA,nrow=270000,ncol=200)
state.7 <- exp((cbind(coyote$c0,coyote$dense,coyote$phi.c) %*% t(cbind(covs$intercept,covs$cov.dense,covs$autoreg))) + 
                 (cbind(bobcat$b0,bobcat$dense,bobcat$phi.b) %*% t(cbind(covs$intercept,covs$cov.dense,covs$autoreg))))

# State 8: skunk, coyote & bobcat
state.8 <- matrix(NA,nrow=270000,ncol=200)
state.8 <- exp((cbind(skunk$s0,skunk$dense,skunk$phi.s) %*% t(cbind(covs$intercept,covs$cov.dense,covs$autoreg))) +
                 (cbind(coyote$c0,coyote$dense,coyote$phi.c) %*% t(cbind(covs$intercept,covs$cov.dense,covs$autoreg))) +
                 (cbind(bobcat$b0,bobcat$dense,bobcat$phi.b) %*% t(cbind(covs$intercept,covs$cov.dense,covs$autoreg))))

density.auto.lo.array <- array(c(state.1, state.2, state.3, state.4, state.5, state.6, state.7, state.8),
                               dim = c(270000, 200, 8))

#now use multinomial logit to transition from log odds to probabilities
density.auto.prob.array <- array(NA, dim = c(270000, 200, 8))

for(i in 1:nrow(covs)){
  density.auto.prob.array[,i,] <- sweep(
    density.auto.lo.array[,i,],
    1,
    rowSums(density.auto.lo.array[,i,]),
    FUN = "/"
  )
}

#####
#get the equilibrium of probability 

true.density.prob <- density.prob.array / (
  density.prob.array + (1 - density.auto.prob.array)
)

#for every value of the covariate gradient, get the average probability and 95%CI from the 90,000 mcmc iterations
dense.quantiles <- array(NA,dim=c(3,200,8))
for (i in 1:8) {
  for (j in 1:200) {
    dense.quantiles[,j,i] <- quantile(true.density.prob[,j,i] , c(.025,.5,.975)) #get the point estimate and the 95%CI for probabilities 
  }                                                                              #of each state under each covariate value 
}

###########
#### Marginal Occupancy Probabilities ####
marg.dense.bob <- dense.quantiles[2,,4]+dense.quantiles[2,,6]+dense.quantiles[2,,7]+dense.quantiles[2,,8] #add probability of each latent state that contains the species
marg.dense.coy <- dense.quantiles[2,,3]+dense.quantiles[2,,5]+dense.quantiles[2,,7]+dense.quantiles[2,,8]
marg.dense.skunk <- dense.quantiles[2,,2]+dense.quantiles[2,,5]+dense.quantiles[2,,6]+dense.quantiles[2,,8]

#### Confidence Intervals ####
s.dense.low <- dense.quantiles[1,,2]+dense.quantiles[1,,5]+dense.quantiles[1,,6]+dense.quantiles[1,,8]  #add the upper and lower limits of CIs of each state to get the CI of marginal
s.dense.high <- dense.quantiles[3,,2]+dense.quantiles[3,,5]+dense.quantiles[3,,6]+dense.quantiles[3,,8]
c.dense.low <- dense.quantiles[1,,3]+dense.quantiles[1,,5]+dense.quantiles[1,,7]+dense.quantiles[1,,8]
c.dense.high <- dense.quantiles[3,,3]+dense.quantiles[3,,5]+dense.quantiles[3,,7]+dense.quantiles[3,,8]
b.dense.low <- dense.quantiles[1,,4]+dense.quantiles[1,,6]+dense.quantiles[1,,7]+dense.quantiles[1,,8]
b.dense.high <- dense.quantiles[3,,4]+dense.quantiles[3,,6]+dense.quantiles[3,,7]+dense.quantiles[3,,8]

s.dense.low[s.dense.low<0] <- 0   #bounding low and high CI between 0 and 1
s.dense.high[s.dense.high>1] <- 1
c.dense.low[c.dense.low<0] <- 0
c.dense.high[c.dense.high>1] <- 1
b.dense.low[b.dense.low<0] <- 0
b.dense.high[b.dense.high>1] <- 1
######## Density plots ######
#pdf(height=6,width=10,"Figures/Marginal Occupancy_Forest Density.pdf")
png("Figures/Marginal_Occupancy_Forest Density.png", units = "in", height=6, width=10, res=300)
par(mfrow=c(1,3),
    oma = c(5,5,2,1))
par(mar= c(0,.5,1,0))
par(family="serif")
par(cex.axis=1.35)
plot(x=cov.dense, y=dense.quantiles[2,,2], type = "l",,lty=2,col="red",ylim=c(0,1),xlab = "") #probability of latent state 2: skunk only
lines(x=cov.dense, y=dense.quantiles[2,,5], lty=2,col="blue")                                 #probability of latent state 5: skunk and coyote             
lines(x=cov.dense, y=dense.quantiles[2,,6], lty=2,col="green")                                #probability of latent state 6: skunk and bobcat
lines(x=cov.dense, y=dense.quantiles[2,,8], lty=2,col="gold")                                 #probability of latent state 8: all 3 species
lines(x=cov.dense, y=marg.dense.skunk)                                                        #marginal probability of use by WSS
polygon(c(cov.dense,rev(cov.dense)),c(s.dense.low,rev(s.dense.high)),
        col= adjustcolor("cornflowerblue", 0.3))
text(x=-.75,y=1,"Spotted Skunk",cex = 1.5)

plot(x=cov.dense, y=dense.quantiles[2,,3], type = "l",,lty=2,col="red",ylim=c(0,1),yaxt="n",ylab="",xlab="")
lines(x=cov.dense, y=dense.quantiles[2,,5], lty=2,col="blue")
lines(x=cov.dense, y=dense.quantiles[2,,7], lty=2,col="green")
lines(x=cov.dense, y=dense.quantiles[2,,8], lty=2,col="gold")
lines(x=cov.dense, y=marg.dense.coy)
polygon(c(cov.dense,rev(cov.dense)),c(c.dense.low,rev(c.dense.high)),
        col= adjustcolor("cornflowerblue", 0.3))
text(x=-1,y=1,"Coyote", cex = 1.5)

plot(x=cov.dense, y=dense.quantiles[2,,4], type = "l",,lty=2,col="red",ylim=c(0,1),yaxt="n",ylab = "",xlab="")
lines(x=cov.dense, y=dense.quantiles[2,,6], lty=2,col="blue")
lines(x=cov.dense, y=dense.quantiles[2,,7], lty=2,col="green")
lines(x=cov.dense, y=dense.quantiles[2,,8], lty=2,col="gold")
lines(x=cov.dense, y=marg.dense.bob)
polygon(c(cov.dense,rev(cov.dense)),c(b.dense.low,rev(b.dense.high)),
        col= adjustcolor("cornflowerblue", 0.3))
text(x=-1,y=1,"Bobcat",cex = 1.5)
mtext(outer=TRUE, side=1,line=3,text = "Forest Density",cex=1.3)
mtext(outer=TRUE, side=2,line=3,text = "Probability of Use",cex=1.3)
mtext(outer=TRUE, side=3,line=0,text = "Marginal use probability as a function of forest density",cex=1.3)
dev.off()

#show the beta estimates for this covariate
tmp <- grep("dens",colnames(none$samples$chain1)) 
none$summary$all.chains[tmp,]

#free up space
rm(list=c("density.auto.lo.array","density.auto.prob.array","density.lo.array","density.prob.array","true.density.prob"))

################################################################################
################################################################################
####################### Developed Area ################
#### Process is repeated for new covariate

# state 1: no species present
# just a matrix of 1s
state.1 <- matrix(1,nrow=270000,ncol=200)

# State 2: just skunk
state.2 <- matrix(NA,nrow=270000,ncol=200)
state.2 <- exp(cbind(skunk$s0,skunk$devel) %*% t(cbind(covs$intercept,covs$cov.devel)))

# State 3: just coyote
state.3 <- matrix(NA,nrow=270000,ncol=200)
state.3 <- exp(cbind(coyote$c0,coyote$devel) %*% t(cbind(covs$intercept,covs$cov.devel)))

# State 4: just bobcat
state.4 <- matrix(NA,nrow=270000,ncol=200)
state.4 <- exp(cbind(bobcat$b0,bobcat$devel) %*% t(cbind(covs$intercept,covs$cov.devel)))

# State 5: skunk & coyote
state.5 <- matrix(NA,nrow=270000,ncol=200)
state.5 <- exp((cbind(skunk$s0,skunk$devel) %*% t(cbind(covs$intercept,covs$cov.devel))) +
                 (cbind(coyote$c0,coyote$devel) %*% t(cbind(covs$intercept,covs$cov.devel))))

# State 6: skunk & bobcat
state.6 <- matrix(NA,nrow=270000,ncol=200)
state.6 <- exp((cbind(skunk$s0,skunk$devel) %*% t(cbind(covs$intercept,covs$cov.devel))) +
                 (cbind(bobcat$b0,bobcat$devel) %*% t(cbind(covs$intercept,covs$cov.devel))))

# State 7: coyote & bobcat
state.7 <- matrix(NA,nrow=270000,ncol=200)
state.7 <- exp((cbind(coyote$c0,coyote$devel) %*% t(cbind(covs$intercept,covs$cov.devel))) + 
                 (cbind(bobcat$b0,bobcat$devel) %*% t(cbind(covs$intercept,covs$cov.devel))))

# State 8: skunk, coyote & bobcat
state.8 <- matrix(NA,nrow=270000,ncol=200)
state.8 <- exp((cbind(skunk$s0,skunk$devel) %*% t(cbind(covs$intercept,covs$cov.devel))) +
                 (cbind(coyote$c0,coyote$devel) %*% t(cbind(covs$intercept,covs$cov.devel))) +
                 (cbind(bobcat$b0,bobcat$devel) %*% t(cbind(covs$intercept,covs$cov.devel))))

devel.lo.array <- array(c(state.1, state.2, state.3, state.4, state.5, state.6, state.7, state.8),
                        dim = c(270000, 200, 8))

#now use multinomial logit to transition from log odds to probabilities
devel.prob.array <- array(NA, dim=c(270000,200,8))

for(i in 1:nrow(covs)){
  devel.prob.array[,i,] <- sweep(
    devel.lo.array[,i,],
    1,
    rowSums(devel.lo.array[,i,]),
    FUN = "/"
  )
}

devel.lo.array[,1,]
rowSums(devel.lo.array[,1,])[1]

devel.lo.array[2,1,] / rowSums(devel.lo.array[,1,])[2]
devel.prob.array[2,1,]

##### Now need to repeat but include the autoregressive coefficient in the equation

# state 1: no species present
# just a matrix of 1s
state.1 <- matrix(1,nrow=270000,ncol=200)

# State 2: just skunk
state.2 <- matrix(NA,nrow=270000,ncol=200)
state.2 <- exp(cbind(skunk$s0,skunk$devel,skunk$phi.s) %*% t(cbind(covs$intercept,covs$cov.devel,covs$autoreg)))

# State 3: just coyote
state.3 <- matrix(NA,nrow=270000,ncol=200)
state.3 <- exp(cbind(coyote$c0,coyote$devel,coyote$phi.c) %*% t(cbind(covs$intercept,covs$cov.devel,covs$autoreg)))

# State 4: just bobcat
state.4 <- matrix(NA,nrow=270000,ncol=200)
state.4 <- exp(cbind(bobcat$b0,bobcat$devel,bobcat$phi.b) %*% t(cbind(covs$intercept,covs$cov.devel,covs$autoreg)))

# State 5: skunk & coyote
state.5 <- matrix(NA,nrow=270000,ncol=200)
state.5 <- exp((cbind(skunk$s0,skunk$devel,skunk$phi.s) %*% t(cbind(covs$intercept,covs$cov.devel,covs$autoreg))) +
                 (cbind(coyote$c0,coyote$devel,coyote$phi.c) %*% t(cbind(covs$intercept,covs$cov.devel,covs$autoreg))))

# State 6: skunk & bobcat
state.6 <- matrix(NA,nrow=270000,ncol=200)
state.6 <- exp((cbind(skunk$s0,skunk$devel,skunk$phi.s) %*% t(cbind(covs$intercept,covs$cov.devel,covs$autoreg))) +
                 (cbind(bobcat$b0,bobcat$devel,bobcat$phi.b) %*% t(cbind(covs$intercept,covs$cov.devel,covs$autoreg))))

# State 7: coyote & bobcat
state.7 <- matrix(NA,nrow=270000,ncol=200)
state.7 <- exp((cbind(coyote$c0,coyote$devel,coyote$phi.c) %*% t(cbind(covs$intercept,covs$cov.devel,covs$autoreg))) + 
                 (cbind(bobcat$b0,bobcat$devel,bobcat$phi.b) %*% t(cbind(covs$intercept,covs$cov.devel,covs$autoreg))))

# State 8: skunk, coyote & bobcat
state.8 <- matrix(NA,nrow=270000,ncol=200)
state.8 <- exp((cbind(skunk$s0,skunk$devel,skunk$phi.s) %*% t(cbind(covs$intercept,covs$cov.devel,covs$autoreg))) +
                 (cbind(coyote$c0,coyote$devel,coyote$phi.c) %*% t(cbind(covs$intercept,covs$cov.devel,covs$autoreg))) +
                 (cbind(bobcat$b0,bobcat$devel,bobcat$phi.b) %*% t(cbind(covs$intercept,covs$cov.devel,covs$autoreg))))

devel.auto.lo.array <- array(c(state.1, state.2, state.3, state.4, state.5, state.6, state.7, state.8),
                             dim = c(270000, 200, 8))

#now use multinomial logit to transition from log odds to probabilities
devel.auto.prob.array <- array(NA, dim = c(270000, 200, 8))

for(i in 1:nrow(covs)){
  devel.auto.prob.array[,i,] <- sweep(
    devel.auto.lo.array[,i,],
    1,
    rowSums(devel.auto.lo.array[,i,]),
    FUN = "/"
  )
}
#####
#get average probability   : the equilibrium probability 

true.devel.prob <- devel.prob.array / (
  devel.prob.array + (1 - devel.auto.prob.array)
)

devel.quantiles <- array(NA,dim=c(3,200,8))
for (i in 1:8) {
  for (j in 1:200) {
    devel.quantiles[,j,i] <- quantile(true.devel.prob[,j,i] , c(.025,.5,.975))
  }
}
###########
#### Marginal Occupancy Probabilities ####
marg.devel.bob <- devel.quantiles[2,,4]+devel.quantiles[2,,6]+devel.quantiles[2,,7]+devel.quantiles[2,,8]
marg.devel.coy <- devel.quantiles[2,,3]+devel.quantiles[2,,5]+devel.quantiles[2,,7]+devel.quantiles[2,,8]
marg.devel.skunk <- devel.quantiles[2,,2]+devel.quantiles[2,,5]+devel.quantiles[2,,6]+devel.quantiles[2,,8]

#### Confidence Intervals ####
s.devel.low <- devel.quantiles[1,,2]+devel.quantiles[1,,5]+devel.quantiles[1,,6]+devel.quantiles[1,,8]
s.devel.high <- devel.quantiles[3,,2]+devel.quantiles[3,,5]+devel.quantiles[3,,6]+devel.quantiles[3,,8]
c.devel.low <- devel.quantiles[1,,3]+devel.quantiles[1,,5]+devel.quantiles[1,,7]+devel.quantiles[1,,8]
c.devel.high <- devel.quantiles[3,,3]+devel.quantiles[3,,5]+devel.quantiles[3,,7]+devel.quantiles[3,,8]
b.devel.low <- devel.quantiles[1,,4]+devel.quantiles[1,,6]+devel.quantiles[1,,7]+devel.quantiles[1,,8]
b.devel.high <- devel.quantiles[3,,4]+devel.quantiles[3,,6]+devel.quantiles[3,,7]+devel.quantiles[3,,8]

s.devel.low[s.devel.low<0] <- 0
s.devel.high[s.devel.high>1] <- 1
c.devel.low[c.devel.low<0] <- 0
c.devel.high[c.devel.high>1] <- 1
b.devel.low[b.devel.low<0] <- 0
b.devel.high[b.devel.high>1] <- 1
######## Developed land plots ######
#pdf(height=6,width=10,"Figures/Marginal Occupancy_Percent Developed.pdf")
png("Figures/Marginal_Occupancy_PercentDeveloped.png", units = "in", height=6, width=10, res=300)
par(mfrow=c(1,3),
    oma = c(5,5,2,1))
par(mar= c(0,.5,1,0))
par(family="serif")
par(cex.axis=1.5)
plot(x=cov.devel, y=devel.quantiles[2,,2], type = "l",,lty=2,col="red",ylim=c(0,1))
lines(x=cov.devel, y=devel.quantiles[2,,5], lty=2,col="blue")
lines(x=cov.devel, y=devel.quantiles[2,,6], lty=2,col="green")
lines(x=cov.devel, y=devel.quantiles[2,,8], lty=2,col="gold")
lines(x=cov.devel, y=marg.devel.skunk)
polygon(c(cov.devel,rev(cov.devel)),c(s.devel.low,rev(s.devel.high)),
        col= adjustcolor("cornflowerblue", 0.3))
text(x=2.5,y=1.0175,"Spotted Skunk", cex=1.5)

plot(x=cov.devel, y=devel.quantiles[2,,3], type = "l",,lty=2,col="red",ylim=c(0,1),yaxt="n")
lines(x=cov.devel, y=devel.quantiles[2,,5], lty=2,col="blue")
lines(x=cov.devel, y=devel.quantiles[2,,7], lty=2,col="green")
lines(x=cov.devel, y=devel.quantiles[2,,8], lty=2,col="gold")
lines(x=cov.devel, y=marg.devel.coy)
polygon(c(cov.devel,rev(cov.devel)),c(c.devel.low,rev(c.devel.high)),
        col= adjustcolor("cornflowerblue", 0.3))
text(x=2,y=1.0175,"Coyote", cex=1.5)

plot(x=cov.devel, y=devel.quantiles[2,,4], type = "l",,lty=2,col="red",ylim=c(0,1),yaxt="n")
lines(x=cov.devel, y=devel.quantiles[2,,6], lty=2,col="blue")
lines(x=cov.devel, y=devel.quantiles[2,,7], lty=2,col="green")
lines(x=cov.devel, y=devel.quantiles[2,,8], lty=2,col="gold")
lines(x=cov.devel, y=marg.devel.bob)
polygon(c(cov.devel,rev(cov.devel)),c(b.devel.low,rev(b.devel.high)),
        col= adjustcolor("cornflowerblue", 0.3))
text(x=2,y=1.0175,"Bobcat", cex=1.5)

mtext(outer=TRUE, side=1,line=3,text = "% Developed Land", cex = 1.3)
mtext(outer=TRUE, side=2,line=3,text = "Probability of Use", cex = 1.3)
mtext(outer=TRUE, side=3,line=0,text = "Marginal use probability as a function of % developed land", cex = 1.3)
dev.off()
###

tmp <- grep("devel",colnames(none$samples$chain1))
none$summary$all.chains[tmp,]

rm(list=c("devel.auto.lo.array","devel.auto.prob.array","devel.lo.array","devel.prob.array","true.devel.prob"))

#######################################################################################
#################################################################################
############   Edge Length   #########################
#REPEAT FOR NEW COVARIATE

# state 1: no species present
# just a matrix of 1s
state.1 <- matrix(1,nrow=270000,ncol=200)

# State 2: just skunk
state.2 <- matrix(NA,nrow=270000,ncol=200)
state.2 <- exp(cbind(skunk$s0,skunk$edge) %*% t(cbind(covs$intercept,covs$cov.edge)))

# State 3: just coyote
state.3 <- matrix(NA,nrow=270000,ncol=200)
state.3 <- exp(cbind(coyote$c0,coyote$edge) %*% t(cbind(covs$intercept,covs$cov.edge)))

# State 4: just bobcat
state.4 <- matrix(NA,nrow=270000,ncol=200)
state.4 <- exp(cbind(bobcat$b0,bobcat$edge) %*% t(cbind(covs$intercept,covs$cov.edge)))

# State 5: skunk & coyote
state.5 <- matrix(NA,nrow=270000,ncol=200)
state.5 <- exp((cbind(skunk$s0,skunk$edge) %*% t(cbind(covs$intercept,covs$cov.edge))) +
                 (cbind(coyote$c0,coyote$edge) %*% t(cbind(covs$intercept,covs$cov.edge))))

# State 6: skunk & bobcat
state.6 <- matrix(NA,nrow=270000,ncol=200)
state.6 <- exp((cbind(skunk$s0,skunk$edge) %*% t(cbind(covs$intercept,covs$cov.edge))) +
                 (cbind(bobcat$b0,bobcat$edge) %*% t(cbind(covs$intercept,covs$cov.edge))))

# State 7: coyote & bobcat
state.7 <- matrix(NA,nrow=270000,ncol=200)
state.7 <- exp((cbind(coyote$c0,coyote$edge) %*% t(cbind(covs$intercept,covs$cov.edge))) + 
                 (cbind(bobcat$b0,bobcat$edge) %*% t(cbind(covs$intercept,covs$cov.edge))))

# State 8: skunk, coyote & bobcat
state.8 <- matrix(NA,nrow=270000,ncol=200)
state.8 <- exp((cbind(skunk$s0,skunk$edge) %*% t(cbind(covs$intercept,covs$cov.edge))) +
                 (cbind(coyote$c0,coyote$edge) %*% t(cbind(covs$intercept,covs$cov.edge))) +
                 (cbind(bobcat$b0,bobcat$edge) %*% t(cbind(covs$intercept,covs$cov.edge))))

edge.lo.array <- array(c(state.1, state.2, state.3, state.4, state.5, state.6, state.7, state.8),
                       dim = c(270000, 200, 8))

#now use multinomial logit to transition from log odds to probabilities
edge.prob.array <- array(NA, dim=c(270000,200,8))

for(i in 1:nrow(covs)){
  edge.prob.array[,i,] <- sweep(
    edge.lo.array[,i,],
    1,
    rowSums(edge.lo.array[,i,]),
    FUN = "/"
  )
}

edge.lo.array[,1,]
rowSums(edge.lo.array[,1,])[1]

edge.lo.array[2,1,] / rowSums(edge.lo.array[,1,])[2]
edge.prob.array[2,1,]

##### Now need to repeat but include the autoregressive coefficient in the equation

# state 1: no species present
# just a matrix of 1s
state.1 <- matrix(1,nrow=270000,ncol=200)

# State 2: just skunk
state.2 <- matrix(NA,nrow=270000,ncol=200)
state.2 <- exp(cbind(skunk$s0,skunk$edge,skunk$phi.s) %*% t(cbind(covs$intercept,covs$cov.edge,covs$autoreg)))

# State 3: just coyote
state.3 <- matrix(NA,nrow=270000,ncol=200)
state.3 <- exp(cbind(coyote$c0,coyote$edge,coyote$phi.c) %*% t(cbind(covs$intercept,covs$cov.edge,covs$autoreg)))

# State 4: just bobcat
state.4 <- matrix(NA,nrow=270000,ncol=200)
state.4 <- exp(cbind(bobcat$b0,bobcat$edge,bobcat$phi.b) %*% t(cbind(covs$intercept,covs$cov.edge,covs$autoreg)))

# State 5: skunk & coyote
state.5 <- matrix(NA,nrow=270000,ncol=200)
state.5 <- exp((cbind(skunk$s0,skunk$edge,skunk$phi.s) %*% t(cbind(covs$intercept,covs$cov.edge,covs$autoreg))) +
                 (cbind(coyote$c0,coyote$edge,coyote$phi.c) %*% t(cbind(covs$intercept,covs$cov.edge,covs$autoreg))))

# State 6: skunk & bobcat
state.6 <- matrix(NA,nrow=270000,ncol=200)
state.6 <- exp((cbind(skunk$s0,skunk$edge,skunk$phi.s) %*% t(cbind(covs$intercept,covs$cov.edge,covs$autoreg))) +
                 (cbind(bobcat$b0,bobcat$edge,bobcat$phi.b) %*% t(cbind(covs$intercept,covs$cov.edge,covs$autoreg))))

# State 7: coyote & bobcat
state.7 <- matrix(NA,nrow=270000,ncol=200)
state.7 <- exp((cbind(coyote$c0,coyote$edge,coyote$phi.c) %*% t(cbind(covs$intercept,covs$cov.edge,covs$autoreg))) + 
                 (cbind(bobcat$b0,bobcat$edge,bobcat$phi.b) %*% t(cbind(covs$intercept,covs$cov.edge,covs$autoreg))))

# State 8: skunk, coyote & bobcat
state.8 <- matrix(NA,nrow=270000,ncol=200)
state.8 <- exp((cbind(skunk$s0,skunk$edge,skunk$phi.s) %*% t(cbind(covs$intercept,covs$cov.edge,covs$autoreg))) +
                 (cbind(coyote$c0,coyote$edge,coyote$phi.c) %*% t(cbind(covs$intercept,covs$cov.edge,covs$autoreg))) +
                 (cbind(bobcat$b0,bobcat$edge,bobcat$phi.b) %*% t(cbind(covs$intercept,covs$cov.edge,covs$autoreg))))

edge.auto.lo.array <- array(c(state.1, state.2, state.3, state.4, state.5, state.6, state.7, state.8),
                            dim = c(270000, 200, 8))

#now use multinomial logit to transition from log odds to probabilities
edge.auto.prob.array <- array(NA, dim = c(270000, 200, 8))

for(i in 1:nrow(covs)){
  edge.auto.prob.array[,i,] <- sweep(
    edge.auto.lo.array[,i,],
    1,
    rowSums(edge.auto.lo.array[,i,]),
    FUN = "/"
  )
}
#####
#get average probability   : the equilibrium probability 
# calculating average psi
true.edge.prob <- edge.prob.array / (
  edge.prob.array + (1 - edge.auto.prob.array)
)

edge.quantiles <- array(NA,dim=c(3,200,8))
for (i in 1:8) {
  for (j in 1:200) {
    edge.quantiles[,j,i] <- quantile(true.edge.prob[,j,i] , c(.025,.5,.975))
  }
}
###########
#### Marginal Occupancy Probabilities ####
marg.edge.bob <- edge.quantiles[2,,4]+edge.quantiles[2,,6]+edge.quantiles[2,,7]+edge.quantiles[2,,8]
marg.edge.coy <- edge.quantiles[2,,3]+edge.quantiles[2,,5]+edge.quantiles[2,,7]+edge.quantiles[2,,8]
marg.edge.skunk <- edge.quantiles[2,,2]+edge.quantiles[2,,5]+edge.quantiles[2,,6]+edge.quantiles[2,,8]
#### Confidence Intervals ####
s.edge.low <- edge.quantiles[1,,2]+edge.quantiles[1,,5]+edge.quantiles[1,,6]+edge.quantiles[1,,8]
s.edge.high <- edge.quantiles[3,,2]+edge.quantiles[3,,5]+edge.quantiles[3,,6]+edge.quantiles[3,,8]
c.edge.low <- edge.quantiles[1,,3]+edge.quantiles[1,,5]+edge.quantiles[1,,7]+edge.quantiles[1,,8]
c.edge.high <- edge.quantiles[3,,3]+edge.quantiles[3,,5]+edge.quantiles[3,,7]+edge.quantiles[3,,8]
b.edge.low <- edge.quantiles[1,,4]+edge.quantiles[1,,6]+edge.quantiles[1,,7]+edge.quantiles[1,,8]
b.edge.high <- edge.quantiles[3,,4]+edge.quantiles[3,,6]+edge.quantiles[3,,7]+edge.quantiles[3,,8]

s.edge.low[s.edge.low<0] <- 0
s.edge.high[s.edge.high>1] <- 1
c.edge.low[c.edge.low<0] <- 0
c.edge.high[c.edge.high>1] <- 1
b.edge.low[b.edge.low<0] <- 0
b.edge.high[b.edge.high>1] <- 1
######## Edge Length plots ######
#pdf(height=6,width=10,"Figures/Marginal Occupancy_Edge Habitat.pdf")
png("Figures/Marginal_Occupancy_Edge Density.png", units = "in", height=6, width=10, res=300)
par(mfrow=c(1,3),
    oma = c(5,5,2,1))
par(mar= c(0,.5,1,0))
par(family="serif")
par(cex.axis=1.3)
plot(x=cov.edge, y=edge.quantiles[2,,2], type = "l",,lty=2,col="red",ylim=c(0,1))
lines(x=cov.edge, y=edge.quantiles[2,,5], lty=2,col="blue")
lines(x=cov.edge, y=edge.quantiles[2,,6], lty=2,col="green")
lines(x=cov.edge, y=edge.quantiles[2,,8], lty=2,col="gold")
lines(x=cov.edge, y=marg.edge.skunk)
polygon(c(cov.edge,rev(cov.edge)),c(s.edge.low,rev(s.edge.high)),
        col= adjustcolor("cornflowerblue", 0.3))
text(x=-.6,y=1.015,"Spotted Skunk",cex=1.5)

plot(x=cov.edge, y=edge.quantiles[2,,3], type = "l",,lty=2,col="red",ylim=c(0,1),yaxt="n")
lines(x=cov.edge, y=edge.quantiles[2,,5], lty=2,col="blue")
lines(x=cov.edge, y=edge.quantiles[2,,7], lty=2,col="green")
lines(x=cov.edge, y=edge.quantiles[2,,8], lty=2,col="gold")
lines(x=cov.edge, y=marg.edge.coy)
polygon(c(cov.edge,rev(cov.edge)),c(c.edge.low,rev(c.edge.high)),
        col= adjustcolor("cornflowerblue", 0.3))
text(x=-1,y=1.015,"Coyote",cex=1.5)

plot(x=cov.edge, y=edge.quantiles[2,,4], type = "l",,lty=2,col="red",ylim=c(0,1),yaxt="n")
lines(x=cov.edge, y=edge.quantiles[2,,6], lty=2,col="blue")
lines(x=cov.edge, y=edge.quantiles[2,,7], lty=2,col="green")
lines(x=cov.edge, y=edge.quantiles[2,,8], lty=2,col="gold")
lines(x=cov.edge, y=marg.edge.bob)
polygon(c(cov.edge,rev(cov.edge)),c(b.edge.low,rev(b.edge.high)),
        col= adjustcolor("cornflowerblue", 0.3))
text(x=-1,y=1.0175,"Bobcat",cex=1.5)
mtext(outer=TRUE, side=1,line=3,text = "Forest Edge Density",cex=1.3)
mtext(outer=TRUE, side=2,line=3,text = "Probability of Use",cex=1.3)
mtext(outer=TRUE, side=3,line=0,text = "Marginal use probability as a function of edge habitat",cex=1.3)
dev.off()

tmp <- grep("edge",colnames(none$samples$chain1))
none$summary$all.chains[tmp,]

rm(list=c("edge.auto.lo.array","edge.auto.prob.array","edge.lo.array","edge.prob.array","true.edge.prob"))
####################################################################################
####################################################################################
###### Elevation ############################
#REPEAT FOR NEW COVARIATE

# state 1: no species present
# just a matrix of 1s
state.1 <- matrix(1,nrow=270000,ncol=200)

# State 2: just skunk
state.2 <- matrix(NA,nrow=270000,ncol=200)
state.2 <- exp(cbind(skunk$s0,skunk$elev) %*% t(cbind(covs$intercept,covs$cov.elev)))

# State 3: just coyote
state.3 <- matrix(NA,nrow=270000,ncol=200)
state.3 <- exp(cbind(coyote$c0,coyote$elev) %*% t(cbind(covs$intercept,covs$cov.elev)))

# State 4: just bobcat
state.4 <- matrix(NA,nrow=270000,ncol=200)
state.4 <- exp(cbind(bobcat$b0,bobcat$elev) %*% t(cbind(covs$intercept,covs$cov.elev)))

# State 5: skunk & coyote
state.5 <- matrix(NA,nrow=270000,ncol=200)
state.5 <- exp((cbind(skunk$s0,skunk$elev) %*% t(cbind(covs$intercept,covs$cov.elev))) +
                 (cbind(coyote$c0,coyote$elev) %*% t(cbind(covs$intercept,covs$cov.elev))))

# State 6: skunk & bobcat
state.6 <- matrix(NA,nrow=270000,ncol=200)
state.6 <- exp((cbind(skunk$s0,skunk$elev) %*% t(cbind(covs$intercept,covs$cov.elev))) +
                 (cbind(bobcat$b0,bobcat$elev) %*% t(cbind(covs$intercept,covs$cov.elev))))

# State 7: coyote & bobcat
state.7 <- matrix(NA,nrow=270000,ncol=200)
state.7 <- exp((cbind(coyote$c0,coyote$elev) %*% t(cbind(covs$intercept,covs$cov.elev))) + 
                 (cbind(bobcat$b0,bobcat$elev) %*% t(cbind(covs$intercept,covs$cov.elev))))

# State 8: skunk, coyote & bobcat
state.8 <- matrix(NA,nrow=270000,ncol=200)
state.8 <- exp((cbind(skunk$s0,skunk$elev) %*% t(cbind(covs$intercept,covs$cov.elev))) +
                 (cbind(coyote$c0,coyote$elev) %*% t(cbind(covs$intercept,covs$cov.elev))) +
                 (cbind(bobcat$b0,bobcat$elev) %*% t(cbind(covs$intercept,covs$cov.elev))))

elev.lo.array <- array(c(state.1, state.2, state.3, state.4, state.5, state.6, state.7, state.8),
                       dim = c(270000, 200, 8))

#now use multinomial logit to transition from log odds to probabilities
elev.prob.array <- array(NA, dim=c(270000,200,8))

for(i in 1:nrow(covs)){
  elev.prob.array[,i,] <- sweep(
    elev.lo.array[,i,],
    1,
    rowSums(elev.lo.array[,i,]),
    FUN = "/"
  )
}

elev.lo.array[,1,]
rowSums(elev.lo.array[,1,])[1]

elev.lo.array[2,1,] / rowSums(elev.lo.array[,1,])[2]
elev.prob.array[2,1,]

##### Now need to repeat but include the autoregressive coefficient in the equation

# state 1: no species present
# just a matrix of 1s
state.1 <- matrix(1,nrow=270000,ncol=200)

# State 2: just skunk
state.2 <- matrix(NA,nrow=270000,ncol=200)
state.2 <- exp(cbind(skunk$s0,skunk$elev,skunk$phi.s) %*% t(cbind(covs$intercept,covs$cov.elev,covs$autoreg)))

# State 3: just coyote
state.3 <- matrix(NA,nrow=270000,ncol=200)
state.3 <- exp(cbind(coyote$c0,coyote$elev,coyote$phi.c) %*% t(cbind(covs$intercept,covs$cov.elev,covs$autoreg)))

# State 4: just bobcat
state.4 <- matrix(NA,nrow=270000,ncol=200)
state.4 <- exp(cbind(bobcat$b0,bobcat$elev,bobcat$phi.b) %*% t(cbind(covs$intercept,covs$cov.elev,covs$autoreg)))

# State 5: skunk & coyote
state.5 <- matrix(NA,nrow=270000,ncol=200)
state.5 <- exp((cbind(skunk$s0,skunk$elev,skunk$phi.s) %*% t(cbind(covs$intercept,covs$cov.elev,covs$autoreg))) +
                 (cbind(coyote$c0,coyote$elev,coyote$phi.c) %*% t(cbind(covs$intercept,covs$cov.elev,covs$autoreg))))

# State 6: skunk & bobcat
state.6 <- matrix(NA,nrow=270000,ncol=200)
state.6 <- exp((cbind(skunk$s0,skunk$elev,skunk$phi.s) %*% t(cbind(covs$intercept,covs$cov.elev,covs$autoreg))) +
                 (cbind(bobcat$b0,bobcat$elev,bobcat$phi.b) %*% t(cbind(covs$intercept,covs$cov.elev,covs$autoreg))))

# State 7: coyote & bobcat
state.7 <- matrix(NA,nrow=270000,ncol=200)
state.7 <- exp((cbind(coyote$c0,coyote$elev,coyote$phi.c) %*% t(cbind(covs$intercept,covs$cov.elev,covs$autoreg))) + 
                 (cbind(bobcat$b0,bobcat$elev,bobcat$phi.b) %*% t(cbind(covs$intercept,covs$cov.elev,covs$autoreg))))

# State 8: skunk, coyote & bobcat
state.8 <- matrix(NA,nrow=270000,ncol=200)
state.8 <- exp((cbind(skunk$s0,skunk$elev,skunk$phi.s) %*% t(cbind(covs$intercept,covs$cov.elev,covs$autoreg))) +
                 (cbind(coyote$c0,coyote$elev,coyote$phi.c) %*% t(cbind(covs$intercept,covs$cov.elev,covs$autoreg))) +
                 (cbind(bobcat$b0,bobcat$elev,bobcat$phi.b) %*% t(cbind(covs$intercept,covs$cov.elev,covs$autoreg))))

elev.auto.lo.array <- array(c(state.1, state.2, state.3, state.4, state.5, state.6, state.7, state.8),
                            dim = c(270000, 200, 8))

#now use multinomial logit to transition from log odds to probabilities
elev.auto.prob.array <- array(NA, dim = c(270000, 200, 8))

for(i in 1:nrow(covs)){
  elev.auto.prob.array[,i,] <- sweep(
    elev.auto.lo.array[,i,],
    1,
    rowSums(elev.auto.lo.array[,i,]),
    FUN = "/"
  )
}
#####
#get average probability   
# calculating average psi

true.elev.prob <- elev.prob.array / (
  elev.prob.array + (1 - elev.auto.prob.array)
)


elev.quantiles <- array(NA,dim=c(3,200,8))
for (i in 1:8) {
  for (j in 1:200) {
    elev.quantiles[,j,i] <- quantile(true.elev.prob[,j,i] , c(.025,.5,.975))
  }
}
###########
#### Marginal Occupancy Probabilities ####
marg.elev.bob <- elev.quantiles[2,,4]+elev.quantiles[2,,6]+elev.quantiles[2,,7]+elev.quantiles[2,,8]
marg.elev.coy <- elev.quantiles[2,,3]+elev.quantiles[2,,5]+elev.quantiles[2,,7]+elev.quantiles[2,,8]
marg.elev.skunk <- elev.quantiles[2,,2]+elev.quantiles[2,,5]+elev.quantiles[2,,6]+elev.quantiles[2,,8]
#### Confidence Intervals ####
s.elev.low <- elev.quantiles[1,,2]+elev.quantiles[1,,5]+elev.quantiles[1,,6]+elev.quantiles[1,,8]
s.elev.high <- elev.quantiles[3,,2]+elev.quantiles[3,,5]+elev.quantiles[3,,6]+elev.quantiles[3,,8]
c.elev.low <- elev.quantiles[1,,3]+elev.quantiles[1,,5]+elev.quantiles[1,,7]+elev.quantiles[1,,8]
c.elev.high <- elev.quantiles[3,,3]+elev.quantiles[3,,5]+elev.quantiles[3,,7]+elev.quantiles[3,,8]
b.elev.low <- elev.quantiles[1,,4]+elev.quantiles[1,,6]+elev.quantiles[1,,7]+elev.quantiles[1,,8]
b.elev.high <- elev.quantiles[3,,4]+elev.quantiles[3,,6]+elev.quantiles[3,,7]+elev.quantiles[3,,8]

s.elev.low[s.elev.low<0] <- 0
s.elev.high[s.elev.high>1] <- 1
c.elev.low[c.elev.low<0] <- 0
c.elev.high[c.elev.high>1] <- 1
b.elev.low[b.elev.low<0] <- 0
b.elev.high[b.elev.high>1] <- 1
######## elevation plots ######
#pdf(height=6,width=10,"Figures/Marginal Occupancy_Elevation.pdf")
png("Figures/Marginal_Occupancy_Elevation.png", units = "in", height=6, width=10, res=300)
par(mfrow=c(1,3),
    oma = c(5,5,2,1))
par(mar= c(0,.5,1,0))
par(family="serif")
par(cex.axis=1.3)
plot(x=cov.elev, y=elev.quantiles[2,,2], type = "l",,lty=2,col="red",ylim=c(0,1))
lines(x=cov.elev, y=elev.quantiles[2,,5], lty=2,col="blue")
lines(x=cov.elev, y=elev.quantiles[2,,6], lty=2,col="green")
lines(x=cov.elev, y=elev.quantiles[2,,8], lty=2,col="gold")
lines(x=cov.elev, y=marg.elev.skunk)
polygon(c(cov.elev,rev(cov.elev)),c(s.elev.low,rev(s.elev.high)),
        col= adjustcolor("cornflowerblue", 0.3))
text(x=-.35,y=1.0175,"Spotted Skunk", cex=1.5)

plot(x=cov.elev, y=elev.quantiles[2,,3], type = "l",,lty=2,col="red",ylim=c(0,1),yaxt="n")
lines(x=cov.elev, y=elev.quantiles[2,,5], lty=2,col="blue")
lines(x=cov.elev, y=elev.quantiles[2,,7], lty=2,col="green")
lines(x=cov.elev, y=elev.quantiles[2,,8], lty=2,col="gold")
lines(x=cov.elev, y=marg.elev.coy)
polygon(c(cov.elev,rev(cov.elev)),c(c.elev.low,rev(c.elev.high)),
        col= adjustcolor("cornflowerblue", 0.3))
text(x=-.75,y=1.0175,"Coyote", cex=1.5)

plot(x=cov.elev, y=elev.quantiles[2,,4], type = "l",,lty=2,col="red",ylim=c(0,1),yaxt="n")
lines(x=cov.elev, y=elev.quantiles[2,,6], lty=2,col="blue")
lines(x=cov.elev, y=elev.quantiles[2,,7], lty=2,col="green")
lines(x=cov.elev, y=elev.quantiles[2,,8], lty=2,col="gold")
lines(x=cov.elev, y=marg.elev.bob)
polygon(c(cov.elev,rev(cov.elev)),c(b.elev.low,rev(b.elev.high)),
        col= adjustcolor("cornflowerblue", 0.3))
text(x=-.75,y=1.0175,"Bobcat",cex=1.5)
mtext(outer=TRUE, side=1,line=3,text = "Elevation", cex=1.3)
mtext(outer=TRUE, side=2,line=3,text = "Probability of Use", cex=1.3)
mtext(outer=TRUE, side=3,line=0,text = "Marginal use probability as a function of elevation", cex=1.3)
dev.off()
###

tmp <- grep("elev",colnames(none$samples$chain1))
none$summary$all.chains[tmp,]

rm(list=c("elev.auto.lo.array","elev.auto.prob.array","elev.lo.array","elev.prob.array","true.elev.prob"))
###################################################################################
###################################################################################
######### Precipitation ############################
#REPEAT FOR NEW COVARIATE

# state 1: no species present
# just a matrix of 1s
state.1 <- matrix(1,nrow=270000,ncol=200)

# State 2: just skunk
state.2 <- matrix(NA,nrow=270000,ncol=200)
state.2 <- exp(cbind(skunk$s0,skunk$precip) %*% t(cbind(covs$intercept,covs$cov.precip)))

# State 3: just coyote
state.3 <- matrix(NA,nrow=270000,ncol=200)
state.3 <- exp(cbind(coyote$c0,coyote$precip) %*% t(cbind(covs$intercept,covs$cov.precip)))

# State 4: just bobcat
state.4 <- matrix(NA,nrow=270000,ncol=200)
state.4 <- exp(cbind(bobcat$b0,bobcat$precip) %*% t(cbind(covs$intercept,covs$cov.precip)))

# State 5: skunk & coyote
state.5 <- matrix(NA,nrow=270000,ncol=200)
state.5 <- exp((cbind(skunk$s0,skunk$precip) %*% t(cbind(covs$intercept,covs$cov.precip))) +
                 (cbind(coyote$c0,coyote$precip) %*% t(cbind(covs$intercept,covs$cov.precip))))

# State 6: skunk & bobcat
state.6 <- matrix(NA,nrow=270000,ncol=200)
state.6 <- exp((cbind(skunk$s0,skunk$precip) %*% t(cbind(covs$intercept,covs$cov.precip))) +
                 (cbind(bobcat$b0,bobcat$precip) %*% t(cbind(covs$intercept,covs$cov.precip))))

# State 7: coyote & bobcat
state.7 <- matrix(NA,nrow=270000,ncol=200)
state.7 <- exp((cbind(coyote$c0,coyote$precip) %*% t(cbind(covs$intercept,covs$cov.precip))) + 
                 (cbind(bobcat$b0,bobcat$precip) %*% t(cbind(covs$intercept,covs$cov.precip))))

# State 8: skunk, coyote & bobcat
state.8 <- matrix(NA,nrow=270000,ncol=200)
state.8 <- exp((cbind(skunk$s0,skunk$precip) %*% t(cbind(covs$intercept,covs$cov.precip))) +
                 (cbind(coyote$c0,coyote$precip) %*% t(cbind(covs$intercept,covs$cov.precip))) +
                 (cbind(bobcat$b0,bobcat$precip) %*% t(cbind(covs$intercept,covs$cov.precip))))

precip.lo.array <- array(c(state.1, state.2, state.3, state.4, state.5, state.6, state.7, state.8),
                         dim = c(270000, 200, 8))

#now use multinomial logit to transition from log odds to probabilities
precip.prob.array <- array(NA, dim=c(270000,200,8))

for(i in 1:nrow(covs)){
  precip.prob.array[,i,] <- sweep(
    precip.lo.array[,i,],
    1,
    rowSums(precip.lo.array[,i,]),
    FUN = "/"
  )
}

precip.lo.array[,1,]
rowSums(precip.lo.array[,1,])[1]

precip.lo.array[2,1,] / rowSums(precip.lo.array[,1,])[2]
precip.prob.array[2,1,]

##### Now need to repeat but include the autoregressive coefficient in the equation

# state 1: no species present
# just a matrix of 1s
state.1 <- matrix(1,nrow=270000,ncol=200)

# State 2: just skunk
state.2 <- matrix(NA,nrow=270000,ncol=200)
state.2 <- exp(cbind(skunk$s0,skunk$precip,skunk$phi.s) %*% t(cbind(covs$intercept,covs$cov.precip,covs$autoreg)))

# State 3: just coyote
state.3 <- matrix(NA,nrow=270000,ncol=200)
state.3 <- exp(cbind(coyote$c0,coyote$precip,coyote$phi.c) %*% t(cbind(covs$intercept,covs$cov.precip,covs$autoreg)))

# State 4: just bobcat
state.4 <- matrix(NA,nrow=270000,ncol=200)
state.4 <- exp(cbind(bobcat$b0,bobcat$precip,bobcat$phi.b) %*% t(cbind(covs$intercept,covs$cov.precip,covs$autoreg)))

# State 5: skunk & coyote
state.5 <- matrix(NA,nrow=270000,ncol=200)
state.5 <- exp((cbind(skunk$s0,skunk$precip,skunk$phi.s) %*% t(cbind(covs$intercept,covs$cov.precip,covs$autoreg))) +
                 (cbind(coyote$c0,coyote$precip,coyote$phi.c) %*% t(cbind(covs$intercept,covs$cov.precip,covs$autoreg))))

# State 6: skunk & bobcat
state.6 <- matrix(NA,nrow=270000,ncol=200)
state.6 <- exp((cbind(skunk$s0,skunk$precip,skunk$phi.s) %*% t(cbind(covs$intercept,covs$cov.precip,covs$autoreg))) +
                 (cbind(bobcat$b0,bobcat$precip,bobcat$phi.b) %*% t(cbind(covs$intercept,covs$cov.precip,covs$autoreg))))

# State 7: coyote & bobcat
state.7 <- matrix(NA,nrow=270000,ncol=200)
state.7 <- exp((cbind(coyote$c0,coyote$precip,coyote$phi.c) %*% t(cbind(covs$intercept,covs$cov.precip,covs$autoreg))) + 
                 (cbind(bobcat$b0,bobcat$precip,bobcat$phi.b) %*% t(cbind(covs$intercept,covs$cov.precip,covs$autoreg))))

# State 8: skunk, coyote & bobcat
state.8 <- matrix(NA,nrow=270000,ncol=200)
state.8 <- exp((cbind(skunk$s0,skunk$precip,skunk$phi.s) %*% t(cbind(covs$intercept,covs$cov.precip,covs$autoreg))) +
                 (cbind(coyote$c0,coyote$precip,coyote$phi.c) %*% t(cbind(covs$intercept,covs$cov.precip,covs$autoreg))) +
                 (cbind(bobcat$b0,bobcat$precip,bobcat$phi.b) %*% t(cbind(covs$intercept,covs$cov.precip,covs$autoreg))))

precip.auto.lo.array <- array(c(state.1, state.2, state.3, state.4, state.5, state.6, state.7, state.8),
                              dim = c(270000, 200, 8))

#now use multinomial logit to transition from log odds to probabilities
precip.auto.prob.array <- array(NA, dim = c(270000, 200, 8))

for(i in 1:nrow(covs)){
  precip.auto.prob.array[,i,] <- sweep(
    precip.auto.lo.array[,i,],
    1,
    rowSums(precip.auto.lo.array[,i,]),
    FUN = "/"
  )
}
#####
#get average probability   
# calculating average psi
true.precip.prob <- precip.prob.array / (
  precip.prob.array + (1 - precip.auto.prob.array)
)

precip.quantiles <- array(NA,dim=c(3,200,8))
for (i in 1:8) {
  for (j in 1:200) {
    precip.quantiles[,j,i] <- quantile(true.precip.prob[,j,i] , c(.025,.5,.975))
  }
}
###########
#### Marginal Occupancy Probabilities ####
marg.precip.bob <- precip.quantiles[2,,4]+precip.quantiles[2,,6]+precip.quantiles[2,,7]+precip.quantiles[2,,8]
marg.precip.coy <- precip.quantiles[2,,3]+precip.quantiles[2,,5]+precip.quantiles[2,,7]+precip.quantiles[2,,8]
marg.precip.skunk <- precip.quantiles[2,,2]+precip.quantiles[2,,5]+precip.quantiles[2,,6]+precip.quantiles[2,,8]

#### Confidence Intervals ####
s.precip.low <- precip.quantiles[1,,2]+precip.quantiles[1,,5]+precip.quantiles[1,,6]+precip.quantiles[1,,8]
s.precip.high <- precip.quantiles[3,,2]+precip.quantiles[3,,5]+precip.quantiles[3,,6]+precip.quantiles[3,,8]
c.precip.low <- precip.quantiles[1,,3]+precip.quantiles[1,,5]+precip.quantiles[1,,7]+precip.quantiles[1,,8]
c.precip.high <- precip.quantiles[3,,3]+precip.quantiles[3,,5]+precip.quantiles[3,,7]+precip.quantiles[3,,8]
b.precip.low <- precip.quantiles[1,,4]+precip.quantiles[1,,6]+precip.quantiles[1,,7]+precip.quantiles[1,,8]
b.precip.high <- precip.quantiles[3,,4]+precip.quantiles[3,,6]+precip.quantiles[3,,7]+precip.quantiles[3,,8]

s.precip.low[s.precip.low<0] <- 0
s.precip.high[s.precip.high>1] <- 1
c.precip.low[c.precip.low<0] <- 0
c.precip.high[c.precip.high>1] <- 1
b.precip.low[b.precip.low<0] <- 0
b.precip.high[b.precip.high>1] <- 1
######## precipation plots ######
pdf(height=6,width=10,"Figures/Marginal Occupancy_Precipitation.pdf")
#png("Figures/Marginal_Occupancy_Precipitation.png", units = "in", height=6, width=10, res=300)
par(mfrow=c(1,3),
    oma = c(5,5,2,1))
par(mar= c(0,.5,1,0))
par(family="serif")
par(cex.axis=1.3)
plot(x=cov.precip, y=precip.quantiles[2,,2], type = "l",,lty=2,col="red",ylim=c(0,1))
lines(x=cov.precip, y=precip.quantiles[2,,5], lty=2,col="blue")
lines(x=cov.precip, y=precip.quantiles[2,,6], lty=2,col="green")
lines(x=cov.precip, y=precip.quantiles[2,,8], lty=2,col="gold")
lines(x=cov.precip, y=marg.precip.skunk)
polygon(c(cov.precip,rev(cov.precip)),c(s.precip.low,rev(s.precip.high)),
        col= adjustcolor("cornflowerblue", 0.3))
text(x=-1.35,y=1,"Spotted Skunk", cex=1.5)

plot(x=cov.precip, y=precip.quantiles[2,,3], type = "l",,lty=2,col="red",ylim=c(0,1),yaxt="n")
lines(x=cov.precip, y=precip.quantiles[2,,5], lty=2,col="blue")
lines(x=cov.precip, y=precip.quantiles[2,,7], lty=2,col="green")
lines(x=cov.precip, y=precip.quantiles[2,,8], lty=2,col="gold")
lines(x=cov.precip, y=marg.precip.coy)
polygon(c(cov.precip,rev(cov.precip)),c(c.precip.low,rev(c.precip.high)),
        col= adjustcolor("cornflowerblue", 0.3))
text(x=-1.75,y=1,"Coyote",cex=1.5)

plot(x=cov.precip, y=precip.quantiles[2,,4], type = "l",,lty=2,col="red",ylim=c(0,1),yaxt="n")
lines(x=cov.precip, y=precip.quantiles[2,,6], lty=2,col="blue")
lines(x=cov.precip, y=precip.quantiles[2,,7], lty=2,col="green")
lines(x=cov.precip, y=precip.quantiles[2,,8], lty=2,col="gold")
lines(x=cov.precip, y=marg.precip.bob)
polygon(c(cov.precip,rev(cov.precip)),c(b.precip.low,rev(b.precip.high)),
        col= adjustcolor("cornflowerblue", 0.3))
text(x=-1.75,y=1,"Bobcat", cex=1.5)
mtext(outer=TRUE, side=1,line=3,text = "Average Precipitation",cex=1.3)
mtext(outer=TRUE, side=2,line=3,text = "Probability of Use", cex=1.3)
mtext(outer=TRUE, side=3,line=0,text = "Marginal use probability as a function of average precipitation", cex=1.3)
dev.off()
###

tmp <- grep("precip",colnames(none$samples$chain1))
none$summary$all.chains[tmp,]

rm(list=c("precip.auto.lo.array","precip.auto.prob.array","precip.lo.array","precip.prob.array","true.precip.prob"))
################################################################################
################################################################################
###### Road Cell Count  #########
#REPEAT FOR NEW COVARIATE

# state 1: no species present
# just a matrix of 1s
state.1 <- matrix(1,nrow=270000,ncol=200)

# State 2: just skunk
state.2 <- matrix(NA,nrow=270000,ncol=200)
state.2 <- exp(cbind(skunk$s0,skunk$road) %*% t(cbind(covs$intercept,covs$cov.road)))

# State 3: just coyote
state.3 <- matrix(NA,nrow=270000,ncol=200)
state.3 <- exp(cbind(coyote$c0,coyote$road) %*% t(cbind(covs$intercept,covs$cov.road)))

# State 4: just bobcat
state.4 <- matrix(NA,nrow=270000,ncol=200)
state.4 <- exp(cbind(bobcat$b0,bobcat$road) %*% t(cbind(covs$intercept,covs$cov.road)))

# State 5: skunk & coyote
state.5 <- matrix(NA,nrow=270000,ncol=200)
state.5 <- exp((cbind(skunk$s0,skunk$road) %*% t(cbind(covs$intercept,covs$cov.road))) +
                 (cbind(coyote$c0,coyote$road) %*% t(cbind(covs$intercept,covs$cov.road))))

# State 6: skunk & bobcat
state.6 <- matrix(NA,nrow=270000,ncol=200)
state.6 <- exp((cbind(skunk$s0,skunk$road) %*% t(cbind(covs$intercept,covs$cov.road))) +
                 (cbind(bobcat$b0,bobcat$road) %*% t(cbind(covs$intercept,covs$cov.road))))

# State 7: coyote & bobcat
state.7 <- matrix(NA,nrow=270000,ncol=200)
state.7 <- exp((cbind(coyote$c0,coyote$road) %*% t(cbind(covs$intercept,covs$cov.road))) + 
                 (cbind(bobcat$b0,bobcat$road) %*% t(cbind(covs$intercept,covs$cov.road))))

# State 8: skunk, coyote & bobcat
state.8 <- matrix(NA,nrow=270000,ncol=200)
state.8 <- exp((cbind(skunk$s0,skunk$road) %*% t(cbind(covs$intercept,covs$cov.road))) +
                 (cbind(coyote$c0,coyote$road) %*% t(cbind(covs$intercept,covs$cov.road))) +
                 (cbind(bobcat$b0,bobcat$road) %*% t(cbind(covs$intercept,covs$cov.road))))

road.lo.array <- array(c(state.1, state.2, state.3, state.4, state.5, state.6, state.7, state.8),
                       dim = c(270000, 200, 8))

#now use multinomial logit to transition from log odds to probabilities
road.prob.array <- array(NA, dim=c(270000,200,8))

for(i in 1:nrow(covs)){
  road.prob.array[,i,] <- sweep(
    road.lo.array[,i,],
    1,
    rowSums(road.lo.array[,i,]),
    FUN = "/"
  )
}

road.lo.array[,1,]
rowSums(road.lo.array[,1,])[1]

road.lo.array[2,1,] / rowSums(road.lo.array[,1,])[2]
road.prob.array[2,1,]

##### Now need to repeat but include the autoregressive coefficient in the equation

# state 1: no species present
# just a matrix of 1s
state.1 <- matrix(1,nrow=270000,ncol=200)

# State 2: just skunk
state.2 <- matrix(NA,nrow=270000,ncol=200)
state.2 <- exp(cbind(skunk$s0,skunk$road,skunk$phi.s) %*% t(cbind(covs$intercept,covs$cov.road,covs$autoreg)))

# State 3: just coyote
state.3 <- matrix(NA,nrow=270000,ncol=200)
state.3 <- exp(cbind(coyote$c0,coyote$road,coyote$phi.c) %*% t(cbind(covs$intercept,covs$cov.road,covs$autoreg)))

# State 4: just bobcat
state.4 <- matrix(NA,nrow=270000,ncol=200)
state.4 <- exp(cbind(bobcat$b0,bobcat$road,bobcat$phi.b) %*% t(cbind(covs$intercept,covs$cov.road,covs$autoreg)))

# State 5: skunk & coyote
state.5 <- matrix(NA,nrow=270000,ncol=200)
state.5 <- exp((cbind(skunk$s0,skunk$road,skunk$phi.s) %*% t(cbind(covs$intercept,covs$cov.road,covs$autoreg))) +
                 (cbind(coyote$c0,coyote$road,coyote$phi.c) %*% t(cbind(covs$intercept,covs$cov.road,covs$autoreg))))

# State 6: skunk & bobcat
state.6 <- matrix(NA,nrow=270000,ncol=200)
state.6 <- exp((cbind(skunk$s0,skunk$road,skunk$phi.s) %*% t(cbind(covs$intercept,covs$cov.road,covs$autoreg))) +
                 (cbind(bobcat$b0,bobcat$road,bobcat$phi.b) %*% t(cbind(covs$intercept,covs$cov.road,covs$autoreg))))

# State 7: coyote & bobcat
state.7 <- matrix(NA,nrow=270000,ncol=200)
state.7 <- exp((cbind(coyote$c0,coyote$road,coyote$phi.c) %*% t(cbind(covs$intercept,covs$cov.road,covs$autoreg))) + 
                 (cbind(bobcat$b0,bobcat$road,bobcat$phi.b) %*% t(cbind(covs$intercept,covs$cov.road,covs$autoreg))))

# State 8: skunk, coyote & bobcat
state.8 <- matrix(NA,nrow=270000,ncol=200)
state.8 <- exp((cbind(skunk$s0,skunk$road,skunk$phi.s) %*% t(cbind(covs$intercept,covs$cov.road,covs$autoreg))) +
                 (cbind(coyote$c0,coyote$road,coyote$phi.c) %*% t(cbind(covs$intercept,covs$cov.road,covs$autoreg))) +
                 (cbind(bobcat$b0,bobcat$road,bobcat$phi.b) %*% t(cbind(covs$intercept,covs$cov.road,covs$autoreg))))

road.auto.lo.array <- array(c(state.1, state.2, state.3, state.4, state.5, state.6, state.7, state.8),
                            dim = c(270000, 200, 8))

#now use multinomial logit to transition from log odds to probabilities
road.auto.prob.array <- array(NA, dim = c(270000, 200, 8))

for(i in 1:nrow(covs)){
  road.auto.prob.array[,i,] <- sweep(
    road.auto.lo.array[,i,],
    1,
    rowSums(road.auto.lo.array[,i,]),
    FUN = "/"
  )
}
#####
#get average probability   
# calculating average psi
true.road.prob <- road.prob.array / (
  road.prob.array + (1 - road.auto.prob.array)
)

road.quantiles <- array(NA,dim=c(3,200,8))
for (i in 1:8) {
  for (j in 1:200) {
    road.quantiles[,j,i] <- quantile(true.road.prob[,j,i] , c(.025,.5,.975))
  }
}
###########
#### Marginal Occupancy Probabilities ####
marg.road.bob <- road.quantiles[2,,4]+road.quantiles[2,,6]+road.quantiles[2,,7]+road.quantiles[2,,8]
marg.road.coy <- road.quantiles[2,,3]+road.quantiles[2,,5]+road.quantiles[2,,7]+road.quantiles[2,,8]
marg.road.skunk <- road.quantiles[2,,2]+road.quantiles[2,,5]+road.quantiles[2,,6]+road.quantiles[2,,8]
################ Confidence Intervals
s.road.low <- road.quantiles[1,,2]+road.quantiles[1,,5]+road.quantiles[1,,6]+road.quantiles[1,,8]
s.road.high <- road.quantiles[3,,2]+road.quantiles[3,,5]+road.quantiles[3,,6]+road.quantiles[3,,8]
c.road.low <- road.quantiles[1,,3]+road.quantiles[1,,5]+road.quantiles[1,,7]+road.quantiles[1,,8]
c.road.high <- road.quantiles[3,,3]+road.quantiles[3,,5]+road.quantiles[3,,7]+road.quantiles[3,,8]
b.road.low <- road.quantiles[1,,4]+road.quantiles[1,,6]+road.quantiles[1,,7]+road.quantiles[1,,8]
b.road.high <- road.quantiles[3,,4]+road.quantiles[3,,6]+road.quantiles[3,,7]+road.quantiles[3,,8]

s.road.low[s.road.low<0] <- 0
s.road.high[s.road.high>1] <- 1
c.road.low[c.road.low<0] <- 0
c.road.high[c.road.high>1] <- 1
b.road.low[b.road.low<0] <- 0
b.road.high[b.road.high>1] <- 1

######## road plots ######
pdf(height=6,width=10,"Figures/Marginal Occupancy_Road.pdf")
#png("Figures/Marginal_Occupancy_Road Density.png", units = "in", height=6, width=10, res=300)
par(mfrow=c(1,3),
    oma = c(5,5,2,1))
par(mar= c(0,.5,1,0))
par(family="serif")
par(cex.axis=1.3)
plot(x=cov.road, y=road.quantiles[2,,2], type = "l",,lty=2,col="red",ylim=c(0,1))
lines(x=cov.road, y=road.quantiles[2,,5], lty=2,col="blue")
lines(x=cov.road, y=road.quantiles[2,,6], lty=2,col="green")
lines(x=cov.road, y=road.quantiles[2,,8], lty=2,col="gold")
lines(x=cov.road, y=marg.road.skunk)
polygon(c(cov.road,rev(cov.road)),c(s.road.low,rev(s.road.high)),
        col= adjustcolor("cornflowerblue", 0.3))
text(x=-.15,y=1,"Spotted Skunk", cex=1.5)

plot(x=cov.road, y=road.quantiles[2,,3], type = "l",,lty=2,col="red",ylim=c(0,1),yaxt="n")
lines(x=cov.road, y=road.quantiles[2,,5], lty=2,col="blue")
lines(x=cov.road, y=road.quantiles[2,,7], lty=2,col="green")
lines(x=cov.road, y=road.quantiles[2,,8], lty=2,col="gold")
lines(x=cov.road, y=marg.road.coy)
polygon(c(cov.road,rev(cov.road)),c(c.road.low,rev(c.road.high)),
        col= adjustcolor("cornflowerblue", 0.3))
text(x=-.5,y=1,"Coyote", cex=1.5)

plot(x=cov.road, y=road.quantiles[2,,4], type = "l",,lty=2,col="red",ylim=c(0,1),yaxt="n")
lines(x=cov.road, y=road.quantiles[2,,6], lty=2,col="blue")
lines(x=cov.road, y=road.quantiles[2,,7], lty=2,col="green")
lines(x=cov.road, y=road.quantiles[2,,8], lty=2,col="gold")
lines(x=cov.road, y=marg.road.bob)
polygon(c(cov.road,rev(cov.road)),c(b.road.low,rev(b.road.high)),
        col= adjustcolor("cornflowerblue", 0.3))
text(x=-.5,y=1,"Bobcat", cex=1.5)
mtext(outer=TRUE, side=1,line=3,text = "Road Density", cex=1.3)
mtext(outer=TRUE, side=2,line=3,text = "Probability of Use", cex=1.3)
mtext(outer=TRUE, side=3,line=0,text = "Marginal use probability as a function of road density",cex=1.3)
dev.off()
###

tmp <- grep("road",colnames(none$samples$chain1))
none$summary$all.chains[tmp,]

rm(list=c("road.auto.lo.array","road.auto.prob.array","road.lo.array","road.prob.array","true.road.prob"))
################################################################################
################################################################################
####### Save the marginal probabilities so that code doesn't have to be re-run everytime to fiddle with these plots
#saveRDS(marg.dense.bob,file = "Figures/RDS_for_figures/marg.dense.bob.rds")
#saveRDS(marg.dense.coy,"Figures/RDS_for_figures/marg.dense.coy.rds")
#saveRDS(marg.dense.skunk,"Figures/RDS_for_figures/marg.dense.skunk.rds")
#saveRDS(marg.devel.bob,"Figures/RDS_for_figures/marg.devel.bob.rds")
#saveRDS(marg.devel.coy,"Figures/RDS_for_figures/marg.devel.coy.rds")
#saveRDS(marg.devel.skunk,"Figures/RDS_for_figures/marg.devel.skunk.rds")
#saveRDS(marg.edge.bob,"Figures/RDS_for_figures/marg.edge.bob.rds")
#saveRDS(marg.edge.coy,"Figures/RDS_for_figures/marg.edge.coy.rds")
#saveRDS(marg.edge.skunk,"Figures/RDS_for_figures/marg.edge.skunk.rds")
#saveRDS(marg.elev.bob,"Figures/RDS_for_figures/marg.elev.bob.rds")
#saveRDS(marg.elev.coy,"Figures/RDS_for_figures/marg.elev.coy.rds")
#saveRDS(marg.elev.skunk,"Figures/RDS_for_figures/marg.elev.skunk.rds")
#saveRDS(marg.precip.bob,"Figures/RDS_for_figures/marg.precip.bob.rds")
#saveRDS(marg.precip.coy,"Figures/RDS_for_figures/marg.precip.coy.rds")
#saveRDS(marg.precip.skunk,"Figures/RDS_for_figures/marg.precip.skunk.rds")
#saveRDS(marg.road.bob,"Figures/RDS_for_figures/marg.road.bob.rds")
#saveRDS(marg.road.coy,"Figures/RDS_for_figures/marg.road.coy.rds")
#saveRDS(marg.road.skunk,"Figures/RDS_for_figures/marg.road.skunk.rds")
#
#saveRDS(s.dense.high,file = "Figures/RDS_for_figures/s.dense.high.rds")
#saveRDS(s.dense.low,file = "Figures/RDS_for_figures/s.dense.low.rds")
#saveRDS(s.devel.high,file = "Figures/RDS_for_figures/s.devel.high.rds")
#saveRDS(s.devel.low,file = "Figures/RDS_for_figures/s.devel.low.rds")
#saveRDS(s.edge.high,file = "Figures/RDS_for_figures/s.edge.high.rds")
#saveRDS(s.edge.low,file = "Figures/RDS_for_figures/s.edge.low.rds")
#saveRDS(s.elev.high,file = "Figures/RDS_for_figures/s.elev.high.rds")
#saveRDS(s.elev.low,file = "Figures/RDS_for_figures/s.elev.low.rds")
#saveRDS(s.precip.high,file = "Figures/RDS_for_figures/s.precip.high.rds")
#saveRDS(s.precip.low,file = "Figures/RDS_for_figures/s.precip.low.rds")
#saveRDS(s.road.high,file = "Figures/RDS_for_figures/s.road.high.rds")
#saveRDS(s.road.low,file = "Figures/RDS_for_figures/s.road.low.rds")
#
#saveRDS(c.dense.high,file = "Figures/RDS_for_figures/c.dense.high.rds")
#saveRDS(c.dense.low,file = "Figures/RDS_for_figures/c.dense.low.rds")
#saveRDS(c.devel.high,file = "Figures/RDS_for_figures/c.devel.high.rds")
#saveRDS(c.devel.low,file = "Figures/RDS_for_figures/c.devel.low.rds")
#saveRDS(c.edge.high,file = "Figures/RDS_for_figures/c.edge.high.rds")
#saveRDS(c.edge.low,file = "Figures/RDS_for_figures/c.edge.low.rds")
#saveRDS(c.elev.high,file = "Figures/RDS_for_figures/c.elev.high.rds")
#saveRDS(c.elev.low,file = "Figures/RDS_for_figures/c.elev.low.rds")
#saveRDS(c.precip.high,file = "Figures/RDS_for_figures/c.precip.high.rds")
#saveRDS(c.precip.low,file = "Figures/RDS_for_figures/c.precip.low.rds")
#saveRDS(c.road.high,file = "Figures/RDS_for_figures/c.road.high.rds")
#saveRDS(c.road.low,file = "Figures/RDS_for_figures/c.road.low.rds")
#
#saveRDS(b.dense.high,file = "Figures/RDS_for_figures/b.dense.high.rds")
#saveRDS(b.dense.low,file = "Figures/RDS_for_figures/b.dense.low.rds")
#saveRDS(b.devel.high,file = "Figures/RDS_for_figures/b.devel.high.rds")
#saveRDS(b.devel.low,file = "Figures/RDS_for_figures/b.devel.low.rds")
#saveRDS(b.edge.high,file = "Figures/RDS_for_figures/b.edge.high.rds")
#saveRDS(b.edge.low,file = "Figures/RDS_for_figures/b.edge.low.rds")
#saveRDS(b.elev.high,file = "Figures/RDS_for_figures/b.elev.high.rds")
#saveRDS(b.elev.low,file = "Figures/RDS_for_figures/b.elev.low.rds")
#saveRDS(b.precip.high,file = "Figures/RDS_for_figures/b.precip.high.rds")
#saveRDS(b.precip.low,file = "Figures/RDS_for_figures/b.precip.low.rds")
#saveRDS(b.road.high,file = "Figures/RDS_for_figures/b.road.high.rds")
#saveRDS(b.road.low,file = "Figures/RDS_for_figures/b.road.low.rds")

#####
#LOAD RDS when needed
marg.dense.bob <- readRDS("Figures/RDS_for_figures/marg.dense.bob.rds")
marg.dense.coy <- readRDS("Figures/RDS_for_figures/marg.dense.coy.rds")
marg.dense.skunk <- readRDS("Figures/RDS_for_figures/marg.dense.skunk.rds")
marg.devel.bob <- readRDS("Figures/RDS_for_figures/marg.devel.bob.rds")
marg.devel.coy <- readRDS("Figures/RDS_for_figures/marg.devel.coy.rds")
marg.devel.skunk <- readRDS("Figures/RDS_for_figures/marg.devel.skunk.rds")
marg.edge.bob <- readRDS("Figures/RDS_for_figures/marg.edge.bob.rds")
marg.edge.coy <- readRDS("Figures/RDS_for_figures/marg.edge.coy.rds")
marg.edge.skunk <- readRDS("Figures/RDS_for_figures/marg.edge.skunk.rds")
marg.elev.bob <- readRDS("Figures/RDS_for_figures/marg.elev.bob.rds")
marg.elev.coy <- readRDS("Figures/RDS_for_figures/marg.elev.coy.rds")
marg.elev.skunk <- readRDS("Figures/RDS_for_figures/marg.elev.skunk.rds")
marg.precip.bob <- readRDS("Figures/RDS_for_figures/marg.precip.bob.rds")
marg.precip.coy <- readRDS("Figures/RDS_for_figures/marg.precip.coy.rds")
marg.precip.skunk <- readRDS("Figures/RDS_for_figures/marg.precip.skunk.rds")
marg.road.bob <- readRDS("Figures/RDS_for_figures/marg.road.bob.rds")
marg.road.coy <- readRDS("Figures/RDS_for_figures/marg.road.coy.rds")
marg.road.skunk <- readRDS("Figures/RDS_for_figures/marg.road.skunk.rds")
#
b.dense.high <- readRDS(file = "Figures/RDS_for_figures/b.dense.high.rds")
b.dense.low <- readRDS(file = "Figures/RDS_for_figures/b.dense.low.rds")
b.devel.high <- readRDS(file = "Figures/RDS_for_figures/b.devel.high.rds")
b.devel.low <- readRDS(file = "Figures/RDS_for_figures/b.devel.low.rds")
b.edge.high <- readRDS(file = "Figures/RDS_for_figures/b.edge.high.rds")
b.edge.low <- readRDS(file = "Figures/RDS_for_figures/b.edge.low.rds")
b.elev.high <- readRDS(file = "Figures/RDS_for_figures/b.elev.high.rds")
b.elev.low <- readRDS(file = "Figures/RDS_for_figures/b.elev.low.rds")
b.precip.high <- readRDS(file = "Figures/RDS_for_figures/b.precip.high.rds")
b.precip.low <- readRDS(file = "Figures/RDS_for_figures/b.precip.low.rds")
b.road.high <- readRDS(file = "Figures/RDS_for_figures/b.road.high.rds")
b.road.low <- readRDS(file = "Figures/RDS_for_figures/b.road.low.rds")
#
c.dense.high <- readRDS(file = "Figures/RDS_for_figures/c.dense.high.rds")
c.dense.low <- readRDS(file = "Figures/RDS_for_figures/c.dense.low.rds")
c.devel.high <- readRDS(file = "Figures/RDS_for_figures/c.devel.high.rds")
c.devel.low <- readRDS(file = "Figures/RDS_for_figures/c.devel.low.rds")
c.edge.high <- readRDS(file = "Figures/RDS_for_figures/c.edge.high.rds")
c.edge.low <- readRDS(file = "Figures/RDS_for_figures/c.edge.low.rds")
c.elev.high <- readRDS(file = "Figures/RDS_for_figures/c.elev.high.rds")
c.elev.low <- readRDS(file = "Figures/RDS_for_figures/c.elev.low.rds")
c.precip.high <- readRDS(file = "Figures/RDS_for_figures/c.precip.high.rds")
c.precip.low <- readRDS(file = "Figures/RDS_for_figures/c.precip.low.rds")
c.road.high <- readRDS(file = "Figures/RDS_for_figures/c.road.high.rds")
c.road.low <- readRDS(file = "Figures/RDS_for_figures/c.road.low.rds")
#
s.dense.high <- readRDS(file = "Figures/RDS_for_figures/s.dense.high.rds")
s.dense.low <- readRDS(file = "Figures/RDS_for_figures/s.dense.low.rds")
s.devel.high <- readRDS(file = "Figures/RDS_for_figures/s.devel.high.rds")
s.devel.low <- readRDS(file = "Figures/RDS_for_figures/s.devel.low.rds")
s.edge.high <- readRDS(file = "Figures/RDS_for_figures/s.edge.high.rds")
s.edge.low <- readRDS(file = "Figures/RDS_for_figures/s.edge.low.rds")
s.elev.high <- readRDS(file = "Figures/RDS_for_figures/s.elev.high.rds")
s.elev.low <- readRDS(file = "Figures/RDS_for_figures/s.elev.low.rds")
s.precip.high <- readRDS(file = "Figures/RDS_for_figures/s.precip.high.rds")
s.precip.low <- readRDS(file = "Figures/RDS_for_figures/s.precip.low.rds")
s.road.high <- readRDS(file = "Figures/RDS_for_figures/s.road.high.rds")
s.road.low <- readRDS(file = "Figures/RDS_for_figures/s.road.low.rds")

################################################################################
################################################################################
##########                                                         #############
##########             6 X 3 Plot with RUG for Paper               #############
##########                                                         #############
################################################################################
################################################################################

 ###### Beta Coeffecient labels are hard coded and will likely need to be changed to reflect new model runs

#pdf(height=12.5,width=6,"Figures/Marginal Occupancy_sixBythree_shrinkage.pdf")
png("Figures/MarginalOccupancy_sixbyThree_shrinkage_RUG.png",units="in",height = 12.5,width=6,res = 300)
par(mfrow=c(6,3),
    oma=c(0,3.5,2,.65)) #.5,3.5,2,.65

par(mar=c(4.5,.55,0,0)) #(3.8,.55,0,0)
par(family="serif")
par(cex.axis=1.35)

############## Tree Density ########################
plot(x=cov.dense, y=marg.dense.skunk, type = "l",,lty=1,col="black",yaxt="n",ylim=c(-.08,1),xlab = "",xaxt="n")        
abline(v=0, lty=2, col="lightgray")
polygon(c(cov.dense,rev(cov.dense)),c(s.dense.low,rev(s.dense.high)),
        col= adjustcolor("lightgreen", 0.3),lty=0)
for (i in 1:712) {
  lines(x=rep(Occ.scaled.mat[i,"tree.density"],2), y=c(-.03,-0.07)) 
}

axis(2,at=c(0,.25,.5,.75,1),mgp=c(0,.5,0),cex.axis=1.35)
axis(1,at=seq(from=cov.dense[25],to=cov.dense[175],length=3),labels=round(seq(from=label.dense[25],to=label.dense[175],length=3),0),mgp=c(0,.6,0),cex.axis=1.35)
text(x=1.5,y=1.125,"Western Spotted Skunk", xpd=NA, cex=1.8)
text(x=par("usr")[1]+(diff(par("usr")[1:2])*.005),y=.95,pos=4,"\u03b2= 0.42 (0.25, 0.60)",cex=1.25,font = 2)


plot(x=cov.dense, y=marg.dense.coy, type = "l",,lty=1,col="black",ylim=c(-.08,1),yaxt="n",ylab="",xlab="",xaxt="n")
abline(v=0, lty=2, col="lightgray")
polygon(c(cov.dense,rev(cov.dense)),c(c.dense.low,rev(c.dense.high)),
        col= adjustcolor("lightgreen", 0.3),lty=0)
for (i in 1:712) {
  lines(x=rep(Occ.scaled.mat[i,"tree.density"],2), y=c(-.03,-0.07)) 
}
axis(2,at=c(0,.25,.5,.75,1),labels = FALSE)
axis(1,at=seq(from=cov.dense[25],to=cov.dense[175],length=3),labels=round(seq(from=label.dense[25],to=label.dense[175],length=3),0),mgp=c(0,.6,0),cex.axis=1.35)
text(x=1.5,y=1.125,"Coyote", xpd=NA, cex=1.8)
text(x=1.5,y=-.385,"Forest Density (trees/ha)",xpd=NA, cex=1.6)
text(x=par("usr")[1]+(diff(par("usr")[1:2])*.005),y=.95,pos=4,"\u03b2= 0.10 (\u22120.09, 0.36)",cex=1.25)

plot(x=cov.dense, y=marg.dense.bob, type = "l",,lty=1,col="black",ylim=c(-.08,1),yaxt="n",ylab = "",xlab="",xaxt="n")
abline(v=0, lty=2, col="lightgray")
polygon(c(cov.dense,rev(cov.dense)),c(b.dense.low,rev(b.dense.high)),
        col= adjustcolor("lightgreen", 0.3),lty=0)
for (i in 1:712) {
  lines(x=rep(Occ.scaled.mat[i,"tree.density"],2), y=c(-.03,-0.07)) 
}
axis(2,at=c(0,.25,.5,.75,1),labels = FALSE)
axis(1,at=seq(from=cov.dense[25],to=cov.dense[175],length=3),labels=round(seq(from=label.dense[25],to=label.dense[175],length=3),0),mgp=c(0,.6,0),cex.axis=1.35)
text(x=1.5,y=1.125,"Bobcat", xpd=NA, cex=1.8)
text(x=par("usr")[1]+(diff(par("usr")[1:2])*.005),y=.95,pos=4,"\u03b2= 0.15 (\u22120.13, 0.56)",cex=1.25)
mtext(outer=TRUE, side=2,line=1.65,cex=1.25,text = "Marginal Probability of Use")

############ Edge   ############
plot(x=cov.edge, y=marg.edge.skunk, type = "l",,lty=1,col="black",yaxt="n",ylim=c(-.08,1),xlab = "",xaxt="n")
abline(v=0, lty=2, col="lightgray")
polygon(c(cov.edge,rev(cov.edge)),c(s.edge.low,rev(s.edge.high)),
        col= adjustcolor("red", 0.3),lty=0)
for (i in 1:712) {
  lines(x=rep(Occ.scaled.mat[i,"edge.length"],2), y=c(-.03,-0.07)) 
}
axis(2,at=c(0,.25,.5,.75,1),mgp=c(0,.5,0),cex.axis=1.35)
axis(1,at=seq(from=cov.edge[25],to=cov.edge[175],length=3),labels=round(seq(from=label.edge[25],to=label.edge[175],length=3),0),mgp=c(0,.6,0),cex.axis=1.35)
text(x=par("usr")[1]+(diff(par("usr")[1:2])*.005),y=.95,pos=4,"\u03b2= \u22120.34 (\u22120.54, \u22120.14)",cex=1.25,font=2)

plot(x=cov.edge, y=marg.edge.coy, type = "l",,lty=2,col="black",ylim=c(-.08,1),yaxt="n",ylab = "",xlab = "",xaxt="n")
abline(v=0, lty=2, col="lightgray")
polygon(c(cov.edge,rev(cov.edge)),c(c.edge.low,rev(c.edge.high)),
        col= adjustcolor("gray", 0.3),lty=0)
for (i in 1:712) {
  lines(x=rep(Occ.scaled.mat[i,"edge.length"],2), y=c(-.03,-0.07)) 
}
axis(2,at=c(0,.25,.5,.75,1),labels = FALSE)
axis(1,at=seq(from=cov.edge[25],to=cov.edge[175],length=3),labels=round(seq(from=label.edge[25],to=label.edge[175],length=3),0),mgp=c(0,.6,0),cex.axis=1.35)
text(x=1,y=-.385,expression(paste("Edge Density (m/",km^2,")")),xpd=NA,cex=1.6)
text(x=par("usr")[1]+(diff(par("usr")[1:2])*.005), y=.95,pos=4, "\u03b2= \u22120.01 (\u22120.25, 0.21)",cex=1.25)

plot(x=cov.edge, y=marg.edge.bob, type = "l",,lty=1,col="black",ylim=c(-.08,1),yaxt="n",ylab = "",xlab = "",xaxt="n")
abline(v=0, lty=2, col="lightgray")
polygon(c(cov.edge,rev(cov.edge)),c(b.edge.low,rev(b.edge.high)),
        col= adjustcolor("red", 0.3),lty=0)
for (i in 1:712) {
  lines(x=rep(Occ.scaled.mat[i,"edge.length"],2), y=c(-.03,-0.07)) 
}
axis(2,at=c(0,.25,.5,.75,1),labels = FALSE)
axis(1,at=seq(from=cov.edge[25],to=cov.edge[175],length=3),labels=round(seq(from=label.edge[25],to=label.edge[175],length=3),0),mgp=c(0,.6,0),cex.axis=1.35)
text(x=par("usr")[1]+(diff(par("usr")[1:2])*.005), y=.95,pos=4, "\u03b2= \u22120.15 (\u22120.68, 0.20)",cex=1.25)

########### Road #########
plot(x=cov.road, y=marg.road.skunk, type = "l",,lty=1,col="black",yaxt="n",ylim=c(-.08,1),xlab = "",xaxt="n")
abline(v=0, lty=2, col="lightgray")
polygon(c(cov.road,rev(cov.road)),c(s.road.low,rev(s.road.high)),
        col= adjustcolor("lightgreen", 0.3),lty=0)
for (i in 1:712) {
  lines(x=rep(Occ.scaled.mat[i,"road"],2), y=c(-.03,-0.07)) 
}
axis(2,at=c(0,.25,.5,.75,1),mgp=c(0,.5,0),cex.axis=1.35)
axis(1,at=seq(from=cov.road[1],to=cov.road[200],length=3),labels=c(0,round(seq(from=label.road[1],to=label.road[200],length=3)[2:3],0)),mgp=c(0,.6,0),cex.axis=1.35)
text(x=par("usr")[1]+(diff(par("usr")[1:2])*.005), y=.95,pos=4, "\u03b2= 0.24 (0.04, 0.43)",cex=1.25,font = 2)


plot(x=cov.road, y=marg.road.coy, type = "l",,lty=2,col="black",ylim=c(-.08,1),yaxt="n",ylab = "",xlab = "",xaxt="n")
abline(v=0, lty=2, col="lightgray")
polygon(c(cov.road,rev(cov.road)),c(c.road.low,rev(c.road.high)),
        col= adjustcolor("gray", 0.3),lty=0)
for (i in 1:712) {
  lines(x=rep(Occ.scaled.mat[i,"road"],2), y=c(-.03,-0.07)) 
}
axis(2,at=c(0,.25,.5,.75,1),labels = FALSE)
axis(1,at=seq(from=cov.road[1],to=cov.road[200],length=3),labels=c(0,round(seq(from=label.road[1],to=label.road[200],length=3)[2:3],0)),mgp=c(0,.6,0),cex.axis=1.35)
text(x=1.8,y=-.385,expression(paste("Road Density (cells/",km^2,")")),xpd=NA,cex=1.6)
text(x=par("usr")[1]+(diff(par("usr")[1:2])*.005), y=.95,pos=4, "\u03b2= 0.01 (\u22120.20, 0.21)",cex=1.25)

plot(x=cov.road, y=marg.road.bob, type = "l",,lty=2,col="black",ylim=c(-.08,1),yaxt="n",ylab = "",xlab = "",xaxt="n")
abline(v=0, lty=2, col="lightgray")
polygon(c(cov.road,rev(cov.road)),c(b.road.low,rev(b.road.high)),
        col= adjustcolor("gray", 0.3),lty=0)
for (i in 1:712) {
  lines(x=rep(Occ.scaled.mat[i,"road"],2), y=c(-.03,-0.07)) 
}
axis(2,at=c(0,.25,.5,.75,1),labels = FALSE)
axis(1,at=seq(from=cov.road[1],to=cov.road[200],length=3),labels=c(0,round(seq(from=label.road[1],to=label.road[200],length=3)[2:3],0)),mgp=c(0,.6,0))
text(x=par("usr")[1]+(diff(par("usr")[1:2])*.005), y=.95,pos=4,"\u03b2= 0.04 (\u22120.28, 0.44)",cex=1.25)

############ % Developed ###########
plot(x=cov.devel, y=marg.devel.skunk, type = "l",,lty=1,col="black",yaxt="n",ylim=c(-.08,1),xlab = "",xaxt="n")
abline(v=0, lty=2, col="lightgray")
polygon(c(cov.devel,rev(cov.devel)),c(s.devel.low,rev(s.devel.high)),
        col= adjustcolor("red", 0.3),lty=0)
for (i in 1:712) {
  lines(x=rep(Occ.scaled.mat[i,"percent.developed"],2), y=c(-.03,-0.07)) 
}
axis(2,at=c(0,.25,.5,.75,1),mgp=c(0,.5,0))
axis(1,at=seq(from=cov.devel[1],to=cov.devel[200],length=3),labels=round(seq(from=label.devel[1],to=label.devel[200],length=3)*100,0),mgp=c(0,.6,0))
text(x=par("usr")[1]+(diff(par("usr")[1:2])*.005),y=.95,pos=4,"\u03b2= \u22120.21 (\u22120.52, 0.01)",cex=1.25)

plot(x=cov.devel, y=marg.devel.coy, type = "l",,lty=1,col="black",ylim=c(-.08,1),yaxt="n",ylab = "",xlab = "",xaxt="n")
abline(v=0, lty=2, col="lightgray")
polygon(c(cov.devel,rev(cov.devel)),c(c.devel.low,rev(c.devel.high)),
        col= adjustcolor("lightgreen", 0.3),lty=0)
for (i in 1:712) {
  lines(x=rep(Occ.scaled.mat[i,"percent.developed"],2), y=c(-.03,-0.07)) 
}
axis(2,at=c(0,.25,.5,.75,1),labels = FALSE)
axis(1,at=seq(from=cov.devel[1],to=cov.devel[200],length=3),labels=round(seq(from=label.devel[1],to=label.devel[200],length=3)*100,0),mgp=c(0,.6,0))
text(x=7.5, y=-.385, "% Developed Land",xpd=NA, cex=1.6)
text(x=9.75,y=.05,"\u03b2= 3.77 (1.72, 6.54)",cex=1.25, font=2)

plot(x=cov.devel, y=marg.devel.bob, type = "l",,lty=2,col="black",ylim=c(-.08,1),yaxt="n",ylab = "",xlab = "",xaxt="n")
abline(v=0, lty=2, col="lightgray")
polygon(c(cov.devel,rev(cov.devel)),c(b.devel.low,rev(b.devel.high)),
        col= adjustcolor("gray", 0.3),lty=0)
for (i in 1:712) {
  lines(x=rep(Occ.scaled.mat[i,"percent.developed"],2), y=c(-.03,-0.07)) 
}
axis(2,at=c(0,.25,.5,.75,1),labels = FALSE)
axis(1,at=seq(from=cov.devel[1],to=cov.devel[200],length=3),labels=round(seq(from=label.devel[1],to=label.devel[200],length=3)*100,0),mgp=c(0,.6,0))
text(x=par("usr")[1]+(diff(par("usr")[1:2])*.005),y=.95,pos=4,"\u03b2= 0.02 (\u22120.51, 0.96)",cex=1.25)

########### Elevation #########
plot(x=cov.elev, y=marg.elev.skunk, type = "l",,lty=1,col="black",yaxt="n",ylim=c(-.08,1),xlab = "",xaxt="n")
abline(v=0, lty=2, col="lightgray")
polygon(c(cov.elev,rev(cov.elev)),c(s.elev.low,rev(s.elev.high)),
        col= adjustcolor("red", 0.3),lty=0)
for (i in 1:712) {
  lines(x=rep(Occ.scaled.mat[i,"elevation"],2), y=c(-.03,-0.07)) 
}
axis(2,at=c(0,.25,.5,.75,1),mgp=c(0,.5,0))
axis(1,at=seq(from=cov.elev[25],to=cov.elev[175],length=3),labels=round(seq(from=label.elev[25],to=label.elev[175],length=3),0),mgp=c(0,.6,0))
text(x=par("usr")[1]+(diff(par("usr")[1:2])*.005),y=.95,pos=4,"\u03b2= \u22121.26 (\u22121.55, \u22121.00)",cex=1.25, font=2)

plot(x=cov.elev, y=marg.elev.coy, type = "l",,lty=1,col="black",ylim=c(-.08,1),yaxt="n",ylab = "",xlab = "",xaxt="n")
abline(v=0, lty=2, col="lightgray")
polygon(c(cov.elev,rev(cov.elev)),c(c.elev.low,rev(c.elev.high)),
        col= adjustcolor("red", 0.3),lty=0)
for (i in 1:712) {
  lines(x=rep(Occ.scaled.mat[i,"elevation"],2), y=c(-.03,-0.07)) 
}
axis(2,at=c(0,.25,.5,.75,1),labels = FALSE)
axis(1,at=seq(from=cov.elev[25],to=cov.elev[175],length=3),labels=round(seq(from=label.elev[25],to=label.elev[175],length=3),0),mgp=c(0,.6,0))
text(x=1,y=-.385,"Elevation (m)",xpd=NA,cex=1.6)
text(x=par("usr")[1]+(diff(par("usr")[1:2])*.005),y=.95,pos=4,"\u03b2= \u22120.45 (\u22120.75, \u22120.16)",cex=1.25, font=2)

plot(x=cov.elev, y=marg.elev.bob, type = "l",,lty=2,col="black",ylim=c(-.08,1),yaxt="n",ylab = "",xlab = "",xaxt="n")
abline(v=0, lty=2, col="lightgray")
polygon(c(cov.elev,rev(cov.elev)),c(b.elev.low,rev(b.elev.high)),
        col= adjustcolor("gray", 0.3),lty=0)
for (i in 1:712) {
  lines(x=rep(Occ.scaled.mat[i,"elevation"],2), y=c(-.03,-0.07)) 
}
axis(2,at=c(0,.25,.5,.75,1),labels = FALSE)
axis(1,at=seq(from=cov.elev[25],to=cov.elev[175],length=3),labels=round(seq(from=label.elev[25],to=label.elev[175],length=3),0),mgp=c(0,.6,0))
text(x=par("usr")[1]+(diff(par("usr")[1:2])*.005),y=.95,pos=4,"\u03b2= \u22120.03 (\u22120.41, 0.39)",cex=1.25)

########### Precip ##########
plot(x=cov.precip, y=marg.precip.skunk, type = "l",,lty=1,col="black",yaxt="n",ylim=c(-.08,1),xlab = "",xaxt="n")
abline(v=0, lty=2, col="lightgray")
polygon(c(cov.precip,rev(cov.precip)),c(s.precip.low,rev(s.precip.high)),
        col= adjustcolor("lightgreen", 0.3),lty=0)
for (i in 1:712) {
  lines(x=rep(Occ.scaled.mat[i,"precip_mm"],2), y=c(-.03,-0.07)) 
}
axis(2,at=c(0,.25,.5,.75,1),mgp=c(0,.5,0))
axis(1,at=seq(from=cov.precip[25],to=cov.precip[175],length=3),labels=round(seq(from=label.precip[25],to=label.precip[175],length=3),0),mgp=c(0,.6,0))
text(x=par("usr")[1]+(diff(par("usr")[1:2])*.005),y=.95,pos=4, "\u03b2= 0.10 (\u22120.05, 0.27)",cex=1.25)

plot(x=cov.precip, y=marg.precip.coy, type = "l",,lty=1,col="black",ylim=c(-.08,1),yaxt="n",ylab = "",xlab = "",xaxt="n")
abline(v=0, lty=2, col="lightgray")
polygon(c(cov.precip,rev(cov.precip)),c(c.precip.low,rev(c.precip.high)),
        col= adjustcolor("lightgreen", 0.3),lty=0)
for (i in 1:712) {
  lines(x=rep(Occ.scaled.mat[i,"precip_mm"],2), y=c(-.03,-0.07)) 
}
axis(2,at=c(0,.25,.5,.75,1),labels = FALSE)
axis(1,at=seq(from=cov.precip[25],to=cov.precip[175],length=3),labels=round(seq(from=label.precip[25],to=label.precip[175],length=3),0),mgp=c(0,.6,0))
text(x=0.25,y=-.385,"Precipitation (mm)",xpd=NA, cex=1.6)
text(x=par("usr")[1]+(diff(par("usr")[1:2])*.005),y=.95,pos=4,"\u03b2= 0.45 (0.18, 0.74)",cex=1.25, font=2)

plot(x=cov.precip, y=marg.precip.bob, type = "l",,lty=1,col="black",ylim=c(-.08,1),yaxt="n",ylab = "",xlab = "",xaxt="n")
abline(v=0, lty=2, col="lightgray")
polygon(c(cov.precip,rev(cov.precip)),c(b.precip.low,rev(b.precip.high)),
        col= adjustcolor("lightgreen", 0.3),lty=0)
for (i in 1:712) {
  lines(x=rep(Occ.scaled.mat[i,"precip_mm"],2), y=c(-.03,-0.07)) 
}
axis(2,at=c(0,.25,.5,.75,1),labels = FALSE)
axis(1,at=seq(from=cov.precip[25],to=cov.precip[175],length=3),labels=round(seq(from=label.precip[25],to=label.precip[175],length=3),0),mgp=c(0,.6,0))
text(x=par("usr")[1]+(diff(par("usr")[1:2])*.005),y=.95,pos=4,"\u03b2= 0.28 (\u22120.06, 0.76)",cex=1.25)
dev.off()


#################################################################################
#################################################################################
###########                                                   ###################
###########           DETECTION PROBABILITIES                 ###################
###########                                                   ###################
################################################################################# 
#################################################################################
#Create the Detection probability Figure

#create data frames to hold MCMC chain values
d.bobcat <- data.frame(pb0=rep(NA,270000),
                       camera=rep(NA,270000),
                       moon=rep(NA,270000))

d.coyote <- data.frame(pc0=rep(NA,270000),
                       camera=rep(NA,270000),
                       moon=rep(NA,270000))

d.skunk <- data.frame(ps0=rep(NA,270000),
                      camera=rep(NA,270000),
                      moon=rep(NA,270000))

#get all the mcmc values into dataframes

##### BOBCAT #####

#intercept
colnames(none$samples$chain1)[17]
d.bobcat$pb0 <- c(none$samples$chain1[,17],
                  none$samples$chain2[,17],
                  none$samples$chain3[,17])

#camera days
colnames(none$samples$chain1)[15]
d.bobcat$camera <- c(none$samples$chain1[,15],
                     none$samples$chain2[,15],
                     none$samples$chain3[,15])

#moon illumination
colnames(none$samples$chain1)[16]
d.bobcat$moon <- c(none$samples$chain1[,16],
                   none$samples$chain2[,16],
                   none$samples$chain3[,16])

######### COYOTE #############

#intercept
colnames(none$samples$chain1)[20]
d.coyote$pc0 <- c(none$samples$chain1[,20],
                  none$samples$chain2[,20],
                  none$samples$chain3[,20])

#camera days
colnames(none$samples$chain1)[18]
d.coyote$camera <- c(none$samples$chain1[,18],
                     none$samples$chain2[,18],
                     none$samples$chain3[,18])

#moon illumination
colnames(none$samples$chain1)[19]
d.coyote$moon <- c(none$samples$chain1[,19],
                   none$samples$chain2[,19],
                   none$samples$chain3[,19])

############# SKUNK #################

#intercept
colnames(none$samples$chain1)[26]
d.skunk$ps0 <- c(none$samples$chain1[,26],
                 none$samples$chain2[,26],
                 none$samples$chain3[,26])

#camera days
colnames(none$samples$chain1)[24]
d.skunk$camera <- c(none$samples$chain1[,24],
                    none$samples$chain2[,24],
                    none$samples$chain3[,24])

#moon illumination
colnames(none$samples$chain1)[25]
d.skunk$moon <- c(none$samples$chain1[,25],
                  none$samples$chain2[,25],
                  none$samples$chain3[,25])
################################################################################
# create range of covariate values
#same process find range of covariate values observed across surveys then divide 
#that range into 200 points to graph over
stations <- read.csv("site_stations_with_detection_covariates.csv")

############### moon illumination ################
range(stations$moon)
tmp <- range(stations$moon.scaled)
mean.moon <- mean(stations$moon)
sd.moon <- sd(stations$moon)
#range: 3.4852 - 543.2734
#scaled range: -1.533901 - 2.170825
cov.moon <- seq(from=tmp[1], to=tmp[2], length=200)
label.moon <- (cov.moon * sd.moon) + mean.moon

############### camera days #####################

mean.cam <- mean(stations$Camera_Days_Good)
sd.cam <- sd(stations$Camera_Days_Good)
range(stations$Camera_Days_Good)
tmp <- range(stations$camera.days.scaled)
#range = 0 - 29
#scaled range = -4.659845 - 5.154952
cov.cam <- seq(from=tmp[1], to=tmp[2], length=200)
label.cam <- (cov.cam * sd.cam) + mean.cam

########
intercept <- rep(1,200)

d.covs <- cbind(intercept,cov.cam,cov.moon)

################################################################################
################################################################################
##### Bobcat detection probability #####
#just the intercept (average conditions for both covariates)
#reported in the paper as average bobcat detection probability
b.int <- plogis(d.bobcat$pb0)
quantile(b.int,c(0.025,.5,0.975)) #point est and 95% CI

# Camera days
b.camera.logit <- cbind(d.bobcat$pb0,d.bobcat$camera) %*% t(d.covs[,1:2])
b.camera.prob <- plogis(b.camera.logit)

b.camera.quant <- matrix(NA,nrow=3, ncol=200)
for (i in 1:ncol(b.camera.prob)) {
  b.camera.quant[,i] <- quantile(b.camera.prob[,i],c(.025,.5,.975))
}

#Moon illumination
b.moon.logit <- cbind(d.bobcat$pb0,d.bobcat$moon) %*% t(d.covs[,c(1,3)])
b.moon.prob <- plogis(b.moon.logit)

b.moon.quant <- matrix(NA,nrow=3, ncol=200)
for (i in 1:ncol(b.moon.prob)) {
  b.moon.quant[,i] <- quantile(b.moon.prob[,i],c(.025,.5,.975))
}

##### Coyote detection prediction #####
#just the intercept (average conditions for both covariates)
#reported in paper as average detection probability of coyotes
c.int <- plogis(d.coyote$pc0)
quantile(c.int,c(0.025,.5,0.975)) #point est and 95% CI

#camera days
c.camera.logit <- cbind(d.coyote$pc0,d.coyote$camera) %*% t(d.covs[,1:2])
c.camera.prob <- plogis(c.camera.logit)

c.camera.quant <- matrix(NA,nrow=3, ncol=200)
for (i in 1:ncol(c.camera.prob)) {
  c.camera.quant[,i] <- quantile(c.camera.prob[,i],c(.025,.5,.975))
}

#moon Illumination
c.moon.logit <- cbind(d.coyote$pc0,d.coyote$moon) %*% t(d.covs[,c(1,3)])
c.moon.prob <- plogis(c.moon.logit)

c.moon.quant <- matrix(NA,nrow=3, ncol=200)
for (i in 1:ncol(c.moon.prob)) {
  c.moon.quant[,i] <- quantile(c.moon.prob[,i],c(.025,.5,.975))
}

##### Skunk detection prediction #####
#just the intercept (average conditions for covariates)
#reported in paper as average detection probability of skunks
s.int <- plogis(d.skunk$ps0)
quantile(s.int,c(0.025,.5,0.975)) #point est and 95% CI

#Camera days
s.camera.logit <- cbind(d.skunk$ps0,d.skunk$camera) %*% t(d.covs[,1:2])
s.camera.prob <- plogis(s.camera.logit)

s.camera.quant <- matrix(NA,nrow=3, ncol=200)
for (i in 1:ncol(s.camera.prob)) {
  s.camera.quant[,i] <- quantile(s.camera.prob[,i],c(.025,.5,.975))
}

#moon illumination
s.moon.logit <- cbind(d.skunk$ps0,d.skunk$moon) %*% t(d.covs[,c(1,3)])
s.moon.prob <- plogis(s.moon.logit)

s.moon.quant <- matrix(NA,nrow=3, ncol=200)
for (i in 1:ncol(s.moon.prob)) {
  s.moon.quant[,i] <- quantile(s.moon.prob[,i],c(.025,.5,.975))
}

#get coeffecient effects for the labels in figure
colnames(none$samples$chain1)[1:50] 

tmp <- grep("camera",colnames(none$samples$chain1))
none$summary$all.chains[tmp,]

tmp <- grep("moon",colnames(none$samples$chain1))
none$summary$all.chains[tmp,]

none$summary$all.chains[1:50,]

#################################################################################
####### DETECTION PROBABILITY PLOTS ##############################################
#################################################################################
################################################################################
################################################################################
##########                                                         #############
##########             2 X 3 Plot with RUG for Paper               #############
##########                                                         #############
################################################################################
################################################################################

  ############## Beta Coeffecient labels are hard coded in figure. may need to change if models are rerun

#pdf(height=6, width=6,"Figures/Detection Covariates_twoXthree_shrinkage.pdf")
png("Figures/detection_covariates_twoxthree_shrinkage_RUG.png", units = "in", height=6, 
    width = 6, res=300 )
par(mfrow=c(2,3),
    oma=c(.15,3.75,3,.5))
par(mar=c(4.25,.5,0,0))
par(family="serif")
par(axis.cex=1.25)
##############  Camera Days ################
plot(x=d.covs[,2],y=s.camera.quant[2,],type="l", xlab= "",
     ylab="",ylim = c(-.04,1),yaxt="n",xaxt="n")
polygon(c(d.covs[,2],rev(d.covs[,2])),c(s.camera.quant[1,],rev(s.camera.quant[3,])),
        col= adjustcolor("lightgreen", 0.3),lty=0)
for (i in 1:2709) {
  lines(x=rep((stations$Camera_Days_Good[i]-mean.cam)/sd.cam,2), y=c(-.01,-.05))
}

abline(v=0,col="lightgray",lty=2)
axis(side=1, at=c(((3-mean.cam)/sd.cam),((14-mean.cam)/sd.cam),((25-mean.cam)/sd.cam)),labels = c(3,14,25),mgp=c(0,.75,0),cex.axis=1.35)
axis(side=2, at=c(0,.25,.5,.75,1), labels=c("0.0","0.25","0.50","0.75","1.00"),mgp=c(0,.65,0),cex.axis=1.35)
text(x=0,y=1.1,"Western Spotted Skunk",xpd=NA,cex=1.75)
text(x=par("usr")[1]+(diff(par("usr")[1:2])*.005),y=.99,pos=4,"\u03b2= 0.20 (0.07, 0.33)",cex=1.25, font=2)


plot(x=d.covs[,2],y=c.camera.quant[2,],type="l", xlab= "",
     ylab="",ylim = c(-.04,1),yaxt="n",xaxt="n")
polygon(c(d.covs[,2],rev(d.covs[,2])),c(c.camera.quant[1,],rev(c.camera.quant[3,])),
        col= adjustcolor("lightgreen", 0.3),lty=0)
for (i in 1:2709) {
  lines(x=rep((stations$Camera_Days_Good[i]-mean.cam)/sd.cam,2), y=c(-.01,-.05))
}
abline(v=0,col="lightgray",lty=2)
axis(side=1, at=c(((3-mean.cam)/sd.cam),((14-mean.cam)/sd.cam),((25-mean.cam)/sd.cam)),labels = c(3,14,25),mgp=c(0,.75,0),cex.axis=1.35)
axis(side=2, at=c(0,.25,.5,.75,1), labels=FALSE)
text(x=0,y=1.1,"Coyote",xpd=NA,cex=1.75)
text(x=0,y=-.24,"Days Camera Active",xpd=NA,cex=1.7)
text(x=par("usr")[1]+(diff(par("usr")[1:2])*.005),y=.99,pos=4,"\u03b2= 0.51 (0.25, 0.82)",cex=1.25, font=2)

plot(x=d.covs[,2],y=b.camera.quant[2,],type="l", xlab= "",
     ylab="Detection Probability",ylim = c(-.04,1),yaxt="n",xaxt="n",lty=1)
polygon(c(d.covs[,2],rev(d.covs[,2])),c(b.camera.quant[1,],rev(b.camera.quant[3,])),
        col= adjustcolor("lightgreen", 0.3),lty=0)
for (i in 1:2709) {
  lines(x=rep((stations$Camera_Days_Good[i]-mean.cam)/sd.cam,2), y=c(-.01,-.05))
}
abline(v=0,col="lightgray",lty=2)
axis(side=1, at=c(((3-mean.cam)/sd.cam),((14-mean.cam)/sd.cam),((25-mean.cam)/sd.cam)),labels = c(3,14,25),mgp=c(0,.75,0),cex.axis=1.35)
axis(side=2, at=c(0,.25,.5,.75,1), labels=FALSE)
text(x=0,y=1.1,"Bobcat",xpd=NA,cex=1.75)
text(x=par("usr")[1]+(diff(par("usr")[1:2])*.005),y=.99,pos=4,"\u03b2= 0.08 (\u22120.06, 0.26)",cex=1.25)

###########  Moon Illumination #######

plot(x=d.covs[,3],y=s.moon.quant[2,],type="l", xlab= "",
     ylab="",ylim = c(-.04,1),yaxt="n",xaxt="n")
polygon(c(d.covs[,3],rev(d.covs[,3])),c(s.moon.quant[1,],rev(s.moon.quant[3,])),
        col= adjustcolor("lightgreen", 0.3),lty=0)
for (i in 1:2709) {
  lines(x=rep((stations$moon[i]-mean.moon)/sd.moon,2), y=c(-.01,-.05))
}
abline(v=0,col="lightgray",lty=2)
axis(side=1, at=c(((70-mean.moon)/sd.moon),((264-mean.moon)/sd.moon),((459-mean.moon)/sd.moon)),labels = c(70,264,459),mgp=c(0,.75,0),cex.axis=1.35)
axis(side=2, at=c(0,.25,.5,.75,1), labels=c("0.0","0.25","0.50","0.75","1.00"),mgp=c(0,.65,0),cex.axis=1.35)
text(x=par("usr")[1]+(diff(par("usr")[1:2])*.005),y=.99,pos=4,"\u03b2= 0.08 (\u22120.03, 0.21)",cex=1.25)

plot(x=d.covs[,3],y=c.moon.quant[2,],type="l", xlab= "",
     ylab="",ylim = c(-.04,1),yaxt="n",xaxt="n",lty=1)
polygon(c(d.covs[,3],rev(d.covs[,3])),c(c.moon.quant[1,],rev(c.moon.quant[3,])),
        col= adjustcolor("lightgreen", 0.3),lty=0)
for (i in 1:2709) {
  lines(x=rep((stations$moon[i]-mean.moon)/sd.moon,2), y=c(-.01,-.05))
}
abline(v=0,col="lightgray",lty=2)
axis(side=1, at=c(((70-mean.moon)/sd.moon),((264-mean.moon)/sd.moon),((459-mean.moon)/sd.moon)),labels = c(70,264,459),mgp=c(0,.75,0),cex.axis=1.35)
axis(side=2, at=c(0,.25,.5,.75,1), labels=FALSE)
text(x=par("usr")[1]+(diff(par("usr")[1:2])*.005),y=.99,pos=4,"\u03b2= 0.07 (\u22120.07, 0.23)",cex=1.25)
text(x=0,y=-.24,"Average Moon Illuminance",xpd=NA,cex=1.7)

plot(x=d.covs[,3],y=b.moon.quant[2,],type="l", xlab= "",
     ylab="Detection Probability",ylim = c(-.04,1),yaxt="n",xaxt="n",lty=2)
polygon(c(d.covs[,3],rev(d.covs[,3])),c(b.moon.quant[1,],rev(b.moon.quant[3,])),
        col= adjustcolor("gray", 0.3),lty=0)
for (i in 1:2709) {
  lines(x=rep((stations$moon[i]-mean.moon)/sd.moon,2), y=c(-.01,-.05))
}
abline(v=0,col="lightgray",lty=2)
axis(side=1, at=c(((70-mean.moon)/sd.moon),((264-mean.moon)/sd.moon),((459-mean.moon)/sd.moon)),labels = c(70,264,459),mgp=c(0,.75,0),cex.axis=1.35)
axis(side=2, at=c(0,.25,.5,.75,1), labels=FALSE)
mtext("Detection Probability",side=2,line=1.5,outer = TRUE,cex=1.25)
text(x=par("usr")[1]+(diff(par("usr")[1:2])*.005),y=.99,pos=4,"\u03b2= 0.04 (\u22120.08, 0.18)",cex=1.25)
dev.off()
#################################################################################