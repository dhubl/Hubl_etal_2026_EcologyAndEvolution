#Must run the covariate and caphist prep script to have all data needed to run
#the NIMBLE model

#this model has full covariates on the second order natural parameters

##this model developed from:
#Rota, C. T., M. A. R. Ferreira, R. W. Kays, T. D. Forrester, E. L. Kalies, 
    #W. J. McShea, A. W. Parsons, and J. J. Millspaugh. 2016. A multispecies 
    #occupancy model for two or more interacting species. Methods in Ecology and Evolution 7:1164

###############
#D Hubl
#2026
###############

### Full Interactions Model ##################################################################################
MS.full <- nimbleCode({  
  #Likelihood   
  #t=time/rep
  #j=site
  #k=survey replicate
  
  #for first Rep of all sites (Season 1)
  for (t in 1:rep) {
    for (j in 1:nsites[t]) {
      z[t,j] ~ dcat(psi[t,j,1:8])    #z[time t ,site j] ~ psi[time t, site j,latent state options]
    }#j
  }#t
  #observation process
  for (t in 1:rep) {
    for (j in 1:nsites[t]) {
      for (k in 1:n.occ) {
        y[t,j,k] ~ dcat(obs[t,j,k,z[t,j],1:8]) #observation data is distributed categorical with probability obs[time t, site j, survey occassion k, latent state, observation state]
      }#k
    }#j
  }#t
  
  #for the first season the natural parameters do not have an autoregression and so need to be separated  
  for (t in 1:rep) {
    for (j in 1:nsites[t]) {
      psi[t,j,1] <- 1 / (1 + exp(f1[t,j]) + exp(f2[t,j]) + exp(f3[t,j]) + exp(f1[t,j] + f2[t,j] + f12[t,j]) + exp(f1[t,j] + f3[t,j] + f13[t,j]) + exp(f2[t,j] + f3[t,j] + f23[t,j]) + exp(f1[t,j] + f2[t,j] + f3[t,j] + f12[t,j] + f13[t,j] + f23[t,j]))
      psi[t,j,2] <- exp(f1[t,j]) / (1 + exp(f1[t,j]) + exp(f2[t,j]) + exp(f3[t,j]) + exp(f1[t,j] + f2[t,j] + f12[t,j]) + exp(f1[t,j] + f3[t,j] + f13[t,j]) + exp(f2[t,j] + f3[t,j] + f23[t,j]) + exp(f1[t,j] + f2[t,j] + f3[t,j] + f12[t,j] + f13[t,j] + f23[t,j]))
      psi[t,j,3] <- exp(f2[t,j]) / (1 + exp(f1[t,j]) + exp(f2[t,j]) + exp(f3[t,j]) + exp(f1[t,j] + f2[t,j] + f12[t,j]) + exp(f1[t,j] + f3[t,j] + f13[t,j]) + exp(f2[t,j] + f3[t,j] + f23[t,j]) + exp(f1[t,j] + f2[t,j] + f3[t,j] + f12[t,j] + f13[t,j] + f23[t,j]))
      psi[t,j,4] <- exp(f3[t,j]) / (1 + exp(f1[t,j]) + exp(f2[t,j]) + exp(f3[t,j]) + exp(f1[t,j] + f2[t,j] + f12[t,j]) + exp(f1[t,j] + f3[t,j] + f13[t,j]) + exp(f2[t,j] + f3[t,j] + f23[t,j]) + exp(f1[t,j] + f2[t,j] + f3[t,j] + f12[t,j] + f13[t,j] + f23[t,j]))
      psi[t,j,5] <- exp(f1[t,j] + f2[t,j] + f12[t,j]) / (1 + exp(f1[t,j]) + exp(f2[t,j]) + exp(f3[t,j]) + exp(f1[t,j] + f2[t,j] + f12[t,j]) + exp(f1[t,j] + f3[t,j] + f13[t,j]) + exp(f2[t,j] + f3[t,j] + f23[t,j]) + exp(f1[t,j] + f2[t,j] + f3[t,j] + f12[t,j] + f13[t,j] + f23[t,j]))
      psi[t,j,6] <- exp(f1[t,j] + f3[t,j] + f13[t,j]) / (1 + exp(f1[t,j]) + exp(f2[t,j]) + exp(f3[t,j]) + exp(f1[t,j] + f2[t,j] + f12[t,j]) + exp(f1[t,j] + f3[t,j] + f13[t,j]) + exp(f2[t,j] + f3[t,j] + f23[t,j]) + exp(f1[t,j] + f2[t,j] + f3[t,j] + f12[t,j] + f13[t,j] + f23[t,j]))
      psi[t,j,7] <- exp(f2[t,j] + f3[t,j] + f23[t,j]) / (1 + exp(f1[t,j]) + exp(f2[t,j]) + exp(f3[t,j]) + exp(f1[t,j] + f2[t,j] + f12[t,j]) + exp(f1[t,j] + f3[t,j] + f13[t,j]) + exp(f2[t,j] + f3[t,j] + f23[t,j]) + exp(f1[t,j] + f2[t,j] + f3[t,j] + f12[t,j] + f13[t,j] + f23[t,j]))
      psi[t,j,8] <- exp(f1[t,j] + f2[t,j] + f3[t,j] + f12[t,j] + f13[t,j] + f23[t,j]) / (1 + exp(f1[t,j]) + exp(f2[t,j]) + exp(f3[t,j]) + exp(f1[t,j] + f2[t,j] + f12[t,j]) + exp(f1[t,j] + f3[t,j] + f13[t,j]) + exp(f2[t,j] + f3[t,j] + f23[t,j]) + exp(f1[t,j] + f2[t,j] + f3[t,j] + f12[t,j] + f13[t,j] + f23[t,j]))
    }#j
  }#t
  
  #natural parameters bring in covariates to occupancy
  #covariates are all standardized
  for (j in 1:nsites[1]) { #first season
    
    f1[1,j]  <- s0  + s.elevation*Occ.scaled.mat[j,1]  + s.precip*Occ.scaled.mat[j,2]  + s.edge*Occ.scaled.mat[j,3]  + s.density*Occ.scaled.mat[j,4]  + s.devel*Occ.scaled.mat[j,5]  + s.road*Occ.scaled.mat[j,6]  # skunk
    f2[1,j]  <- c0  + c.elevation*Occ.scaled.mat[j,1]  + c.precip*Occ.scaled.mat[j,2]  + c.edge*Occ.scaled.mat[j,3]  + c.density*Occ.scaled.mat[j,4]  + c.devel*Occ.scaled.mat[j,5]  + c.road*Occ.scaled.mat[j,6]  # coyote
    f3[1,j]  <- b0  + b.elevation*Occ.scaled.mat[j,1]  + b.precip*Occ.scaled.mat[j,2]  + b.edge*Occ.scaled.mat[j,3]  + b.density*Occ.scaled.mat[j,4]  + b.devel*Occ.scaled.mat[j,5]  + b.road*Occ.scaled.mat[j,6]  # bobcat
    f12[1,j] <- sc0 + sc.elevation*Occ.scaled.mat[j,1] + sc.precip*Occ.scaled.mat[j,2] + sc.edge*Occ.scaled.mat[j,3] + sc.density*Occ.scaled.mat[j,4] + sc.devel*Occ.scaled.mat[j,5] + sc.road*Occ.scaled.mat[j,6] # skunk x coyote interaction
    f13[1,j] <- sb0 + sb.elevation*Occ.scaled.mat[j,1] + sb.precip*Occ.scaled.mat[j,2] + sb.edge*Occ.scaled.mat[j,3] + sb.density*Occ.scaled.mat[j,4] + sb.devel*Occ.scaled.mat[j,5] + sb.road*Occ.scaled.mat[j,6] # skunk x bobcat interaction
    f23[1,j] <- cb0 + cb.elevation*Occ.scaled.mat[j,1] + cb.precip*Occ.scaled.mat[j,2] + cb.edge*Occ.scaled.mat[j,3] + cb.density*Occ.scaled.mat[j,4] + cb.devel*Occ.scaled.mat[j,5] + cb.road*Occ.scaled.mat[j,6] # coyote x bobcat interaction
    
  }#j
  
  #Autoregression function requires a matrix indicating 1 or 0 if each species is present for each latent state.    
  #species are the columns skunk, coyote, bobcat
  #latent states are the rows
  # s c b
  # 0,0,0
  # 1,0,0
  # 0,1,0
  # 0,0,1
  # 1,1,0
  # 1,0,1
  # 0,1,1
  # 1,1,1  
  
  for (j in 1:nsites[2]) { #season 2 with autoregression function
    
    f1[2,j] <- phi.s*species.mat[z[1,j],1] + s0  + s.elevation*Occ.scaled.mat[j,1]  + s.precip*Occ.scaled.mat[j,2]  + s.edge*Occ.scaled.mat[j,3]  + s.density*Occ.scaled.mat[j,4]  + s.devel*Occ.scaled.mat[j,5]  + s.road*Occ.scaled.mat[j,6]  # skunk
    f2[2,j] <- phi.c*species.mat[z[1,j],2] + c0  + c.elevation*Occ.scaled.mat[j,1]  + c.precip*Occ.scaled.mat[j,2]  + c.edge*Occ.scaled.mat[j,3]  + c.density*Occ.scaled.mat[j,4]  + c.devel*Occ.scaled.mat[j,5]  + c.road*Occ.scaled.mat[j,6]  # coyote
    f3[2,j] <- phi.b*species.mat[z[1,j],3] + b0  + b.elevation*Occ.scaled.mat[j,1]  + b.precip*Occ.scaled.mat[j,2]  + b.edge*Occ.scaled.mat[j,3]  + b.density*Occ.scaled.mat[j,4]  + b.devel*Occ.scaled.mat[j,5]  + b.road*Occ.scaled.mat[j,6]  # bobcat
    f12[2,j] <- sc0 + sc.elevation*Occ.scaled.mat[j,1] + sc.precip*Occ.scaled.mat[j,2] + sc.edge*Occ.scaled.mat[j,3] + sc.density*Occ.scaled.mat[j,4] + sc.devel*Occ.scaled.mat[j,5] + sc.road*Occ.scaled.mat[j,6] # skunk x coyote interaction
    f13[2,j] <- sb0 + sb.elevation*Occ.scaled.mat[j,1] + sb.precip*Occ.scaled.mat[j,2] + sb.edge*Occ.scaled.mat[j,3] + sb.density*Occ.scaled.mat[j,4] + sb.devel*Occ.scaled.mat[j,5] + sb.road*Occ.scaled.mat[j,6] # skunk x bobcat interaction
    f23[2,j] <- cb0 + cb.elevation*Occ.scaled.mat[j,1] + cb.precip*Occ.scaled.mat[j,2] + cb.edge*Occ.scaled.mat[j,3] + cb.density*Occ.scaled.mat[j,4] + cb.devel*Occ.scaled.mat[j,5] + cb.road*Occ.scaled.mat[j,6] # coyote x bobcat interaction
    
  }#j
  
  #observation model
  # 
  
  #
  for (t in 1:rep){
    for(j in 1:nsites[t]){
      for(k in 1:n.occ){
        #given state 1=no occupancy
        obs[t,j,k,1,1] <- 1      
        obs[t,j,k,1,2] <- 0 
        obs[t,j,k,1,3] <- 0
        obs[t,j,k,1,4] <- 0
        obs[t,j,k,1,5] <- 0
        obs[t,j,k,1,6] <- 0
        obs[t,j,k,1,7] <- 0
        obs[t,j,k,1,8] <- 0
        
        #given state2= skunk only
        obs[t,j,k,2,1] <- 1 - ps[t,j,k]
        obs[t,j,k,2,2] <-     ps[t,j,k] 
        obs[t,j,k,2,3] <- 0
        obs[t,j,k,2,4] <- 0 
        obs[t,j,k,2,5] <- 0
        obs[t,j,k,2,6] <- 0
        obs[t,j,k,2,7] <- 0
        obs[t,j,k,2,8] <- 0
        
        #given state 3= coyotes only
        obs[t,j,k,3,1] <- 1 - pc[t,j,k]
        obs[t,j,k,3,2] <- 0
        obs[t,j,k,3,3] <-     pc[t,j,k]
        obs[t,j,k,3,4] <- 0
        obs[t,j,k,3,5] <- 0
        obs[t,j,k,3,6] <- 0
        obs[t,j,k,3,7] <- 0
        obs[t,j,k,3,8] <- 0
        
        #given state 4= bobcats only
        obs[t,j,k,4,1] <- 1 - pb[t,j,k]
        obs[t,j,k,4,2] <- 0
        obs[t,j,k,4,3] <- 0
        obs[t,j,k,4,4] <-     pb[t,j,k] 
        obs[t,j,k,4,5] <- 0
        obs[t,j,k,4,6] <- 0
        obs[t,j,k,4,7] <- 0
        obs[t,j,k,4,8] <- 0
        
        #given state 5 = skunk and coyote
        obs[t,j,k,5,1] <- (1 - ps[t,j,k]) * (1 - pc[t,j,k])
        obs[t,j,k,5,2] <-      ps[t,j,k]  * (1 - pc[t,j,k])
        obs[t,j,k,5,3] <- (1 - ps[t,j,k]) *      pc[t,j,k]
        obs[t,j,k,5,4] <- 0  
        obs[t,j,k,5,5] <-      ps[t,j,k]  *      pc[t,j,k]
        obs[t,j,k,5,6] <- 0
        obs[t,j,k,5,7] <- 0
        obs[t,j,k,5,8] <- 0
        
        #given state 6 = skunk and bobcat
        obs[t,j,k,6,1] <- (1 - ps[t,j,k]) * (1 - pb[t,j,k])
        obs[t,j,k,6,2] <-      ps[t,j,k]  * (1 - pb[t,j,k])
        obs[t,j,k,6,3] <- 0 
        obs[t,j,k,6,4] <- (1 - ps[t,j,k]) *      pb[t,j,k]
        obs[t,j,k,6,5] <- 0
        obs[t,j,k,6,6] <-      ps[t,j,k]  *      pb[t,j,k]
        obs[t,j,k,6,7] <- 0
        obs[t,j,k,6,8] <- 0
        
        #given state 7 = coyote and bobcat
        obs[t,j,k,7,1] <- (1 - pc[t,j,k]) * (1 - pb[t,j,k])
        obs[t,j,k,7,2] <- 0 
        obs[t,j,k,7,3] <-      pc[t,j,k]  * (1 - pb[t,j,k])
        obs[t,j,k,7,4] <- (1 - pc[t,j,k]) *      pb[t,j,k]
        obs[t,j,k,7,5] <- 0
        obs[t,j,k,7,6] <- 0
        obs[t,j,k,7,7] <-      pc[t,j,k]  *      pb[t,j,k]
        obs[t,j,k,7,8] <- 0
        
        #given state 8 = skunk, coyote, and bobcat
        obs[t,j,k,8,1] <- (1 - ps[t,j,k]) * (1 - pc[t,j,k]) * (1 - pb[t,j,k])
        obs[t,j,k,8,2] <-      ps[t,j,k]  * (1 - pc[t,j,k]) * (1 - pb[t,j,k])
        obs[t,j,k,8,3] <- (1 - ps[t,j,k]) *      pc[t,j,k]  * (1 - pb[t,j,k])
        obs[t,j,k,8,4] <- (1 - ps[t,j,k]) * (1 - pc[t,j,k]) *      pb[t,j,k] 
        obs[t,j,k,8,5] <-      ps[t,j,k]  *      pc[t,j,k]  * (1 - pb[t,j,k])
        obs[t,j,k,8,6] <-      ps[t,j,k]  * (1 - pc[t,j,k]) *      pb[t,j,k]
        obs[t,j,k,8,7] <- (1 - ps[t,j,k]) *      pc[t,j,k]  *      pb[t,j,k]
        obs[t,j,k,8,8] <-      ps[t,j,k]  *      pc[t,j,k]  *      pb[t,j,k]
        
        
        #add covariates
        #CH.effort[] is a matrix that indicates if a survey was conducted or not, 1 if yes, 0 if no
        ps[t,j,k] <- (plogis(ps0 + ps.camera*camera.NIMBLE.det[t,j,k] + ps.moon*moon.NIMBLE.det[t,j,k])) * CH.effort[t,j,k]
        pc[t,j,k] <- (plogis(pc0 + pc.camera*camera.NIMBLE.det[t,j,k] + pc.moon*moon.NIMBLE.det[t,j,k])) * CH.effort[t,j,k]
        pb[t,j,k] <- (plogis(pb0 + pb.camera*camera.NIMBLE.det[t,j,k] + pb.moon*moon.NIMBLE.det[t,j,k])) * CH.effort[t,j,k]
      }#k
    }#j
  }#t  
  
  #Priors 
  #using shrinkage priors on all except intercepts and the autoregressive parameter
  
  ### Detection process ### 
  
  #skunk
  ps0 ~ dnorm(0,sd=1)
  ps.camera ~ dnorm(0,sd=sg.s.cam)
  ps.moon ~ dnorm(0,sd=sg.s.moon)
  
  sg.s.cam ~ dexp(1)
  sg.s.moon ~ dexp(1)
  
  #coyote
  pc0 ~ dnorm(0,sd=1)
  pc.camera ~ dnorm(0,sd=sg.c.cam)
  pc.moon ~ dnorm(0,sd=sg.c.moon)
  
  sg.c.cam ~ dexp(1)
  sg.c.moon ~ dexp(1)
  
  #bobcat
  pb0 ~ dnorm(0,sd=1)
  pb.camera ~ dnorm(0,sd=sg.b.cam)
  pb.moon ~ dnorm(0,sd=sg.b.moon)
  
  sg.b.cam ~ dexp(1)
  sg.b.moon ~ dexp(1)
  
  
  #Priors - occupancy 
  
  #skunk
  s0 ~ dnorm(0,sd=1) 
  s.elevation ~ dnorm(0,sd=sg.s.elev)
  s.precip ~ dnorm(0,sd=sg.s.prec)
  s.edge ~ dnorm(0,sd=sg.s.edge)
  s.density ~ dnorm(0,sd=sg.s.dens)
  s.devel ~ dnorm(0,sd=sg.s.dev)
  s.road ~ dnorm(0,sd=sg.s.road)
  
  sg.s.elev ~ dexp(1)
  sg.s.prec ~ dexp(1)
  sg.s.edge ~ dexp(1)
  sg.s.dens ~ dexp(1)
  sg.s.dev ~ dexp(1)
  sg.s.road ~ dexp(1)
  
  #coyote
  c0 ~ dnorm(0,sd=1)
  c.elevation ~ dnorm(0,sd=sg.c.elev)
  c.precip ~ dnorm(0,sd=sg.c.prec)
  c.edge ~ dnorm(0,sd=sg.c.edge)
  c.density ~ dnorm(0,sd=sg.c.dens)
  c.devel ~ dnorm(0,sd=sg.c.dev)
  c.road ~ dnorm(0,sd=sg.c.road)
  
  sg.c.elev ~ dexp(1)
  sg.c.prec ~ dexp(1)
  sg.c.edge ~ dexp(1)
  sg.c.dens ~ dexp(1)
  sg.c.dev ~ dexp(1)
  sg.c.road ~ dexp(1)
  
  #bobcat
  b0 ~ dnorm(0,sd=1)
  b.elevation ~ dnorm(0,sd=sg.b.elev)
  b.precip ~ dnorm(0,sd=sg.b.prec)
  b.edge ~ dnorm(0,sd=sg.b.edge)
  b.density ~ dnorm(0,sd=sg.b.dens)
  b.devel ~ dnorm(0,sd=sg.b.dev)
  b.road ~ dnorm(0,sd=sg.b.road) 
  
  sg.b.elev ~ dexp(1)
  sg.b.prec ~ dexp(1)
  sg.b.edge ~ dexp(1)
  sg.b.dens ~ dexp(1)
  sg.b.dev ~ dexp(1)
  sg.b.road ~ dexp(1)
  
  #skunk/coyote
  sc0 ~ dnorm(0,sd=1)
  sc.elevation ~ dnorm(0,sd=sg.sc.elev)
  sc.precip ~ dnorm(0,sd=sg.sc.prec)
  sc.edge ~ dnorm(0,sd=sg.sc.edge)
  sc.density ~ dnorm(0,sd=sg.sc.dens)
  sc.devel ~ dnorm(0,sd=sg.sc.dev)
  sc.road ~ dnorm(0,sd=sg.sc.road)
  
  sg.sc.elev ~ dexp(1)
  sg.sc.prec ~ dexp(1)
  sg.sc.edge ~ dexp(1)
  sg.sc.dens ~ dexp(1)
  sg.sc.dev ~ dexp(1)
  sg.sc.road ~ dexp(1)
  
  #skunk/bobcat
  sb0 ~ dnorm(0,sd=1)
  sb.elevation ~ dnorm(0,sd=sg.sb.elev)
  sb.precip ~ dnorm(0,sd=sg.sb.prec)
  sb.edge ~ dnorm(0,sd=sg.sb.edge)
  sb.density ~ dnorm(0,sd=sg.sb.dens)
  sb.devel ~ dnorm(0,sd=sg.sb.dev)
  sb.road ~ dnorm(0,sd=sg.sb.road)
  
  sg.sb.elev ~ dexp(1)
  sg.sb.prec ~ dexp(1)
  sg.sb.edge ~ dexp(1)
  sg.sb.dens ~ dexp(1)
  sg.sb.dev ~ dexp(1)
  sg.sb.road ~ dexp(1)
  
  #coyote/bobcat
  cb0 ~ dnorm(0,sd=1)
  cb.elevation ~ dnorm(0,sd=sg.cb.elev)
  cb.precip ~ dnorm(0,sd=sg.cb.prec)
  cb.edge ~ dnorm(0,sd=sg.cb.edge)
  cb.density ~ dnorm(0,sd=sg.cb.dens)
  cb.devel ~ dnorm(0,sd=sg.cb.dev)
  cb.road ~ dnorm(0,sd=sg.cb.road)
  
  sg.cb.elev ~ dexp(1)
  sg.cb.prec ~ dexp(1)
  sg.cb.edge ~ dexp(1)
  sg.cb.dens ~ dexp(1)
  sg.cb.dev ~ dexp(1)
  sg.cb.road ~ dexp(1)
  
  
  #Priors - Autoregression
  phi.s ~ dnorm(0,sd=1)
  phi.c ~ dnorm(0,sd=1)
  phi.b ~ dnorm(0,sd=1)
  
  
})#close of NIMBLE code


#data
data <- list(y = CH,
             CH.effort = CH.effort,
             species.mat = species.mat,
             Occ.scaled.mat = Occ.scaled.mat,
             camera.NIMBLE.det = camera.NIMBLE.det,
             moon.NIMBLE.det = moon.NIMBLE.det)

#constants
nsites <- c(712,239) #number of sites in rep1 and rep2
rep <- 2             #number of seasons
n.occ <- 8           #total number of occasions in detection history in single season
constants <-  list(nsites=nsites,
                   rep=rep,
                   n.occ=n.occ)
#inits
z.int <- matrix(NA, nrow = 2, ncol = 712) #set the initial value for latent state. finding highest possible state for each site
sk <- array(0,dim=c(2,712,8))
coy <- array(0,dim=c(2,712,8))
bob <- array(0,dim=c(2,712,8))
for (t in 1:rep) {
  for (j in 1:nsites[t]) {
    for (k in 1:n.occ) {
      if ((CH[t,j,k]==2) | (CH[t,j,k]==5) | (CH[t,j,k]==6) | (CH[t,j,k]==8)) {
        sk[t,j,k] <- 1
      }
      if ((CH[t,j,k]==3) | (CH[t,j,k]==5) | (CH[t,j,k]==7) | (CH[t,j,k]==8)) {
        coy[t,j,k] <- 1
      }
      if((CH[t,j,k]==4) | (CH[t,j,k]==6) | (CH[t,j,k]==7) | (CH[t,j,k]==8)) {
        bob[t,j,k] <- 1
      }
    }
  }
}


for (t in 1:rep) {
  for (j in 1:nsites[t]) {
    
    if((sum(sk[t,j,1:8])==0) & (sum(coy[t,j,1:8])==0) & (sum(bob[t,j,1:8])==0)){
      z.int[t,j] <-1
    }
    else if((sum(sk[t,j,1:8])>0) & (sum(coy[t,j,1:8])==0) & (sum(bob[t,j,1:8])==0)){
      z.int[t,j] <- 2
    }
    else if((sum(sk[t,j,1:8])==0) & (sum(coy[t,j,1:8])>0) & (sum(bob[t,j,1:8])==0)){
      z.int[t,j] <- 3
    }
    else if((sum(sk[t,j,1:8])==0) & (sum(coy[t,j,1:8])==0) & (sum(bob[t,j,1:8])>0)){
      z.int[t,j] <- 4
    }
    else if((sum(sk[t,j,1:8])>0) & (sum(coy[t,j,1:8])>0) & (sum(bob[t,j,1:8])==0)){
      z.int[t,j] <- 5
    }
    else if((sum(sk[t,j,1:8])>0) & (sum(coy[t,j,1:8])==0) & (sum(bob[t,j,1:8])>0)){
      z.int[t,j] <- 6
    }
    else if((sum(sk[t,j,1:8])==0) & (sum(coy[t,j,1:8])>0) & (sum(bob[t,j,1:8])>0)){
      z.int[t,j] <- 7
    }
    else{
      z.int[t,j] <- 8
    }
  }
}


inits <- function(){list(z = z.int,
                         #Detection
                         #skunk
                         ps0 = rnorm(1,0,sd=1),
                         ps.camera = rnorm(1,0,sd=1),
                         ps.moon = rnorm(1,0,sd=1),
                         sg.s.cam = rexp(1,1),
                         sg.s.moon = rexp(1,1),
                         #coyote
                         pc0 = rnorm(1,0,sd=1),
                         pc.camera = rnorm(1,0,sd=1),
                         pc.moon = rnorm(1,0,sd=1),
                         sg.c.cam = rexp(1,1),
                         sg.c.moon = rexp(1,1),
                         #bobcat
                         pb0 = rnorm(1,0,sd=1),
                         pb.camera = rnorm(1,0,sd=1),
                         pb.moon = rnorm(1,0,sd=1),
                         sg.b.cam = rexp(1,1),
                         sg.b.moon = rexp(1,1),
                         #Occupancy
                         #skunk
                         s0 = rnorm(1,0,sd=1), 
                         s.elevation = rnorm(1,0,sd=1),
                         s.precip = rnorm(1,0,sd=1),
                         s.edge = rnorm(1,0,sd=1),
                         s.density = rnorm(1,0,sd=1),
                         s.devel = rnorm(1,0,sd=1),
                         s.road = rnorm(1,0,sd=1),
                         sg.s.elev = rexp(1,1),
                         sg.s.prec = rexp(1,1),
                         sg.s.edge = rexp(1,1),
                         sg.s.dens = rexp(1,1),
                         sg.s.dev = rexp(1,1),
                         sg.s.road = rexp(1,1),
                         #coyote
                         c0 = rnorm(1,0,sd=1), 
                         c.elevation = rnorm(1,0,sd=1),
                         c.precip = rnorm(1,0,sd=1),
                         c.edge = rnorm(1,0,sd=1),
                         c.density = rnorm(1,0,sd=1),
                         c.devel = rnorm(1,0,sd=1),
                         c.road = rnorm(1,0,sd=1),
                         sg.c.elev = rexp(1,1),
                         sg.c.prec = rexp(1,1),
                         sg.c.edge = rexp(1,1),
                         sg.c.dens = rexp(1,1),
                         sg.c.dev = rexp(1,1),
                         sg.c.road = rexp(1,1),
                         #bobcat
                         b0 = rnorm(1,0,sd=1), 
                         b.elevation = rnorm(1,0,sd=1),
                         b.precip = rnorm(1,0,sd=1),
                         b.edge = rnorm(1,0,sd=1),
                         b.density = rnorm(1,0,sd=1),
                         b.devel = rnorm(1,0,sd=1),
                         b.road = rnorm(1,0,sd=1),
                         sg.b.elev = rexp(1,1),
                         sg.b.prec = rexp(1,1),
                         sg.b.edge = rexp(1,1),
                         sg.b.dens = rexp(1,1),
                         sg.b.dev = rexp(1,1),
                         sg.b.road = rexp(1,1),
                         #skunk/coyote
                         sc0 = rnorm(1,0,sd=1), 
                         sc.elevation = rnorm(1,0,sd=1),
                         sc.precip = rnorm(1,0,sd=1),
                         sc.edge = rnorm(1,0,sd=1),
                         sc.density = rnorm(1,0,sd=1),
                         sc.devel = rnorm(1,0,sd=1),
                         sc.road = rnorm(1,0,sd=1),
                         sg.sc.elev = rexp(1,1),
                         sg.sc.prec = rexp(1,1),
                         sg.sc.edge = rexp(1,1),
                         sg.sc.dens = rexp(1,1),
                         sg.sc.dev = rexp(1,1),
                         sg.sc.road = rexp(1,1),
                         #skunk/bobcat
                         sb0 = rnorm(1,0,sd=1), 
                         sb.elevation = rnorm(1,0,sd=1),
                         sb.precip = rnorm(1,0,sd=1),
                         sb.edge = rnorm(1,0,sd=1),
                         sb.density = rnorm(1,0,sd=1),
                         sb.devel = rnorm(1,0,sd=1),
                         sb.road = rnorm(1,0,sd=1),
                         sg.sb.elev = rexp(1,1),
                         sg.sb.prec = rexp(1,1),
                         sg.sb.edge = rexp(1,1),
                         sg.sb.dens = rexp(1,1),
                         sg.sb.dev = rexp(1,1),
                         sg.sb.road = rexp(1,1),
                         #coyote/bobcat
                         cb0 = rnorm(1,0,sd=1), 
                         cb.elevation = rnorm(1,0,sd=1),
                         cb.precip = rnorm(1,0,sd=1),
                         cb.edge = rnorm(1,0,sd=1),
                         cb.density = rnorm(1,0,sd=1),
                         cb.devel = rnorm(1,0,sd=1),
                         cb.road = rnorm(1,0,sd=1),
                         sg.cb.elev = rexp(1,1),
                         sg.cb.prec = rexp(1,1),
                         sg.cb.edge = rexp(1,1),
                         sg.cb.dens = rexp(1,1),
                         sg.cb.dev = rexp(1,1),
                         sg.cb.road = rexp(1,1),
                         #Autoregression
                         phi.s = rnorm(1,0,sd=1),
                         phi.c = rnorm(1,0,sd=1),
                         phi.b = rnorm(1,0,sd=1)
)}

#params
params=c('ps0','ps.camera','ps.moon',
         'pc0','pc.camera','pc.moon',
         'pb0','pb.camera','pb.moon',
         's0','s.elevation','s.precip','s.edge','s.density','s.devel','s.road',
         'c0','c.elevation','c.precip','c.edge','c.density','c.devel','c.road',
         'b0','b.elevation','b.precip','b.edge','b.density','b.devel','b.road',
         'sc0','sc.elevation','sc.precip','sc.edge','sc.density','sc.devel','sc.road',
         'sb0','sb.elevation','sb.precip','sb.edge','sb.density','sb.devel','sb.road',
         'cb0','cb.elevation','cb.precip','cb.edge','cb.density','cb.devel','cb.road',
         'phi.s','phi.c','phi.b','z'
)

#
ni <- 100000
nb <- 10000
nc <- 3
nt <- 1

## run model 

samples.full<- nimbleMCMC(
  code = MS.full,  
  data=data,
  constants = constants, 
  inits = inits,
  monitors = params,
  niter = ni,
  nburnin = nb,
  nchains = nc,
  thin = nt,
  summary = TRUE,
  WAIC = TRUE,
  samplesAsCodaMCMC = TRUE)

saveRDS(samples.full,"Full_Interactions.rds")  #save the resulting run for future use

##check diagnostics
MCMCtrace(object = samples.full$samples,
          pdf = FALSE,
          ind = TRUE,
          params= c('ps0','ps.camera','ps.moon',
                    'pc0','pc.camera','pc.moon',
                    'pb0','pb.camera','pb.moon',
                    's0','s.elevation','s.precip','s.edge','s.density','s.devel','s.road',
                    'c0','c.elevation','c.precip','c.edge','c.density','c.devel','c.road',
                    'b0','b.elevation','b.precip','b.edge','b.density','b.devel','b.road',
                    'sc0','sc.elevation','sc.precip','sc.edge','sc.density','sc.devel','sc.road',
                    'sb0','sb.elevation','sb.precip','sb.edge','sb.density','sb.devel','sb.road',
                    'cb0','cb.elevation','cb.precip','cb.edge','cb.density','cb.devel','cb.road',
                    'phi.s','phi.c','phi.b'
          ),
          iter = 90000)


tmp <- grep("z", colnames(samples.full$samples$chain1)) #find position of latent state estimates
               
#summary 
samples.full$WAIC
samples.full$summary$all.chains[-tmp,]

#Gelman-Rubin diagnostic
gelman.diag(samples.full$samples[,-tmp]) 
