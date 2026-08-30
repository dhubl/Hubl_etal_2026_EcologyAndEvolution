#following the goodness of fit test method from:
#Broms, K. M., M. B. Hooten, and R. M. Fitzpatrick. 2016. Model selection and 
    #assessment for multi-species occupancy models. Ecology 97:1759–1770.

#run model setup script to get covariate values and capture histories of sites
#covariates and caphist prep.R script
###########
#D Hubl
#2025
#####################################################################
#dependicies
library(nimble)
#####################################################################
rm(list=setdiff(ls(), c("camera.NIMBLE.det","moon.NIMBLE.det","Occ.scaled.mat","Occ.mat","caphist.full.all")))
#####################################################################
#read in the model that has estimated latent states.
# use the most complex model
full <- readRDS("Full_Interactions.rds")

#model estimate of each site's true state for every mcmc iteration
range(grep("z",colnames(full$samples$chain1))) #which columns are the z parameters
c1 <- full$samples$chain1[,seq(from=55,to=1477,by=2)] #just getting season 1 z parameters
c2 <- full$samples$chain2[,seq(from=55,to=1477,by=2)]
c3 <- full$samples$chain3[,seq(from=55,to=1477,by=2)]
full.z.1 <- rbind(c1, c2, c3)

c1 <- full$samples$chain1[,seq(from=56,to=1478,by=2)] #just getting season 2 z parameters
c2 <- full$samples$chain2[,seq(from=56,to=1478,by=2)]
c3 <- full$samples$chain3[,seq(from=56,to=1478,by=2)]
full.z.2 <- rbind(c1, c2, c3)
dim(full.z.2) #only the first 239 sites were actually sampled in season 2 so this could be trimmed if helpful 
###########################
#get mcmc estimates of all other parameters
############### full interactions model ###############################
par.full <- rbind(full$samples$chain1[,1:54],full$samples$chain2[,1:54],full$samples$chain3[,1:54])
dim(par.full)
colnames(par.full)
###########################
#detection histories for each site

dim(caphist.full.all) #rows are sites, columns are surveys, page1 is season1

###########################
#function to get the Categorical probabilities of each latent community state 
#given the site covariates and the estimated coefficient values of the mcmc iteration

################ full interaction model ##########################

psi.full <- function(state,mcmc,site) { #state is the model estimated latent state of the site in season 1
  #season 1
  f1  <- par.full[mcmc,"s0"]  + par.full[mcmc,"s.elevation"]*Occ.scaled.mat[site,1] + par.full[mcmc,"s.precip"]*Occ.scaled.mat[site,2]  + par.full[mcmc,"s.edge"]*Occ.scaled.mat[site,3]  + par.full[mcmc,"s.density"]*Occ.scaled.mat[site,4]  + par.full[mcmc,"s.devel"]*Occ.scaled.mat[site,5]  + par.full[mcmc,"s.road"]*Occ.scaled.mat[site,6]  # skunk
  f2  <- par.full[mcmc,"c0"]  + par.full[mcmc,"c.elevation"]*Occ.scaled.mat[site,1] + par.full[mcmc,"c.precip"]*Occ.scaled.mat[site,2]  + par.full[mcmc,"c.edge"]*Occ.scaled.mat[site,3]  + par.full[mcmc,"c.density"]*Occ.scaled.mat[site,4]  + par.full[mcmc,"c.devel"]*Occ.scaled.mat[site,5]  + par.full[mcmc,"c.road"]*Occ.scaled.mat[site,6]  # coyote
  f3  <- par.full[mcmc,"b0"]  + par.full[mcmc,"b.elevation"]*Occ.scaled.mat[site,1] + par.full[mcmc,"b.precip"]*Occ.scaled.mat[site,2]  + par.full[mcmc,"b.edge"]*Occ.scaled.mat[site,3]  + par.full[mcmc,"b.density"]*Occ.scaled.mat[site,4]  + par.full[mcmc,"b.devel"]*Occ.scaled.mat[site,5]  + par.full[mcmc,"b.road"]*Occ.scaled.mat[site,6]  # bobcat
  f12 <- par.full[mcmc, "sc0"] + par.full[mcmc,"sc.elevation"]*Occ.scaled.mat[site,1] + par.full[mcmc,"sc.precip"]*Occ.scaled.mat[site,2]  + par.full[mcmc,"sc.edge"]*Occ.scaled.mat[site,3]  + par.full[mcmc,"sc.density"]*Occ.scaled.mat[site,4]  + par.full[mcmc,"sc.devel"]*Occ.scaled.mat[site,5]  + par.full[mcmc,"sc.road"]*Occ.scaled.mat[site,6]# skunk x coyote interaction
  f13 <- par.full[mcmc, "sb0"] + par.full[mcmc,"sb.elevation"]*Occ.scaled.mat[site,1] + par.full[mcmc,"sb.precip"]*Occ.scaled.mat[site,2]  + par.full[mcmc,"sb.edge"]*Occ.scaled.mat[site,3]  + par.full[mcmc,"sb.density"]*Occ.scaled.mat[site,4]  + par.full[mcmc,"sb.devel"]*Occ.scaled.mat[site,5]  + par.full[mcmc,"sb.road"]*Occ.scaled.mat[site,6]# skunk x bobcat interaction
  f23 <- par.full[mcmc, "cb0"] + par.full[mcmc,"cb.elevation"]*Occ.scaled.mat[site,1] + par.full[mcmc,"cb.precip"]*Occ.scaled.mat[site,2]  + par.full[mcmc,"cb.edge"]*Occ.scaled.mat[site,3]  + par.full[mcmc,"cb.density"]*Occ.scaled.mat[site,4]  + par.full[mcmc,"cb.devel"]*Occ.scaled.mat[site,5]  + par.full[mcmc,"cb.road"]*Occ.scaled.mat[site,6]# coyote x bobcat interaction
  
  psi1 <- 1 / (1 + exp(f1) + exp(f2) + exp(f3) + exp(f1 + f2 + f12) + exp(f1 + f3 + f13) + exp(f2 + f3 + f23) + exp(f1 + f2 + f3 + f12 + f13 + f23))
  psi2 <- exp(f1) / (1 + exp(f1) + exp(f2) + exp(f3) + exp(f1 + f2 + f12) + exp(f1 + f3 + f13) + exp(f2 + f3 + f23) + exp(f1 + f2 + f3 + f12 + f13 + f23))
  psi3 <- exp(f2) / (1 + exp(f1) + exp(f2) + exp(f3) + exp(f1 + f2 + f12) + exp(f1 + f3 + f13) + exp(f2 + f3 + f23) + exp(f1 + f2 + f3 + f12 + f13 + f23))
  psi4 <- exp(f3) / (1 + exp(f1) + exp(f2) + exp(f3) + exp(f1 + f2 + f12) + exp(f1 + f3 + f13) + exp(f2 + f3 + f23) + exp(f1 + f2 + f3 + f12 + f13 + f23))
  psi5 <- exp(f1 + f2 + f12) / (1 + exp(f1) + exp(f2) + exp(f3) + exp(f1 + f2 + f12) + exp(f1 + f3 + f13) + exp(f2 + f3 + f23) + exp(f1 + f2 + f3 + f12 + f13 + f23))
  psi6 <- exp(f1 + f3 + f13) / (1 + exp(f1) + exp(f2) + exp(f3) + exp(f1 + f2 + f12) + exp(f1 + f3 + f13) + exp(f2 + f3 + f23) + exp(f1 + f2 + f3 + f12 + f13 + f23))
  psi7 <- exp(f2 + f3 + f23) / (1 + exp(f1) + exp(f2) + exp(f3) + exp(f1 + f2 + f12) + exp(f1 + f3 + f13) + exp(f2 + f3 + f23) + exp(f1 + f2 + f3 + f12 + f13 + f23))
  psi8 <- exp(f1 + f2 + f3 + f12 + f13 + f23) / (1 + exp(f1) + exp(f2) + exp(f3) + exp(f1 + f2 + f12) + exp(f1 + f3 + f13) + exp(f2 + f3 + f23) + exp(f1 + f2 + f3 + f12 + f13 + f23))
  
  s1 <-  rbind(psi1, psi2, psi3, psi4, psi5, psi6, psi7, psi8)
  
  species.mat <-  matrix(data=c(0,0,0,
                                1,0,0,
                                0,1,0,
                                0,0,1,
                                1,1,0,
                                1,0,1,
                                0,1,1,
                                1,1,1),
                         byrow=TRUE, nrow=8)
  
  #season 2 will depend on what the estimated latent state of season 1 
  f1  <- par.full[mcmc,"phi.s"]*species.mat[state,1] + par.full[mcmc,"s0"]  + par.full[mcmc,"s.elevation"]*Occ.scaled.mat[site,1] + par.full[mcmc,"s.precip"]*Occ.scaled.mat[site,2]  + par.full[mcmc,"s.edge"]*Occ.scaled.mat[site,3]  + par.full[mcmc,"s.density"]*Occ.scaled.mat[site,4]  + par.full[mcmc,"s.devel"]*Occ.scaled.mat[site,5]  + par.full[mcmc,"s.road"]*Occ.scaled.mat[site,6]  # skunk
  f2  <- par.full[mcmc,"phi.c"]*species.mat[state,2] + par.full[mcmc,"c0"]  + par.full[mcmc,"c.elevation"]*Occ.scaled.mat[site,1] + par.full[mcmc,"c.precip"]*Occ.scaled.mat[site,2]  + par.full[mcmc,"c.edge"]*Occ.scaled.mat[site,3]  + par.full[mcmc,"c.density"]*Occ.scaled.mat[site,4]  + par.full[mcmc,"c.devel"]*Occ.scaled.mat[site,5]  + par.full[mcmc,"c.road"]*Occ.scaled.mat[site,6]  # coyote
  f3  <- par.full[mcmc,"phi.b"]*species.mat[state,3] + par.full[mcmc,"b0"]  + par.full[mcmc,"b.elevation"]*Occ.scaled.mat[site,1] + par.full[mcmc,"b.precip"]*Occ.scaled.mat[site,2]  + par.full[mcmc,"b.edge"]*Occ.scaled.mat[site,3]  + par.full[mcmc,"b.density"]*Occ.scaled.mat[site,4]  + par.full[mcmc,"b.devel"]*Occ.scaled.mat[site,5]  + par.full[mcmc,"b.road"]*Occ.scaled.mat[site,6]  # bobcat
  f12 <- par.full[mcmc, "sc0"] + par.full[mcmc,"sc.elevation"]*Occ.scaled.mat[site,1] + par.full[mcmc,"sc.precip"]*Occ.scaled.mat[site,2]  + par.full[mcmc,"sc.edge"]*Occ.scaled.mat[site,3]  + par.full[mcmc,"sc.density"]*Occ.scaled.mat[site,4]  + par.full[mcmc,"sc.devel"]*Occ.scaled.mat[site,5]  + par.full[mcmc,"sc.road"]*Occ.scaled.mat[site,6]# skunk x coyote interaction
  f13 <- par.full[mcmc, "sb0"] + par.full[mcmc,"sb.elevation"]*Occ.scaled.mat[site,1] + par.full[mcmc,"sb.precip"]*Occ.scaled.mat[site,2]  + par.full[mcmc,"sb.edge"]*Occ.scaled.mat[site,3]  + par.full[mcmc,"sb.density"]*Occ.scaled.mat[site,4]  + par.full[mcmc,"sb.devel"]*Occ.scaled.mat[site,5]  + par.full[mcmc,"sb.road"]*Occ.scaled.mat[site,6]# skunk x bobcat interaction
  f23 <- par.full[mcmc, "cb0"] + par.full[mcmc,"cb.elevation"]*Occ.scaled.mat[site,1] + par.full[mcmc,"cb.precip"]*Occ.scaled.mat[site,2]  + par.full[mcmc,"cb.edge"]*Occ.scaled.mat[site,3]  + par.full[mcmc,"cb.density"]*Occ.scaled.mat[site,4]  + par.full[mcmc,"cb.devel"]*Occ.scaled.mat[site,5]  + par.full[mcmc,"cb.road"]*Occ.scaled.mat[site,6]# coyote x bobcat interaction
  
  psi1 <- 1 / (1 + exp(f1) + exp(f2) + exp(f3) + exp(f1 + f2 + f12) + exp(f1 + f3 + f13) + exp(f2 + f3 + f23) + exp(f1 + f2 + f3 + f12 + f13 + f23))
  psi2 <- exp(f1) / (1 + exp(f1) + exp(f2) + exp(f3) + exp(f1 + f2 + f12) + exp(f1 + f3 + f13) + exp(f2 + f3 + f23) + exp(f1 + f2 + f3 + f12 + f13 + f23))
  psi3 <- exp(f2) / (1 + exp(f1) + exp(f2) + exp(f3) + exp(f1 + f2 + f12) + exp(f1 + f3 + f13) + exp(f2 + f3 + f23) + exp(f1 + f2 + f3 + f12 + f13 + f23))
  psi4 <- exp(f3) / (1 + exp(f1) + exp(f2) + exp(f3) + exp(f1 + f2 + f12) + exp(f1 + f3 + f13) + exp(f2 + f3 + f23) + exp(f1 + f2 + f3 + f12 + f13 + f23))
  psi5 <- exp(f1 + f2 + f12) / (1 + exp(f1) + exp(f2) + exp(f3) + exp(f1 + f2 + f12) + exp(f1 + f3 + f13) + exp(f2 + f3 + f23) + exp(f1 + f2 + f3 + f12 + f13 + f23))
  psi6 <- exp(f1 + f3 + f13) / (1 + exp(f1) + exp(f2) + exp(f3) + exp(f1 + f2 + f12) + exp(f1 + f3 + f13) + exp(f2 + f3 + f23) + exp(f1 + f2 + f3 + f12 + f13 + f23))
  psi7 <- exp(f2 + f3 + f23) / (1 + exp(f1) + exp(f2) + exp(f3) + exp(f1 + f2 + f12) + exp(f1 + f3 + f13) + exp(f2 + f3 + f23) + exp(f1 + f2 + f3 + f12 + f13 + f23))
  psi8 <- exp(f1 + f2 + f3 + f12 + f13 + f23) / (1 + exp(f1) + exp(f2) + exp(f3) + exp(f1 + f2 + f12) + exp(f1 + f3 + f13) + exp(f2 + f3 + f23) + exp(f1 + f2 + f3 + f12 + f13 + f23))
  
  s2 <-  rbind(psi1, psi2, psi3, psi4, psi5, psi6, psi7, psi8)
  
  return(cbind(s1,s2))
}

###############################
#function to calculate probability of observing each 
#community state given the latent state


#this function requires that the detection probability matrices for each species
#are created before the function is called

#ps= probability of detecting skunk
#pc= probability of detecting coyote
#pc= probability of detecting bobcat
#Community States: 1= no species
#                  2= skunk
#                  3= coyote
#                  4= bobcat
#                  5= skunk and coyote
#                  6= skunk and bobcat
#                  7= coyote and bobcat
#                  8= all three species

obs.prob <- function(o,t,j,k) { #o=which state was observed, t=season, j=site, k=survey# 
  
  if (is.na(o)) {
    print(paste0("site",j,"survey",k))
  } else {
    if (o==1) { #prob of observing state 1 in each latent state
      return(c(1,
               1 - ps[t,j,k],
               1 - pc[t,j,k],
               1 - pb[t,j,k],
               (1 - ps[t,j,k]) * (1 - pc[t,j,k]),
               (1 - ps[t,j,k]) * (1 - pb[t,j,k]),
               (1 - pc[t,j,k]) * (1 - pb[t,j,k]),
               (1 - ps[t,j,k]) * (1 - pc[t,j,k]) * (1 - pb[t,j,k])
      ))
    } else {
      if (o==2) { #prob of observing state 2 in each latent state
        return(c(0,
                 ps[t,j,k],
                 0,
                 0,
                 ps[t,j,k] * (1 - pc[t,j,k]),
                 ps[t,j,k] * (1 - pb[t,j,k]),
                 0,
                 ps[t,j,k] * (1 - pc[t,j,k]) * (1 - pb[t,j,k])
        ))
      } else {
        if (o==3) { #prob of observing state 3 in each latent state
          return(c(0,
                   0,
                   pc[t,j,k],
                   0,
                   (1 - ps[t,j,k]) * pc[t,j,k],
                   0,
                   pc[t,j,k]  * (1 - pb[t,j,k]),
                   (1 - ps[t,j,k]) * pc[t,j,k] * (1 - pb[t,j,k])
          ))
        } else {
          if (o==4) { #prob of observing state 4 in each latent state
            return(c(0,
                     0,
                     0,
                     pb[t,j,k],
                     0,
                     (1 - ps[t,j,k]) * pb[t,j,k],
                     (1 - pc[t,j,k]) * pb[t,j,k],
                     (1 - ps[t,j,k]) * (1 - pc[t,j,k]) * pb[t,j,k]))
          } else {
            if (o==5) { #prob of observing state 5 in each latent state
              return(c(0,
                       0,
                       0,
                       0,
                       ps[t,j,k] * pc[t,j,k],
                       0,
                       0,
                       ps[t,j,k] * pc[t,j,k] * (1 - pb[t,j,k])
              ))
            } else {
              if (o==6) { #prob of observing state 6 in each latent state
                return(c(0,
                         0,
                         0,
                         0,
                         0,
                         ps[t,j,k] * pb[t,j,k],
                         0,
                         ps[t,j,k] * (1 - pc[t,j,k]) * pb[t,j,k]
                ))
              } else {
                if (o==7) { #prob of observing state 7 in each latent state
                  return(c(0,
                           0,
                           0,
                           0,
                           0,
                           0,
                           pc[t,j,k] * pb[t,j,k],
                           (1 - ps[t,j,k]) * pc[t,j,k] * pb[t,j,k]
                  ))
                } else {
                  if (o==8) { #prob of observing state 8 in each latent state
                    return(c(0,
                             0,
                             0,
                             0,
                             0,
                             0,
                             0,
                             ps[t,j,k] * pc[t,j,k] * pb[t,j,k]
                    ))
                  }
                }
              }
            }
          }
        }
      }
    }
  }
}
############################
#function to generate a new detection history for the site given its latent state
#and detection covariates
#the detection probability matrices for each species must already exist before 
#calling this function

pred.obs <- function(l,t,j,k) { #l=modeled latent state,t=season, j=site, k=survey#
  if (l==1) { #prob of observing each state given latent state 1
    return(c(1,      
             0,
             0,
             0,
             0,
             0,
             0,
             0
    ))
  } else {
    if (l==2) { #prob of observing each state given latent state 2
      return(c(1 - ps[t,j,k],
               ps[t,j,k], 
               0,
               0, 
               0,
               0,
               0,
               0
      ))
    } else {
      if (l==3) { #prob of observing each state given latent state 3
        return(c(1 - pc[t,j,k],
                 0,
                 pc[t,j,k],
                 0,
                 0,
                 0,
                 0,
                 0
        ))
      } else {
        if (l==4) { #prob of observing each state given latent state 4
          return(c(1 - pb[t,j,k],
                   0,
                   0,
                   pb[t,j,k],
                   0,
                   0,
                   0,
                   0
          ))
        } else {
          if (l==5) { #prob of observing each state given latent state 5
            return(c((1 - ps[t,j,k]) * (1 - pc[t,j,k]),
                     ps[t,j,k] * (1 - pc[t,j,k]),
                     (1 - ps[t,j,k]) * pc[t,j,k],
                     0,
                     ps[t,j,k] * pc[t,j,k],
                     0,
                     0,
                     0
            ))
          } else {
            if (l==6) { #prob of observing each state given latent state 6
              return(c((1 - ps[t,j,k]) * (1 - pb[t,j,k]),
                       ps[t,j,k] * (1 - pb[t,j,k]),
                       0, 
                       (1 - ps[t,j,k]) * pb[t,j,k],
                       0,
                       ps[t,j,k] * pb[t,j,k],
                       0,
                       0
              ))
            } else {
              if (l==7) { #prob of observing each state given latent state 7
                return(c((1 - pc[t,j,k]) * (1 - pb[t,j,k]),
                         0, 
                         pc[t,j,k] * (1 - pb[t,j,k]),
                         (1 - pc[t,j,k]) * pb[t,j,k],
                         0,
                         0,
                         pc[t,j,k] * pb[t,j,k],
                         0
                ))
              } else {
                if (l==8) { #prob of observing each state given latent state 8
                  return(c((1 - ps[t,j,k]) * (1 - pc[t,j,k]) * (1 - pb[t,j,k]),
                           ps[t,j,k] * (1 - pc[t,j,k]) * (1 - pb[t,j,k]),
                           (1 - ps[t,j,k]) * pc[t,j,k] * (1 - pb[t,j,k]),
                           (1 - ps[t,j,k]) * (1 - pc[t,j,k]) * pb[t,j,k],
                           ps[t,j,k] *  pc[t,j,k] * (1 - pb[t,j,k]),
                           ps[t,j,k] * (1 - pc[t,j,k]) * pb[t,j,k],
                           (1 - ps[t,j,k]) * pc[t,j,k] * pb[t,j,k],
                           ps[t,j,k] * pc[t,j,k] * pb[t,j,k]
                  ))
                }
              }
            }
          }
        }
      }
    }
  }
}

##################################
#Bayesian P Value Goodness of Fit 

bay.p <- matrix(NA,nrow=2,ncol=270000)
site.dev <- rep(NA,712)
pred.dev <- rep(NA,712)
ps <- array(NA,dim = c(2,712,8))
pc <- array(NA,dim = c(2,712,8))
pb <- array(NA,dim = c(2,712,8))

for (mcmc in 1:270000){
  for (t in 1:2) {
    for (site in 1:712) { #generate the detection probability matrices for the current mcmc iteration for the functions to use
      for (k in 1:8) {
        ps[t,site,k] <- (plogis(par.full[mcmc,"ps0"] + par.full[mcmc,"ps.camera"]*camera.NIMBLE.det[t,site,k]  + par.full[mcmc,"ps.moon"]*moon.NIMBLE.det[t,site,k])) 
        pc[t,site,k] <- (plogis(par.full[mcmc,"pc0"] + par.full[mcmc,"pc.camera"]*camera.NIMBLE.det[t,site,k]  + par.full[mcmc,"pc.moon"]*moon.NIMBLE.det[t,site,k])) 
        pb[t,site,k] <- (plogis(par.full[mcmc,"pb0"] + par.full[mcmc,"pb.camera"]*camera.NIMBLE.det[t,site,k]  + par.full[mcmc,"pb.moon"]*moon.NIMBLE.det[t,site,k])) 
      }#k
    }#site
  }#t 
  
  for (site in 1:712) {
    l1 <- full.z.1[mcmc,site] #model predicted latent state for the site in season 1
    l2 <- full.z.2[mcmc,site] #model predicted latent state for the site in season 2
    
    dh1 <- caphist.full.all[site,,1] #detection history of the site in season 1
    dh2 <- caphist.full.all[site,,2] #detection history of the site in season 2
    
    k1 <- which(!is.na(dh1))         #which surveys actually happened in season 1
    k2 <- which(!is.na(dh2))         #which surveys actually happened in season 2
    
    obs.mat1 <- matrix(NA,nrow=8,ncol=length(k1))  #matrix to accept probabilities of observing each value in the detection history of season 1
    obs.mat2 <- matrix(NA,nrow=8,ncol=length(k2))  #matrix to accept probabilities of observing each value in the detection history of season 2      
    
    dh1.hat <- rep(NA,length(k1))   #create vector for model predicted detection history of season 1
    dh2.hat <- rep(NA,length(k2))   #create vector for model predicted detection history of season 2
    
    pred.obs.mat1 <- matrix(NA,nrow=8,ncol=length(k1))  #matrix to accept probabilities of observing each value in the predicted season 1 detection history
    pred.obs.mat2 <- matrix(NA,nrow=8,ncol=length(k2))  #matrix to accept probabilities of observing each value in the predicted season 2 detection history
    
    for (i in 1:length(k1)) { #season 1 observation probabilities
      obs.mat1[,i] <- obs.prob(dh1[k1[i]],t=1,site,k1[i]) #vector of the probabilities of observing community state for each latent state
      
      dh1.hat[i] <- rcat(1,prob=pred.obs(l=l1, t=1, site, k1[i]))   #generate a value for predicted detection history given latent state, estimated psi and detections
      
      pred.obs.mat1[,i] <- obs.prob(dh1.hat[i],t=1, site, k1[i])  #vector of the probabilities of observing the predicted community state for each latent state
    }#i
    
    site.psi <- psi.full(l1,mcmc,site) #community state probabilities of the site for season 1 (column 1) and 2 (column 2)
    
    if (length(k2)==0) { #if the site was not re-sampled in season 2
      
      site.dev[site] <- log(sum(site.psi[,1]*apply(X=obs.mat1,MARGIN = 1, FUN = prod)))       #log probability of the site's detection history
      pred.dev[site] <- log(sum(site.psi[,1]*apply(X=pred.obs.mat1, MARGIN = 1, FUN = prod))) #log probability of the site's predicted detection history
      
    } else{  #if the site was re-sampled in season 2
      
      for (i in 1:length(k2)) { #season 2 observation probabilities
        obs.mat2[,i] <- obs.prob(dh2[k2[i]],t=2,site,k2[i]) #vector of the probabilities of observing community state for each latent state
        
        dh2.hat[i] <- rcat(1,prob=pred.obs(l=l2, t=2, site, k2[i]))   #generate a value for predicted detection history given latent state, estimated psi and detections
        
        pred.obs.mat2[,i] <- obs.prob(dh2.hat[i],t=2, site, k2[i])  #vector of the probabilities of observing the predicted community state for each latent state
      }#i
      
      site.dev[site] <- log((sum(site.psi[,1]*apply(X=obs.mat1,MARGIN = 1, FUN = prod)))*(sum(site.psi[,2]*apply(X=obs.mat2,MARGIN = 1, FUN = prod))))       #log probability of the site's detection history
      pred.dev[site] <- log((sum(site.psi[,1]*apply(X=pred.obs.mat1, MARGIN = 1, FUN = prod)))*(sum(site.psi[,2]*apply(X=pred.obs.mat2, MARGIN = 1, FUN = prod))))#log probability of the site's predicted detection history
      
    }
  }#site
  bay.p[1,mcmc] <- sum(site.dev) * (-2)   #deviance of observed detection history
  bay.p[2,mcmc] <- sum(pred.dev) * (-2)   #deviance of predicted detection history
}#mcmc

p <- bay.p[1,] / bay.p[2,]                 # observed deviance / predicted deviance
length(which(p>1)) / 270000                 # Bayesian p value;should be approximately 0.5 if model fits the data


