#HW05 Real stuff
bio = read.table("C:/Users/Zachary/Desktop/Winter 2017/STA 723 Case Studies/STA_723_case/HW05/bioassay.txt", header = TRUE)
library(ggplot2)
library(R2jags)
library(R2WinBUGS)
library(MCMCPack)
library(rjags)
library(lme4)
# Build two models.  One for site and one for protocol.  I think that's the best solution I can tell.
# Way to paramterized without it.

# Build both of these models in JAGS
# I need to figure out how to do this withANOVA.  They aren't balanced so it's not that simple
#
###################################################################################
# Hierarchiacl with varying intercepts for both and varying slope for site
## Maybe a little bit more simple.  Use frecuentist for test this?


###################################################################################
# Hierarchical with varying intercepts for both.
## For varying slope for protocol and random effects fo the other stuff


##########################################################
# Final Model
# Rnadom effects for site and protocol
# EE continuous and EE squared
# ZM
# Find away to do posterior predictive ditsributions
# Posterior predictive checks for different values of EE
# t-tests for them
# Something like that
##########################################################
bio$uterus = as.numeric(levels(bio$uterus))[bio$uterus]
bio$weight = as.numeric(levels(bio$weight))[bio$weight]
bio$EE = as.numeric(levels(bio$EE))[bio$EE]
bio$ZM = as.numeric(levels(bio$ZM))[bio$ZM]
bio$EE2 = (bio$EE)^2
na.index = which(is.na(bio$uterus))
bio = bio[-na.index,]

# Hierarchical models
bio.frame = as.data.frame(bio)
bio.frame$protnum = as.integer(bio.frame$protocol)
bio.frame$labnum = as.integer(bio.frame$lab)

labnum = bio.frame$labnum
protocol = bio.frame$protnum
Y = bio.frame$uterus
EE = bio.frame$EE
EE2 = bio.frame$EE2
ZM = bio.frame$ZM
N = nrow(bio.frame)
aN = length(unique(bio.frame$labnum))
gN = length(unique(bio.frame$protnum))
data = list(labnum = labnum, protocol = protocol, Y=Y, EE = EE,EE2 = EE2, ZM = ZM,
            N = N, aN = aN, gN = gN)
hier.bio = function(){
  for(i in 1:N){
    Y[i] ~ dnorm(mu[i],phi)
    mu[i] = int + alpha[labnum[i]] + gamma[protocol[i]] + beta1 * EE[i] + beta2 * ZM[i] + beta3 * EE2[i]
      }
  alpha[1] <- 0
  gamma[1] <- 0
  for(j in 2:aN){
    alpha[j] ~ dnorm(alpha.mu, alpha.phi)
  }
  for(k in 2:gN){
    gamma[k] ~ dnorm(gamma.mu, gamma.phi)
  }
  # Overall variance
  phi ~ dgamma(.00001,.00001)
  sigma2 <- 1 / (phi)
  sigma <- sqrt(sigma2)
  # Lab
  alpha.mu ~ dnorm(0,.00001)
  alpha.phi ~ dgamma(.00001,.00001)
  sigma2.alpha <- 1 / alpha.phi
  sigma.alpha <- sqrt(sigma2.alpha)
  
  # Protocol
  gamma.mu ~ dnorm(0,.00001)
  gamma.phi ~ dgamma(.00001,.00001)
  sigma2.gamma <- 1 / gamma.phi
  sigma.gamma <- sqrt(sigma2.gamma)
  
  # EE
  beta1 ~ dnorm(0,.00001)
  # ZM
  beta2 ~ dnorm(0,.00001)
  # EE2
  beta3 ~ dnorm(0,.00001)
  # Overall covariates
  int ~ dnorm(0,.00001)
}
parameters = c("int", "beta1", "beta2","beta3", "alpha", "gamma", "alpha.mu", "gamma.mu",
               "sigma2.alpha","sigma.alpha","sigma2.gamma","sigma.gamma","sigma2","sigma")

model.file = "hier_bio.txt"
write.model(hier.bio, model.file)

sims.real = jags(data, inits = NULL, parameters.to.save = parameters, model.file = hier.bio,n.chains = 2, n.iter = 10000)
post.samps.real = sims.real$BUGSoutput$sims.matrix
summary(post.samps.real)

# Posterior predictive checks
dat.mat = model.matrix(lm(uterus~ -1 + EE + ZM + EE2 , data = bio.frame))
prot.mat = model.matrix(lm(uterus~-1+protocol, data = bio.frame))
lab.mat = model.matrix(lm(uterus~-1+lab,data =  bio.frame))
intercept = rep(1,N)
mod.mat = cbind(intercept,lab.mat,prot.mat,dat.mat)

coefs = cbind(post.samps.real[,"int"],post.samps.real[,1:19],post.samps.real[,25:28],post.samps.real[,21:23])
head(post.samps.real[,1:19])

preds.mat = matrix(NA, ncol = nrow(post.samps.real), nrow = nrow(bio.frame))
for(i in 1:nrow(post.samps.real)){
  mu =  mod.mat %*% coefs[i,]
  post.pred = rnorm(length(mu),mu,post.samps.real[,"sigma"])
  preds.mat[,i] = post.pred
  if(i %% 100 == 0){
    print(i)
  }
}

levels.ee = unique(bio.frame$EE)
levels.zm = unique(bio.frame$ZM)
lab.mat = matrix(NA, nrow = 264, ncol = 19)
prot.mat = matrix(NA,ncol = 4, nrow = 1030)
EE.mat = matrix(NA,ncol = 8, nrow = 737)
zm.mat = matrix(NA,ncol = 3, nrow = 2186)
for(i in 1:19){
  length.vals = length(which(bio.frame$labnum == i))
  lab.mat[1:length.vals,i] = which(bio.frame$labnum == i)
  if(i <= 4){
    length.prot = length(which(bio.frame$protnum == i))
    if(length.prot > 0){
      prot.mat[1:length.prot,i] = which(bio.frame$protnum ==i)
    }
  }
  if(i <= 8){
    length.ee = length(which(bio.frame$EE == levels.ee[i]))
    if(length.ee > 0){
      EE.mat[1:length.ee,i] = which(bio.frame$EE == levels.ee[i])
    }
  }
  if(i <= 3){
    length.zm = length(which(bio.frame$ZM == levels.zm[i]))
    if(length.zm > 0){
    zm.mat[1:length.zm,i] = which(bio.frame$ZM == levels.zm[i])
    }
  }
}

lab1.index = which(bio.frame$labnum ==1)
lab2.index = which(bio.frame$labnum ==2)
lab3.index = which(bio.frame$labnum ==3)
lab4.index= which(bio.frame$labnum ==4)
lab5.index= which(bio.frame$labnum ==5)
lab6.index= which(bio.frame$labnum ==6)
lab7.index= which(bio.frame$labnum ==7)
lab8.index= which(bio.frame$labnum ==8)
lab9.index= which(bio.frame$labnum ==9)
lab10.index= which(bio.frame$labnum ==10)
lab11.index= which(bio.frame$labnum ==11)
lab12.index= which(bio.frame$labnum ==12)
lab13.index= which(bio.frame$labnum ==13)
lab14.index= which(bio.frame$labnum ==14)
lab15.index= which(bio.frame$labnum ==15)
lab16.index= which(bio.frame$labnum ==16)
lab17.index= which(bio.frame$labnum ==17)
lab18.index= which(bio.frame$labnum ==18)
lab19.index= which(bio.frame$labnum ==19)
lab.mat = matrix(NA, nrow = 264, ncol = 19)

prot1.index = which(bio.frame$protnum ==1)
prot2.index= which(bio.frame$protnum ==2)
prot3.index= which(bio.frame$protnum ==3)
prot4.index= which(bio.frame$protnum ==4)
prot.mat = matrix(NA,ncol = 4, nrow = 1030)

EEcontrol = which(bio.frame$EE == 0)
EE.01 = which(bio.frame$EE == 0.01)
EE.03 = which(bio.frame$EE == 0.03)
EE.1 = which(bio.frame$EE == 0.1)
EE.3 = which(bio.frame$EE == 0.3)
EE1 = which(bio.frame$EE == 1)
EE3 = which(bio.frame$EE == 3)
EE10 = which(bio.frame$EE == 10)
EE.mat = matrix(NA,ncol = 8, nrow = 484)

ZMcontrol = which(bio.frame$ZM == 0)
ZM.1= which(bio.frame$ZM == 0.1)
ZM1= which(bio.frame$ZM == 1)
zm.mat = matrix(NA,ncol = 3, nrow = 2186)

# Play with this function more
# Overleaf
# Plots
# Write Write WRite

# THe error of this function has to do with when I join the two sequences I think
# Each of the sites and controls

# Site 1 and control
lab.cont.mat = matrix(NA, ncol = length(unique(bio.frame$labnum)), nrow = 264)
lab.01.mat = matrix(NA, ncol = length(unique(bio.frame$labnum)), nrow = 264)
lab.03.mat= matrix(NA, ncol = length(unique(bio.frame$labnum)), nrow = 264)
lab.1.mat= matrix(NA, ncol = length(unique(bio.frame$labnum)), nrow = 264)
lab.3.mat= matrix(NA, ncol = length(unique(bio.frame$labnum)), nrow = 264)
lab1.mat= matrix(NA, ncol = length(unique(bio.frame$labnum)), nrow = 264)
lab3.mat= matrix(NA, ncol = length(unique(bio.frame$labnum)), nrow = 264)
lab10.mat= matrix(NA, ncol = length(unique(bio.frame$labnum)), nrow = 264)

lab.zm.cont = matrix(NA, ncol = length(unique(bio.frame$labnum)), nrow = 264)
lab.1.zm = matrix(NA, ncol = length(unique(bio.frame$labnum)), nrow = 264)
lab1.zm= matrix(NA, ncol = length(unique(bio.frame$labnum)), nrow = 264)

prot.cont.mat = matrix(NA, ncol = length(unique(bio.frame$protnum)), nrow = 1000)
prot.01.mat = matrix(NA, ncol = length(unique(bio.frame$protnum)), nrow = 1000)
prot.03.mat= matrix(NA, ncol = length(unique(bio.frame$protnum)), nrow = 1000)
prot.1.mat= matrix(NA, ncol = length(unique(bio.frame$protnum)), nrow = 1000)
prot.3.mat= matrix(NA, ncol = length(unique(bio.frame$protnum)), nrow = 1000)
prot1.mat= matrix(NA, ncol = length(unique(bio.frame$protnum)), nrow = 1000)
prot3.mat= matrix(NA, ncol = length(unique(bio.frame$protnum)), nrow = 1000)
prot10.mat= matrix(NA, ncol = length(unique(bio.frame$protnum)), nrow = 1000)
prot.zm.cont = matrix(NA, ncol = length(unique(bio.frame$protnum)), nrow = 1000)
prot.1.zm = matrix(NA, ncol = length(unique(bio.frame$protnum)), nrow = 1000)
prot1.zm= matrix(NA, ncol = length(unique(bio.frame$protnum)), nrow = 1000)

for(i in 1:19){
  
}

for(i in 1:4){
  
}
p.vals.ee.mat.lab = matrix(NA,nrow = 19, ncol = 8)
p.vals.zm.mat.lab = matrix(NA,nrow = 19, ncol = 3)
p.vals.ee.mat.prot = matrix(NA,nrow = 4, ncol = 8)
p.vals.zm.mat.prot = matrix(NA, nrow = 4, ncol = 3)
dim(preds.mat)

for(i in 1:19){
  length.lab = length(which(lab.mat[is.na(lab.mat[,i]) == FALSE,i] %in% EE.mat[,1]))
  if(length.lab > 0){
    lab.cont.mat[1:length.lab,i] = which(lab.mat[is.na(lab.mat[,i]) == FALSE,i] %in% EE.mat[,1])
    lab.cont.data = preds.mat[lab.cont.mat[1:length.lab,i],]
    lab.cont.data1 = apply(lab.cont.data,1,mean)
  }
  length.lab = length(which(lab.mat[is.na(lab.mat[,i]) == FALSE,i] %in% EE.mat[,2]))
  if(length.lab > 0){
    lab.01.mat[1:length.lab,i] = which(lab.mat[is.na(lab.mat[,i]) == FALSE,i] %in% EE.mat[,2])
    lab.01.data = preds.mat[lab.01.mat[1:length.lab,i],]
    lab.01.data1 = apply(lab.01.data,1,mean)
  }
  length.lab = length(which(lab.mat[is.na(lab.mat[,i]) == FALSE,i] %in% EE.mat[,3]))
  if(length.lab > 0){
    lab.03.mat[1:length.lab,i] = which(lab.mat[is.na(lab.mat[,i]) == FALSE,i] %in% EE.mat[,3])
    lab.03.data = preds.mat[lab.03.mat[1:length.lab,i],]
    lab.03.data1 = apply(lab.03.data,1,mean)
  }
  length.lab = length(which(lab.mat[is.na(lab.mat[,i]) == FALSE,i] %in% EE.mat[,4]))
  if(length.lab > 0){
    lab.1.mat[1:length.lab,i] = which(lab.mat[is.na(lab.mat[,i]) == FALSE,i] %in% EE.mat[,4])
    lab.1.data = preds.mat[lab.1.mat[1:length.lab,i],]
    lab.1.data1 = apply(lab.1.data,1,mean)
  }
  length.lab = length(which(lab.mat[is.na(lab.mat[,i]) == FALSE,i] %in% EE.mat[,5]))
  if(length.lab > 0){
    lab.3.mat[1:length.lab,i] = which(lab.mat[is.na(lab.mat[,i]) == FALSE,i] %in% EE.mat[,5])
    lab.3.data = preds.mat[lab.3.mat[1:length.lab,i],]
    lab.3.data1 = apply(lab.3.data,1,mean)
  }
  length.lab = length(which(lab.mat[is.na(lab.mat[,i]) == FALSE,i] %in% EE.mat[,6]))
  if(length.lab > 0){
    lab1.mat[1:length.lab,i] = which(lab.mat[is.na(lab.mat[,i]) == FALSE,i] %in% EE.mat[,6])
    lab1.data = preds.mat[lab1.mat[1:length.lab,i],]
    lab1.data1 = apply(lab1.data,1,mean)
  }
  length.lab = length(which(lab.mat[is.na(lab.mat[,i]) == FALSE,i] %in% EE.mat[,7]))
  if(length.lab > 0){
    lab3.mat[1:length.lab,i] = which(lab.mat[is.na(lab.mat[,i]) == FALSE,i] %in% EE.mat[,7])
    lab3.data = preds.mat[lab3.mat[1:length.lab,i],]
    lab3.data1 = apply(lab3.data,1,mean)
  }
  length.lab = length(which(lab.mat[is.na(lab.mat[,i]) == FALSE,i] %in% EE.mat[,8]))
  if(length.lab > 0){
    lab10.mat[1:length.lab,i] = which(lab.mat[is.na(lab.mat[,i]) == FALSE,i] %in% EE.mat[,8])
    lab10.data = preds.mat[lab10.mat[1:length.lab,i],]
    lab10.data1 = apply(lab10.data,1,mean)
  }
  length.lab = length(which(lab.mat[is.na(lab.mat[,i]) == FALSE,i] %in% zm.mat[,1]))
  if(length.lab > 0){
    lab.zm.cont[1:length.lab,i] = which(lab.mat[is.na(lab.mat[,i]) == FALSE,i] %in% zm.mat[,1])
    lab.zm.data =preds.mat[lab.zm.cont[1:length.lab,i],]
    lab.zm.data1 = apply(lab.zm.data,1,mean)
  }
  length.lab = length(which(lab.mat[is.na(lab.mat[,i]) == FALSE,i] %in% zm.mat[,2]))
  if(length.lab > 0){
    lab.1.zm[1:length.lab,i] = which(lab.mat[is.na(lab.mat[,i]) == FALSE,i] %in% zm.mat[,2])
    lab.1.zm.data = preds.mat[lab.1.zm[1:length.lab,i],]
    lab.1.zm.data1 = apply(lab.1.zm.data,1,mean)
  }
  length.lab = length(which(lab.mat[is.na(lab.mat[,i]) == FALSE,i] %in% zm.mat[,3]))
  if(length.lab > 0){
    lab1.zm[1:length.lab,i] = which(lab.mat[is.na(lab.mat[,i]) == FALSE,i] %in% zm.mat[,3])
    lab1.zm.data = preds.mat[lab1.zm[1:length.lab,i],]
    lab1.zm.data1 = apply(lab1.zm.data,1,mean)
  }
  if(i <= 4){
    length.prot = length(which(prot.mat[is.na(prot.mat[,i]) == FALSE,i] %in% EE.mat[,1]))
    if(length.lab > 0){
      prot.cont.mat[1:length.prot,i] = which(prot.mat[is.na(prot.mat[,i]) == FALSE,i] %in% EE.mat[,1])
      prot.cont.data = preds.mat[prot.cont.mat[1:length.prot,i],]
      prot.cont.data1 = apply(prot.cont.data,1,mean)
    }
    length.prot = length(which(prot.mat[is.na(prot.mat[,i]) == FALSE,i] %in% EE.mat[,2]))
    if(length.lab > 0){
      prot.01.mat[1:length.prot,i] = which(prot.mat[is.na(prot.mat[,i]) == FALSE,i] %in% EE.mat[,2])
      prot.01.data = preds.mat[prot.01.mat[1:length.prot,i],]
      prot.01.data1 = apply(prot.01.data,1,mean)
    }
    length.prot = length(which(prot.mat[is.na(prot.mat[,i]) == FALSE,i] %in% EE.mat[,3]))
    if(length.lab > 0){
      prot.03.mat[1:length.prot,i] = which(prot.mat[is.na(prot.mat[,i]) == FALSE,i] %in% EE.mat[,3])
      prot.03.data = preds.mat[prot.03.mat[1:length.prot,i],]
      prot.03.data1 = apply(prot.03.data,1,mean)
    }
    length.prot = length(which(prot.mat[is.na(prot.mat[,i]) == FALSE,i] %in% EE.mat[,4]))
    if(length.lab > 0){
      prot.1.mat[1:length.prot,i] = which(prot.mat[is.na(prot.mat[,i]) == FALSE,i] %in% EE.mat[,4])
      prot.1.data = preds.mat[prot.1.mat[1:length.prot,i],]
      prot.1.data1 = apply(prot.1.data,1,mean)
    }
    length.prot = length(which(prot.mat[is.na(prot.mat[,i]) == FALSE,i] %in% EE.mat[,5]))
    if(length.lab > 0){
      prot.3.mat[1:length.prot,i] = which(prot.mat[is.na(prot.mat[,i]) == FALSE,i] %in% EE.mat[,5])
      prot.3.data = preds.mat[prot.3.mat[1:length.prot,i],]
      prot.3.data1 = apply(prot.3.data,1,mean)
    }
    length.prot = length(which(prot.mat[is.na(prot.mat[,i]) == FALSE,i] %in% EE.mat[,6]))
    if(length.lab > 0){
      prot1.mat[1:length.prot,i] = which(prot.mat[is.na(prot.mat[,i]) == FALSE,i] %in% EE.mat[,6])
      prot1.data = preds.mat[prot1.mat[1:length.prot,i],]
      prot1.data1 = apply(prot1.data,1,mean)
    }
    length.prot = length(which(prot.mat[is.na(prot.mat[,i]) == FALSE,i] %in% EE.mat[,7]))
    if(length.lab > 0){
      prot3.mat[1:length.prot,i] = which(prot.mat[is.na(prot.mat[,i]) == FALSE,i] %in% EE.mat[,7])
      prot3.data = preds.mat[prot3.mat[1:length.prot,i],]
      prot3.data1 = apply(prot3.data,1,mean)
    }
    length.prot = length(which(prot.mat[is.na(prot.mat[,i]) == FALSE,i] %in% EE.mat[,8]))
    if(length.lab > 0){
      prot10.mat[1:length.prot,i] = which(prot.mat[is.na(prot.mat[,i]) == FALSE,i] %in% EE.mat[,8])
      prot10.data = preds.mat[prot10.mat[1:length.prot,i],]
      prot10.data1 = apply(prot10.data,1,mean)
    }
    # ZM
    length.prot = length(which(prot.mat[is.na(prot.mat[,i]) == FALSE,i] %in% zm.mat[,1]))
    if(length.lab > 0){
      prot.zm.cont[1:length.prot,i] = which(prot.mat[is.na(prot.mat[,i]) == FALSE,i] %in% zm.mat[,1])
      prot.zm.cont.data = preds.mat[prot.zm.cont[1:length.prot,i],]
      prot.zm.cont.data1 = apply(prot.zm.cont.data,1,mean)
    }
    length.prot = length(which(prot.mat[is.na(prot.mat[,i]) == FALSE,i] %in% zm.mat[,2]))
    if(length.lab > 0){
      prot.1.zm[1:length.prot,i] = which(prot.mat[is.na(prot.mat[,i]) == FALSE,i] %in% zm.mat[,2])
      prot.1.zm.data = preds.mat[prot.1.zm[1:length.prot,i],]
      prot.1.zm.data1 = apply(prot.1.zm.data,1,mean)
    }
    length.prot = length(which(prot.mat[is.na(prot.mat[,i]) == FALSE,i] %in% zm.mat[,3]))
    if(length.lab > 0){
      prot1.zm[1:length.prot,i] = which(prot.mat[is.na(prot.mat[,i]) == FALSE,i] %in% zm.mat[,3])
      prot1.zm.data = preds.mat[prot1.zm[1:length.prot,i],]
      prot1.zm.data1 = apply(prot1.zm.data,1,mean)
    }
  }
  # Here add the t-tests
  # Site Should I use equal variances?  Yes, make note of the strong assumption
  
  p.vals.ee.mat.lab[i,2] = t.test(lab.cont.data,lab.01.data, var.equal = TRUE, paired = FALSE)$p.value # t test
  p.vals.ee.mat.lab[i,3] = t.test(lab.cont.data,lab.03.data, var.equal = TRUE, paired = FALSE)$p.value
  p.vals.ee.mat.lab[i,4] = t.test(lab.cont.data,lab.1.data, var.equal = TRUE, paired = FALSE)$p.value
  p.vals.ee.mat.lab[i,5] = t.test(lab.cont.data,lab.3.data, var.equal = TRUE, paired = FALSE)$p.value
  p.vals.ee.mat.lab[i,6] = t.test(lab.cont.data,lab1.data, var.equal = TRUE, paired = FALSE)$p.value
  p.vals.ee.mat.lab[i,7] = t.test(lab.cont.data,lab3.data, var.equal = TRUE, paired = FALSE)$p.value
  if( i != 1){
  p.vals.ee.mat.lab[i,8] = t.test(lab.cont.data,lab10.data, var.equal = TRUE, paired = FALSE)$p.value
}
  p.vals.zm.mat.lab[i,2] = t.test(lab.zm.data,lab.1.zm.data, var.equal = TRUE, paired = FALSE)$p.value
  p.vals.zm.mat.lab[i,3] = t.test(lab.zm.data,lab1.zm.data, var.equal = TRUE, paired = FALSE)$p.value
  if(i <= 4){
    p.vals.ee.mat.prot[i,2] = t.test( prot.cont.data1,prot.01.data1, var.equal = TRUE, paired = FALSE)$p.value
    p.vals.ee.mat.prot[i,3] = t.test( prot.cont.data1,prot.03.data1, var.equal = TRUE, paired = FALSE)$p.value
    p.vals.ee.mat.prot[i,4] = t.test( prot.cont.data1,prot.1.data1, var.equal = TRUE, paired = FALSE)$p.value
    p.vals.ee.mat.prot[i,5] = t.test( prot.cont.data1,prot.3.data1, var.equal = TRUE, paired = FALSE)$p.value
    p.vals.ee.mat.prot[i,6] = t.test( prot.cont.data1,prot1.data1, var.equal = TRUE, paired = FALSE)$p.value
    p.vals.ee.mat.prot[i,7] = t.test( prot.cont.data1,prot3.data1, var.equal = TRUE, paired = FALSE)$p.value
    p.vals.ee.mat.prot[i,8] = t.test( prot.cont.data1,prot10.data1, var.equal = TRUE, paired = FALSE)$p.value
    p.vals.zm.mat.prot[i,2] = t.test( prot.zm.cont.data1,prot.1.zm.data1, var.equal = TRUE, paired = FALSE)$p.value
    p.vals.zm.mat.prot[i,3] = t.test( prot.zm.cont.data1,prot1.zm.data1, var.equal = TRUE, paired = FALSE)$p.value
  }
  #prot.cont.data
  # Prot
  
}

p.val.thresh = .05

#p.vals.ee.mat.lab = matrix(NA,nrow = 19, ncol = 8)
sig.mat.lab = p.vals.ee.mat.lab < p.val.thresh
#p.vals.zm.mat.lab = matrix(NA,nrow = 19, ncol = 3)
sig.zm.lab = p.vals.zm.mat.lab < p.val.thresh
#p.vals.ee.mat.prot = matrix(NA,nrow = 4, ncol = 8)
sig.ee.prot = p.vals.ee.mat.prot < p.val.thresh
#p.vals.zm.mat.prot = matrix(NA, nrow = 4, ncol = 3)
sig.zm.prot = p.vals.zm.mat.prot < p.val.thresh

########################################################################
# plots EDA
par(mar = c(1.8,1.8,1.3,1.3), mfrow = c(1,1))
boxplot(bio.frame$weight ~ bio.frame$protocol, main = "Weight by Protocol")
boxplot(bio.frame$uterus ~ bio.frame$protocol, main = "Uterus by Protocol")

# Posterior
par(mar = c(1.8,1.8,1.3,1.3), mfrow = c(1,1))
hist(post.samps.real[,"int"], breaks = 60, main = "Intercept", freq = FALSE)
lines(density(post.samps.real[,"int"]))
hist(post.samps.real[,"alpha.mu"], breaks = 60, main = expression(mu[alpha]), freq = FALSE)
lines(density(post.samps.real[,"alpha.mu"]))
hist(post.samps.real[,"gamma.mu"], breaks = 60, main = expression(mu[gamma]), freq = FALSE)
lines(density(post.samps.real[,"gamma.mu"]))
hist(post.samps.real[,"beta1"], breaks = 60, main = expression(beta[1]), freq = FALSE)
lines(density(post.samps.real[,"beta1"]))
hist(post.samps.real[,"beta2"], breaks = 60, main = expression(beta[2]), freq = FALSE)
lines(density(post.samps.real[,"beta2"]))
hist(post.samps.real[,"beta3"], breaks = 60, main = expression(beta[3]), freq = FALSE)
lines(density(post.samps.real[,"beta3"]))

# Lab 1 visualization 6 possibility, 9, 11, 16
lab1.index = which(bio.frame$labnum == 9)
lab1.preds = preds.mat[lab1.index,]
lab1.preds = lab1.preds[lab1.preds > 0]
lab1.real = bio.frame$uterus[lab1.index]
par(mar = c(1.8,1.8,1.3,1.3))
plot(density(lab1.real), main = "Hatano Lab")
legend("topright",lty = c(1,2), c("Real Data", "Pred. Data"))
lines(density(lab1.preds) ,lty = 2)

# Protocol
prot1.index = which(bio.frame$protnum == 3)
prot1.preds = preds.mat[prot1.index,]
prot1.preds = prot1.preds[prot1.preds > 0]
prot1.real = bio.frame$uterus[prot1.index]
par(mar = c(1.8,1.8,1.3,1.3))
plot(density(prot1.real), main = "Protocol C")
legend("topright",lty = c(1,2) ,c("Real Data", "Pred. Data"))
lines(density(prot1.preds) ,lty = 2)

# Now we use these indices to do the t-tests
# I think I'll probably be able to incorporate the t-tests into this loop.
# Tonight though, I am going to start working on overleaf

sum(bio.frame$EE > 0)
sum(bio.frame$ZM > 0)
