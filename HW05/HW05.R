bio = read.table("http://stat.duke.edu/sites/stat.duke.edu/files/bioassay.txt", header = TRUE)
bio = read.table("C:/Users/Zachary/Desktop/Winter 2017/STA 723 Case Studies/STA_723_case/HW05/bioassay.txt", header = TRUE)
library(ggplot2)
library(R2jags)
library(R2WinBUGS)
library(MCMCPack)
library(rjags)
library(lme4)
# VArying the amount of agonist or antagonist
# Response = weight of uterus
par(mar = c(1.8,1.8,1.3,1.3))

bio$uterus = as.numeric(levels(bio$uterus))[bio$uterus]
bio$weight = as.numeric(levels(bio$weight))[bio$weight]
bio$EE = as.numeric(levels(bio$EE))[bio$EE]
bio$ratio = bio$uterus / (1000*bio$weight)
na.index = which(is.na(bio$uterus) == TRUE)
bio = bio[-na.index,]

bio.frame = as.data.frame(bio)
bio.frame$EE = as.factor(bio.frame$EE)
bio.frame$ZM = as.factor(bio.frame$ZM)
bio.frame$labnum = as.integer(bio.frame$lab)

## We eliminate the NA
s


length(unique(bio$lab))
# EDA
plot(bio$weight,bio$uterus)
f = ggplot(bio, aes(x = weight, y = uterus, color = lab))
fp = f + geom_point()
fp + facet_wrap(.~lab)
## Uterus by weight
par(mar = c(1.8,1.8,1.3,1.3), mfrow = c(1,4))
levels.lab = unique(bio$lab)
for(i in 1:4){
  plot(bio$weight[bio$lab == levels.lab[i]] , bio$uterus[bio$lab == levels.lab[i]])
}

## Uterus and weight by lab
boxplot(bio$uterus ~ bio$lab)
boxplot(bio$weight ~ bio$lab)
par(mar = c(1.8,1.8,1.3,1.3), mfrow = c(1,4))
# EM and what
for(i in 1:4){
  plot(bio$EE[bio$lab == levels.lab[i]],bio$uterus[bio$lab == levels.lab[i]])
  legend("bottomright",paste(levels.lab[i]))
}
par(mar = c(1.8,1.8,1.2,1.2), mfrow = c(1,2))
for(i in 3:4){
  boxplot(bio$uterus[bio$lab == levels.lab[i]]~bio$EE[bio$lab == levels.lab[i]])
  legend("topleft",paste(levels.lab[i]))
}
boxplot(bio$uterus ~ bio$EE)
legend("topleft","All")
par(mar = c(1.8,1.8,1.3,1.3), mfrow = c(4,5))
for(i in 1:19){
  plot(bio$ZM[bio$lab == levels.lab[i]],bio$uterus[bio$lab == levels.lab[i]])
  legend("topright",paste(i))
}
par(mar = c(1.8,1.8,1.2,1.2), mfrow = c(1,2))
for(i in 3:4){
  boxplot(bio$uterus[bio$lab == levels.lab[i]]~bio$ZM[bio$lab == levels.lab[i]])
  legend("topright",paste(levels.lab[i]))
}
boxplot(bio$uterus ~ bio$ZM)
legend("topleft","All")

par(mar = c(1.8,1.8,1.3,1.3), mfrow = c(4,5))
for(i in 1:19){
  boxplot(bio$uterus[bio$lab == levels.lab[i]]~bio$protocol[bio$lab == levels.lab[i]])
  legend("topright",paste(i))
}
par(mar = c(1.8,1.8,1.3,1.3), mfrow = c(3,4))
levels.EE = unique(bio$EE)
for(i in 1:8){
  boxplot(bio$uterus[bio$EE == levels.EE[i]] ~ bio$protocol[bio$EE == levels.EE[i]])
  legend("topleft",paste("EE= ",levels.EE[i]))
}
levels.ZM = unique(bio$ZM)
for(i in 1:3){
  boxplot(bio$uterus[bio$ZM == levels.ZM[i]] ~ bio$protocol[bio$ZM == levels.ZM[i]])
  legend("topleft",paste("ZM= ",levels.ZM[i]))
  
}
boxplot(bio$uterus ~ bio$protocol)
legend("topleft","All")
# Group effect = I'm skeptical initially
for(i in 1:19){
  plot(bio$group[bio$lab == levels.lab[i]],bio$uterus[bio$lab == levels.lab[i]])
  legend("topleft",paste(levels.lab[i]))
}

## Protocol
par(mar = c(1.8,1.8,1.2,1.2), mfrow = c(1,1))
plot(bio$protocol,bio$uterus)
f = ggplot(bio, aes(x = weight, y = uterus, color = protocol))
fp = f + geom_point()
fp

par(mar = c(1.8,1.8,1.2,1.2), mfrow = c(3,2))
hist(bio$uterus, breaks = 60)
hist(bio$uterus[bio$protocol == "A"], main = "A", breaks = 60)
hist(bio$uterus[bio$protocol == "B"], main = "B", breaks = 60)
hist(bio$uterus[bio$protocol == "C"], main = "C", breaks = 60)
hist(bio$uterus[bio$protocol == "D"], main = "D", breaks = 60)

par(mar = c(1.8,1.8,1.2,1.2), mfrow = c(3,2))
hist(log(bio$uterus), breaks = 60)
hist(log(bio$uterus[bio$protocol == "A"]), main = "A", breaks = 60)
hist(log(bio$uterus[bio$protocol == "B"]), main = "B", breaks = 60)
hist(log(bio$uterus[bio$protocol == "C"]), main = "C", breaks = 60)
hist(log(bio$uterus[bio$protocol == "D"]), main = "D", breaks = 60)

par(mar = c(1.8,1.8,1.2,1.2), mfrow = c(3,2))
hist(bio$ratio, breaks = 60)
hist(bio$ratio[bio$protocol == "A"], main = "A", breaks = 60)
hist(bio$ratio[bio$protocol == "B"], main = "B", breaks = 60)
hist(bio$ratio[bio$protocol == "C"], main = "C", breaks = 60)
hist(bio$ratio[bio$protocol == "D"], main = "D", breaks = 60)

cor(bio$uterus,bio$weight, na.rm = TRUE)

# Uterus by EE and ZM
par(mfrow = c(1,1))
boxplot(bio.frame$uterus ~ bio.frame$EE)
boxplot(bio.frame$uterus ~ bio.frame$ZM)

# Table of EE and lab

sum(bio.frame$EE != 0)
sum(bio.frame$ZM != 0)
table(bio.frame$lab[bio.frame$ZM != 0])

for(i in 1:19){
  plot(bio$group[bio$lab == levels.lab[i]],bio$uterus[bio$lab == levels.lab[i]])
  legend("topleft",paste(levels.lab[i]))
}

for(i in 1:19){
  print(unique(bio.frame$EE[bio.frame$lab == levels.lab[i]]))
}

boxplot(bio.frame$uterus ~ bio.frame$protocol)
# Prelim Models
rand.lab.prot = lmer(uterus ~ 1 + (1|protocol) + (1 |lab), data = bio.frame)
rand.lab.ee = lmer(uterus ~ 1 + (1|EE) + (EE|lab), data = bio.frame)


basic.hier = lmer(uterus ~ 1 + (1 | protocol) + (weight | protocol) + (1 | lab) + (EE | lab) + (ZM | lab), data = bio.frame)
basic.hier = lmer(uterus ~ 1 (1 | lab) + ( | lab) + (ZM | lab) , data = bio.frame)
basic.lab = lmer(uterus ~ 1 + numEE + (numEE | lab) + ZM + (ZM | lab) , data = bio.frame)
basic.lab.int = lm(uterus~factor(EE)+ factor(protocol) + factor(ZM) + factor(lab) + factor(lab)*factor(EE) + factor(lab)*factor(ZM), data = bio.frame)
summary(basic.lab.int)
basic.prot.int = lm(uterus~factor(EE) +factor(lab) +factor(ZM) + factor(protocol) + factor(protocol)*factor(EE) + factor(protocol)*factor(ZM), data = bio.frame)
summary(basic.prot.int)

lmer(V1 ~ (1|V2) + V3)
lmer(V1 ~ (1|V2) + V3 + (0+V3|V2))

lmer(V1 ~ (1+V3|V2) + V3)

lmer(V1 ~ (1+V3*V4|V2) + V3*V4)


# Hierarchical models
bio.frame = as.data.frame(bio)
bio.frame$EE = as.factor(bio.frame$EE)
bio.frame$ZM = as.factor(bio.frame$ZM)
bio.frame$protnum = as.integer(bio.frame$protocol)
bio.frame$labnum = as.integer(bio.frame$lab)
bio.frame$numEE = as.integer(bio.frame$EE)
bio.frame$numZM = as.integer(bio.frame$ZM)

labnum = bio.frame$labnum
protocol = bio.frame$protnum
weight = bio.frame$weight
group = bio.frame$group
Y = bio.frame$uterus
EE = bio.frame$numEE
ZM = bio.frame$numZM
N = nrow(bio.frame)
aN = length(unique(bio.frame$labnum))
gN = length(unique(bio.frame$protnum))
b2N = length(unique(bio.frame$numEE))
b3N = length(unique(bio.frame$numZM))
data = list(labnum = labnum, protocol = protocol, weight = weight, Y=Y, EE = EE, ZM = ZM,
            N = N, aN = aN, gN = gN, b2N = b2N, b3N = b3N, group = group)
hier.bio = function(){
  for(i in 1:N){
    Y[i] ~ dnorm(mu[i],phi)
    mu[i] <- int + alpha[labnum[i]] + gamma[protocol[i]] + beta1 * weight[i] + beta2[EE[i]] + beta3[ZM[i]] + beta4 * group[i] 
  }
  alpha[1] <- 0
  gamma[1] <- 0
  beta2[1] <- 0
  beta3[1] <- 0
  for(j in 2:aN){
    alpha[j] ~ dnorm(alpha.mu, alpha.phi)
  }
  for(k in 2:gN){
    gamma[k] ~ dnorm(gamma.mu, gamma.phi)
  }
  for(l in 2:b2N){
    beta2[l] ~ dnorm(beta2.mu, beta2.phi)
  }
  for(m in 2:b3N){
    beta3[m] ~ dnorm(beta3.mu, beta3.phi)
  }
  # Overall variance
  phi ~ dgamma(.00001,.00001)
  sigma2 <- 1 / (phi*phi)
  # Lab
  alpha.mu ~ dnorm(0,.00001)
  alpha.phi ~ dgamma(.00001,.00001)
  sigma2.alpha <- 1 / alpha.phi
  
  # Protocol
  gamma.mu ~ dnorm(0,.00001)
  gamma.phi ~ dgamma(.00001,.00001)
  sigma2.gamma <- 1 / gamma.phi
  
  # EE
  beta2.mu ~ dnorm(0,.00001)
  beta2.phi ~ dgamma(.00001,.00001)
  sigma2.beta2 <- 1 / beta2.phi
  
  # ZM
  beta3.mu ~ dnorm(0,.00001)
  beta3.phi ~ dgamma(.00001,.00001)
  sigma2.beta3 <- 1 / beta3.phi
  
  # Overall covariates
  int ~ dnorm(0,.00001)
  beta1 ~ dnorm(0,.00001)
  beta4 ~ dnorm(0,.00001)
}
parameters = c("int", "beta1", "beta4", "alpha", "gamma", "beta2","beta3", "alpha.mu", "gamma.mu","beta2.mu", "beta3.mu",
               "sigma2.alpha","sigma2.gamma","sigma2.beta2","sigma2.beta3")

model.file = "hier_bio.txt"
write.model(hier.bio, model.file)

sims = jags(data, inits = NULL, parameters.to.save = parameters, model.file = hier.bio,n.chains = 2, n.iter = 10000)
post.samps = sims$BUGSoutput$sims.matrix
summary(post.samps)
quantile(post.samps[,"beta4"],c(.025,.975))
# More simple model with just site, group, and EE, ZM
table(bio.frame$protocol,bio.frame$lab)
table(bio.frame$EE,bio.frame$lab)
table(bio.frame$ZM, bio.frame$lab)

# Posterior analysis

###############################################################
#
###############################################################
bio.frame = as.data.frame(bio)
bio.frame$EE = as.factor(bio.frame$EE)
bio.frame$ZM = as.factor(bio.frame$ZM)
bio.frame$protnum = as.integer(bio.frame$protocol)
bio.frame$labnum = as.integer(bio.frame$lab)
bio.frame$numEE = as.integer(bio.frame$EE)
bio.frame$numZM = as.integer(bio.frame$ZM)
#bio.frame$EE = as.numeric(levels(bio.frame$EE))[bio.frame$EE]
#bio.frame$ZM = as.numeric(levels(bio.frame$ZM))[bio.frame$ZM]
mod.lm = lm(uterus ~ + factor(EE) + factor(ZM), data = bio.frame)
mod.mat = model.matrix(mod.lm)
mod.mat = matrix(0,ncol = 11, nrow = nrow(bio.frame))
for(i in 1:8){
  mod.mat[,i] = as.numeric(bio.frame$EE == levels(bio.frame$EE)[i])
}
for(i in 1:3){
  mod.mat[,i+8] = as.numeric(bio.frame$ZM == levels(bio.frame$ZM)[i])
}
colnames(mod.mat) = c("EE0","EE0.01","EE0.03","EE0.1","EE0.3","EE1","EE3","EE10","ZM0","ZM0.1","ZM1")

EE0 = mod.mat[,1]
EE0.01 = mod.mat[,2]
EE0.03= mod.mat[,3]
EE0.1= mod.mat[,4]
EE0.3= mod.mat[,5]
EE1= mod.mat[,6]
EE3= mod.mat[,7]
EE10= mod.mat[,8]
ZM0= mod.mat[,9]
ZM0.1= mod.mat[,10]
ZM1= mod.mat[,11]
labnum = bio.frame$labnum
protocol = bio.frame$protnum
weight = bio.frame$weight
group = bio.frame$group
Y = bio.frame$uterus
EE = bio.frame$EE
ZM = bio.frame$ZM
N = nrow(bio.frame)
aN = length(unique(bio.frame$labnum))
gN = length(unique(bio.frame$protnum))
b2N = length(unique(bio.frame$numEE))
b3N = length(unique(bio.frame$numZM))
data = list(labnum = labnum, protocol = protocol, Y=Y, EE0 = EE0, EE0.01 = EE0.01, EE0.03 = EE0.03
            , EE0.1 = EE0.1, EE0.3 = EE0.3, EE1 = EE1, EE3 = EE3, EE10 = EE10, ZM0 = ZM0, ZM0.1 = ZM0.1,
            ZM1 = ZM1,N = N, aN = aN, gN = gN)
hier.bio.lin = function(){
  for(i in 1:N){
    Y[i] ~ dnorm(mu[i],phi)
    mu[i] <- int + alpha[labnum[i]] + gamma[protocol[i]] + beta_EE0[labnum[i]] * EE0[i] +beta_EE0.01[labnum[i]] * EE0.01[i]+beta_EE0.03[labnum[i]] * EE0.03[i]+beta_EE0.1[labnum[i]] * EE0.1[i] + 
      beta_EE0.3[labnum[i]] * EE0.3[i] + beta_EE1[labnum[i]] * EE1[i] + beta_EE3[labnum[i]] * EE3[i] + beta_EE3[labnum[i]] * EE3[i]+ beta_EE10[labnum[i]] * EE10[i] + beta_ZM0[labnum[i]] * ZM0[i] + 
      beta_ZM0.1[labnum[i]] * ZM0.1[i] + beta_ZM1[labnum[i]] * ZM1[i] + beta_EE_prot0[protocol[i]] * EE0[i] +beta_EE_prot0.01[protocol[i]] * EE0.01[i] + beta_EE_prot0.03[protocol[i]] * EE0.03[i] + 
      beta_EE_prot0.1[protocol[i]] * EE0.1[i] + beta_EE_prot0.3[protocol[i]] * EE0.3[i]+beta_EE_prot1[protocol[i]] * EE1[i] + beta_EE_prot3[protocol[i]] * EE3[i] + beta_EE_prot3[protocol[i]] * EE3[i] + 
      beta_EE_prot10[protocol[i]] * EE10[i] + beta_ZM_prot0[protocol[i]] * ZM0[i] + beta_ZM_prot0.1[protocol[i]] * ZM0.1[i] + beta_ZM_prot1[protocol[i]] * ZM1[i]
  }
  alpha[1] <- 0
  gamma[1] <- 0
  for(j in 2:aN){
    alpha[j] ~ dnorm(alpha.mu, alpha.phi)
  }
  for(m in 1:aN){
    beta_EE0[m] ~ dnorm(0, .00001)
    beta_EE0.01[m] ~ dnorm(0, .00001)
    beta_EE0.03[m] ~ dnorm(0, .00001)
    beta_EE0.1[m] ~ dnorm(0, .00001)
    beta_EE0.3[m] ~ dnorm(0, .00001)
    beta_EE1[m] ~ dnorm(0, .00001)
    beta_EE3[m] ~ dnorm(0, .00001)
    beta_EE10[m] ~ dnorm(0, .00001)
    beta_ZM0[m] ~ dnorm(0, .00001)
    beta_ZM0.1[m] ~ dnorm(0, .00001)
    beta_ZM1[m] ~ dnorm(0, .00001)
  }
  for(k in 2:gN){
    gamma[k] ~ dnorm(gamma.mu, gamma.phi)
  }
  for(l in 1:gN){
    beta_EE_prot0[l] ~ dnorm(0,1E-06)
    beta_EE_prot0.01[l] ~ dnorm(0,1E-06)
    beta_EE_prot0.03[l] ~ dnorm(0,1E-06)
    beta_EE_prot0.1[l] ~ dnorm(0,1E-06)
    beta_EE_prot0.3[l] ~ dnorm(0,1E-06)
    beta_EE_prot1[l] ~ dnorm(0,1E-06)
    beta_EE_prot3[l]~ dnorm(0,1E-06)
    beta_EE_prot10[l] ~ dnorm(0,1E-06)
    beta_ZM_prot0[l] ~ dnorm(0,1E-06)
    beta_ZM_prot0.1[l] ~ dnorm(0,1E-06)
    beta_ZM_prot1[l]~ dnorm(0,1E-06)
  }
  # Overall variance
  phi ~ dgamma(.00001,.00001)
  sigma2 <- 1 / (phi*phi)
  # Lab
  alpha.mu ~ dnorm(0,.00001)
  alpha.phi ~ dgamma(.00001,.00001)
  sigma2.alpha <- 1 / alpha.phi
  
  # Protocol
  gamma.mu ~ dnorm(0,.00001)
  gamma.phi ~ dgamma(.00001,.00001)
  sigma2.gamma <- 1 / gamma.phi
  
  # EE
  #beta_EE.mu ~ dnorm(0,.00001)
  #beta_EE.phi ~ dgamma(.00001,.00001)
  #sigma2.beta_EE <- 1 / beta_EE.phi
  
  # ZM
  #beta_ZM.mu ~ dnorm(0,.00001)
  #beta_ZM.phi ~ dgamma(.00001,.00001)
  #sigma2.beta_ZM <- 1 / beta_EE.phi
  
  # Overall covariates
  int ~ dnorm(0,.00001)
}
parameters = c("int", "alpha", "gamma", "alpha.mu", "gamma.mu",
               "beta_EE0","beta_EE0.01","beta_EE0.03","beta_EE0.1","beta_EE0.3","beta_EE1","beta_EE3","beta_EE10",
               "beta_ZM0","beta_ZM0.1","beta_ZM1",
               "beta_EE_prot0","beta_EE_prot0.01","beta_EE_prot0.03","beta_EE_prot0.1","beta_EE_prot0.3","beta_EE_prot1","beta_EE_prot3","beta_EE_prot10",
               "beta_ZM_prot0","beta_ZM_prot0.1","beta_ZM_prot1",
               "sigma2.alpha","sigma2.gamma")

model.file = "hier_bio_lin.txt"
write.model(hier.bio.lin, model.file)

sims.lin = jags(data, inits = NULL, parameters.to.save = parameters, model.file = hier.bio.lin,n.chains = 2, n.iter = 20000,n.thin = 1)
###################################################
post.anova.draws = sims.lin$BUGSoutput$sims.matrix
summary(post.anova.draws)





###############################################
post.samps.lin = sims.lin$BUGSoutput$sims.matrix
apply(post.samps.lin,2,quantile,c(.025,.975))
alpha.index = 1:19
beta.EE.index = 21:39
beta.ZM.index = 40:58
gamma.index = 60:64
post.samps.lin[1,gamma.index]
## Alpha posterior draws
par(mar = c(1.8,1.8,1.3,1.3), mfrow = c(4,5))
for(i in 1:19){
  plot(density(post.samps.lin[,alpha.index[i]]))
}
par(mar = c(1.8,1.8,1.3,1.3), mfrow = c(4,5))
for(i in 1:19){
  plot(density(post.samps.lin[,beta.EE.index[i]]))
}
par(mar = c(1.8,1.8,1.3,1.3), mfrow = c(4,5))
for(i in 1:19){
  plot(density(post.samps.lin[,beta.ZM.index[i]]))
}

par(mar = c(1.8,1.8,1.3,1.3), mfrow = c(2,2))
for(i in 1:4){
  plot(density(post.samps.lin[,gamma.index[i]]))
}

## Beta EE posterior draws
summary(post.samps.lin)
quantile(post.samps[,"beta4"],c(.025,.975))

# Figure out if this is valid in the slightest.  Build in a posterior predictive check
# Figure out how to do the anova stuff.
# I think it will all come in through the interactino term
