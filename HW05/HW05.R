bio = read.table("http://stat.duke.edu/sites/stat.duke.edu/files/bioassay.txt", header = TRUE)
bio = read.table("C:/Users/Zachary/Desktop/Winter 2017/STA 723 Case Studies/STA_723_case/HW05/bioassay.txt", header = TRUE)
library(ggplot2)
library(R2jags)
library(R2WinBUGS)
library(rjags)
library(lme4)
# VArying the amount of agonist or antagonist
# Response = weight of uterus
par(mar = c(1.8,1.8,1.3,1.3))

na.index = which(is.na(bio$uterus))

bio$uterus = as.numeric(levels(bio$uterus))[bio$uterus]
bio$weight = as.numeric(levels(bio$weight))[bio$weight]
bio$ratio = bio$uterus / (1000*bio$weight)

bio.frame = as.data.frame(bio)
bio.frame$EE = as.factor(bio.frame$EE)
bio.frame$ZM = as.factor(bio.frame$ZM)
bio.frame$labnum = as.integer(bio.frame$lab)

## We eliminate the NA
s
bio = bio[-na.index,]

length(unique(bio$lab))
# EDA
plot(bio$weight,bio$uterus)
f = ggplot(bio, aes(x = weight, y = uterus, color = lab))
fp = f + geom_point()
fp + facet_wrap(.~lab)
## Uterus by weight
par(mar = c(1.8,1.8,1.3,1.3), mfrow = c(4,5))
levels.lab = unique(bio$lab)
for(i in 1:19){
  plot(bio$weight[bio$lab == levels.lab[i]] , bio$uterus[bio$lab == levels.lab[i]])
}

## Uterus and weight by lab
boxplot(bio$uterus ~ bio$lab)
boxplot(bio$weight ~ bio$lab)
par(mar = c(1.8,1.8,1.3,1.3), mfrow = c(4,5))
# EM and what
for(i in 1:19){
  plot(bio$EE[bio$lab == levels.lab[i]],bio$uterus[bio$lab == levels.lab[i]])
  legend("bottomright",paste(i))
}
for(i in 1:19){
  boxplot(bio$uterus[bio$lab == levels.lab[i]]~bio$EE[bio$lab == levels.lab[i]])
  legend("topleft",paste(i))
}
par(mar = c(1.8,1.8,1.3,1.3), mfrow = c(4,5))
for(i in 1:19){
  plot(bio$ZM[bio$lab == levels.lab[i]],bio$uterus[bio$lab == levels.lab[i]])
  legend("topright",paste(i))
}
par(mar = c(1.8,1.8,1.3,1.3), mfrow = c(4,5))
for(i in 1:19){
  boxplot(bio$uterus[bio$lab == levels.lab[i]]~bio$ZM[bio$lab == levels.lab[i]])
  legend("topright",paste(i))
}

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
basic.hier = lmer(uterus ~ 1 + (1 | protocol) + (weight | protocol) + (1 | lab) + (EE | lab) + (ZM | lab), data = bio.frame)
basic.hier = lmer(uterus ~ 1 (1 | lab) + (EE | lab) + (ZM | lab) , data = bio.frame)

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

# Posterior analysis

###############################################################
#
###############################################################
bio.frame = as.data.frame(bio)
bio.frame$EE = as.factor(bio.frame$EE)
bio.frame$ZM = as.factor(bio.frame$ZM)
bio.frame$protnum = as.integer(bio.frame$protocol)
bio.frame$labnum = as.integer(bio.frame$lab)
#bio.frame$numEE = as.integer(bio.frame$EE)
#bio.frame$numZM = as.integer(bio.frame$ZM)
bio.frame$EE = as.numeric(levels(bio.frame$EE))[bio.frame$EE]
bio.frame$ZM = as.numeric(levels(bio.frame$ZM))[bio.frame$ZM]


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
data = list(labnum = labnum, protocol = protocol, Y=Y, EE = EE, ZM = ZM,
            N = N, aN = aN, gN = gN)
hier.bio.lin = function(){
  for(i in 1:N){
    Y[i] ~ dnorm(mu[i],phi)
    mu[i] <- int + alpha[labnum[i]] + gamma[protocol[i]] + beta_EE[labnum[i]] * EE[i] + beta_ZM[labnum[i]] * ZM[i]
  }
  alpha[1] <- 0
  gamma[1] <- 0
  beta_EE[1] <- 0
  beta_ZM[1] <- 0
  for(j in 2:aN){
    alpha[j] ~ dnorm(alpha.mu, alpha.phi)
    beta_EE[j] ~ dnorm(0, .00001)
    beta_ZM[j] ~ dnorm(0, .00001)
  }
  for(k in 2:gN){
    gamma[k] ~ dnorm(gamma.mu, gamma.phi)
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
parameters = c("int", "alpha", "gamma", "beta_EE","beta_ZM", "alpha.mu", "gamma.mu",
               "sigma2.alpha","sigma2.gamma")

model.file = "hier_bio_lin.txt"
write.model(hier.bio.lin, model.file)

sims.lin = jags(data, inits = NULL, parameters.to.save = parameters, model.file = hier.bio.lin,n.chains = 2, n.iter = 20000,n.thin = 1)
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
