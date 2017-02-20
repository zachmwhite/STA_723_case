# HW04
library(xlsx)
library(R2jags)
library(rjags)
library(R2WinBUGS)

spring = read.xlsx2("~/Winter 2017/STA 723 Case Studies/STA_723_case/HW04/springbok.xls",sheetIndex = 1)
spring = read.xlsx2("C:/Users/Zachary/Desktop/Winter 2017/STA 723 Case Studies/STA_723_case/HW04/springbok.xls",1)

spring$HourFromNoon = round(as.numeric(levels(spring$HourFromNoon))[spring$HourFromNoon], 3)
spring$COUNTS = as.numeric(levels(spring$COUNTS))[spring$COUNTS]
spring$DATE = as.numeric(levels(spring$DATE))[spring$DATE]
spring$YEAR = as.numeric(levels(spring$YEAR))[spring$YEAR]
spring = spring[,-1]

spring.mat = as.matrix(spring)
pairs(spring)

ifelse(spring$DATE >= 1 & spring$DATE <= 31,spring$month = 1,
       ifelse(spring$DATE >31 & spring$DATE <= 59,spring$DATE = 2,
              ifelse))

m1 <- glm(num_awards ~ prog + math, family="poisson", data=p)

full.reg = lm(COUNTS~ ., data = spring)
summary(full.reg)

site1 = spring[spring$SITEI == 1,]
lm.model = lm(COUNTS~ DATE, data = site1)
x = seq(0,30,by = 1)
x.data = data.frame(DATE = x)
preds = predict.glm(lm.model,x.data)

plot(site1$DATE,site1$COUNTS)
lines(x,preds, col = "black")
for(i in 2:12){
  site.spec = spring[spring$SITEI == i,]
  lm.spec = lm(COUNTS~ DATE, data = site.spec)
  x = seq(0,30, by = 1)
  x.frame = data.frame(DATE = x)
  pred = predict.lm(lm.spec, x.frame)
  lines(x,pred, col = i)
}

########################################3
# Hours after noon
lm.model = lm(COUNTS~ HourFromNoon, data = site1)
x = seq(0,500,by = 1)
x.data = data.frame(HourFromNoon = x)
preds = predict.glm(lm.model,x.data)

plot(site1$HourFromNoon,site1$COUNTS)
lines(x,preds, col = "black")
for(i in 2:12){
  site.spec = spring[spring$SITEI == i,]
  lm.spec = lm(COUNTS~ HourFromNoon, data = site.spec)
  x = seq(0,500, by = 1)
  x.frame = data.frame(HourFromNoon = x)
  pred = predict.lm(lm.spec, x.frame)
  lines(x,pred, col = i)
}

###############################
# EDA
###############################
par(mar = c(1.8,1.8,1.2,1.2),mfrow = c(3,4))
for(i in 1:length(unique(spring$SITEI))){
  plot(spring$YEAR[spring$SITEI == i] , spring$COUNTS[spring$SITEI == i], ylab = paste("Count"))
  legend("topright", legend = paste("Site ",i))
}

##############################################
# JAGS
#############################################

model.hier = function(){
  for(i in 1:n){
    Y[i] ~ dpois(mu[i])
    log(mu[i]) <- alpha[year[i]] + delta[siteid[i]]  + beta1 * date[i] + beta2 * hour[i]
  }
  for(j in 1:aN){
<<<<<<< HEAD
    alpha[j] ~ dnorm(0, inv.tau2) #YEAR
  }
  for(k in 1:dN){
    delta[k] ~ dnorm(0, inv.phi2) #SITEID
  }
  # Priors for Year
  #alpha ~ dnorm(0,.0001)
  inv.tau2 ~ dgamma(0.001,.001)
  tau2 <- 1 / inv.tau2
  
  # Priors for SITEID
  #delta ~ dnorm(0,.0001)
  inv.phi2 ~ dgamma(.001,.001)
  phi2 <- 1 / inv.phi2
  
  # Intercepts and other Betas
  beta0 ~ dnorm(0,.0001)
  beta1 ~ dnorm(0,.0001)
  beta2 ~ dnorm(0,.0001)
  
}




model.hier = function(){
  for(i in 1:n){
    log(mu[i]) <- alpha[year[i]] + delta[siteid[i]] + beta0 +beta1 * date[i] + beta2 * hour[i]
    Y[i] ~ dpois(mu[i])
  }
  alpha[1] <- 0
  delta[1] <- 0
  
  for(j in 2:aN){
    alpha[j] ~ dnorm(A, inv.tau2) #YEAR
  }
  for(k in 2:dN){
    delta[k] ~ dnorm(D, inv.phi2) #SITEID
  }
  # Priors for Year
  A ~ dnorm(0,.0001)
=======
    alpha[j] ~ dnorm(alpha.0, inv.tau2) #YEAR
  }
  for(k in 1:dN){
    delta[k] ~ dnorm(delta.0, inv.phi2) #SITEID
  }
  # Priors for Year
  alpha.0 ~ dnorm(0,.0001)
>>>>>>> 75a6b3eb953dc2d09a88d42aa01eda070913ff68
  inv.tau2 ~ dgamma(0.001,.001)
  tau2 <- 1 / inv.tau2
  
  # Priors for SITEID
<<<<<<< HEAD
  D ~ dnorm(0,.0001)
=======
  delta.0 ~ dnorm(0,.0001)
>>>>>>> 75a6b3eb953dc2d09a88d42aa01eda070913ff68
  inv.phi2 ~ dgamma(.001,.001)
  phi2 <- 1 / inv.phi2
  
  # Intercepts and other Betas
  beta0 ~ dnorm(0,.0001)
  beta1 ~ dnorm(0,.0001)
  beta2 ~ dnorm(0,.0001)
  
}



data.jags = list(Y = spring$COUNTS, n = nrow(spring), year = spring$YEAR - 1989,
                 siteid = spring$SITEI, date = spring$DATE, hour = spring$HourFromNoon,
<<<<<<< HEAD
                 aN = length(unique(spring$YEAR)), dN = length(unique(spring$SITEI)))

parameters = c("alpha","delta","beta1","beta2","tau2","phi2","A","D","beta0")

jags.res = jags(model = model.hier, data = data.jags, n.chains = 1,
                n.iter = 22000, n.burnin = 2000, inits = NULL,
                par = parameters, n.thin = 1)

out = as.mcmc(jags.res$BUGSoutput$sims.matrix)
out.mat = as.data.frame(out)
mean(out[,"beta1"] < 0)
mean(out[,"beta2"] > 0)

par(mfrow = c(4,4))
for(i in 1:ncol(out)){
  hist(out[,i], main = paste(i))
}
quantile(out[,"beta1"], c(.025,.975))
quantile(out[,"beta2"], c(.025,.975))

par(mfrow = c(1,1))
hist(out[,"A"], main = expression(alpha), freq = FALSE)
lines(density(out[,"A"]))
hist(out[,"D"], main = expression(delta), freq = FALSE)
lines(density(out[,"D"]))
alpha.index = 3:15
delta.index = 19:30


############################################
springbok.int <- function(){
  for (n in 1:N){
    log(mu[n]) <- overall + alpha[site[n]] + beta[year[n]] + phi* (alpha[site[n]] * beta[year[n]])+ gamma*date[n] + delta*hour[n] 
    Y[n] ~ dpois(mu[n])
  }
  
  alpha[1] <- 0
  beta[1] <- 0
  
  for (i in 2:I) {
    alpha[i] ~ dnorm(alpha.mu, alpha.phi)
  }
  
  for (t in 2:T) {
    beta[t] ~ dnorm(beta.mu,  beta.phi)
  }
  
  overall ~ dnorm(0, 0.00001)
  gamma ~ dnorm(0, 0.00001)
  delta ~ dnorm(0, 0.00001)
  phi ~ dnorm(0,0.00001)
  
  alpha.mu ~ dnorm(0.0, 1.0E-6)
  alpha.sigma ~ dunif(0, 100)
  alpha.phi <-1/(alpha.sigma*alpha.sigma)
  
  beta.mu ~ dnorm(0.0, 1.0E-6)
  beta.phi <- pow(beta.sigma, -2)
  beta.sigma ~ dunif(0, 100)
}


model.file = "springbok.int.txt"
write.model(springbok.int, model.file)


Y = spring$COUNTS
site = spring$SITEI
year = spring$YEAR-1989
date = spring$DATE
hour = spring$HourFromNoon
N = dim(spring)[1]
T = 13
I = 12

data = list(I=I, N=N, T=T, Y=Y, site=site,year=year,date=date,hour=hour)  

freq = glm(spring$COUNTS~ factor(spring$YEAR) +factor(spring$SITEI) +
             spring$DATE + spring$HourFromNoon, family = poisson(link = "log"))

alpha.ini = coef(freq)[1:13]
beta.ini = c(0,coef(freq)[14:24])
delta.ini = coef(freq)[26]
gamma.ini = coef(freq)[25]


inits = function() {
  # based on ols  
  list(gamma=gamma.ini,
       delta=delta.ini,
       alpha.sigma=sd(alpha.ini),
       beta.sigma=sd(beta.ini),
       alpha.mu= mean(alpha.ini),
       beta.mu = mean(beta.ini))
}



parameters.to.save = c("overall","alpha", "beta", "alpha.sigma", "beta.sigma", "alpha.mu", "beta.mu",
                       "gamma", "delta", "phi") 


load.module("glm")  

sim = jags(data, inits=inits, parameters.to.save, model.file=springbok.int, n.chains=2, n.iter=5000)

sim = jags(data, inits=sim$BUGSoutput$last.values, parameters.to.save,
           model.file=springbokmodel, n.chains=2, n.iter=50000)

output = sim$BUGSoutput$sims.matrix
output.int = sim$BUGSoutput$sims.matrix
head(output)
output.mat = as.data.frame(output)
alpha.index = 1:12
beta.index = 15:27
hist(output.mat[,"alpha.mu"], freq = FALSE, main = expression(mu[alpha]))
lines(density(output.mat[,"alpha.mu"]))

# Interaction
out.int.mat = as.data.frame(output.int)

quantile(output[,"delta"], c(.025,.975))
hist(output[,"delta"], breaks = 60, main = "No Int. Delta",col="grey")
hist(out.int.mat[,"delta"], breaks = 60, main = "Int. Delta",col="grey")

hist(output[,"gamma"], breaks = 60, main = "No Int. Gamma",col="grey")
hist(out.int.mat[,"gamma"], breaks = 60, main = "Int. Gamma",col="grey")

quantile(output[,"gamma"], c(.025,.975))

# Other plots
alpha.post = output[,alpha.index]
beta.post = output[,beta.index]
alpha.int.post = out.int.mat[,alpha.index]
beta.int.post = out.int.mat[,beta.index]
boxplot(alpha.post)

sum.1990 = sum(spring$COUNTS[spring$YEAR == 1990])
sum(spring$COUNTS[spring$YEAR == 1991])
sum(spring$COUNTS[spring$YEAR == 1992])
sum(spring$COUNTS[spring$YEAR == 1993])
sum(spring$COUNTS[spring$YEAR == 1994])
sum(spring$COUNTS[spring$YEAR == 1995])
sum(spring$COUNTS[spring$YEAR == 1996])
sum(spring$COUNTS[spring$YEAR == 1997])
sum(spring$COUNTS[spring$YEAR == 1998])
sum(spring$COUNTS[spring$YEAR == 1999])
sum(spring$COUNTS[spring$YEAR == 2000])
sum(spring$COUNTS[spring$YEAR == 2001])
sum(spring$COUNTS[spring$YEAR == 2002])
#sum(spring$COUNTS[spring$YEAR == 1990])

##############################################
# Exploratory analysis
# Boxplot x axis = year, y = boxplots hours from noon, site 12
par(mar = c(2,2,2,2))
boxplot(HourFromNoon ~ YEAR, data = spring[spring[,"SITEI"] == 12,], ylim = c(-10,4))
legend("topright","site 12")
# Boxplot x axis = year, y = boxplots hours from noon, site 11
boxplot(HourFromNoon~ YEAR, data = spring[spring[,"SITEI"] == 11,])
legend("topright","site 11")

# Arbitrary year
boxplot(DATE ~ YEAR, data = spring[spring[,"SITEI"] == 12,])
legend("topright", "site 12")

#Posterior Summaries
boxplot(alpha.post[,c(1,5:12,2:4)], main = "No Int. Site")
boxplot(alpha.int.post[,c(1,5:12,2:4)], main = "Int Site")

#adfalsdfj
names(out.int.mat)

exp(quantile(out.int.mat[,"delta"], c(.025, .975)))
exp(quantile(out.int.mat[,"gamma"], c(.025, .975)))

quantile(out.int.mat[,"alpha.mu"] ,c(.025,.975))
quantile(output.mat[,"alpha.mu"], c(.025,.975))
quantile(out.int.mat[,"beta.mu"], c(.025,.975))
quantile(output.mat[,"beta.mu"] , c(.025,.975))
=======
                 dN = length(unique(spring$SITEI)), aN = length(unique(spring$YEAR)))

parameters = c("alpha1","alpha2","alpha3","alpha4","alpha5","alpha6","alpha7","alpha8",
               "alpha9","alpha10","alpha11","alpha12","alpha13","delta1","delta2","delta3"
               ,"delta4","delta5","delta6","delta7","delta8","delta9","delta10","delta11"
               ,"delta12","beta1","beta2","tau2","phi2")

jags.res = jags(model = model.hier, data = data.jags, n.chains = 1,
                n.iter = 10000, n.burnin = 2000, inits = NULL,
                par = parameters)

hier.bugs=  as.mcmc(jags.res$BUGSoutput$sims.matrix)

bf.bugs = as.mcmc(bf.sim$BUGSoutput$sims.matrix)  # create an MCMC object 

plot(bf.sim)
summary(bf.sim)
>>>>>>> 75a6b3eb953dc2d09a88d42aa01eda070913ff68
