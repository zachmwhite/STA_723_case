# HW04
library(xlsx)
library(R2jags)
library(rjags)

spring = read.xlsx2("~/Winter 2017/STA 723 Case Studies/STA_723_case/HW04/springbok.xls",sheetIndex = 1)

spring$HourFromNoon = round(as.numeric(spring$HourFromNoon), 3)
spring$COUNTS = as.numeric(spring$COUNTS)
spring$DATE = as.numeric(spring$DATE)
spring$YEAR = as.numeric(spring$YEAR)
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

##############################################
# JAGS
#############################################

model.hier = function(){
  for(i in 1:n){
    Y[i] ~ dpois(mu[i])
    log(mu[i]) <- alpha[year[i]] + delta[siteid[i]] + beta0 + beta1 * date[i] + beta2 * hour[i]
  }
  for(j in 1:aN){
    alpha[j] ~ dnorm(alpha, inv.tau2) #YEAR
  }
  for(k in 1:dN){
    delta[k] ~ dnorm(delta, inv.phi2) #SITEID
  }
  # Priors for Year
  alpha ~ dnorm(0,.0001)
  inv.tau2 ~ dgamma(0.001,.001)
  tau2 <- 1 / inv.tau2
  
  # Priors for SITEID
  delta ~ dnorm(0,.0001)
  inv.phi2 ~ dgamma(.001,.001)
  phi2 <- 1 / inv.phi2
  
  # Intercepts and other Betas
  beta0 ~ dnorm(0,.0001)
  beta1 ~ dnorm(0,.0001)
  beta2 ~ dnorm(0,.0001)
  
}

data.jags = list(Y = spring$COUNTS, n = nrow(spring), year = spring$YEAR,
                 siteid = spring$SITEI, date = spring$DATE, hour = spring$HourFromNoon,
                 aN = length(unique(spring$SITEI)), dN = length(unique(spring$YEAR)))

parameters = c("alpha1","alpha2","alpha3","alpha4","alpha5","alpha6","alpha7","alpha8",
               "alpha9","alpha10","alpha11","alpha12","alpha13","delta1","delta2","delta3"
               ,"delta4","delta5","delta6","delta7","delta8","delta9","delta10","delta11"
               ,"delta12","beta0","beta1","beta2","tau2","phi2")

jags.res = jags(model = model.hier, data = data.jags, n.chains = 1,
                n.iter = 10000, n.burnin = 2000, inits = NULL,
                par = parameters)
