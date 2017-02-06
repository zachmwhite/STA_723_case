# HW 03 Meta analysis
install.packages(c("meta","metafor","rmeta"))
library(meta)
library(metafor)
library(rmeta)
library(plotrix)


# Assume normality of Effect Sizes
author = c("Cantor","Zierler","Wilkins","Gottlieb","Brenniman",
           "Young","Alvanja")
year = c(1987,1988,1986,1982,1980,1981,1978)
adj.OR = c(1.19,1.60,2.20,1.18,.98,1.15,1.69)
lcl = c(1.07,1.20,.71,.95,.77,.70,1.07)
ucl = c(1.32,2.10,6.82,1.45,1.25,1.89,2.67)
method = c("logistic","MH","logisic","adj","adj","logistic","adj")
quality = c(78,71,61,49,46,45,43)

data = as.data.frame(cbind(author,year,adj.OR,lcl,ucl,method,quality))
data$year = as.numeric(data$year)
data$adj.OR = as.numeric(data$adj.OR)
data$lcl = as.numeric(data$lcl)
data$ucl = as.numeric(data$ucl)
data$quality = as.numeric(quality)
data$log.odds = log(adj.OR)
data$log.lcl = log(lcl)
data$log.ucl = log(ucl)
data$var = (-(lcl - adj.OR)/1.96)^2
meta.data = data
#############################################
# Random Effects Model
#############################################
rma.model = rma(adj.OR,var,data = meta.data)
summary(rma.model)


#############################################

weighted.ave = adj.OR*quality / sum(quality)
cont = quality / sum(quality)
theta.hat = sum(adj.OR*quality) / sum(quality)
y.bar = sum(adj.OR*quality) /sum(quality)

# Q test
sum(quality*(adj.OR - weighted.ave)^2)
plot(quality,ucl - lcl)

# Odds ratio with Confidence Interval
plotCI(1:7,y = adj.OR,li = lcl,ui = ucl, xlab = "Study Number", ylab = "Odds Raio")

# Fixed Effects

# Random Effects
cat("model {
                         # Likelihood
    for (j in 1:Nstud) {
    P[j] <- 1/V[j]      # Calculate precision
    Y[j] ~ dnorm(delta[j],P[j])
    delta[j] ~ dnorm(d,prec)
    }
    
    # Priors
    d ~ dnorm(0,1.0E-6)
    prec <- 1/tau.sq
    tau.sq <- tau*tau   # tau.sq = between-study variance
    tau ~ dunif(0,10)   # Uniform on SD
    OR<-exp(d)          #exponentiate to get back to OR scale
    prob.OR1<-step(d)   #calc prob of OR > 1
    }", file="betaMeta1.txt")

blocker<-read.csv("...path to your data.../BLOCKER.csv", sep=",", header=T)
Nstud<-22

blockerDat <- list(Y=blocker$Y,V=blocker$V,Nstud=Nstud)
blockerParams <- c("d","tau","OR", "prob.OR1")
blockerInits<-function(){
  list("delta"=c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),"d"=c(0),
       "tau"=c(1))
}
set.seed(100)
library(R2jags)

jag.blocker1<-jags(data=blockerDat, 
                   inits=blockerInits, 
                   parameters.to.save=blockerParams, n.iter=1000, n.thin=20,
                   model.file="betaMeta1.txt")

jag.blocker1

# Meta Regression
