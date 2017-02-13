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
data$log.var = (-(data$log.ucl - data$log.odds)/1.96)^2
meta.data = data
inv.var = 1 / data$log.var
#Change the way the weights work

#############################################
# Random Effects Model
#############################################

# Weighting method 1 = Inverse Variance weighting
weight1 = 1 / data$log.var
rma.weight1 = rma(log.odds,  log.var,measure = "OR",method = "DL", data = meta.data, weights = weight1)
summary(rma.weight1)
forest(rma.weight1)
funnel(rma.weight1)
# Weighting method 2
weight2 = quality / sum(quality)
rma.weight2.test = rma(log.odds, log.var, measure = "OR", method = "DL", data = meta.data, weights = meta.data$quality)
rma.weight2 = rma(log.odds, log.var, measure = "OR", method = "DL", data = meta.data, weights = weight2)
forest(rma.weight2)
forest(rma.weight2.test)
funnel(rma.weight2)
funnel(rma.weight2.test)
# Weighting method 3 = A combination of the two.
# Turn them into z-scores
qual.scale = (quality - min(quality))/max(quality)
var.scale = -(meta.data$log.var - max(meta.data$log.var))/max(meta.data$log.var)
comp.weight = qual.scale + var.scale
rma.weight3 = rma(log.odds,log.var, measure = "OR", method = "DL", data = meta.data, weights = comp.weight)
forest(rma.weight3)
funnel(rma.weight3)


# Comparisons
forest(rma.weight1, main = "Random Effect",slab = meta.data$author, refline = 0)

forest(rma.fixed, main = "Fixed Effect", slab = meta.data$author, refline = 0)
funnel(rma.weight1, main = "Random Effect")
funnel(rma.fixed, main = "Fixed Effect")

forest(rma.weight2, main = "Quality Score",slab = meta.data$author, refline = 0)
forest(rma.weight3, main = "Composite of Inverse Variance and Quality",slab = meta.data$author, refline = 0)
funnel(rma.weight2, main = "Quality Score")
funnel(rma.weight3, main = "Composite of Inverse Variance and Quality")
summary(rma.weight1)
.1902 + c(-1,1)*1.96*.0794
exp(.1902 + c(-1,1)*1.96*.0794)
confint(rma.weight1)
summary(rma.weight2)
.3335 + c(-1,1)*1.96*.1114
exp(.3335 + c(-1,1)*1.96*.1114)
confint(rma.weight2)
summary(rma.weight3)
.2624 + c(-1,1)*1.96*.0751
exp(.2624 + c(-1,1)*1.96*.0751)
confint(rma.weight3)

summary(rma.weight1)$estimate + c(-1,1)*1.96*summary(rma.weight1)$se

rma.model.1 = rma(log.odds,measure = "OR",var,data = meta.data, weights = )
summary(rma.model)
forest(rma.model, slab = meta.data$author, refline = 0)
funnel(rma.model)

#############################################

weighted.ave = adj.OR*quality / sum(quality)
cont = quality / sum(quality)
theta.hat = sum(adj.OR*quality) / sum(quality)
y.bar = sum(adj.OR*quality) /sum(quality)

# Q test
sum(quality*(adj.OR - weighted.ave)^2)
plot(quality,ucl - lcl)

# Odds ratio with Confidence Interval
par(mfrow = c(2,1))
plotCI(1:7,x = adj.OR,li = lcl,ui = ucl, xlab = "Odds Ratio", err = "x", ylab = "")
plotCI(1:7,x = meta.data$log.odds,li = meta.data$log.lcl,ui = meta.data$log.ucl, xlab = "Log-Odds Ratio", err = "x", ylab = "")


# Fixed Effects
weight1 = 1 / data$log.var
rma.fixed = rma(log.odds,  log.var,measure = "OR",method = "FE", data = meta.data, weights = weight1)
summary(rma.fixed)
summary(rma.weight1)
forest(rma.fixed)
funnel(rma.fixed)

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
