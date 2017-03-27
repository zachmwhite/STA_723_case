library(R2jags)
library(R2WinBUGS)
library(glmnet)
library(vcd)
library(MatchIt)
library(Zelig)
library(leaps)
library(ATE)
library(Matching)
###########################################################
# Causal part
rhs = read.table("C:/Users/Zachary/Desktop/Winter 2017/STA 723 Case Studies/STA_723_case/HW07/rhc.txt", 
                 header = TRUE)
rhs_new = read.table("C:/Users/Zachary/Desktop/Winter 2017/STA 723 Case Studies/STA_723_case/HW07/rhc_new.txt", 
                     header = TRUE)
rhs_study =read.table("C:/Users/Zachary/Desktop/Winter 2017/STA 723 Case Studies/STA_723_case/HW07/rhc_study.txt", 
                      header = TRUE)

###########################################################
# propensity scores
## Possible models
#regsubsets(rhs[2:53],rhs$treatment,method = "adjr2", nbest = 10)

## Simple Model, no interactions
rep.index = which(names(rhs) == "dth30")
caus.simp.study = rhs[,-rep.index]
caus.simp.mod = glm(treatment~., data = caus.simp.study, family = binomial())
which(summary(caus.simp.mod)$coefficients[,4] > .05)
caus.simp.form = as.formula("treatment ~ ninsclas + cat1 + cat2 + card + neuro + hema + seps + trauma + 
                            dnr1 + surv2md1 + scoma1 + wtkilo1 + meanbp1 + resp1 + hrt1 + pafi1 +
                            paco21 + ph1 + alb1 + psychhx + transhx + wt0")
mod.mat = model.matrix(lm(treatment ~ ninsclas + cat1 + cat2 + card + neuro + hema + seps + trauma + 
                            dnr1 + surv2md1 + scoma1 + wtkilo1 + meanbp1 + resp1 + hrt1 + pafi1 +
                            paco21 + ph1 + alb1 + psychhx + transhx + wt0 - 1,data = rhs))
caus.simp = matchit(caus.simp.form, data = rhs, method = "nearest")
caus.match.simp = summary(caus.simp)
caus.match.simp.data = match.data(caus.simp)[,1:ncol(rhs)]
par(mar = c(1.8,1.8,1.3,1.3))
plot(caus.simp,type = "hist")


caus.simp.true = caus.match.simp.data[caus.match.simp.data$treatment == TRUE,]
caus.simp.false = caus.match.simp.data[caus.match.simp.data$treatment == FALSE,]
caus.simp.response = as.formula("dth30 ~ treatment + ninsclas + cat1 + cat2 + card + neuro + hema + seps + trauma + 
                            dnr1 + surv2md1 + scoma1 + wtkilo1 + meanbp1 + resp1 + hrt1 + pafi1 +
                            paco21 + ph1 + alb1 + psychhx + transhx + wt0")

###################################
# Possible stuff Model based estimate
z.out = zelig(caus.simp.response, data = caus.match.simp.data,
              model = "logit")

x.out = setx(z.out, treatment =0)
x1.out = setx(z.out, treatment = 1)
s.out = sim(z.out, x = x.out, x1 = x1.out)
summary(s.out)
# According to this and the ATE package = it is about .0732

##############################################
# AVerage treatment effect on the treated
caus.simp.form = as.formula("treatment ~ ninsclas + cat1 + cat2 + card + neuro + hema + seps + trauma + 
                            dnr1 + surv2md1 + scoma1 + wtkilo1 + meanbp1 + resp1 + hrt1 + pafi1 +
                            paco21 + ph1 + alb1 + psychhx + transhx + wt0")
caus.simp = matchit(caus.simp.form, data = rhs, method = "nearest")

z.out1 = zelig(caus.simp.response, caus.match.simp.data,
               model = "logit")
x.out1 = setx(z.out1, caus.match.simp.data[caus.match.simp.data$treatment == TRUE,])
s.out1 = sim(z.out1, x = x.out1, x1 = x.out2)
summary(s.out1)
test = s.out1$qi()
#########################################################
# Average treatment effect (Overall)
z.out2 = zelig(caus.simp.response, caus.match.simp.data, model = "logit")
x.out2 <- setx(z.out2, caus.match.simp.data[caus.match.simp.data$treatment == TRUE,])
s.out2 <- sim(z.out2, x = x.out2, )
summary(s.out2)
test = s.out1$qi
#########################################################
# I left off right here.  This won't work.
# we can't subset
ate.all <- c(s.out1$qi$att.ev, -s.out2$qi$att.ev)
mean(ate.all)
sd(ate.all)
quantile(ate.all, c(0.025, 0.975))
########################################################
## Full model, no interactions
n.full = names(rhs_study)[2:53]
f.full = as.formula(paste("rhc~", paste(n[!n %in% "y"], collapse = "+")))
matched.full = matchit(f.full, data = rhs_study, method = "nearest")
sum.match.full = summary(matched.full)
data.matched.full = match.data(matched.full)[,1:ncol(rhs_study)]

test.int = s.out1$get_qi("att.ev")

# expected values = ev
# Predicted values = pv
# fd = first diferences = Treatment effect
# It will definitely have something to do with the first differences
# I think that' what it is.  I am still not sure how to do it all.
# Look at the Matching package
# Look at the ATE package
###########################################################
# Effect size

###########################################################
# ATE package
resp.tret.index = which(colnames(rhs) == c("treatment","dth30"))
test.ate = ATE(as.numeric(rhs$dth30),as.numeric(rhs$treatment),mod.mat)

#############################################3
# Matching Package
simp.vals = c("ninsclas", "cat1", "cat2", "card","neuro", "hema", "seps" , "trauma" , 
  "dnr1" , "surv2md1" , "scoma1" , "wtkilo1" , "meanbp1" , "resp1" , "hrt1" , "pafi1" ,
  "paco21" , "ph1" , "alb1" , "psychhx" , "transhx" , "wt0")
simp.matrix = which(names(rhs) == c(simp.vals))
test.match.ate = Match(rhs$dth30, rhs$treatment,mod.mat, estimand = "ATE" )
test.match.att = Match(rhs$dth30,rhs$treatment,mod.mat,estimand = "ATT")
########################################################
# Testing out some othe results
caus.simp.form = as.formula("treatment ~ ninsclas + cat1 + cat2 + card + neuro + hema + seps + trauma + 
                            dnr1 + surv2md1 + scoma1 + wtkilo1 + meanbp1 + resp1 + hrt1 + pafi1 +
                            paco21 + ph1 + alb1 + psychhx + transhx + wt0")
caus.simp = matchit(caus.simp.form, data = rhs, method = "nearest")

z.out1 = zelig(caus.simp.response, caus.match.simp.data,
               model = "logit")
x.out1 = setx(z.out1, caus.match.simp.data[caus.match.simp.data$treatment == TRUE,])
x.out2 = setx(z.out1, caus.match.simp.data[caus.match.simp.data$treatment == FALSE,] )
s.out1 = sim(z.out1, x = x.out1, x.out2)
summary(s.out1)
test = s.out1$qi()
#########################################################
# Average treatment effect (Overall)
z.out2 = zelig(caus.simp.response, caus.match.simp.data, model = "logit")
x.out2 <- setx(z.out2, caus.match.simp.data[caus.match.simp.data$treatment == TRUE,])
s.out2 <- sim(z.out2, x = x.out2 )
summary(s.out2)
test = s.out1$get_qi()

#########################################################
# Finalized, sort of 
#######################################################
# Possible stuff Model based estimate
z.out = zelig(caus.simp.response, data = caus.match.simp.data,
              model = "logit")

x.out = setx(z.out, treatment =0)
x1.out = setx(z.out, treatment = 1)
s.out = sim(z.out, x = x.out, x1 = x1.out)
summary(s.out)
# According to this and the Matching package = it is about .0732
# Matching Package
# simp.vals = c("ninsclas", "cat1", "cat2", "card","neuro", "hema", "seps" , "trauma" , 
#              "dnr1" , "surv2md1" , "scoma1" , "wtkilo1" , "meanbp1" , "resp1" , "hrt1" , "pafi1" ,
#              "paco21" , "ph1" , "alb1" , "psychhx" , "transhx" , "wt0")
#simp.matrix = which(names(rhs) == c(simp.vals))
test.match.ate = Match(rhs$dth30, rhs$treatment,mod.mat, estimand = "ATE" )
test.match.att = Match(rhs$dth30,rhs$treatment,mod.mat,estimand = "ATT")

summary(test.match.ate)
summary(test.match.att)

#####################################

