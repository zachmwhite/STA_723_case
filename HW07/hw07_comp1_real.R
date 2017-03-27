library(R2jags)
library(R2WinBUGS)
library(glmnet)
library(vcd)
library(MatchIt)
library(Zelig)
library(leaps)
library(ATE)
library(Matching)
library(ggplot2)
library(BayesTree)
library(randomForest)
###########################################################
# Causal part
rhs = read.table("C:/Users/Zachary/Desktop/Winter 2017/STA 723 Case Studies/STA_723_case/HW07/rhc.txt", 
                 header = TRUE)
rhs_new = read.table("C:/Users/Zachary/Desktop/Winter 2017/STA 723 Case Studies/STA_723_case/HW07/rhc_new.txt", 
                     header = TRUE)
rhs_study.orig =read.table("C:/Users/Zachary/Desktop/Winter 2017/STA 723 Case Studies/STA_723_case/HW07/rhc_study.txt", 
                      header = TRUE)

rhs_study$surv30.logit = log(rhs_study$surv30 / (1 - rhs_study$surv30) )
par(mar = c(1.8,1.8,1.3,1.3))
#HIstogram of logit(response)
hist(rhs_study$surv30.logit, breaks = 50)
# Outlier detection
summary(rhs_study$surv30.logit)
outlier.index = which.max(rhs_study$surv30.logit)
rhs_study = rhs_study[-outlier.index,]
# Finalized Histogram
hist(rhs_study$surv30.logit, breaks = 50, main = "Hist. logit(surv30)", freq = FALSE)
lines(density(rhs_study$surv30.logit), col = "red")

test.gg = ggplot(rhs_study, aes(x = rhc, y = surv30.logit))
test.gg + geom_boxplot() 
boxplot(rhs_study$surv30.logit~rhs_study$rhc, main = "logit(surv30) by RHC")

dens.prob = ggplot(rhs_study.orig, aes(x = surv30))
#dens.prob = 
  
table(rhs$treatment,rhs$dth30)


density.gg = ggplot(rhs_study, aes(x = surv30.logit,color = as.factor(rhc)))
density.gg + geom_density() +  guides(color=FALSE)

density.full = ggplot(rhs_study, aes(x= surv30.logit))
density.full + geom_density()
#####################################################################
# Basic OLS
## Calculate MSE
surv30.index = which(colnames(rhs_study) == "surv30")
rhs_study = rhs_study[,-surv30.index]
full.ols = lm(surv30.logit~., data = rhs_study)
mse.full.ols = mean((full.ols$fitted.values - rhs_study$surv30.logit)^2)

# OLS with interaction terms
## Calculate MSE
full.int.ols = lm(surv30.logit ~ . + .:rhc, data = rhs_study)
plot(full.int.ols)
mse.full.int = mean((full.int.ols$fitted.values - rhs_study$surv30.logit)^2)

# Random Forest
## Calculate MSE
rand.forest.full.20 = randomForest(surv30.logit ~ ., data = rhs_study,
                                ntree = 20)
mse.forest.20 = mean((rand.forest.full.20$predicted - rhs_study$surv30.logit)^2)

rand.forest.full.100 = randomForest(surv30.logit ~ ., data = rhs_study,
                                   ntree = 100)
mse.forest.100 = mean((rand.forest.full.100$predicted - rhs_study$surv30.logit)^2)

rand.forest.full.500 = randomForest(surv30.logit ~ ., data = rhs_study,
                                    ntree = 500)
mse.forest.500 = mean((rand.forest.full.500$predicted - rhs_study$surv30.logit)^2)

rand.forest.full.1000 = randomForest(surv30.logit ~ ., data = rhs_study,
                                    ntree = 1000)
mse.forest.1000 = mean((rand.forest.full.1000$predicted - rhs_study$surv30.logit)^2)

########################################################
# RAndom Forest with interaction
rand.forest.int.20 = randomForest(surv30.logit ~ . + .:rhc, data = rhs_study,
                                  ntree = 20)
mse.forest.int.20 = mean((rand.forest.full.20$predicted - rhs_study$surv30.logit)^2)

rand.forest.int.100 = randomForest(surv30.logit ~ .+ .:rhc, data = rhs_study,
                                   ntree = 100)
mse.forest.int.100 = mean((rand.forest.full.100$predicted - rhs_study$surv30.logit)^2)

rand.forest.int.500 = randomForest(surv30.logit ~ .+ .:rhc, data = rhs_study,
                                   ntree = 500)
mse.forest.int.500 = mean((rand.forest.full.500$predicted - rhs_study$surv30.logit)^2)

rand.forest.int.1000 = randomForest(surv30.logit ~ .+ .:rhc, data = rhs_study,
                                    ntree = 1000)
mse.forest.int.1000 = mean((rand.forest.full.1000$predicted - rhs_study$surv30.logit)^2)

# BART or something
## Calculate MSE
bart.20 = bart(rhs_study[,1:(ncol(rhs_study)-1)],rhs_study$surv30.logit,ntree = 20)
pred.bart.20 = bart.20$yhat.train
mse.pred.20 = rep(0,1000)
for(i in 1:1000){
  mse.pred.20[i] = mean((pred.bart.20[i,] - rhs_study$surv30.logit)^2)
}
hist(mse.pred.20, breaks = 50)
mse.bart.20 = mean(mse.pred.20)

bart.50 = bart(rhs_study[,1:(ncol(rhs_study)-1)],rhs_study$surv30.logit,ntree = 50)
pred.bart.50 = bart.50$yhat.train
mse.pred.50 = rep(0,1000)
for(i in 1:1000){
  mse.pred.50[i] = mean((pred.bart.50[i,] - rhs_study$surv30.logit)^2)
}
hist(mse.pred.50, breaks = 50)
mse.bart.50 = mean(mse.pred.50)

bart.100 = bart(rhs_study[,1:(ncol(rhs_study)-1)],rhs_study$surv30.logit,ntree = 100)
pred.bart.100 = bart.100$yhat.train
mse.pred.100 = rep(0,1000)
for(i in 1:1000){
  mse.pred.100[i] = mean((pred.bart.100[i,] - rhs_study$surv30.logit)^2)
}
hist(mse.pred.100, breaks = 50)
mse.bart.100 = mean(mse.pred.100)
# 250 trees
bart.250 = bart(rhs_study[,1:(ncol(rhs_study)-1)],rhs_study$surv30.logit,ntree = 250)
pred.bart.250 = bart.250$yhat.train
mse.pred.250 = rep(0,1000)
for(i in 1:1000){
  mse.pred.250[i] = mean((pred.bart.250[i,] - rhs_study$surv30.logit)^2)
}
hist(mse.pred.250, breaks = 50)
mse.bart.250 = mean(mse.pred.250)

bart.500 = bart(rhs_study[,1:(ncol(rhs_study)-1)],rhs_study$surv30.logit,ntree = 500)
pred.bart.500 = bart.500$yhat.train
mse.pred.500 = rep(0,1000)
for(i in 1:1000){
  mse.pred.500[i] = mean((pred.bart.500[i,] - rhs_study$surv30.logit)^2)
}
hist(mse.pred.500, breaks = 50)
mse.bart.500 = mean(mse.pred.500)

bart.1000 = bart(rhs_study[,1:(ncol(rhs_study)-1)],rhs_study$surv30.logit,ntree = 1000)
pred.bart.1000 = bart.1000$yhat.train
mse.pred.1000 = rep(0,1000)
for(i in 1:1000){
  mse.pred.1000[i] = mean((pred.bart.1000[i,] - rhs_study$surv30.logit)^2)
}
hist(mse.pred.1000, breaks = 50)
mse.bart.1000 = mean(mse.pred.1000)

# Whichever model is best use that to do the prediction.  How to find PMSE?
rhs_new_true = cbind(rep(TRUE,nrow(rhs_new)),rhs_new[,-1])
colnames(rhs_new_true)[1] = "rhc"
rhs_new_false = cbind(rep(FALSE,nrow(rhs_new)),rhs_new[,-1])
colnames(rhs_new_false)[1] = "rhc"
# No interaction term

# Interaction term.  Lowest MSE.  Predictions scores
true.preds.int.ols = predict(full.int.ols,rhs_new_true)
false.preds.int.ols = predict(full.int.ols,rhs_new_false)
diff.preds = true.preds.int.ols - false.preds.int.ols
hist(diff.preds, breaks = 50)
abline(v = 0, col = "red")
mean(diff.preds)

pos.indices = which(diff.preds > 0)
rhs_new_positive = rhs_new[pos.indices,]
sum.int.ols = summary(full.int.ols)
sum.int.ols$coefficients
which(sum.int.ols$coefficients[,1] > 0 & sum.int.ols$coefficients[,4] < 0.05)
# Based on this one.
# We should explore the ones with other things?
# I need to figure this out.  I a list of significant positive variables in the model.
# However, I haven't really looked at those values with differences.

# Possible signifcant ones
# Male, Education, cat1CHF, cat1Colon Cancer, cat1Lung Cancer

# cat1MOSF w/ Sepsis, cat2MOSF w/ Sepsis, respYes, traumaYes, meanbp1
# wblc1, hema1, sod1, psychhx, renalhx, liverhx, immunhx, transhx
# She does want some Bayesian stuff for prediction.  Find the individual one with 0 in the confidence interval

mse.forest.100
mse.forest.1000
mse.bart.100
mse.bart.20
mse.bart.1000
mse.full.ols
mse.full.int

###############################
mean(rhs_new_positive$sex == "Male")
mean(rhs_new$sex == "Male")

mean(rhs_new_positive$cat1 == "CHF")
mean(rhs_new$cat1 == "CHF")

mean(rhs_new_positive$cat1 == "COPD")
mean(rhs_new$cat1 == "COPD")

mean(rhs_new_positive$cat1 == "Lung Cancer")
mean(rhs_new$cat1 == "Lung Cancer")

mean(rhs_new_positive$cat1 == "MOSF w/Sepsis")
mean(rhs_new$cat1 == "MOSF w/Sepsis")

mean(rhs_new_positive$resp == "Yes")
mean(rhs_new$resp == "Yes")

mean(rhs_new_positive$trauma == "Yes")
mean(rhs_new$trauma == "Yes")

mean(rhs_new_positive$psychhx == TRUE)
mean(rhs_new$psychhx == TRUE)

mean(rhs_new_positive$renalhx == TRUE)
mean(rhs_new$renalhx == TRUE)

mean(rhs_new_positive$liverhx == TRUE)
mean(rhs_new$liverhx == TRUE)

mean(rhs_new_positive$immunhx == TRUE)
mean(rhs_new$immunhx == TRUE)

mean(rhs_new_positive$transhx == TRUE)
mean(rhs_new$transhx == TRUE)

mean(rhs_new_positive$cat1 == "Colon Cancer")
mean(rhs_new$cat1 == "Colon Cancer")

# Resp, CHF, Male, immunhx, transhx