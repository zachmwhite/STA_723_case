# HW 06
library(ggplot2)
library(R2jags)
library(R2WinBUGS)
library(MCMCPack)
library(rjags)
library(lme4)
library(gee)
library(geepack)
library(doBy)
library(gridExtra)
par(mar = c(1.8,1.8,1.3,1.3), mfrow = c(1,1))
tree = read.csv("C:/Users/Zachary/Desktop/Winter 2017/STA 723 Case Studies/STA_723_case/HW06/diamdata.csv", header = TRUE)
# Exploratory plots
unique.id.n = length(unique(tree$ID))
unique.id = sort(unique(tree$ID))

plot(tree$yr,tree$cm)
plot(tree$annualprec, tree$cm)
plot(tree$summerpdsi, tree$cm)
plot(tree$wintertemp, tree$cm)

unique(tree$wintertemp)
unique(tree$annualprec)
unique(tree$summerpdsi)
table(tree$yr,tree$wintertemp)
table(tree$yr, tree$annualprec)
table(tree$yr, tree$summerpdsi)


plot(tree$yr[which(tree$ID == unique.id[1])], tree$cm[which(tree$ID == unique.id[1])], type = "l", xlim = c(1993,2011), ylim = c(0,90), xlab = "year", ylab = "Tree cm")
for(i in 2:unique.id.n){
  lines(tree$yr[which(tree$ID == unique.id[i])], tree$cm[which(tree$ID == unique.id[i])])
}
year = ggplot(tree, aes(x = yr, y = cm, col = ID, group = factor(ID)))
year + geom_line(alpha=.3)
geom_line(aes(colour=variable),alpha=.1)

# I'll need to change the rest of these plots because they aren't entirely true.  The linearity doesn't hold up.
plot(tree$annualprec[which(tree$ID == unique.id[1])], tree$cm[which(tree$ID == unique.id[1])], xlim = c(1,2.1), ylim = c(0,90), type = "l", xlab = "annual prec", ylab = "Tree cm")
for(i in 2:unique.id.n){
  lines(tree$annualprec[which(tree$ID == unique.id[i])], tree$cm[which(tree$ID == unique.id[i])], lty = i)
}
prec = ggplot(tree,aes(x = annualprec, y = cm, color = ID))
prec + geom_point()
# Summer pdsi

summary(tree$summerpdsi)
plot(tree$summerpdsi[which(tree$ID == unique.id[1])], tree$cm[which(tree$ID == unique.id[1])], xlim = c(-2.9,.6), ylim = c(0,90), type = "l", xlab = "annual prec", ylab = "Tree cm")
for(i in 2:unique.id.n){
  lines(tree$summerpdsi[which(tree$ID == unique.id[i])], tree$cm[which(tree$ID == unique.id[i])], lty = i)
}
f = ggplot(tree, aes(x = summerpdsi, y = cm, color = ID))
f + geom_point()

plot(tree$wintertemp[which(tree$ID == unique.id[1])], tree$cm[which(tree$ID == unique.id[1])], xlim = c(2,6), ylim = c(0,90), type = "l", xlab = "annual prec", ylab = "Tree cm")
for(i in 2:unique.id.n){
  lines(tree$wintertemp[which(tree$ID == unique.id[i])], tree$cm[which(tree$ID == unique.id[i])], lty = i)
}
wintertemp = ggplot(tree, aes(x = wintertemp, y = cm, color = ID))
wintertemp + geom_point()

ggplot(tree, aes(x=yr, y = cm, color = ID, group = factor(ID))) + geom_line()

diff = rep(NA, nrow(tree))
# Differences
diff[1] = 0
for(i in 2:nrow(tree)){
  if(tree$ID[i] == tree$ID[i-1]){
    diff[i] = tree$cm[i] - tree$cm[i-1]
  }
  else{
    diff[i] = 0
  }
}
tree$diff = diff

prec = ggplot(tree,aes(x = annualprec, y = diff, color = ID,group = factor(annualprec)))
prec + geom_point()
prec + geom_boxplot()

f = ggplot(tree, aes(x = summerpdsi, y = diff, color = ID,group = factor(summerpdsi)))
f + geom_point()
f + geom_boxplot()

wintertemp = ggplot(tree, aes(x = wintertemp, y = diff, color = ID,group = factor(wintertemp)))
wintertemp + geom_point()
wintertemp + geom_boxplot()
boxplot(tree$diff ~ tree$annualprec)

year = ggplot(tree, aes(x = yr, y = diff, col = ID, group = factor(ID)))
year + geom_line(alpha=.3)
geom_line(aes(colour=variable),alpha=.1)

hist(diff, breaks = 50,main = "Difference in Diameter")


# This stuff seems weird.  Like there isn't enough variation to really model anything.
# Should I model change in cm

# Hierarchical Model
tree$unscaled.yr = tree$yr
tree$yr = tree$yr - min(tree$yr)
h.m = lmer(cm ~ yr + (1 | ID), data = tree)
h.m.prec = lmer(cm ~ yr + annualprec + (1 | ID), data = tree)
h.m.pdsi = lmer(cm ~ yr + summerpdsi + (1 | ID), data = tree)
h.m.temp = lmer(cm ~ yr + wintertemp + (1 | ID), data = tree)
h.m.all = lmer(cm ~ yr + annualprec + summerpdsi + wintertemp + (1 | ID), data = tree)
h.m.all.int = lmer(diff ~ yr + annualprec + summerpdsi + wintertemp + annualprec*wintertemp + (1 | ID), data = tree)
# Prect
ranef(h.m.all)
coef(h.m.all)
plot(h.m.all, ylab = "residuals", xlab = "fitted values")

## Posterior checks
## Model Validation
## Go on a little bit more
## Bayesian Model
hm.tot.model = function(){
  for(i in 1:N){
    
  }
  alpha[1] <- 0
  for(i in 2:aN){
    
  }
}

 
 ## the values

# GEE General Estimating Equations
tree = tree[order(tree$ID,tree$yr),]
tree$int = NA
unik <- !duplicated(tree$ID)  ## logical vector of unique values 
first.index = seq_along(tree$ID)[unik]
first.vals = tree$cm[first.index]
mult = table(tree$ID)
intercept = rep(first.vals, times = mult)
tree$int = intercept
## indices 
tree.1 = tree[-first.index,]
without.int = gee(cm ~ yr + annualprec + summerpdsi + wintertemp, data =  tree.1, id = ID,
                  corstr = "exchangeable")
ind.gee = gee(cm ~ yr + annualprec + summerpdsi + wintertemp + int, data =  tree.1, id = ID,
              corstr = "exchangeable")
ind.gee.diff = gee(diff ~ yr + annualprec + summerpdsi + wintertemp + annualprec*wintertemp + int,  
                   data = tree.1, id = ID, corstr = "exchangeable")

exc.gee = geeglm(cm~ yr + annualprec + summerpdsi + wintertemp, data = tree,
              id = ID, corstr = "exchangeable")
esticon(ind.gee,diag(5))
summary(ind.gee)$coefficients
# Robust = sandwich
# Naive = correlation matrix is specified


###############################################################################
# Predictive checks
test.preds = predict(h.m.all,newdata = tree)
tree$pred.hm = test.preds
par(mar = c(1.8,1.8,1.3,1.3), mfrow = c(1,2))
p1 = ggplot(tree, aes(x=yr, y = cm, color = ID, group = factor(ID))) + geom_line(alpha = .4)
# Hierarchical Model
p2 = ggplot(tree, aes(x=yr, y = pred.hm, color = ID, group = factor(ID))) + geom_line(alpha = .4) +
  scale_colour_gradient( low="red")
# GEE
tree.1$pred.gee = predict(ind.gee, data = tree.1)
p3 = ggplot(tree.1, aes(x = yr, y = pred.gee, color = ID, group = factor(ID))) + geom_line(alpha = .4)+
  scale_color_gradient(low = "green")
grid.arrange(p1,p2,p3)

test.preds = predict(h.m.all.int,newdata = tree)
tree$pred.hm.int = test.preds
par(mar = c(1.8,1.8,1.3,1.3), mfrow = c(1,2))
p1 = ggplot(tree, aes(x=yr, y = diff, color = ID, group = factor(ID))) + geom_line(alpha = .4)
# Hierarchical Model
p2 = ggplot(tree, aes(x=yr, y = pred.hm.int, color = ID, group = factor(ID))) + geom_line(alpha = .4) +
  scale_colour_gradient( low="red")
# GEE
tree.1$pred.gee.int = predict(ind.gee.diff, data = tree.1)
p3 = ggplot(tree.1, aes(x = yr, y = pred.gee.int, color = ID, group = factor(ID))) + geom_line(alpha = .4)+
  scale_color_gradient(low = "green")
grid.arrange(p1,p2,p3)


resid = tree.1$cm - tree.1$pred.gee


###################################################
# Good plots
## Residual plots
plot(h.m.all, ylab = "residuals", xlab = "fitted values")
h.m.resid = residuals(h.m.all)
h.m.fitted = fitted.values(h.m.all)
plot(h.m.fitted, h.m.resid, xlab = "fitted values", ylab = "residuals", col = "steelblue", main = "Hierarchical")
abline(h = 0)
plot(ind.gee$fitted.values,ind.gee$residuals, xlab = "fitted values", ylab = "residuals", col= "tomato", main = "GEE")
abline(h = 0)
## Differences
h.m.int.resid = residuals(h.m.all.int)
h.m.int.fitted = fitted.values(h.m.all.int)
plot(h.m.int.fitted, h.m.int.resid, xlab = "fitted values", ylab = "residuals", col = "steelblue", main = "Hierarchical")
abline(h = 0)
plot(ind.gee.diff$fitted.values,ind.gee.diff$residuals, xlab = "fitted values", ylab = "residuals", col= "tomato", main = "GEE")
abline(h = 0)

#Residuals for Hierarchical Model
hist(h.m.resid, breaks = 40, main = "Hierarchical")
hist(h.m.int.resid, breaks = 50, main = "Hierarchical")
# Residuals for GEE
hist(ind.gee.diff$residuals, breaks = 48, main = "GEE")

# QQ Plots
par(mfrow = c(1,1))
#fitted.q = qqnorm(tree$cm)
#fitted.q.1 = qqnorm(tree.1$cm)
# Hierarchical Model
qqnorm(h.m.resid, main = "Hierarchical")
qqline(h.m.resid)
# GEE
qqnorm(ind.gee$residuals, main = "GEE")
qqline(ind.gee$residuals)

## Differences
qqnorm(h.m.int.resid, main = "Hierarchical")
qqline(h.m.int.resid)
# GEE
qqnorm(ind.gee.diff$residuals, main = "GEE")
qqline(ind.gee.diff$residuals)
qqnorm(h.m.all)

# Heavy tails

## Predictive ones using our data
p1
p2
p3
grid.arrange(p1,p2,p3)

##############################################################
# Summaries
summary(ind.gee)
naive.se = summary(ind.gee)$coefficients[,2]
robust.se = summary(ind.gee)$coefficients[,4]
lower.naive = ind.gee$coefficients - naive.se *1.96
upper.naive = ind.gee$coefficients + naive.se *1.96
lower.robust = ind.gee$coefficients - robust.se *1.96
upper.robust = ind.gee$coefficients + robust.se *1.96
cbind(lower.naive,upper.naive,lower.robust,upper.robust)
# R2
1 - sum((tree$cm - tree$pred.hm)^2)/ sum((tree$cm - mean(tree$cm))^2)
1 - sum((tree.1$cm - tree.1$pred.gee)^2) / sum((tree.1$cm - mean(tree.1$cm))^2)
# We have probably overfit the data

summary(ind.gee.diff)
naive.se = summary(ind.gee.diff)$coefficients[,2]
robust.se = summary(ind.gee.diff)$coefficients[,4]
lower.naive = ind.gee.diff$coefficients - naive.se *1.96
upper.naive = ind.gee.diff$coefficients + naive.se *1.96
lower.robust = ind.gee.diff$coefficients - robust.se *1.96
upper.robust = ind.gee.diff$coefficients + robust.se *1.96
cbind(lower.naive,upper.naive,lower.robust,upper.robust)
confint(h.m.all.int)


data("faithful")
eruption.lm = lm(eruptions ~ waiting, data=faithful)
summary(eruption.lm)
preds = predict(eruption.lm, faithful)
1 - sum((faithful$eruptions - preds)^2) / sum((faithful$eruptions - mean(faithful$eruptions))^2)

summary(h.m.all)
confint(h.m.all)
confint(h.m.all.int)

cbind()



