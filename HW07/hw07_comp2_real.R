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
library(ggplot2)
library(BayesTree)
library(doBy)
###########################################################
# Causal part
rhs = read.table("C:/Users/Zachary/Desktop/Winter 2017/STA 723 Case Studies/STA_723_case/HW07/rhc.txt", 
                 header = TRUE)
rhs_new = read.table("C:/Users/Zachary/Desktop/Winter 2017/STA 723 Case Studies/STA_723_case/HW07/rhc_new.txt", 
                     header = TRUE)
rhs_study =read.table("C:/Users/Zachary/Desktop/Winter 2017/STA 723 Case Studies/STA_723_case/HW07/rhc_study.txt", 
                      header = TRUE)
# Flip the dth30 variable.  I believe
rhs$dth30 = !rhs$dth30

# All the EDA plots are the same basically
table(rhs$treatment,rhs$dth30)

# Matching
mod.mat = model.matrix(glm(dth30~ . + .:treatment, data = rhs, family = binomial()))
test.match.att = Match(rhs$dth30,rhs$treatment,prop.scores + rhs$treatment,estimand = "ATT")
test.match.att$est

mod.mat1 = model.matrix(glm(dth30~ ., data = rhs, family = binomial()))
test.match.att1 = Match(rhs$dth30,rhs$treatment,mod.mat1,estimand = "ATT")
test.match.att1$est

#exp(test.match.att$est) / (exp(test.match.att$est) + 1 )
# Propensity Score
## I should use all the variables.

prop.model = glm(treatment~ ., rhs,family = binomial())
prop.scores = prop.model$fitted.values
prop.score.treat = prop.scores[rhs$treatment == TRUE]
prop.score.cont = prop.scores[rhs$treatment == FALSE]
quantile.points = c(0,quantile(prop.scores, seq(.1,.9,by= .1)),1)

prop.plot.frame = as.data.frame(cbind(prop.scores,as.logical(rhs$treatment)))
colnames(prop.plot.frame)[2] = "treatment"
prop.plot = ggplot(prop.plot.frame, aes(x = prop.scores, color = treatment))
prop.plot + geom_density()
par(mar = c(1.8,1.8,1.3,1.3))
plot(density(prop.score.cont), xlim = c(0,1), col = "blue", main = "Propensity score")
lines(density(prop.score.treat), col = "red")
lines(density(prop.scores), col = "grey")
abline(v = quantile.points)
# Test the balance
# Bins for propensity scores
bins.treat = findInterval(prop.score.treat, seq(0,1, by = .1))
bins.cont = findInterval(prop.score.cont, seq(0,1,by = .1))

unique(which(prop.scores > .95))
unique(which(prop.scores < .05))
## t-test
## ANOVA
## A summary of th table? Make note of unbalanced ones.
mb = MatchBalance(treat~., data = rhs, mathout. =)
inverse.weight = NA
inverse.weight[rhs$treatment ==TRUE] = 1 / prop.scores[rhs$treatment == TRUE]
inverse.weight[rhs$treatment ==FALSE] = 1 / (1 - prop.scores[rhs$treatment == FALSE])

rhs$prop.scores = prop.scores
#weighted.prop = prop.scores * inverse.weight
#par(mar = c(1.8,1.8,1.3,1.3))
#plot(density(weighted.prop), xlim = c(0,1), col = "blue", main = "Propensity score")
#lines(density(weighted.prop[rhs$treatment == TRUE]), col = "red")
#lines(density(weighted.prop[rhs$treatment == FALSE]), col = "grey")
test.glm = glm(-dth30 ~ prop.scores + treatment, data = rhs, weights = inverse.weight)
summary(test.glm)
exp(-3.16) / (1 + exp(-3.16))
# Average treatment effect


# Balance Quantiles

# Matching package verification

# Bootstrap the variance

# We are interested in ATT

###########################################################3
###
### Example 1: calculating the average treatment effect for the treated
###

library(Zelig)

## propensity score matching
rhs.matchit = rhs[,-c(55,56)]
f.full = as.formula(paste("treatment~", paste(n[!n %in% "y"], collapse = "+")))
m.out1 <- matchit(f.full , data = rhs.matchit, method = "nearest")

n1 = names(rhs.matchit)
f.full1 = as.formula(paste("dth30~", paste(n1[!n1 %in% "dth30"], collapse = "+")))
z.out1 = zelig(f.full1, data = match.data(m.out1), model = "logit")
x.out1 = setx(z.out1, data = match.data(m.out1, "treatment"))
s.out1 <- sim(z.out1, x = x.out1)

x.out1 = setx(z.out1, data = match.data(m.out1, "treatment"))


## fit the linear model to the control group controlling for propensity score and 
## other covariates
z.out1 <- zelig(re78 ~ age + educ + black + hispan + nodegree + married + re74 + re75 +
                  distance, data = match.data(m.out1, "control"), model = "ls")
user.prompt()

## set the covariates to the covariates of matched treated units
## use conditional prediction by setting cond = TRUE.
x.out1 <- setx(z.out1, data = match.data(m.out1, "treat"), fn = NULL, cond = TRUE)
user.prompt()

## simulate quantities of interest
s.out1 <- sim(z.out1, x = x.out1)
user.prompt()

#####################################################3
# New package
rhs.new = rhs[,-c(55,56)]
ps.rhs = ps(treatment~. - dth30, data = rhs)

##################################################################################
# Testing things out
# I think I've got this stuff down at this point
rhs = rhs[,1:54]
prop.model = glm(treatment~ ., data = rhs, family = binomial())
ps.values = prop.model$fitted.values
rhs$ps.values = NA
rhs$ps.values[as.numeric(names(ps.values))] = ps.values

# We will use SMR weights
smrw = NA
smrw[rhs$treatment == "TRUE"] = 1
smrw[rhs$treatment == "FALSE"] = (ps.values[rhs$treatment == FALSE])/ (1 - ps.values[rhs$treatment == FALSE])
rhs$smrw = smrw

summaryBy(smrw ~ treatment,data = rhs, FUN = sum)

weighted.stuff = smrw * ps.values
plot(density(weighted.stuff), xlim = )
sort(weighted.stuff, decreasing = TRUE)[1:10]


ggplot(data = rhs, aes(x = ps.values, color = factor(treatment), fill = factor(treatment), weight = smrw))+
  geom_density(alpha = .2) +  guides(color=FALSE, fill = FALSE)
ggplot(data = rhs, aes(x = ps.values, color = factor(treatment), fill = factor(treatment))) +
  geom_density(alpha = .2) + guides(color = FALSE, fill = FALSE)


logit.smrw = glm(dth30 ~ treatment,
                 data = rhs,
                 family = quasibinomial(),
                 weights = smrw)
exp(coef(logit.smrw))
summary(logit.smrw)
