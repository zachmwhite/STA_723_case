# Component 1
library(R2jags)
library(R2WinBUGS)
library(glmnet)
library(vcd)
library(MatchIt)
library(Zelig)
library(MASS)
###########################################################
# Prediction part
rhs = read.table("C:/Users/Zachary/Desktop/Winter 2017/STA 723 Case Studies/STA_723_case/HW07/rhc.txt", 
                 header = TRUE)
rhs_new = read.table("C:/Users/Zachary/Desktop/Winter 2017/STA 723 Case Studies/STA_723_case/HW07/rhc_new.txt", 
                     header = TRUE)
rhs_study =read.table("C:/Users/Zachary/Desktop/Winter 2017/STA 723 Case Studies/STA_723_case/HW07/rhc_study.txt", 
                      header = TRUE)

###########################################################
# Exploratory
#full.model = glm(dth30~., data = rhs, family = binomial(link = "logit"))
full.model = lm(surv30 ~ . , data = rhs_study)
full.matrix = model.matrix(full.model)
summary(full.model)
# Not significant variables
which(summary(full.model)$coefficients[,4] > .05)
# wt0TRUE, malighx, gibledhx, liverhx, chrpulhx, chfhx,cardiohx, crea1, pot1, sod1, hema1, wblc1, ph1
# resp1, meanbp1, wtkilo1, traumaYes, sepsYes, renalYes, cardYes
par(mfrow = c(2,2))
plot(full.model)
dim(full.matrix)

# Simplified model
form.simp = as.formula("surv30~rhc+age+sex+income+ninsclas+cat1+cat2+resp+
                       neuro + gastr + meta + hema + ortho + das2d3pc + dnr1 + 
                       ca + surv2md1 + aps1 + scoma1 + temp1 + resp1 + hrt1 + pafi1 + 
                       paco21 + bili1 + alb1 + dementhx + psychhx + renalhx + immunhx +
                       transhx + amihx")
simp.model = lm(form.simp, data = rhs_study)
summary(simp.model)
plot(simp.model)
# Both of these models show that rhc is significant AND negative.
full.rhc = summary(full.model)$coefficients["rhcTRUE",]
simp.rhc = summary(simp.model)$coefficients["rhcTRUE",]
# They are actually pretty comparable in this case
##################################################################################
tab = table(rhs$dth30,rhs$treatment)
tab
mosaic(tab)

# Exploratory Data Analysis
# Exploratory plots
par(mar = c(1.8,1.8,1.3,1.3),mfrow = c(4,5))
for(i in 1:53){
  if(is.logical(rhs_study[,i]) == TRUE){
    boxplot(rhs_study[,54]~rhs_study[,i],main = paste(colnames(rhs_study)[i]))
  }
  else{
  plot(rhs_study[,i],rhs_study[,54], main = paste(colnames(rhs_study)[i]))
  }
}

pred.set = rhs_new[,-54]
preds.new = predict(full.model,pred.set)
# For prediction = Just find the model matrix or something

#####################################################
# Exploratory propensity scores
# 
rep.index = which(names(rhs_study) == "surv30")
prop.study = rhs_study[,-rep.index]
prop.model = glm(rhc~., data = prop.study, family = binomial())
which(summary(prop.model)$coefficients[,4] > .05)
prop.simp.form = as.formula("rhc ~ ninsclas + cat1 + cat2 + card + neuro + hema + seps + trauma + 
                       dnr1 + surv2md1 + scoma1 + wtkilo1 + meanbp1 + resp1 + hrt1 + pafi1 +
                       paco21 + phi1 + alb1 + psychhx + transhx + wt0")
# Propensity score
#rep.index = which(names(rhs_study) == "surv30")
#prop.study = rhs_study[,-rep.index]
#n <- names(prop.study)[2:53]
#f <- as.formula(paste("rhc ~", paste(n[!n %in% "y"], collapse = " + ")))
#matched = matchit(f, data = prop.study,method = "nearest")
#sum.match = summary(matched)
## This function uses som neural network.

## Matching data
#data.matched = match.data(sum.match)[1:ncol(prop.study)]
#sum.match$nn

## Complete data, no interactions
n.full = names(rhs_study)[2:53]
f.full = as.formula(paste("rhc~", paste(n[!n %in% "y"], collapse = "+")))
matched.full = matchit(f.full, data = rhs_study, method = "nearest")
sum.match.full = summary(matched.full)
data.matched.full = match.data(matched.full)[,1:ncol(rhs_study)]

rhc.true = data.matched.full[data.matched.full$rhc == TRUE,]
rhc.false = data.matched.full[data.matched.full$rhc == FALSE,]
mean(rhc.true$surv30)
mean(rhc.false$surv30)
t.test(rhc.true$surv30,rhc.false$surv30)

full.matched.model = lm(surv30 ~ . , data = rbind(rhc.true,rhc.false))
matched.rhc.full = summary(full.matched.model)$coefficients["rhcTRUE",]
## t-test is significant when we do it like this.  
# Seems like rhc might even be associated with lower prognosis score

#################################
## Simple, no interactions
rep.index = which(names(rhs_study) == "surv30")
prop.study = rhs_study[,-rep.index]
prop.model = glm(rhc~., data = prop.study, family = binomial())
which(summary(prop.model)$coefficients[,4] > .05)
prop.simp.form = as.formula("rhc ~ ninsclas + cat1 + cat2 + card + neuro + hema + seps + trauma + 
                       dnr1 + surv2md1 + scoma1 + wtkilo1 + meanbp1 + resp1 + hrt1 + pafi1 +
                       paco21 + ph1 + alb1 + psychhx + transhx + wt0")
matched.simp = matchit(prop.simp.form,rhs_study, method = "nearest")

prop.glm = glm(prop.simp.form,data = rhs_study, family = binomial())
### Actual Results

data.matched.simp = match.data(matched.simp)[,1:ncol(rhs_study)]

rhc.simp.true = data.matched.simp[data.matched.simp$rhc == TRUE,]
rhc.simp.false = data.matched.simp[data.matched.simp$rhc == FALSE,]



t.test(rhc.true$surv30,rhc.false$surv30)
rhc.simp.model = lm(surv30 ~ . , data = rbind(rhc.simp.true,rhc.simp.false))
matched.rhc.simp = summary(rhc.simp.model)$coefficients["rhcTRUE",]
plot(rhc.simp.model)

## Maybe try some different matching methodologies
### Full
matched.simp.ful = matchit(prop.simp.form, data = rhs_study, method = "full")
plot(matched.simp.ful, type = "hist")
full.match.data = match.data(matched.simp.ful)[,1:ncol(rhs_study)]
matched.simp.opt = matchit(prop.simp.form, data = rhs_study, method = "optimal")



matched.rhc.full
matched.rhc.simp
full.rhc
simp.rhc
plot(rhc.simp.model)
plot(full.matched.model)
plot(full.model)
plot(simp.model)

# I think I probably should calculate BIC.  Do it.
###################################################
# Best subset for the propensity scores
n.full = names(rhs)[2:53]
f.full = as.formula(paste("treatment~", paste(n[!n %in% "y"], collapse = "+")))
leaps.caus = regsubsets(f.full,data=rhs,nbest=10, method = "backward")
str(leaps.caus)
str(summary(leaps.caus))
bic.min.index = which.min(summary(leaps.caus)$bic)
which(summary(leaps.caus)$which[bic.min.index,] == TRUE)

# Best subset for the actual model
leaps.mod = re
#########################################################
# And then should I cross it back with the new dataset.  Predict the prognosis scores?
prob.prog = predict(prop.glm,rhs_new, "response")
pred.probs.num = ifelse(prob.prog > .5,1,0)
other.preds.num = 1-pred.probs
pred.probs = as.logical(pred.probs.num)
other.preds = as.logical(other.preds.num)
pred.set = cbind(pred.probs,rhs_new[,-1])
pred.set2 = cbind(other.preds,rhs_new[,-1])
colnames(pred.set)[1] = "rhc"
colnames(pred.set2)[1] = "rhc"
test.simp.model = lm(form.simp, data = data.matched.simp)
# Preprocessing
colon.index = which(rhs_new$cat1 == "Colon Cancer")
pred.set = pred.set[-colon.index,]
pred.set2 = pred.set2[-colon.index,]
pred.prog1 = predict(test.simp.model,pred.set)
pred.prog2 = predict(test.simp.model, pred.set2)
mean(pred.prog1[pred.set$rhc == TRUE] - pred.prog2[pred.set2$rhc == FALSE])
mean(pred.prog1[pred.set$rhc == FALSE] - pred.prog2[pred.set2$rhc == TRUE])

rbinom(length(pred.prog),1,pred.prog)

###########################################################################
# Figure out what to do with the second part
###########################################################################
# I need to figure out how to incorporate the second part
# Find a way to simulate new values based on the matches
# Do matching with the unobserved
# Find propensity scores.
# If that is the only thing we're changing then we're not getting the right thing
# We need to account for some bias as well

