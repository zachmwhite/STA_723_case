# First one
ars.data = read.table("http://www2.stat.duke.edu/~st118/fye12takehome.txt", sep = " ", header = TRUE)
ars.data <- read.table("/home/ggg/Desktop/Courses/firstyearcourses/STA723/Assignment 1/data.txt")
head(ars.data)
summary(ars.data)

# visualize switch(arsenic)/(log(arsenic))
# Moving window:
data_arsenic_order <- ars.data[order(ars.data$arsenic),]
n = dim(ars.data)[1]
window_size = 400
avg_switch_window <- rep(0,n-window_size)
avg_arsenic_window <- rep(0,n-window_size)
for (i in 1:(n-window_size)) {
  avg_switch_window[i] <- mean(data_arsenic_order$switch[i:(i+window_size)])
  avg_arsenic_window[i] <- mean(data_arsenic_order$arsenic[i:(i+window_size)])
}

par(mfrow = c(1,1))
plot(avg_arsenic_window, log(avg_switch_window/(1-avg_switch_window)), col="red", 
     type="l", xlab="arsenic", ylab="")
temp <- (ars.data$switch==0)*(min(log(avg_switch_window/(1-avg_switch_window)))+(rnorm(n,0,.15))^2)+
        (ars.data$switch==1)*(max(log(avg_switch_window/(1-avg_switch_window)))-rnorm(n,0,.15)^2)
points(ars.data$arsenic, temp, pch=".")
#plot(avg_arsenic_window, avg_switch_window, col="red", type="l", ylab="p")
plot(log(avg_arsenic_window), log(avg_switch_window/(1-avg_switch_window)), col="blue", 
     type="l", xlab="log(arsenic)", ylab="")
points(log(ars.data$arsenic), temp, pch=".")
#plot(log(avg_arsenic_window), avg_switch_window, col="blue", type="l", ylab="p")


# visualize switch(distance)/(log(distance))
data_distance_order <- ars.data[order(ars.data$dist),]
window_size = 400
avg_switch_window <- rep(0,n-window_size)
avg_distance_window <- rep(0,n-window_size)
for (i in 1:(n-window_size)) {
  avg_switch_window[i] <- mean(data_distance_order$switch[i:(i+window_size)])
  avg_distance_window[i] <- mean(data_distance_order$dist[i:(i+window_size)])
}
#par(mfrow = c(1,1))
plot(avg_distance_window, log(avg_switch_window/(1-avg_switch_window)), col="red", type="l", xlab="distance", ylab="")
temp <- (ars.data$switch==0)*(min(log(avg_switch_window/(1-avg_switch_window)))+(rnorm(n,0,.15))^2)+
  (ars.data$switch==1)*(max(log(avg_switch_window/(1-avg_switch_window)))-rnorm(n,0,.15)^2)
points(ars.data$dist, temp, pch=".")

plot(log(avg_distance_window), log(avg_switch_window/(1-avg_switch_window)), col="blue", type="l", xlab="log(distance)", ylab="")
points(log(ars.data$dist), temp, pch=".")
# Consider spline, perhaps in shorter distances, terrain or other factors are more pertinent.
# Perhaps control with mode of transporatation!

ars.data$arsenic <- log(ars.data$arsenic)

hist(ars.data$educ)
# Factor education:
ars.data$educ <- 1*as.numeric(ars.data$educ>=1 & ars.data$educ<=6) + 
                  2*as.numeric(ars.data$educ>=7 & ars.data$educ<=12) + 
                  3*as.numeric(ars.data$educ>=13)
ars.data$educ <- as.factor(ars.data$educ)

sum(ars.data$switch==0 & ars.data$assoc==0)
sum(ars.data$switch==0 & ars.data$assoc==1)
sum(ars.data$switch==1 & ars.data$assoc==0)
sum(ars.data$switch==1 & ars.data$assoc==1)
# <- if anything, a negative relation

#*****************************************

library(pROC)
par(mfrow = c(1,1))

# Full logistic Model
full.log = glm(switch~. , data = ars.data, family = binomial())
summary(full.log)
full.log.roc=roc(ars.data$switch ~ predict(full.log))
full.log.roc
#anova(full.log, test = "Chisq")
#plot(full.log.roc, col="red")

#take out assoc:
full.noassoc.log = glm(switch~.-assoc , data = ars.data, family = binomial())
summary(full.noassoc.log)
full.noassoc.log.roc=roc(ars.data$switch ~ predict(full.noassoc.log))
full.noassoc.log.roc
#anova(full.log, test = "Chisq")
#plot(full.log.roc, col="red")

# Nah, just take the interaction between arsenic and distance:
interactions2.log = glm(switch ~ arsenic + dist + educ + arsenic*educ, data = ars.data, family = binomial())
summary(interactions2.log)
#anova(interactions2.log, test = "Chisq")
interactions2.log.roc=roc(ars.data$switch ~ predict(interactions2.log))
interactions2.log.roc

# Nah, just take the interaction between arsenic and distance add assoc:
interactions3.log = glm(switch ~ arsenic + dist + educ + +assoc + arsenic*educ, data = ars.data, family = binomial())
summary(interactions2.log)
#anova(interactions2.log, test = "Chisq")
interactions3.log.roc=roc(ars.data$switch ~ predict(interactions3.log))
interactions3.log.roc

# all interactions:
all.interactions.log = glm(switch ~ .*., data = ars.data, family = binomial())
summary(all.interactions.log)
all.interactions.log.roc=roc(ars.data$switch ~ predict(all.interactions.log))
all.interactions.log.roc
#plot(all.interactions.log.roc, add=TRUE, col='blue')

# null:
all.interactions.log = glm(switch ~ .*., data = ars.data, family = binomial())
summary(all.interactions.log)
all.interactions.log.roc=roc(ars.data$switch ~ predict(all.interactions.log))
all.interactions.log.roc

# not impressive, only association interactions?
interactions1.log <- glm(switch~.+assoc*., data = ars.data, family = binomial())
summary(interactions1.log)
anova(interactions1.log, test = "Chisq")
interactions1.log.roc=roc(ars.data$switch ~ predict(interactions1.log))
plot(interactions1.log.roc, add=TRUE, col='green')

# Nah, just take the interaction between arsenic and distance:
interactions2.log = glm(switch ~ arsenic + dist + educ + arsenic*educ, data = ars.data, family = binomial())
summary(interactions2.log)
anova(interactions2.log, test = "Chisq")
interactions2.log.roc=roc(ars.data$switch ~ predict(interactions2.log))
plot(interactions2.log.roc, add=TRUE, col='grey')
legend("bottomright", lty=1, col=c("red", "blue", "green", "grey"), 
       legend=c("All covariates", "All Interactions", "Association Interactions", "Final Model"),cex=0.7)
interactions2.log.coef <- interactions2.log$coefficients
exp(confint.default(interactions2.log))
interactions2.log.roc
 library(aod)
 wald.test(b = coef(interactions2.log), Sigma = vcov(interactions2.log), Terms = 1:9)

#**********************************************
# Switch(arsenic,distance)|education,association
par(mfrow = c(1,3))
cols=c("red", "blue", "green", "orange", "magenta", "black")
data_arsenic_order <- ars.data[order(ars.data$arsenic),]
dseq=seq(25,185,40)
delta=20
el=0
sc=0
hg=0
for (d in dseq) {
  arsenic_range = seq(-.7, 2.3, 0.01)
  fitted_switch <- rep(0, length(arsenic_range))
  for (i in 1:length(arsenic_range)) {
    fitted_switch[i] = 1/(1+exp(-sum(interactions2.log.coef*
                                       c(1,arsenic_range[i],d,el,sc,hg,
                                         arsenic_range[i]*el,arsenic_range[i]*sc,arsenic_range[i]*hg))))
  }
  if (d==dseq[1]) { plot(arsenic_range, fitted_switch, type="l", 
                         ylim=c(0,1), col=cols[which(dseq==d)], ylab="", xlab="log(arsenic)") }
  if (d>dseq[1]) { lines(arsenic_range, fitted_switch, type="l", col=cols[which(dseq==d)]) }
  #***********
  temp_data=ars.data[which(ars.data$dist>=d-delta & ars.data$dist<=d+delta),c(1,2)]
  temp_data_arsenic_order <- temp_data[order(temp_data$arsenic),]
  n = dim(temp_data)[1]
  window_size = 75
  if (n > window_size) {
    avg_switch_window <- rep(0,n-window_size)
    avg_arsenic_window <- rep(0,n-window_size)
    for (i in 1:(n-window_size)) {
      avg_switch_window[i] <- mean(temp_data_arsenic_order$switch[i:(i+window_size)])
      avg_arsenic_window[i] <- mean(temp_data_arsenic_order$arsenic[i:(i+window_size)])
    }
    lines(avg_arsenic_window, avg_switch_window, col=cols[which(dseq==d)], lty=2)
  }
  legend("bottomright", legend=paste("distance=",dseq,"m",sep=""), cex=0.6, col=cols[1:length(dseq)], lty=1)
}
#**********************************************
# Switch(distance,arsenic)|education,association
cols=c("red", "blue", "green", "orange", "magenta", "black")
data_dist_order <- ars.data[order(ars.data$dist),]
aseq=seq(0,2,0.5)
delta=0.25
el=0
sc=0
hg=0
for (a in aseq) {
  dist_range = seq(1,175, 1)
  fitted_switch <- rep(0, length(dist_range))
  for (i in 1:length(dist_range)) {
    fitted_switch[i] = 1/(1+exp(-sum(interactions2.log.coef*
                                       c(1,a,dist_range[i],el,sc,hg,
                                         a*el,a*sc,a*hg))))
  }
  if (a==aseq[1]) { plot(dist_range, fitted_switch, type="l", 
                         ylim=c(0,1), col=cols[which(aseq==a)], ylab="", xlab="distance") }
  if (a>aseq[1]) { lines(dist_range, fitted_switch, type="l", col=cols[which(aseq==a)]) }
  #***********
  temp_data=ars.data[which(ars.data$arsenic>=a-delta & ars.data$arsenic<=a+delta),c(1,3)]
  temp_data_dist_order <- temp_data[order(temp_data$dist),]
  n = dim(temp_data)[1]
  window_size = 75
  if (n > window_size) {
    avg_switch_window <- rep(0,n-window_size)
    avg_dist_window <- rep(0,n-window_size)
    for (i in 1:(n-window_size)) {
      avg_switch_window[i] <- mean(temp_data_dist_order$switch[i:(i+window_size)])
      avg_dist_window[i] <- mean(temp_data_dist_order$dist[i:(i+window_size)])
    }
    lines(avg_dist_window, avg_switch_window, col=cols[which(aseq==a)], lty=2)
  }
  legend("topright", legend=paste("log(arsenic)=",aseq,sep=""), cex=0.6, col=cols[1:length(aseq)], lty=1)
}

#**********************************************
# Switch(arsenic,distance)|education,association
cols=c("red", "blue", "green", "orange", "magenta", "black")
data_educ_order <- ars.data[order(ars.educ$arsenic),]
eseq=c(0,1,2,3)
el=0
sc=0
hg=0
d=50
for (e in eseq) {
  el=(e==1); sc=(e==2); hg=(e==3)
  arsenic_range = seq(-.7, 1.8, 0.01)
  fitted_switch <- rep(0, length(arsenic_range))
  for (i in 1:length(arsenic_range)) {
    fitted_switch[i] = 1/(1+exp(-sum(interactions2.log.coef*
                                       c(1,arsenic_range[i],d,el,sc,hg,
                                         arsenic_range[i]*el,arsenic_range[i]*sc,arsenic_range[i]*hg))))
  }
  if (e==eseq[1]) { plot(arsenic_range, fitted_switch, type="l", 
                         ylim=c(0.2,1), col=cols[which(eseq==e)], ylab="", xlab="log(arsenic)") }
  if (e>eseq[1]) { lines(arsenic_range, fitted_switch, type="l", col=cols[which(eseq==e)]) }
  #***********
  temp_data=ars.data[which(ars.data$educ==e),c(1,2)]
  temp_data_arsenic_order <- temp_data[order(temp_data$arsenic),]
  n = dim(temp_data)[1]
  window_size = 75
  if (n > window_size) {
    avg_switch_window <- rep(0,n-window_size)
    avg_arsenic_window <- rep(0,n-window_size)
    for (i in 1:(n-window_size)) {
      avg_switch_window[i] <- mean(temp_data_arsenic_order$switch[i:(i+window_size)])
      avg_arsenic_window[i] <- mean(temp_data_arsenic_order$arsenic[i:(i+window_size)])
    }
    lines(avg_arsenic_window, avg_switch_window, col=cols[which(eseq==e)], lty=2)
  }
  legend("bottomright", legend=c("No education","Elementary", "Secondary", "High"), cex=0.6, col=cols[1:length(eseq)], lty=1)
}
#**********************************************

#**********************************************
# 
# 
# interactions.log.coef <- interactions.log$coefficients
# a=1/(1+exp(-sum(interactions.log.coef*c(1,log(0.6),log(1+1),0,1,0,log(0.6)*log(1+1)))))
# b=1/(1+exp(-sum(interactions.log.coef*c(1,log(0.6),log(1*1.1+1),0,1,0,log(0.6)*log(1*1.1+1)))))
# b-a
# 
# interactions.prob = glm(switch ~ arsenic + dist + educ + arsenic*dist, data = ars.data, family = binomial(link="probit"))
# summary(interactions.prob)
# interactions.prob.coef <- interactions.prob$coefficients
# a=pnorm(sum(interactions.prob.coef*c(1,log(0.6),log(1+1),0,1,0,log(0.6)*log(1+1))))
# b=pnorm(sum(interactions.prob.coef*c(1,log(0.6),log(1*1.1+1),0,1,0,log(0.6)*log(1*1.1+1))))
# b-a
# 
# interactions.lpm = glm(switch ~ arsenic + dist + educ + arsenic*dist, data = ars.data)
# summary(interactions.lpm)
# interactions.lpm.coef <- interactions.lpm$coefficients
# a=(sum(interactions.prob.coef*c(1,log(0.6),log(1+1),0,1,0,log(0.6)*log(1+1))))
# b=(sum(interactions.prob.coef*c(1,log(0.6),log(1*1.1+1),0,1,0,log(0.6)*log(1*1.1+1))))
# b-a

#**********************************************
# interactions.log.coef <- interactions.log$coefficients
# 
# 
# before=1/(1+exp(-sum(interactions.log.coef*c(1,log(a),log(d+1),0,se.bef,0,log(a)*log(d+1)))))
# after=1/(1+exp(-sum(interactions.log.coef*c(1,log(a*(1+.1*inc.a)),log(d*(1+.1*inc.d)+1),0,se.aft,0,log(a*(1+.1*inc.a))*log(d*(1+.1*inc.d)+1)))))
# "logit"
# after-before
# 
# interactions.prob = glm(switch ~ arsenic + dist + educ + arsenic*dist, data = ars.data, family = binomial(link="probit"))
# #summary(interactions.prob)
# interactions.prob.coef <- interactions.prob$coefficients
# before=pnorm(sum(interactions.prob.coef*c(1,log(a),log(d+1),0,se.bef,0,log(a)*log(d+1))))
# after=pnorm(sum(interactions.prob.coef*c(1,log(a*(1+.1*inc.a)),log(d*(1+.1*inc.d)+1),0,se.aft,0,log(a*(1+.1*inc.a))*log(d*(1+.1*inc.d)+1))))
# "probit"
# after-before
# 
# interactions.lpm = glm(switch ~ arsenic + dist + educ + arsenic*dist, data = ars.data)
# #summary(interactions.lpm)
# interactions.lpm.coef <- interactions.lpm$coefficients
# before=(sum(interactions.prob.coef*c(1,log(a),log(d+1),0,se.bef,0,log(a)*log(d+1))))
# after=(sum(interactions.prob.coef*c(1,log(a*(1+.1*inc.a)),log(d*(1+.1*inc.d)+1),0,se.aft,0,log(a*(1+.1*inc.a))*log(d*(1+.1*inc.d)+1))))
# "LPM"
# after-before

# Full probit Model
full.prob = glm(switch ~ arsenic + dist + educ + arsenic*educ, data = ars.data, family = binomial(link="probit"))
summary(full.prob)

# ANOVA 
anova(full.prob, test = "Chisq")

# **************************** ENOUGH FOR TODAY!


# Add interactions?
# all interactions:
interactions0.prob = glm(switch ~ .*., data = ars.data, family = binomial(link="probit"))
summary(interactions0.prob)
anova(interactions0.prob, test = "Chisq")

# not impressive, only association interactions?
interactions1.prob <- glm(switch~.+assoc*., data = ars.data, family = binomial(link="probit"))
summary(interactions1.prob)
anova(interactions1.prob, test = "Chisq")

# Nah, just take the interaction between arsenic and distance:
interactions.prob = glm(switch ~ arsenic + dist + educ + arsenic*dist, data = ars.data, family = binomial(link="probit"))
summary(interactions.prob)
anova(interactions.prob, test = "Chisq")

#***************************************************

# Full LPM Model
full.lpm = lm(switch~arsenic + dist + educ + arsenic*dist, data = ars.data)
summary(full.lpm)

# ANOVA 
anova(full.lpm, test = "Chisq")

# Add interactions?
# all interactions:
interactions0.lpm = lm(switch ~ .*., data = ars.data)
summary(interactions0.lpm)
anova(interactions0.lpm, test = "Chisq")

# not impressive, only association interactions?
interactions1.lpm <- glm(switch~.+assoc*., data = ars.data)
summary(interactions1.lpm)
anova(interactions1.lpm, test = "Chisq")

# Nah, just take the interaction between arsenic and distance:
interactions.lpm = glm(switch ~ arsenic + dist + educ + arsenic*dist, data = ars.data)
summary(interactions.lpm)
anova(interactions.lpm, test = "Chisq")

#***************************************************


#****************************************************************

# Prediction:

# Cross validation
n = nrow(ars.data)
n.15 = floor(n*.15)
n.iter = 100
n.methods = 6
accuracy = matrix(0, nrow = n.iter, ncol = n.methods)
deviance = matrix(0, nrow = n.iter, ncol = n.methods)
colnames(deviance) = c("Baseline","Baseline_no_assoc", "Our_model", "Our_model_with_assoc", "All_interactions", "null")
colnames(accuracy) = c("Baseline","Baseline_no_assoc", "Our_model", "Our_model_with_assoc", "All_interactions", "null")
for(i in 1:n.iter){
  test.index = sample(n,n.15)
  train.data = ars.data[-test.index,]
  test.data = ars.data[test.index,]
  # Baseline model
  ## Fit the model
  full.train.lm = glm(switch~., data= train.data, family = binomial())
  ## Predict using the full model
  full.fitted = predict(full.train.lm, newdata = test.data, type="response")
  ## Classifying the predictions as switched or not switched
  full.results = ifelse(full.fitted > .5, 1, 0)
  ## Calculating the Miscalculation rate
  misclass.full = mean(full.results != test.data$switch)
  ## Converting misclassification rate to accuracy. I did it this way because I thought misclassification might be usefu
  accuracy[i,1] = 1-misclass.full
  deviance[i,1] = summary(full.train.lm)$deviance
  
  # Taking out association out of the full model
  almost.train.lm = glm(switch~arsenic+dist+educ, data = train.data, family = binomial())
  almost.fitted = predict(almost.train.lm, newdata = test.data, type="response")
  almost.results = ifelse(almost.fitted > .5,1,0)
  misclass.almost = mean(almost.results != test.data$switch)
  accuracy[i,2] = 1-misclass.almost
  deviance[i,2] = summary(almost.train.lm)$deviance
  
  # With interactions = arsenic:education, educ:dist, arsenic:dist, I didn't included the association interactions here,but we could
  interaction.lm = glm(switch~arsenic+dist+educ+arsenic:educ, data = train.data, family = binomial())
  interaction.fitted = predict(interaction.lm, newdata = test.data, type="response")
  interaction.results = ifelse(interaction.fitted > .5,1,0)
  misclass.interaction = mean(interaction.results != test.data$switch)
  accuracy[i,3] = 1 - misclass.interaction
  deviance[i,3] = summary(interaction.lm)$deviance

  # With interactions = arsenic:education, educ:dist, arsenic:dist, I didn't included the association interactions here,but we could
  interaction1.lm = glm(switch~arsenic+dist+assoc+educ+arsenic:educ, data = train.data, family = binomial())
  interaction1.fitted = predict(interaction1.lm, newdata = test.data, type="response")
  interaction1.results = ifelse(interaction1.fitted > .5,1,0)
  misclass.interaction1 = mean(interaction1.results != test.data$switch)
  accuracy[i,4] = 1 - misclass.interaction1
  deviance[i,4] = summary(interaction1.lm)$deviance
  
    
  # Without the arsenic and distance interaction because it didn't seem effective
  without.ars.dist.int = glm(switch~.*., data = train.data[,c(1,2,3,4,5)], family = binomial())
  without.fitted = predict(without.ars.dist.int, newdata = test.data[,c(1,2,3,4,5)], type="response")
  without.results = ifelse(without.fitted > .5, 1,0)
  missclass.without = mean(without.results != test.data$switch)
  accuracy[i,5] = 1 - missclass.without
  deviance[i,5] = summary(without.ars.dist.int)$deviance
  
  # Null stuff
  deviance[i,6] = summary(without.ars.dist.int)$null
  null.rate = table(train.data$switch)[2] / nrow(train.data)
  # Not sure about this next step
  accuracy[i,6] = null.rate
  
  
}

# Some analyses
apply(accuracy,2,mean)
apply(accuracy,2,sd)
apply(deviance,2,mean)
apply(deviance,2,sd)


# Deviance
summary(full.train.lm)$deviance
summary(almost.train.lm)$deviance
summary(interaction.lm)$deviance
summary(without.ars.dist.int)$deviance

# Summaries
summary(full.train.lm)
summary(almost.train.lm)
summary(interaction.lm)
summary(without.ars.dist.int)

# ANOVA analyses
anova(full.train.lm, test = "Chisq")
anova(almost.train.lm, test = "Chisq")
anova(interaction.lm, test = "Chisq")
anova(without.ars.dist.int, test = "Chisq")


# These are some more tests we could do to validate the results, 
# but they might not be necessary if we end up doing the logit, probit, and lpm

# ROC curve

# Confusion matrices
fitted.results = predict(full.train.log)
