# First one
ars.data = read.table("http://www2.stat.duke.edu/~st118/fye12takehome.txt", sep = " ", header = TRUE)
ars.data <- read.table("/home/ggg/Desktop/Courses/firstyearcourses/STA723/Assignment 1/data.txt")
head(ars.data)

# Plots of the predictors.  It is clear that many of them aren't normal, and that might be changing our results.
par(mfrow = c(1,2))
hist(ars.data$arsenic, freq = FALSE, ylim = c(0,.6), main = "Arsenic", xlab="Arsenic")
lines(density(ars.data$arsenic))
hist(ars.data$dist, freq = FALSE, ylim = c(0,.02), main = "Distance", xlab="Distance")
lines(density(ars.data$dist))
par(mfrow = c(1,1))
hist(ars.data$educ, freq = FALSE, main="",xlab = "Education", breaks=18)
lines(density(ars.data$educ))

# Some possible transformations
ars.data$arsenic <- log(ars.data$arsenic)
ars.data$dist <- log(ars.data$dist + 1)

par(mfrow = c(1,2))
hist(ars.data$arsenic, freq = FALSE, main = "log(Arsenic)", xlab="log(Arsenic)")
lines(density(ars.data$arsenic))
hist(ars.data$dist, freq = FALSE, main = "log(Distance + 1)", xlab="log(Distance + 1)")
lines(density(ars.data$dist))

# Factor education:
ars.data$educ <- 1*as.numeric(ars.data$educ>=1 & ars.data$educ<=6) + 
                  2*as.numeric(ars.data$educ>=7 & ars.data$educ<=12) + 
                  3*as.numeric(ars.data$educ>=13)
ars.data$educ <- as.factor(ars.data$educ)

#*****************************************

# Full logistic Model
full.log = glm(switch~., data = ars.data, family = binomial())
summary(full.log)

# ANOVA 
anova(full.log, test = "Chisq")
str(full.log)

# Add interactions?
# all interactions:
interactions0.log = glm(switch ~ .*., data = ars.data, family = binomial())
summary(interactions0.log)
anova(interactions0.log, test = "Chisq")

# not impressive, only association interactions?
interactions1.log <- glm(switch~.+assoc*., data = ars.data, family = binomial())
summary(interactions1.log)
anova(interactions1.log, test = "Chisq")

# Nah, just take the interaction between arsenic and distance:
interactions.log = glm(switch ~ arsenic + dist + educ + arsenic*dist, data = ars.data, family = binomial())
summary(interactions.log)
anova(interactions.log, test = "Chisq")

#**********************************************

# Full probit Model
full.prob = glm(switch~arsenic + dist + educ + arsenic*dist, data = ars.data, family = binomial(link="probit"))
summary(full.prob)

# ANOVA 
anova(full.prob, test = "Chisq")

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

# Officially add the interaction term to the dataset:
ars.data$arsenic.dist <- ars.data$arsenic * ars.data$dist

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
  full.train.lm = glm(switch~arsenic+dist+assoc+educ, data= train.data, family = binomial())
  ## Predict using the full model
  full.fitted = predict(full.train.lm, newdata = test.data[,c(1,2,3,4,5)])
  ## Classifying the predictions as switched or not switched
  full.results = ifelse(full.fitted > .5, 1, 0)
  ## Calculating the Miscalculation rate
  misclass.full = mean(full.results != test.data$switch)
  ## Converting misclassification rate to accuracy. I did it this way because I thought misclassification might be usefu
  accuracy[i,1] = 1-misclass.full
  deviance[i,1] = summary(full.train.lm)$deviance
  
  # Taking out association out of the full model
  almost.train.lm = glm(switch~arsenic+dist+educ, data = train.data, family = binomial())
  almost.fitted = predict(almost.train.lm, newdata = test.data[,c(1,2,3,5)])
  almost.results = ifelse(almost.fitted > .5,1,0)
  misclass.almost = mean(almost.results != test.data$switch)
  accuracy[i,2] = 1-misclass.almost
  deviance[i,2] = summary(almost.train.lm)$deviance
  
  # With interactions = arsenic:education, educ:dist, arsenic:dist, I didn't included the association interactions here,but we could
  interaction.lm = glm(switch~arsenic+dist+educ+arsenic.dist, data = train.data, family = binomial())
  interaction.fitted = predict(interaction.lm, newdata = test.data[,c(1,2,3,5,6)])
  interaction.results = ifelse(interaction.fitted > .5,1,0)
  misclass.interaction = mean(interaction.results != test.data$switch)
  accuracy[i,3] = 1 - misclass.interaction
  deviance[i,3] = summary(interaction.lm)$deviance

  # With interactions = arsenic:education, educ:dist, arsenic:dist, I didn't included the association interactions here,but we could
  interaction1.lm = glm(switch~arsenic+dist+assoc+educ+arsenic.dist, data = train.data, family = binomial())
  interaction1.fitted = predict(interaction1.lm, newdata = test.data[,c(1,2,3,4,5,6)])
  interaction1.results = ifelse(interaction1.fitted > .5,1,0)
  misclass.interaction1 = mean(interaction1.results != test.data$switch)
  accuracy[i,4] = 1 - misclass.interaction1
  deviance[i,4] = summary(interaction1.lm)$deviance
  
    
  # Without the arsenic and distance interaction because it didn't seem effective
  without.ars.dist.int = glm(switch~.*., data = train.data[,c(1,2,3,4,5)], family = binomial())
  without.fitted = predict(without.ars.dist.int, newdata = test.data[,c(1,2,3,4,5)])
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

# Plots for each of the continuous variables.  Probably not the best way to do it.
## Arsenic
range(log(ars.data$arsenic))
x.ars = seq(0,3,by = .01)


## Distance
range(log(ars.data$dist + 1))
x.dist = seq(0,6,by = .01)
