# First one
ars.data = read.table("http://www2.stat.duke.edu/~st118/fye12takehome.txt", sep = " ", header = TRUE)
head(ars.data)

# Goals of this analysis
# What predicts why people switch wells? Question of prediction
# Graphical and numerical summaries that are important for communicating results
# Basically write a report to help them understand what things predict why people switch wells
# What's the rationale of people switching

#asdf

# Arsenic = amount of arsenic in well
# educ = years of schooling of head of household
# dist = distance in meters to nearest known safe well
# assoc = 1 = members of household are active in community organizations

# Full logistic Model
full.log = glm(switch~., data = ars.data, family = binomial())
exp(confint(full.log))

# Factors
well.fac.data = ars.data
fac.switch = as.factor(well.fac.data$switch)
well.fac.data$switch = fac.switch
well.fac.data$assoc = as.factor(well.fac.data$assoc)
# Conditional plots.  Just a way to visualize the variables and the switching
par(mfrow = c(2,2))
cdplot(switch~arsenic, data = well.fac.data, main = "Arsenic")
cdplot(switch~dist, data = well.fac.data, main = "Distance")
cdplot(switch~assoc, data = well.fac.data, main = "Association")
cdplot(switch~educ, data = well.fac.data, main = "Education")

# Plots of the predictors.  It is clear that many of them aren't normal, and that might be changing our results.
hist(ars.data$arsenic, freq = FALSE, ylim = c(0,.6))
lines(density(ars.data$arsenic))
hist(ars.data$dist, freq = FALSE, ylim = c(0,.02))
lines(density(ars.data$dist))
hist(ars.data$educ, freq = FALSE)
lines(density(ars.data$educ))

# Some possible transformations
hist(log(ars.data$arsenic + 1) , freq = FALSE)
lines(density(log(ars.data$arsenic + 1)))
hist(log(ars.data$dist + 1) , freq = FALSE)
lines(density(log(ars.data$dist + 1)))
hist(log(ars.data$educ + 1), freq = FALSE )
lines(density(log(ars.data$educ + 1)))
# These might help but not a ton.

# ANOVA 
anova(full.log, test = "Chisq")
str(full.log)
#summarise()

# Our main goal is prediction.  
glm(switch~., data = ars.data, family = binomial())

# Cross validation
n.15 = n*.15
n = nrow(ars.data)
n.iter = 100
n.methods = 5
accuracy = matrix(0, nrow = n.iter, ncol = n.methods)
deviance = matrix(0, nrow = n.iter, ncol = n.methods)
colnames(deviance) = c("full_log","without_assoc","interaction", "without_one_int", "null")
colnames(accuracy) = c("full_log","without_assoc","interaction", "without_one_int", "null")
for(i in 1:n.iter){
  test.index = sample(n,n.15)
  train.data = ars.data[-test.index,]
  test.data = ars.data[test.index,]
  # Full model
  ## Fit the model
  full.train.lm = glm(switch~., data= train.data, family = binomial())
  ## Predict using the full model
  full.fitted = predict(full.train.lm, newdata = test.data, type = 'response')
  ## Classifying the predictions as switched or not switched
  full.results = ifelse(full.fitted > .5, 1, 0)
  ## Calculating the Miscalculation rate
  misclass.full = mean(full.results != test.data$switch)
  ## Converting misclassification rate to accuracy. I did it this way because I thought misclassification might be usefu
  accuracy[i,1] = 1-misclass.full
  deviance[i,1] = summary(full.train.lm)$deviance
  
  # Taking out association out of the full model
  almost.train.lm = glm(switch~educ+arsenic+dist, data = train.data, family = binomial())
  almost.fitted = predict(almost.train.lm, newdata = test.data[,c(1,2,3,5)])
  almost.results = ifelse(almost.fitted > .5,1,0)
  misclass.almost = mean(almost.results != test.data$switch)
  accuracy[i,2] = 1-misclass.almost
  deviance[i,2] = summary(almost.train.lm)$deviance
  
  # With interactions = arsenic:education, educ:dist, arsenic:dist, I didn't included the association interactions here,but we could
  interaction.lm = glm(switch~educ + arsenic + dist + assoc + arsenic:educ + educ:dist + arsenic:dist, data = train.data, family = binomial())
  interaction.fitted = predict(interaction.lm, newdata = test.data)
  interaction.results = ifelse(interaction.fitted > .5,1,0)
  misclass.interaction = mean(interaction.results != test.data$switch)
  accuracy[i,3] = 1 - misclass.interaction
  deviance[i,3] = summary(interaction.lm)$deviance
  
  # Without the arsenic and distance interaction because it didn't seem effective
  without.ars.dist.int = glm(switch~educ+arsenic+dist+arsenic:educ+educ:dist, data = train.data, family = binomial())
  without.fitted = predict(without.ars.dist.int, newdata = test.data[,c(1,2,3,5)])
  without.results = ifelse(without.fitted > .5, 1,0)
  missclass.without = mean(without.results != test.data$switch)
  accuracy[i,4] = 1 - missclass.without
  deviance[i,4] = summary(without.ars.dist.int)$deviance
  
  # Null stuff
  deviance[i,5] = summary(without.ars.dist.int)$null
  null.rate = table(train.data$switch)[2] / nrow(train.data)
  # Not sure about this next step
  accuracy[i,5] = null.rate
  
  
}

# Some analyses
apply(accuracy,2,mean)
apply(deviance,2,mean)

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
#but they might not be necessary if we end up doing the logit, probit, and lpm

# ROC curve

# Confusion matrices


train.index
fitted.results = predict(full.train.log)

#####
interactions = glm(switch~arsenic + dist +assoc + educ + arsenic:assoc + arsenic:assoc + dist:assoc, data = ars.data, family = binomial())
summary(interactions)

# Clearly the predictors are not normal
# Use prediction and deviance because they both tell us different parts of the story.
# The rates aren't  great.  Deviance might also be valuable
# Process of figuring it out
# I didn't think a Bayesian model would be necessary