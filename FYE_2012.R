# First one
ars.data = read.table("http://www2.stat.duke.edu/~st118/fye12takehome.txt", sep = " ", header = TRUE)
head(ars.data)

# Goals of this analysis
# What predicts why people switch wells? Question of prediction
# Graphical and numerical summaries that are important for communicating results
# Basically write a report to help them understand what things predict why people switch wells
# What's the rationale of people switching

# Arsenic = amount of arsenic in well
# educ = years of schooling of head of household
# dist = distance in meters to nearest known safe well
# assoc = 1 = members of household are active in community organizations

full.log = glm(switch~., data = ars.data, family = binomial())
exp(confint(full.log))
par(mfrow = c(2,2))
# Factors
well.fac.data = ars.data
fac.switch = as.factor(well.fac.data$switch)
well.fac.data$switch = fac.switch
well.fac.data$assoc = as.factor(well.fac.data$assoc)
# Conditional plots
cdplot(switch~arsenic, data = well.fac.data, main = "Arsenic")
cdplot(switch~dist, data = well.fac.data, main = "Distance")
cdplot(switch~assoc, data = well.fac.data, main = "Association")
cdplot(switch~educ, data = well.fac.data, main = "Education")
# Anova, currently not working
ars.log = glm(switch~arsenic, data = ars.data, binomial())
educ.log = glm(switch~educ, data = ars.data, binomial())
dist.log = glm(switch~dist, data = ars.data, binomial())
assoc.log = glm(switch~assoc, data = ars.data, binomial())
anova(ars.log, educ.log, dist.log, assoc.log, test = "Chisq")

anova(full.log, test = "Chisq")
str(full.log)
summarise()
# Cross Validation


# Bayesian

# Our main goal is prediction.  So I should be RMSE, MSE or something like that
# We don't actually use those in logistic regression do we?

# DELETE THIS BEFORE TURNING IT IN
# https://www.r-bloggers.com/how-to-perform-a-logistic-regression-in-r/
# DELETE

glm(switch~., data = ars.data, family = binomial())

# Cross validation
n = nrow(ars.data)
n.15 = n*.15
n.iter = 100
n.methods = 3
acc = matrix(0, nrow = n.iter, ncol = n.methods)
colnames(acc) = c("full_log","without_assoc","bayes")
for(i in 1:n.iter){
  test.index = sample(n,n.15)
  train.data = ars.data[-test.index,]
  test.data = ars.data[test.index,]
  full.train.lm = glm(switch~., data= train.data, family = binomial())
  full.fitted = predict(full.train.lm, newdata = test.data, type = 'response')
  full.results = ifelse(full.fitted > full.fitted, 1, 0)
  misclass.full = mean(full.results != test.data$switch)
  acc[i,1] = 1-misclass.full
  
  # Taking out association
  almost.train.lm = glm(switch~educ+arsenic+dist, data = train.data, family = binomial())
  almost.fitted = predict(almost.train.lm, newdata = test.data[,c(1,2,3,5)])
  almost.results = ifelse(almost.fitted > almost.fitted,1,0)
  misclass.almost = mean(almost.results != test.data$switch)
  acc[i,2] = 1-misclass.almost
  
  # Bayesian methodology right here
}
train.index
fitted.results = predict(full.train.log)
