# Same framework as last week
# Investigate the effect of distance = sacel: 20 meters
# Controlled for education and arsenic
# Type 1 error fixed at .05 = significance level

install.packages("pwr")
pwr.f2.test() # For general linear model

ars.data = read.table("http://www2.stat.duke.edu/~st118/fye12takehome.txt", sep = " ", header = TRUE)
ars.data = read.table("C://Users//Zachary//Desktop//Winter 2017//STA 723 Case Studies//STA_723_case//HW01//fye12takehome.txt",sep = " ", header = TRUE)

ars.data$dist = ars.data$dist / 20
# Switch
switch = ars.data$switch
ave.switch = mean(switch)
draws = rbinom(n,1,ave.switch)

# ARsenic
arsenic = ars.data$arsenic
ave.ars = mean(arsenic)
# Distance
distance = ars.data$dist
ave.dist = mean(distance)
var.dist = var(distance)
a = 1
a.1 = 2
b = 1/48
b.1 = 1/24

pseudo.distance = seq(1,300, by = 200)

hist(distance, freq = FALSE)
curve(dgamma(x,a,b), add = TRUE)

# Association
assoc = ars.data$assoc
ave.assoc = mean(assoc)
# Education
educ = ars.data$educ
ave.educ = mean(educ)


# Game plan = figure out how to draw from the distributions
n = 1:5000


##############################################
# Boot Strap method
##############################################
mod2 <- glm(Y ~ offset(I(1.3 * X1)) + X2, family = "binomial") 
init.model = glm(switch ~ arsenic + dist + educ, data = ars.data, family = binomial())
coef(init.model)
ars.coef = coef(init.model)[2]
educ.coef = coef(init.model)[4]
test.eff.size = -.01
init.fixed.model = glm(switch~ offset(I(ars.coef * arsenic)) + offset(I(educ * educ.coef)) + offset(I(test.eff.size * dist))
                       , data = ars.data, family = binomial())
summary(init.fixed.model)
# Just testing stuff out
new.data = ars.data[1:100,]
new.fixed.model = glm(switch~ offset(I(ars.coef * arsenic)) + offset(I(educ * educ.coef)) + offset(I(test.eff.size * dist))
                      , data = new.data, family = binomial())
int.sized.data = ars.data[1:500,]
int.sized.fixed.model = glm(switch~ offset(I(ars.coef * arsenic)) + offset(I(educ * educ.coef)) + offset(I(test.eff.size * dist))
                      , data = int.sized.data, family = binomial())

#################################
# Actual Coding
bootstrap.data = ars.data[,c("arsenic","dist","educ")]
init.model = glm(switch ~ arsenic + dist + educ, data = ars.data, family = binomial())
manip.model = init.model
coef(init.model)
intercept = coef(init.model)[1]
ars.coef = coef(init.model)[2]
educ.coef = coef(init.model)[4]
test.eff.size = -.2
beta = c(intercept,ars.coef,test.eff.size,educ.coef)


# Another way
manip.model$coefficients = beta
pred.probs = predict.glm(manip.model,bootstrap.data, type = "response")
preds = ifelse(pred.probs > .5,1,0)

power.sim = function(effect.size,N,nreps = 1000){
  bootstrap.data = ars.data[,c("arsenic","dist","educ")]
  init.model = glm(switch ~ arsenic + dist + educ, data = ars.data, family = binomial())
  manip.model = init.model
  significance = rep(NA,nreps)
  intercept = coef(init.model)[1]
  ars.coef = coef(init.model)[2]
  educ.coef = coef(init.model)[4]
  beta = c(intercept,ars.coef,effect.size,educ.coef)
  manip.model$coefficients = beta
  for(i in 1:nreps){
    n = sample(nrow(bootstrap.data),N,replace = TRUE)
    test.data = bootstrap.data[n,]
    pred.probs = predict.glm(manip.model,test.data,type = "response")
    preds = rbinom(N,1,pred.probs)
    #preds = ifelse(pred.probs > .5,1,0)
    complete.test.data = cbind(preds,test.data)
    test.model = glm(preds~arsenic+dist+educ, data = complete.test.data, family = binomial(),
                     control = list(maxit = 100))
    significance[i] = ifelse(summary(test.model)$coefficients[3,4] < .05,1,0)
  }
  mean(significance)
}

mean(power.sim(-.01,10000,1000))

#pred.test.probs = predict.glm(init.model,bootstrap.data,type = "response")
#preds.test = ifelse(pred.test.probs > .5,1,0)

power.matrix = matrix(0,nrow = length(seq(100,5000,by = 50)),ncol = 5)
effect.vec = c(-.20,-.10,-.02,.05,.1)
samp.size = seq(100,5000,by = 50)

for(i in 3:length(effect.vec)){
  for(j in 1:length(samp.size)){
    power.matrix[j,i] = power.sim(effect.vec[i],samp.size[j],100)
    print(j)
  }
  print(i)
}

new.samp.size = seq(100,10000,by = 50)
small.effect = rep(0,length(new.samp.size))
  for(j in 1:length(new.samp.size)){
    small.effect[j] = power.sim(effect.vec[3],new.samp.size[j],100)
    print(c(j,small.effect[j]))
  }




  
##################################
# Use covariance structure
##################################
