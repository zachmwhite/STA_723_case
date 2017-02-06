# Same framework as last week
# Investigate the effect of distance = sacel: 20 meters
# Controlled for education and arsenic
# Type 1 error fixed at .05 = significance level

install.packages("pwr")
pwr.f2.test() # For general linear model

ars.data = read.table("http://www2.stat.duke.edu/~st118/fye12takehome.txt", sep = " ", header = TRUE)
ars.data = read.table("C://Users//Zachary//Desktop//Winter 2017//STA 723 Case Studies//STA_723_case//HW01//fye12takehome.txt",sep = " ", header = TRUE)

ars.data$dist = ars.data$dist / 20
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

for(i in 1:length(effect.vec)){
  for(j in 1:length(samp.size)){
    power.matrix[j,i] = power.sim(effect.vec[i],samp.size[j],100)
    print(j)
  }
  print(i)
}

new.samp.size = seq(100,30000,by = 500)
small.effect = rep(0,length(new.samp.size))
for(j in 1:length(new.samp.size)){
  small.effect[j] = power.sim(effect.vec[3],new.samp.size[j],100)
  print(c(j,small.effect[j]))
}


# Effect size of -.20
## Area of interest 0-650
large.samp.size = seq(25,1000,by = 10)
large.pred.effect = rep(0,length(large.samp.size))
for(j in 1:length(large.samp.size)){
  large.pred.effect[j] = power.sim(effect.vec[1],large.samp.size[j],500)
  print(c(large.samp.size[j],large.pred.effect[j]))
}

min(large.samp.size[large.pred.effect >= .8])

plot(large.samp.size,large.pred.effect, cex =  .8, xlab = "Sample Size", ylab = "Power", main = "Effect size = -0.20")
lines(large.samp.size,large.pred.effect)
abline(h = .8, col = "red")
abline(v = 255, col = "red")

# EFfect size of .20
large.pos.size = seq(25,1000,by = 10)
large.pos.effect = rep(0,length(large.samp.size))
for(j in 1:length(large.samp.size)){
  large.pos.effect[j] = power.sim(.2,large.pos.size[j],500)
  print(c(large.pos.size[j],large.pos.effect[j]))
}

min(large.pos.size[large.pos.effect >= .8])

plot(large.pos.size,large.pos.effect, cex =  .8, xlab = "Sample Size", ylab = "Power", main = "Effect size = -0.20")
lines(large.pos.size,large.pos.effect)
abline(h = .8, col = "red")
abline(v = min(large.pos.size[large.pos.effect >= .8]), col = "red")

# Effect size of -.17
## Area of interest 0-650
ref.pred.samp.size = seq(25,1000,by = 10)
ref.pred.effect = rep(0,length(ref.pred.samp.size))
for(j in 1:length(ref.pred.samp.size)){
  ref.pred.effect[j] = power.sim(-.17,ref.pred.samp.size[j],500)
  print(c(ref.pred.samp.size[j],ref.pred.effect[j]))
}
plot(ref.pred.samp.size,ref.pred.effect, cex =  .8, xlab = "Sample Size", ylab = "Power", main = "Effect size = -0.17")
lines(ref.pred.samp.size,ref.pred.effect)
abline(h = .8, col = "red")
abline(v = min(ref.pred.samp.size[ref.pred.effect >= .8])
       , col = "red")


# Positive MLE
pos.mle.samp.size = seq(25,1000,by = 10)
pos.mle.effect = rep(0,length(pos.mle.samp.size))
for(j in 1:length(pos.mle.samp.size)){
  pos.mle.effect[j] = power.sim(.17,pos.mle.samp.size[j],500)
  print(c(pos.mle.samp.size[j],pos.mle.effect[j]))
}
plot(pos.mle.samp.size,pos.mle.effect, cex =  .8, xlab = "Sample Size", ylab = "Power", main = "Effect size = -0.17")
lines(pos.mle.samp.size,pos.mle.effect)
abline(h = .8, col = "red")
abline(v = min(pos.mle.samp.size[pos.mle.effect >= .8])
       , col = "red")


# Effect Size of -.10
## Area of interest  100-1300
neg.med.samp.size = seq(25,1300,by = 10)
neg.med.effect = rep(0,length(neg.med.samp.size))
for(j in 1:length(neg.med.samp.size)){
  neg.med.effect[j] = power.sim(-.10,neg.med.samp.size[j],500)
  print(c(neg.med.samp.size[j],neg.med.effect[j]))
}
plot(neg.med.samp.size,neg.med.effect, cex =  .8, xlab = "Sample Size", ylab = "Power", main = "Effect size = -0.10")
lines(neg.med.samp.size,neg.med.effect)
abline(h = .8, col = "red")
abline(v = min(neg.med.samp.size[neg.med.effect >= .8])
       , col = "red")
 # Positive .10
pos.med.samp.size = seq(25,1300,by = 10)
pos.med.effect = rep(0,length(pos.med.samp.size))
for(j in 1:length(pos.med.samp.size)){
  pos.med.effect[j] = power.sim(.10,pos.med.samp.size[j],500)
  print(c(pos.med.samp.size[j],pos.med.effect[j]))
}
plot(pos.med.samp.size,pos.med.effect, cex =  .8, xlab = "Sample Size", ylab = "Power", main = "Effect size = -0.10")
lines(pos.med.samp.size,pos.med.effect)
abline(h = .8, col = "red")
abline(v = min(pos.med.samp.size[pos.med.effect >= .8])
       , col = "red")


# Effect Size of -.02
## Area of interest Huge
#new.samp.size = seq(100,30000,by = 500)
#small.effect = rep(0,length(new.samp.size))
#for(j in 1:length(new.samp.size)){
#  small.effect[j] = power.sim(effect.vec[3],new.samp.size[j],100)
#  print(c(j,small.effect[j]))
#}
#plot(new.samp.size,small.effect, cex =  .8, xlab = "Sample Size", ylab = "Power", main = "Effect size = -0.02")
#lines(new.samp.size,small.effect)
#abline(h = .8, col = "red")
#abline(v = min(new.samp.size[small.effect >= .8])
#       , col = "red")


#Effect Size of .05
## Area of interest 1000-5000
pos.small.samp.size = seq(700,5000,by = 20)
pos.small.effect = rep(0,length(pos.small.samp.size))
for(j in 1:length(pos.small.samp.size)){
  pos.small.effect[j] = power.sim(.05,pos.small.samp.size[j],500)
  print(c(pos.small.samp.size[j],pos.small.effect[j]))
}

plot(pos.small.samp.size,pos.small.effect, cex =  .6, xlab = "Sample Size", ylab = "Power", main = "Effect size = 0.05")
lines(pos.small.samp.size,pos.small.effect)
abline(h = .8, col = "red")
abline(v = min(pos.small.samp.size[pos.small.effect >= .8])
       , col = "red")

# Negative Small = -.05
neg.small.samp.size = seq(700,5000,by = 20)
neg.small.effect = rep(0,length(neg.small.samp.size))
for(j in 1:length(neg.small.samp.size)){
  neg.small.effect[j] = power.sim(-.05,neg.small.samp.size[j],500)
  print(c(neg.small.samp.size[j],neg.small.effect[j]))
}

plot(neg.small.samp.size,neg.small.effect, cex =  .6, xlab = "Sample Size", ylab = "Power", main = "Effect size = 0.05")
lines(neg.small.samp.size,neg.small.effect)
abline(h = .8, col = "red")
abline(v = min(pos.small.samp.size[pos.small.effect >= .8])
       , col = "red")

samp.size[20]
# EFfect size of .10
## Area of interest 0-2600
#pos.med.samp.size = seq(20,2600,by = 10)
#pos.med.effect = rep(0,length(pos.med.samp.size))
#for(j in 1:length(pos.med.samp.size)){
#  pos.med.effect[j] = power.sim(effect.vec[5],pos.med.samp.size[j],500)
#  print(c(pos.med.samp.size[j],pos.med.effect[j]))
#}

#samp.size[50]
#plot(pos.med.samp.size,pos.med.effect, cex =  .6, xlab = "Sample Size", ylab = "Power", main = "Effect size = 0.10")
#lines(pos.med.samp.size,pos.med.effect)
#abline(h = .8, col = "red")
#abline(v = min(pos.med.samp.size[pos.med.effect >= .8])
#       , col = "red")

# .03
#Effect Size of -.03
#Area of interest Huge
very.small.samp.size = seq(100,30000,by = 500)
very.small.effect = rep(0,length(new.samp.size))
for(j in 1:length(new.samp.size)){
  very.small.effect[j] = power.sim(-.03,very.small.samp.size[j],100)
  print(c(very.small.samp.size[j],very.small.effect[j]))
}

very.pos.small.samp.size = seq(100,30000,by = 300)
very.pos.small.effect = rep(0,length(new.samp.size))
for(j in 1:length(new.samp.size)){
  very.pos.small.effect[j] = power.sim(-.03,very.pos.small.samp.size[j],100)
  print(c(very.pos.small.samp.size[j],very.pos.small.effect[j]))
}

# .20
large.samp.size
large.pred.effect
large.pos.size
large.pos.effect

# .17
ref.pred.samp.size
ref.pred.effect 
pos.mle.samp.size
pos.mle.effect
# .10
neg.med.samp.size.plot = neg.med.samp.size[neg.med.samp.size <= 1000]
neg.med.effect.plot = neg.med.effect[1:length(neg.med.samp.size.plot)]
pos.med.samp.size.plot = pos.med.samp.size[pos.med.samp.size <= 1000]
pos.med.effect.plot = pos.med.effect[1:length(pos.med.samp.size.plot)]
#.05
neg.small.samp.size.plot = neg.small.samp.size[neg.small.samp.size <= 1000]
neg.small.effect.plot = neg.small.effect[1:length(neg.small.samp.size.plot)]
pos.small.samp.size.plot = pos.small.samp.size[pos.small.samp.size <= 1000]
pos.small.effect.plot = pos.small.effect[1:length(pos.small.samp.size.plot)]

par(mar=c(5.1, 4.1, 4.1, 8.1),xpd = TRUE)
plot(large.samp.size,large.pred.effect, type = "l", col = "blue", lwd = 2, xlab = "Sample Size", ylab = "Power", main = "Power by Sample Size", ylim = c(0, 1.1))
lines(large.pos.size, large.pos.effect, type = "l", col = "cadetblue1", lty = 2, lwd = 1)
lines(ref.pred.samp.size,ref.pred.effect, type = "l", lwd=2, col = "red")
lines(pos.mle.samp.size,pos.mle.effect, type = "l", lwd = 1, lty = 2,col = "brown1")
lines(neg.med.samp.size.plot,neg.med.effect.plot, type = "l", lwd = 2, col = "green")
lines(pos.med.samp.size.plot,pos.med.effect.plot, type = "l", lwd = 1, lty = 2, col = "darkolivegreen1")
lines(neg.small.samp.size.plot,neg.small.effect.plot, type = "l", lwd = 2, col = "gold")
lines(pos.small.samp.size.plot,pos.small.effect.plot, type = "l", lwd = 1, lty = 2, col = "gold4")

legend("topright", inset=c(-0.25,0),title = "Effect Size" ,legend = c("-0.20","MLE","-0.10","-0.05","0.05","0.10","+MLE","0.20"), 
       lty = c(1,1,1,1,2,2,2,2), col = c("blue","red","green","gold","gold4","darkolivegreen1","brown1","cadetblue1"))


#.03
very.pos.small.samp.size
very.pos.small.effect 
very.small.samp.size 
very.small.effect 