library(MASS)
library(xtable)
library(MCMCpack)
rm(list=ls())

#setwd("~/Dropbox/School/601/hw11")
setwd("C:/Users/Philip/Dropbox/School/601/hw11")
dat=read.table('data_set.csv',sep=",",header=T)
dim(dat)

post_sum <- function(mat){ 
  summean = apply(mat,2,mean)
  sumsd = apply(mat,2,sd) 
  sumCI = t(apply(mat,2,quantile,prob=c(.025,.975)))
  sumtot = cbind(summean,sumsd,sumCI)
  colnames(sumtot)[1:2] = c("mean" , "std dev")
  return(sumtot)
}

dat=read.table('data_set.csv',sep=",",header=T)

Y = c(dat[,1])
Xobs = X = as.matrix(dat[,2:3])

miss_mod = function(Y=Y,X=Xobs,p=ncol(Xobs),mm = rep(0,p), tm = 1/100,mb = rep(0,p+1),
tb = 1/100,a = 1,b = 1,nu = 3,M = diag(p), burn = 1e3,reps = 1e3 ){

  n = length(Y)
  p = ncol(Xobs)

  miss = which(apply(Xobs, 1, function(x) any(is.na(x))))
  which.miss = apply(Xobs, 1, function(x) which(is.na(x)))[miss]
  nmiss = length(miss)

  Xint = cbind(1,Xobs)
  X[is.na(X)] =0 

  SIG = matrix(0,ncol=p^2, nrow=(burn+reps))
  mu = matrix(0,ncol=p, nrow=(burn+reps)) 
  beta = matrix(0,ncol=(p+1), nrow=(burn+reps)) 
  phi = numeric(burn+reps)

  phi[1] = 1

  st <- proc.time()

  for(i in 2:(reps+burn)){
   
    Xmu_mat = Reduce('+',lapply(1:n,function(j,m,v){(m[j,] - v)%*%
               t(m[j,] - v )},m=X,v=mu[i-1,]) )
    mu_mat = tm * (mu[i-1,] - mm) %*% t(mu[i-1,] - mm)
    sig = riwish(n + nu + 1, M + Xmu_mat + mu_mat)
    sigi = solve(sig)
    SIG[i,] = sig

    m = 1/(n + tm) * ( apply(X,2,sum) + tm * mm )
    s = 1/(n + tm) * sig
    mu[i,] = mvrnorm(1,m,s)  

    sig1 = solve(sigi + beta[i-1,2:3] %*% t(beta[i-1,2:3])*phi[i-1])

    for(j in 1:nmiss){
      mu1 = sig1  %*% (beta[i-1,2:3] * (Y[miss[j]] - 
           beta[i-1,1]) * phi[i-1] + sigi %*% mu[i,] )
      ind = which.miss[[j]]
      if(length(ind) ==p){
        m = mu1[ind]
        s = sig1[ind,ind]
      } else{
        m = mu1[ind] + sig1[ind,-ind] %*% solve(sig1[-ind,-ind]) %*% 
            (X[miss[j],-ind] - mu1[-ind])
        s = sig1[ind,ind] - sig1[ind,-ind] %*% solve(sig1[-ind,-ind]) %*% sig1[-ind,ind]
      }
      X[miss[j],which.miss[[j]]] = mvrnorm(1,m,s)
    }
  
    Xint = cbind(1,X)
  
    s = solve(t(Xint) %*% Xint + tb * diag(p+1))
    m = s %*% (t(Xint) %*% Y + tb * mb)
    beta[i,] = mvrnorm(1,m,s/phi[i-1]) 

    as = a + (n + p + 1)/2
    bs = b + 1/2 * ( sum((Y - Xint %*% beta[i,])^2) + tb * sum((beta[i,] - mb)^2) )
    phi[i] = rgamma(1,as,bs)

    time_its <- (proc.time() - st)[3] / (i-1)
    time_used <- round((proc.time() - st)[3]/(60),digits=4)
    time_left <- round(time_its * (reps+burn - i )/(60),digits=4)    

    cat("\r", i, "of", reps + burn,", about", time_left , "minutes left") 
    flush.console()
  
    if(i == (reps+burn)) cat("\r")
  }

  mat = cbind(beta,phi,1/phi,mu,SIG)[-(1:burn),]
  colnames(mat) =c(paste("b",0:p,sep=""),"phi","sig2",paste("mu",1:p,sep=""),
      paste("sig",1:(p^2),sep=""))
  return(mat)
}

mat = miss_mod(Y,Xobs,reps=1e4,burn=1e3)
post.sum = post_sum(mat)
xtable(post.sum[-9,])

inds = (1:11)[-10]
png("trace.png")
par(mfrow=c(5,2),mar=c(4,4.2,.1,.1))
for(i in inds) plot(mat[,i],type="l",ylab=colnames(mat)[i],xlab="iteration",cex.lab=1.4)
dev.off()

freq = lm(Y ~Xobs)
sum.freq = summary(freq)
xtable(sum.freq)

GIS.mat = post.sum[c(1,1,2,2,3,3),c(3,1,4)]
GIS.mat[c(2,4,6),2] = coef(freq)
GIS.mat[c(2,4,6),1] = confint(freq)[,1]
GIS.mat[c(2,4,6),3] = confint(freq)[,2]
GIS.mat= t(GIS.mat)

mn = min(GIS.mat)
mx = max(GIS.mat)

eps = .1
pdf("comp.pdf")
par(mar=c(7,5,1,1))
plot(1:6,GIS.mat[2,],ylim=c(mn,mx),xaxt="n", ann=FALSE,las=2,cex=2,pch=20)
axis(1,at=1:6,las=2,label= c("b0_bayes","b0_freq","b1_bayes","b2_freq","b1_bayes","b2_freq"),cex.axis=1.4)
title(ylab="Estimates",cex.lab=1.4)
segments(1:6,GIS.mat[1,],1:6,GIS.mat[3,],lwd=2)
segments(1:6-eps,GIS.mat[1,],1:6+eps,GIS.mat[1,],lwd=2)
segments(1:6-eps,GIS.mat[3,],1:6+eps,GIS.mat[3,],lwd=2)
dev.off()