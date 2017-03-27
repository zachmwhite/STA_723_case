library(R2jags)
library(R2WinBUGS)
library(glmnet)
library(xlsx)
library(mice)
library(mi)
library(dplyr)
library(lattice)
library(VIM)
#############################################################
ecb.data = read.xlsx2("C:/Users/Zachary/Desktop/Winter 2017/STA 723 Case Studies/STA_723_case/HW08/RDD_causestudies1.xlsx",
                      sheetIndex = 1, startRow = 1, header = TRUE, as.data.frame = TRUE,
                      stringsAsFactors=FALSE)
ecb.data = read.xlsx2("~/Winter 2017/STA 723 Case Studies/STA_723_case/HW08/RDD_causestudies1.xlsx",
                      sheetIndex = 1, startRow = 1, header = TRUE, as.data.frame = TRUE,
                      stringsAsFactors=FALSE)
#"~/Winter 2017/STA 723 Case Studies/STA_723_case/HW08"
ecb.int = lapply(ecb.data,type.convert,as.is=TRUE)
ecb.int = as.data.frame(ecb.int)
# To do it for all names
char.index = which(sapply(ecb.int,class) == "character")
# do do it for some names in a vector named 'col_names'
ecb.int[char.index] <- lapply(ecb.int[char.index] , factor)
test.ecb = as.data.frame(ecb.int)
# Basically, I need to figure out what to do because I don't like it0
ecb.data = test.ecb
# Machine from campus
ecb.data = ecb.int
invest.grade.ind = which(colnames(ecb.data) == "Investment.Grade..Y.N.")
ecb.data= ecb.data[,-invest.grade.ind]


#############################################################
dim(ecb.data)
apply(ecb.data == -999999,2,sum)
apply(ecb.data == -999999,2,mean) 
mean(ecb.data == -999999)

# Missingness by bank variable.
z.mis = ecb.data == -999999
test.matrix = matrix(NA,nrow = 26, ncol = 9)
colnames(test.matrix) = levels(ecb.data$ISIN)
rownames(test.matrix) = names(ecb.data)
levels.isin = levels(ecb.data$ISIN)
for(i in 1:26){
  test = which(z.mis[,i] == TRUE)
  levels.test = ecb.data[test,1]
  test.matrix[i,] = table(levels.test)
}

# Missing values by the class of variable
class.matrix = matrix(0,nrow = 26, ncol = 3)
colnames(class.matrix) = unique(sapply(ecb.test.data,class))
rownames(class.matrix) = names(ecb.test.data)
levels.class = unique(sapply(ecb.test.data,class))

for(i in 1:26){
  sum.na = sum(z.mis[,i])
  class.current = class(ecb.test.data[,i])
  if(class.current == "factor"){
    class.matrix[i,1] = sum.na
  }
  if(class.current == "numeric"){
    class.matrix[i,2] = sum.na
  }
  if(class.current == "integer"){
    class.matrix[i,3] = sum.na
  }
}

perc.class.matrix = class.matrix / nrow(ecb.test.data)
# Missing variables are only numerical.

apply(test.matrix,2,sum) / sum(test.matrix)

#### Indivual variables
var.sum = apply(test.matrix,1,sum)
var.sum / sum(var.sum)
which(var.sum == 0)

ecb.test.data = ecb.data
ecb.test.data[ecb.test.data == -999999] = NA
test = mice(ecb.test.data)

test$method

test$predictorMatrix

################################################
# http://www.stat.columbia.edu/~gelman/research/published/mipaper.pdf

ecb.test.data = ecb.data
ecb.test.data[ecb.test.data == -999999] = NA
test = mice(ecb.test.data)
# MI package
ecb.missing = missing_data.frame(ecb.test.data)
image(ecb.missing)

# Structural problems
# mi.info(ecb.missing)

# preprocess
# ecb.new = mi.preprocess(ecb.missing)

# Info
#info = mi.info(ecb.test.data)
# chaning non-negative to continuous
info.upd = update(info, "type", list("var" = "continuous"))
# Formulas for the imputation
info$imp.formula
# Change formula
inf.upd = update(info,"imp.formula", list("var" = "var ~ cov1 + cov2 + ... covp"))
##########################
# Actual imputations and properties
ecb.new = mi.preprocess(ecb.test.data)
imp = mi(ecb.missing)
par(mfrow = c(1,3))
plot(imp)
http://www.stat.columbia.edu/~gelman/arm/examples/sis
# Imputatation chains.  Check convergence
chains = as.bugs.array(imp$mcmc) # it actually says imp@mcmc

# run more
imp = mi(imp, run.past.convergence = TRUE, n.iter = 10)

# Completed datasets
imp.dat.all = mi.completed(imp) # Datasets for all the iterations.  One list
imp.dat = mi.data.frame(imp, m = 1)

# We can then pool the complete case

###########################################
# March 27
# Doing MNAR in MICE
delta = c(0,-5,-10,-15,-20)
post = mice(ecb.test.data,maxit = 0)$post
imp.all = vector("list", length(delta))
for(i in 1:length(delta)){
  d = delta[i]
  cmd = paste("imp[[j]][,i] = imp[[j]][,i] + ",d)
  post["IG"] = cmd
  imp = mice(ecb.test.data,post = post, seed = i*2, print = FALSE)
  imp.all[[i]] = imp
}

test.imp = mice(ecb.test.data, maxit = 0, m = 0)
test.imp$method
#########################################################################
# Chains for imputations
########################################################################
delta = c(0,-5,-10,-15,-20)
post = mice(ecb.test.data,maxit = 0)$post
imp.all = vector("list", length(delta))
for(i in 1:length(delta)){
  d = delta[i]
  cmd = paste("imp[[j]][,i] = imp[[j]][,i] + ",d)
  post["IG"] = cmd
  imp = mice(ecb.test.data,post = post, seed = i*2, print = FALSE)
  imp.all[[i]] = imp
}


impute <- function (a, a.impute){
  ifelse (is.na(a), a.impute, a)
}




