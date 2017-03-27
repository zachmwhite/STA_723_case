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
ecb.int[] = lapply(ecb.data,type.convert,as.is=TRUE)
# To do it for all names
char.index = which(sapply(ecb.int,class) == "character")
# do do it for some names in a vector named 'col_names'
ecb.int[char.index] <- lapply(ecb.int[char.index] , factor)
test.ecb = as.data.frame(ecb.int)
# Basically, I need to figure out what to do because I don't like it0
ecb.data = test.ecb
invest.grade.ind = which(colnames(ecb.data) == "Investment.Grade..Y.N.")
ecb.data= ecb.data[,-invest.grade.ind]
#############################################################
dim(ecb.data)
apply(ecb.data == -999999,2,sum)
apply(ecb.data == -999999,2,mean) 
mean(ecb.data == -999999)

z.mis = ecb.data == -999999
test.matrix = matrix(NA,nrow = 27, ncol = 9)
colnames(test.matrix) = levels(ecb.data$ISIN)
rownames(test.matrix) = names(ecb.data)
levels.isin = levels(ecb.data$ISIN)


for(i in 1:27){
  test = which(z.mis[,i] == TRUE)
  levels.test = ecb.data[test,1]
  test.matrix[i,] = table(levels.test)
}

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

# Imputatation chains.  Check convergence
chains = as.bugs.array(imp$mcmc) # it actually says imp@mcmc

# run more
imp = mi(imp, run.past.convergence = TRUE, n.iter = 10)

# Completed datasets
imp.dat.all = mi.completed(imp) # Datasets for all the iterations.  One list
imp.dat = mi.data.frame(imp, m = 1)

# We can then pool the complete case
