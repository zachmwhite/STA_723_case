# Gelman Missingness
library(arm)
impute <- function (a, a.impute){
  ifelse (is.na(a), a.impute, a)
}

random.imp <- function (a){
  missing <- is.na(a)
  n.missing <- sum(missing)
  a.obs <- a[!missing]
  imputed <- a
  imputed[missing] <- sample (a.obs, n.missing, replace=TRUE)
  return (imputed)
}

n.sims = 10
n = 386
for(i in 1:n.isms){
  # Imputing Deal.Value.Euro..Face.
  deal.val = lm(Deal.Value.Euro..Face. ~ ISIN + Callable..Y.N. + Float..Y.N. + Parent.Specific.Industry.Group..SIG. + Years.to.Maturity +
                  Subordinated.Debt..Y.N. + Effective.Rating..Launch. + IG , data = ecb.test.data)
  pred.1 = rnorm(n, predict(deal.val),sigma.hat(deal.val))
  deal.val.imp = impute(ecb.test.data$Deal.Value.Euro..Face., pred.1)
  
  # Imputing 
}

n.sims <- 10
for (s in 1:n.sims){
  lm.1 <- lm (earnings ~ interest.imp + male + over65 + white +
                immig + educ_r + workmos + workhrs.top + any.ssi + any.welfare +
                any.charity)
  pred.1 <- rnorm (n, predict(lm.1), sigma.hat(lm.1))
  earnings.imp <- impute (earnings, pred.1)
  
  lm.1 <- lm (interest ~ earnings.imp + male + over65 + white +
                immig + educ_r + workmos + workhrs.top + any.ssi + any.welfare +
                any.charity)
  pred.1 <- rnorm (n, predict(lm.1), sigma.hat(lm.1))
  interest.imp <- impute (interest, pred.1)
}

test = mice(ecb.test.data)
