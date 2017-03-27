rhs1 = rhs[,1:54]
prop.model1 = glm(treatment~ ., data = rhs1, family = binomial())
ps.values1 = prop.model1$fitted.values
rhs1$ps.values = NA
rhs1$ps.values1[as.numeric(names(ps.values1))] = ps.values1

# We will use SMR weights
smrw1 = NA
smrw1[rhs1$treatment == "TRUE"] = 1
smrw1[rhs1$treatment == "FALSE"] = (ps.values[rhs1$treatment == FALSE])/ (1 - ps.values[rhs1$treatment == FALSE])
rhs1$smrw = smrw1

large.small = c(unique(which(prop.scores > .95)),unique(which(prop.scores < .05)))

rhs1 = rhs1[-large.small,]

summaryBy(smrw ~ treatment,data = rhs1, FUN = sum)

weighted.stuff = smrw1 * ps.values1
plot(density(weighted.stuff), xlim = )
sort(weighted.stuff, decreasing = TRUE)[1:10]

ggplot(data = rhs1, aes(x = ps.values, color = factor(treatment), fill = factor(treatment), weight = smrw))+
  geom_density(alpha = .3)

large.small = c(unique(which(prop.scores > .95)),unique(which(prop.scores < .05)))

rhs.trimmed = rhs[-large.small,]
ggplot(rhs.trimmed, aes(x = ps.values, color = as.factor(treatment), fill = as.factor(treatment))) + 
  geom_density(alpha = .2) + guides(color = FALSE, fill = FALSE)

logit.smrw = glm(dth30 ~ treatment,
                 data = rhs1,
                 family = quasibinomial(),
                 weights = smrw)
exp(coef(logit.smrw))
summary(logit.smrw)

1- exp(.0569)

