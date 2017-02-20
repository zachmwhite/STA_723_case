bio = read.table("http://stat.duke.edu/sites/stat.duke.edu/files/bioassay.txt", header = TRUE)
bio = read.table("C:/Users/Zachary/Desktop/Winter 2017/STA 723 Case Studies/STA_723_case/HW05/bioassay.txt", header = TRUE)
library(ggplot2)
# VArying the amount of agonist or antagonist
# Response = weight of uterus
par(mar = c(1.8,1.8,1.2,1.2))

na.index = which(is.na(bio$uterus))

bio$uterus = as.numeric(levels(bio$uterus))[bio$uterus]
bio$weight = as.numeric(levels(bio$weight))[bio$weight]
bio$ratio = bio$uterus / (1000*bio$weight)

## We eliminate the NA
s
bio = bio[-na.index,]

length(unique(bio$lab))
# EDA
plot(bio$weight,bio$uterus)
f = ggplot(bio, aes(x = weight, y = uterus, color = lab))
fp = f + geom_point()
fp + facet_wrap(.~lab)
## Uterus by weight
par(mar = c(1.8,1.8,1.2,1.2), mfrow = c(4,5))
levels.lab = unique(bio$lab)
for(i in 1:19){
  plot(bio$weight[bio$lab == levels.lab[i]] , bio$uterus[bio$lab == levels.lab[i]])
}

## Uterus and weight by lab
boxplot(bio$uterus ~ bio$lab)
boxplot(bio$weight ~ bio$lab)

# EM and what
for(i in 1:19){
  plot(bio$EE[bio$lab == levels.lab[i]],bio$uterus[bio$lab == levels.lab[i]])
  legend("topleft",paste(levels.lab[i]))
}

for(i in 1:19){
  plot(bio$ZM[bio$lab == levels.lab[i]],bio$uterus[bio$lab == levels.lab[i]])
  legend("topleft",paste(levels.lab[i]))
}

## Protocol
par(mar = c(1.8,1.8,1.2,1.2), mfrow = c(1,1))
plot(bio$protocol,bio$uterus)
f = ggplot(bio, aes(x = weight, y = uterus, color = protocol))
fp = f + geom_point()
fp

par(mar = c(1.8,1.8,1.2,1.2), mfrow = c(3,2))
hist(bio$uterus, breaks = 60)
hist(bio$uterus[bio$protocol == "A"], main = "A", breaks = 60)
hist(bio$uterus[bio$protocol == "B"], main = "B", breaks = 60)
hist(bio$uterus[bio$protocol == "C"], main = "C", breaks = 60)
hist(bio$uterus[bio$protocol == "D"], main = "D", breaks = 60)

par(mar = c(1.8,1.8,1.2,1.2), mfrow = c(3,2))
hist(log(bio$uterus), breaks = 60)
hist(log(bio$uterus[bio$protocol == "A"]), main = "A", breaks = 60)
hist(log(bio$uterus[bio$protocol == "B"]), main = "B", breaks = 60)
hist(log(bio$uterus[bio$protocol == "C"]), main = "C", breaks = 60)
hist(log(bio$uterus[bio$protocol == "D"]), main = "D", breaks = 60)

par(mar = c(1.8,1.8,1.2,1.2), mfrow = c(3,2))
hist(bio$ratio, breaks = 60)
hist(bio$ratio[bio$protocol == "A"], main = "A", breaks = 60)
hist(bio$ratio[bio$protocol == "B"], main = "B", breaks = 60)
hist(bio$ratio[bio$protocol == "C"], main = "C", breaks = 60)
hist(bio$ratio[bio$protocol == "D"], main = "D", breaks = 60)

cor(bio$uterus,bio$weight, na.rm = TRUE)
# Prelim Models


# Hierarchical models

# Posterior analysis