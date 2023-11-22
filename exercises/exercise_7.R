# load the package
library(glmmTMB)
# Variance partitioning with random effect models
# load the data
blossoms <- read.csv("~/BIOS4/data/blossoms.csv", header=TRUE)
names(blossoms)
blossoms$pop <- as.factor(blossoms$pop)
# Exercise 1
# create a model
m <- glmmTMB(UBW ~ 1 + (1|pop/patch), data = blossoms)
summary(m)

# Exercise 2
m1 <- glmmTMB(GAD ~ UBW + (1|pop), data = blossoms)
summary(m1)

newx <- seq(min(blossoms$UBW), max(blossoms$UBW), length.out=200)
plot(blossoms$UBW, blossoms$GAD, pch=16, col="grey", xlab="UBW", ylab="GAD")
for (i in 1 : length(levels(blossoms$pop))) {
  y_hat <- coef(m1)$cond$pop[i,1]+ coef(m1)$cond$pop[i,2]*newx
  lines(newx, y_hat, col=i, lwd=2)
}  
