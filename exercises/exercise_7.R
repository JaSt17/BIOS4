# load the package
library(glmmTMB)
library(rsq)
# Variance partitioning with random effect models
# load the data
blossoms <- read.csv("~/BIOS4/data/blossoms.csv", header=TRUE)
# get rid of NA values
blossoms <- na.omit(blossoms)
names(blossoms)
blossoms$pop <- as.factor(blossoms$pop)
# Exercise 1
# create a model for ASD and GAD with pop
m <- glmmTMB(ASD ~ 1 + (1|pop), data = blossoms)
summary(m)
VarCorr(m)

# extract the variances from the model summary
VarAmongGroups <- attr(VarCorr(m)$cond$pop, "stddev")**2
VarWithinGroups <- attr(VarCorr(m)$cond, "sc")**2

# calculate the percent of the variance explained by groups:
Var_among <- VarAmongGroups/(VarAmongGroups+VarWithinGroups)*100
Var_within <- VarWithinGroups/(VarAmongGroups+VarWithinGroups)*100
# calculate the squared CV
CV2_Among = VarAmongGroups/mean(blossoms$GAD)**2
CV2_Within = VarWithinGroups/mean(blossoms$GAD)**2
CV2_Total = CV2_Among + CV2_Within
# create a dataset 
df = data.frame(Mean = mean(blossoms$GAD), SD = sd(blossoms$GAD),
                Among = Var_among,
                Within = Var_within,
                CV2_Among, CV2_Within, CV2_Total)
df = apply(df, MARGIN=2, FUN=round, digits=4)
df

"""
Interpretation: We can se that the intercept of the model is 19~~ 18.7 which is the mean
of the UBW. The standard deviation is 2.91. The variance among groups explains 25 % of the
Variation in UBW. The variance within groups explains 75 % of the variation in UBW.
"""

# Exercise 2
# create a model for ASD and GAD with pop
m1 <- glmmTMB(ASD ~ GAD + (GAD + 1|pop),REML = FALSE, data = blossoms)
summary(m1)
VarCorr(m1)
coef(m1)

# extract the variances from the model summary
VarAmongGroups <- attr(VarCorr(m1)$cond$pop, "stddev")**2
VarWithinGroups <- attr(VarCorr(m1)$cond, "sc")**2

# calculate the percent of the variance explained by groups:
Var_among <- VarAmongGroups/(VarAmongGroups+VarWithinGroups)*100
Var_within <- VarWithinGroups/(VarAmongGroups+VarWithinGroups)*100
# calculate the squared CV
CV2_Among = VarAmongGroups/mean(blossoms$GAD)**2
CV2_Within = VarWithinGroups/mean(blossoms$GAD)**2
CV2_Total = CV2_Among + CV2_Within
# create a dataset 
df = data.frame(Mean = mean(blossoms$GAD), SD = sd(blossoms$GAD),
                Among = Var_among,
                Within = Var_within,
                CV2_Among, CV2_Within, CV2_Total)
df = apply(df, MARGIN=2, FUN=round, digits=4)
df

newx <- seq(min(blossoms$GAD), max(blossoms$GAD), length.out=200)
plot(blossoms$GAD, blossoms$ASD, pch=1, col="grey", xlab="GAD", ylab="ASD")
for (i in 1 : length(levels(blossoms$pop))) {
  y_hat <- coef(m1)$cond$pop[i,1]+ coef(m1)$cond$pop[i,2]*newx
  lines(newx, y_hat, col=i, lwd=2)
}  

"""
Interpretation: We allow for different intercepts of the model for each population.
We see thath 46 % of the variance is within populations and 54 % is among populations.

"""
# create a model
m2_1 <- glmmTMB(ASD ~ 1 + (1|pop/patch),REML = FALSE, data = blossoms)
summary(m2_1)
# create a model
m2_2 <- glmmTMB(ASD ~ GAD + (1|pop/patch),REML = FALSE, data = blossoms)
summary(m2_2)
VarCorr(m2_2)
coef(m2_2)
