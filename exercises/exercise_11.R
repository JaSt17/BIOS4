# load packages
library(Hmsc)
library(glmmTMB)
# create random data
x = rnorm(200, 10, 3)
y = -2 + 0.4*x + rnorm(200, 0, 2)
# fit the model with lm
m1 = lm(y~x)
# fit the model with Hmsc
m2 = Hmsc(Y = as.matrix(y), XData = data.frame(x), XFormula = ~x,
          distr="normal")
# computing chanis
m2 = sampleMcmc(m2, samples=1000, transient=1000, thin=1,
                nChains=2, verbose=F)
# compare the results
summary(m1)$coef
mpost = convertToCodaObject(m2)
summary(mpost$Beta)

# plot the chains
plot(mpost$Beta)

# get the effective sample size
effectiveSize(mpost$Beta)
# get the potential scale reduction factor 
gelman.diag(mpost$Beta, multivariate=F)$psrf

# fitting a linear mixed model
set.seed(145)
x1 = rnorm(200, 10, 2)
groupmeans = rep(rnorm(10, 20, 4), each=20)
groupID = as.factor(rep(paste0("Group", 1:10), each=20))
y = 2 + 1.5*x1 + groupmeans + rnorm(200, 0, 2)
m1 = glmmTMB(y~x1 + (1|groupID))

# fit a mixed model with the Hmsc package
studyDesign = data.frame(group = as.factor(groupID))
rL1 = HmscRandomLevel(units = groupID)
m2 = Hmsc(Y = as.matrix(y), XData = data.frame(x1), XFormula = ~x1,
          studyDesign = studyDesign, ranLevels = list(group = rL1),
          distr="normal")
m2 = sampleMcmc(m2, samples=1000, transient=1000, thin=1,
                nChains=2, nParallel=2, verbose=F)
# compare the results
mpost = convertToCodaObject(m2)
summary(mpost$Beta)
getPostEstimate(m2, "Omega")$mean

# Exercise
# read data 
dat <- read.csv("~/BIOS4/data/Eulaema.csv")
# convert variablaes to factors
dat$SA = as.factor(dat$SA)
dat$method = as.factor(dat$method)
# create a subset of the data
XData = data.frame(dat[,4:12])
studyDesign = data.frame(SA = dat$SA)
rL1 = HmscRandomLevel(units = dat$SA)
m2 = Hmsc(Y = as.matrix(dat$Eulaema_nigrita), XData = XData, XFormula = ~forest.,
          studyDesign = studyDesign, ranLevels = list(SA = rL1),
          distr="poisson")
m2 = sampleMcmc(m2, samples=1000, transient=5000, thin=5,
                nChains=2, nParallel=2, verbose=500)
save(m2, file="m2.RData")
load("m2.RData")
mpost = convertToCodaObject(m2)
plot(mpost$Beta)
m1 = glmmTMB(Eulaema_nigrita ~ forest. + (1|SA), REML=F, family="poisson", data=dat)
