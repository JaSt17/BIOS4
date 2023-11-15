#load packages
library(MASS)
# get the data
data <- read.csv("~/BIOS4/data/Eulaema.csv")
names(data)
# createa a model
m = glm.nb(Eulaema_nigrita ~ MAP + forest., data = data)
summary(m)
# calculate the diviance
1- m$deviance/m$null.deviance

# simulate data
newforest = seq(min(dat$forest.), max(dat$forest.), length.out=200)
newMAP = rep(mean(dat$MAP), length(newforest))
y_hat = predict(m, newdata=list(MAP=newMAP,
                                forest.=newforest),type="response")
newMAP2 = rep(mean(dat$MAP)+sd(dat$MAP), length(newforest))
y_hat2 = predict(m, newdata=list(MAP=newMAP2,
                                 forest.=newforest),type="response")
newMAP3 = rep(mean(dat$MAP)-sd(dat$MAP), length(newforest))
y_hat3 = predict(m, newdata=list(MAP=newMAP3,
                                 forest.=newforest),type="response")

# include all of the simulated data into a new dataframe 
newdata = data.frame(forest.=newforest, y_hat=y_hat, y_hat2=y_hat2, y_hat3=y_hat3)
# plot the data
