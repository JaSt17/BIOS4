#load packages
library(MASS)
library(ggplot2)
library(hrbrthemes)
# example
x = rnorm(200, 10, 3)
eta = -2 + 0.2*x
y = ceiling(exp(eta + rpois(200, 0.3)))
par(mfrow=c(1,2))
plot(x, eta, las=1)
plot(x, y, las=1)

m = glm(y~x, family="poisson")
summary(m)

par(mfrow=c(1,1))
plot(x, y, las=1, col="darkgrey", pch=16)
xx = seq(min(x), max(x), 0.01)
list(x=xx)
y_hat = predict(m, newdata=list(x=xx), type="response", se.fit=T)
lines(xx, y_hat$fit)
lines(xx, y_hat$fit+1.96*y_hat$se.fit, lty=2)
lines(xx, y_hat$fit-1.96*y_hat$se.fit, lty=2)
polygon(c(xx, rev(xx)),
        c(y_hat$fit+1.96*y_hat$se.fit,
          rev(y_hat$fit-1.96*y_hat$se.fit)),
        col = rgb(0,1,0,.5), border = FALSE)


# get the data
dat <- read.csv("~/BIOS4/data/Eulaema.csv")
names(dat)
# create a histogram for the data using ggplot2
ggplot(dat, aes(x = Eulaema_nigrita)) +
  geom_histogram(binwidth = 50, fill = "yellow", color = "black") +
  theme_ipsum() +
  theme(    
    axis.line.x.bottom = element_line(color = 'black'),
    axis.line.y.left = element_line(color = 'black'),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = 8),
    axis.title.x = element_text(size = 8),
    axis.text.x = element_text(size = 8),
    axis.title.y = element_text(size = 8),
    axis.text.y = element_text(size = 8),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 8),
    legend.position = c(0.9, 0.8),
    legend.key.size = unit(0.5, 'cm')) +
  ggtitle("Histogram of Eulaema nigrita abundances in the dataset")+
  xlab("Number of Euleama nigrita in the observation")+
  ylab("frequency")
# save the plot
ggsave("../plots/Eulaema_nigrita_histogram.png", width = 16, height = 8, units = "cm", dpi = 300)
# createa a model
m = glm(Eulaema_nigrita ~ MAP + forest.+ Pseason ,data = dat, family = poisson)
summary(m)
# rateher use negativ binomal
m = glm.nb(Eulaema_nigrita ~ MAP + forest.+ Pseason, data = dat)
summary(m)
# check the correalation of MAP and Pseason
cor(dat$MAP, dat$Pseason, method = "pearson")
# create a model for MAP and forest
m1 = glm.nb(Eulaema_nigrita ~ MAP + forest., data = dat)
summary(m1)
# create a model for MAP and Pseason
m2 = glm.nb(Eulaema_nigrita ~ MAP + Pseason, data = dat)
summary(m2)

# calculate the diviance
1- m$deviance/m$null.deviance
1- m1$deviance/m1$null.deviance
1- m2$deviance/m2$null.deviance

# simulate data
newforest = seq(min(dat$forest.), max(dat$forest.), length.out=200)
newPseason = seq(min(dat$Pseason), max(dat$Pseason), length.out=200)
newMAP = rep(mean(dat$MAP), length(newforest))
y_hat = predict(m1, newdata=list(MAP=newMAP,
                                forest.=newforest),type="response")
y_hat_P = predict(m2, newdata=list(MAP=newMAP,
                                Pseason=newPseason),type="response")
newMAP2 = rep(mean(dat$MAP)+sd(dat$MAP), length(newforest))
y_hat2 = predict(m1, newdata=list(MAP=newMAP2,
                                 forest.=newforest),type="response")
y_hat2_P = predict(m2, newdata=list(MAP=newMAP2,
                                 Pseason=newPseason),type="response")
newMAP3 = rep(mean(dat$MAP)-sd(dat$MAP), length(newforest))
y_hat3 = predict(m1, newdata=list(MAP=newMAP3,
                                 forest.=newforest),type="response")
y_hat3_P = predict(m2, newdata=list(MAP=newMAP3,
                                 Pseason=newPseason),type="response")

# include all of the simulated data into a new dataframe 
newdata1 = data.frame(forest.=newforest, y_hat=y_hat, y_hat2=y_hat2, y_hat3=y_hat3)
newdata2 = data.frame(Pseason=newPseason, y_hat=y_hat_P, y_hat2=y_hat2_P, y_hat3=y_hat3_P)

# plot the data for forest and MAP
plot <- ggplot() +
  geom_line(data = newdata1, aes(x = forest., y = y_hat, color = "MAP = Mean"), linewidth = 1.2) +
  geom_line(data = newdata1, aes(x = forest., y = y_hat2, color = "MAP = Mean + SD"), linewidth = 1.2) +
  geom_line(data = newdata1, aes(x = forest., y = y_hat3, color = "MAP = Mean - SD"), linewidth = 1.2) +
  geom_point(data = dat, aes(x = forest., y = Eulaema_nigrita), size = 1.5, shape = 1) +
  scale_color_manual(name = "", values = c("MAP = Mean" = "black", "MAP = Mean - SD" = "lightblue", "MAP = Mean + SD" = "darkblue")) +
  theme_ipsum() +
  theme(    
    axis.line.x.bottom = element_line(color = 'black'),
    axis.line.y.left = element_line(color = 'black'),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = 8),
    axis.title.x = element_text(size = 8),
    axis.text.x = element_text(size = 8),
    axis.title.y = element_text(size = 8),
    axis.text.y = element_text(size = 8),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 8),
    legend.position = c(0.9, 0.8),
    legend.key.size = unit(0.5, 'cm')) +
  ggtitle("Impact of Forest cover and mean annual precipitation \non El. nigrita abundance") +
  xlab("Forest cover") +
  ylab("El. nigrita abundance")
plot
# save the plot
ggsave("../plots/Eulaema_nigrita_forest.png", width = 16, height = 8, units = "cm", dpi = 300)
# plot the data for Pseason and MAP
plot <- ggplot() +
  geom_line(data = newdata2, aes(x = Pseason, y = y_hat, color = "MAP = Mean"), linewidth = 1.2) +
  geom_line(data = newdata2, aes(x = Pseason, y = y_hat2, color = "MAP = Mean + SD"), linewidth = 1.2) +
  geom_line(data = newdata2, aes(x = Pseason, y = y_hat3, color = "MAP = Mean - SD"), linewidth = 1.2) +
  geom_point(data = dat, aes(x = Pseason, y = Eulaema_nigrita), size = 1.5, shape = 1) +
  scale_color_manual(name = "", values = c("MAP = Mean" = "black", "MAP = Mean - SD" = "lightblue", "MAP = Mean + SD" = "darkblue")) +
  theme_ipsum() +
  theme(    
    axis.line.x.bottom = element_line(color = 'black'),
    axis.line.y.left = element_line(color = 'black'),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = 8),
    axis.title.x = element_text(size = 8),
    axis.text.x = element_text(size = 8),
    axis.title.y = element_text(size = 8),
    axis.text.y = element_text(size = 8),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 8),
    legend.position = c(0.2, 0.8),
    legend.key.size = unit(0.5, 'cm')) +
  ggtitle("Impact of precipitation seasonality and mean annual precipitation \non El. nigrita abundance") +
  xlab("precipitation seasonality in %") +
  ylab("El. nigrita abundance")
plot
# save the plot
ggsave("../plots/Eulaema_nigrita_Pseason.png", width = 16, height = 8, units = "cm", dpi = 300)
