# load packages
library(rms)
# define functions
logit = function(x) log(x/(1-x))
invlogit = function(x) 1/(1+exp(-x))
# simulate data
x = rnorm(200, 10, 3)
eta = -2 + 0.4*x + rnorm(200, 0, 2)
p = invlogit(eta)
y = rbinom(200, 1, p)
par(mfrow=c(1,3))
# viziualize the data
plot(x, eta, las=1)
plot(x, p, las=1)
plot(x, y, las=1)
# create a model
m = glm(y~x, family=binomial(link="logit"))
summary(m)
# get the coeficients
coefs = summary(m)$coefficients
x_pred = seq(from=min(x), to=max(x), by=0.01)
y_hat = coefs[1,1] + coefs[2,1]*x_pred
p_hat = invlogit(y_hat)
par(mfrow=c(1,1))
plot(x, y, las=1)
lines(x_pred, p_hat, col="red", lwd=2)
abline(h=0.5, lty=2)
# get the corresponding x value for the y=0.5
x_value = (logit(0.5) - coefs[1,1])/coefs[2,1]
abline(v=x_value, lty=2)
# get the model discription
mod1b <- lrm(y ~ x)
print(mod1b)
# discrimination of the model
mean(p_hat[which(y==1)]) - mean(p_hat[which(y==0)])

# data exercise 
# load the data
dormancy <- read.csv("~/BIOS4/data/dormancy.csv", header=TRUE)
names(dormancy)
# get rid of the NA value 
dormancy <- na.omit(dormancy)
# create a subdata set
subdat = dormancy[dormancy$pop=="CC",]

# crate Successes and Failures
germ = subdat$germ2 * subdat$nseed #Successes
notgerm = subdat$nseed - germ #Failures

mod1 = glm(germ2 ~ timetosowing + MCseed, "binomial", weights = nseed , data=subdat)
summary(mod1)
coefs <- summary(mod1)$coefficients
coefs
time_pred = seq(from=min(subdat$timetosowing), to=max(subdat$timetosowing), by=0.5)
y_hat = coefs[1,1] + coefs[2,1]*time_pred
y_hat2 = coefs[1,1] + coefs[2,1]*time_pred - coefs[3,1]*sd(subdat$MCseed)
y_hat3 = coefs[1,1] + coefs[2,1]*time_pred + coefs[3,1]*sd(subdat$MCseed)

#-------------------------------------
# transform the data to binary
y <- rbinom(length(subdat$germ2/subdat$nseed), 1, subdat$germ2/subdat$nseed)
#--------------------------------------
# create a new dataframe with the predicted data
newdata <- data.frame(time_pred = time_pred, y_hat = invlogit(y_hat),
                      y_hat2 = invlogit(y_hat2), y_hat3 = invlogit(y_hat3))
#calculate the x value for the y=0.5
x_value <- (logit(0.5) - coefs[1, 1]) / coefs[2, 1] 
plot <- ggplot() +
  geom_line(data = newdata, aes(x = time_pred, y = y_hat)) +
  geom_ribbon(data = newdata, aes(x = time_pred, ymin = y_hat2, ymax = y_hat3), alpha = 0.2, fill = "#5e569b") +
  geom_point(data = subdat, aes(x = timetosowing, y = germ2/nseed), size = 1, shape = 21, fill = "#5e569b") +
  geom_segment(aes(x = 0, y = 0.5, xend = x_value, yend = 0.5), linetype = "dashed") +
  geom_segment(aes(x =  x_value, y = 0, xend = x_value, yend = 0.5), linetype = "dashed") +
  xlab("Time to sowing (days)") +
  ylab("Rate of germinations") +
  ggtitle("Duration of after-ripening required for germination") +
  theme_ipsum() +
  theme( 
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line.x.bottom = element_line(color = 'black'),
    axis.line.y.left = element_line(color = 'black'),
    panel.border = element_blank(),
    plot.title = element_text(size = 8),
    axis.title.x = element_text(size = 8),
    axis.text.x = element_text(size = 8),
    axis.title.y = element_text(size = 8),
    axis.text.y = element_text(size = 8))
plot
plot(subdat$timetosowing,y, las=1, xlab="Time to sowing (days)", ylab="Probability of germination")
lines(time_pred, p_hat, col="red", lwd=2)
abline(h=0.5, lty=2)
# get the corresponding x value for the y=0.5
x_value = (logit(0.5) - coef[1,1])/coef[2,1]
abline(v=x_value, lty=2)
# duration of after-ripening required for the expected germination to be 0.5 is 107 days
