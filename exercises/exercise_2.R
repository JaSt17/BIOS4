# set the seed for reproducibility
set.seed(85)
# create random data
x <- rnorm(n=200, mean=10, sd=2)
y <- 0.4*x + rnorm(200, 0, 1)
# create a linear model
model <- lm(y~x)
# save the coefficients
cf <- model$coef

# createa a dataframe
df <- data.frame(x=x, y=y)
# ration of covariance and variance
cov(df$y, df$x)/var(df$x)
# how y changes if x changes for a standard deviation change in x
(cf[2]*(mean(x) + sd(x)))- (cf[2]*mean(x))
# calculate the r^2 value correlation factor
cor(x,y)^2
# variance in the predicted values
y_hat <- cf[1] + cf[2]x
var(y_hat)
var(y_hat)/var(y)

#Exersice 2

# install and load the package
install.packages("hrbrthemes")
install.packages("ggplot2")
library(ggplot2)
library(hrbrthemes)

# read the data
birds <- read.delim("~/BIOS4/data/bird_allometry.txt")
head(birds)
names(birds)

# create a linear model
x <- birds$Body.mass..g.
y <- birds$Brain.mass..g.
# scale values to log values
x <- log(x)
y <- log(y)

out <- data.frame(x=x, y=y)
model <- lm(y~x)
summary(model)
# plot the model to the scatter plot
plot <-ggplot(out, aes(x=x, y=y)) +
  geom_point(shape=18, size=1, color="blue") +
  stat_smooth(method=lm, se=FALSE, color="red", lwd=0.5) +
  theme_ipsum() +
  xlim(1,10) +
  ylim(-2,3) +
  theme(    
    axis.line.x.bottom = element_line(color='black'),
    axis.line.y.left = element_line(color='black'),
    panel.grid.major = element_blank(),
    plot.title = element_text(size=8),
    axis.title.x = element_text(size=8),
    axis.text.x = element_text(size=8),
    axis.title.y = element_text(size=8),
    axis.text.y = element_text(size=8)) +
  ggtitle("Relationship between Body mass and Brain mass") +
  xlab("Body mass (log g)") +
  ylab("Brain mass (log g)")
plot
ggsave("plot.png", plot, width = 15, height = 10, units = "cm", dpi = 300)
# optional Exercise
set.seed(85)
# create random data
x <- rnorm(n=100, mean=10, sd=1)
y <- 0.5*x + rnorm(100, 0, 1)
error_values <- c(0,0.1,0.2,0.3,0.4,0.5)
slope <- c()
corrected_slope <- c()
for (i in error_values){
  errors <- rnorm(100, 0, i)
  x_error <- x + errors
  K = 1 - i**2/var(x)
  m <- lm(y~x_error)
  cf <- m$coef
  slope <- c(slope,cf[2])
  corrected_slope <- c(corrected_slope,cf[2]/K)
}
slope
corrected_slope
df <- data.frame(error_values, slope, corrected_slope)
# plot the estimated slope and corrected slope to the error values
plot <- ggplot() +
  geom_point(data = df, aes(x = error_values, y = corrected_slope, color = "corrected slopes"), size = 3) +
  geom_point(data = df, aes(x = error_values, y = slope, color = "slopes"), size = 3) +
  scale_color_manual(values = c("corrected slopes" = "blue", "slopes" = "red"), name= "") +
  theme_ipsum() +
  xlim(0,0.5) +
  ylim(0.25,0.65) +
  theme(    
    axis.line.x.bottom = element_line(color='black'),
    axis.line.y.left = element_line(color='black'),
    panel.grid.major = element_blank(),
    plot.title = element_text(size=8),
    axis.title.x = element_text(size=8),
    axis.text.x = element_text(size=8),
    axis.title.y = element_text(size=8),
    axis.text.y = element_text(size=8),
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 8),
    legend.key.size = unit(0.5,'cm'),
    legend.position = c(0.25,0.25)) +
  ggtitle("Difference between slopes and corrected slopes") +
  xlab("Error standard deviation in x") +
  ylab("Estimated slope")
plot
