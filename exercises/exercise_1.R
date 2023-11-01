# install and load the package
install.packages("hrbrthemes")
install.packages("ggplot2")
library(ggplot2)
library(hrbrthemes)

#set seed for reproducibility
set.seed(1)
# producing normal distribution data
x <- rnorm(50,10,2)
# calculating the coefficient of variation
CV_x = sqrt(var(x))/mean(x)
# create a output array
out = NULL
# bootstrap 1000 times
for(i in 1:1000){
  # sampling with replacement
  x_sample = sample(x,50,replace = TRUE)
  # calculating the coefficient of variation
  CV_x_sample = sqrt(var(x_sample))/mean(x_sample)
  # add the result to the output array
  out = c(out,CV_x_sample)
}
# histogram of the output
hist(out)
# calculate the 95% confidence interval
quantile(out,c(0.025,0.975))

# optional exersice 
# Use simulated data to show the close relationship between the SD of log-transformed data and the CV on arithmetic scale.
# set seed for reproducibility
set.seed(1)
# producing normal distribution data
x <- rnorm(100,10,2)
# calculating the coefficient of variation
CV_x = sqrt(var(x))/mean(x)
log_sd_x = sd(log(x))
# create a output array
out = data.frame(matrix(ncol = 2, nrow = 0))
colnames(out) = c("CV_x","log_sd_x")
# bootstrap 1000 times
for (i in 1:1000){
  # sampling with replacement
  x_sample = sample(x,100,replace = TRUE)
  # calculating the coefficient of variation
  CV_x_sample = sqrt(var(x_sample))/mean(x_sample)
  log_sd_x_sample = sd(log(x_sample))
  # add the result to the output array
  out = rbind(out,data.frame(CV_x_sample,log_sd_x_sample))
}

# plot the relationship between CV and log_sd
plot <-ggplot(out, aes(x=CV_x_sample, y=log_sd_x_sample)) +
  geom_point(shape=18, size=1, color="blue") +
  stat_smooth(method=lm, se=FALSE, color="red", lwd=0.5) +
  theme_ipsum() +
  theme(    
    axis.line.x.bottom = element_line(color='black'),
    axis.line.y.left = element_line(color='black'),
    panel.grid.major = element_blank(),
    plot.title = element_text(size=10),
    axis.title.x = element_text(size=8),
    axis.text.x = element_text(size=8),
    axis.title.y = element_text(size=8),
    axis.text.y = element_text(size=8)) +
  ggtitle("Compare Log(SD) to CV") +
  xlab("CV") +
  ylab("log(SD)")
plot

