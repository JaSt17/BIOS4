# load packages
library(ggplot2)
library(hrbrthemes)
library(dplyr)
library(tidyr)
library(tidyverse)
library(car)

# load the data
data <- read.csv("")
x1 <- rnorm(100)
x2 <- rnorm(100)
x3 <- rnorm(100)
y <- 1 + 2*x1 + 3*x2 + 4*x3 + rnorm(100)
data <- data.frame(x1, x2, x3, y)

# multiple regression
model <- lm(y ~ x1 + x2 + x3)
summary(model)
# save the coefficients
coef <- model$coefficients
coef
# calculate the variance of the predicted values
y_hat <- coef[1] + coef[2]*x1 + coef[3]*x2 + coef[4]*x3
var(y_hat)
var(y_hat)/var(y)

# normalize the values
x1_norm <- (x1 - mean(x1))/sd(x1)
x2_norm <- (x2 - mean(x2))/sd(x2)
x3_norm <- (x3 - mean(x3))/sd(x3)

model2 <- lm(y ~ x1_norm + x2_norm + x3_norm)
summary(model2)

# check the variables for correlation
cor(x1, x2)
cor(x1, x3)
cor(x2, x3)
VIF = 1/(1 - cor(x1, x2)^2)
VIF

# Data exercise 1
# load the data
alpineplants <- read.csv("~/BIOS4/data/alpineplants.csv")
names(alpineplants)
# get rid of the NA values
alpineplants <- na.omit(alpineplants)
# analyse the factors that have an impact on the growth of the plant Carex.bigelowii
model_carex <- lm(Carex.bigelowii ~ mean_T_winter + min_T_winter + max_T_winter +
                    mean_T_summer + min_T_summer + max_T_summer +light +
                    soil_moist + altitude , data=alpineplants)
summary(model_carex)
# calculate the multicollinearity
vif(model_carex)
# calculate the correlation
data <- alpineplants[,c("mean_T_winter","min_T_winter","max_T_winter","mean_T_summer",
                        "min_T_summer","max_T_summer","light","soil_moist","altitude")]
cor(data)

# we see that only two factors that have an impact are altitude and mean_T_summer
model_1 <- lm(Carex.bigelowii ~ mean_T_summer + min_T_summer+ altitude, data=alpineplants)
summary(model_1)
# calculate the multicollinearity
r2 <- summary(model_1)$r.squared
VIF = 1/(1 - r2)
VIF
# check the correlation of the variables
cor(alpineplants$altitude, alpineplants$min_T_summer)
cor(alpineplants$mean_T_summer, alpineplants$min_T_summer)
cor(alpineplants$altitude, alpineplants$mean_T_summer)
# we see that altitude and mean_T_summer are correlated
# we can therefore remove one of the variables
model_2 <- lm(Carex.bigelowii ~ min_T_summer + altitude, data=alpineplants)
summary(model_2)

# analyse the factors that have an impact on the growth of the plant thalictrum
model_thalictrum <- lm(Thalictrum.alpinum ~ mean_T_winter + min_T_winter + max_T_winter +
                         mean_T_summer + min_T_summer + max_T_summer +light +
                         soil_moist + altitude , data=alpineplants)
summary(model_thalictrum)
# the only two factors that have an impact are summer temperature and winter mean temperature
model_3 <- lm(Thalictrum.alpinum ~ mean_T_summer + max_T_summer + min_T_summer, data=alpineplants)
summary(model_3)
# look for correlation in the data
cor(alpineplants$mean_T_summer, alpineplants$max_T_summer)
cor(alpineplants$mean_T_summer, alpineplants$min_T_summer)
cor(alpineplants$max_T_summer, alpineplants$min_T_summer)
# we see that mean_T_summer and max_T_summer are correlated
# we can therefore remove one of the variables
model_4 <- lm(Thalictrum.alpinum ~ min_T_summer + mean_T_summer, data=alpineplants)
summary(model_4)


# Data exercise 2
# load the data
blossoms <- read.csv("~/BIOS4/data/blossoms.csv")
names(blossoms)
# use tidyverse to get an overview over the data
blossoms %>%
  group_by(pop) %>%
  summarise(LBWm = mean(LBW, na.rm=T),
            LBWsd = sd(LBW, na.rm=T),
            GSDm = mean(GSD, na.rm=T),
            GSDsd = sd(GSD, na.rm=T),
            ASDm = mean(ASD, na.rm=T),
            ASDsd = sd(ASD, na.rm=T))
# get rid of NaN values
blossoms <- na.omit(blossoms)
# calculate the corrleation between the variables
data <- blossoms[,c("ASD","GAD","GSD","LBL","LBW","UBL","UBW","GW","GA")]
cor(data)
# plot ASD agains GSD for the different poplulations
plot <-ggplot(blossoms) +
  geom_point( aes(x=GA, y=GW), shape=18, size=2, color = "red") +
  geom_point( aes(x=GA, y=UBW), shape=18, size=2, color = "blue") +
  theme_ipsum() +
  theme(    
    axis.line.x.bottom = element_line(color='black'),
    axis.line.y.left = element_line(color='black'),
    panel.border = element_blank(),
    plot.title = element_text(size=8),
    axis.title.x = element_text(size=8),
    axis.text.x = element_text(size=8),
    axis.title.y = element_text(size=8),
    axis.text.y = element_text(size=8),
    legend.text = element_text(size=8),
    legend.title = element_text(size=8),
    legend.position = c(0.9,0.9),
    legend.key.size = unit(0.5,'cm')) +
  ggtitle("ASD and GSD") +
  xlab("ASD") +
  ylab("GSD")
plot
