# load packages
library(ggplot2)
library(hrbrthemes)
library(dplyr)
library(tidyr)
library(tidyverse)
library(car)

# Data exercise 1
# load the data
alpineplants <- read.csv("~/BIOS4/data/alpineplants.csv")
names(alpineplants)
# get rid of the NA values
alpineplants <- na.omit(alpineplants)
# get ride of the outliner line in the data
alpineplants <- alpineplants[alpineplants$min_T_winter > -8,]
# analyse the factors that have an impact on the growth of the plant Carex.bigelowii
model_carex <- lm(sqrt(Carex.bigelowii) ~ mean_T_winter + min_T_winter + max_T_winter +
                    mean_T_summer + min_T_summer + max_T_summer +light + snow +
                    soil_moist + altitude , data=alpineplants)
# get the resiudals from the model
residuals <- resid(model_carex)
hist(residuals)
summary(model_carex)
anova(model_carex)
# calculate the multicollinearity
vif(model_carex)
# calculate the correlation
data <- alpineplants[,c("mean_T_winter","min_T_winter","max_T_winter","mean_T_summer",
                        "min_T_summer","max_T_summer","light","snow","soil_moist","altitude")]
cor(data)

# we see that only 3 factors have an impact are altitude and mean_T_summer
model_1 <- lm(Carex.bigelowii ~ mean_T_summer + min_T_summer+ altitude, data=alpineplants)
summary(model_1)
anova(model_1)
# calculate the multicollinearity
vif(model_1)
# check the correlation of the variables
cor(alpineplants$altitude, alpineplants$min_T_summer)
cor(alpineplants$mean_T_summer, alpineplants$min_T_summer)
cor(alpineplants$altitude, alpineplants$mean_T_summer)
# we see that altitude and mean_T_summer are correlated
# we can therefore remove one of the variables
model_2 <- lm(Carex.bigelowii ~ min_T_summer + altitude, data=alpineplants)
summary(model_2)
vif(model_2)

# analyse the factors that have an impact on the growth of the plant thalictrum
model_thalictrum <- lm(Thalictrum.alpinum ~ mean_T_winter + min_T_winter + max_T_winter +
                         mean_T_summer + min_T_summer + max_T_summer +light + snow +
                         soil_moist + altitude , data=alpineplants)
summary(model_thalictrum)
anova(model_thalictrum)
# the only two factors that have an impact are summer temperature and winter mean temperature
model_3 <- lm(Thalictrum.alpinum ~ mean_T_summer + mean_T_winter + max_T_winter, data=alpineplants)
summary(model_3)
anova(model_3)
vif(model_3)
# we see that mean_T_summer and max_T_summer are correlated
# we can therefore remove one of the variables
model_4 <- lm(Thalictrum.alpinum ~ mean_T_winter + mean_T_summer, data=alpineplants)
summary(model_4)


# Data exercise 2
# load the data
blossoms <- read.csv("~/BIOS4/data/blossoms.csv")
names(blossoms)
# use tidyverse to get an overview over the data
blossoms %>%
  count(pop)
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
blossoms$pop <- as.factor(blossoms$pop)
# get the datafram for pop = S9 and pop = s27
subset_blossoms <- blossoms[blossoms$pop == c("S9","S27"),]

# plot GA and UBW for S9 and S27
plot <-ggplot(subset_blossoms, aes(LBW, UBW, color= pop)) +
  geom_point() +
  stat_smooth(method = "lm", se = FALSE) +
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
    legend.title = element_blank(),
    legend.position = c(0.9,0.2),
    legend.key.size = unit(0.5,'cm')) +
  ggtitle("ASD and GSD") +
  xlab("ASD") +
  ylab("GSD")
plot

# ANCOVA-FUll model
blossom.m1 <- lm(UBW ~ GA * pop, data=blossoms)
anova(blossom.m1)
# reduced model
blossom.m2 <- lm(UBW ~ GA + pop, data=blossoms)
anova(blossom.m2)
summary(blossom.m2)

# compare the full and reduced model
anova(blossom.m1, blossom.m2)
# --> to slopes