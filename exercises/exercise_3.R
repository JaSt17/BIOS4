# exersice 3  
set.seed(100)
groups = as.factor(rep(c("Low", "Medium", "High"), each=50))
x = c(rnorm(50, 10, 3), rnorm(50, 13, 3), rnorm(50, 14, 3))
plot(groups, x, las=1, xlab="",
     ylab="Body size (g)")
# anova to test for differences between groups
model <- lm(x~groups)
summary(model)
anova(model)
# TukeyHSD to test for differences between groups
TukeyHSD(aov(x~groups))

groups = factor(groups, levels=c("Low", "Medium", "High"))
m = lm(x~groups-1)
summary(m)
confint(m)

# Data exercise
# load packages 
library('ggplot2')
library('hrbrthemes')
library('viridis')
library("cowplot")
library("broom")
# get the data
dat <- read.csv("~/BIOS4/data/butterflies.csv")
# look at all the header names of the columns
names(dat)
# rename the maternal host and larval host columns
dat$MaternalHost = paste0(dat$MaternalHost, "M")
dat$LarvalHost = paste0(dat$LarvalHost, "L")
DT_means = tapply(dat$DevelopmentTime, list(dat$MaternalHost, dat$LarvalHost), mean)
AW_means = tapply(dat$AdultWeight, list(dat$MaternalHost, dat$LarvalHost), mean)
GR_means = tapply(dat$GrowthRate, list(dat$MaternalHost, dat$LarvalHost), mean)
# plot the data
# plot the development time
plot1 <- ggplot(dat, aes(x = MaternalHost, y = DevelopmentTime, fill = LarvalHost)) +
  geom_boxplot() +
  theme_ipsum() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6, option="H") +
  theme(    
    axis.line.x.bottom = element_line(color='black'),
    axis.line.y.left = element_line(color='black'),
    panel.grid.major = element_blank(),
    plot.title = element_text(size=8),
    axis.title.x = element_text(size=8),
    axis.text.x = element_text(size=8),
    axis.title.y = element_text(size=8),
    axis.text.y = element_text(size=8)) +
  ggtitle("Development time of butterflies") +
  xlab("Maternal host") +
  ylab("Development time (days)")
# plot the adult weight
plot2 <- ggplot(dat, aes(x = MaternalHost, y = AdultWeight, fill = LarvalHost)) +
  geom_boxplot() +
  theme_ipsum() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6, option="H") +
  theme(    
    axis.line.x.bottom = element_line(color='black'),
    axis.line.y.left = element_line(color='black'),
    panel.grid.major = element_blank(),
    plot.title = element_text(size=8),
    axis.title.x = element_text(size=8),
    axis.text.x = element_text(size=8),
    axis.title.y = element_text(size=8),
    axis.text.y = element_text(size=8)) +
  ggtitle("Adult weight of butterflies") +
  xlab("Maternal host") +
  ylab("Adult weight (mg)")
# plot the growth rate
plot3 <- ggplot(dat, aes(x = MaternalHost, y = GrowthRate, fill = LarvalHost)) +
  geom_boxplot() +
  theme_ipsum() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6, option="H") +
  theme(    
    axis.line.x.bottom = element_line(color='black'),
    axis.line.y.left = element_line(color='black'),
    panel.grid.major = element_blank(),
    plot.title = element_text(size=8),
    axis.title.x = element_text(size=8),
    axis.text.x = element_text(size=8),
    axis.title.y = element_text(size=8),
    axis.text.y = element_text(size=8)) +
  ggtitle("Growth rate of butterflies") +
  xlab("Maternal host") +
  ylab("Growth rate (mg/day)")

# Arrange the plots in a 1x3 grid
combined_plot <- plot_grid(plot1, plot2, plot3, ncol = 2)
combined_plot

# ANOVA
# check presumptions for a ANOVA
# check for normal distribution with Shapiro-Wilk test
DT_shapiro <- tapply(dat$DevelopmentTime, list(dat$MaternalHost, dat$LarvalHost), shapiro.test)
AW_shapiro <- tapply(dat$AdultWeight, list(dat$MaternalHost, dat$LarvalHost), shapiro.test)
GR_shapiro <- tapply(dat$GrowthRate, list(dat$MaternalHost, dat$LarvalHost), shapiro.test)
# get the p-value for each group
DT_shapiro_p <- sapply(DT_shapiro, function(x) x$p.value)
AW_shapiro_p <- sapply(AW_shapiro, function(x) x$p.value)
GR_shapiro_p <- sapply(GR_shapiro, function(x) x$p.value)
# check for homogeneity of variance
bartlett.test(dat$DevelopmentTime ~ interaction(dat$MaternalHost, dat$LarvalHost))
bartlett.test(dat$AdultWeight ~ interaction(dat$MaternalHost, dat$LarvalHost))
bartlett.test(dat$GrowthRate ~ interaction(dat$MaternalHost, dat$LarvalHost))
# create linear models
DT_model <- lm(DevelopmentTime ~ MaternalHost * LarvalHost-1, data = dat)
summary(DT_model)
AW_model <- lm(AdultWeight ~ MaternalHost * LarvalHost-1, data = dat)
summary(AW_model)
GR_model <- lm(GrowthRate ~ MaternalHost * LarvalHost-1, data = dat)
summary(GR_model)
# run the anova
anova(DT_model)
anova(AW_model)
anova(GR_model)
# run the TukeyHSD
TukeyHSD(aov(DT_model))
TukeyHSD(aov(AW_model))
TukeyHSD(aov(GR_model))

#-------------------------------------------
# Hence only the parameter Adult weight fulfilled all the requirements for an
# Anova I decided to do a research question on the effect of the maternal host
# and the Larval Host on the adult weight
#-------------------------------------------
#create boxplots for data visualization
plot4 <- ggplot(dat, aes(x = LarvalHost, y = AdultWeight, fill = MaternalHost)) +
  geom_boxplot() +
  theme_ipsum() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6, option="H") +
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
    legend.key.size = unit(0.5,'cm')) +
  ggtitle("Adult weight of butterflies") +
  xlab("Larval host") +
  ylab("Adult weight (mg)")
plot4
ggsave("Boxplot_Adultweight_Larvalhost_Maternalhost.png", width = 16, height = 10, units = "cm")
# Create an interaction factor variable in your data that combines the two host variables
dat$InteractionFactor <- interaction(dat$LarvalHost, dat$MaternalHost)
# create densitiy plots for data visualization
plot4b <- ggplot(dat, aes(x=AdultWeight, fill= factor(InteractionFactor), color= factor(InteractionFactor)))+
  geom_density(alpha=0.5, linewidth=0.2,) +
  scale_fill_manual(values = c("#66101F","#855A5C","#2874A6","#0D2C54") ,name = "Groups: Larval host ~ Maternal host", labels =c("Barbarea ~ Barbarea","Berteroa ~ Barbarea","Barbarea ~ Berteroa","Bertero ~ Berteroa"))+
  scale_color_manual(values = c("#66101F","#855A5C","#2874A6","#0D2C54"), guide = FALSE)+
  theme_ipsum()+
  theme(    
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line.x.bottom = element_line(color='black'),
    axis.line.y.left = element_line(color='black'),
    panel.border = element_blank(),
    plot.title = element_text(size=8),
    axis.title.x = element_text(size=8),
    axis.text.x = element_text(size=8),
    axis.title.y = element_text(size=8),
    axis.text.y = element_text(size=8),
    legend.title = element_text(size = 6),
    legend.text = element_text(size = 6),
    legend.key.size = unit(0.3,'cm'),
    legend.position = c(0.85,0.9)) +
  ggtitle("Adult weight of butterflies") +
  ylab("Density") +
  xlab("Adult weight (mg)")
plot4b
ggsave("Densitiy_plot.png",plot4b,width = 16, height=10, units = 'cm',dpi = 300)
# do the ANOVA for MaternalHost and LarvalHost
anova(AW_model)
# do the TukeyHSD for MaternalHost and LarvalHost
TukeyHSD(aov(AW_model))

# it looks like the Larval host has a significant effect on the adult weight
# but the Maternal host does not
# if we look at the difference of the adult weight between the two Materinal hosts
# we can see that the difference is not significant
t.test(AdultWeight ~ MaternalHost, data = dat)
# on the other hand the difference between the two Larval hosts is significant
t.test(AdultWeight ~ LarvalHost, data = dat)

# now we want to know if there is a difference in the Adultweight to the sex of the butterfly
# plot the boxplot again but this time include the points and color them by their sex
# linear model with interaction
Sex_AW_model <- lm(AdultWeight ~ Sex * InteractionFactor-1, data = dat)
summary(Sex_AW_model)
# anova for sex and the interaction factor
anova(Sex_AW_model)
TukeyHSD(aov(Sex_AW_model))
# ttest for the individual impact of sex on the Adultweights
t.test(AdultWeight ~ Sex, data = dat)

# Plot the boxplot for the data seperated with the interaction factor
plot5 <- ggplot(dat, aes(x = InteractionFactor, y = AdultWeight, fill = Sex)) +
  geom_boxplot() +
  theme_ipsum() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6, option="C") +
  theme(    
    axis.line.x.bottom = element_line(color='black'),
    axis.line.y.left = element_line(color='black'),
    panel.grid.major = element_blank(),
    plot.title = element_text(size=8),
    axis.title.x = element_text(size=8),
    axis.text.x = element_text(angle = 45, hjust = 1, size=8),
    axis.title.y = element_text(size=8),
    axis.text.y = element_text(size=8)) +
  ggtitle("Adult weight of butterflies") +
  xlab("") +
  ylab("Adult weight (mg)")
plot5
ggsave("Boxplot_Adultweight_Sex_Larvalhost_Maternalhost.png", width = 16, height = 10, units = "cm")

# hence we already saw that the Larval host has a significant effect on the adult weight
# we combine the sex and the Larval host to see if they have an effect on the adult weight
Sex_Lh_AW_model <-lm(dat$AdultWeight ~ dat$Sex * dat$LarvalHost-1)
summary(Sex_Lh_AW_model)
anova(Sex_Lh_AW_model)
TukeyHSD(aov(Sex_Lh_AW_model))
# Plot the boxplot for the data seperated with the interaction factor
plot6 <- ggplot(dat, aes(x = LarvalHost, y = AdultWeight, fill = Sex)) +
  geom_boxplot() +
  theme_ipsum() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6, option="C") +
  theme(    
    axis.line.x.bottom = element_line(color='black'),
    axis.line.y.left = element_line(color='black'),
    panel.grid.major = element_blank(),
    plot.title = element_text(size=8),
    axis.title.x = element_text(size=8),
    axis.text.x = element_text(size=8),
    axis.title.y = element_text(size=8),
    axis.text.y = element_text(size=8)) +
  ggtitle("Adult weight of butterflies") +
  xlab("") +
  ylab("Adult weight (mg)")
plot6

