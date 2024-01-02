#load packages
library(ggplot2)
library(hrbrthemes)
library(glmmTMB)
library(dplyr)
library(RColorBrewer)
library(lme4)
library(apaTables)

#load the data
data <- read.csv("~/BIOS4/Exam/exam2023_data.csv")
head(data)
names(data)

# get ride of NA values
data <- na.omit(data)

#landscape position is a factor
data$landscape_position <- factor(data$landscape_position)

# new column with the percentage cover of exotic plants
data$ExoticPlant_cover <- data$ExoticPerennialHerb_cover +
  data$ExoticPerennialGrass_cover +
  data$ExoticAnnualHerb_cover +
  data$ExoticAnnualGrass_cover

# new column with the percentage cover of native plants
data$NativePlant_cover <- data$NativePerennialHerb_cover + 
  data$NativePerennialGrass_cover 
  
# histogram of Exotic Plant cover
histo1 <- ggplot(data) +
  geom_histogram(aes(x = ExoticPlant_cover ), binwidth = 2, fill = "#B99470",alpha = 0.5, color = "black") +
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
  ggtitle("Histogram of exotic plant cover")+
  xlab("Percentage cover per quadrat")+
  ylab("frequency")
plot(histo1)

# histogram of Native Plant cover
histo2 <- ggplot(data) +
  geom_histogram(aes(x = NativePlant_cover ), binwidth = 2, fill = "#5F7F52",alpha = 0.5, color = "black") +
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
  ggtitle("Histogram of native plant cover")+
  xlab("Percentage cover per quadrat")+
  ylab("frequency")
plot(histo2)

# Try to fit different models on the data
# interesting abiotic factors are:
# 1. landscape position (factor)
# 2. annual precipitation (mm)
# 3. precipitation warmest quarter (mm)
# 4. precipitation coldest quarter (mm)
# 5. Estimated potassium concentration in % of soil
# 6. Estimated thorium concentration (ppm)
# 7. Estimated uranium concentration (ppm)
# 8. Incoming solar radiation in January of the sample year (WH/m2)
# 9. Incoming solar radiation in July of the sample year (WH/m2)

# start fitting models with the most important abiotic factors
# start with a model including all abiotic factors and then remove the least important ones

# start with exotic plant cover
# test different complex models 
model1 <- glmmTMB(data = data, ExoticPlant_cover~annual_precipitation+ 
                precipitation_warmest_quarter + precipitation_coldest_quarter +
                K_perc + Th_ppm + U_ppm + SRad_Jan + SRad_Jul +
                (1 | Landscape.position))
model2 <- glmmTMB(data = data, ExoticPlant_cover~annual_precipitation+ 
                precipitation_warmest_quarter + precipitation_coldest_quarter +
                K_perc + Th_ppm + U_ppm + SRad_Jan + SRad_Jul +
                (1 | Landscape.position), family="poisson")
model3 <- lm(ExoticPlant_cover~ Landscape.position + annual_precipitation+ 
                precipitation_warmest_quarter + precipitation_coldest_quarter +
                K_perc + Th_ppm + U_ppm + SRad_Jan + SRad_Jul, data=data)

# evaluate the different models
mlist <- list(model1, model2, model3)
AICTab <- AIC(model1, model2, model3)
AICTab$logLik <- unlist(lapply(mlist, logLik))
AICTab <- AICTab[order(AICTab$AIC, decreasing=F),]
AICTab$delta <- round(AICTab$AIC - min(AICTab$AIC), 2)
lh <- exp(-0.5*AICTab$delta)
AICTab$w <- round(lh/sum(lh), 2)
AICTab

# model 1 (mixed linear model) is the model which fits best
# now we reduce the number of abiotic factors to make the model simpler
m1 <- lm(ExoticPlant_cover~ Landscape.position + annual_precipitation+ 
           precipitation_warmest_quarter + precipitation_coldest_quarter +
           K_perc + Th_ppm + U_ppm + SRad_Jan + SRad_Jul, data=data)
m2 <- lm(ExoticPlant_cover~ Landscape.position + annual_precipitation+ 
           precipitation_warmest_quarter + precipitation_coldest_quarter +
           K_perc + U_ppm + SRad_Jan + SRad_Jul, data=data)
m3 <- lm(ExoticPlant_cover~ Landscape.position + annual_precipitation+ 
           precipitation_warmest_quarter + precipitation_coldest_quarter +
           K_perc + U_ppm, data=data)
m4<- lm(ExoticPlant_cover~ annual_precipitation+ 
          precipitation_warmest_quarter + precipitation_coldest_quarter +
          K_perc + U_ppm, data=data)
m5<- lm(ExoticPlant_cover~ annual_precipitation+ 
          precipitation_coldest_quarter +
          K_perc + U_ppm, data=data)
m6<- lm(ExoticPlant_cover~ annual_precipitation+ 
          precipitation_coldest_quarter + K_perc, data=data)
m7<- lm(ExoticPlant_cover~ 1, data=data)
# evaluate the different models
mlist <- list(m1, m2, m3, m4, m5, m6, m7)
AICTab <- AIC(m1, m2, m3, m4, m5, m6, m7)
AICTab$logLik <- unlist(lapply(mlist, logLik))
AICTab <- AICTab[order(AICTab$AIC, decreasing=F),]
AICTab$delta <- round(AICTab$AIC - min(AICTab$AIC), 2)
lh <- exp(-0.5*AICTab$delta)
AICTab$w <- round(lh/sum(lh), 2)
AICTab

# show the summary of the best fitting model 
summary(m3)
anova(m3)
# calculate the effect size of the different abiotic factors
effectsize_native <- eta_squared(anova(m3), partial = TRUE)

# now repeat the steps for native plant cover
# test different complex models
model1 <- glmmTMB(data = data, NativePlant_cover~annual_precipitation+ 
                     precipitation_warmest_quarter + precipitation_coldest_quarter +
                     K_perc + Th_ppm + U_ppm + SRad_Jan + SRad_Jul +
                     (1 | Landscape.position))
model2 <- glmmTMB(data = data, NativePlant_cover~annual_precipitation+ 
                     precipitation_warmest_quarter + precipitation_coldest_quarter +
                     K_perc + Th_ppm + U_ppm + SRad_Jan + SRad_Jul +
                     (1 | Landscape.position), family="poisson")
model3 <- lm(NativePlant_cover~ Landscape.position + annual_precipitation+
                precipitation_warmest_quarter + precipitation_coldest_quarter +
                K_perc + Th_ppm + U_ppm + SRad_Jan + SRad_Jul, data=data)

# evaluate the different models
mlist <- list(model1, model2, model3)
AICTab <- AIC(model1, model2, model3)
AICTab$logLik <- unlist(lapply(mlist, logLik))
AICTab <- AICTab[order(AICTab$AIC, decreasing=F),]
AICTab$delta <- round(AICTab$AIC - min(AICTab$AIC), 2)
lh <- exp(-0.5*AICTab$delta)
AICTab$w <- round(lh/sum(lh), 2)
AICTab

# model 3 (the simple linear model) is the model which fits best
# now we reduce the number of abiotic factors to make the model simpler
m1 <- lm(NativePlant_cover~ Landscape.position + annual_precipitation+ 
           precipitation_warmest_quarter + precipitation_coldest_quarter +
           K_perc + Th_ppm + U_ppm + SRad_Jan + SRad_Jul, data=data)
m2 <- lm(NativePlant_cover~ Landscape.position + annual_precipitation+ 
           precipitation_warmest_quarter + precipitation_coldest_quarter +
           K_perc + U_ppm + SRad_Jan + SRad_Jul, data=data)
m3 <- lm(NativePlant_cover~ Landscape.position + annual_precipitation+ 
           precipitation_warmest_quarter + precipitation_coldest_quarter +
           K_perc + U_ppm, data=data)
m4<- lm(NativePlant_cover~ annual_precipitation+ 
          precipitation_warmest_quarter + precipitation_coldest_quarter +
          K_perc + U_ppm, data=data)
m5<- lm(NativePlant_cover~ annual_precipitation+ 
          precipitation_coldest_quarter +
          K_perc + U_ppm, data=data)
m6<- lm(NativePlant_cover~ annual_precipitation+ 
          precipitation_coldest_quarter + K_perc, data=data)
m7<- lm(NativePlant_cover~ 1, data=data)
# evaluate the different models
mlist <- list(m1, m2, m3, m4, m5, m6, m7)
AICTab <- AIC(m1, m2, m3, m4, m5, m6, m7)
AICTab$logLik <- unlist(lapply(mlist, logLik))
AICTab <- AICTab[order(AICTab$AIC, decreasing=F),]
AICTab$delta <- round(AICTab$AIC - min(AICTab$AIC), 2)
lh <- exp(-0.5*AICTab$delta)
AICTab$w <- round(lh/sum(lh), 2)
AICTab

# show the summary of the best fitting model 
summary(m3)
anova(m3)
# calculate the effect size of the different abiotic factors
effectsize_native <- eta_squared(anova(m3), partial = TRUE)

#create a dataframe which has the Parameter for both native and exotic plant cover
effectsize <- data.frame(Parameter = rep(c("Landscape",
                                       "annual precipitation",
                                       "precipitation (warmest quarter)",
                                       "precipitation (coldest quarter)",
                                       "K concentration in %",
                                       "U concentration in ppm"), each = 2),
                         Value = c(effectsize_native$Eta2_partial[1], effectsize_exotic$Eta2_partial[1],
                                   effectsize_native$Eta2_partial[2], effectsize_exotic$Eta2_partial[2],
                                   effectsize_native$Eta2_partial[3], effectsize_exotic$Eta2_partial[3],
                                   effectsize_native$Eta2_partial[4], effectsize_exotic$Eta2_partial[4],
                                   effectsize_native$Eta2_partial[5], effectsize_exotic$Eta2_partial[5],
                                   effectsize_native$Eta2_partial[6], effectsize_exotic$Eta2_partial[6]),
                         Group = rep(c("native", "exotic"), each = 1))
# plot the effect size of the different abiotic factors on native and exotic plant cover
# scatterplot of exotic plant cover vs. native plant cover
bar1 <- ggplot(effectsize, aes(x = Parameter, y = Value, fill = Group)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  scale_fill_manual(values = c("#B99470", "#5F7F52")) +
  theme(    
    axis.line.x.bottom = element_line(color = 'black'),
    axis.line.y.left = element_line(color = 'black'),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = 8),
    axis.title.x = element_text(size = 8),
    axis.text.x = element_text(size = 8, angle = 45, hjust = 1),
    axis.title.y = element_text(size = 8),
    axis.text.y = element_text(size = 8),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 8),
    legend.position = c(0.9, 0.8),
    legend.key.size = unit(0.5, 'cm')) +
  ggtitle("Effect sizes of abiotic factors on the observation of exotic and native plants")+
  xlab("")+
  ylab("effect size")
plot(bar1)


