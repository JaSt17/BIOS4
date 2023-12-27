#load packages
library(ggplot2)
library(MASS)
library(glmmTMB)

#load the data
data <- read.csv("~/BIOS4/Exam/exam2023_data.csv")
head(data)
names(data)

# get ride of NA values
data <- na.omit(data)

#landscape position is a factor
data$landscape_position <- factor(data$landscape_position)

# new colum with the percentage cover of exotic plants
data$ExoticPlant_cover = data$ExoticPerennialHerb_cover +
  data$ExoticPerennialGrass_cover + 
  data$ExoticAnnualHerb_cover +
  data$ExoticAnnualGrass_cover +
  data$ExoticShrub_cover

# new colum with the percentage cover of native plants
data$NativePlant_cover <- data$NativePerennialHerb_cover + 
  data$NativePerennialGrass_cover + 
  data$NativePerennialFern_cover + 
  data$NativePerennialGraminoid_cover +
  data$NativeShrub_cover 

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

# model 2 is the best model now we reduce the number of abiotic factors to make the model simpler
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

 