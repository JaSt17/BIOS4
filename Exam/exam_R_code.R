#load packages
library(ggplot2)
library(hrbrthemes)
library(dplyr)
library(tidyr)
library(MASS)

#load the data
data <- read.csv("~/BIOS4/Exam/exam2023_data.csv")
head(data)
names(data)

# get ride of NA values
data <- na.omit(data)

# new column with the percentage cover of exotic herbs and grasses
data$ExoticPlant_cover <- data$ExoticPerennialHerb_cover +
  data$ExoticPerennialGrass_cover +
  data$ExoticAnnualHerb_cover +
  data$ExoticAnnualGrass_cover

# new column with the percentage cover of native herbs and grasses
data$NativePlant_cover <- data$NativePerennialHerb_cover + 
  data$NativePerennialGrass_cover 

# Reshape data into long format
long_data <- data %>%
  gather(variable, value, c(ExoticPlant_cover, NativePlant_cover))

# change the names of the variables
long_data$variable <- gsub("ExoticPlant_cover", "Exotic herb & grass cover", long_data$variable)
long_data$variable <- gsub("NativePlant_cover", "Native herb & grass cover", long_data$variable)

# Create the combined histogram plot
combined_histo <- ggplot(long_data) +
  geom_histogram(aes(x = value, fill = variable), binwidth = 2, alpha = 0.5,
                 color = "black") +
  scale_fill_manual(values = c("#B99470", "#5F7F52")) +
  # Customize theme
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
    legend.position = "none") +
  
  ggtitle("Histogram of Exotic and Native herbs and grass cover") +
  xlab("Percentage cover per quadrat") +
  ylab("number of quadrats") +
  
  # Facet by variable
  facet_wrap(~variable, scales = "free_y", ncol = 1)

# Display the combined plot
print(combined_histo)
# save the plot
ggsave("C:/Users/jaro_/OneDrive/Dokumente/BIOS4/Exam/combined_histo.png", width = 16, height = 8, units = "cm", dpi = 300)

# Interesting abiotic factors are:
# 1. annual precipitation (mm)
# 2. precipitation warmest quarter (mm)
# 3. precipitation coldest quarter (mm)
# 4. MrVBF  (Valley Bottom Flatness index at the quadrat) scaled between(10-10^5m)
# 5. Estimated potassium concentration in (%)
# 6. Estimated thorium concentration (ppm)
# 7. Estimated uranium concentration (ppm)
# 8. Incoming solar radiation in January of the sample year (WH/m2)
# 9. Incoming solar radiation in July of the sample year (WH/m2)

# adjust the potassium concentration to smaller its per unit change to 10%
data$K_perc <- data$K_perc*10

# start with native herbs and grass cover
# try poisson regression first
model1 <- glm(data = data, NativePlant_cover~annual_precipitation + 
                precipitation_warmest_quarter + precipitation_coldest_quarter +
                + MrVBF + K_perc + Th_ppm + U_ppm + SRad_Jan + SRad_Jul, family="poisson")
summary(model1)
# Residual deviance: 5377.7  on 336  degrees of freedom <- over dispersion
# adjust by using negative binomial
model2 <- glm.nb(data = data, NativePlant_cover~annual_precipitation + 
                   precipitation_warmest_quarter + precipitation_coldest_quarter +
                   + MrVBF + K_perc + Th_ppm + U_ppm + SRad_Jan + SRad_Jul)
summary(model2)
# start with most complex model and exclude factors step by step to reduce complexity
m1 <- glm.nb(data = data, NativePlant_cover~annual_precipitation + 
               precipitation_warmest_quarter + precipitation_coldest_quarter +
               + MrVBF + K_perc + Th_ppm + U_ppm + SRad_Jan + SRad_Jul)
m2 <- glm.nb(data = data, NativePlant_cover~annual_precipitation + 
               precipitation_warmest_quarter + precipitation_coldest_quarter +
               + MrVBF + K_perc + Th_ppm + U_ppm)
m3 <- glm.nb(data = data, NativePlant_cover~annual_precipitation + 
                     precipitation_warmest_quarter + precipitation_coldest_quarter +
                     + MrVBF + K_perc + Th_ppm)
m4 <- glm.nb(data = data, NativePlant_cover~annual_precipitation + 
                precipitation_warmest_quarter + precipitation_coldest_quarter +
                + MrVBF + K_perc)
m5 <- glm.nb(data = data, NativePlant_cover~annual_precipitation + MrVBF + K_perc)
m6 <- glm.nb(data = data, NativePlant_cover~ MrVBF + K_perc)
m7<- glm.nb(NativePlant_cover~ 1, data=data)
# evaluate the different models
mlist <- list(m1, m2, m3, m4, m5, m6, m7)
AICTab <- AIC(m1, m2, m3, m4, m5, m6, m7)
AICTab$logLik <- unlist(lapply(mlist, logLik))
AICTab <- AICTab[order(AICTab$AIC, decreasing=F),]
AICTab$delta <- round(AICTab$AIC - min(AICTab$AIC), 2)
lh <- exp(-0.5*AICTab$delta)
AICTab$w <- round(lh/sum(lh), 2)
AICTab

# get the summary of the model with the lowest AIC
summary(m4)
anova(m4)
# Extract coefficients (estimates)
coefficients <- coef(m4)[-1]
# Extract standard errors
standard_errors <- summary(m4)$coefficients[, "Std. Error"][-1]
# Combine the results into a data frame
effects_native <- data.frame(Estimate = coefficients, `Std.Error` = standard_errors)
# Add the names of the variables
effects_native$variable <- c("annual precipitation",
                          "precipitation(warm)",
                          "precipitation(cold)",
                          "MrVBF",
                          "Potassium (K) in 10%")

# Create the bar plot for the abiotic factors for native herbs and grass cover
bar1 <- ggplot(effects_native, aes(x = variable, y = Estimate)) +
  geom_bar(aes(y = Estimate), stat = "identity", fill = "#5F7F52", alpha = 0.7, position = "identity") +
  geom_errorbar(aes(ymin = Estimate - Std.Error, ymax = Estimate + Std.Error), width = 0.2, position = "identity", color = "black") +
  geom_hline(yintercept = 0, color = "black", linetype = "dashed") +
  ylim(-0.22, 0.12) +
  # Customize theme
  theme(    
    axis.line.x.bottom = element_line(color = 'black'),
    axis.line.y.left = element_line(color = 'black'),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white"),
    plot.title = element_text(size = 8),
    axis.title.x = element_text(size = 8),
    axis.text.x = element_text(size = 8),
    axis.title.y = element_text(size = 8),
    axis.text.y = element_text(size = 8),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 8),
    legend.position = c(0.95, 0.2),
    legend.key.size = unit(0.3, 'cm')
  ) +
  
  # Add title and labels
  ggtitle("Effects of abiotic factors on the log rate of percentual native herbs and grasses cover") +
  xlab("") +
  ylab("change to the log rate per one-unit increase")

# Display the plot
print(bar1)
# save the plot
ggsave("C:/Users/jaro_/OneDrive/Dokumente/BIOS4/Exam/native_barplot.png", width = 16, height = 7, units = "cm", dpi = 300)

# now repeat it exotic herbs and grass cover
# try possion first 
model1 <- glm(data = data, ExoticPlant_cover~annual_precipitation + 
                precipitation_warmest_quarter + precipitation_coldest_quarter +
                + MrVBF + K_perc + Th_ppm + U_ppm + SRad_Jan + SRad_Jul, family="poisson")
summary(model1)
# Residual deviance: 5567.8  on 336  degrees of freedom
# adjust for overdispersion use negative binomial
model2 <- glm.nb(data = data, ExoticPlant_cover~annual_precipitation + 
                   precipitation_warmest_quarter + precipitation_coldest_quarter +
                   + MrVBF + K_perc + Th_ppm + U_ppm + SRad_Jan + SRad_Jul)
summary(model2)

# start with most complex model and exclude factors step by step to reduce complexity
m1 <- glm.nb(data = data, ExoticPlant_cover~annual_precipitation + 
               precipitation_warmest_quarter + precipitation_coldest_quarter +
               + MrVBF + K_perc + Th_ppm + U_ppm + SRad_Jan + SRad_Jul)
m2 <- glm.nb(data = data, ExoticPlant_cover~annual_precipitation + 
               precipitation_warmest_quarter + precipitation_coldest_quarter +
               + MrVBF + K_perc + Th_ppm + U_ppm)
m3 <- glm.nb(data = data, ExoticPlant_cover~annual_precipitation + 
               precipitation_warmest_quarter + precipitation_coldest_quarter +
               + MrVBF + K_perc + Th_ppm)
m4 <- glm.nb(data = data, ExoticPlant_cover~annual_precipitation + 
               precipitation_warmest_quarter + precipitation_coldest_quarter +
               + MrVBF + K_perc)
m5 <- glm.nb(data = data, ExoticPlant_cover~annual_precipitation + MrVBF + K_perc)
m6 <- glm.nb(data = data, ExoticPlant_cover~ MrVBF + K_perc)
m7<- glm.nb(ExoticPlant_cover~ 1, data=data)
# evaluate the different models
mlist <- list(m1, m2, m3, m4, m5, m6, m7)
AICTab <- AIC(m1, m2, m3, m4, m5, m6, m7)
AICTab$logLik <- unlist(lapply(mlist, logLik))
AICTab <- AICTab[order(AICTab$AIC, decreasing=F),]
AICTab$delta <- round(AICTab$AIC - min(AICTab$AIC), 2)
lh <- exp(-0.5*AICTab$delta)
AICTab$w <- round(lh/sum(lh), 2)
AICTab

# get the summary of the model with the lowest AIC
summary(m4)
anova(m4)
# Extract coefficients (estimates)
coefficients <- coef(m4)[-1]
# Extract standard errors
standard_errors <- summary(m4)$coefficients[, "Std. Error"][-1]
# Combine the results into a data frame
effects_exotic <- data.frame(Estimate = coefficients, `Std.Error` = standard_errors)
# Add the names of the variables
effects_exotic$variable <- c("annual precipitation",
                             "precipitation(warm)",
                             "precipitation(cold)",
                             "MrVBF",
                             "Potassium (K) in 10%")

# Create the bar plot for the abiotic factors for exotic herbs and grass cover
bar2 <- ggplot(effects_exotic, aes(x = variable ,y = Estimate)) +
  geom_bar(aes(y = Estimate), stat = "identity", fill = "#B99470", alpha = 0.7, position = "identity") +
  geom_errorbar(aes(ymin = Estimate - Std.Error, ymax = Estimate + Std.Error), width = 0.2, position = "identity", color = "black") +
  geom_hline(yintercept = 0, color = "black", linetype = "dashed") +
  ylim(-0.12, 0.1) +
  # Customize theme
  theme(    
    axis.line.x.bottom = element_line(color = 'black'),
    axis.line.y.left = element_line(color = 'black'),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white"),
    plot.title = element_text(size = 8),
    axis.title.x = element_text(size = 8),
    axis.text.x = element_text(size = 8),
    axis.title.y = element_text(size = 8),
    axis.text.y = element_text(size = 8),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 8),
    legend.position = c(0.95, 0.2),
    legend.key.size = unit(0.3, 'cm')
  ) +
  # Add title and labels
  ggtitle("Effects of abiotic factors on the log rate of percentual exotic herbs and grasses cover") +
  xlab("") +
  ylab("change to the log rate per one-unit increase")

print(bar2)
# save the plot
ggsave("C:/Users/jaro_/OneDrive/Dokumente/BIOS4/Exam/exotic_barplot.png", width = 16, height = 7, units = "cm", dpi = 300)
