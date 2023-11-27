# load packages
library(ggplot2)
library(hrbrthemes)
library(glmmTMB)
library(dplyr)

# get the data
tephritis <- read.delim("~/BIOS4/data/tephritis.txt")
head(tephritis)
names(tephritis)

# get an overview of the data
# overview body length
histo1 <- ggplot(tephritis, aes(x = BL )) +
  geom_histogram(binwidth = 0.1, fill = "darkblue",alpha = 0.5, color = "black") +
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
  ggtitle("Histogram of body length of tephritis")+
  xlab("Body length in mm")+
  ylab("frequency")
print(histo1)

# density compare body length between regions
densityplot1 <- ggplot(tephritis, aes(x=BL, fill= factor(Baltic), color= factor(Baltic)))+
  geom_density(alpha=0.5, linewidth=0.2,) +
  scale_fill_manual(values = c("#855A5C","#2874A6") ,name = "Region of Baltic", labels = c("East", "West"))+
  scale_color_manual(values = c("#855A5C","#2874A6") , guide = FALSE)+
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
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 8),
    legend.key.size = unit(0.5,'cm'),
    legend.position = c(0.9,0.9)) +
  ggtitle("Comparison of body length of tephritis\nfrom different Regions of the Baltic see") +
  xlab("body length in mm") +
  ylab("Density")
print(densityplot1)

# density compare body length between differet host plants
densityplot2 <- ggplot(tephritis, aes(x=BL, fill= factor(Hostplant), color= factor(Hostplant)))+
  geom_density(alpha=0.5, linewidth=0.2,) +
  scale_fill_manual(values = c("#4E5166","#2CA58D") ,name = "Hostplants:", labels = c("Heterophyllum", "Oleraceum"))+
  scale_color_manual(values = c("#4E5166","#2CA58D") , guide = FALSE)+
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
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 8),
    legend.key.size = unit(0.5,'cm'),
    legend.position = c(0.9,0.9)) +
  ggtitle("Comparison of body length of tephritis\nfrom different hostplants") +
  xlab("body length in mm") +
  ylab("Density")
print(densityplot2)

# create a new factor variable that resembles all possible combination of hostplant and baltic
tephritis$HostplantBaltic <- factor(paste(tephritis$Hostplant, tephritis$Baltic, sep = "+"))

# density compare body length between different host plants and regions
densityplot3 <- ggplot(tephritis, aes(x=BL, fill= factor(HostplantBaltic), color= factor(HostplantBaltic)))+
  geom_density(alpha=0.5, linewidth=0.2,) +
  scale_fill_manual(values = c("#4E5166","#2CA58D","#855A5C","#2874A6") ,name = "Group:", labels = c("Heterophyllum East", "Oleraceum East", "Heterophyllum West", "Oleraceum West"))+
  scale_color_manual(values = c("#4E5166","#2CA58D","#855A5C","#2874A6") , guide = FALSE)+
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
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 8),
    legend.key.size = unit(0.2,'cm'),
    legend.position = c(0.9,0.9)) +
  ggtitle("Comparison of body length of tephritis\nfrom different hostplants and regions") +
  xlab("body length in mm") +
  ylab("Density")
print(densityplot3)

# create boxplots plots for the 4 different groups
boxplot <- ggplot(tephritis, aes(x=HostplantBaltic, y=BL, fill= factor(HostplantBaltic),))+
  geom_boxplot(alpha=0.5, linewidth=0.2,) +
  scale_fill_manual(values = c("#4E5166","#2CA58D","#855A5C","#2874A6"), guide = FALSE)+
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
    axis.text.x = element_text(size = 8, angle = 20, hjust = 1),
    axis.title.y = element_text(size=8),
    axis.text.y = element_text(size=8),
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 8),
    legend.key.size = unit(0.2,'cm'),
    legend.position = c(0.9,0.9)) +
  ggtitle("Comparison of body length of tephritis\nfrom different hostplants + regions") +
  xlab("") +
  ylab("body length in mm")
print(boxplot)

# create different models wit different complexity
model1 <- lm(BL ~ Hostplant*Baltic*Sex*Patry, data = tephritis)
model2 <- lm(BL ~ Hostplant*Patry*Baltic+Sex, data = tephritis)
model3 <- lm(BL ~ Hostplant*Patry+Hostplant+Sex, data = tephritis)
model4 <- lm(BL ~ Patry*Baltic, data = tephritis)
model5 <- lm(BL ~ Hostplant, data = tephritis)
model6 <- lm(BL ~ 1, data = tephritis)
# include a mixed effect model that account for the hierachical structure of the data
model7 <- lmer(BL ~ Patry +(1|Baltic/Hostplant), data = tephritis)

# compare the models
mlist = list(model1, model2, model3, model4, model5, model6, model7)
AICTab = AIC(model1, model2, model3, model4, model5, model6, model7)
AICTab$logLik = unlist(lapply(mlist, logLik))
AICTab = AICTab[order(AICTab$AIC, decreasing=F),]
AICTab$delta = round(AICTab$AIC - min(AICTab$AIC), 2)
lh = exp(-0.5*AICTab$delta)
AICTab$w = round(lh/sum(lh), 2)
# show the dataframe that contains the AIC values
AICTab

# show the summary of the best fitting model
summary(model2)
anova(model2)

# create a barplot that shows the difference in body length between the two regions
# and the different host plants
barplot <- ggplot(tephritis, aes(x = Patry, y = BL, fill = factor(HostplantBaltic))) +
  geom_bar(stat = "summary", fun.y = mean, position = position_dodge(), alpha = 0.5,
           color = "black") +
  scale_fill_manual(values = c("#4E5166","#2CA58D","#855A5C","#2874A6"), name = "Baltic:",
                    labels = c("Heterophyllum East", "Oleraceum East", "Heterophyllum West", "Oleraceum West")) +
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
    axis.text.y = element_text(size = 8),
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 8),
    legend.key.size = unit(0.3, 'cm'),
    legend.position = c(0.9, 0.9)
  ) +
  ggtitle("Comparison of body length of tephritis\nfrom different hostplants and regions") +
  xlab("") +
  ylab("body length in mm") +
  coord_cartesian(ylim = c(4.1, 4.8))

print(barplot)

# Calculate mean values
means <- tephritis %>%
  group_by(Patry, HostplantBaltic) %>%
  summarize(mean_BL = mean(BL))

# Create an interaction plot between the different host plants and the two regions
interaction_plot <- ggplot(tephritis, aes(x = Patry, y = BL, color = HostplantBaltic)) +
  geom_point(data = means, aes(y = mean_BL), position = position_dodge(width = 0.5), size = 3) +
  geom_line(data = means, aes(y = mean_BL, group = HostplantBaltic), position = position_dodge(width = 0.5), size = 1) + 
  scale_color_manual(values = c("#4E5166","#2CA58D","#855A5C","#2874A6") , name = "Groups:",
                     labels = c("Heterophyllum East", "Oleraceum East", "Heterophyllum West", "Oleraceum West")) +
  theme_ipsum()+
  ylim(4,5)+
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
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 8),
    legend.key.size = unit(0.5,'cm'),
    legend.position = c(0.9,0.9)) +
  labs(x = "Baltic Region", y = "Body Length in mm") +
  ggtitle("Interaction Plot of Body Length\nby Baltic Region and Host Plant")

# Display the plot
print(interaction_plot)
