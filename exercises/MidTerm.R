# load packages
library(ggplot2)
library(hrbrthemes)
library(glmmTMB)
library(dplyr)
library(RColorBrewer)
library(lme4)
library(apaTables)

# get the data
tephritis <- read.delim("~/BIOS4/data/tephritis.txt")
head(tephritis)
names(tephritis)
# create a new factor variable that resembles all possible combination of hostplant and baltic
tephritis$HostplantBaltic <- factor(paste(tephritis$Hostplant, tephritis$Baltic, sep = "+"))
# create a new factor variable that resembles all possible combination of hostplant and Patry
tephritis$HostplantPatry <- factor(paste(tephritis$Hostplant, tephritis$Patry, sep = "+"))
# create a new factor variable that resembles all possible combination of hostplant and baltic
tephritis$BalticPatry <- factor(paste(tephritis$Baltic, tephritis$Patry, sep = "+"))
# create a new factor variable that resembles all possible combination of hostplant and baltic and Patry
tephritis$HostplantBalticPatry <- factor(paste(tephritis$Hostplant, tephritis$Baltic, tephritis$Patry, sep = "+"))
names(tephritis)


# get an overview of the data
# overview body length
histo1 <- ggplot(tephritis, aes(x = BL )) +
  geom_histogram(binwidth = 0.08, fill = "darkblue",alpha = 0.5, color = "black") +
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

ggsave("Histogram.png",histo1, width = 10, height = 10, units = "cm", dpi = 300)

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

# density compare body length between different host plants and regions
densityplot3 <- ggplot(tephritis, aes(x=BL, fill= factor(HostplantBaltic), color= factor(HostplantBaltic)))+
  geom_density(alpha=0.5, linewidth=0.2,) +
  scale_fill_manual(values = c("#4E5166","#2CA58D","#855A5C","#2874A6") ,name = "Groups:", labels = c("Heterophyllum East", "Oleraceum East", "Heterophyllum West", "Oleraceum West"))+
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
    legend.position = c(0.92,0.9)) +
  ggtitle("Comparison of body length of tephritis\nbetween different hostplants and regions") +
  xlab("body length in mm") +
  ylab("Density")
print(densityplot3)

# density compare body length between different host plants and patry
densityplot4 <- ggplot(tephritis, aes(x=BL, fill= factor(HostplantPatry), color= factor(HostplantPatry)))+
  geom_density(alpha=0.5, linewidth=0.2,) +
  scale_fill_manual(values = c("#4E5166","#2CA58D","#855A5C","#2874A6") ,
                    name = "Groups:", labels = c("Heterophyllum Sympatry", "Oleraceum Sympatry", "Heterophyllum Allopatry", "Oleraceum Allopatry"))+
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
    legend.position = c(0.92,0.9)) +
  ggtitle("Comparison of body length of tephritis\nbetween different hostplants and patry") +
  xlab("body length in mm") +
  ylab("Density")
print(densityplot4)

# density compare body length between different host plants and regions
densityplot5 <- ggplot(tephritis, aes(x=BL, fill= factor(BalticPatry), color= factor(BalticPatry)))+
  geom_density(alpha=0.5, linewidth=0.2,) +
  scale_fill_manual(values = c("#4E5166","#2CA58D","#855A5C","#2874A6") ,
                    name = "Groups:", labels = c("East Sympatry", "West Sympatry", "East Allopatry", "West Allopatry"))+
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
    legend.position = c(0.92,0.9)) +
  ggtitle("Comparison of body length of tephritis\nfrom different regions and patry") +
  xlab("body length in mm") +
  ylab("Density")
print(densityplot5)

ggsave("Densitiy1.png",densityplot3, width = 10, height = 10, units = "cm", dpi = 300)
ggsave("Densitiy2.png",densityplot4, width = 10, height = 10, units = "cm", dpi = 300)
ggsave("Densitiy3.png",densityplot5, width = 10, height = 10, units = "cm", dpi = 300)

# create a color palette for the 8 different groups
coul <- brewer.pal(8, "BrBG") 

# create boxplots plots for the 8 different groups
boxplot <- ggplot(tephritis, aes(x=HostplantBalticPatry, y=BL, fill= factor(HostplantBalticPatry),))+
  geom_boxplot(alpha=0.5, linewidth=0.2,) +
  scale_fill_manual(values = coul, guide = FALSE)+
  scale_x_discrete(labels = c("EHA","EHS","WHA","WHS","EOA","EOS","WOA","WOS")) +  # Set custom labels for the x-axis
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
    axis.text.x = element_text(size = 8, angle = 45, hjust = 1),
    axis.title.y = element_text(size=8),
    axis.text.y = element_text(size=8),
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 8),
    legend.key.size = unit(0.2,'cm'),
    legend.position = c(0.9,0.9)) +
  ggtitle("Comparison of body length of tephritis\nfrom different hostplants, regions and patrys") +
  xlab("") +
  ylab("body length in mm")
print(boxplot)

ggsave("Boxplot.png",boxplot, width = 10, height = 10, units = "cm", dpi = 300)

# create different models wit different complexity
model1 <- lm(BL ~ Hostplant*Baltic*Patry, data = tephritis)
model2 <- lm(BL ~ Hostplant*Baltic+Patry, data = tephritis)
model3 <- lm(BL ~ Hostplant*Patry+Baltic, data = tephritis)
model4 <- lm(BL ~ Patry*Baltic+Hostplant, data = tephritis)
model5 <- lm(BL ~ Baltic, data = tephritis)
model6 <- lm(BL ~ 1, data = tephritis)
# include a mixed effect model that account for the hierachical structure of the data
model7 <- lmer(BL ~ 1 +(1|HostplantBalticPatry), data = tephritis)

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
summary(model1)
# make an anova to see if the groups are different
anova(model1)
# calculate the effect size
eta_squared(anova(model1), partial = TRUE)
# make a post hoc test to see which groups are different
TukeyHSD(aov(model1))


# create a barplot that shows the difference in body length between the two regions
# and the different host plants
barplot <- ggplot(tephritis, aes(x = Patry, y = BL, fill = factor(HostplantBaltic))) +
  geom_bar(stat = "summary", fun.y = mean, position = position_dodge(), alpha = 0.5,
           color = "black") +
  scale_fill_manual(values = c("#4E5166","#2CA58D","#855A5C","#2874A6"), name = "Group:",
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
    legend.position = c(0.9, 0.85)
  ) +
  ggtitle("Comparison of mean body length of tephritis from different\nhostplants and regions between Allopatry and Sympatry") +
  xlab("") +
  ylab("body length in mm") +
  coord_cartesian(ylim = c(4.1, 4.8))

print(barplot)
ggsave("Bar Plot.png",barplot, width = 10, height = 10, units = "cm", dpi = 300)

# Calculate mean values
means <- tephritis %>%
  group_by(Patry, HostplantBaltic) %>%
  summarize(mean_BL = mean(BL))

# Create an interaction plot between the different host plants and the two regions
interaction_plot1 <- ggplot(tephritis, aes(x = Patry, y = BL, color = HostplantBaltic)) +
  geom_point(data = means, aes(y = mean_BL), position = position_dodge(width = 0.5), size = 3) +
  geom_line(data = means, aes(y = mean_BL, group = HostplantBaltic), position = position_dodge(width = 0.5), size = 1) + 
  scale_color_manual(values = c("#4E5166","#2CA58D","#855A5C","#2874A6") , name = "Groups:",
                     labels = c("Heterophyllum East", "Heterophyllum West", "Oleraceum East", "Oleraceum West")) +
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
    legend.key.size = unit(0.3,'cm'),
    legend.position = c(0.9,0.8)) +
  labs(x = "", y = "Body Length in mm") +
  ggtitle("Interaction Plot of body length of different groups of\nHostplants and Regions by Allopatry and Sympatry")

# Display the plot
print(interaction_plot1)

# save the plots
ggsave("Interaction Plot1.png",interaction_plot1, width = 10, height = 10, units = "cm", dpi = 300)

# Calculate mean values
means1 <- tephritis %>%
  group_by(Baltic, HostplantPatry) %>%
  summarize(mean_BL = mean(BL))

# Create an interaction plot between the different host plants and the two regions
interaction_plot2 <- ggplot(tephritis, aes(x = Baltic, y = BL, color = HostplantPatry)) +
  geom_point(data = means1, aes(y = mean_BL), position = position_dodge(width = 0.5), size = 3) +
  geom_line(data = means1, aes(y = mean_BL, group = HostplantPatry), position = position_dodge(width = 0.5), size = 1) + 
  scale_color_manual(values = c("#4E5166","#2CA58D","#855A5C","#2874A6") , name = "Groups:",
                     labels = c("Heterophyllum Allopatry", "Heterophyllum Sympatry", "Oleraceum Allopatry", "Oleraceum Sympatry")) +
  theme_ipsum() +
  ylim(4, 5) +
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
    legend.key.size = unit(0.3, 'cm'),
    legend.position = c(0.9, 0.8)) +
  labs(x = "", y = "Body Length in mm") +
  ggtitle("Interaction Plot of body length of different groups of\nHostplants and Patrys by Baltic-region")


print(interaction_plot2)
# save the plots
ggsave("Interaction Plot2.png",interaction_plot2, width = 10, height = 10, units = "cm", dpi = 300)

# Calculate mean values
means2 <- tephritis %>%
  group_by(Hostplant, BalticPatry) %>%
  summarize(mean_BL = mean(BL))

# Create an interaction plot between the different host plants and the two regions
interaction_plot3 <- ggplot(tephritis, aes(x = Hostplant, y = BL, color = BalticPatry)) +
  geom_point(data = means2, aes(y = mean_BL), position = position_dodge(width = 0.5), size = 3) +
  geom_line(data = means2, aes(y = mean_BL, group = BalticPatry), position = position_dodge(width = 0.5), size = 1) + 
  scale_color_manual(values = c("#4E5166","#2CA58D","#855A5C","#2874A6") , name = "Groups:",
                     labels = c("East Allopatry", "West Sympatry", "East Sympatry", "West Allopatry")) +
  theme_ipsum() +
  ylim(4, 5) +
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
    legend.key.size = unit(0.3, 'cm'),
    legend.position = c(0.9, 0.8)) +
  labs(x = "", y = "Body Length in mm") +
  ggtitle("Interaction Plot of body length of different groups of\nRegions and Patrys by Hostplants")


print(interaction_plot3)
# save the plots
ggsave("Interaction Plot3.png",interaction_plot3, width = 10, height = 10, units = "cm", dpi = 300)
