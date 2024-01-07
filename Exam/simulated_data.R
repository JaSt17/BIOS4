#simulate data
newAnnualPrecipitation <- seq(min(data$annual_precipitation),
                              max(data$annual_precipitation),
                              length.out=200)
new_MrVBF <- rep(mean(data$MrVBF), length(newAnnualPrecipitation))
newK_perc <- rep(mean(data$K_perc), length(newAnnualPrecipitation))
newK_perc_plus_SD <- rep(mean(data$K_perc)+ sd(data$K_perc), length(newAnnualPrecipitation))
newK_perc_minus_SD <- rep(mean(data$K_perc)- sd(data$K_perc), length(newAnnualPrecipitation))
newPrecipitation_warmest_quarter <- rep(mean(data$precipitation_warmest_quarter), length(newAnnualPrecipitation))
newPrecipitation_coldest_quarter <- rep(mean(data$precipitation_coldest_quarter), length(newAnnualPrecipitation))
simulatedData1 <- predict(m4, newdata=list(annual_precipitation=newAnnualPrecipitation,
                                           precipitation_warmest_quarter = newPrecipitation_warmest_quarter,
                                           precipitation_coldest_quarter = newPrecipitation_coldest_quarter,
                                           MrVBF = new_MrVBF,
                                           K_perc = newK_perc),type="response")
simulatedData2 <- predict(m4, newdata=list(annual_precipitation=newAnnualPrecipitation,
                                           precipitation_warmest_quarter = newPrecipitation_warmest_quarter,
                                           precipitation_coldest_quarter = newPrecipitation_coldest_quarter,
                                           MrVBF = new_MrVBF,
                                           K_perc = newK_perc_plus_SD),type="response")
simulatedData3 <- predict(m4, newdata=list(annual_precipitation=newAnnualPrecipitation,
                                           precipitation_warmest_quarter = newPrecipitation_warmest_quarter,
                                           precipitation_coldest_quarter = newPrecipitation_coldest_quarter,
                                           MrVBF = new_MrVBF,
                                           K_perc = newK_perc_minus_SD),type="response")
newdata1 = data.frame(forest.=newAnnualPrecipitation,
                      y_hat=simulatedData1,
                      y_hat2=simulatedData2,
                      y_hat3=simulatedData3)
# plot the data for forest and MAP
plot <- ggplot() +
  geom_point(data = data, aes(x = K_perc, y = NativePlant_cover), size = 1.5, shape = 1) +
  scale_color_manual(name = "", values = c("MAP = Mean" = "black", "MAP = Mean - SD" = "lightblue", "MAP = Mean + SD" = "darkblue")) +
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
  ggtitle("") +
  xlab("annual precipitation") +
  ylab("nativ cover in percent")
plot