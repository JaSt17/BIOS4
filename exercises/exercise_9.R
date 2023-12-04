# install packages:
library(lavaan)
library(semPlot)
library(piecewiseSEM)
# load data:
plants <- read.csv("~/BIOS4/data/alpineplants.csv")
# adjust the data:
plants = na.omit(plants)
plants = as.data.frame(scale(plants))
round(colMeans(plants), 2)
round(apply(plants, 2, sd), 2)
# fit simple model:
m1 = lm(Carex.bigelowii ~ snow + min_T_winter + soil_moist, data=plants)
summary(m1)

# Extract coefficients
coefficients <- coef(m1)

# fit a third model
m2 = psem(lm(soil_moist~snow, data=plants),
          lm(min_T_winter~snow, data=plants),
          lm(Carex.bigelowii~min_T_winter+soil_moist, data=plants),
          data=plants)
summary(m2)
plot(m2)
m2b = psem(lm(soil_moist~snow, data=plants),
           lm(min_T_winter~snow, data=plants),
           lm(Carex.bigelowii~min_T_winter+soil_moist, data=plants),
           min_T_winter %~~% soil_moist,
           data=plants)
summary(m2b)
plot(m2b)
# plot the third model
m3 = psem(soil_moist~1,
          lm(min_T_winter~snow, data=plants),
          lm(Carex.bigelowii~min_T_winter, data=plants),
          min_T_winter %~~% soil_moist,
          data=plants)
summary(m3)
plot(m3)
# caompare the AICS
AIC(m2b, m3)

# repeate the analysis with the data fromt the other plant species
# fit simple model:
m1 = lm(Thalictrum.alpinum ~ snow + min_T_winter + soil_moist, data=plants)
summary(m1)

# Extract coefficients
coefficients <- coef(m1)
coefficients

# fit a third model
m2 = psem(lm(soil_moist~snow, data=plants),
          lm(min_T_winter~snow, data=plants),
          lm(Thalictrum.alpinum~min_T_winter+soil_moist, data=plants),
          data=plants)
summary(m2)
plot(m2)
m2b = psem(lm(soil_moist~snow, data=plants),
           lm(min_T_winter~snow, data=plants),
           lm(Thalictrum.alpinum~min_T_winter+soil_moist, data=plants),
           min_T_winter %~~% soil_moist,
           data=plants)
summary(m2b)
plot(m2b)
# plot the third model
m3 = psem(soil_moist~1,
          lm(min_T_winter~snow, data=plants),
          lm(Thalictrum.alpinum~min_T_winter, data=plants),
          min_T_winter %~~% soil_moist,
          data=plants)
summary(m3)
plot(m3)
# caompare the AICS
AIC(m2b, m3)
