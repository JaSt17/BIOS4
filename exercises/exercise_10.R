C = matrix(c(0.7, 0.2, -0.3,
             0.2, 1.2, 0.4,
             -0.3, 0.4, 0.6),
           nrow=3)
C
C==t(C)
# calculate the correlation matrix with the covariance matrix
corMatrix = matrix(C, nrow=3)
for (i in range(1, 3)){
    for (j in range(1, 3)){
        corMatrix[i, j] = C[i, j]/sqrt(C[i, i]*C[j, j])
    }
}
corMatrix
# multivariantdistribution
library(MASS)
library(ellipse)
set.seed(1)

# generate the multivariant distribution
X = data.frame(mvrnorm(200, mu=c(0,0,0), Sigma=C))
colnames(X) = c("z1", "z2", "z3")
head(X)
# plot the multivariant distribution
means = c(apply(X[,1:2], 2, mean))
plot(X$z1, X$z2, las=1)
lines(ellipse(cov(X[,1:2]), centre=means))

eigen(C)
# plot the eigenvectors
plot(X$z1, X$z2, las=1, col="grey")
lines(ellipse(cov(X[,1:2]), centre=means))
arrows(means[1], means[2],
       means[1]+eigen(C)$vectors[1,1],
       means[2]+eigen(C)$vectors[2,1],
       code=2, length=0.1, lwd=2)
arrows(means[1], means[2],
       means[1]+eigen(C)$vectors[1,2],
       means[2]+eigen(C)$vectors[2,2],
       code=2, length=0.1, lwd=2)
# proportion of varianve associated with each eigenvector of C
eigen(C)$values/sum(eigen(C)$values)
# Confirm that the eigenvectors are of unit lenght (lenght=1)
sqrt(sum(eigen(C)$vectors[,1]^2))
sqrt(sum(eigen(C)$vectors[,2]^2))
sqrt(sum(eigen(C)$vectors[,3]^2))
# calculate the angle between the eigenvectors
180/pi*acos(eigen(C)$vectors[,1]%*%eigen(C)$vectors[,2])
180/pi*acos(eigen(C)$vectors[,1]%*%eigen(C)$vectors[,3])
180/pi*acos(eigen(C)$vectors[,2]%*%eigen(C)$vectors[,3])
# reconstruct C by its eigenvalues and eigenvectors
# we can use the transpose of the eigenvectors 
# because they are orthogonal
# symmetric matrix, the eigenvalues are always real and eigenvectors 
# corresponding to distinct eigenvalues are always orthogonal
eigen(C)$vectors%*%diag(eigen(C)$values)%*%t(eigen(C)$vectors)

#Principal Component Analysis
dim(as.matrix(X))
dim(as.matrix(eigen(C)$vectors[,1]))

t1 = as.matrix(X) %*% eigen(C)$vectors[,1]
t2 = as.matrix(X) %*% eigen(C)$vectors[,2]
t3 = as.matrix(X) %*% eigen(C)$vectors[,3]
c(var(X[,1]), var(X[,2]), var(X[,3]))
c(var(t1), var(t2), var(t3))

# Do an actual PCA
pca = princomp(X)
summary(pca)
# proportional variance explained by each prinzipal component
pca$sdev**2/sum(pca$sdev**2)
# look at the loadings of each original variable on each principal component
pca$loadings[1:3, 1:3]
# A PCA illustarted by a biplot
biplot(pca, col=c("grey", "black"), cex=c(.5, 1))

# use PCA for an example from the lecure
# load the data
plant <- read.csv("~/BIOS4/data/alpineplants.csv")
# get rid of NA 
plant = na.omit(plant)
names(plants)
# create a new matrix with the data using only 3 coulmns
plantData = plant[, c("mean_T_winter", "mean_T_summer", "soil_moist")]
# do a PCA
plantPCA = princomp(plantData)
summary(plantPCA)
# proportional variance explained by each prinzipal component
plantPCA$sdev**2/sum(plantPCA$sdev**2)
# look at the loadings of each original variable on each principal component
plantPCA$loadings[1:3, 1:3]
# A PCA illustarted by a biplot
biplot(plantPCA, col=c("grey", "black"), cex=c(.5, 1))

# use principal component analysis to reduce the environmental variables
# into a reduced set of principal components
names(plants)
# create a new matrix with the enviromental variables
plantData2 = plant[, c("mean_T_winter", "mean_T_summer", "soil_moist", "snow",
                       "soil_moist", "light", "altitude")]
# do a PCA
plantPCA2 = princomp(plantData2)
summary(plantPCA2)
pc1 <- plantPCA2$scores[,1]
pc2 <- plantPCA2$scores[,2]
pc3 <- plantPCA2$scores[,3]
pc4 <- plantPCA2$scores[,4]
pc5 <- plantPCA2$scores[,5]
pc6 <- plantPCA2$scores[,6]
pc7 <- plantPCA2$scores[,7]
# make a model with all of the principal components
completePCAmodel = lm(plant$Carex.bigelowii ~ pc1 + pc2 + pc3 + pc4 + pc5 + pc6 + pc7)
summary(completePCAmodel)
Q = plantPCA2$loadings
b = as.matrix(summary(completePCAmodel)$coefficients[-1,1])
dim(Q)
dim(b)
Q%*%b
# get the variance explained by each principal component
variance_explained <- plantPCA2$sdev**2/sum(plantPCA2$sdev**2)
cumulative_variance <- cumsum(variance_explained)
plot(cumulative_variance, type = "b", pch = 16, xlab = "Number of Principal Components", ylab = "Cumulative Proportion of Variance Explained", main = "Cumulative Variance Plot")
# make a model with only the first 2 principal components
reducedPCAmodel = lm(plant$Carex.bigelowii ~ pc1+ pc2+ pc3)
summary(reducedPCAmodel)
plantPCA2$loadings[1:7, 1:7]
