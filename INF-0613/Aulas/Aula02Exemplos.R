## PCA


rm(list = ls())
library(datasets)
data("iris")
summary(iris)

iris.pca1 <- prcomp(iris[, 1:4], scale = T)
iris.pca1

summary(iris.pca1)

iris.pca2 <- princomp(iris[, 1:4], cor = T)
iris.pca2

summary(iris.pca2)

head(iris.pca1$x[, 1:2])


# K-means

cl <- kmeans(iris[,1:4], 3)
clpca1 <- kmeans(iris.pca1$x[, 1:2], 3)

cl <- kmeans(iris[,1:4],3, nstart = 1000)
clpca1 <- kmeans(iris.pca1$x[, 1:2], 3, nstart = 1000)

# install.packages("ggfortify")

library(ggfortify)
library(cluster)

autoplot(cl, data = iris[, 1:4])
autoplot(clpca1, data = iris.pca1$x[, 1:2])

autoplot(fanny(iris.pca1$x[, 1:2], 3), frame= T)