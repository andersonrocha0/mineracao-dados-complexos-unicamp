# Prova

library(DMwR)
minmax.scale <- function(x) (x -min(x)) / (max(x) - min(x))
set.seed(1234)
df <- iris[, 1:4]
scores1 <- lofactor(df, k=5)
r <- kmeans(df, 3)
centers <- r$centers[r$cluster, ]
distances <- sqrt(rowSums((df - centers) ^2))
mcr <- ave(distances, r$cluster, FUN = mean)
scores2 <- distances / mcr
scores1 <- minmax.scale(scores1)
scores2 <- minmax.scale(scores2)
outliers1 <- order(scores1, decreasing = T)[1:5]
outliers2 <- order(scores2, decreasing = T)[1:5]
scores3 <- (scores1 + scores2) / 2
outliers3 <- order(scores3, decreasing = T)[1:10]
