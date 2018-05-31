# Agrupamento por hierarquia

iris2d=prcomp(iris[,-5])$x[,1:2] # PCA
ii=iris2d[1:20,]
dd=dist(ii) # Calcula as distâncias da matriz

# Complete link ou MAX
h.c=hclust(dd,"complete")
plot(h.c,main="Complete")
plot(h.c,main="Complete",hang=-1)

# Single link ou MIN
h.s=hclust(dd,"single")
plot(h.s,main="Single")

# Average
h.a=hclust(dd,"ave")
plot(h.a,main="Average")

# Ward's
h.w=hclust(dd,"ward")
plot(h.w,main="Ward")

# Plotar os dados iris2d
plot(iris2d)

# Matriz de distância
dd=dist(iris2d)
xx <- iris2d

# Plotando os clusters em cima dos pontos
c.s=cutree(hclust(dd,"single"),k=5)
plot(xx,col=c.s,main="Single")

c.c=cutree(hclust(dd,"complete"),k=5)
plot(xx,col=c.c,main="Complete")

c.a=cutree(hclust(dd,"ave"),k=5)
plot(xx,col=c.a,main="Average")

c.w=cutree(hclust(dd,"ward.D"),k=5)
plot(xx,col=c.w,main="ward")


# Agrupamento por densidade

#install.packages("dbscan")
library(dbscan)
kNNdist(iris2d, k=4)
kNNdistplot(iris2d, k=4)


# Variações do kmeans

#install.packages("flexclust")
# k-medians
library(flexclust)
clmedians <- kcca(iris[,1:4],k=3)

#install.packages("cluster")
# k-medoids
library(cluster)
clmedoid <- pam(iris[,1:4],k=3)

# Fuzzy c-means
#install.packages("e1071")
library(e1071)
clcmeans <- cmeans(iris[,1:4],3,m=2)

# Escolha do k
#install.packages("fpc")
library(fpc)

kmeansruns(iris[,1:4],criterion="asw",
           iter.max=100,runs=100,
           scaledata=FALSE,alpha=0.001,
           critout=FALSE,plot=FALSE)