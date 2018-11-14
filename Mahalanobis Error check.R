library(stats)
library(mclust)
library(dbscan)
library(ggplot2)
library(graphics)
library(cluster)
library(kernlab)
library(igraph)
library(tidyr)
library(dplyr)
library(gmm)

setwd("C:/Users/James.Pleuss/Documents/Research/Obesity/Fort Jackson Pennington")
fj=read.csv("FJ Total Dataset.csv")

fjclean=fj %>%
  filter(!is.na(X7021.Waist.to.hip.thigh.right) & !is.na(X0010.Body.Height))

####   PCA for Fort Jackson   ###
fjcleandata = fjclean[,9:168] %>%
  select(-X1530.Head.Girth) %>% select( - X2030.Torso.width.at.waist) %>%
  filter(complete.cases(.))

fjcleandatacov=cov(fjcleandata)
fjcleanmeans=0
for (i in 1:length(fjcleandata)){
  fjcleanmeans=mean(fjcleandata[,i])
}

fjpca=prcomp(~., data =fjcleandata, center=TRUE, scale=TRUE)

PCA1=fjpca$rotation[,1]
PCA2=fjpca$rotation[,2]
PCA3=fjpca$rotation[,3]
PCA4=fjpca$rotation[,4]
PCA5=fjpca$rotation[,5]
fjPCA5=data.frame(PCA1, PCA2, PCA3,PCA4,PCA5)


####   Clustering off of PCA ####
fjpcapredict=predict(fjpca,newdata=fjcleandata)
fjpcapredict5=fjpcapredict[,1:5]

k4=kmeans(fjpcapredict5, centers=4,nstart=100)
fjcleandata$pca5k4=k4$cluster


## Determine Mahalanobis distance##
fjcleandata %>% 
  select(-pca5k4) %>% 
  filter(fjcleandata$pca5k4==1) %>% 
  as.matrix() %>% mahalanobis(center=fjcleanmeans,cov=fjcleandatacov)

