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
PBRC=read.csv("PBRC Measurement Data.csv")
PBRChealth= read.csv("PBRC Data.csv")
Tanita=read.csv("Final Tanita ID.csv") #Column 'Tissue (% fat) - Total' for Body Fat measurement

#If I have to find BP through cuts
#PBRChealth$HighBP = PBRChealth$DBPAvg<=80 | PBRChealth$SBPAvg >= 120

#Joining all necessary measurements together
TanitaSimple= Tanita %>% select(SubjectID, Tissue....fat....Total)
PBRCtotal=left_join(PBRC,TanitaSimple, by="SubjectID")
PBRCtotal=left_join(PBRCtotal,PBRChealth,by="SubjectID")

#HighBPID=PBRChealth %>% filter(HighBP == TRUE) %>% select(SubjectID,SBPAvg, DBPAvg)

#PBRCtotal %>%
#  filter(HighBP==TRUE) %>% mutate(X0010.Body.Height=X0010.Body.Height/2.54) %>% mutate(X7021.Waist.to.hip.thigh.right=X7021.Waist.to.hip.thigh.right/2.54) %>%
#  select(SBPAvg, DBPAvg, X0010.Body.Height,X7021.Waist.to.hip.thigh.right) 

#looking for height (0010) to high waist (0085)

PBRCMutate=PBRCtotal %>%
  mutate(X7021.Waist.to.hip.thigh.right=X7021.Waist.to.hip.thigh.right/2.54) %>%
  mutate(X0010.Body.Height=X0010.Body.Height/2.54) %>%
  filter(!is.na(X7021.Waist.to.hip.thigh.right | X0010.Body.Height))

fjclean=fj %>%
  filter(!is.na(X7021.Waist.to.hip.thigh.right) & !is.na(X0010.Body.Height))

#Overlay of High Blood Pressure from PBRC to FJ
plotBP = fj %>% 
  select(X7021.Waist.to.hip.thigh.right,X0010.Body.Height) %>%
  filter(!is.na(X7021.Waist.to.hip.thigh.right | X0010.Body.Height)) %>%
  ggplot(aes(x=X7021.Waist.to.hip.thigh.right,y=X0010.Body.Height),color="FJ") + 
  coord_cartesian(xlim=c(11,35),ylim=c(56,80)) +
  geom_point(alpha=.3) +
  geom_point(data=PBRCMutate, aes(color="PBRC")) +
  geom_point(data=PBRCMutate %>% filter(High.Blood.Pressure==1), aes(color="High BP")) +
  scale_color_manual(name="Legend", values=c("FJ"="black","PBRC"="blue","High BP"="yellow"), limits=c("FJ","PBRC","High BP")) +
  labs(title="High Blood Pressure", x="Waist to Hip (inches)",y="Body Height (inches)") +
  theme(legend.position = c(.9,.15))
plotBP

#Overlay of BMI from PBRC to FJ
plotBMI = fj %>% 
  select(X7021.Waist.to.hip.thigh.right,X0010.Body.Height) %>%
  filter(!is.na(X7021.Waist.to.hip.thigh.right | X0010.Body.Height)) %>%
  ggplot(aes(x=X7021.Waist.to.hip.thigh.right,y=X0010.Body.Height, color="FJ")) + 
  coord_cartesian(xlim=c(11,35),ylim=c(56,80)) +
  geom_point(alpha=.3) +
  geom_point(data=PBRCMutate, aes(color="PBRC")) +
  geom_point(data=PBRCMutate %>% filter(BMI > 30), aes(color="BMI > 30")) +
  scale_color_manual(name="Legend", values=c("FJ"="black","PBRC"="blue","BMI > 30"="red"), limits=c("FJ","PBRC","BMI > 30")) +
  labs(title="BMI Over 30", x="Waist to Hip (inches)",y="Body Height (inches)") +
  theme(legend.position = c(.9,.15))
plotBMI

plotBMI2= PBRCMutate %>% 
  #select(X7021.Waist.to.hip.thigh.right,X0010.Body.Height) %>%
  filter(!is.na(X7021.Waist.to.hip.thigh.right | X0010.Body.Height)) %>%
  ggplot(aes(x=X7021.Waist.to.hip.thigh.right,y=X0010.Body.Height, fill=BMI)) + 
  coord_cartesian(xlim=c(11,35),ylim=c(56,80)) +
  geom_point(data=fj, fill="black",shape=16, color="black") +
  geom_point(shape=21, size=2.5) +
  scale_fill_gradient(low="white", high="red") +
  labs(title="BMI Overlay", x="Waist to Hip (inches)",y="Body Height (inches)", fill="BMI") +
  #scale_color_manual(name="Legend", values=c("Percent Fat"=Tissue....fat....Total), limits=c("Percent Fat")) +
  theme(legend.position = c(.9,.2))
plotBMI2

#Overlay of Tissue Fat % from PBRC to FJ
plotTan= PBRCMutate %>% 
  #select(X7021.Waist.to.hip.thigh.right,X0010.Body.Height) %>%
  filter(!is.na(X7021.Waist.to.hip.thigh.right | X0010.Body.Height)) %>%
  ggplot(aes(x=X7021.Waist.to.hip.thigh.right,y=X0010.Body.Height, fill=Tissue....fat....Total)) + 
  coord_cartesian(xlim=c(11,35),ylim=c(56,80)) +
  geom_point(data=fj, fill="black",shape=16, color="black") +
  geom_point(shape=21, size=2.5) +
  scale_fill_gradient(low="white", high="red") +
  labs(title="Percent Fat Overlay", x="Waist to Hip (inches)",y="Body Height (inches)", fill="Percent Fat") +
  #scale_color_manual(name="Legend", values=c("Percent Fat"=Tissue....fat....Total), limits=c("Percent Fat")) +
  theme(legend.position = c(.9,.2))
plotTan
  #geom_point(data=PBRCMutate %>% filter(BMI > 30), aes(color="BMI > 30")) +
  
  

PBRCMutate %>% filter(!is.na(Tissue....fat....Total)) %>% ggplot(aes(x=X7021.Waist.to.hip.thigh.right,y=X0010.Body.Height,color=Tissue....fat....Total)) +
  geom_point() + 

#Overlay of Resting Heart Rate > 75 from PBRC to FJ
plotRHR = fj %>% 
  select(X7021.Waist.to.hip.thigh.right,X0010.Body.Height) %>%
  filter(!is.na(X7021.Waist.to.hip.thigh.right | X0010.Body.Height)) %>%
  ggplot(aes(x=X7021.Waist.to.hip.thigh.right,y=X0010.Body.Height), color="FJ") + 
  coord_cartesian(xlim=c(11,35),ylim=c(56,80)) +
  geom_point(alpha=.3) +
  geom_point(data=PBRCMutate, aes(color="PBRC")) +
  geom_point(data=PBRCMutate %>% filter(RestingHR60 > 70), aes(color="Resting HR"), size=2) +
  scale_color_manual(name="Legend", values=c("FJ"="black","PBRC"="blue","Resting HR"="green"), limits=c("FJ","PBRC","Resting HR")) +
  labs(title="Resting Heart Rate Over 75 BPM", x="Waist to Hip (inches)",y="Body Height (inches)") +
  theme(legend.position = c(.9,.15))

plotRHR

#Combine all together and do a clustering
PBRCMutate$PBRC=1
fj$PBRC=0
fj$SubjectID=fj$scanID
alldata=full_join(fj,PBRCMutate)
alldata=alldata %>% filter(!is.na(X7021.Waist.to.hip.thigh.right) & !is.na(X0010.Body.Height))
gmm1=alldata %>%
  select(X0010.Body.Height,X7021.Waist.to.hip.thigh.right) %>%
  filter(!is.na(X7021.Waist.to.hip.thigh.right) & !is.na(X0010.Body.Height)) %>%
  Mclust(G=2)
alldata$Cluster=gmm1$classification
alldata$ProbC1=gmm1$z[,1]
alldata$ProbC2=gmm1$z[,2]
PBRCCluster=alldata %>% 
  filter(PBRC==1)
PBRCCluster %>% filter(Cluster==1) %>% write.csv(file="Right Cluster.csv",sep=",")
PBRCCluster %>% filter(Cluster==2) %>% write.csv(file="Left Cluster.csv",sep=",")


#Determine Statistical Differences between the two groups
t.test(PBRCCluster$Tissue....fat....Total[PBRCCluster$Cluster==1],PBRCCluster$Tissue....fat....Total[PBRCCluster$Cluster==2])
#P-value of .4873 for Body Fat %.  No significant difference

t.test(PBRCCluster$RestingHR60[PBRCCluster$Cluster==1],PBRCCluster$RestingHR60[PBRCCluster$Cluster==2])
#p-value of .3675 for Resting Heart Rate.  No difference

t.test(PBRCCluster$SBPAvg[PBRCCluster$Cluster==1],PBRCCluster$SBPAvg[PBRCCluster$Cluster==2])
#p-value of .5099 for SBPAvg.  no difference

t.test(PBRCCluster$DBPAvg[PBRCCluster$Cluster==1],PBRCCluster$DBPAvg[PBRCCluster$Cluster==2])
#p-value of .8322 for DBP Average.  No difference

#------------------------------------------------#
t.test(PBRCCluster$BMI[PBRCCluster$Cluster==1],PBRCCluster$BMI[PBRCCluster$Cluster==2])
#p-value of .003715 for BMI.  Significant difference in BMI between the two.
#------------------------------------------------#

t.test(PBRCCluster$Percentage.of.bodyfat..Siri.....[PBRCCluster$Cluster==1],PBRCCluster$Percentage.of.bodyfat..Siri.....[PBRCCluster$Cluster==2])
#p-value of .2285 for Siri bodyfat.  No difference

PBRCCluster %>% select(High.Blood.Pressure,SBPAvg,DBPAvg)

mean(PBRCCluster$High.Blood.Pressure[PBRCCluster$Cluster==1])
sum(PBRCCluster$High.Blood.Pressure[PBRCCluster$Cluster==2])


PBRCCluster %>% ggplot(aes(x=X7021.Waist.to.hip.thigh.right,y=X0010.Body.Height,color=Cluster))+
  geom_point()

totclust=alldata %>% 
  select(X0010.Body.Height,X7021.Waist.to.hip.thigh.right)
totclust=totclust[complete.cases(totclust),] 
totclust = totclust %>% as.matrix() %>%
  matrix(ncol=ncol(totclust)) 

#or try to do a mixture model
gmm1=fj %>%
  select(X0010.Body.Height,X7021.Waist.to.hip.thigh.right) %>%
  filter(!is.na(X7021.Waist.to.hip.thigh.right) & !is.na(X0010.Body.Height)) %>%
  Mclust(G=2)

gmm3=fj %>%
  select(X0010.Body.Height,X7021.Waist.to.hip.thigh.right) %>%
  filter(!is.na(X7021.Waist.to.hip.thigh.right) & !is.na(X0010.Body.Height)) %>%
  Mclust(G=3)

fjclean$Cluster3=gmm3$classification

#plot cluster of 3
fjclean %>%
  ggplot(aes(x=X7021.Waist.to.hip.thigh.right,y=X0010.Body.Height,color=Cluster3)) +
  geom_point()

#+ stat_density2d(aes(alpha=..density..), geom="raster", contour = FALSE)

plotpbrc = PBRC %>%
  select(X7021.Waist.to.hip.thigh.right,X0010.Body.Height) %>%
  mutate(X7021.Waist.to.hip.thigh.right=X7021.Waist.to.hip.thigh.right/2.54) %>%
  mutate(X0010.Body.Height=X0010.Body.Height/2.54) %>%
  filter(!is.na(X7021.Waist.to.hip.thigh.right | X0010.Body.Height)) %>%
  ggplot(aes(x=X7021.Waist.to.hip.thigh.right,y=X0010.Body.Height)) + geom_point( color="blue") + 
  coord_cartesian(xlim=c(11,35),ylim=c(56,80))

plotfj + plotpbrc
#plot(fj$X0010.Body.Height~fj$X0085.High.waist.height)
#plot(fj$X5070.Waist.to.high.hip.back,fj$X0010.Body.Height)

plot(fj$X7021.Waist.to.hip.thigh.right,fj$X0010.Body.Height, xlim=c(10,30),ylim=c(55,80))
#Definite Cluster to Right
hipheight=data.frame(fj$X7021.Waist.to.hip.thigh.right,fj$X0010.Body.Height)
hipheight=hipheight[complete.cases(hipheight),]
hipheight=as.matrix(hipheight)
hipheight=matrix(hipheight,ncol=ncol(hipheight))
hipheightscale=scale(hipheight) 

PBRChipheight=data.frame(PBRC$X7021.Waist.to.hip.thigh.right/2.54,PBRC$X0010.Body.Height/2.54,PBRChealth$HighBP)
PBRChipheight=PBRChipheight[complete.cases(PBRChipheight),]
PBRChipheight=as.matrix(PBRChipheight)
PBRChipheight=matrix(PBRChipheight,ncol=ncol(PBRChipheight))
PBRChipheightscale=PBRChipheight
PBRChipheightscale[,1:2]=scale(PBRChipheightscale[,1:2]) 



#hdbscan method
c1=dbscan(hipheightscale, eps=.20,minPts=40)
plot(hipheight,col=c1$cluster+1,pch=20, xlab="Waist to Hip Thigh Right (inches)",ylab="Height (inches)", main="HDBScan Clustering")
points(PBRChipheight[,1],PBRChipheight[,2], col="blue", pch=20)
points(PBRChipheight[,1][PBRChipheight[,3]==1],PBRChipheight[,2][PBRChipheight[,3]==1], col = "black", bg="yellow", pch=21)
legend("bottomright", legend = c("Long Legs, Short Torso","Short Legs, Long Torso","Overlay of Pennington Data","High BP in Pennington Data"), pch=c(20,20,20,21),pt.bg=c("red","green","blue","yellow"),col=c("red","green","blue","black"))

c2=dbscan(hipheight, eps=.15,minPts=50)
plot(hipheight,col=c2$cluster+1,pch=20, xlab="Waist to Hip Thigh Right (Standardized)",ylab="Height (Standardized)", main="HDBScan Clustering")

#agglomerative clustering ward method
dhipheight=dist(hipheight, method="euclidean")
c3=hclust(dhipheight, method="ward.D2")
plot(c3)
groups=cutree(c3,k=2)
plot(hipheight,col=groups,pch=20, xlab="Waist to Hip Thigh Right",ylab="Height", main = "Hierarchical Clustering Ward Method")

#hierarchical cluster 
c4=hclust(hipheight, method="centroid")
groups2=cutree(c4,k=3)
plot(hipheight,col=groups2,pch=20, xlab="Waist to Hip Thigh Right",ylab="Height", main="Hierarchical Clustering Centroid Method")

#kmeans clustering
c5 = kmeans(hipheight, 2)
plot(hipheight,col=c5$cluster+1,pch=20, xlab="Waist to Hip Thigh Right",ylab="Height", main = "KMeans Clustering")

#spectral clustering

c6 = specc(hipheight, 2)
plot



plot(fj$X7530.Hip.thigh.girth,fj$X0010.Body.Height)

?hclust
