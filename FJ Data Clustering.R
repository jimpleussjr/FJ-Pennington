library(stats)
library(mclust)
library(dbscan)
library(ggplot2)
library(graphics)
library(ClusterR)
library(kernlab)
library(igraph)
library(tidyr)
library(dplyr)
library(gmm)
library(caret)  # for pre processing the 
library(compare)
library(flexclust)
library(rgl)
library(car)
library(kableExtra)
library(formattable)


#setwd("C:/Users/James.Pleuss/Documents/Research/Obesity/Fort Jackson Pennington")


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
setdiff(names(fjclean),names(PBRCclean))
#PBRCtotal %>%
#  filter(HighBP==TRUE) %>% mutate(X0010.Body.Height=X0010.Body.Height/2.54) %>% mutate(X7021.Waist.to.hip.thigh.right=X7021.Waist.to.hip.thigh.right/2.54) %>%
#  select(SBPAvg, DBPAvg, X0010.Body.Height,X7021.Waist.to.hip.thigh.right) 

#looking for height (0010) to high waist (0085)

PBRCMutate=PBRCtotal %>%
  mutate(X7021.Waist.to.hip.thigh.right=X7021.Waist.to.hip.thigh.right/2.54) %>%
  mutate(X0010.Body.Height=X0010.Body.Height/2.54) %>%
  filter(!is.na(X7021.Waist.to.hip.thigh.right | X0010.Body.Height))

#### Preprocessing of data ####
## Pennington does not have X8039.Armlength.max, X9019.Inseam.to.Ankle, X9029.Inseam.to.Foot, X9039.Sideseam.at.waist.max, X9049.Sideseam.leg-ankle, X9560.Neck.to.buttock.center.back
###  Also need to transform Pennington data to the units of Fort Jackson
PBRCclean=PBRC %>% select(-X.,-DOB.Year,-AgeAtEnrollment,-Gender,-Race,-RaceOther,-Ethnicity,-ID,-MDATE,-Percentage.of.bodyfat..Siri.....,-BMI.BMI)
PBRCclean[,c(2:46,48:60,63:153)]=PBRCclean[,c(2:46,48:60,63:153)]/2.54
PBRCclean[,154]=PBRCclean[,154]*2.205
fjclean = fj %>% select(-X8039.Armlength.max,-X9019.Inseam.to.Ankle,-X9029.Inseam.to.Foot,-X9039.Sideseam.at.waist.max,-X9049.Sideseam.leg.ankle, -X9560.Neck.to.buttock.center.back)
fjclean=fjclean %>% select(.,-X0996.Upper.torso.torsion, -X3911.Shoulder.angle.right, -X3910.Shoulder.angle.left)
fjclean$Gender=ifelse(fjclean$gender=="M",0,1)

setdiff(names(fjclean),names(PBRCclean))
### Eliminate X0996.Upper.torso.torsion and X3910.Shoulder.Angle.Right and X3911.Shoulder.Angle.Left due to differing units of measure ####
PBRCclean=PBRCclean %>% select(.,-X0996.Upper.torso.torsion, -X3911.Shoulder.angle.right, -X3910.Shoulder.angle.left)
PBRCclean$Gender=ifelse(PBRC$Gender=="Male",0,1)
fjclean = fjclean %>%
  #select(-platoon, -LastName, -Firstname,-First.Name.Only,-Name,-datecreated,-armlength_cv.Armlength_CV)
  select(-platoon, -scanID, -LastName, -Firstname,-First.Name.Only,-Name,-datecreated,-gender,-armlength_cv.Armlength_CV,-BMI.BMI,-CHAP,-Med.Board)
  #select(-X0640.waistband.front.to.vertical, -X0645.waistband.back.to.vertical, $-X2030.Torso.width.at.waist, -X0690.waistband.front.height, -X0695.waistband.back.height,-X0990.3D.waistband.left.to.crotch, -X6015.Crotch.length.at.waistband, $-X0165.scapula.height, -X0090.Buttock.height, -X6520.Waist.Band, -X9021.Inseam.right, -X9030.Sideseam.left, -X9031.Sideseam.right, -X9036.Sideseam.at.waist.right, -X9041.Sideseam.ankle.right, -X0110.Knee.height, -X0620.Distance.belly.to.vertical, -X0995.3D.waistband.right.to.crotch, -X4050.Neck.front.to.waist, -X7015.Waistband.to.buttock.height.left, -X7540.Belly.circumference, -X7545.Maximum.belly.circumference, -X9010.Inside.leg.ankle.left, -X9011.Inside.leg.ankle.right, -X9032.sideseam.3D.waistband.left, -X9033.sideseam.3D.waistband.right, -X9035.Sideseam.at.waist.left, -X9040.Sideseam.ankle.left, -X3911.Shoulder.angle.right, -X1010.Neck.diameter, -X4010.Across.front.width, -X5030.Neck.to.across.back.width.armpit.level, -X5040.Neck.to.waist.center.back, -X5070.Waist.to.high.hip.back, -X7010.Waist.to.buttock.height.left, -X9511.Thigh.girth.right, -X9540.calf.girth.left) %>%
fjclean= fjclean %>%  
  select(-X0640.waistband.front.to.vertical, -X0645.waistband.back.to.vertical, -X0690.waistband.front.height, -X0695.waistband.back.height,-X0990.3D.waistband.left.to.crotch, -X6015.Crotch.length.at.waistband, -X0600.Distance.breast.to.vertical ,-X2030.Torso.width.at.waist, -X0510.Distance.7CV...vertical, -X4510.Bust.chest.girth.horizontal, -X5070.Waist.to.high.hip.back, -X9041.Sideseam.ankle.right, -X9010.Inside.leg.ankle.left) %>%
  filter(complete.cases(.))
#fjcleanscale=as.data.frame(scale(fjcleandata))
#fjcleanscale=preProcess(fjclean)
#fjcleandatascale=predict(fjcleanscale,fjclean)
#PBRCcleandatascale=predict(fjcleanscale,PBRCcleandata)
fjclean = fjclean %>% filter(X9800.Weight>70)


PBRCclean = PBRCclean %>%
  #select(-X0640.waistband.front.to.vertical, -X0645.waistband.back.to.vertical, $-X2030.Torso.width.at.waist, -X0690.waistband.front.height, -X0695.waistband.back.height,-X0990.3D.waistband.left.to.crotch, -X6015.Crotch.length.at.waistband, $-X0165.scapula.height, -X0090.Buttock.height, -X6520.Waist.Band, -X9021.Inseam.right, -X9030.Sideseam.left, -X9031.Sideseam.right, -X9036.Sideseam.at.waist.right, -X9041.Sideseam.ankle.right, -X0110.Knee.height, -X0620.Distance.belly.to.vertical, -X0995.3D.waistband.right.to.crotch, -X4050.Neck.front.to.waist, -X7015.Waistband.to.buttock.height.left, -X7540.Belly.circumference, -X7545.Maximum.belly.circumference, -X9010.Inside.leg.ankle.left, -X9011.Inside.leg.ankle.right, -X9032.sideseam.3D.waistband.left, -X9033.sideseam.3D.waistband.right, -X9035.Sideseam.at.waist.left, -X9040.Sideseam.ankle.left, -X3911.Shoulder.angle.right, -X1010.Neck.diameter, -X4010.Across.front.width, -X5030.Neck.to.across.back.width.armpit.level, -X5040.Neck.to.waist.center.back, -X5070.Waist.to.high.hip.back, -X7010.Waist.to.buttock.height.left, -X9511.Thigh.girth.right, -X9540.calf.girth.left) %>%
  select(-X0640.waistband.front.to.vertical, -X0645.waistband.back.to.vertical, -X0690.waistband.front.height, -X0695.waistband.back.height,-X0990.3D.waistband.left.to.crotch, -X6015.Crotch.length.at.waistband, -X0600.Distance.breast.to.vertical ,-X2030.Torso.width.at.waist, -X0510.Distance.7CV...vertical, -X4510.Bust.chest.girth.horizontal, -X5070.Waist.to.high.hip.back, -X9041.Sideseam.ankle.right, -X9010.Inside.leg.ankle.left) %>%
  filter(complete.cases(.)) 

### Eliminate observation with weight outside of FJ data
PBRCclean = PBRCclean %>% filter(X9800.Weight >84.9)

PBRCcleandata=PBRCclean %>%
  select(-SubjectID)
### We now have a dataset for each of our groups that has the same variables.
setdiff(names(fjclean),names(PBRCcleandata))


# 
# #Overlay of High Blood Pressure from PBRC to FJ
# plotBP = fj %>% 
#   select(X7021.Waist.to.hip.thigh.right,X0010.Body.Height) %>%
#   filter(!is.na(X7021.Waist.to.hip.thigh.right | X0010.Body.Height)) %>%
#   ggplot(aes(x=X7021.Waist.to.hip.thigh.right,y=X0010.Body.Height),color="FJ") + 
#   coord_cartesian(xlim=c(11,35),ylim=c(56,80)) +
#   geom_point(alpha=.3) +
#   geom_point(data=PBRCMutate, aes(color="PBRC")) +
#   geom_point(data=PBRCMutate %>% filter(High.Blood.Pressure==1), aes(color="High BP")) +
#   scale_color_manual(name="Legend", values=c("FJ"="black","PBRC"="blue","High BP"="yellow"), limits=c("FJ","PBRC","High BP")) +
#   labs(title="High Blood Pressure", x="Waist to Hip (inches)",y="Body Height (inches)") +
#   theme(legend.position = c(.9,.15))
# plotBP
# 
# #Overlay of BMI from PBRC to FJ
# plotBMI = fj %>% 
#   select(X7021.Waist.to.hip.thigh.right,X0010.Body.Height) %>%
#   filter(!is.na(X7021.Waist.to.hip.thigh.right | X0010.Body.Height)) %>%
#   ggplot(aes(x=X7021.Waist.to.hip.thigh.right,y=X0010.Body.Height, color="FJ")) + 
#   coord_cartesian(xlim=c(11,35),ylim=c(56,80)) +
#   geom_point(alpha=.3) +
#   geom_point(data=PBRCMutate, aes(color="PBRC")) +
#   geom_point(data=PBRCMutate %>% filter(BMI > 30), aes(color="BMI > 30")) +
#   scale_color_manual(name="Legend", values=c("FJ"="black","PBRC"="blue","BMI > 30"="red"), limits=c("FJ","PBRC","BMI > 30")) +
#   labs(title="BMI Over 30", x="Waist to Hip (inches)",y="Body Height (inches)") +
#   theme(legend.position = c(.9,.15))
# plotBMI
# 
# plotBMI2= PBRCMutate %>% 
#   #select(X7021.Waist.to.hip.thigh.right,X0010.Body.Height) %>%
#   filter(!is.na(X7021.Waist.to.hip.thigh.right | X0010.Body.Height)) %>%
#   ggplot(aes(x=X7021.Waist.to.hip.thigh.right,y=X0010.Body.Height, fill=BMI)) + 
#   coord_cartesian(xlim=c(11,35),ylim=c(56,80)) +
#   geom_point(data=fj, fill="black",shape=16, color="black") +
#   geom_point(shape=21, size=2.5) +
#   scale_fill_gradient(low="white", high="red") +
#   labs(title="BMI Overlay", x="Waist to Hip (inches)",y="Body Height (inches)", fill="BMI") +
#   #scale_color_manual(name="Legend", values=c("Percent Fat"=Tissue....fat....Total), limits=c("Percent Fat")) +
#   theme(legend.position = c(.9,.2))
# plotBMI2
# 
# #Overlay of Tissue Fat % from PBRC to FJ
# plotTan= PBRCMutate %>% 
#   #select(X7021.Waist.to.hip.thigh.right,X0010.Body.Height) %>%
#   filter(!is.na(X7021.Waist.to.hip.thigh.right | X0010.Body.Height)) %>%
#   ggplot(aes(x=X7021.Waist.to.hip.thigh.right,y=X0010.Body.Height, fill=Tissue....fat....Total)) + 
#   coord_cartesian(xlim=c(11,35),ylim=c(56,80)) +
#   geom_point(data=fj, fill="black",shape=16, color="black") +
#   geom_point(shape=21, size=2.5) +
#   scale_fill_gradient(low="white", high="red") +
#   labs(title="Percent Fat Overlay", x="Waist to Hip (inches)",y="Body Height (inches)", fill="Percent Fat") +
#   #scale_color_manual(name="Legend", values=c("Percent Fat"=Tissue....fat....Total), limits=c("Percent Fat")) +
#   theme(legend.position = c(.9,.2))
# plotTan
#   #geom_point(data=PBRCMutate %>% filter(BMI > 30), aes(color="BMI > 30")) +
#   
#   
# 
# PBRCMutate %>% filter(!is.na(Tissue....fat....Total)) %>% ggplot(aes(x=X7021.Waist.to.hip.thigh.right,y=X0010.Body.Height,color=Tissue....fat....Total)) +
#   geom_point() 
# 
# #Overlay of Resting Heart Rate > 75 from PBRC to FJ
# plotRHR = fj %>% 
#   select(X7021.Waist.to.hip.thigh.right,X0010.Body.Height) %>%
#   filter(!is.na(X7021.Waist.to.hip.thigh.right | X0010.Body.Height)) %>%
#   ggplot(aes(x=X7021.Waist.to.hip.thigh.right,y=X0010.Body.Height), color="FJ") + 
#   coord_cartesian(xlim=c(11,35),ylim=c(56,80)) +
#   geom_point(alpha=.3) +
#   geom_point(data=PBRCMutate, aes(color="PBRC")) +
#   geom_point(data=PBRCMutate %>% filter(RestingHR60 > 70), aes(color="Resting HR"), size=2) +
#   scale_color_manual(name="Legend", values=c("FJ"="black","PBRC"="blue","Resting HR"="green"), limits=c("FJ","PBRC","Resting HR")) +
#   labs(title="Resting Heart Rate Over 75 BPM", x="Waist to Hip (inches)",y="Body Height (inches)") +
#   theme(legend.position = c(.9,.15))
# 
# plotRHR
# 
# #Combine all together and do a clustering
# PBRCMutate$PBRC=1
# fj$PBRC=0
# fj$SubjectID=fj$scanID
# alldata=full_join(fj,PBRCMutate)
# alldata=alldata %>% filter(!is.na(X7021.Waist.to.hip.thigh.right) & !is.na(X0010.Body.Height))
# gmm1=alldata %>%
#   select(X0010.Body.Height,X7021.Waist.to.hip.thigh.right) %>%
#   filter(!is.na(X7021.Waist.to.hip.thigh.right) & !is.na(X0010.Body.Height)) %>%
#   Mclust(G=2)
# alldata$Cluster=gmm1$classification
# alldata$ProbC1=gmm1$z[,1]
# alldata$ProbC2=gmm1$z[,2]
# PBRCCluster=alldata %>% 
#   filter(PBRC==1)
# PBRCCluster %>% filter(Cluster==1) %>% write.csv(file="Right Cluster.csv",sep=",")
# PBRCCluster %>% filter(Cluster==2) %>% write.csv(file="Left Cluster.csv",sep=",")
# 
# #GMM Clustering on FJ only
# gmm2=fjclean %>%
#   select(X0010.Body.Height,X7021.Waist.to.hip.thigh.right) %>%
#   Mclust(G=2)
# fjclean$Cluster=gmm2$classification
# fjclean$ProbC1=gmm2$z[,1]
# fjclean$ProbC2=gmm2$z[,2]
# 
# #### prop test of BMI in Fort Jackson Data  ####
# 
# prop.test(c(length(fjclean$BMI[fjclean$Cluster==1 & fjclean$BMI>=25]), length(fjclean$BMI[fjclean$Cluster==2 & fjclean$BMI>=25])), c(length(fjclean$BMI[fjclean$Cluster==1]), length(fjclean$BMI[fjclean$Cluster==2])))
# 
# # Proportion of Males in Cluster 1 for FJ(right cluster)
# length(fjclean$gender[fjclean$gender=="M" & fjclean$Cluster==1])/length(fjclean$gender[fjclean$Cluster==1])
# #7293
# 
# # Proportion of Males in Cluster 2 for FJ(left cluster)
# length(fjclean$gender[fjclean$gender=="M" & fjclean$Cluster==2])/length(fjclean$gender[fjclean$Cluster==2])
# #.7289
# 
# #####         Tests for Pennington Data #####
# 
# #Determine Statistical Differences between the two groups
# t.test(PBRCCluster$Tissue....fat....Total[PBRCCluster$Cluster==1],PBRCCluster$Tissue....fat....Total[PBRCCluster$Cluster==2])
# #P-value of .4873 for Body Fat %.  No significant difference
# 
# t.test(PBRCCluster$RestingHR60[PBRCCluster$Cluster==1],PBRCCluster$RestingHR60[PBRCCluster$Cluster==2])
# #p-value of .3675 for Resting Heart Rate.  No difference
# 
# t.test(PBRCCluster$SBPAvg[PBRCCluster$Cluster==1],PBRCCluster$SBPAvg[PBRCCluster$Cluster==2])
# #p-value of .5099 for SBPAvg.  no difference
# 
# t.test(PBRCCluster$DBPAvg[PBRCCluster$Cluster==1],PBRCCluster$DBPAvg[PBRCCluster$Cluster==2])
# #p-value of .8322 for DBP Average.  No difference
# 
# #------------------------------------------------#
# t.test(PBRCCluster$BMI[PBRCCluster$Cluster==1],PBRCCluster$BMI[PBRCCluster$Cluster==2])
# #p-value of .003715 for BMI.  Significant difference in BMI between the two.  (Cluster 2 (left cluster) has significantly higher BMI)
# #------------------------------------------------#
# 
# t.test(PBRCCluster$Percentage.of.bodyfat..Siri.....[PBRCCluster$Cluster==1],PBRCCluster$Percentage.of.bodyfat..Siri.....[PBRCCluster$Cluster==2])
# #p-value of .2285 for Siri bodyfat.  No difference
# 
# 
# ##Proportion Tests###
# 
# # Proportion Test for High Blood Pressure###
# prop.test(c(length(PBRCCluster$High.Blood.Pressure[PBRCCluster$Cluster==1 & PBRCCluster$High.Blood.Pressure==1]), length(PBRCCluster$High.Blood.Pressure[PBRCCluster$Cluster==2 & PBRCCluster$High.Blood.Pressure==1])), c(length(PBRCCluster$High.Blood.Pressure[PBRCCluster$Cluster==1]), length(PBRCCluster$High.Blood.Pressure[PBRCCluster$Cluster==2])))
# ### P-value of .7495
# 
# # Proportion Test for BMI >= 30 (obese)###
# prop.test(c(length(PBRCCluster$BMI[PBRCCluster$Cluster==1 & PBRCCluster$BMI>=30]), length(PBRCCluster$BMI[PBRCCluster$Cluster==2 & PBRCCluster$BMI>=30])), c(length(PBRCCluster$BMI[PBRCCluster$Cluster==1]), length(PBRCCluster$BMI[PBRCCluster$Cluster==2])))
# ### P-value of .009146.  Cluster 2 (left cluster) with significantly higher proportion of obese people.
# 
# # Proportion Test for BMI >= 25 in Males (overweight or obese)###
# prop.test(c(length(PBRCCluster$BMI[PBRCCluster$Cluster==1 & PBRCCluster$BMI>=25 & PBRCCluster$Gender.x=="Male"] ), length(PBRCCluster$BMI[PBRCCluster$Cluster==2 & PBRCCluster$BMI>=25 & PBRCCluster$Gender.x=="Male"])), c(length(PBRCCluster$BMI[PBRCCluster$Cluster==1 & PBRCCluster$Gender.x=="Male"]), length(PBRCCluster$BMI[PBRCCluster$Cluster==2 & PBRCCluster$Gender.x=="Male"])), alternative="less")
#   ### P-value of .02617  Cluster 2 (left cluster) with higher proportion of obese people. Did less than test to check if it holds true.
# 
# # Proportion Test for BMI >= 25 in Males (overweight or obese)###
# prop.test(c(length(PBRCCluster$BMI[PBRCCluster$Cluster==1 & PBRCCluster$BMI>=25 & PBRCCluster$Gender.x=="Female"] ), length(PBRCCluster$BMI[PBRCCluster$Cluster==2 & PBRCCluster$BMI>=25 & PBRCCluster$Gender.x=="Female"])), c(length(PBRCCluster$BMI[PBRCCluster$Cluster==1 & PBRCCluster$Gender.x=="Female"]), length(PBRCCluster$BMI[PBRCCluster$Cluster==2 & PBRCCluster$Gender.x=="Female"])), alternative="less")
# ### P-value of .03044  Cluster 2 (left cluster) with higher proportion of obese people. Did less than test to check if it holds true.
# 
# # Proportion Test for BMI >= 25 (overweight or obese)###
# prop.test(c(length(PBRCCluster$BMI[PBRCCluster$Cluster==1 & PBRCCluster$BMI>=25]), length(PBRCCluster$BMI[PBRCCluster$Cluster==2 & PBRCCluster$BMI>=25])), c(length(PBRCCluster$BMI[PBRCCluster$Cluster==1]), length(PBRCCluster$BMI[PBRCCluster$Cluster==2])))
# ### P-value of .004204  Cluster 2 (left cluster) with significantly higher proportion of overweight or obese people.
# 
# # Proportion Test for Resting Heart Rate > 75###
# prop.test(c(length(PBRCCluster$RestingHR60[PBRCCluster$Cluster==1 & PBRCCluster$RestingHR60>75]), length(PBRCCluster$RestingHR60[PBRCCluster$Cluster==2 & PBRCCluster$RestingHR60>75])), c(length(PBRCCluster$RestingHR60[PBRCCluster$Cluster==1]), length(PBRCCluster$RestingHR60[PBRCCluster$Cluster==2])))
# ### P-value of 1  No difference in proportion of those with high resting heart rates.
# 
# # Proportion of Males in Cluster 1 (right cluster)
# length(PBRCCluster$Gender.x[PBRCCluster$Gender.x=="Male" & PBRCCluster$Cluster==1])/length(PBRCCluster$Gender.x[PBRCCluster$Cluster==1])
# #.4945
# 
# # Proportion of Males in Cluster 2 (left cluster)
# length(PBRCCluster$Gender.x[PBRCCluster$Gender.x=="Male" & PBRCCluster$Cluster==2])/length(PBRCCluster$Gender.x[PBRCCluster$Cluster==2])
# #.561
# 
# # Proportion of White in Cluster 1 (right cluster)
# length(PBRCCluster$Gender.x[PBRCCluster$Race.y=="White" & PBRCCluster$Cluster==1])/length(PBRCCluster$Gender.x[PBRCCluster$Cluster==1])
# #.747
# 
# # Proportion of White in Cluster 2 (left cluster)
# length(PBRCCluster$Gender.x[PBRCCluster$Race.y=="White" & PBRCCluster$Cluster==2])/length(PBRCCluster$Gender.x[PBRCCluster$Cluster==2])
# #.6585
# 
# # Proportion of Black or African American in Cluster 1 (right cluster)
# length(PBRCCluster$Gender.x[PBRCCluster$Race.y=="Black or African American" & PBRCCluster$Cluster==1])/length(PBRCCluster$Gender.x[PBRCCluster$Cluster==1])
# #.2308
# 
# # Proportion of Black or African American in Cluster 2 (left cluster)
# length(PBRCCluster$Gender.x[PBRCCluster$Race.y=="Black or African American" & PBRCCluster$Cluster==2])/length(PBRCCluster$Gender.x[PBRCCluster$Cluster==2])
# #.2683
# 
# # Average Age in Cluster 1 (right cluster)
# mean(PBRCCluster$AgeAtEnrollment.x[PBRCCluster$Cluster==1])
# #35.36
# 
# # Average Age in Cluster 2 (left cluster)
# mean(PBRCCluster$AgeAtEnrollment.x[PBRCCluster$Cluster==2])
# #32.78
# 
# # Linear Model for BMI Controlling for bias elements#
# lmmodel=lm(BMI~as.factor(Cluster)+Race.y+Gender.x,data=PBRCCluster )
# summary(lmmodel)
# bmiresid=rstandard(lmmodel)
# plot(lmmodel)
# boxplot(bmiresid)$out
# 
#   # Linear Model for RHR Controlling for bias elements#
# lmmodelRHR=lm(RestingHR60~as.factor(Cluster)+Race.y+Gender.x+AgeAtEnrollment.x,data=PBRCCluster )
# 
# summary(lmmodelRHR)
# 
# 
# 
# 
# 
# mean(PBRCCluster$X7021.Waist.to.hip.thigh.right[PBRCCluster$Cluster==2])
# 
# 
# PBRCCluster %>% select(High.Blood.Pressure,SBPAvg,DBPAvg)
# 
# mean(PBRCCluster$High.Blood.Pressure[PBRCCluster$Cluster==1])
# sum(PBRCCluster$High.Blood.Pressure[PBRCCluster$Cluster==2])
# 
# 
# PBRCCluster %>% ggplot(aes(x=X7021.Waist.to.hip.thigh.right,y=X0010.Body.Height,color=Cluster))+
#   geom_point()
# 
# totclust=alldata %>% 
#   select(X0010.Body.Height,X7021.Waist.to.hip.thigh.right)
# totclust=totclust[complete.cases(totclust),] 
# totclust = totclust %>% as.matrix() %>%
#   matrix(ncol=ncol(totclust)) 
# 
# #or try to do a mixture model
# gmm1=fj %>%
#   select(X0010.Body.Height,X7021.Waist.to.hip.thigh.right) %>%
#   filter(!is.na(X7021.Waist.to.hip.thigh.right) & !is.na(X0010.Body.Height)) %>%
#   Mclust(G=2)
# 
# gmm3=fj %>%
#   select(X0010.Body.Height,X7021.Waist.to.hip.thigh.right) %>%
#   filter(!is.na(X7021.Waist.to.hip.thigh.right) & !is.na(X0010.Body.Height)) %>%
#   Mclust(G=3)
# 
# fjclean$Cluster3=gmm3$classification
# 
# #plot cluster of 3
# fjclean %>%
#   ggplot(aes(x=X7021.Waist.to.hip.thigh.right,y=X0010.Body.Height,color=Cluster3)) +
#   geom_point()
# 
# #+ stat_density2d(aes(alpha=..density..), geom="raster", contour = FALSE)
# 
# plotpbrc = PBRC %>%
#   select(X7021.Waist.to.hip.thigh.right,X0010.Body.Height) %>%
#   mutate(X7021.Waist.to.hip.thigh.right=X7021.Waist.to.hip.thigh.right/2.54) %>%
#   mutate(X0010.Body.Height=X0010.Body.Height/2.54) %>%
#   filter(!is.na(X7021.Waist.to.hip.thigh.right | X0010.Body.Height)) %>%
#   ggplot(aes(x=X7021.Waist.to.hip.thigh.right,y=X0010.Body.Height)) + geom_point( color="blue") + 
#   coord_cartesian(xlim=c(11,35),ylim=c(56,80))
# 
# plotfj + plotpbrc
# #plot(fj$X0010.Body.Height~fj$X0085.High.waist.height)
# #plot(fj$X5070.Waist.to.high.hip.back,fj$X0010.Body.Height)
# 
# plot(fj$X7021.Waist.to.hip.thigh.right,fj$X0010.Body.Height, xlim=c(10,30),ylim=c(55,80))
# #Definite Cluster to Right
# hipheight=data.frame(fj$X7021.Waist.to.hip.thigh.right,fj$X0010.Body.Height)
# hipheight=hipheight[complete.cases(hipheight),]
# hipheight=as.matrix(hipheight)
# hipheight=matrix(hipheight,ncol=ncol(hipheight))
# hipheightscale=scale(hipheight) 
# 
# PBRChipheight=data.frame(PBRC$X7021.Waist.to.hip.thigh.right/2.54,PBRC$X0010.Body.Height/2.54,PBRChealth$HighBP)
# PBRChipheight=PBRChipheight[complete.cases(PBRChipheight),]
# PBRChipheight=as.matrix(PBRChipheight)
# PBRChipheight=matrix(PBRChipheight,ncol=ncol(PBRChipheight))
# PBRChipheightscale=PBRChipheight
# PBRChipheightscale[,1:2]=scale(PBRChipheightscale[,1:2]) 
# 
# 
# 
# #hdbscan method
# c1=dbscan(hipheightscale, eps=.20,minPts=40)
# plot(hipheight,col=c1$cluster+1,pch=20, xlab="Waist to Hip Thigh Right (inches)",ylab="Height (inches)", main="HDBScan Clustering")
# points(PBRChipheight[,1],PBRChipheight[,2], col="blue", pch=20)
# points(PBRChipheight[,1][PBRChipheight[,3]==1],PBRChipheight[,2][PBRChipheight[,3]==1], col = "black", bg="yellow", pch=21)
# legend("bottomright", legend = c("Long Legs, Short Torso","Short Legs, Long Torso","Overlay of Pennington Data","High BP in Pennington Data"), pch=c(20,20,20,21),pt.bg=c("red","green","blue","yellow"),col=c("red","green","blue","black"))
# 
# c2=dbscan(hipheight, eps=.15,minPts=50)
# plot(hipheight,col=c2$cluster+1,pch=20, xlab="Waist to Hip Thigh Right (Standardized)",ylab="Height (Standardized)", main="HDBScan Clustering")
# 
# #agglomerative clustering ward method
# dhipheight=dist(hipheight, method="euclidean")
# c3=hclust(dhipheight, method="ward.D2")
# plot(c3)
# groups=cutree(c3,k=2)
# plot(hipheight,col=groups,pch=20, xlab="Waist to Hip Thigh Right",ylab="Height", main = "Hierarchical Clustering Ward Method")
# 
# #hierarchical cluster 
# c4=hclust(hipheight, method="centroid")
# groups2=cutree(c4,k=3)
# plot(hipheight,col=groups2,pch=20, xlab="Waist to Hip Thigh Right",ylab="Height", main="Hierarchical Clustering Centroid Method")
# 
# #kmeans clustering
# c5 = kmeans(hipheight, 2)
# plot(hipheight,col=c5$cluster+1,pch=20, xlab="Waist to Hip Thigh Right",ylab="Height", main = "KMeans Clustering")
# 
# #spectral clustering
# 
# c6 = specc(hipheight, 2)
# 
# 
# 
# 
# plot(fj$X7530.Hip.thigh.girth,fj$X0010.Body.Height)


####   PCA for Fort Jackson   ###

fjpreProcess=preProcess(fjclean, method =c("center","scale"))
fjcleanscale=predict(fjpreProcess,fjclean)
#fjcleanscale$gender2=gender
PBRCpre=preProcess(PBRCcleandata,method= c("center","scale"))
PBRCcleanscale=predict(PBRCpre,PBRCcleandata)
#PBRCcleanscale2$gender2=ifelse(PBRCclushealth$Gender.y=="Male",0,1)

#fjcleandatacov=fjcleanscale %>% select(-pca5k4, -allk4, -k4top5, -Cluster) %>% cov()
fjcleanscalecov=cov(fjcleanscale)

fjcleanmeans=0
for (i in 1:length(fjcleanscale[1,])){
  fjcleanmeans[i]=mean(fjcleanscale[,i])
}
#fjcleanmeans=fjcleanmeans[c(1:137,142)]

#sum(complete.cases(fjcleandata))
#fjcleandata=fjcleanscale %>% select(-pca5k4, -allk4, -k4top5, -Cluster)
fjpca=prcomp(~., data =fjcleanscale, center=FALSE, scale=FALSE)
summary(fjpca)

plot(fjpca, type="l")
write.csv(fjpca$rotation, file="fjpca.csv")
PCA1=fjpca$rotation[,1]
PCA2=fjpca$rotation[,2]
PCA3=fjpca$rotation[,3]
PCA4=fjpca$rotation[,4]
PCA5=fjpca$rotation[,5]
PCA6=fjpca$rotation[,6]
#gender=ifelse(fjclean$gender=="M",0,1)
fjPCA5=data.frame(PCA1, PCA2, PCA3,PCA4,PCA5)
fjPCA6=data.frame(PCA1, PCA2, PCA3,PCA4,PCA5, PCA6)

fpcor=cor(fjcleanscale[complete.cases(fjcleanscale),])
fpcor[fpcor>.9]
write.csv(fpcor,file="fpcormatrix.csv")
write.csv(fjcleanscalecov,file="fpcovmatrix.csv")

####   Clustering off of PCA ####
fjpcapredict=predict(fjpca,newdata=fjcleanscale)
fjpcapredict5=as.matrix(fjpcapredict[,1:5])
fjpcapredict6=as.matrix(fjpcapredict[,1:6])
#fjpcapredict5=cbind(fjpcapredict5,gender)
fjpcacluster=kmeans(fjpcapredict5,4,nstart=100)


##### Get PC values for Pennington ####


####   Clustering off of PCA ####
PBRCpcapredict=predict(fjpca,newdata=PBRCcleanscale)
PBRCpcapredict5=PBRCpcapredict[,1:5]
#PBRCcleanscale=PBRCcleanscale %>% select(-Cluster)

findLinearCombos(PBRCcleandata)
CholPBRC=chol(cov(PBRCcleandata), pivot=TRUE)


# Determine number of clusters
wss <- (nrow(fjpcapredict5)-1)*sum(apply(fjpcapredict5,2,var))
for (i in 2:15) {
  clus= kmeans(fjpcapredict6, centers=i, nstart=100)
  wss[i] <- clus$betweenss/clus$totss }
plot(2:15, wss[2:15], type="b", xlab="Number of Clusters",
     ylab="Percent Variance Accounted for by Clusters", main="Cluster Number Impact on Variance")
#abline(0,1, add=TRUE)
wssslope=0
for (i in 3:15) wssslope[i]=wss[i+1]-wss[i]

plot(3:15, wssslope[3:15], type="b", xlab="Number of Clusters",
     ylab="difference in % variance accounted for")

#looks like 4 is a good number of clusters
set.seed(123)
k4=kmeans(fjpcapredict5, centers=4,nstart=100)
k4$centers
fjcleanscale$pca5k4=k4$cluster

set.seed(123)
k46=kmeans(fjpcapredict6, centers=4,nstart=100)
fjcleanscale$pca6k4=k46$cluster

#### How do these clusters apply to Pennington
predict.kmeans(k4,PBRCpcapredict5)

###Using flexclust to cluster ####
fjk4=kcca(fjpcapredict5, k=4)

## Determine Mahalanobis distance##
MNPCA5=c(rep(0,4))
#MNPCAsd=0
for (i in 1:4){
  MNPCA5[i]=fjcleanscale %>% 
    select(-pca5k4, -pca6k4, -allk4, -k4top5) %>% 
    #select(-pca5k4, -allk4, -k4top5) %>% 
    filter(fjcleanscale$pca5k4==i) %>% 
    as.matrix() %>% 
    mahalanobis(center=fjcleanmeans,cov=fjcleanscalecov, tol=1e-20) %>%
    mean()
    
   
}
MNPCA5=sqrt(MNPCA5)
mean(MNPCA5)
#mean(sqrt(MNPCA5))

MNPCA6=0
for (i in 1:4){
  MNPCA6[i]=fjcleanscale %>% 
    select(-pca5k4, -pca6k4, -allk4, -k4top5) %>% 
    #select(-pca5k4, -allk4, -k4top5) %>% 
    filter(fjcleanscale$pca6k4==i) %>% 
    as.matrix() %>% 
    mahalanobis(center=fjcleanmeans,cov=fjcleanscalecov, tol=1e-20) %>%
    mean()
  
  
}
MNPCA6=sqrt(MNPCA6)
mean(MNPCA6)

fjcleanscale  %>%  mutate(same=(fjcleanscale$pca5k4==fjcleanscale$allk4))  %>% select(same) %>% summarise(sum(same))
#### Clustering off entire data set #####
set.seed(123)
k4all=fjcleanscale %>%
  #select(-pca5k4, -allk4, -k4top5) %>%
  select(-pca5k4, -pca6k4,-allk4,-k4top5) %>%
  as.matrix() %>%
  kmeans(centers=4,nstart=100)

fjcleanscale$allk4=k4all$cluster

MNAll=0
for (i in 1:4){
  MNAll[i]=fjcleanscale %>% 
    select(-pca5k4, -pca6k4, -allk4, -k4top5) %>% 
    #select(-pca5k4, -allk4, -k4top5) %>% 
    filter(fjcleanscale$allk4==i) %>% 
    as.matrix() %>% mahalanobis(center=fjcleanmeans,cov=fjcleanscalecov, tol=1e-20)  %>%
    mean()
}
MNAll=sqrt(MNAll)
mean(MNAll)
##### Clustering off of the top 5 variables from the first 5 Principle Components
### X9800.Weight, X0691.3D.waistband.front.height, X6030.Dev..waist.band.from.waist..back., X5080.Waist.to.buttock, X0630.Distance.back.in.belly.height.to.vertical
fjtop5=fjcleanscale %>%
  select(X9800.Weight,X0691.3D.waistband.front.height,X6030.Dev..waist.band.from.waist..back.,X5080.Waist.to.buttock,X0630.Distance.back.in.belly.height.to.vertical)
  #select(X9800.Weight,X0691.3D.waistband.front.height,X0641.3D.waistband.front.to.vertical,X5080.Waist.to.buttock,X5085.Distance.waistband...buttock)
set.seed(123)
k4top5=kmeans(fjtop5,centers=4,nstart=100)
fjcleanscale$k4top5=k4top5$cluster

MNk4top5=0
for (i in 1:4){
  MNk4top5[i]=fjcleanscale %>% 
    select(-pca5k4, -allk4, -k4top5, -pca6k4) %>% 
    #select( -k4top5) %>% 
    filter(fjcleanscale$k4top5==i) %>% 
    as.matrix() %>% mahalanobis(center=fjcleanmeans,cov=fjcleanscalecov, tol=1e-20) %>%
    mean()
}
MNk4top5=sqrt(MNk4top5)

mean(MNPCA5)
mean(MNAll)
mean(MNk4top5)



#### Assign Cluster to Pennington principal component data
ObsCluster=matrix(,ncol=5, nrow=length(PBRCpcapredict5[,1]))

# Create function that determines the euclidean distance between to equally lengthed vectors
distance=function(x,y)
{
  ss=0
  for (i in 1:length(x)){
    if(!is.na(x[i]) & !is.na(y[i])) ss=ss+(y[i]-x[i])^2
  }
  return(sqrt(ss))
}

## Determine the distance of each observation to the cluster centroid
for (j in 1:length(PBRCpcapredict5[,1])) {

    for (k in 1:length(fjpcacluster$centers[,1])){
      ObsCluster[j,k]=distance(fjpcacluster$centers[k,],PBRCpcapredict5[j,])
    }
  
}


## Assign Each Observation to the cluster it is nearest to
for (i in 1:length(ObsCluster[,1])){
  ObsCluster[i,5]=which(ObsCluster[i,]==min(ObsCluster[i,1:4]))[1]  
}

PBRCclean$Cluster=as.factor(ObsCluster[,5])
PBRCcleanscale$Cluster=as.factor(ObsCluster[,5])
#PBRCcleanscale2$Cluster=as.factor(ObsCluster[,5])


fjclean$Cluster=as.factor(fjpcacluster$cluster)
fjcleanscale$Cluster=fjpcacluster$cluster


##### Check Melahanobis Distance for PBRC Cluster Assignment
PBRCcleanscalecov=cov(PBRCcleanscale)
#fjcleanscalecov=cov(fjcleanscale$)

PBRCcleanmeans=0
for (i in 1:length(PBRCcleanscale[1,])){
  PBRCcleanmeans[i]=mean(PBRCcleanscale[,i])
}

PBRCcleanmeans2=0
for (i in 1:length(PBRCcleanscale2[1,])){
  PBRCcleanmeans2[i]=mean(PBRCcleanscale2[,i])
}

MNPBRCtop5=0
MNPBRCsd=0
for (i in 1:4){
  MNPBRCtop5[i]=PBRCcleanscale %>% 
    #select(-pca5k4, -allk4, -k4top5) %>% 
    select( -Cluster) %>% 
    filter(PBRCcleanscale$Cluster==i) %>% 
    as.matrix() %>% mahalanobis(center=PBRCcleanmeans,cov=fjcleandatacov, tol=1e-22)   %>% 
    mean()
  
  MNPBRCsd[i]=PBRCcleanscale %>% 
    #select(-pca5k4, -allk4, -k4top5) %>% 
    select( -Cluster) %>% 
    filter(PBRCcleanscale$Cluster==i) %>% 
    as.matrix() %>% mahalanobis(center=PBRCcleanmeans,cov=PBRCcleanscalecov, tol=1e-22)   %>% 
    sd()
}


#### Euclidean Distance to Centroids from the data points

FJClus1 = fjcleanscale %>% select(-Cluster,-pca5k4,-allk4,-k4top5, -pca6k4) %>%
  filter(fjcleanscale$Cluster=="1")

FJClus2 = fjcleanscale %>% select(-Cluster,-pca5k4,-allk4,-k4top5, -pca6k4) %>%
  filter(fjcleanscale$Cluster=="2")

FJClus3 = fjcleanscale %>% select(-Cluster,-pca5k4,-allk4,-k4top5, -pca6k4) %>%
  filter(fjcleanscale$Cluster=="3")
FJClus4 = fjcleanscale %>% select(-Cluster,-pca5k4,-allk4,-k4top5, -pca6k4) %>%
  filter(fjcleanscale$Cluster=="4")

FJCent1=0
for (i in 1:length(FJClus1[1,])){
  FJCent1[i]=mean(FJClus1[,i])
}

FJCent2=0
for (i in 1:length(FJClus2[1,])){
  FJCent2[i]=mean(FJClus2[,i])
}

FJCent3=0
for (i in 1:length(FJClus3[1,])){
  FJCent3[i]=mean(FJClus3[,i])
}

FJCent4=0
for (i in 1:length(FJClus4[1,])){
  FJCent4[i]=mean(FJClus4[,i])
}
  
FJEuc1 = 0
for (j in 1:length(FJClus1[,1])) {
  FJEuc1[j]=0
    for(k in 1:length(FJClus1[1,])) {
      FJEuc1[j]=FJEuc1[j]+(FJClus1[j,k]-FJCent1[k])^2
    }
  FJEuc1[j]=sqrt(FJEuc1[j])
}

FJEuc2 = 0
for (j in 1:length(FJClus2[,1])) {
  FJEuc2[j]=0
  for(k in 1:length(FJClus2[1,])) {
    FJEuc2[j]=FJEuc2[j]+(FJClus2[j,k]-FJCent2[k])^2
  }
  FJEuc2[j]=sqrt(FJEuc2[j])
}

FJEuc3 = 0
for (j in 1:length(FJClus3[,1])) {
  FJEuc3[j]=0
  for(k in 1:length(FJClus3[1,])) {
    FJEuc3[j]=FJEuc3[j]+(FJClus3[j,k]-FJCent3[k])^2
  }
  FJEuc3[j]=sqrt(FJEuc3[j])
}

FJEuc4 = 0
for (j in 1:length(FJClus4[,1])) {
  FJEuc4[j]=0
  for(k in 1:length(FJClus4[1,])) {
    FJEuc4[j]=FJEuc4[j]+(FJClus4[j,k]-FJCent4[k])^2
  }
  FJEuc4[j]=sqrt(FJEuc4[j])
}


PBRCClus1 = PBRCcleanscale %>% select(-Cluster) %>%
  filter(PBRCcleanscale$Cluster=="1")

PBRCClus2 = PBRCcleanscale %>% select(-Cluster) %>%
  filter(PBRCcleanscale$Cluster=="2")

PBRCClus3 = PBRCcleanscale %>% select(-Cluster) %>%
  filter(PBRCcleanscale$Cluster=="3")
PBRCClus4 = PBRCcleanscale %>% select(-Cluster) %>%
  filter(PBRCcleanscale$Cluster=="4")

PBRCCent1=0
for (i in 1:length(PBRCClus1[1,])){
  PBRCCent1[i]=mean(PBRCClus1[,i])
}

PBRCCent2=0
for (i in 1:length(PBRCClus2[1,])){
  PBRCCent2[i]=mean(PBRCClus2[,i])
}

PBRCCent3=0
for (i in 1:length(PBRCClus3[1,])){
  PBRCCent3[i]=mean(PBRCClus3[,i])
}

PBRCCent4=0
for (i in 1:length(PBRCClus4[1,])){
  PBRCCent4[i]=mean(PBRCClus4[,i])
}

# PBRCClus1 = PBRCcleanscale2 %>% select(-Cluster) %>%
#   filter(PBRCcleanscale$Cluster=="1")
# 
# PBRCClus2 = PBRCcleanscale2 %>% select(-Cluster) %>%
#   filter(PBRCcleanscale$Cluster=="2")
# 
# PBRCClus3 = PBRCcleanscale2 %>% select(-Cluster) %>%
#   filter(PBRCcleanscale$Cluster=="3")
# PBRCClus4 = PBRCcleanscale2 %>% select(-Cluster) %>%
#   filter(PBRCcleanscale$Cluster=="4")

PBRCEuc1 = 0
for (j in 1:length(PBRCClus1[,1])) {
  PBRCEuc1[j]=0
  for(k in 1:length(PBRCClus1[1,])) {
    PBRCEuc1[j]=PBRCEuc1[j]+(PBRCClus1[j,k]-PBRCCent1[k])^2
  }
  PBRCEuc1[j]=sqrt(PBRCEuc1[j])
}

PBRCEuc2 = 0
for (j in 1:length(PBRCClus2[,1])) {
  PBRCEuc2[j]=0
  for(k in 1:length(PBRCClus2[1,])) {
    PBRCEuc2[j]=PBRCEuc2[j]+(PBRCClus2[j,k]-PBRCCent2[k])^2
  }
  PBRCEuc2[j]=sqrt(PBRCEuc2[j])
}

PBRCEuc3 = 0
for (j in 1:length(PBRCClus3[,1])) {
  PBRCEuc3[j]=0
  for(k in 1:length(PBRCClus3[1,])) {
    PBRCEuc3[j]=PBRCEuc3[j]+(PBRCClus3[j,k]-PBRCCent3[k])^2
  }
  PBRCEuc3[j]=sqrt(PBRCEuc3[j])
  
}

PBRCEuc4 = 0
for (j in 1:length(PBRCClus4[,1])) {
  PBRCEuc4[j]=0
  for(k in 1:length(PBRCClus4[1,])) {
    PBRCEuc4[j]=PBRCEuc4[j]+(PBRCClus4[j,k]-PBRCCent4[k])^2
  }
  PBRCEuc4[j]=sqrt(PBRCEuc4[j])
}

mean(PBRCEuc1)
mean(FJEuc1)
sd(PBRCEuc1)
sd(FJEuc1)

mean(PBRCEuc2)
mean(FJEuc2)
sd(PBRCEuc2)
sd(FJEuc2)

mean(PBRCEuc3)
mean(FJEuc3)
sd(PBRCEuc3)
sd(FJEuc3)

mean(PBRCEuc4)
mean(FJEuc4)
sd(PBRCEuc4)
sd(FJEuc4)

test = as.matrix(c(c(mean(FJEuc1),sd(FJEuc1)),c(mean(PBRCEuc1),sd(PBRCEuc1))))

#### Visualize Cluster location based on two cluster phenomenon
#Overlay of BMI from PBRC to FJ
plotClust = fj %>% 
  select(X7021.Waist.to.hip.thigh.right,X0010.Body.Height) %>%
  filter(!is.na(X7021.Waist.to.hip.thigh.right | X0010.Body.Height)) %>%
  ggplot(aes(x=X7021.Waist.to.hip.thigh.right,y=X0010.Body.Height)) + 
  coord_cartesian(xlim=c(11,35),ylim=c(56,80)) +
  geom_point(alpha=.3) +
  geom_point(data=PBRCclean, aes(color=Cluster)) 
 # geom_point(data=PBRCMutate %>% filter(BMI > 30), aes(color="BMI > 30")) +
 # scale_color_manual(name="Legend", values=c("FJ"="black","PBRC"="blue","BMI > 30"="red"), limits=c("FJ","PBRC","BMI > 30")) +
 # labs(title="BMI Over 30", x="Waist to Hip (inches)",y="Body Height (inches)") +
  #theme(legend.position = c(.9,.15))

### Add Health Information to the Cluster 
PBRCclushealth=left_join(PBRCclean,PBRChealth,"SubjectID")
PBRCclushealth=left_join(PBRCclushealth,Tanita,"SubjectID")
write.csv(PBRCclushealth,file="PBRCCluster.csv")
write.csv(fjclean,file="FJCluster.csv")

pairwise.t.test(PBRCclushealth$BMI,PBRCclushealth$Cluster)
#### 
mean(PBRCclushealth$BMI[PBRCclushealth$Cluster==3 & !is.na(PBRCclushealth$BMI)])
pairwise.t.test(fjclean$BMI.BMI,fjclean$Cluster)


#### Violin/Boxplot on clusters to BMI
fjbmiclusplot=ggplot(fjclean, aes(x=Cluster, y=BMI.BMI))+geom_violin() + 
  geom_boxplot(width=.1, fill="black", outlier.colour=NA)+
  stat_summary(fun.y=median, geom="point",fill="white",shape=21,size=2.5)+labs(title="Fort Jackson BMI Clusters", yaxis="BMI")
pbrcbmiclusplot=ggplot(PBRCclushealth, aes(x=Cluster, y=BMI))+geom_violin() + 
  geom_boxplot(width=.1, fill="black", outlier.colour=NA)+
  stat_summary(fun.y=median, geom="point",fill="white",shape=21,size=2.5) +labs(title="PBRC BMI Clusters")


fjbmiclusplot=ggplot(fjclean, aes(x=Cluster, y=BMI.BMI))+geom_violin() + 
  geom_boxplot(width=.1, fill="black", outlier.colour=NA)+
  stat_summary(fun.y=median, geom="point",fill="white",shape=21,size=2.5)+labs(title="Fort Jackson BMI Clusters", yaxis="BMI")
pbrcrhrclusplot=ggplot(PBRCclushealth, aes(x=Cluster, y=RestingHR60))+geom_violin() + 
  geom_boxplot(width=.1, fill="black", outlier.colour=NA)+
  stat_summary(fun.y=median, geom="point",fill="white",shape=21,size=2.5) +labs(title="PBRC Resting HR Clusters")
pairwise.t.test(PBRCclushealth$RestingHR60,PBRCclushealth$Cluster)


length(PBRCclushealth$High.Blood.Pressure[PBRCclushealth$Cluster==1 & PBRCclushealth$High.Blood.Pressure==1])/length(PBRCclushealth$High.Blood.Pressure[PBRCclushealth$Cluster==1])
###.077 High BP Rate in Cluster 1
length(PBRCclushealth$High.Blood.Pressure[PBRCclushealth$Cluster==2 & PBRCclushealth$High.Blood.Pressure==1])/length(PBRCclushealth$High.Blood.Pressure[PBRCclushealth$Cluster==2])
###.148 High BP Rate in Cluster 2
length(PBRCclushealth$High.Blood.Pressure[PBRCclushealth$Cluster==3 & PBRCclushealth$High.Blood.Pressure==1])/length(PBRCclushealth$High.Blood.Pressure[PBRCclushealth$Cluster==3])
###.208 High BP Rate in Cluster 3
length(PBRCclushealth$High.Blood.Pressure[PBRCclushealth$Cluster==4 & PBRCclushealth$High.Blood.Pressure==1])/length(PBRCclushealth$High.Blood.Pressure[PBRCclushealth$Cluster==4])
###.167 High BP Rate in Cluster 4


length(fjclean$Med.Board[fjclean$Cluster==1 & fjclean$Med.Board==1])/length(fjclean$Cluster[fjclean$Cluster==1])
###.0124 Med Board Rate in Cluster 1
length(fjclean$Med.Board[fjclean$Cluster==2 & fjclean$Med.Board==1])/length(fjclean$Cluster[fjclean$Cluster==2])
###.00416 Med Board Rate in Cluster 2
length(fjclean$Med.Board[fjclean$Cluster==3 & fjclean$Med.Board==1])/length(fjclean$Cluster[fjclean$Cluster==3])
###.0065 Med Board Rate in Cluster 3
length(fjclean$Med.Board[fjclean$Cluster==4 & fjclean$Med.Board==1])/length(fjclean$Cluster[fjclean$Cluster==4])
###.00597 Med Board Rate in Cluster 4


#new plot area using rgl package
plot3d( fjclean$X0691.3D.waistband.front.height, fjclean$X9800.Weight,fjclean$X0641.3D.waistband.front.to.vertical, type="p", ylab="Weight", xlab="Waistband Height", zlab = "Waistband Vertical", main = "Clustering of People by Height and Waistband")

#add the points
points3d(fjclean$X0691.3D.waistband.front.height,fjclean$X9800.Weight,  fjclean$X0641.3D.waistband.front.to.vertical, pch=21, col=fjclean$Cluster, add=T)

#3d scatter plot of fort jackson clustering using car package
scatter3d(fjclean$X0691.3D.waistband.front.height,fjclean$X9800.Weight,  fjclean$X6030.Dev..waist.band.from.waist..back., parallel=FALSE,bg="white",ylab="Weight", xlab="Waistband Height", zlab = "Waistband Vertical",surface=FALSE, axis.ticks = TRUE, groups=fjclean$Cluster, ellipsoid=TRUE,axis.col=c("black","black","black"))

scatter3d(PBRCclushealth$X0691.3D.waistband.front.height,PBRCclushealth$X9800.Weight,  PBRCclushealth$X6030.Dev..waist.band.from.waist..back., parallel=FALSE,bg="white",ylab="Weight", xlab="Waistband Height", zlab = "Waistband Vertical",surface=FALSE, axis.ticks = TRUE, groups=PBRCclushealth$Cluster, ellipsoid=FALSE,axis.col=c("black","black","black"), add=TRUE)
scatter3d(PBRCclushealth$X0691.3D.waistband.front.height,PBRCclushealth$X9800.Weight,  PBRCclushealth$X0630.Distance.back.in.belly.height.to.vertical, parallel=FALSE,bg="white",ylab="Weight", xlab="Waistband Height", zlab = "Waistband Vertical",surface=FALSE, axis.ticks = TRUE, groups=PBRCclushealth$Cluster, ellipsoid=FALSE,axis.col=c("black","black","black"), add=TRUE)

scatter3d(PBRCmetric$X9800.Weight, PBRCmetric$X0180.Neck.height.front,  PBRCmetric$X0030.Neck.height, xlab="",ylab="",zlab="", parallel=FALSE,bg="white",surface=FALSE, axis.ticks = TRUE, groups=PBRCmetric$Cluster, ellipsoid=FALSE,axis.col=c("black","black","black"))
scatter3d(fjmetric$X9800.Weight,fjmetric$X0180.Neck.height.front,  fjmetric$X0030.Neck.height,xlab="",ylab="",zlab="", parallel=FALSE,bg="white",surface=FALSE, axis.ticks = TRUE, groups=fjmetric$Cluster, ellipsoid=FALSE,axis.col=c("black","black","black"))
PBRCmetric=PBRCclushealth %>% mutate(X9800.Weight=X9800.Weight/2.205,X0180.Neck.height.front=X0180.Neck.height.front*2.54,X0030.Neck.height=X0030.Neck.height*2.54)
fjmetric=fjclean %>% mutate(X9800.Weight=X9800.Weight/2.205,X0180.Neck.height.front=X0180.Neck.height.front*2.54,X0030.Neck.height=X0030.Neck.height*2.54)
scatter3d(PBRCmetric$X9800.Weight,PBRCmetric$X0180.Neck.height.front,PBRCmetric$X0030.Neck.height,xlab="",ylab="",zlab="",bg="white",groups=PBRCmetric$Cluster, surface=FALSE, axis.ticks=FALSE)


### Table comparing fat data####
PBRCclushealth=PBRCclushealth %>% mutate(Fat.Mass.VAT.Perc=Fat.Mass...VAT/(DXA.Weight* 10)) %>%
  mutate(Fat.Mass.Tot.Perc=Fat.Mass...Total/(DXA.Weight* 10)) %>%
  mutate(Lean.Mass.VAT.Perc=Lean.Mass...VAT/(DXA.Weight*10)) %>%
  mutate(Lean.Mass.Tot.Perc=Lean.Mass...Total/(DXA.Weight*10)) %>%
  mutate(Tissue....fat....Total=Tissue....fat....Total)

####  Creating data frame for mean and sd of fat percentages via clusters  
Fatdf=PBRCclushealth %>%
  select(Lean.Mass.VAT.Perc, Lean.Mass.Tot.Perc, Fat.Mass.Tot.Perc, Fat.Mass.VAT.Perc,Cluster) %>%
  filter(complete.cases(.)) %>%
  group_by(Cluster) %>% summarise(LeanTotPercMean=mean(Lean.Mass.Tot.Perc),LeanTotPercSd=sd(Lean.Mass.Tot.Perc),LeanVATPercMean=mean(Lean.Mass.VAT.Perc),LeanVATPercSd=sd(Lean.Mass.VAT.Perc),FatTotPercMean=mean(Fat.Mass.Tot.Perc),FatTotPercSd=sd(Fat.Mass.Tot.Perc),FatVATPercMean=mean(Fat.Mass.VAT.Perc),FatVATPercSd=sd(Fat.Mass.VAT.Perc))


#### Table depicting the percentages ##### 
Fatdf %>%
  mutate(
    Cluster = row.names(.),
    LeanVATPercMean=color_bar("lightgreen")(round(LeanVATPercMean,2)),
    LeanTotPercMean=color_bar("yellow")(round(LeanTotPercMean,2)),
    FatVATPercMean=color_bar("lightblue")(round(FatVATPercMean,2)),
    FatTotPercMean=color_bar("orange")(round(FatTotPercMean,2))
    ) %>%
  select(Cluster,everything()) %>%
  kable("html", escape=F, digits=2,align="l", col.names=c("Cluster","Mean","Standard Deviation","Mean","Standard Deviation","Mean","Standard Deviation","Mean","Standard Deviation")) %>%
  kable_styling(c("striped","bordered"),full_width=F) %>%
  column_spec(c(2,4,6,8),width="1.7in",bold=T,border_left = T) %>%
  column_spec(1,bold=T) %>%
  add_header_above(c(" ", "Lean Total %"=2,"Lean VAT %"=2,"Fat Total %"=2,"Fat VAT %"=2))

##### violin/boxplot for PBRC BMI
pbrcBMIclusplot=ggplot(PBRCclushealth, aes(x=Cluster, y=BMI))+geom_violin() + 
  geom_boxplot(width=.1, fill="black", outlier.colour=NA)+
  stat_summary(fun.y=median, geom="point",fill="white",shape=21,size=2.5) +labs(title="BMI for PBRC Clusters")
pairwise.t.test(PBRCclushealth$BMI,PBRCclushealth$Cluster)

##### violin/boxplot for FJ BMI
fjBMIclusplot=ggplot(fjclean, aes(x=Cluster, y=BMI.BMI))+geom_violin() + 
  geom_boxplot(width=.1, fill="black", outlier.colour=NA)+
  stat_summary(fun.y=median, geom="point",fill="white",shape=21,size=2.5) +labs(title="BMI for Fort Jackson Clusters")
pairwise.t.test(fjclean$BMI.BMI,fjclean$Cluster)

##### violin/boxplot for Lean Mass VAT
pbrcLMVATclusplot=ggplot(PBRCclushealth, aes(x=Cluster, y=Lean.Mass.VAT.Perc))+geom_violin() + 
  geom_boxplot(width=.1, fill="black", outlier.colour=NA)+
  stat_summary(fun.y=median, geom="point",fill="white",shape=21,size=2.5) +labs(title="PBRC Lean Mass VAT Clusters")
pairwise.t.test(PBRCclushealth$Lean.Mass.VAT.Perc,PBRCclushealth$Cluster)

##### violin/boxplot for Lean Mass Total
pbrcLMTotclusplot=ggplot(PBRCclushealth, aes(x=Cluster, y=Lean.Mass.Tot.Perc))+geom_violin() + 
  geom_boxplot(width=.1, fill="black", outlier.colour=NA)+
  stat_summary(fun.y=median, geom="point",fill="white",shape=21,size=2.5) +labs(title="PBRC Lean Mass Total Clusters")
pairwise.t.test(PBRCclushealth$Lean.Mass.Tot.Perc,PBRCclushealth$Cluster)


##### violin/boxplot for Fat MAss Total
pbrcFMTotclusplot=ggplot(PBRCclushealth, aes(x=Cluster, y=Fat.Mass.Tot.Perc))+geom_violin() + 
  geom_boxplot(width=.1, fill="black", outlier.colour=NA)+
  stat_summary(fun.y=median, geom="point",fill="white",shape=21,size=2.5) +labs(title="PBRC Fat Mass Total Clusters")
pairwise.t.test(PBRCclushealth$Fat.Mass.Tot.Perc,PBRCclushealth$Cluster)


##### violin/boxplot for Fat MAss VAT 
pbrcFMTotclusplot=ggplot(PBRCclushealth, aes(x=Cluster, y=Fat.Mass.VAT.Perc))+geom_violin() + 
  geom_boxplot(width=.1, fill="black", outlier.colour=NA)+
  stat_summary(fun.y=median, geom="point",fill="white",shape=21,size=2.5) +labs(title="PBRC Fat Mass VAT Clusters")
pairwise.t.test(PBRCclushealth$Fat.Mass.VAT.Perc,PBRCclushealth$Cluster)
