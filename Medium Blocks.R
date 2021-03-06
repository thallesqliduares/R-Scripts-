################### Medium Blocks #####################
#######################################################

#set directory#

setwd("C:\\Program Files\\R\\Dados")

list.files()

#upload packages

library(readxl)
library(plm)

#upload database

data<-readxl::read_excel("C:\\Program Files\\R\\Dados\\Base SNIS_Medio.xlsx")


# Outputs #

data$AguaProd<-log(data$AG006/mean(data$AG006))

data$EsgCol<-log(data$ES005/mean(data$ES005))


#Water Connections # 

data$LigAtv<-log(data$AG002/mean(data$AG002))

# Proxy for stock capital: length of network#

data$ExtRed<-log(data$AG005/mean(data$AG005))

# Price of contracted out services will be used as reference price##

data$p1<-log(data$p_s/mean(data$p_s))

data$p2<-log(data$p_l/mean(data$p_l))-data$p1

data$p3<-log(data$p_e/mean(data$p_e))-data$p1

# Cost variable

data$ct<-log(data$FN015/mean(data$FN015))-data$p1


# Translog Cost Function ## 


# Interaction terms for water produced


data$AgEsg<-(data$AguaProd)*(data$EsgCol)

data$AgLig<-(data$AguaProd)*(data$LigAtv)

data$AgExt<-(data$AguaProd)*(data$ExtRed)

data$Agp2<-(data$AguaProd)*(data$p2)

data$Agp3<-(data$AguaProd)*(data$p3)

data$AgAg<-0.5*((data$AguaProd)*(data$AguaProd))


# Interaction terms for sewage collected #

data$EsgLig<-(data$EsgCol)*(data$LigAtv)

data$EsgExt<-(data$EsgCol)*(data$ExtRed)

data$Esgp2<-(data$EsgCol)*(data$p2)

data$Esgp3<-(data$EsgCol)*(data$p3)

data$EsgEsg<-0.5*((data$EsgCol)*(data$EsgCol))


# Interaction terms for water connections

data$LigExt<-(data$LigAtv)*(data$ExtRed)

data$Ligp2<-(data$LigAtv)*(data$p2)

data$Ligp3<-(data$LigAtv)*(data$p3)

data$LigLig<-0.5*((data$LigAtv)*(data$LigAtv))


# Interaction terms for length of network


data$Extp2<-(data$ExtRed)*(data$p2)

data$Extp3<-(data$ExtRed)*(data$p3)

data$ExtExt<-0.5*((data$ExtRed)*(data$ExtRed))


#Interaction terms for labor price

data$p2p3<-(data$p2)*(data$p3)

data$p2p2<-0.5*((data$p2)*(data$p2))


#Interaction terms for energy price

data$p3p3<-0.5*((data$p3)*(data$p3))



#############Cost share equations##########

#share labor

data$s_l<-log(data$FN010/mean(data$FN015))#-data$p1

#share energy

data$s_e<-log(data$FN013/mean(data$FN015))#-data$p1
#share services

#data$s_s<-log(data$FN014/mean(data$FN015))-data$p1


# Imposing matrix of restrictions for SUR estimation

R<-matrix(0,nrow=14,ncol=60)

R[1,6]<-R[2,7]<-R[3,11]<-R[4,12]<-R[5,16]<-R[6,17]<-R[7,20]<-R[8,21]<-R[9,23]<-R[10,24]<-R[11,26]<-R[12,26]<-R[13,27]<-R[14,28]<-1
R[1,47]<-R[2,54]<-R[3,48]<-R[4,55]<-R[5,49]<-R[6,56]<-R[7,50]<-R[8,57]<-R[9,51]<-R[10,58]<-R[11,53]<-R[12,59]<-R[13,52]<-R[14,60]<--1

#R

#Estimating the SUR model

est1<-plm(list(ct~AguaProd+EsgCol+LigAtv+ExtRed+p2+p3+AgEsg+AgLig+AgExt+Agp2+Agp3+AgAg+
                 +EsgLig+EsgExt+Esgp2+Esgp3+EsgEsg+LigExt+Ligp2+Ligp3+LigLig+Extp2+Extp3+ExtExt+p2p3+p2p2+p3p3
               +factor(data$Bloco)+factor(data$Ano),
               s_l~AguaProd+EsgCol+LigAtv+ExtRed+p2+p3,
               s_e~AguaProd+EsgCol+LigAtv+ExtRed+p2+p3),data,model="random",
          random.method ="nerlove", restrict.matrix = R)

summary(est1)

# Elasticities of translog cost function

#Water produced

elas_agua_md<-0.692+0.104*mean(data$EsgCol)+0.082*mean(data$LigAtv)+0.601*mean(data$ExtRed)-0.238*mean(data$p2)+0.572*mean(data$p3)-1.449*mean(data$AguaProd)

elas_agua_md


#Sewage collected

elas_esgcol_md<-0.021+0.104*mean(data$AguaProd)+0.470*mean(data$LigAtv)-0.324*mean(data$ExtRed)-0.031*mean(data$p2)-0.063*mean(data$p3)-0.123*mean(data$EsgCol)

elas_esgcol_md

#Water connections

elas_lig_md<-0.028+0.082*mean(data$AguaProd)+0.470*mean(data$EsgCol)+0.197*mean(data$ExtRed)-0.024*mean(data$p2)+0.085*mean(data$p3)+0.853*mean(data$LigAtv)

elas_lig_md

#Length network

elas_ext_md<--0.077+0.601*mean(data$AguaProd)-0.324*mean(data$EsgCol)+0.197*mean(data$LigAtv)+0.383*mean(data$p2)+0.001*mean(data$p3)+0.411*mean(data$ExtRed)

elas_ext_md

#share labor

share_labor_md<-0.544-0.894*mean(data$AguaProd)-0.006*mean(data$EsgCol)+0.585*mean(data$LigAtv)-0.345*mean(data$ExtRed)+1.044*mean(data$p2)+0.098*mean(data$p3)

share_labor_md

#share energy

share_energy_md<-0.065+0.277*mean(data$AguaProd)+0.141*mean(data$EsgCol)+0.164*mean(data$LigAtv)-0.579*mean(data$ExtRed)+0.098*mean(data$p2)+1.023*mean(data$p3)

share_energy_md



