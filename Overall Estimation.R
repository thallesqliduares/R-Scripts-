############ All Blocks #################

#set directory

setwd("C:\\Program Files\\R\\Dados")

#upload packages

library(readxl)
library(plm)

#Upload database

data<-readxl::read_excel("C:\\Program Files\\R\\Dados\\Base SNIS.xlsx")


# Outputs: water produced and sewage collected

data$AguaProd<-log(data$AG006/mean(data$AG006))

data$EsgCol<-log(data$ES005/mean(data$ES005))


#Water Connections 

data$LigAtv<-log(data$AG002/mean(data$AG002))

# Proxy for capital stock: network length 

data$ExtRed<-log(data$AG005/mean(data$AG005))

# Price of contracted out services will be used as reference price

data$p1<-log(data$p_s/mean(data$p_s))

data$p2<-log(data$p_l/mean(data$p_l))-data$p1

data$p3<-log(data$p_e/mean(data$p_e))-data$p1

# Cost variable

data$ct<-log(data$FN015/mean(data$FN015))-data$p1



#######Translog Cost Function ######### 


#Interaction terms for water produced


data$AgEsg<-(data$AguaProd)*(data$EsgCol)

data$AgLig<-(data$AguaProd)*(data$LigAtv)

data$AgExt<-(data$AguaProd)*(data$ExtRed)

data$Agp2<-(data$AguaProd)*(data$p2)

data$Agp3<-(data$AguaProd)*(data$p3)

data$AgAg<-0.5*((data$AguaProd)*(data$AguaProd))


# Interaction terms for sewage collected 

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


#########Cost share equations#########

# share labor

data$s_l<-log(data$FN010/mean(data$FN015))

# share energy

data$s_e<-log(data$FN013/mean(data$FN015))

#share services (do not enter in system of equations)

#data$s_s<-log(data$FN014/mean(data$FN015))-data$p1


# Imposing restriction matrix for SUR estimation

R<-matrix(0,nrow=14,ncol=93)

R[1,6]<-R[2,7]<-R[3,11]<-R[4,12]<-R[5,16]<-R[6,17]<-R[7,20]<-R[8,21]<-R[9,23]<-R[10,24]<-R[11,26]<-R[12,26]<-R[13,27]<-R[14,28]<-1
R[1,80]<-R[2,87]<-R[3,81]<-R[4,88]<-R[5,82]<-R[6,89]<-R[7,83]<-R[8,90]<-R[9,84]<-R[10,91]<-R[11,86]<-R[12,92]<-R[13,85]<-R[14,93]<--1

#R

#Estimation of SUR model

est1<-plm(list(ct~AguaProd+EsgCol+LigAtv+ExtRed+p2+p3+AgEsg+AgLig+AgExt+Agp2+Agp3+AgAg+
                 +EsgLig+EsgExt+Esgp2+Esgp3+EsgEsg+LigExt+Ligp2+Ligp3+LigLig+Extp2+Extp3
               +ExtExt+p2p3+p2p2+p3p3+factor(data$Bloco)+factor(data$Ano),
               s_l~AguaProd+EsgCol+LigAtv+ExtRed+p2+p3,
               s_e~AguaProd+EsgCol+LigAtv+ExtRed+p2+p3),data,model="random",
          random.method = "nerlove", restrict.matrix = R)

summary(est1)

# Elasticities of translog cost function

#Water produced

elas_agua_br<-0.376-0.005*mean(data$EsgCol)-0.072*mean(data$LigAtv)+0.335*mean(data$ExtRed)-0.051*mean(data$p2)+0.408*mean(data$p3)-0.156*mean(data$AguaProd)

elas_agua_br

#Sewage collected

elas_esgcol_br<-0.102-0.005*mean(data$AguaProd)-0.015*mean(data$LigAtv)-0.028*mean(data$ExtRed)+0.037*mean(data$p2)-0.032*mean(data$p3)+0.023*mean(data$EsgCol)

elas_esgcol_br

#Water connections

elas_lig_br<-0.175-0.072*mean(data$AguaProd)-0.015*mean(data$EsgCol)+0.125*mean(data$ExtRed)-0.113*mean(data$p2)-0.015*mean(data$p3)+0.093*mean(data$LigAtv)

elas_lig_br

#Length network

elas_ext_br<--0.166+0.335*mean(data$AguaProd)-0.028*mean(data$EsgCol)+0.125*mean(data$LigAtv)-0.005*mean(data$p2)-0.402*mean(data$p3)-0.579*mean(data$ExtRed)

elas_ext_br

#Share labor

share_labor_br<-0.266-0.051*mean(data$AguaProd)+0.037*mean(data$EsgCol)-0.113*mean(data$LigAtv)-0.005*mean(data$ExtRed)+0.049*mean(data$p2)-0.228*mean(data$p3)

share_labor_br

#Share energy

share_energy_br<-0.206+0.408*mean(data$AguaProd)-0.032*mean(data$EsgCol)-0.015*mean(data$LigAtv)-0.402*mean(data$ExtRed)-0.228*mean(data$p2)+0.155*mean(data$p3)

share_energy_br

