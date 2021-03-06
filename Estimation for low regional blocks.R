#set directory#

## Low-level blocks of utilities ##
## Water produced in the range of [0,50.000) m�

setwd("C:\\Program Files\\R\\Dados")

list.files()

#upload packages#

library(readxl)
library(plm)

#upload database#

data<-readxl::read_excel("C:\\Program Files\\R\\Dados\\Base SNIS_Low.xlsx")


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


#Modelsingle cost equation#

#est1<-plm(ct~AguaProd+EsgCol+LigAtv+ExtRed+p2+p3+v8+v9+v10+v11+v12+v14+v15+
#            v16+v17+v19+v20+v21+factor(data$Estado), data, model="pooling")

#summary(est1)

#############Cost share equations##########

#share labor

data$s_l<-log(data$FN010/mean(data$FN015))#-data$p1

#share energy

data$s_e<-log(data$FN013/mean(data$FN015))#-data$p1
#share services

#data$s_s<-log(data$FN014/mean(data$FN015))-data$p1


#Criando matriz de restri��es para estima��o do sistema de equa��es

R<-matrix(0,nrow=14,ncol=75)

R[1,6]<-R[2,7]<-R[3,11]<-R[4,12]<-R[5,16]<-R[6,17]<-R[7,20]<-R[8,21]<-R[9,23]<-R[10,24]<-R[11,26]<-R[12,26]<-R[13,27]<-R[14,28]<-1
R[1,62]<-R[2,69]<-R[3,63]<-R[4,70]<-R[5,64]<-R[6,71]<-R[7,65]<-R[8,72]<-R[9,66]<-R[10,73]<-R[11,68]<-R[12,74]<-R[13,67]<-R[14,75]<--1

#R

#Estimating SUR model with specific effects of year and block #

est1<-plm(list(ct~AguaProd+EsgCol+LigAtv+ExtRed+p2+p3+AgEsg+AgLig+AgExt+Agp2+Agp3+AgAg+
                 +EsgLig+EsgExt+Esgp2+Esgp3+EsgEsg+LigExt+Ligp2+Ligp3+LigLig+Extp2+Extp3+ExtExt+p2p3+p2p2
               +p3p3+factor(data$Bloco)+factor(data$Ano),
               s_l~AguaProd+EsgCol+LigAtv+ExtRed+p2+p3,
               s_e~AguaProd+EsgCol+LigAtv+ExtRed+p2+p3),data,model="random",
          random.method = "nerlove",restrict.matrix = R)
summary(est1)

# Elasticities of translog cost function

#Water produced

elas_agua_low<-0.710-0.025*mean(data$EsgCol)+0.005*mean(data$LigAtv)+0.382*mean(data$ExtRed)+0.075*mean(data$p2)+0.193*mean(data$p3)-0.337*mean(data$AguaProd)

elas_agua_low

#Sewage collected

elas_esg_low<-0.105-0.025*mean(data$AguaProd)-0.013*mean(data$LigAtv)-0.018*mean(data$ExtRed)-0.031*mean(data$p2)-0.010*mean(data$p3)+0.037*mean(data$EsgCol)

elas_esg_low

#Water connections

elas_lig_low<-0.046+0.0053*mean(data$AguaProd)-0.013*mean(data$EsgCol)-0.014*mean(data$ExtRed)-0.073*mean(data$p2)-0.032*mean(data$p3)+0.043*mean(data$LigAtv)

elas_lig_low

#Length network

elas_ext_low<--0.257+0.382*mean(data$AguaProd)-0.018*mean(data$EsgCol)-0.014*mean(data$LigAtv)+0.225*mean(data$p2)-0.290*mean(data$p3)-0.574*mean(data$ExtRed)

elas_ext_low

#Share labor

share_labor_low<-0.518+0.075*mean(data$AguaProd)-0.031*mean(data$EsgCol)-0.073*mean(data$LigAtv)+0.225*mean(data$ExtRed)+0.142*mean(data$p2)-0.231*mean(data$p3)

share_labor_low

#Share energy

share_energy_low<-0.087+0.193*mean(data$AguaProd)-0.010*mean(data$EsgCol)-0.032*mean(data$LigAtv)-0.290*mean(data$ExtRed)-0.231*mean(data$p2)+0.314*mean(data$p3)

share_energy_low


## restriciion matrix for Estimation the SUR model without year and block specific effects

Z<-matrix(0,nrow=14,ncol=42)

Z[1,6]<-Z[2,7]<-Z[3,11]<-Z[4,12]<-Z[5,16]<-Z[6,17]<-Z[7,20]<-Z[8,21]<-Z[9,23]<-Z[10,24]<-Z[11,26]<-Z[12,26]<-Z[13,27]<-Z[14,28]<-1
Z[1,29]<-Z[2,36]<-Z[3,30]<-Z[4,37]<-Z[5,31]<-Z[6,38]<-Z[7,32]<-Z[8,39]<-Z[9,33]<-Z[10,40]<-Z[11,35]<-Z[12,41]<-Z[13,34]<-Z[14,42]<--1


## Estimation without year and block specific effects


est2<-plm(list(ct~AguaProd+EsgCol+LigAtv+ExtRed+p2+p3+AgEsg+AgLig+AgExt+Agp2+Agp3+AgAg+
                 +EsgLig+EsgExt+Esgp2+Esgp3+EsgEsg+LigExt+Ligp2+Ligp3+LigLig+Extp2+Extp3+ExtExt+p2p3+p2p2
               +p3p3,
               s_l~AguaProd+EsgCol+LigAtv+ExtRed+p2+p3,
               s_e~AguaProd+EsgCol+LigAtv+ExtRed+p2+p3),data,model="random", random.method = "nerlove",restrict.matrix = Z)
summary(est2)

#Elasticities

elas_agua_low2<-0.947+0.030*mean(data$EsgCol)+0.086*mean(data$LigAtv)+0.203*mean(data$ExtRed)+0.058*mean(data$p2)+0.262*mean(data$p3)-0.323*mean(data$AguaProd)

elas_agua_low2

#Sewage collected

elas_esg_low2<-0.079+0.030*mean(data$AguaProd)-0.030*mean(data$LigAtv)-0.036*mean(data$ExtRed)-0.018*mean(data$p2)-0.014*mean(data$p3)+0.042*mean(data$EsgCol)

elas_esg_low2

#Water connections

elas_lig_low2<-0.007+0.086*mean(data$AguaProd)-0.030*mean(data$EsgCol)-0.013*mean(data$ExtRed)-0.060*mean(data$p2)-0.038*mean(data$p3)+0.022*mean(data$LigAtv)

elas_lig_low2

#Length network

elas_ext_low2<--0.796+0.203*mean(data$AguaProd)-0.036*mean(data$EsgCol)-0.013*mean(data$LigAtv)+0.142*mean(data$p2)-0.385*mean(data$p3)-0.555*mean(data$ExtRed)

elas_ext_low2

#Share labor

share_labor_low2<-0.369+0.058*mean(data$AguaProd)-0.018*mean(data$EsgCol)-0.060*mean(data$LigAtv)+0.142*mean(data$ExtRed)+0.106*mean(data$p2)-0.283*mean(data$p3)

share_labor_low2

#Share energy

share_energy_low2<-0.143+0.262*mean(data$AguaProd)-0.014*mean(data$EsgCol)-0.038*mean(data$LigAtv)-0.385*mean(data$ExtRed)-0.283*mean(data$p2)+0.358*mean(data$p3)

share_energy_low2

