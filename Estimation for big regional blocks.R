#setando diretorio #

## High Blocks ##

setwd("C:\\Program Files\\R\\Dados")

# pacote para geração das tabelas #

install.packages("stargazer")

list.files()

#carregando pacotes #
library(stargazer)
library(readxl)
library(plm)

#upload base de dados #


data<-readxl::read_excel("C:\\Program Files\\R\\Dados\\Base SNIS_High.xlsx")

#pdata<-pdata.frame(data, index=c("Bloco","Ano"))#



# Visualizando os dados

#View(data)

#Variavel para custo

data$ct<-log(data$FN015/mean(data$FN015))-data$p1

#mean(data$ct)

# Outputs #

data$AguaProd<-log(data$AG006/mean(data$AG006))
data$EsgCol<-log(data$ES005/mean(data$ES005))

#valores médios

#mean(data$AguaProd)
#mean(data$EsgCol)

#Economias Ativas # 

data$LigAtv<-log(data$AG002/mean(data$AG002))


#mean(data$LigAtv)

# Estoque de capital #

data$ExtRed<-log(data$AG005/mean(data$AG005))

#mean(data$ExtRed)


# Preço dos insumos, preço de outros serviços será usado como referencia ##

data$p1<-log(data$p_s/mean(data$p_s))

#mean(data$p1)

data$p2<-log(data$p_l/mean(data$p_l))-data$p1

#mean(data$p2)

data$p3<-log(data$p_e/mean(data$p_e))-data$p1

#mean(data$p3)

# Variaveis Translog Function ## 

#v1

data$AguaProd<-log(data$AG006/mean(data$AG006))
#mean(data$AguaProd)
#v2

data$EsgCol<-log(data$ES005/mean(data$ES005))
#mean(data$EsgCol)
#v3 

data$ExtRed<-log(data$AG005/mean(data$AG005))
#mean(data$ExtRed)
#v4 

data$LigAtv<-log(data$AG002/mean(data$AG002))
#mean(data$LigAtv)
#v5

data$p2<-log(data$p_l/mean(data$p_l))-data$p1 
#mean(data$p2)
#v6

data$p3<-log(data$p_e/mean(data$p_e))-data$p1 
#mean(data$p3)
#v7

# Termos de Interação para Agua Produzida

#v8

data$v1<-((data$AguaProd)*(data$EsgCol))

#v9

data$v2<-((data$AguaProd)*(data$LigAtv))

#v10

data$v3<-((data$AguaProd)*(data$ExtRed))
#v11

data$v4<-((data$AguaProd)*(data$p2))/data$p1

#v12

data$v5<-((data$AguaProd)*(data$p3))/data$p1


# Termos de Interação para Esgoto Tratado

#v14

data$v6<-((data$EsgCol)*(data$LigAtv))
#mean(data$v13)
#v15

data$v7<-((data$EsgCol)*(data$ExtRed))
#mean(data$v14)
#v16

data$v8<-((data$EsgCol)*(data$p2))/data$p1
#mean(data$v15)
#v17

data$v9<-((data$EsgCol)*(data$p3))/data$p1
#mean(data$v16)
#v18

#v19

data$v10<-0.5*((data$EsgCol)*(data$EsgCol))
#mean(data$v17)
#Termos de Interação para ligação ativa

#v20

data$v11<-((data$LigAtv)*(data$ExtRed))

#v21

data$v12<-((data$LigAtv)*(data$p2))/data$p1
#mean(data$v19)
#v22

data$v13<-((data$LigAtv)*(data$p3))/data$p1
#mean(data$v20)
#v23



#v24

data$v14<-0.5*((data$LigAtv)*(data$LigAtv))

#mean(data$v21)

#Termos de Interação para ExtRed

#v25

data$v15<-((data$ExtRed)*(data$p2))/data$p1
#mean(data$v22)
#v26

data$v16<-((data$ExtRed)*(data$p3))/data$p1
#mean(data$v23)
#v27

#v28

data$v17<-0.5*((data$ExtRed)*(data$ExtRed))
#mean(data$v24)

#Termos de Interação para preço trabalho p2

#v29

data$v18<-((data$p2)*(data$p3))/data$p1
#mean(data$v25)
#v30

#v31

data$v19<-0.5*((data$p2)*(data$p2))
#mean(data$v26)
#Termos de Interaçaõ para preço energia p3

#v32


#v33

data$v20<-0.5*(data$p3)*(data$p3)
#mean(data$v27)
#Finalmente para v4

# E termo quadratico para agua que havia esquecido

data$v21<-0.5*((data$AguaProd)*(data$AguaProd))
#mean(data$v28)

#Modelo single cost equation#

est1<-plm(ct~AguaProd+EsgCol+LigAtv+ExtRed+p2+p3+v8+v9+v10+v11+v12+v14+v15+
            v16+v17+v19+v20+v21+factor(data$Estado), data, model="pooling")

summary(est1)
stargazer(est1)   
#View(data)

#############Cost share equations##########

#share labor

data$s_l<-log(data$FN010/mean(data$FN015))-data$p1

#share energia

data$s_e<-log(data$FN013/mean(data$FN015))-data$p1

#share serviços

data$s_s<-log(data$FN014/mean(data$FN015))-data$p1

#mean(pdata$s_l)

#Criando matriz de restrições para estimação do sistema de equações

R<-matrix(0,nrow=14,ncol=42)

R[1,6]<-R[2,7]<-R[3,11]<-R[4,12]<-R[5,15]<-R[6,16]<-R[7,19]<-R[8,20]<-R[9,22]<-R[10,23]<-R[11,25]<-R[12,25]<-R[13,26]<-R[14,27]<-1
R[1,29]<-R[2,36]<-R[3,30]<-R[4,37]<-R[5,31]<-R[6,38]<-R[7,32]<-R[8,39]<-R[9,33]<-R[10,40]<-R[11,35]<-R[12,41]<-R[13,34]<-R[14,42]<--1

#R

#Estimando o sistema de equações de custos #

est2<-plm(list(ct~AguaProd+EsgCol+LigAtv+ExtRed+p2+p3+v1+v2+v3+v4+v5+v6+v7+
                 v8+v9+v10+v11+v12+v13+v14+v15+v16+v17+v18+v19+v20+v21,
               s_l~AguaProd+EsgCol+LigAtv+ExtRed+p2+p3,
               s_e~AguaProd+EsgCol+LigAtv+ExtRed+p2+p3),data,model="random", random.method = "nerlove",restrict.matrix = R)
summary(est2)

# CAlculo das elasticidades 

elas_agua3<--0.444-2.699*mean(data$EsgCol)-8.7*mean(data$LigAtv)+12.766*mean(data$ExtRed)+1.104*mean(data$p2)-0.348*mean(data$p3)+0.326*mean(data$AguaProd)

elas_agua3

elas_esgcol3<-1.502-2.699*mean(data$AguaProd)+2.432*mean(data$LigAtv)-1.695*mean(data$ExtRed)-0.082*mean(data$p2)-0.185*mean(data$EsgCol)+0.656*mean(data$EsgCol)

elas_esgcol3

elas_ligatv3<-2.459-8.7*mean(data$AguaProd)+2.432*mean(data$EsgCol)-15.48*mean(data$ExtRed)-1.485*mean(data$p2)-0.194*mean(data$p3)+14.024*mean(data$LigAtv)

elas_ligatv3

elas_extred3<--4.946+12.766*mean(data$AguaProd)-1.695*mean(data$EsgCol)-15.48*mean(data$LigAtv)+1.511*mean(data$p2)+0.743*mean(data$p3)+11.909*mean(data$ExtRed)

elas_extred3

share_labor3<-0.544-0.894*mean(data$AguaProd)-0.006*mean(data$EsgCol)+0.585*mean(data$LigAtv)-0.345*mean(data$ExtRed)+1.044*mean(data$p2)+0.098*mean(data$p3)

share_labor3

share_energy3<-0.065+0.277*mean(data$AguaProd)+0.141*mean(data$EsgCol)+0.164*mean(data$LigAtv)-0.579*mean(data$ExtRed)+0.098*mean(data$p2)+1.023*mean(data$p3)

share_energy3
