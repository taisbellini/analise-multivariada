#################################################
library(MASS)
library(xtable)
library(ggplot2)
library(tidyverse)
library(caret)  ##Te fornece medidas de ajuste (matriz de confus?o)#############Banco iris


#######Exemplo iris
x=iris
View(iris)


#comparando setosa e versicolor
Sp = rep(c("SE","VE"), rep(50,2))
xx=data.frame(especie=Sp,x[-c(101:150),1:4])
dc <- lda(xx[,1]~., xx[,2:5])
dc

dc$scaling  ####coeficientes da funcao

pred<-predict(dc)$class ##classificando as observacoes
y <-predict(dc)$x ########gerando os escores discriminantes y-m


tc <- table(xx[,1],pred) # Tabela de classifica??o
#xtable(tc)
TEA=(tc[1,2]+tc[2,1])/nrow(xx)  ########percentual empirico de erro classificacao


####outra funcao pra predizer (fornece as sprobs a posteriori)
p.1<-dc%>%predict(xx)

####################Avaliar ajuste
confusionMatrix(as.factor(xx[,1]),pred)
########################################

# score esta centralizado em zero (y-m)
yy=data.frame(Sp,y)
yy_1=mean(yy[yy[,1]=="SE",2])
yy_2=mean(yy[yy[,1]=="VE",2])

##boxplot dos escores
boxplot(yy[,2]~yy[,1], cex=1.2,cex.lab=1.2, xlab = "especie", ylab="Scores")


##densidade empirica dos escores 
windows()
plot(dc)

##densidade empirica dos escores usando ggplot

windows()
ggplot(yy, aes(x = y)) +
  geom_density(aes(color = Sp))
###################################################

##calculando "na mao" os escores e a constante discriminante
x_SE=xx[xx[,1]=="SE",2:5]
x_VE=xx[xx[,1]=="VE",2:5]

x_SE_bar=colMeans(x_SE)
x_VE_bar=colMeans(x_VE)
SSp=(cov(x_SE)*(nrow(x_SE)-1)+cov(x_VE)*(nrow(x_VE)-1))/(nrow(x_SE)+nrow(x_VE)-2)

b<-t(x_SE_bar-x_VE_bar)%*%solve(SSp)  #coeficientes b
#b<-t(x_VE_bar-x_SE_bar)%*%solve(SSp)  #coeficientes b

f=sqrt(b%*%SSp%*%t(b))  #####b%*%SSp%*%t(b)

bb<-as.vector(b)/as.numeric(f)
# eh igual ao dc$scaling

Y <- as.vector(bb%*%t(xx[,2:5]))  #escores discriminantes  corrigidos y




#m = 0.5*(t(x_SE_bar-x_VE_bar))%*%solve(SSp)%*%((x_SE_bar+x_VE_bar))###constante m discriminante

#slide 30
m = 0.5*(bb%*%((x_SE_bar+x_VE_bar)))###constante m discriminante corrigida

Y_m<- as.vector(bb%*%t(xx[,2:5]))- as.numeric(m) #escores discriminantes  corrigidos y-m

YY=data.frame(Sp,Y_m)
YY_1=mean(YY[yy[,1]=="SE",2]) 
YY_2=mean(YY[yy[,1]=="VE",2])

#slide 23
delta2 <-  t(x_SE_bar-x_VE_bar)%*%solve(SSp)%*%(x_SE_bar-x_VE_bar)
TOE=pnorm(-sqrt(delta2)/2) ########percentual teorico de erro de classificacao 

  

##densidade empirica dos escores ## usando ggplot
YY=data.frame(xx[,1],Y)
windows()
ggplot(YY, aes(x = YY[,2])) +
  geom_density(aes(color = as.factor(YY[,1])))
##############


###############################################################################
###############################################################
#####################################################################
#comparando setosa e virginica
Sp = rep(c("SE","VI"), rep(50,2))
xx=data.frame(especie=Sp,x[-c(51:100),1:4])
dc <- lda(xx[,1]~., xx[,2:5])
dc

dc$scaling  ####coeficientes da funcao

pred<-predict(dc)$class ##classificando as observacoes
y <-predict(dc)$x ########gerando os escores discriminantes y-m


tc <- table(xx[,1],pred) # Tabela de classifica??o
TEA=(tc[1,2]+tc[2,1])/nrow(xx)

####################################################################
####################################################
#########################################################
#comparando versicolor e virginica
Sp = rep(c("VE","VI"), rep(50,2))
xx=data.frame(especie=Sp,x[-c(1:50),1:4])
dc <- lda(xx[,1]~., xx[,2:5])
dc

dc$scaling  ####coeficientes da funcao

pred<-predict(dc)$class ##classificando as observacoes
y <-predict(dc)$x ########gerando os escores discriminantes


tc <- table(xx[,1],pred) # Tabela de classifica??o
TEA=(tc[1,2]+tc[2,1])/nrow(xx)
###################################
##########################################
#Repetindo a comparacao entre versicolor e virginica usando 25 amostras de treino de cada especie
#xy=xx[-c(1:50),]
# Selecionando as amostras
set.seed(333)
treinog1 <- sort(sample(1:50,25,replace=FALSE))
treinog2 <- sort(sample(51:100,25,replace=FALSE))
treino <- c(treinog1,treinog2)

######aplicando  analise discriminante
#Sp = rep(c("VE","VI"), rep(50,2))
#xx=data.frame(Sp,)
dc <- lda(xx[,1]~., xx[,2:5],prior = c(1,1)/2, subset = treino)
dc

dc$scaling  ####coeficientes da funcao

#pred<-predict(dc)$class 
pred<-predict(dc, xx[-treino, ])$class ##classificando as observacoes
y <-predict(dc, xx[-treino, ])$x ########gerando os escores discriminantes y-m


tc <- table(xx[-treino,1],pred) # Tabela de classifica??o
TEA=(tc[1,2]+tc[2,1])/length(treino)
#xtable(tc)

############################percentual teorico de erro de classificacao 
x_VE=xx[treinog1,2:5]
x_VI=xx[treinog2,2:5]

x_VE_bar=colMeans(x_VE)
x_VI_bar=colMeans(x_VI)
SSp=(cov(x_VE)*(nrow(x_VE)-1)+cov(x_VI)*(nrow(x_VI)-1))/(nrow(x_VE)+nrow(x_VI)-2)

b<-t(x_VE_bar-x_VI_bar)%*%solve(SSp)

delta2 <-  t(x_VE_bar-x_VI_bar)%*%solve(SSp)%*%(x_VE_bar-x_VI_bar)
TOE=pnorm(-sqrt(delta2)/2) ########percentual teorico de erro de classificacao 



##################################################################
#####################################################################
##################################################################3
#################Exemplo 11.8 JW
read.table("Wichern_data/T11-2.dat", header=F,sep="")->xx
x=xx[,-2]
colnames(x)=c("site", "fresh", "marine")
View(x)

dc <- lda(x[,1]~., x[,2:3])
dc

dc$scaling  ####coeficientes da funcao

pred<-predict(dc)$class ##classificando as observacoes
y <-predict(dc)$x ########gerando os escores discriminantes  y-m

windows()
plot(dc)

tc <- table(x[,1],pred)

###################densidades
yy=data.frame(x[,1],y)

windows()
ggplot(yy, aes(x = yy[,2])) +
  geom_density(aes(color = as.factor(yy[,1])))

###############
x_al=x[x[,1]==1,2:3]
x_ca=x[x[,1]==2,2:3]

x_al_bar=colMeans(x_al)
x_ca_bar=colMeans(x_ca)
SSp=(cov(x_al)*(ncol(x_al)-1)+cov(x_ca)*(ncol(x_ca)-1))/(ncol(x_al)+ncol(x_ca)-2)

b<-t(x_al_bar-x_ca_bar)%*%solve(SSp)  #coeficientes b

f=sqrt(b%*%t(b)) #####b x t(b)
#f=b%*%SSp%*%t(b)  #####b%*%SSp%*%t(b)

bb<-as.vector(b)/as.numeric(f)

Y <- as.vector(b%*%t(x[,2:3]))  #escores discriminantes  y


m = 0.5*(b%*%((x_al_bar+x_ca_bar)))  ###constante m discriminante

Y_m <- as.vector(b%*%t(x[,2:3])) - as.numeric(m)  #escores discriminantes y-m
##densidade empirica dos escores ## usando ggplot
YY=data.frame(x[,1],Y_m)

windows()
ggplot(YY, aes(x = YY[,2])) +
  geom_density(aes(color = as.factor(YY[,1])))

mean(YY[YY[,1]==1,2]) 
mean(YY[YY[,1]==2,2]) 
