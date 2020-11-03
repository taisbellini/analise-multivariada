if (!require(ellipse)) install.packages('ellipse') else require(ellipse)
if (!require(Hotelling)) install.packages('Hotelling') else require(Hotelling)
if (!require(ggplot2)) install.packages('ggplot2') else require(ggplot2)
if (!require(GGally)) install.packages('GGally') else require(GGally)
if (!require(mvShapiroTest)) install.packages('mvShapiroTest') else require(mvShapiroTest)
if (!require(car)) install.packages('car') else require(car)
if (!require(qcc)) install.packages('qcc') else require(qcc)
if (!require(corrplot)) install.packages('corrplot') else require(corrplot)
if (!require(scatterplot3d)) install.packages('scatterplot3d') else require(scatterplot3d)
if (!require(rgl)) install.packages('rgl') else require(rgl)
if (!require(ade4)) install.packages('ade4') else require(ade4) # multivariate analysis
if (!require(mvtnorm)) install.packages('mvtnorm') else require(mvtnorm)
if (!require(ggpubr)) install.packages('ggpubr') else require(ggpubr)
if (!require(TeachingDemos)) install.packages('TeachingDemos') else require(TeachingDemos)

##############Data sets no R
require(carData)
require(datasets) 
require(mlbench)
if(!require(AppliedPredictiveModeling)) install.packages('AppliedPredictiveModeling') else require(AppliedPredictiveModeling)
library(help = "carData")
library(help = "datasets")
library(help = "mlbench")
library(help = "AppliedPredictiveModeling")

###########################################################################
######################AULA 1- INTRO##############################
####Análise Exploratória

#Exemplo 1-  car data (pacote "datasets")
X=cars  ##x1 velocidade   ###x2 distancia até a parada
head(X)

#################Medidas-resumo
x_bar=colMeans(X) #vetor de medias amostrais
S=cov(X)  ###matriz de covariancias 
R=cor(X)  ####correlacoes amostrais

##############Visão univariada
par(mfrow=c(1,ncol(X)))
for (i in 1:ncol(X)){
  plot(density(X[,i]), main=paste("x",i), xlab=paste("x",i))  
}

par(mfrow=c(1,ncol(X)))
for (i in 1:ncol(X)){
  boxplot((X[,i]),main=paste("x",i) )  
}

par(mfrow=c(1,ncol(X)))
for (i in 1:ncol(X)){
  hist((X[,i]), main=paste("x",i), xlab=paste("x",i))  
}
#############################################

ggpairs(X) ##########scatter 2x2

ggpairs(X, upper=list(combo= 'blank', continuous='blank'))

ggpairs(X, upper=list(combo= 'box_no_facet', continuous='density'))

ggpairs(X, upper=list(combo= 'box_no_facet', continuous='points'))



###################################################################
###############Exemplo 2  Data on transaction times in branch offices of a large Australian bank.
X=carData::Transact

#################Medidas-resumo
x_bar=colMeans(X) #vetor de medias amostrais
S=cov(X)  ###matriz de covariancias 
R=cor(X)  ####correlacoes amostrais

##############Visão univariada
par(mfrow=c(1,3))
for (i in 1:ncol(X)){
  plot(density(X[,i]), main=paste("x",i), xlab=paste("x",i))  
}

par(mfrow=c(1,3))
for (i in 1:ncol(X)){
  boxplot((X[,i]), main=paste("x",i))  
}

par(mfrow=c(1,3))
for (i in 1:ncol(X)){
  hist((X[,i]), main=paste("x",i), xlab=paste("x",i))  
}
#############################################
ggpairs(X) ##scatter 2x2

#########scaterrs 3D

# Observe que os pontos nao formam uma superficies
# Eu poderia usar 1 unica variavel resumo para explicar as 3 variaveis e levar adiante apens 1 ao inves de 3
# Estimar por regressao linear

windows()
scatterplot3d(X, pch=16, angle = 55, color="steelblue", grid=T, box=T)

scatter3d(x= X$t1, y= X$t2, z=X$time, surface=T, ellipsoid = F, grid = FALSE) #fit = "smooth")  ###########scatter 3D###########


###############################################################
################## Exemplo 3 - simulado ################################
set.seed(70)
sig=matrix(c(9,4,4,4), ncol=2)
n=20
X_1=rmvnorm(n, mean=c(1,2), sigma=sig, method="eigen") 
X_2=rmvnorm(n, mean=c(8,8), sigma=sig, method="eigen") 
X_3=rmvnorm(n, mean=c(18,20), sigma=sig, method="eigen") 
gru<-as.factor(gl(3,20, length=60, labels = c("A", "B", "C")))

X=data.frame(rbind(X_1,X_2,X_3))

X=cbind(X,gru)
colnames(X)<-c("x1", "x2", "grupo")

#X=data.frame(X,gru)
######Medidas-resumo de cada grupo
X1_bar=colMeans(X_1); S1=cov(X_1)
X2_bar=colMeans(X_2); S1=cov(X_2)
X3_bar=colMeans(X_3); S1=cov(X_3)



################# visao geometrica

ggpairs(X[,1:2], mapping=aes(colour=as.factor(X[,3])))
#ggpairs(X[,-3], mapping=aes(colour=as.factor(X[,3])), upper=list(combo= 'blank', continuous='blank'))


ggscatter(X,x="x1", y= "x2", color="grupo", pallete="jco", shape="grupo", ellipse=TRUE, ellipse.level=0.95, mean.point = TRUE)
#########Scatter 2x2


############Exemplo 4############################
#####################################################
##################################################3333
####Banco iris
X=iris
ggpairs(X[,-5], mapping=aes(colour=as.factor(X[,5])), upper=list(combo= 'blank', continuous='blank'))


####com apenas 2 variáveis "Petal.Length"and "Sepal.Length"
ggscatter(iris,x="Petal.Length", y= "Sepal.Length", color="Species", pallete="jco", shape="Species", mean.point = TRUE, ellipse=TRUE, ellipse.level=0.95)
#########Scatter 2x2 



###################################################
####################Exemplo 5 -  Carta de controle de Hotelling

sig=matrix(c(9,4,4,4), ncol=2)
n=50
X=rmvnorm(n, mean=c(1,2), sigma=sig, method="eigen") 
X_new=rmvnorm(10, mean=c(10,10), sigma=sig, method="eigen")
q <- mqcc(X, type = "T2.single", newdata = X_new)
summary(q)
windows()
ellipseChart(q)

#ellipseChart(q, show.id = TRUE)


################################Exemplo 6########
###Banco de dados "mtcars". 

#Comparar de variáveis de desempenho de veículos com número distintos de marchas (4, 5 ou 6 marchas).
#Consumo de combustível ($X_1$), Peso ($X_2$), Tempo pra percorrer 1/4 de milha ($X_3$)
#e Potência bruta ($X_4$).


X=mtcars
X=X[,c(1,4,6,7,10)]
#gear=1,2,3,4,5 

#mvShapiro.Test(X[,-5]) #Normalidade multivariada

#######################PLot visao geométrica  ggpairs
windows()
ggpairs(X[,-5], mapping=aes(colour=as.factor(X[,5])), upper=list(combo= 'blank', continuous='blank'))


#b) #########Tentar agrupar observacoes
X=mtcars
X=X[,c(1,4,6,7)]

install.packages('aplpack')
library(aplpack)
windows()
faces(mtcars, main='Motor Trend Cars')  ####Funcao faces  (Chernoff Faces)
faces(X[1:20,], main='Motor Trend Cars')  ####Funcao faces  (Chernoff Faces)


windows()
stars(scale(X[1:20,]), len = 0.6, key.loc = c(14,3),
      main = "Motor Trend Cars", draw.segments = FALSE,
      frame.plot = TRUE, nrow = 4, cex = .7)  ########Grafico de Estrelas


