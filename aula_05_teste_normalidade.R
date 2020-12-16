########################################################################
########################verificar normalidadde multivariada####################
##########################################################################
require(mvtnorm)
require(plot3D)
require(mvShapiroTest)
require(car)

x=iris[,-5]  #banco iris
#x=mtcars ## banco mtcars
#x=carData::Pottery
#x=carData::States[,-1]
#x<-as.matrix(read.table("T4-3.dat", header = F)[,-5]) ### Dados Exemplo 4.14 JW
#x<-read.table("T4-3.dat", header = F)[,-5] ### Dados Exemplo 4.14 JW

#############################################################################
########################Normalidade Multivriada#############################
###############################################################################

#par(mfrow=c(1,2))
plot(x)  ##scatter 2x2

windows()
par(mfrow=c(2,2)) ###### Modificar de acordo com numero de variveis
for(g in 1:ncol(x)){
plot(density(x[,g]), main=paste("x_",g))} ## densidade empirica

windows()
par(mfrow=c(2,2))
for(g in 1:ncol(x)){
  hist(x[,g], xlab=paste("x_",g), main=NULL)}
  
  
#######Verificando Normalidade univariada########
windows()
par(mfrow=c(2,2)) 
for( i in 1:ncol(x)){
  qqPlot(x[,i], dist="norm", mean=0, sd=1, main=paste("x_",i), ylab=paste("empirical"))}

##########KS univariado##################
v=rep(0,ncol(x))
for(j in 1:ncol(x)){
  v[j]=ks.test(scale(x[,j]),"pnorm",0,1)$p.value }
v


#############shapiro univariado###########3
W=v=rep(0,ncol(x))
for(k in 1:4){
W[k]=shapiro.test(x[,k])$p.value }
W


#############Teste de Normalidade Multivariada#############
x=as.matrix(x)
mvShapiro.Test(x)  ## teste Shapiro

#calcula a distancia estatistica 
#depois validamos se o d2 tem dist quiquadrado (que eh o caso da normal multivariada)
d2<-mahalanobis(x, colMeans(x), cov(x), inverted = FALSE)  ##dist multvariada ~~ QUi-q
d2

windows()
qqPlot(d2, dist="chisq", df=ncol(x), main=paste("Chi-dist"), ylab=paste("d2"))

