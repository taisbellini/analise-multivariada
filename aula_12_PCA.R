require(GGally)
require(mvShapiroTest)
#############################Exemplo 8.1#########################################

E=matrix(c(1,-2,0,-2,5,0,0,0,2), ncol= 3)
E        
k=eigen(E)$values 
k
sum(k)
e=eigen(E)$vectors
e
p<-k[1]/sum(k)
p

#########################corr X com Y#####################
rx1y1=e[1,1]*sqrt(k[1])/sqrt(E[1,1])
rx2y1= e[2,1]*sqrt(k[1])/sqrt(E[2,2])
rx3y1= e[3,1]*sqrt(k[1])/sqrt(E[3,3])         

rx1y2=e[1,2]*sqrt(k[2])/sqrt(E[1,1])
rx2y2= e[2,2]*sqrt(k[2])/sqrt(E[2,2])
rx3y2= e[3,2]*sqrt(k[2])/sqrt(E[3,3])         

rx1y3=e[1,3]*sqrt(k[3])/sqrt(E[1,1])
rx2y3= e[2,3]*sqrt(k[3])/sqrt(E[2,2])
rx3y3= e[3,3]*sqrt(k[3])/sqrt(E[3,3])

co=matrix(c(rx1y1,rx2y1,rx3y1,rx1y2,rx2y2,rx3y2,rx1y3, rx2y3,rx3y3), ncol=3)

#Colunas: componntes principais
#Linhas: variaveis
#Celulas: correlacao da variavel com o componente principal

##########################################################3
#########################Exemplo 8.2#####################3
###########################################################


E=matrix(c(1,4,4,100), ncol=2)
E        
k=eigen(E)$values 
k
sum(k)
e=eigen(E)$vectors
e
p1<-k[1]/sum(k)
p1

#########################corr X com Y#####################
rx1y1=e[1,1]*sqrt(k[1])/sqrt(E[1,1])
rx2y1= e[2,1]*sqrt(k[1])/sqrt(E[2,2])
      

rx1y2=e[1,2]*sqrt(k[2])/sqrt(E[1,1])
rx2y2= e[2,2]*sqrt(k[2])/sqrt(E[2,2])
       

co=matrix(c(rx1y1,rx2y1,rx1y2,rx2y2), ncol=2)


#######PCA na matriz de correlacao#####################
C=solve(sqrt(diag(2)*E))%*%E%*%solve(sqrt(diag(2)*E))
C        
kk=eigen(C)$values 
kk
sum(kk)
ee=eigen(C)$vectors
ee
## como sao apenas duas varaiveis, o resultados em ee ficam meio iguais, pq so tem uma covariancia. Nao eh regra.
pp1<-kk[1]/sum(kk)
pp1


rx1y1=ee[1,1]*sqrt(kk[1])/sqrt(C[1,1])
rx2y1= ee[2,1]*sqrt(kk[1])/sqrt(C[2,2])

rx1y2=ee[1,2]*sqrt(kk[2])/sqrt(C[1,1])
rx2y2= ee[2,2]*sqrt(kk[2])/sqrt(C[2,2])


co_cor=matrix(c(rx1y1,rx2y1,rx1y2,rx2y2), ncol=2)


require(ellipse)
windows()
par(mfrow=c(1,2))
plot(ellipse(E,centre=c(0,0),level=0.05,npoints=10000),type='l',asp=1, xlab="x1", ylab="x2", main="PCA na matriz de covariancia")
plot(ellipse(C,centre=c(0,0),level=0.05,npoints=10000),type='l',asp=1, xlab="x1", ylab="x2", main="PCA na matriz de correlacao")


#########################################################
#################Exemplo USArrests
#N?mero de pris?es efetuadas (por 100.000 habitantes) em 50 estados americanos em 1973.
#Vari?veis: assalto, viol?ncia sexual (VS), assassinato e porcentagem de moradores na ?rea urbana (PMAU).
x=USArrests
View(x)

############Analise descritiva Explorat?ria
colMeans(x)
cov(x)
cor(x)
summary(x)

#windows()
#plot(x)

require(GGally)
windows()
ggpairs(x)

################################################################
###############################################################
##Testando normalidade
require(car)
windows()
par(mfrow=c(2,2)) 
for( i in 1:ncol(x)){
  qqPlot(x[,i], dist="norm", mean=0, sd=1, main=paste("x_",i), ylab=paste("empirical"))}

W=rep(0,ncol(x))
for(k in 1:ncol(x)){
  W[k]=shapiro.test(x[,k])$p.value }
W


# teste normalidade multivariado
d2<-mahalanobis(x, colMeans(x), cov(x), inverted = FALSE)  ##dist multvariada ~~ QUi-q
d2

windows()
qqPlot(d2, dist="chisq", df=ncol(x), main=paste("Chi-dist"), ylab=paste("d2"))

x=as.matrix(x)
mvShapiro.Test(x)  ## teste Shapiro 


###########################################################
############################################################
################PCA usando funcao prcomp

pc<-prcomp(x, scale.=F)  
pc
summary(pc)
 
#par(mfrow=c(1,2))
screeplot(pc, type = "l")
 
windows()
biplot(pc)
 
# windows()
# biplot(pc)
 
y<-pc$x
y

plot(y[,1],y[,2])

 ##########Extraindo os autopares diretamente atraves da diagonalizacao de S=cov(x)
S=cov(x)
k=eigen(S)$values 
k
sum(k)
e=eigen(S)$vectors
e


###################################################
#Extraindo os pc com os dados padronizados
pcn<-prcomp(x, scale.=T)  
pcn
summary(pcn)

# Antes, a variavel assalto estava dominando PC1. Agora, temos um maior equilibrio, 
# com os dados padronizados (scale = T)

#par(mfrow=c(1,2))
screeplot(pcn, type = "l")

windows()
biplot(pcn)

yn<-pcn$x
yn

## Ordenando os dados
ss=data.frame(ord=seq(1,50,by=1), y=yn[,1])
ss_ord = ss[order(ss[,2], decreasing=T),]

windows() 
plot(yn[,1],yn[,2])
############Da no mesmo padronizar os dados antes (usando a funcao scale) e aplicar 
#a funcao prcomp com scale.=F
#QUando padronizamos os dados a matriz de cov se torna matriz de correlacao S=R 
z=scale(x)
pcc<-prcomp(z, scale.=F) 
pcc  #padronizando antes
pcn # padronizano apenas dentro da funcao prcomp

#################################################################################
#####################################################################################

##Testando normalidade do pc 1 e pc2
windows()
par(mfrow=c(1,2)) 
for( i in 1:2){
  qqPlot(yn[,i], dist="norm", mean=0, sd=1, main=paste("yn_",i), ylab=paste("empirical"))}

W=rep(0,2)
for(k in 1:2){
  W[k]=shapiro.test(yn[,k])$p.value }
W

require(mvShapiroTest) # teste normalidade multivariado

yn=as.matrix(yn)
mvShapiro.Test(yn)  ## teste Shapiro
# n rejeita (quando botamos apenas o x, quase rejeita (a 5% rejeita))

d2<-mahalanobis(yn, colMeans(yn), cov(yn), inverted = FALSE)  ##dist multvariada ~~ QUi-q
d2

windows()
qqPlot(d2, dist="chisq", df=ncol(yn), main=paste("Chi-dist"), ylab=paste("d2"))


##########################################################33
#############################################################
#Exemplo iris
#comprimento e largura
#da sepala e comprimento e largura da petala.

X=iris
x=X[,-5]

windows()
#ggpairs(x)
ggpairs(X[,-5], mapping=aes(colour=as.factor(X[,5])))


pc<-prcomp(x, scale.=T)  
pc
summary(pc)

windows()
#par(mfrow=c(1,2))
screeplot(pc, type = "l")

windows()
biplot(pc, xlabs=iris[,5])


################################################################
######################################################################
##########################Exemplo  salgado##########################
sabor<-c(2.75, 3.9, 3.12, 4.58, 3.97, 3.01, 4.19, 3.82)
aroma<-c(4.03, 4.12, 3.97, 4.86, 4.34, 3.98, 4.65, 4.12)
massa<-c(2.8, 3.4, 3.62, 4.34, 4.28, 2.9, 4.52, 3.62)
recheio<-c(2.62, 3.52, 3.05, 4.82, 4.98, 2.82, 4.77, 3.71)

x<-cbind(sabor, aroma, massa, recheio)
x

summary(x)
cov(x)
cor(x)
pc<-prcomp(x, scale=F)
pc
summary(pc)

windows()
#par(mfrow=c(1,2))
screeplot(pc, type = "l")

windows()
biplot(pc)

y=pc$x


###Ordenar os salgados 
ss=data.frame(ord=seq(1,8, by= 1),y=y[,1])
ss_ord=ss[order(ss[,2], decreasing=TRUE), ]


######################################################################
##########################Exemplo SOLO##########################
#n=25 amostras de solo
#x1= porcentagem de areia, x2=porcentagem de sedimentos,
#x3=porcentagem de argila, x4=quantidade de material org?nico

read.table("ExemploSolo.txt", header=T,sep="")->x
x=x[,-1]
View(x)

summary(x)
cov(x)
cor(x)
pc<-prcomp(x, scale.= FALSE)
pc
summary(pc)

windows()
par(mfrow=c(1,2))
screeplot(pc, type = "l")
biplot(pc)

windows()
biplot(pc)

y<-pc$x
y

############################################################
#################################################################
#########################outro exemplo
####################################
## Tabela 6.9

read.table("T6-9.dat", header=F,sep="")->y
x=y[,-4]
colnames(x)<-c("comprimento", "largura", "altura")
View(x)


#######visao geom?trica
#ggpairs(x, mapping=aes(colour=as.factor(xx[,4])), upper=list(combo= 'blank', continuous='blank'))
windows()
ggpairs(x, mapping=aes(colour=as.factor(y[,4])))

#pca
pc<-prcomp(x, scale.= FALSE)
pc
summary(pc)

windows()
par(mfrow=c(1,2))
screeplot(pc, type = "l")
biplot(pc)

windows()
biplot(pc)

y<-pc$x
y


###Ordenar os as tartarugas 
ss=data.frame(ord=seq(1,48, by= 1),y=y[,1])
ss_ord=ss[order(ss[,2], decreasing=TRUE), ]

############################################################################
#############################################################################
#########Exemplo 9.4 JW (tabela 8.4 do JW)
#retornos semanais de acoes em 103 semanas referentes a 5 companhias
read.table("T8-4.dat", header=F,sep="")->x
colnames(x) <-c("JPMorgan","Citibank","WellsFargo","RDShell","Texaco")

