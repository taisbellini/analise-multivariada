#########################################################################
############################################################################
########################Teste de Hip?tese Diferen?a entre 2 M?dias########################

###Amostra pareada
######################Exemplo 1 ########################################
#queremos testar se mud=0
require(ellipse)
require(Hotelling)
require(GGally)
require(mvShapiroTest)
#require(car)
#require(carData)
#quantidade de oxig^enio bioqumico (X1) e da quantidade de solidos suspensos (X2).
#2 laboratorios
x<-matrix(c(6,6,18,8,11,34,28,71,43,33,20,27,23,64,44,30,75,26,124,54,30,14,25,28,36,35,15,44,42,54,34,29,39,15,13,22,29,31,64,30,64,56,20,21), ncol=4)
x
colnames(x)=c("x1-lb1","x2-lb1","x1-lb2","x2-lb2" )
#cor(x)
d1=x[,1]-x[,3]
d2=x[,2]-x[,4]
d<-matrix(c(d1,d2), ncol=2)
d

xx=data.frame(d)
windows()
ggpairs(xx) ##########scatter 2x2


Sd=cov(d)
n=nrow(d)
p=ncol(d)
d_bar=colMeans(d)
alpha=0.05
##############################testando hip?tese de diferenca igual a zero##############3
mu=c(0,0)
T2_cal<-n*mahalanobis(d_bar, mu, cov(d), inverted = FALSE)
T2_cal

q=((n-1)*p)/(n-p)*qf(1-alpha,p,n-p)
q

#qchisq(0.95,2)

#Rej Ho a 5%

###################################Elipse de confian?a################################
plot(ellipse(Sd/n,centre=d_bar,level=1-alpha,npoints=1000),type='l',asp=1,xlim=c(-20,20), xlab= "mud1", ylab= "mud2") 
text(d_bar[1],d_bar[2],"[-9.36,13.27]")
text(0,0,"[0,0]")

##Intervalos Simulataneos
Ls=c()
Li=c()
for (i in 1:p) {
  Ls[i]=(d_bar[i])+sqrt(q*Sd[i,i]/n)
  Li[i]=(d_bar[i])-sqrt(q*Sd[i,i]/n)
  
}

Lim=rbind(Ls,Li)
#colnames(Lim)<-colnames(x)



############univariados Bonferroni
Lsb=c()
Lib=c()
for (i in 1:p) {
  Lsb[i]=(d_bar[i])+qt(1-(alpha/(2*p)),n-1)*sqrt(Sd[i,i]/n)
  Lib[i]=(d_bar[i])-qt(1-(alpha/(2*p)),n-1)*sqrt(Sd[i,i]/n)
  
}

Limb=rbind(Lsb,Lib)
#colnames(Limb)<-colnames(x)
#rownames(Limb)<-c("upper", "lower")

l=cbind(Lim,Limb)
colnames(l)=c("dx1_T","dx2_T","dx1_bonf","dx2_bonf" )


##########################################################################################
#############################################
#Amostras Independentes
#############################Exemplo 2
x=mtcars

#QUeremos estar hip?teses mu1-mu2 = 0

#am=0 dados automatico
#am=1 dados manual
# quereos buscar as vari?veis mpg, hp, wt, qsec


X=x[,c(1,4,6,7,9)]

############Visao geom?trica
windows()
ggpairs(X[,-5], mapping=aes(colour=as.factor(X[,5])), upper=list(combo= 'blank', continuous='blank'))


###############m?dia e vari?ncia das amostras

x1_bar=colMeans(X[which(X$am==0),-5])
S1=cov(X[which(X$am==0),-5])
n1=nrow(X[which(X$am==0),-5])
x2_bar=colMeans(X[which(X$am==1),-5])
S2=cov(X[which(X$am==1),-5])
n2=nrow(X[which(X$am==1),-5])
n=n1+n2

S_pool=((n1-1)*S1+(n2-1)*S2)/(n-2)

Sco= ((1/n1)+(1/n2))* S_pool

alpha=0.05
p=ncol(X[,-5])
mu=c(0,0,0,0)

T2_cal<-mahalanobis(x1_bar-x2_bar, mu,Sco, inverted = FALSE)
T2_cal

q=qf(1-alpha,p,n1+n2-p-1)*((n1+n2-2)*p)/(n1+n2-p-1)
q

#Rej Ho a 5%

###################Usando a funcao hotelling.test (pacote Hotelling)

fit = hotelling.test(.~am, data = X)
fit

###Conferindo os resultados
F_puro=T2_cal*1/(((n-2)*p)/(n-p-1))

1-pf(F_puro,p,n-p-1)



##Intervalos Simulataneos
Ls=c()
Li=c()
for (i in 1:p) {
  Ls[i]=(x1_bar[i]-x2_bar[i])+sqrt(qf(1-alpha,p,n-p-1)*(((n-2)*p)/((n-p-1)))*Sco[i,i])
  Li[i]=(x1_bar[i]-x2_bar[i])-sqrt(qf(1-alpha,p,n-p-1)*(((n-2)*p)/((n-p-1)))*Sco[i,i])
  
}

Lim=rbind(Ls,Li)
#colnames(Lim)<-colnames(x)



############univariados Bonferroni
Lsb=c()
Lib=c()
for (i in 1:p) {
  Lsb[i]=(x1_bar[i]-x2_bar[i])+qt(1-(alpha/(2*p)),n-2)*sqrt(Sco[i,i])
  Lib[i]=(x1_bar[i]-x2_bar[i])-qt(1-(alpha/(2*p)),n-2)*sqrt(Sco[i,i])
  
}

Limb=rbind(Lsb,Lib)
#colnames(Limb)<-colnames(x)
#rownames(Limb)<-c("upper", "lower")

l=cbind(Lim,Limb)
colnames(l)=c("dx1_T","dx2_T","dx3_T","dx4_T","dx1_bonf","dx2_bonf","dx3_bonf","dx4_bonf" )



##################################################################################
##################################################################################
#####################################################################################
##########################################################################################
#############################################
#Amostras Independentes
#############################Exemplo 2 com 2 variaveis mpg e qsec 
x=mtcars

#QUeremos estar hip?teses mu1-mu2 = 0

#am=0 dados automatico
#am=1 dados manual


X=x[,c(1,7,9)]

windows()
ggpairs(X[,-3], mapping=aes(colour=as.factor(X[,3])), upper=list(combo= 'blank', continuous='blank'))

fit = hotelling.test(.~am, data = X)
fit
#Rej Ho a 5%


###################################Elipse de confian?a################################
x1_bar=colMeans(X[which(X$am==0),-3])
S1=cov(X[which(X$am==0),-3])
n1=nrow(X[which(X$am==0),-3])
x2_bar=colMeans(X[which(X$am==1),-3])
S2=cov(X[which(X$am==1),-3])
n2=nrow(X[which(X$am==1),-3])
n=n1+n2

S_pool=((n1-1)*S1+(n2-1)*S2)/(n-2)

Sco= ((1/n1)+(1/n2))* S_pool

xbar=x1_bar-x2_bar

windows()
plot(ellipse(Sco,centre=xbar,level=1-alpha,npoints=1000),type='l',asp=1, xlim=c(-12,2), xlab= "mud1", ylab= "mud2") 
text(xbar[1],xbar[2],"[-7.24,0.82]")
text(0,0,"[0,0]")

### IC T e Bonf??


#########################################################################
###Banco IRIS
##a) comparar as 3 esp?cies 2x2.
x=iris
##1:50 - setosa, 51:100 - versicolor, 101:150 - virginica

####b) Montar IC T2 e bonferroni para dif m?dias em cada uma das 4 vari?veis 
x1_bar=colMeans(x[1:50,-5])
