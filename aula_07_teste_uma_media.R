require(GGally)
require (ellipse)
require(mvShapiroTest)
require(Hotelling)
##########################################
################################################################
########################Teste de Hip?tese Uma M?dia########################

######################Exemplo 1########################

x=matrix(c(154,136,91,125,133,125,93,80,132,107,108,90,54,89,93,77,43,50,125,76), ncol=2)
colnames(x)<-c("sist", "diast")

xx=data.frame(x)
windows()
ggpairs(xx) ##########scatter 2x2

sigma<-matrix(c(400,240,240,225), ncol=2)

x_bar=colMeans(x)

n=nrow(x)
p=ncol(x)
alpha=0.05

q_cal<-n*mahalanobis(x_bar, c(120,80), sigma, inverted = FALSE)
q_cal

q=qchisq(1-alpha,p)
q
#N?o Rej Ho a 5%



########simulando outras vetores de m?dias Xbarra########
#################xbarra=[117.6,70]###################

q1_cal<-n*mahalanobis(c(117.6,70), c(120,80), sigma, inverted = FALSE)
q1_cal
#Rej Ho a 5%

#################xbarra=[110,90]###################
q2_cal<-n*mahalanobis(c(110,90), c(120,80), sigma, inverted = FALSE)
q2_cal
#Rej Ho a 5%
#############Sistematizado######################

x_bars=matrix(c(117.6, 117.6, 110, 80.5, 70, 90), ncol=2)
q_cals=matrix(rep(0,3), nrow=1)
for(k in 1:3){
  q_cals[,k]<-n*mahalanobis(x_bars[k,], c(120,80), sigma, inverted = FALSE)
}
q_cals

#######################Regiao de Ho
####################Elipse###################################
mu=c(120,80)
windows()
plot(ellipse(sigma/n,centre=mu,level=1-alpha,npoints=1000),type='l',asp=1, xlab= "x_bar1", ylab= "x_bar2") 
text(117.6,80.5,"[117.6 , 80.5]")  ####x_bar
text(117.6,70,"[117.6 , 70]")   ####x_bar
text(110,90,"[110 , 90]")    ####x_bar




###################Exemplo 1 usando a matriz de covariancia amostral S##########################

#S<-matrix(c(561.82,540.89, 540.89,680.72), ncol=2)
S=cov(x)

T2_cal<-n*mahalanobis(x_bar, c(120,80), S, inverted = FALSE)
T2_cal

q=qf(1-alpha,p,n-p)*((n-1)*p)/(n-p)
q

#N?o Rej Ho a 5%

########simulando outras vetores de m?dias Xbarra########

#################xbarra=[117.6,70]###################

T2_cal1<-n*mahalanobis(c(117.6,70), c(120,80), S, inverted = FALSE)
T2_cal1
#N?o Rej Ho a 5%

#################xbarra=[110,90]###################

T2_cal2<-n*mahalanobis(c(110,90), c(120,80), S, inverted = FALSE)
T2_cal2

# Rej Ho a 5%

#############Sistematzado######################

x_barss=matrix(c(117.6, 117.6, 110, 80.5, 70, 90), ncol=2)
T2_cals=matrix(rep(0,3), nrow=1)
for(k in 1:3){
  T2_cals[,k]<-n*mahalanobis(x_barss[k,], c(120,80), S, inverted = FALSE)
}
T2_cals

#######################Regiao de Ho
########################Elipse#################################
Windows()
plot(ellipse(S/n,centre=c(120,80),level=1-alpha,npoints=1000),type='l',asp=1, xlab= "x_bar1", ylab= "x_bar2") 
text(117.6,80.5,"[117.6 , 80.5]") ####x_bar
text(117.6,70,"[117.6 , 70]")     ####x_bar
text(110,90,"[110 , 90]")         ####x_bar


##########################Elipse de Confianca############################
############################################################################
########Para o ex1, verificar se o vetores mu=[132,60] e mu=[125,75]  pertencem a regiao de confianca 95%#########3
###muHo=(120,80)  valores iniciais
alpha=0.05
n=nrow(x)  ##n=10
p=ncol(x)  ##p=2
q=qf(1-alpha,p,n-p)*((n-1)*2)/(n-2)
x_bar=colMeans(x)  ############vetor de m?dias amostrais

##u=[132,60]###########
T2_cal1<-n*mahalanobis(x_bar, c(132,60), S, inverted = FALSE)
T2_cal1
q

##u=[125,75]###########
T2_cal2<-n*mahalanobis(x_bar, c(125,75), S, inverted = FALSE)
T2_cal2

##u=[120,80]###########
T2_cal3<-n*mahalanobis(x_bar, c(120,80), S, inverted = FALSE) ###vetor mu original do ex 1
T2_cal3


##############Sistematizado#############################

mus=matrix(c(132, 125, 120, 60, 75, 80), ncol=2)
T2_cals=matrix(rep(0,3), nrow=1)
for(k in 1:3){
  T2_cals[,k]<-10*mahalanobis(x_bar, mus[k,], S, inverted = FALSE)
}
T2_cals


######################Elipse de confianca########################
plot(ellipse(S/n,centre=x_bar,level=0.95,npoints=1000),type='l',asp=1, xlab= "mu1", ylab= "mu2") 
text(132,60,"[132,60]")  #####mu
text(125,75,"[125,75]")  #####mu
text(120,80,"[120,80]")  #####mu


################################################################################################
#######################Exemplo 2###############################################################
#transpiracao de 20 mulheres saudaveis. 
#Taxa de suor (X1), conteudo de sodio (X2) e conteudo de potassio (X3).
S=matrix(c(2.879, 10.01, -1.81, 10.01, 199.788, -5.64, -1.81, -5.64, 3.628), ncol=3)
S
n=20
p=3
alpha=0.1

x_bar=c(4.64, 45.4, 9.965)
mu=c(4,50,10)

T2_cal=n*mahalanobis(x_bar, mu, SS, inverted = FALSE)
T2_cal

q=qf(1-alpha,p,n-p)*((n-1)*p)/(n-p)
q
q1=qchisq(1-alpha,p)
q1

#Rej Ho a 10%

########simulando outro vetor de m?dias Xbarra########

#################xbarra=[4,50,15]###################

T2_cal1<-n*mahalanobis(c(4,50,15), c(4,50,10), SS, inverted = FALSE)
T2_cal1
#Rej Ho a 5%


##########################################################################################
#########################################################################################
###ICs exemplo 2
alpha=0.1
##Intervalos Simulataneos
Ls=c()
Li=c()
for (i in 1:p) {
  Ls[i]=(x_bar[i])+sqrt(q*S[i,i]/n)
  Li[i]=(x_bar[i])-sqrt(q*S[i,i]/n)
  
}

Lim=rbind(Ls,Li)
#colnames(Lim)<-colnames(x)



############univariados Bonferroni
Lsb=c()
Lib=c()
for (i in 1:p) {
  Lsb[i]=(x_bar[i])+qt(1-(alpha/(2*p)),n-1)*sqrt(S[i,i]/n)
  Lib[i]=(x_bar[i])-qt(1-(alpha/(2*p)),n-1)*sqrt(S[i,i]/n)
  
}

Limb=rbind(Lsb,Lib)
#colnames(Limb)<-colnames(x)
#rownames(Limb)<-c("upper", "lower")

l=cbind(Lim,Limb)
colnames(l)=c("x1_T","x2_T","x3_T","x1_bonf","x2_bonf","x3_bonf" )



########################Exemplo 3 (dados exemplo 5.3 JW)############################################
#Dados de radiacao de fornos de microonda. Porta fechada (X1) e porta aberta (X2).

y1<-c(0.15,	  0.09,	  0.18	,  0.10,	  0.05	,  0.12	,  0.08	 , 0.05	,  0.08	,  0.10	,  0.07,	  
      0.02,	  0.01,	  0.10,	  0.10,	  0.10,	  0.02,	  0.10	,  0.01	,  0.40	,  0.10	,  0.05,	  
      0.03,	  0.05,	  0.15,	  0.10	,  0.15	,  0.09	,  0.08	,  0.18	,  0.10	,  0.20,	  0.11	, 
      0.30	,  0.02,	  0.20	,  0.20	,  0.30	,  0.30	,  0.40	,  0.30	,  0.05)

y1=y1^(1/4)
y2<-c( 0.30	,  0.09,	  0.30	,  0.10	,  0.10	,  0.12	  ,0.09	,  0.10	,  0.09	,  0.10
,       0.07,	  0.05	,  0.01	 , 0.45	,  0.12	,  0.20	,  0.04	,  0.10	,  0.01	,  0.60,
       0.12	,  0.10	,  0.05	  ,0.05	,  0.15	,  0.30	  ,0.15	  ,0.09	,  0.09	  ,0.28
,       0.10,	  0.10	,  0.10	,  0.30	,  0.12	,  0.25	,  0.20	,  0.40	,  0.33,	  0.32,
       0.12,	  0.12
)
y2=y2^(1/4)
y=data.frame(y1,y2)

y_bar=colMeans(y)
Sy=cov(y)
n=nrow(y)
p=ncol(y)
alpha=0.05

###################################Verificar se o vetor mu =[0.562,0.589]
mu1=c(0.7,0.7)
T2_cal<-n*mahalanobis(y_bar, mu1, Sy, inverted = FALSE)
T2_cal

q=qf(1-alpha,p,n-p)*((n-1)*p)/(n-p)
q
#N?o Rej Ho a 5%

##################Elipse de Confian?a
plot(ellipse(Sy/n,centre=colMeans(y),level=1-alpha,npoints=1000),type='l',asp=1,xlim= c(0.4,0.71), ylim=c(0.5,0.71),xlab= "mu1", ylab= "mu2") 
text(0.562,0.589,"mu=[0.562,0.589]")  #####mu
text(0.7,0.7,"mu1=[0.7,0.7]")

##########################IC Exemplo 3
##ICs T2 e Bonferroni

alpha=0.05

##Intervalos Simulataneos
Ls=c()
Li=c()
for (i in 1:p) {
  Ls[i]=(y_bar[i])+sqrt(q*Sy[i,i]/n)
  Li[i]=(y_bar[i])-sqrt(q*Sy[i,i]/n)
  
}

Lim=rbind(Ls,Li)
colnames(Lim)<-c("x1","x2")



############univariados Bonferroni
Lsb=c()
Lib=c()
for (i in 1:p) {
  Lsb[i]=(y_bar[i])+qt(1-(alpha/(2*p)),n-1)*sqrt(Sy[i,i]/n)
  Lib[i]=(y_bar[i])-qt(1-(alpha/(2*p)),n-1)*sqrt(Sy[i,i]/n)
  
}

Limb=rbind(Lsb,Lib)
colnames(Limb)<-c("x1", "x2")
#rownames(Limb)<-c("upper", "lower")

l=cbind(Lim,Limb)
colnames(l)=c("x1_T","x2_T","x1_bonf","x2_bonf" )


####################################################################
##Banco IRIS.
####a) verificar se a Elipse de confian?a cobre mu=[6,3,4,1] e mu=[5,2,5,2]
####b)Montar IC T2 e bonferroni pra cada uma das 4 vari?veis 
x=iris[,-5]
x_barra = colMeans(x)

#a) Verificar se os vetores mu1=[6,3,4,1] e mu2=[5,2,5,2] estão dentro da elipse
mu1 = c(6,3,4,1)
mu2 = c(5,2,5,2)
n = nrow(x)
p = ncol(x)
Sx = cov(x)

# Teste de hipotese (qcal tem que ser < q)
alpha = 0.05
q=qf(1-alpha,p,n-p)*((n-1)*p)/(n-p)
q

q1_cal <-n*mahalanobis(x_barra, mu1, Sx, inverted = FALSE)
q1_cal
#Rejeita mu1 a 10%

q2_cal <-n*mahalanobis(x_barra, mu2, Sx, inverted = FALSE)
q2_cal
#Rejeita mu2 a 10%

#b) Montar intervalos

##Intervalos Simulataneos
Ls=c()
Li=c()
for (i in 1:p) {
  Ls[i]=(x_barra[i])+sqrt(q*Sx[i,i]/n)
  Li[i]=(x_barra[i])-sqrt(q*Sx[i,i]/n)
  
}

Lim=rbind(Ls,Li)
colnames(Lim)<-c("x1","x2", "x3", "x4")
Lim

############univariados Bonferroni
Lsb=c()
Lib=c()
for (i in 1:p) {
  Lsb[i]=(x_barra[i])+qt(1-(alpha/(2*p)),n-1)*sqrt(Sx[i,i]/n)
  Lib[i]=(x_barra[i])-qt(1-(alpha/(2*p)),n-1)*sqrt(Sx[i,i]/n)
  
}

Limb=rbind(Lsb,Lib)
colnames(Limb)<-c("x1", "x2", "x3", "x4")
#rownames(Limb)<-c("upper", "lower")
Limb

l=cbind(Lim,Limb)
colnames(l)=c("x1_T","x2_T","x3_T","x4_T","x1_bonf","x2_bonf","x3_bonf","x4_bonf")
l

#mu1 esta dentro em ambos (menos na variavel x4)
#mu2 esta fora em ambos
