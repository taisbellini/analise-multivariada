#############An?lise fatorial
require(mvtnorm)
#require(GGally)
require(mvShapiroTest)
require(psych)
#rm(list = ls(all.names = TRUE))
##########################################################################
############################################################################
#############################################################################
#########Exemplo 9.4 JW (tabela 8.4 do JW)
read.table("Wichern_data/T8-4.dat", header=F,sep="")->x
colnames(x) <-c("JPMorgan","Citibank","WellsFargo","RDShell","Texaco")

####################################################################################
########################################################################################
###########Exemplo 9.8 JW (Dados simulados a partir da matriz de correla??o da pag 550) 
set.seed(33)
# limpar todas as vari?veis 
rm(list = ls(all.names = TRUE))
#############################################################################
##Notas de 220 individuos em 6 diferentes disciplinas

#############################################################################
##Simulando os 220 dados a partir de um vetor de m?dias e da matriz de correla??es amostrais
mu<-c(8.2, 8.0, 7.6, 9.1, 9.4, 8.9)
mcor<- rbind(cbind(1,0.439,0.410,0.288,0.329,0.248),cbind(0.439,1,0.351,0.354,0.320,0.329),
             cbind(0.410,0.351,1,0.164,0.190,0.181),cbind(0.288,0.354,0.164,1,0.595,0.470),
             cbind(0.329,0.320,0.190,0.595,1,0.464),cbind(0.248,0.329,0.181,0.470,0.464,1)) ###correlacao
vvar <- rbind(1,1.5,0.8,1.3,1.7,2) ####variancias
mvar <- diag(sqrt(c(vvar)))
mcov <- mvar%*%mcor%*%mvar
nvar <- 6
n=220

#######fixando semente
xm=rmvnorm(n, mean=mu, sigma=mcov)
#inames<-c("Ga?lico","Ingl?s","Hist?ria","Aritm?tica","?lgebra","Geometria") 
#xbar=apply(xm,2,mean)
S=cov(xm)  ###covariancia amostral
x <- (xm%*%solve(chol(S)))%*%chol(mcov)
colnames(x) <-c("Ga?lico","Ingl?s","Hist?ria","Aritm?tica","?lgebra","Geometria")
xbar=apply(xm,2,mean)
#########################################################################################
###########################################################################################
##########################################################################################
sabor<-c(2.75, 3.9, 3.12, 4.58, 3.97, 3.01, 4.19, 3.82)
aroma<-c(4.03, 4.12, 3.97, 4.86, 4.34, 3.98, 4.65, 4.12)
massa<-c(2.8, 3.4, 3.62, 4.34, 4.28, 2.9, 4.52, 3.62)
recheio<-c(2.62, 3.52, 3.05, 4.82, 4.98, 2.82, 4.77, 3.71)

x<-cbind(sabor, aroma, massa, recheio)
x

summary(x)
cov(x)
cor(x)

############################################################################
##############################################################################
#############AN?LISE FATORIAL - ROTEIRO#################################################

#######################Usando PCA pra estimar as cargas fatoriais
#fitpca<-principal(x,3,rotate="none", #n.obs=103) 
fitpca<-principal(x,3,rotate="none") ##default=extrai os scores usando MQO ("M?nimos Quadrados Ordin?rios")
fitpca

# h2 = sum(PCi^2). Significa a porcentagem de cada variavel que esta sendo explicada pelos fatores

load<-fitpca$loadings

windows()
plot(load)
text(load, labels=names(x))

yy<-fitpca$scores
yy
plot(yy)


######################Calculo dos residuos via PCA#############################

c=fitpca$communality
#c
v=fitpca$uniquenesses
v
#l=fitpca$loadings
#l
E=load%*%t(load)+diag(v)

Res=cor(x)-E
# E eh a reconstrucao da matriz de covariancias.
# a variancia(diag principal) nós conseguimos reconstruir. Porem, a covariancia nao (e nao seria esperado pois usamos 3 fatores e os dados tem 5 vars).
#Observamos que fica proximo e na matriz de residuos abaixo vemos o quando ficou "inexplicado".


#Res1=fitpca$residual-diag(v)


#####################################Comparando com o uso da fun?ao prcomp
pc=prcomp(x, scale.=T)
y<-pc$x
y

cbind(y[,1:2],yy[,1:2])   #######Scores via prcomp e via principal

## a diferenca se da pois na AF, multiplicamos os autovetores por sqrt(lambda).  
#A diferenca eh apenas em escala

cbind(scale(y)[,1:2],yy[,1:2])   
# se padronizamos y, temos que sqrt(lambda)*e = e, pois temos e com var 1. 

screeplot(pc, type = "l")
windows()
biplot(pc)

######usando MV para estimar as cargas########################
###################################################################
fitmle<-factanal(x,2, rotation="none", scores="regression")  
fitmle

##rotation "Varimax" gera novos fatores ortogonais que maximizam a variabiliade das cargas dentro de cada fator 
## method= "regression" extrai os scores usando o metodo chamado regression "Regress?o"
## method= "Bartlett" extrai os escores usando o m?todo chamado WLS (MQP) "Minimos Quadrados Ponderados"

load<-fitmle$loadings
# matriz L

windows()
plot(load)
text(load, labels=names(x))

yz<-fitmle$scores
yz
######################Calculo dos residuos via MV#############################

#c=fitpca$communality
#c
v=fitmle$uniquenesses
v
#l=fitpca$loadings
#l
E=load%*%t(load)+diag(v)

Res=cor(x)-E

#Res1=fitmle$residual-diag(v)

#################Teste para o numero de fatores retidos por MV
##JW p?gina 502, eq (9.39)
n=nrow(x)
p=ncol(x)
m=2

qcal=(n-1-(2*p+4*m+5)/6)*log(det(E)/det(cor(x)))
qcal

###qcall=  ((n-1-(2*p+4*m+5)/6)-((2*m)/3))*log(det(Eml)/det(cor(x)))                                        # usado no pacote

qq=qchisq(0.95,0.5*((p-m)^2-p-m))
qq
1-pchisq(qcal,0.5*((p-m)^2-p-m))    ##############p valor

##N?o Rej Ho, isto ?, 2 fatores s?o suficientes

mm=0.5*(2*p+1-sqrt(8*p+1)) ##Condicao para aplicar o teste:  
## m (numero de fatores retidos) menor ou igual a mm
m<=mm


## Varimax
fitmle<-factanal(x,2, rotation="varimax", scores="regression")  
fitmle


