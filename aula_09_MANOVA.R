require(ggplot2)
require(GGally)
require(mvShapiroTest)
install.packages('HSAUR')
install.packages('car')
install.packages('heplots')
library(heplots)

#################MANOVA (1 fator)##################################3
######################################Exemmplo 6.9#######################
y1<-c(9,6,9,0,2,3,1,2)
y2<-c(3,2,7,4,0,8,9,7)
f<-as.factor(c("1","1","1","2","2","3","3","3"))
y<-cbind(y1,y2,f)

#######################PLot visao geom?trica  ggplot
#ggpairs(y[,-3], mapping=aes(colour=as.factor(f)))

#windows()
#ggplot(y, aes(x=y1, y=y2)) + 
 # geom_point(aes(colour=f)) + 
 # labs(x="y1", y="y2", title="mtcars data", 
   #    colour="f")
y<-data.frame(y1,y2,f)
windows()
ggpairs(y[,-3], mapping=aes(colour=f), upper=list(combo= 'blank', continuous='blank'))


#y<-cbind(y1,y2,f)
y=as.matrix(y)
mnv = manova(y[,1:2] ~ as.factor(y[,3]))
mnv

summary(mnv, test = "Wilks")  #MANOVA
# Rej Ho a 5%

summary.aov(mnv)              #ANOVA para cada vari?vel 

# Rej Ho para y1 e n?o Rej Ho para y2


#summary(mnv, test = "Pillai")

##############Comparaoes multiplas usando T test com correcao de bonferroni
xbar<-aggregate(y[,1:2]~y[,3], data=y,FUN=mean)
colnames(xbar)=c("grupo", "xbar1", "xbar2")

#windows()
#ggpairs(xbar[,-1], mapping=aes(colour=as.factor(xbar[,1])), upper=list(combo= 'blank', continuous='blank'))


k=3 ## numero de grupos
p=2 ## n?mero de vari?veis
n1=3
n2=2
n3=3
n=n1+n2+n3

##produzindo a matriz W
W= (n1-1)*cov(y[1:3,1:2])+(n2-1)*cov(y[4:5,1:2])+(n3-1)*cov(y[6:8,1:2])
alpha=0.05

#######Variavel y1
#t1 contra t2
Lim_Sup_12_y1=(mean(y[1:3,1])-mean(y[4:5,1]))+qt(1-(alpha/(p*k*(k-1))),n-k)*sqrt(((1/n1)+(1/n2))*(W[1,1]/(n-3)))
Lim_Inf_12_y1=(mean(y[1:3,1])-mean(y[4:5,1]))-qt(1-(alpha/(p*k*(k-1))),n-k)*sqrt(((1/n1)+(1/n2))*(W[1,1]/(n-3)))
##t1 contra t3
Lim_Sup_13_y1=(mean(y[1:3,1])-mean(y[6:8,1]))+qt(1-(alpha/(p*k*(k-1))),n-k)*sqrt(((1/n1)+(1/n3))*(W[1,1]/(n-3)))
Lim_Inf_13_y1=(mean(y[1:3,1])-mean(y[6:8,1]))-qt(1-(alpha/(p*k*(k-1))),n-k)*sqrt(((1/n1)+(1/n3))*(W[1,1]/(n-3)))
##t2 contra t3
Lim_Sup_23_y1=(mean(y[4:5,1])-mean(y[6:8,1]))+qt(1-(alpha/(p*k*(k-1))),n-k)*sqrt(((1/n2)+(1/n3))*(W[1,1]/(n-3)))
Lim_Inf_23_y1=(mean(y[4:5,1])-mean(y[6:8,1]))-qt(1-(alpha/(p*k*(k-1))),n-k)*sqrt(((1/n2)+(1/n3))*(W[1,1]/(n-3)))

Lim_y1=rbind(c(Lim_Sup_12_y1, Lim_Inf_12_y1),c(Lim_Sup_13_y1, Lim_Inf_13_y1),c(Lim_Sup_23_y1, Lim_Inf_23_y1))
rownames(Lim_y1)<-c("12","13","23")
colnames(Lim_y1)<-c("Ls", "Li")

#######Variavel y2 #######Apenas pra ilustrar pois a anova nao foi significativa para a y2
#t1 contra t2
Lim_Sup_12_y2=(mean(y[1:3,2])-mean(y[4:5,2]))+qt(1-(alpha/(p*k*(k-1))),n-k)*sqrt(((1/n1)+(1/n2))*(W[2,2]/(n-3)))
Lim_Inf_12_y2=(mean(y[1:3,2])-mean(y[4:5,2]))-qt(1-(alpha/(p*k*(k-1))),n-k)*sqrt(((1/n1)+(1/n2))*(W[2,2]/(n-3)))
##t1 contra t3
Lim_Sup_13_y2=(mean(y[1:3,2])-mean(y[6:8,2]))+qt(1-(alpha/(p*k*(k-1))),n-k)*sqrt(((1/n1)+(1/n3))*(W[2,2]/(n-3)))
Lim_Inf_13_y2=(mean(y[1:3,2])-mean(y[6:8,2]))-qt(1-(alpha/(p*k*(k-1))),n-k)*sqrt(((1/n1)+(1/n3))*(W[2,2]/(n-3)))
##t2 contra t3
Lim_Sup_23_y2=(mean(y[4:5,2])-mean(y[6:8,2]))+qt(1-(alpha/(p*k*(k-1))),n-k)*sqrt(((1/n2)+(1/n3))*(W[2,2]/(n-3)))
Lim_Inf_23_y2=(mean(y[4:5,2])-mean(y[6:8,2]))-qt(1-(alpha/(p*k*(k-1))),n-k)*sqrt(((1/n2)+(1/n3))*(W[2,2]/(n-3)))

Lim_y2=rbind(c(Lim_Sup_12_y2, Lim_Inf_12_y2),c(Lim_Sup_13_y2, Lim_Inf_13_y2),c(Lim_Sup_23_y2, Lim_Inf_23_y2))
rownames(Lim_y2)<-c("12","13","23")
colnames(Lim_y2)<-c("Ls", "Li")
#############################################Exemplo 2###################mtcars
####################################################################################
#############################Exemplo com 2 variaveis mpg e qsec 
x=mtcars
Y=x[,c(1,7,10)]

#mvShapiro.Test(as.matrix(Y[,-3])) ####Teste de normalidade multivariada
YY=data.frame(Y)
windows()
#ggpairs(Y[,-3], mapping=aes(colour=as.factor(Y[,3])), upper=list(combo= 'blank', continuous='blank'))
ggpairs(YY[,-3], mapping=aes(colour=as.factor(YY[,3])))


Y=as.matrix(Y)

mnv = manova(Y[,1:2] ~ as.factor(Y[,3]))
mnv


summary(mnv, test = "Wilks")  #MANOVA
summary.aov(mnv)              #ANOVA para cada vari?vel 

n1=nrow(Y[which(Y[,3]==3),1:2])
n2=nrow(Y[which(Y[,3]==4),1:2])
n3=nrow(Y[which(Y[,3]==5),1:2])
n=n1+n2+n3
p=2
k=3
############################Produzindo a matriz W
W= (n1-1)*cov(Y[which(Y[,3]==3),1:2])+(n2-1)*cov(Y[which(Y[,3]==4),1:2])+(n3-1)*cov(Y[which(Y[,3]==5),1:2])

alpha=0.05

######Intervalos Bonferroni
#######Variavel mpg
#t1 contra t2
Lim_Sup_12_y1=(mean(Y[which(Y[,3]==3),1])-mean(Y[which(Y[,3]==4),1]))+qt(1-(alpha/(p*k*(k-1))),n-k)*sqrt(((1/n1)+(1/n2))*(W[1,1]/(n-k)))
Lim_Inf_12_y1=(mean(Y[which(Y[,3]==3),1])-mean(Y[which(Y[,3]==4),1]))-qt(1-(alpha/(p*k*(k-1))),n-k)*sqrt(((1/n1)+(1/n2))*(W[1,1]/(n-k)))
##t1 contra t3
Lim_Sup_13_y1=(mean(Y[which(Y[,3]==3),1])+mean(Y[which(Y[,3]==5),1]))+qt(1-(alpha/(p*k*(k-1))),n-k)*sqrt(((1/n1)+(1/n3))*(W[1,1]/(n-k)))
Lim_Inf_13_y1=(mean(Y[which(Y[,3]==3),1])-mean(Y[which(Y[,3]==5),1]))+qt(1-(alpha/(p*k*(k-1))),n-k)*sqrt(((1/n1)+(1/n3))*(W[1,1]/(n-k)))
##t2 contra t3
Lim_Sup_23_y1=(mean(Y[which(Y[,3]==4),1])+mean(Y[which(Y[,3]==5),1]))+qt(1-(alpha/(p*k*(k-1))),n-k)*sqrt(((1/n2)+(1/n3))*(W[1,1]/(n-k)))
Lim_Inf_23_y1=(mean(Y[which(Y[,3]==4),1])-mean(Y[which(Y[,3]==5),1]))+qt(1-(alpha/(p*k*(k-1))),n-k)*sqrt(((1/n2)+(1/n3))*(W[1,1]/(n-k)))

Lim_y1=rbind(c(Lim_Sup_12_y1, Lim_Inf_12_y1),c(Lim_Sup_13_y1, Lim_Inf_13_y1),c(Lim_Sup_23_y1, Lim_Inf_23_y1))
rownames(Lim_y1)<-c("34","35","45")
colnames(Lim_y1)<-c("Ls", "Li")

#######Variavel qsec #######
#t1 contra t2
Lim_Sup_12_y2=(mean(Y[which(Y[,3]==3),2])-mean(Y[which(Y[,3]==4),2]))+qt(1-(alpha/(p*k*(k-1))),n-k)*sqrt(((1/n1)+(1/n2))*(W[2,2]/(n-k)))
Lim_Inf_12_y2=(mean(Y[which(Y[,3]==3),2])-mean(Y[which(Y[,3]==4),2]))-qt(1-(alpha/(p*k*(k-1))),n-k)*sqrt(((1/n1)+(1/n2))*(W[2,2]/(n-k)))
##t1 contra t3
Lim_Sup_13_y2=(mean(Y[which(Y[,3]==3),2])+mean(Y[which(Y[,3]==5),2]))+qt(1-(alpha/(p*k*(k-1))),n-k)*sqrt(((1/n1)+(1/n3))*(W[2,2]/(n-k)))
Lim_Inf_13_y2=(mean(Y[which(Y[,3]==3),2])-mean(Y[which(Y[,3]==5),2]))+qt(1-(alpha/(p*k*(k-1))),n-k)*sqrt(((1/n1)+(1/n3))*(W[2,2]/(n-k)))
##t2 contra t3
Lim_Sup_23_y2=(mean(Y[which(Y[,3]==4),2])+mean(Y[which(Y[,3]==5),2]))+qt(1-(alpha/(p*k*(k-1))),n-k)*sqrt(((1/n2)+(1/n3))*(W[2,2]/(n-k)))
Lim_Inf_23_y2=(mean(Y[which(Y[,3]==4),2])-mean(Y[which(Y[,3]==5),2]))+qt(1-(alpha/(p*k*(k-1))),n-k)*sqrt(((1/n2)+(1/n3))*(W[2,2]/(n-k)))

Lim_y2=rbind(c(Lim_Sup_12_y2, Lim_Inf_12_y2),c(Lim_Sup_13_y2, Lim_Inf_13_y2),c(Lim_Sup_23_y2, Lim_Inf_23_y2))
rownames(Lim_y2)<-c("34","35","45")
colnames(Lim_y2)<-c("Ls", "Li")

#######################plotar as m?dias
#Y=as.matrix(Y)
xbar<-aggregate(Y[,1:2]~Y[,3], data=Y,FUN=mean)
colnames(xbar)=c("grupo", "x1", "x2")

vlab=c("x1", "x2")
pairs(xbar[,-1], vlab, panel=function(x,y){text(x,y,levels(as.factor(Y[,3])))
  lines(x,y)})


############################################Exemplo 2 com mais vari?veis. ExercpicioRodar MANOVA
###########################################################################
x=mtcars
X=x[,c(1,4,6,7,10)]
#gear=1,2,3,4,5 

#mvShapiro.Test(X[,-5])  Normalidade multivariada

#######################PLot visao geom?trica  ggpairs
windows()
ggpairs(X[,-5], mapping=aes(colour=as.factor(X[,5])))

X=as.matrix(X)

mnv = manova(X[,1:4] ~ as.factor(X[,5]))
mnv


summary(mnv, test = "Wilks")  #MANOVA
summary.aov(mnv)              #ANOVA para cada vari?vel 


# Rej Ho a 5 % para todas em separado

#######################plotar as m?dias
#Y=as.matrix(Y)
xbar1<-aggregate(X[,1:4]~X[,5], data=Y,FUN=mean)
colnames(xbar1)=c("grupo", "x1", "x2", "x3", "x4")

vlab=c("x1", "x2", "x3", "x4" )
pairs(xbar1[,-1], vlab, panel=function(x,y){text(x,y,levels(as.factor(X[,5])))
  lines(x,y)})


######################MANOVA (2 fatores)############################
######################Exemplo 6.13#######################################
#An experiment was conducted to determine the optimum conditions for 
#extruding plastic film. Three responses were measured in relation to two factors,
#rate of extrusion and amount of an additive.
tear<-c(6.5,6.2,5.8,6.5,6.5,6.7,6.6,7.2,7.1,6.8,6.9,7.2,6.9,6.1,6.3,7.1,7.0,7.2,7.5,7.6)
gloss<-c(9.5,9.9,9.6,9.6,9.2,9.1,9.3,8.3,8.4,8.5,9.1,10.0,9.9,9.5,9.4,9.2,8.8,9.7,10.1,9.2)
opacity<-c(4.4,6.4,3.0,4.1,0.8,2.8,4.1,3.8,1.6,3.4,5.7,2.0,3.9,1.9,5.7,8.4,5.2,6.9,2.7,1.9)
y<-cbind(tear,gloss,opacity)

additive<- gl(2,10, labels = c("1.0", "1.5"))
rate <- gl(2, 5, length = 20, labels = c("-10", "10"))
inte<-as.factor(c(rep(-10,5), rep(10,5), rep(-15,5), rep(15,5)))

####################plotando efeitos principais nas m?dias univariadas
windows()
par(mfrow=c(2,3))
plot(tear~additive)
plot(gloss~additive)
plot(opacity~additive)
plot(tear~rate)
plot(gloss~rate)
plot(opacity~rate)

############scatter 2x2 considerando interacao dos fatores 

Y=data.frame(tear,gloss,opacity,additive, rate, inte)
ggpairs(Y[,1:3], mapping=aes(colour=additive))
ggpairs(Y[,1:3], mapping=aes(colour=rate))
ggpairs(Y[,1:3], mapping=aes(colour=inte))



##########################Olhando as medias dentro dos n?veis dos fatores
#yz=as.matrix(yy)
xbar_a<-aggregate(y~additive, data=y,FUN=mean)
xbar_r<-aggregate(y~rate, data=y,FUN=mean)
xbar_ar<-aggregate(y~inte, data=y,FUN=mean)


######################plotando m?dias usando pacote ggplot2 
add=c(1.0,1.0,1.5,1.5)
rat=c(-10,10,-10,10)
xx=rbind(xbar_ar[2:3,-1],rbind(xbar_ar[c(1,3),-1]))
f=data.frame(add, rat, xx)
f

#var 1
m_tear=ggplot(f, aes(x = add,y = tear))+ geom_point(colour = rat+11)+labs(x = "additive", y = "tear", color = "rate") 

m_gloss=ggplot(f, aes(x = add,y = gloss))+ geom_point(colour = rat+11)+labs(x = "additive", y = "gloss", color = "rate") 

m_opacity=ggplot(f, aes(x = add,y = opacity))+ geom_point(colour = rat+11)+labs(x = "additive", y = "opacity", color = "rate") 

##############PLotando efeitos principais e interacao usando a funcao pairs 
vlabb=c("tear", "gloss", "opacity")

windows()
#par(mfrow=c(1,3))
# fator additive
pairs(xbar_a, vlabb, panel=function(x,y){text(x,y,levels(additive))
  lines(x,y)})

# fator rate
pairs(xbar_r, vlabb, panel=function(x,y){text(x,y,levels(rate))
  lines(x,y)})

# olhar a diferenca de inclinacao da reta de -10 a 10 e de -15 a 15
pairs(xbar_ar, vlabb, panel=function(x,y){text(x,y,levels(inte))
  lines(x,y)})

######################MANOVA 
mnv <- manova(y ~ rate * additive)
mnv

summary(mnv, test = "Wilks") # ANOVA table of Wilks' lambda
# significativo para os fatores mas nao para a interacao

summary.aov(mnv)             # univariate ANOVA tables

######################plotando vetores bivariados de m?dias usando o pacote HEPLOT
##No exemplo, tear x gloss

#############PLot dos vetores dois a dois considerando apenas os efeitos principais (sem intera??o)
pairs(mnv)

##  PLot dos vetores de m?dias incluindo intera??o (Um pra cada dupla de variaveis)

# Compare evidence and effect scaling
colors = c("red", "darkblue", "darkgreen", "brown")
heplot(mnv, size="evidence", col=colors, cex=1.25)
heplot(mnv, size="effect", add=TRUE, lwd=4, term.labels=FALSE, col=colors)



################intera??o

intMeans <- termMeans(mnv, 'rate:additive', abbrev.levels=2)
#rownames(intMeans) <- apply(expand.grid(c(???Lo???,???Hi???), c(???Lo???, ???Hi???)), 1, paste, collapse=???:???)
points(intMeans[,1], intMeans[,2], pch=18, cex=1.2, col="brown")
text(intMeans[,1], intMeans[,2], rownames(intMeans), adj=c(0.5,1), col="brown")
lines(intMeans[c(1,3),1], intMeans[c(1,3),2], col="brown")
lines(intMeans[c(2,4),1], intMeans[c(2,4),2], col="brown")
#Neste grafico, tracamos a linha da variacao de additive, fixando o rate
# Quando rate esta em 10, ha bastante variacao de 1 a 1.5 de additive
# quando rate esta em -10, nao ha muita variacao do 1 ao 1.5

#Ainda, mesmo sem tracar a linha, visualmente conseguimos observar a variacao fixando o additive
# quando o additive esta em 1 (linha passa pelo 1 da reta de additive), vemos que ha bastante diferenca entre o -10 e 10
# quando additive eta em 1.5, vemos que quase nao ha diferenca entre -10 e 10 de rate



#############################################################################################
#####################################################Outra maneira de rodar a manova
plastic.mod <- lm(cbind(tear, gloss, opacity) ~ rate*additive)
anova(plastic.mod)
pairs(plastic.mod)

#############################################################################################
########################Exercicio 6.18   JW#######################################
read.table("T6-18.dat", header=F,sep="")->y
x=y[,-5]
colnames(x)=c("x1", "x2", "species", "time")
View(x)

ggpairs(x[,1:2], mapping=aes(colour=as.factor(x$species)))
ggpairs(x[,1:2], mapping=aes(colour=as.factor(x$time)))

Y=cbind(x[,1], x[,2])
f1=as.factor(x[,3])
f2=as.factor(x[,4])
######################MANOVA 
mnv <- manova(Y[,1:2] ~ f1 * f2)
mnv

summary(mnv, test = "Wilks") # ANOVA table of Wilks' lambda
summary.aov(mnv)             # univariate ANOVA tables


##plotando as m?dias dentro dos niveis fatores
intMeans <- termMeans(mnv, 'f1:f2', abbrev.levels=2)

######################plotando m?dias usando pacote ggplot2
##com a esp?cies no eixo x
F1=c(rep("SS",3),rep("JL",3),rep("LP",3))
F2=as.numeric(rep(c(1,2,3),3))

g=data.frame(F1,F2,intMeans)

m_x1=ggplot(g, aes(x = F1,y = g[,3]))+ geom_point(colour = F2+1)+labs(x = "especie", y = "x1", color = "epoca")  

m_x2=ggplot(g, aes(x = F1,y = g[,4]))+ geom_point(colour = F2+1)+labs(x = "especie", y = "x1", color = "epoca") 

##com a ?poca no eixo x
ff1=(c(rep(1,3),rep(2,3),rep(3,3)))
ff2=as.factor(rep(c(1,2,3),3))

g1=data.frame(ff1,ff2,intMeans)

m_x1=ggplot(g1, aes(x = ff2,y = g1[,3]))+ geom_point(colour = ff1+1)+labs(x = "epoca", y = "x1", color = "especie") 

m_x2=ggplot(g1, aes(x = ff2,y = g1[,4]))+ geom_point(colour = ff1+1)+labs(x = "epoca", y = "x1", color = "especie") 

