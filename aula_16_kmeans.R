############################################################################################3
################                      K MEANS               ################################
##############################################################################################
####################

####Exemplo 12.11

a<-matrix(c(5,-1,1,-3,3,1,-2,-2),ncol=2)

plot(a)
text(a,labels=c("A","B","C","D"))

km=kmeans(a, 2) ## escolha aleatoria dos centroides (default) 

km$cluster  ########Mostra os grupos
km$centers ########### mostra os centr?ides finais

fitted(km, method = "centers")  ###############Mostra os centroides e os grupos



plot(a,pch=16,col=km$cluster, xlab="x1", ylab="x2", main="Ex12.11 - centroide aleatorio")
text(a,labels=c("A","B","C","D"))



########################################### utilizando centroides AB e CD
center<-rbind(colMeans(a[1:2, ]),colMeans(a[3:4,]))  
k=kmeans(a,center) 
k$cluster  ########Mostra os grupos
k$centers ########### mostra os centr?ides finais

fitted(k, method = "centers")  ###############Mostra os centroides e os grupos

windows()
plot(a,pch=16,col=k$cluster, xlab="x1", ylab="x2", main="Ex12.11 - centroides AB e CD")
text(a,labels=c("A","B","C","D"))


################################Utilizando as 2 primeiras obs
center=a[1:2,]
ks=kmeans(a,center) 
ks$cluster  ########Mostra os grupos
ks$centers ########### mostra os centr?ides finais

fitted(ks, method = "centers")  ###############Mostra os centroides e os grupos

windows()
plot(a,pch=16,col=ks$cluster, xlab="x1", ylab="x2", main="Ex12.11 - centroides 2 primeiros")
text(a,labels=c("A","B","C","D"))

######################################################
############################OUTRO EXEMPLO
### Dados de 21 paises e 4 variaveis, de acordo com o banco de dados da ONU    
#EV - expectativa de vida
#Edu - ?ndice de educa??o
#PIB - produto interno bruto
#EP - ?ndice de estabilidade pol?tica

read.table("Ex-ID-Mingoti.txt", header=T,sep="")->x
x

set.seed(1)     ###############usando centroide aleatorio (fixando semente)
km=kmeans(x[,-1],4) 
km$cluster
km$centers

cbind(x[,1],fitted(km, method = "centers") )

#kmeans(x, 4)

#########distancia entre os clusters
dc=dist(km$centers,method="euclidean") ## Metodo de distancia utilizado - Euclidiana
dc
#md1<- as.dist(dc, diag=T)
#md1
 
###Plotar dispercao 2x2 considerando os clusters
windows()
plot(x, col=km$cluster)
points(km$centers, col=1:5, pch=8)

###Plotar em 2 dimensoes usando scores da PCA (PC1 e PC2)

pc<-prcomp(x[,-1], scale.= T)
pc
summary(pc)
z<-pc$x

windows()
plot(z[,1],z[,2],pch=16,col=km$cluster,xlab="pc1", ylab="pc2", main="EX Mingoti - centroides aleatorios")
text(z,labels=x[,1])


####################Usando centr?ide via Cluster hierarquico (Ward)
####################

d=dist(x[,-1],method="euclidean")
m<- as.dist(d, diag=T)
m

hc=hclust(m, method = "ward.D2")

#hc$labels
hc$height
hc$center
windows()
plot(hc, labels=x[,1])
rect.hclust(hc, k=4, border=1:3)
abline(h=1.5, col="red", lty=2)
##################determinando os centroides
grupos=matrix(cutree(hc, h=1.5), ncol=1) ###3definir crte baseado em uma dist?ncia
paises=data.frame(paises=x[,1])
ord=cbind(paises,grupos)
ord
g1=colMeans(x[which(ord[,2]==1),-1])
g2=colMeans(x[which(ord[,2]==2),-1])
g3=colMeans(x[which(ord[,2]==3),-1])
g4=colMeans(x[which(ord[,2]==4),-1])

center<-rbind(g1,g2,g3,g4)

kh=kmeans(x[,-1],center)
kh$cluster
#kmeans(x, center)
cbind(x[,1],fitted(kh, method = "centers") )

#########distancia entre os clusters
kh$centers
dc2=dist(kh$centers,method="euclidean") ## Metodo de distancia utilizado - Euclidiana
md2<- as.dist(dc2, diag=T)
md2


windows()
plot(z[,1],z[,2],pch=16,col=kh$cluster,xlab="pc1", ylab="pc2", main="EX Mingoti - 4 centroides do Hclust" )
text(z,labels=x[,1])


###########################Usando centr?ide dos 4 primeiros paises

centerr=x[1:4,-1]
kk=kmeans(x[,-1],centerr)
kk$cluster
#kmeans(x, centerr)
cbind(x[,1],fitted(kk, method = "centers") )

#########distancia entre os clusters
kk$centers
dc3=dist(kk$centers,method="euclidean") ## Metodo de distancia utilizado - Euclidiana
md3<- as.dist(dc3, diag=T)
md3

windows()
plot(z[,1],z[,2],pch=16,col=kk$cluster, xlab="pc1", ylab="pc2", main="EX Mingoti - centroide 4 primeiros paises do banco")
text(z,labels=x[,1])

#################################################################
#############Banco iris
#######An?lise confirmat?ria
##Kmeans usando cent?roide aleatorio
x=iris

k=kmeans(x[,-5], 3) ## escolha aleatoria dos centroides (default) 

k$cluster  ########Mostra os grupos
k$centers ########### mostra os centr?ides finais

cbind(x[,5],fitted(k, method = "centers"))  ###############Mostra os centroides e os grupos

###Plotar em 2 dimensoes usando scores da PCA (PC1 e PC2)

pc<-prcomp(x[,-5], scale.= T)
pc
summary(pc)
z<-pc$x

windows()
plot(z[,1],z[,2],pch=16,col=k$cluster,xlab="pc1", ylab="pc2", main="IRIS data - centroides aleatorios")
text(z,labels=x[,5])

##Kmeans usando centroide de acordo com os verdadeiros grupos

g1=colMeans(x[which(x[,5]=="setosa"),-5])
g2=colMeans(x[which(x[,5]=="versicolor"),-5])
g3=colMeans(x[which(x[,5]=="virginica"),-5])

center=rbind(g1,g2,g3)

kk=kmeans(x[,-5], center) ## escolha aleatoria dos centroides (default) 

kk$cluster  ########Mostra os grupos
kk$centers ########### mostra os centr?ides finais

cbind(x[,5],fitted(kk, method = "centers"))  ###############Mostra os centroides e os grupos

###Plotar em 2 dimensoes usando scores da PCA (PC1 e PC2)

windows()
plot(z[,1],z[,2],pch=16,col=kk$cluster,xlab="pc1", ylab="pc2", main="IRIS data - centroides aleatorios")
text(z,labels=x[,5])

