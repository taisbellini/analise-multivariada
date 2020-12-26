###############Hierarchical Agglomerative Clusters

#############Exemplo 12.3
dist=matrix(c(0,9,3,6,11,9,0,7,5,10,3,7,0,9,2,6,5,9,0,8,11,10,2,8,0), ncol=5)
dist
m <- as.dist(dist, diag=T)


hc1=hclust(m, method = "single") 
hc2=hclust(m, method = "complete") 
hc3=hclust(m, method = "average") 
hc4=hclust(m, method = "centroid")
hc5=hclust(m, method = "ward.D2")


hc1$height
hc2$height
hc3$height
hc4$height
hc5$height




## Dendograma de cluster hierarquico 
windows()
par(mfrow=c(2,3))

plot(hc1, main= "Ex12.3 - single")
plot(hc2, main= "Ex12.3 - complete" )
plot(hc3, main= "Ex12.3 - average")
plot(hc4, main= "Ex12.3 - centroid")
plot(hc5, main= "Ex12.3 - ward.D2")


#####################Exemplo 12.4
##similaridade na primeira letra da grafia dos n?meros 1 a 10 em 11 idiomas

read.table("Wichern_data/T12-3.dat", fill=T, header=F,sep="")->d
rownames(d)=c("E",  "N", "Da", "Du",  "G", "Fr", "Sp",  "I",  "P",  "H", "Fi")
colnames(d)=c("E",  "N", "Da", "Du",  "G", "Fr", "Sp",  "I",  "P",  "H", "Fi")
d[upper.tri(d)] <- t(d)[upper.tri(d)]
d
dd<-10-d
dd
m<- as.dist(dd, diag=T)
m

hc=hclust(m,method= "ward.D2")  ###########usando metodo ward


hc$height

## Dendograma de cluster hierarquico 
windows()
#par(mfrow=c(2,3))
plot(hc, main= "Ex12.4 - ward", hang=-1)
abline(h=5.5, col="red", lty=2)
rect.hclust(hc, k=3, border=1:3)

##################Exempo alternativo
############################################################
renda<-c(5,6,15,16,25,30)
escolaridade<-c(5,6,14,15,20,19)
x=data.frame(renda, escolaridade)
rownames(x)=c("1","2","3","4","5","6")

d=dist(x,method="euclidean")
m<- as.dist(d, diag=T)
m

hc=hclust(m, method = "complete")

hc$height

plot(hc, main= "ExY - ward", hang=-1 )
abline(h=6, col="red", lty=2)

windows()
plot(hc, main= "ExY - ward", hang=-1 )
rect.hclust(hc, k=3, border=1:3)

###########Outras formas de colorir
#windows()
#plot(hc, labels=x[,1], main= "ExY - ward", hang=-1 )
#rect.hclust(hc, k=3, border="red")
#rect.hclust(hc, h=0.3, border="blue")

######################################################
############################ EXEMPLO  MINGOTI
### Dados de 21 paises e 4 variaveis, de acordo com o banco de dados da ONU    
#EV - expectativa de vida
#Edu - ?ndice de educa??o
#PIB - produto interno bruto
#EP - ?ndice de estabilidade pol?tica


read.table("Ex-ID-Mingoti.txt", header=T,sep="")->x
x
View(x)
####################agrupando via m?todo hierarquico (Ward)

d=dist(x[,-1],method="euclidean")
m<- as.dist(d, diag=T)
m


hc=hclust(m, method = "ward.D2")

#hc$labels
hc$height
#hc$center

#grupos=matrix(cutree(hc, k=4), ncol=1) ####definir cortes baseado no numero k de grupos
grupos=matrix(cutree(hc, h=1.5), ncol=1) ###3definir crte baseado em uma dist?ncia
paises=data.frame(paises=x[,1])
ord=cbind(paises,grupos)
ord

windows()
plot(hc, labels=x[,1], main= "Ex-ID-Mingoti - ward", hang=-1 )
abline(h=1.5, col="red", lty=2)

windows()
plot(hc, labels=x[,1], main= "Ex-ID-Mingoti - ward", hang=-1 )
rect.hclust(hc, k=4, border=1:4)

#####outra maneira de colorir
require(dendextend)
hc_col_dend <- hc %>% as.dendrogram() %>%  color_branches(h = 1.5) # Fun??o que colore os clusters
windows()
plot(hc_col_dend)

###Plotar em 2 dimensoes usando scores da PCA (PC1 e PC2)

pc<-prcomp(x[,-1], scale.= T)
pc
summary(pc)
z<-pc$x


windows()
plot(z[,1],z[,2], main= "procurando agrupamento via pca") ############buscando agrupamentos via pca
text(z,labels=x[,1])

#####################agrupando (via pcs) utilizando m?todo hierarquico (Ward)
##ulilizando 2 pc
dpc=dist(z[,1:2],method="euclidean")
mpc<- as.dist(dpc, diag=T)
mpc

hcpc=hclust(mpc, method = "ward.D2")

windows()
plot(hcpc, labels=x[,1], main= "Ex12.4 - ward nos pcs", hang=-1)
abline(h=1.5, col="red", lty=2)
rect.hclust(hc, k=4, border=1:3)
#################################################################
#############Banco iris
#######An?lise confirmat?ria
x=iris
View(iris)
d=dist(x[,-5],method="euclidean")
m<- as.dist(d, diag=T)
m


hc=hclust(m, method = "complete")

#hc$labels
hc$height
#hc$center
ggpairs(x[,-5], mapping=aes(colour=as.factor(x[,5])))

windows()
plot(hc, labels=x[,5], main= "IRIS data - ward", hang=-1 )
abline(h=3, col="red", lty=2)
rect.hclust(hc, k=3, border=1:3)
###Plotar em 2 dimensoes usando scores da PCA (PC1 e PC2)

pc<-prcomp(x[,-5], scale.= T)
pc
summary(pc)
z<-pc$x

windows()
plot(z[,1],z[,2],pch=16, col= "red",xlab="pc1", ylab="pc2", main="IRIS data")
text(z,labels=x[,5])