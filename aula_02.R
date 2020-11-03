require(mvtnorm)
######################################
#gerar uma Normal Bivariada com Cov=0
######################################

set.seed(70)

sm <- matrix(c(4,0,0,1), ncol=2)
sm
n<-100
x <- rmvnorm(n, mean=c(0,0), sigma=sm, method="chol")
x
S=cov(x)
cor(x)

######################################
#scatter de x1 e x2
######################################
windows()
plot(x, xlim=c(-6,6), ylim=c(-6,6))
text(2,0,"{2,0}")
text(0,2,"{0,2}")
text(0,0,"{0,0}")


##########################################
#gerar os vetores (2,0) e (0,2)
##########################################

v1<-c(2,0)

v2<-c(0,2)

##################################################
#distancia euclidiana entre v1, v2 e a media (0,0)
##################################################

#ds1=sqrt(v1[1,1]^2)
ds1=sqrt(v1%*%v1)
#ds2=sqrt(v2[1,2]^2)
ds2=sqrt(v2%*%v2)



###################################################
#distancia estatistica entre v1, v2 e a media (0,0)
###################################################
dsc1=sqrt(v1%*%solve(S*diag(2))%*%v1)
dsc2=sqrt(v2%*%solve(S*diag(2))%*%v2)

#dsc1=sqrt((v1[1]^2)/diag(S)[1])
#dsc2=sqrt((v2[2]^2)/diag(S)[2])
dsc1
dsc2


#################################################
#distancia estatistica considerando a covariancia
#################################################

set.seed(70)
require(mvtnorm)
smm <- matrix(c(4,1.7,1.7,1), ncol=2)
smm
n<-100
xw <- rmvnorm(n, mean=c(0,0), sigma=smm, method="chol")
xw
Sw=cov(xw)
cor(xw)

######################################
#scatter de x1 e x2
######################################

plot(xw, xlim=c(-6,6), ylim=c(-6,6))
text(2,0,"{2,0}")
text(0,2,"{0,2}")
text(0,0,"{0,0}")

##################################################
#comparando scatter de x1 e x2 com Cov =0 e Cov=1.7
##################################################

windows()
par(mfrow=c(1,2))
plot(x, xlim=c(-6,6), ylim=c(-6,6))
text(2,0,"{2,0}")
text(0,2,"{0,2}")
text(0,0,"{0,0}")
plot(xw, xlim=c(-6,6), ylim=c(-6,6))
text(2,0,"{2,0}")
text(0,2,"{0,2}")
text(0,0,"{0,0}")

#####################################################################
#gera autovetores para projetar observacoes nos novos eixos rotados 
#####################################################################

z<-eigen(Sw)$vectors
t<-eigen(Sw)$values
t


q<-xw%*%z
q
Sq=cov(q)

################################
#projetando vetor (2,0)
########3#######################

#v1mod<-t(v1)%*%z
v1mod<-t(z)%*%v1

################################
#projetando vetor (0,2)
################################

#v2mod<-t(v2)%*%z
v2mod<-t(z)%*%v2

#############################################
#scatter das observacoes projetadas
#############################################

windows()
plot(q, xlim=c(-6,6), ylim=c(-6,6))
text(v1mod[1],v1mod[2],"{2,0}")
text(v2mod[1],v2mod[2],"{0,2}")
text(0,0,"{0,0}")



#######################################
#distancias para o vetor (2,0)
#######################################
desc1=sqrt(t(v1mod)%*%solve(Sq*diag(2))%*%v1mod)

#desc1=sqrt((v1mod[1]^2/t[1])+(v1mod[2]^2/t[2]))
ds1
dsc1
desc1

#desc1=sqrt(s11mod^2/var(q[,1])+s12mod^2/var(q[,2]))
#desc11=t[1,1]*(2*z[1,1])^2+t[2,1]*(2*z[1,2])^2
#desc11=1.775793*(2*z[1,1])^2+0.2252027*(2*z[1,2])^2



#######################################
#distancias para o vetor (0,2)
#######################################
desc2=sqrt(t(v2mod)%*%solve(Sq*diag(2))%*%v2mod)
#desc2=sqrt((v2mod[1]^2/t[1])+(v2mod[2]^2/t[2]))

ds2
dsc2
desc2


#desc2=sqrt(s21mod^2/var(q[,1])+s22mod^2/var(q[,2]))
#desc22=t[1,1]*(2*z[2,1])^2+t[2,1]*(2*z[2,2])^2


#################Scaterr comparativo############

# windows()
# par(mfrow=c(1,2))
# 
# plot(xw, xlim=c(-6,6), ylim=c(-6,6))
# text(2,0,"{2,0}")
# text(0,2,"{0,2}")
# text(0,0,"{0,0}")
# plot(q, xlim=c(-6,6), ylim=c(-6,6))
# text(v1mod,"{2,0}")
# text(v2mod,"{0,2}")
# text(0,0,"{0,0}")


##########################Representando a elipse nos 2 casos
require (ellipse)
windows()
par(mfrow=c(1,2))
plot(ellipse(S,centre=c(0,0),level=0.9,npoints=1000),type='l',asp=1)  
plot(ellipse(Sw,centre=c(2,1),level=0.9,npoints=1000),type='l',asp=1)
