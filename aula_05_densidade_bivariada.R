require(mvtnorm)
#require(rgl)
require(plot3D)

n=1000
sm <- matrix(c(4,3.5,3.5,4), ncol=2)
sm
x <- rmvnorm(n, mean=c(0,0), sigma=sm, method="chol")
plot(x)

set.seed(70)


a<-matrix(c(0,0), ncol=1)
b<-c(4,0,0,4)
b<-rbind(b,c(4,2,2,4))
b<-rbind(b,c(4,3,3,4))
b<-rbind(b, c(4,3.9,3.9,4))
n<-100000
windows()
par(mfrow=c(2,4))  
#i=1
for(i in 1:4){

sm <- matrix(b[i,], ncol=2)
sm
mu<-a 
#x1<-runif(n,-6,8)
#x2<-runif(n,-5,7)
x1<-rnorm(n,mu[1,1],sqrt(sm[1,1]))
x2<-rnorm(n,mu[2,1], sqrt(sm[2,2])) 
x<-data.frame(x1,x2)
y<-dmvnorm(x, mean = a, sigma = sm, log = FALSE) 
#scatterplot3d(x[,1],x[,2],y)
#aux <-paste(b[i,1],b[i,2],b[i,3],b[i,4], (b[i,2]/sqrt(b[i,1]*b[i,4])))
aux <-paste(b[i,1],b[i,2],b[i,3],b[i,4])
aux1 <-paste((b[i,2]/sqrt(b[i,1]*b[i,4])))
titulo <- paste("COV = ", aux, "r = ", aux1)
scatter3D(x[,1],x[,2],y, colvar = y, colkey = TRUE, shade = 0.1,box = T, theta = 700)
title(main=titulo)
#}

##################################################################3
###########################curva de n?vel#######################
##################################################################
#i=1
#for(i in 1:4){

#mu<-matrix(c(0,0))
#sm <- matrix(c(4,0,0,4), ncol=2)
dnormm<-function(x,mu,sm){
  p<-dim(x)[1]
  k1<-(2*pi)^(p/2)
  k2<-sqrt(det(sm))
  k3<-t(x-mu)%*%solve(sm)%*%(x-mu)
  dens<-(exp(-0.5*k3))/(k1*k2)
  return(dens)
}

#mu<-matrix(c(0,0))
#sm <- matrix(c(4,0,0,4), ncol=2)

X<-seq(-4,4,0.1)
Y<-seq(-4,4,0.1)
k<-1
xx1<-c()
xx2<-c()
z<-matrix(ncol=length(X),nrow=length(Y))
for (h in 1:length(X)){
  for (j in 1:length(Y)){
    xx1<-X[h]
    xx2<-Y[j]
    xx<-matrix(c(xx1,xx2))
    z[h,j]<-dnormm(xx,mu,sm)
    
    k<-k+1
  }
}
#windows()
contour(X,Y,z,col='red')
}

###########################################################################
####################################################################################


#windows()
######################################################################################
#######Densidade Uma por vez###################################
############################################################################

  
  
n<-100000
sm <- matrix(c(4,1.5,1.5,4), ncol=2)
sm
mu<-matrix(c(2,1), ncol=1)
#mu<-data.frame(mu)

x1<-rnorm(n,mu[1,1],sqrt(sm[1,1]))
x2<-rnorm(n,mu[2,1], sqrt(sm[2,2]))
x<-data.frame(x1,x2)
y<-dmvnorm(x, mean = mu, sigma = sm, log = FALSE)

windows()
scatter3D(x[,1],x[,2],y, colvar = y, colkey = T, shade = 0.1,box = T, theta = 700)
aux <-paste(sm[1,1], sm[2,1],sm[2,1],sm[2,2])
aux1 <-paste(sm[2,1]/sqrt(sm[1,1]*sm[2,2]))
titulo <- paste("COV = ", aux, "r = ", aux1)
title(main=titulo)


#persp3D(x[,1],x[,2],y, col="lightblue")

#dev.new()
#contour(x[,1],x[,2],y,col='red')

#contour(x1,x2,y, xlab="X", ylab="Y", nlevels = 10, method = "simple",xlim = c(-6, 8), ylim = c(-5, 7), col = "blue")
#title(main="Curvas de iso-densidades" , xlab="X", ylab="Y")

#windows()
#par(mfrow=c(1,2))
#scatterplot3d(x[,1],x[,2],y)
#scatter3D(x[,1],x[,2],y, colvar = x1, colkey = T, shade = 0.1,box = T, theta = 150)
#scatter3D(xx[,1],xx[,2],yy, colvar = xx1, colkey = T, shade = 0.1,box = T, theta = 150)
#scatter3D(x[,1],x[,2],y, colvar = x1, colkey = T, shade = 0.1,box = T, theta = 50)
#scatter3D(x[,1],x[,2],y, colvar = x1, colkey = T, shade = 0.1,box = T, theta = 350)

####################################################################################
########################Curva de n?vel uma por vez###
################################################################################



dnormm<-function(x,mu,sm){
  p<-dim(x)[1]
  k1<-(2*pi)^(p/2)
  k2<-sqrt(det(sm))
  k3<-t(x-mu)%*%solve(sm)%*%(x-mu)
  dens<-(exp(-0.5*k3))/(k1*k2)
  return(dens)
}

mu<-matrix(c(0,0))
sm <- matrix(c(4,3,3,4), ncol=2)

X<-seq(-5,5,0.1)
Y<-seq(-5,5,0.1)
k<-1
x1<-c()
x2<-c()
z<-matrix(ncol=length(X),nrow=length(Y))
for (i in 1:length(X)){
  for (j in 1:length(Y)){
    x1<-X[i]
    x2<-Y[j]
    x<-matrix(c(x1,x2))
    z[i,j]<-dnormm(x,mu,sm)
    
    k<-k+1
  }
}
windows()
#contour(X,Y,z,col='red', nlevels = 10)
contour(X,Y,z,col='red', levels = c(0.005,0.01,0.015, 0.02))
aux <-paste(sm[1,1], sm[2,1],sm[2,1],sm[2,2])
aux1 <-paste(sm[2,1]/sqrt(sm[1,1]*sm[2,2]))
titulo <- paste("COV = ", aux, "r = ", aux1)
title(main=titulo)






