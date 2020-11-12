### Lista 01 ###

#### Cap 1 ####

#### 1.12 ####

if (!require(mvtnorm)) install.packages("mvtnorm")
library(mvtnorm)

dist = function(P) {
  return(max(abs(P[1]), abs(P[2])))
}

#a) 
P = c(-3, 4)
P_dist = dist(P)
paste("Distance of P to the origin: " , P_dist)

#b) 
# Os pontos que terao distancia 1 da origem sao os que tem +-1 em uma das coordenadas 
#e um numero entre 0 e 1 (+-) na outra 
min = seq(from= -0.9, to = 0.9, by=0.1)
max = c(rep(1,19), rep(-1,19))

#Todas as combinacoes que geram dist = 1
x1 = c(min, max)
x2 = c(max, min)
points = as.matrix(expand.grid(x1 = x1, x2 = x2))
points = points[((abs(points[,"x1"])) == 1 | (abs(points[, "x2"]) == 1)),]

plot(points)

points_dist = apply(points, 1, dist)
plot(points_dist)

#c)

dist_gen = function(P){
  abs_values = sapply(P, abs)
  return(max(abs_values))
}

P_gen = c(2, -3, 5, -8)
dist_gen(P_gen)

#### 1.17 ####

data = read.table("../Wichern_data/T1-9.dat", sep = "\t")
head(data)
colnames(data) = c("Country", "100m", "200m", "400m", "800m", "1500m", "3000m", "Marathon")

x_barra = sapply(data[,2:8], mean)
x_barra

Sn = cov(data[2:8])
Sn

R = cor(data[2:8])
R

# Observa-se que entre distancias com metragem masi proxima, a correlacao eh maior
# enquanto grandes diferencas de distancia tem uma correlacao menor
# Max: 1500m - 3000m: 0.97
# Min: 100m - Marathon: 0.66

#### 1.18 ####

#Convertendo os tempos para metros/segundo

data2 = data
data2[,2] = sapply(data2[,2], function(x){100/x})
data2[,3] = sapply(data2[,3], function(x){200/x})
data2[,4] = sapply(data2[,4], function(x){400/x})
data2[,5] = sapply(data2[,5], function(x){800/(x*60)})
data2[,6] = sapply(data2[,6], function(x){1500/(x*60)})
data2[,7] = sapply(data2[,7], function(x){3000/(x*60)})
data2[,8] = sapply(data2[,8], function(x){42195/(x*60)})

data2

x_barra2 = sapply(data2[,2:8], mean)
x_barra2

Sn2 = cov(data2[2:8])
Sn2

R2 = cor(data2[2:8])
R2

#Observa-se o mesmo resultados para a variavel convertida para metros por segundo

#### Cap 2 ####

#### 2.30 ####

mu = c(4,3,2,1)
sigma = rbind(c(3,0,2,2), c(0,1,1,0), c(2,1,9,-2), c(2,0,-2,4))

X = rmvnorm(100, mean = mu, sigma = sigma)

X1 = X[,1:2]
X1 = t(X1)
X2 = X[,3:4]
X2 = t(X2)

A = c(1,2)
B = rbind(c(1,-2), c(2,-1))

## a) E(X1) 
eX1 = apply(X1, 1, mean)
eX1

## b) E(AX1)
AX1 = A%*%X1
eAX1 = apply(AX1, 1, mean)
eAX1

## c) Cov(X1)
covX1 = cov(t(X1))
covX1