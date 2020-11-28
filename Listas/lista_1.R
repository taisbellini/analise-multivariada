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
remove(list = ls())
mu = c(4,3,2,1)
sigma = rbind(c(3,0,2,2), c(0,1,1,0), c(2,1,9,-2), c(2,0,-2,4))

X = rmvnorm(100, mean = mu, sigma = sigma)

# X1 100x2
X1 = X[,1:2]

# X2 100x2
X2 = X[,3:4]

A = c(1,2)
B = rbind(c(1,-2), c(2,-1))

# AX1 1x100
AX1 = A%*%t(X1)
AX1

# BX2 2x100
BX2 = B%*%t(X2)
BX2

## a) E(X1) 
# 1x2
eX1 = apply(X1, 2, mean)
eX1

## b) E(AX1)
# escalar
eAX1 = apply(AX1, 1, mean)
eAX1

## c) Cov(X1)
# 2x2
covX1 = cov(X1)
covX1

## d) Cov(AX1)
# escalar
covAX1 = cov(t(AX1))
covAX1

## e) E(X2)
# 1x2
eX2 = apply(X2, 2, mean)
eX2

## f) E(BX2)
# 1x2
eBX2 = apply(BX2, 1, mean)
eBX2

## g) Cov(X2)
# 2x2
covX2 = cov(X2)
covX2

## h) Cov(BX2)
# 2x2
covBX2 = cov(t(BX2))
covBX2

## i) Cov(X1, X2)
# 2x2
covX1X2 = cov(X1,X2)
covX1X2

## j) Cov(AX1, BX2)
# 1x2
covAX1BX2 = cov(t(AX1), t(BX2))
covAX1BX2

#### 2.31 ####
remove(list = ls())
mu = c(4,3,2,1)
sigma = rbind(c(3,0,2,2), c(0,1,1,0), c(2,1,9,-2), c(2,0,-2,4))

X = rmvnorm(100, mean = mu, sigma = sigma)

# X1 100x2
X1 = X[,1:2]

# X2 100x2
X2 = X[,3:4]

A = c(1,-1)
B = rbind(c(2,-1), c(0,1))

# AX1 1x100
AX1 = A%*%t(X1)
AX1

# BX2 2x100
BX2 = B%*%t(X2)
BX2

## a) E(X1) 
# 1x2
eX1 = apply(X1, 2, mean)
eX1

## b) E(AX1)
# escalar
eAX1 = apply(AX1, 1, mean)
eAX1

## c) Cov(X1)
# 2x2
covX1 = cov(X1)
covX1

## d) Cov(AX1)
# escalar
covAX1 = cov(t(AX1))
covAX1

## e) E(X2)
# 1x2
eX2 = apply(X2, 2, mean)
eX2

## f) E(BX2)
# 1x2
eBX2 = apply(BX2, 1, mean)
eBX2

## g) Cov(X2)
# 2x2
covX2 = cov(X2)
covX2

## h) Cov(BX2)
# 2x2
covBX2 = cov(t(BX2))
covBX2

## i) Cov(X1, X2)
# 2x2
covX1X2 = cov(X1,X2)
covX1X2

## j) Cov(AX1, BX2)
# 1x2
covAX1BX2 = cov(t(AX1), t(BX2))
covAX1BX2

#### Cap 3 ####

#### 3.2 ####

# a)
X = cbind(c(3,6,3), c(4,-2,1))
colnames(X) = c("x1", "x2")

x_barra = apply(X, 2, mean)
x_barra

plot(X, xlim=c(0,6), ylim = c(-4,4), xlab = "x1", ylab="x2")
points(x_barra[1], x_barra[2], col = "blue")
text(x_barra[1],x_barra[2],"media amostral", pos = 3)


# c) 
d1 = c(-1,2,-1)
d2 = c(3,-3,0)
d1td2 = t(d1)%*%d2
Ld1 = sqrt(6)
Ld2 = sqrt(18)
cos = d1td2/(Ld1*Ld2)
cos

X = cbind(c(3,6,3), c(4,-2,1))
var(X)
cor(X)

#### 3.5 ####

X1 = cbind(c(9,5,1), c(1,3,2))
var(X1)

X2 = cbind(c(3,6,3), c(4,-2,1))
var(X2)


#### 3.9 ####
remove(list=ls())

X = cbind(c(12,18,14,20,16), c(17,20,16,18,19), c(29,38,30,38,35))
x_barra = apply(X, 2, mean)
one = rep(1,5)
x = one%*%t(x_barra)
X - x

S = cov(X)
det(S)

#### 3.11 ####

# definindo S e D ^ -1/2
S = cbind(c(252.04, -68.43), c(-68.43, 123.67))
minDsqr = cbind(c(1/sqrt(252.04), 0), c(0, 1/sqrt(123.67)))

s11 = 252.04
s12 = s21 = -68.43
s22 = 123.67

# Definindo R
r11 = s11/(sqrt(s11)*sqrt(s11))
r12 = s12/(sqrt(s11)*sqrt(s22))
r21 = s21/(sqrt(s22)*sqrt(s11))
r22 = s22/(sqrt(s22)*sqrt(s22))
R = cbind(c(r11, r12), c(r12, r22))

# Calculando R pela definicao do resultado 3-29
R_calc = minDsqr%*%S%*%minDsqr

R
R_calc
#Observa-se que ambos sao iguais, portanto, R = D^-1/2 S D^-1/2

#Definindo D^1/2
Dsqr = cbind(c(sqrt(252.04), 0), c(0, sqrt(123.67)))
# Calculando S pela definicao do resultado 3-30
S_calc = Dsqr%*%R%*%Dsqr

S
S_calc
#Observa-se que ambos sao iguais, portanto, S = D^1/2 R D^1/2

#### 3.14 ####
remove(list = ls())
X = cbind(c(9,5,1), c(1,3,2))
x_barra = as.matrix(apply(X, 2, mean))
S = cov(X)

c = as.matrix(c(-1,2))
b = as.matrix(c(2,3))

sample_mean_c = t(c)%*%x_barra
sample_mean_c
sample_mean_b = t(b)%*%x_barra
sample_mean_b

sample_var_c = t(c)%*%S%*%c
sample_var_c

sample_var_b = t(b)%*%S%*%b
sample_var_b

sample_cov = t(b)%*%S%*%c
sample_cov


#### 3.19 ####
remove(list = ls())

#Definindo S e R
S = cbind(c(0.856,0.635,0.173), c(0.635, 0.568, 0.127), c(0.173,0.128, 0.171))

r11 = S[1,1]/(sqrt(S[1,1])*sqrt(S[1,1]))
r12 = S[1,2]/(sqrt(S[1,1])*sqrt(S[2,2]))
r13 = S[1,3]/(sqrt(S[1,1])*sqrt(S[3,3]))
r21 = S[2,1]/(sqrt(S[2,2])*sqrt(S[1,1]))
r22 = S[2,2]/(sqrt(S[2,2])*sqrt(S[2,2]))
r23 = S[2,3]/(sqrt(S[2,2])*sqrt(S[3,3]))
r31 = S[3,1]/(sqrt(S[3,3])*sqrt(S[1,1]))
r32 = S[3,2]/(sqrt(S[3,3])*sqrt(S[2,2]))
r33 = S[3,3]/(sqrt(S[3,3])*sqrt(S[3,3]))


R = cbind(c(r11, r12, r13), c(r12, r22, r23), c(r31, r32, r33))

# det(S) pelo calculo de determinante:
det_S = det(S)

# det(S) pelo calculo do exercicio:
det_S_calc = S[1,1]*S[2,2]*S[3,3]*det(R)

#Observa-se que os dois sao iguais
det_S
det_S_calc

#### 3.20 ####
remove(list = ls())

# Carrega e limpa os dados
split = function(x){
  row = strsplit(trimws(x), " ", fixed = T)
  row = unlist(row)
  return(row[-2])
}

data = read.table("../Wichern_data/T3-2.dat", sep = "\t", stringsAsFactors = F)
data = lapply(data[,1], split)
data = lapply(data, function(z){ z[!is.na(z) & z != ""]})
data = do.call(rbind, data)
colnames(data) = c("x1", "x2")
X = apply(data, 2, function(x){as.numeric(x)})

# a) 

# Obtendo as estatisticas resumo

x_barra = apply(X, 2, mean)
S = cov(X)

# Vamos definir a combinacao a'X
a = as.matrix(c(-1,1))

sample_mean = t(a)%*%x_barra
sample_var = t(a)%*%S%*%a

# b) 

X_diff = apply(X, 1, function(x){x[2] - x[1]})
sample_mean_first = mean(X_diff)
sample_var_first = var(X_diff)

sample_mean
sample_mean_first

sample_var
sample_var_first

#### Cap 4 ####

#### 4.2 ####

#b) 

remove(list = ls())
require(mvtnorm)
require(plot3D)

mu = c(0,2)
sigma = matrix(c(2, 0.5*sqrt(2), 0.5*sqrt(2),1), ncol = 2)

autovv = eigen(sigma)

#### 4.3 ####
remove(list = ls())

# d)
sigma = cbind(c(1,-2, 0), c(-2,5, 0), c(0,0,2))
A = rbind(c(1/2, 1/2, 0), c(0,0,1))
A%*%sigma%*%t(A)

# e)
Ae = rbind(c(0, 1, 0), c(-5/2,1,-1))
Ae%*%sigma%*%t(Ae)

#### 4.5 ####


# b)
remove(list = ls())

sig11 = 5
sig12 = c(-2, 0)
sig21 = matrix(c(-2,0))
sig22 = matrix(c(1,0,0,2), ncol = 2)

sig = sig11 - sig12%*%solve(sig22)%*%sig21

# c)
remove(list = ls())

sig11 = 2
sig12 = c(1, 2)
sig21 = matrix(c(1,2))
sig22 = matrix(c(1,1,1,3), ncol = 2)

sig = sig11 - sig12%*%solve(sig22)%*%sig21

#### 4.23 ####
require(car)

# a) 
remove(list = ls())
data = c(-0.6, 3.1, 25.3, -16.8, -7.1, -6.2, 25.2, 22.6, 26)
qqPlot(data, distribution = "norm")

qq = qqnorm(data)
qqline(data)

# b) Coeficiente 
cor(qq$x, qq$y)

#### 4.24 ####
remove(list = ls())

# Carrega e limpa os dados
split = function(x){
  row = strsplit(trimws(x), " ", fixed = T)
  row = unlist(row)
  row = row[row != ""]
  return(row)
}

data = read.table("../Wichern_data/P1-4.DAT", header = F, sep = "\t", stringsAsFactors = F)
data = lapply(data[,1], split)
data = do.call(rbind, data)
colnames(data) = c("x1", "x2", "x3")
X = apply(data, 2, function(x){as.numeric(x)})

# a) 

#qqplot x1
qqx1 = qqnorm(X[,1])
qqx1
qqline(X[,1])
qqPlot(X[,1])

# Dentro da banda de confianca, mas uma linha nao muito reta

# qqplot x2
qqx2 = qqnorm(X[,2])
qqx2
qqline(X[,2])
qqPlot(X[,2])

# parece mais proximo da normal que x1, pontos mais em cima da linha

# b) 
n = length(X[,1])
rqx1 = cor(qqx1$x, qqx1$y)
# [1] 0.9402035
# valor na tabela: 0.9351
# Nao rejeita hipotese de normalidade pois rq > valor da tabela

n = length(X[,2])
rqx2 = cor(qqx2$x, qqx1$y)
# [1] 0.9402035
# valor na tabela: 0.6818
# Rejeita hipotese de normalidade pois rq < valor da tabela


#### 4.25 ####

# Primeiro calcula a distancia estatistica ao quadrado
# Esta distancia, se X eh Normal, tem dist chi-quadrado
# Verificamos entao o qqplot para ver se d se aproxima da chi-quadrado

d2<-mahalanobis(X, colMeans(X), cov(X), inverted = FALSE) 
qqPlot(d2, dist="chisq", df=ncol(X), main=paste("Chi-dist"), ylab=paste("d2"))

# Observa-se que os pontos estão en sua maioria dentro da banda de confiança
# e que estão relativamente proximos da reta


#### 4.26 ####
remove(list = ls())
require(ggplot2)
age = c(1, 2, 3, 3, 4, 5, 6, 8,9,11)
price = c(18.95, 19, 17.95,15.54,14,12.95,8.94, 7.49, 6, 3.99)

data = cbind(age, price)
colnames(data) = c("x1", "x2")

# a) 
d2<-mahalanobis(data, colMeans(data), cov(data), inverted = FALSE) 
d2
# b) 

qch = qchisq(0.5, 2)

in50 = (d2 <= qch)
sum(in50)

data_contour = data.frame(cbind(data, in50))

pl <- ggplot(data_contour, aes(x1, x2))
pl + geom_point(aes(color = in50))

#c) 
qqPlot(d2, dist="chisq", df=ncol(data), main=paste("Chi-dist"), ylab=paste("d2"))
qq = qqnorm(data[,2])
cor(qq$x, qq$y)

#### 4.28 ####
remove(list = ls())
# Carrega e limpa os dados
split = function(x){
  row = strsplit(trimws(x), " ", fixed = T)
  row = unlist(row)
  row = row[row != ""]
  return(row)
}

# pag 60 pdf
data = read.table("../Wichern_data/T1-5.dat", header = F, sep = "\t", stringsAsFactors = F)
data = lapply(data[,1], split)
data = do.call(rbind, data)
colnames(data) = paste("x", seq(1:7), sep = "")
X = apply(data, 2, function(x){as.numeric(x)})

solar_rad = X[,2]

qq = qqnorm(solar_rad)
qq
qqline(solar_rad)
qqPlot(solar_rad)

# Observa-se bastante pontos fora da banda nos quantis mais inferiores
cor(qq$x, qq$y)
#[1] 0.9693258


#### 4.29 ####

#a) 
d2<-mahalanobis(X[,5:6], colMeans(X[,5:6]), cov(X[,5:6]), inverted = FALSE) 
d2

#b) 
qch = qchisq(0.5, 2)

in50 = (d2 <= qch)
sum(in50)
paste("Proportion:", sum(in50)/length(d2))

#c) 
qqPlot(d2, dist="chisq", df=ncol(X[,5:6]), main=paste("Chi-dist"), ylab=paste("d2"))


#### 4.34 ####
remove(list = ls())

# Carrega e limpa os dados
split = function(x){
  row = strsplit(trimws(x), " ", fixed = T)
  row = unlist(row)
  row = row[row != ""]
  return(row)
}

# pag 64 pdf
data = read.table("../Wichern_data/T1-8.DAT", header = F, sep = "\t", stringsAsFactors = F)
data = lapply(data[,1], split)
data = do.call(rbind, data)
colnames(data) = paste("x", seq(1:6), sep = "")
X = apply(data, 2, function(x){as.numeric(x)})

colnames(X) = paste("x", seq(1:6), sep ="")

###Verificando Normalidade univariada###
##qqplot
par(mfrow=c(3,2)) 
for( i in 1:ncol(X)){
  qqPlot(X[,i], dist="norm", main=paste("x_",i), ylab=paste("empirical"))}

###shapiro univariado###
W=rep(0,ncol(X))
for(k in ncol(X)){
  W[k]=shapiro.test(X[,k])$p.value }
W

### coef correlacao ###
## ref value 0.5: 0.9591
Q=rep(0,ncol(X))
for(k in ncol(X)){
  qq = qqnorm(X[,k])
  Q[k] = cor(qq$x, qq$y)
}
Q
sum(Q >= 0.9591)

# Tanto o teste shapiro quanto o teste do coeficiente de correlacao rq 
# Indicam que apenas para a variavel x6 aceitariamos a hipotese de normalidade

### Verificando a normalidade bivariada ###
# Fazendo pelo teste da distancia estatistica e contorno de 50%
biv_test = rbind(
  c(1, 2),
  c(1, 3), 
  c(1, 4),
  c(1, 5),
  c(1, 6),
  c(2, 3),
  c(2, 4),
  c(2, 5), 
  c(2, 6),
  c(3, 4),
  c(3, 5),
  c(3, 6),
  c(4, 5),
  c(4, 6),
  c(5, 6)
)
biv_test = cbind(biv_test, rep(0, length(biv_test[,1])))
colnames(biv_test) = c("var1", "var2", "proportion 50%")

for(row in length(biv_test[,1])){
  vars = biv_test[row,-3]
  X_biv = X[,vars]
  d2<-mahalanobis(X_biv, colMeans(X_biv), cov(X_biv), inverted = FALSE)  ##dist multvariada ~~ QUi-q
  qch = qchisq(0.5, 2)
  in50 = (d2 <= qch)
  biv_test[row, 3] = sum(in50)/length(d2)
}
biv_test

# Apenas 5 e 6 apresentam valor proximo de 50%, mas ainda nao exatamente

#### 4.36 ####
remove(list = ls())
data = read.table("../Wichern_data/T1-9.dat", header = F, sep = "\t", stringsAsFactors = F)
head(data)

###Verificando Normalidade univariada###
##qqplot
par(mfrow=c(3,3)) 
for( i in 2:ncol(data)){
  qqPlot(data[,i], dist="norm", main=paste("x_",i), ylab=paste("empirical"))}

###shapiro univariado###
W=rep(0,ncol(data))
for(k in ncol(data[,-1])){
  W[k]=shapiro.test(data[,k])$p.value }
W

### coef correlacao ###
## ref value 0.5: 0.9787
Q=rep(0,ncol(data))
for(k in ncol(data)){
  qq = qqnorm(data[,k])
  Q[k] = cor(qq$x, qq$y)
}
Q
sum(Q >= 0.9787)

# Nao ha evidencia que nenhuma das variaveis tenha distribuicao normal

###Teste de Normalidade Multivariada###
x=as.matrix(data[,-1])
mvShapiro.Test(x)  ## teste Shapiro
#p-value = 1.201e-15
# Nao ha evidencia de normalidade multivariada

#calcula a distancia estatistica 
#depois validamos se o d2 tem dist quiquadrado (que eh o caso da normal multivariada)
d2<-mahalanobis(x, colMeans(x), cov(x), inverted = FALSE)  ##dist multvariada ~~ QUi-q
d2

qqPlot(d2, dist="chisq", df=ncol(x), main=paste("Chi-dist"), ylab=paste("d2"))


