## -----------------------------------------------------------------------------
plot(as.factor(mtcars$hp))
plot(as.factor(mtcars$hp),col=c("red","yellow","blue"))
plot(as.factor(mtcars$hp),mtcars$carb)
plot(women$height,women$weight)
fit=lm(height~weight,data=women)
plot(fit)

## -----------------------------------------------------------------------------
knitr::kable (head(mtcars,n=10))

## -----------------------------------------------------------------------------
set.seed(13456)
n <- 400
u <- runif(n)
x <- 2/(1-u)^(1/2) # F(x) =1-(2/x)^2, 2<=x
hist(x, prob = TRUE, main = expression(f(x)==8/x^3))
y <- seq(2, max(x), .01)
lines(y, 8/y^3,col='red')

## -----------------------------------------------------------------------------
make_simulate_data=function(n){
rlist=c()
for (i in 1:n){
U=runif(3,-1,1)
if (abs(U[3]) >= abs(U[2]) & abs(U[3]) >= abs(U[1])){
  x=U[2]
} else{
  x=U[3]
}
rlist=c(rlist,x)
}
make_simulate_data=rlist
}
x=make_simulate_data(10000)#10000个随机数
hist(x,freq=F,main = expression(f(x)==3/4(1-x^2)))
lines(density(x),col='red')

## -----------------------------------------------------------------------------
set.seed(13456)
n <- 1000
u <- runif(n)
y<- 2/(1-u)^(1/4) -2
hist(y, prob = TRUE, main = expression(f(y)==64/(2+y)^5))
x <- seq(0,  max(y), .01)
lines(x, 64/(2+x)^5,col='red')

## -----------------------------------------------------------------------------
set.seed(123456)
m <- 1e5; x <- runif(m, min=0, max=pi/3)
theta.hat <- mean(sin(x)) * (pi/3)
print(theta.hat)
print(cos(0) - cos(pi/3))

## -----------------------------------------------------------------------------
set.seed(123)
MC.Phi <- function(x, R = 10000, antithetic = TRUE) {
  u <- runif(R/2,0,1)
  if (!antithetic) v <- runif(R/2) else v <- 1 - u
  u <- c(u, v)
  cdf <- numeric(length(x))
  for (i in 1:length(x)) {
    g <- exp(u)
    cdf[i] <- mean(g) 
  }
  cdf
}
exact_value=exp(1)-1;
m <- 1000
x<-1.95
MC1 <- MC2 <- numeric(m)
for (i in 1:m) {
  MC1[i] <- MC.Phi(x, R = 1000, anti = FALSE)
  MC2[i] <- MC.Phi(x, R = 1000)
}

print(c(exact_value,mean(MC1),mean( MC2),sd(MC1),sd(MC2),(var(MC1)-var(MC2))/var(MC1)))

## -----------------------------------------------------------------------------
set.seed(1)
n<- 10000
theta.hat <- se <- numeric(2)
g <- function(x) {
x^2*exp(-x^2/2)/sqrt(2*pi) * (x > 1) * (x < Inf)
}

x <- rexp(n, 1) 
fg <- g(x) / (exp(-x))
theta.hat[1] <- mean(fg)
se[1] <- sd(fg)

x <- rgamma(n,shape=3,scale = 1)
fg <- g(x) / (x^2*exp(-x)/2)
theta.hat[2] <- mean(fg)
se[2] <- sd(fg)
 rbind(theta.hat, se)

## -----------------------------------------------------------------------------
set.seed(1)
M <- 10000
k <- 5 
N <- 50
T2 <- numeric(k)
estimates <- matrix(0, N, 2)

g=function(x) {exp(-x)/(1+x^2)*(x>0)*(x<1)}
f=function(x){exp(-x)/(exp(-(j-1)/5)-exp(-j/5))}

for (i in 1:N) {
  estimates[i, 1] <- mean(g(runif(M)))
  for (j in 1:k){
    u<-runif(2000,0,1)
    v<- -log(exp(-(j-1)/5)-u*(exp(-(j-1)/5)-exp(-j/5)))
    T2[j] <- mean(g(v)/f(v))
  }
  estimates[i,2] = sum(T2)
  
}
apply(estimates, 2,mean)
apply(estimates, 2,sd)

## -----------------------------------------------------------------------------
set.seed(1)
n<-20
alpha<-0.05
L<-numeric(2020)
U<-numeric(2020)
for(i in 1:2020){
    x<-rlnorm(n,meanlog = 0,sdlog = 1)
    y = log(x)
    L[i]<-mean(y)-((sqrt(n/(n-1))*sd(y))/sqrt(n))*qt(1-alpha/2,df = n-1)
    U[i]<-mean(y)+((sqrt(n/(n-1))*sd(y))/sqrt(n))*qt(1-alpha/2,df = n-1)
}
mean(L<0&U>0)

## -----------------------------------------------------------------------------
set.seed(1)
n<-20
alpha<-0.05
L<-numeric(2020)
U<-numeric(2020)
for(i in 1:2020){
  X <- rchisq(n, df = 2)
  L[i]<-mean(X)-((sqrt(n/(n-1))*sd(X))/sqrt(n))*qt(1-alpha/2,df = n-1)
  U[i]<-mean(X)+((sqrt(n/(n-1))*sd(X))/sqrt(n))*qt(1-alpha/2,df = n-1)
}
mean(L<2&U>2)

## -----------------------------------------------------------------------------
set.seed(77)
alpha <- 0.05
n <- 50
m <- 5000
beta<-seq(1,100,4)
sktests=numeric(m)
N=length(beta)
pwr <- numeric(N)
cv <- qnorm(0.975, 0, sqrt(6*(n-2) / ((n+1)*(n+3))))
sk<-function(x){
  xb<-mean(x)
  v3<-mean((x-xb)^3)
  v2<-mean((x-xb)^2)
  return(v3/v2^1.5)
}
for (i in 1:N) {
  for (j in 1:m) {
  x=rbeta(n,beta[i],beta[i])
  sktests[j] <- as.integer(abs(sk(x)) >= cv)
}
pwr[i] <- mean(sktests)
}
plot(beta, pwr, type = "b",
xlab = bquote(beta), ylim = c(0,1))
abline(h = .1, lty = 3)
se <- sqrt(pwr * (1-pwr) / m) 
lines(beta, pwr+se, lty = 3)
lines(beta, pwr-se, lty = 3)

## -----------------------------------------------------------------------------
set.seed(1)
alpha <- 0.05
n <- 50
m <- 1000
t<-seq(1,100,4)
sktests=numeric(m)
N=length(t)
pwr <- numeric(N)
cv <- qnorm(0.975, 0, sqrt(6*(n-2) / ((n+1)*(n+3))))
sk<-function(x){
  xb<-mean(x)
  v3<-mean((x-xb)^3)
  v2<-mean((x-xb)^2)
  return(v3/v2^1.5)
}
for (i in 1:N) {
  for (j in 1:m) {
  x=rt(n,t[i])
  sktests[j] <- as.integer(abs(sk(x)) >= cv)
}
pwr[i] <- mean(sktests)
}
plot(t, pwr, type = "b",
xlab = bquote(t), ylim = c(0,1))
abline(h = .1, lty = 3)
se <- sqrt(pwr * (1-pwr) / m) 
lines(t, pwr+se, lty = 3)
lines(t, pwr-se, lty = 3)

## -----------------------------------------------------------------------------
set.seed(7)
alpha <- 0.055
count5test <- function(x, y) {
X <- x - mean(x)
Y <- y - mean(y)
outx <- sum(X > max(Y)) + sum(X < min(Y))
outy <- sum(Y > max(X)) + sum(Y < min(X))
return(as.integer(max(c(outx, outy)) > 5))
}
n<-c(10,100,1000)
power=rep(0,3)
m=2500
sigma1 <- 1
sigma2 <- 1.5
for (i in 1:3) {
  power[i]<- mean(replicate(m, expr={
  x <- rnorm(n[i], 0, sigma1)
  y <- rnorm(n[i], 0, sigma2) 
  count5test(x, y)
}))
}
print(power)

## -----------------------------------------------------------------------------
set.seed(7)
n<-c(10,100,1000)
power=rep(0,3)
m=2500
sigma1 <- 1
sigma2 <- 1.5
for (i in 1:3) {
  power[i]<- mean(replicate(m, expr={
  x <- rnorm(n[i], 0, sigma1)
  y <- rnorm(n[i], 0, sigma2) 
  return(as.integer(var.test(x,y,alternative = c("two.sided",'less','greater'),conf.level = 0.945)$p.value<alpha))
}))
}
print(power)

## -----------------------------------------------------------------------------
set.seed(1)
n1 <- 20
n2 <- 30
mu1 <- 0
mu2 <- 0
sigma1 <- 1
sigma2 <- 1
m <- 2000
alphahat0<-alphahat1<-numeric(m)

cf <- function(x, y) {
  X <- x - mean(x)
  Y <- y - mean(y)
  outx <- sum(X > max(Y)) + sum(X < min(Y))
  outy <- sum(Y > max(X)) + sum(Y < min(X))
  return(as.integer(max(c(outx, outy)) > 5))
}

cfp <- function(z) {
  x <- z[1:(length(z)/2)]
  y <- z[-(1:(length(z)/2))]
  X <- x - mean(x)
  Y <- y - mean(y)
  outx <- sum(X > max(Y)) + sum(X < min(Y)) 
  outy <- sum(Y > max(X)) + sum(Y < min(X))
  return(as.integer(max(c(outx, outy)) > 5))
}

for (i in 1:m){
  x <- rnorm(n1, mu1, sigma1)
  y <- rnorm(n2, mu2, sigma2)
  x <- x - mean(x) 
  y <- y - mean(y)
  z <- c(x,y)
  per <- function(t,n) {
    op <- numeric(n)
    for (j in 1:n){
      p <- sample(1:length(t),length(t),replace <- F)
      op[j] <- cfp(t[p])
    }
    sum(op)/n
  }
  alphahat0[i]<-cf(x,y)
  alphahat1[i]<-per(z,m)
}
alphahat2<-mean(alphahat0)
alphahat3<-mean(alphahat1)
round(c(cf<-alphahat2,cfp<-alphahat3),6)


## -----------------------------------------------------------------------------
set.seed(7)
library(RANN) 
library(energy)
library(Ball)
library(boot)
mu1<-0
mu2<-0
sigma1<-1
sigma2<-2
m <- 1000
k<-3
p<-2
mu <- 0.3
n1 <- n2 <- 30
R<-999
Tn <- function(z, ix, sizes,k) {
  n1 <- sizes[1]; 
  n2 <- sizes[2]; 
  n <- n1 + n2
  if(is.vector(z)) z <- data.frame(z,0);
  z <- z[ix, ];
  NN <- nn2(data=z, k=k+1) 
  block1 <- NN$nn.idx[1:n1,-1]
  block2 <- NN$nn.idx[(n1+1):n,-1]
  i1 <- sum(block1 < n1 + .5); i2 <- sum(block2 > n1+.5)
  (i1 + i2) / (3 * n)
}
 N = c(n1,n2)
eqdist.nn <- function(z,sizes,k){
  boot.obj <- boot(data=z,statistic=Tn,R=R,
                   sim = "permutation", sizes = sizes,k=k)
  tb <- c(boot.obj$t0,boot.obj$t)
  p.value <- mean(tb>=tb[1])
  list(statistic=tb[1],p.value=p.value)
}
p.values <- matrix(NA,m,3)
for(i in 1:m){
  x <- matrix(rnorm(n1*p,mu1,sigma1),ncol=p);
  y <- matrix(rnorm(n1*p,mu2,sigma2),ncol=p);
  z <- rbind(x,y)
  p.values[i,1] <- eqdist.nn(z,N,k)$p.value
  p.values[i,2] <- eqdist.etest(z,sizes=N,R=R)$p.value
  p.values[i,3] <- bd.test(x=x,y=y,num.permutations=999,seed=i*12345)$p.value
}
alpha <- 0.05;
pow <- colMeans(p.values<alpha)
pow

## -----------------------------------------------------------------------------
set.seed(7)
library(RANN) 
library(energy)
library(Ball)
library(boot)
mu1<-0
mu2<-1
sigma1<-1
sigma2<-2
m <- 1000
k<-3
p<-2
mu <- 0.3
n1 <- n2 <- 30
R<-999
Tn <- function(z, ix, sizes,k) {
  n1 <- sizes[1]; 
  n2 <- sizes[2]; 
  n <- n1 + n2
  if(is.vector(z)) z <- data.frame(z,0);
  z <- z[ix, ];
  NN <- nn2(data=z, k=k+1) 
  block1 <- NN$nn.idx[1:n1,-1]
  block2 <- NN$nn.idx[(n1+1):n,-1]
  i1 <- sum(block1 < n1 + .5); i2 <- sum(block2 > n1+.5)
  (i1 + i2) / (3 * n)
}
 N = c(n1,n2)
eqdist.nn <- function(z,sizes,k){
  boot.obj <- boot(data=z,statistic=Tn,R=R,
                   sim = "permutation", sizes = sizes,k=k)
  tb <- c(boot.obj$t0,boot.obj$t)
  p.value <- mean(tb>=tb[1])
  list(statistic=tb[1],p.value=p.value)
}
p.values <- matrix(NA,m,3)
for(i in 1:m){
  x <- matrix(rnorm(n1*p,mu1,sigma1),ncol=p);
  y <- matrix(rnorm(n1*p,mu2,sigma2),ncol=p);
  z <- rbind(x,y)
  p.values[i,1] <- eqdist.nn(z,N,k)$p.value
  p.values[i,2] <- eqdist.etest(z,sizes=N,R=R)$p.value
  p.values[i,3] <- bd.test(x=x,y=y,num.permutations=999,seed=i*12345)$p.value
}
alpha <- 0.05;
pow <- colMeans(p.values<alpha)
pow

## -----------------------------------------------------------------------------
set.seed(7)
library(RANN) 
library(energy)
library(Ball)
library(boot)
m <- 1000
k<-3
p<-2
mu <- 0.3
n1 <- n2 <- 30
R<-999
Tn <- function(z, ix, sizes,k) {
  n1 <- sizes[1]; 
  n2 <- sizes[2]; 
  n <- n1 + n2
  if(is.vector(z)) z <- data.frame(z,0);
  z <- z[ix, ];
  NN <- nn2(data=z, k=k+1) 
  block1 <- NN$nn.idx[1:n1,-1]
  block2 <- NN$nn.idx[(n1+1):n,-1]
  i1 <- sum(block1 < n1 + .5); i2 <- sum(block2 > n1+.5)
  (i1 + i2) / (3 * n)
}
 N = c(n1,n2)
eqdist.nn <- function(z,sizes,k){
  boot.obj <- boot(data=z,statistic=Tn,R=R,
                   sim = "permutation", sizes = sizes,k=k)
  tb <- c(boot.obj$t0,boot.obj$t)
  p.value <- mean(tb>=tb[1])
  list(statistic=tb[1],p.value=p.value)
}
p.values <- matrix(NA,m,3)
for(i in 1:m){
  x <- matrix(rt(n1*p,1),ncol=p)
  y <- matrix(rt(n1*p,1),ncol=p)
  z <- rbind(x,y)
  p.values[i,1] <- eqdist.nn(z,N,k)$p.value
  p.values[i,2] <- eqdist.etest(z,sizes=N,R=R)$p.value
  p.values[i,3] <- bd.test(x=x,y=y,num.permutations=999,seed=i*12345)$p.value
}
alpha <- 0.05;
pow0<- colMeans(p.values<alpha)
pow0

for(i in 1:m){
  sigma= sample(c(1, 10), size = n1*p,
replace = TRUE, prob = c(0.5,0.5))
x <- matrix(rnorm(n1*p, 0, sigma),ncol=p);
sigma= sample(c(1, 10), size = n1*p,
replace = TRUE, prob = c(0.5,0.5))
y <- matrix(rnorm(n1*p, 0, sigma),ncol=p);
  z <- rbind(x,y)
  p.values[i,1] <- eqdist.nn(z,N,k)$p.value
  p.values[i,2] <- eqdist.etest(z,sizes=N,R=R)$p.value
  p.values[i,3] <- bd.test(x=x,y=y,num.permutations=999,seed=i*12345)$p.value
}
alpha <- 0.05;
pow1 <- colMeans(p.values<alpha)
pow1

## -----------------------------------------------------------------------------
set.seed(7)
library(RANN) 
library(energy)
library(Ball)
library(boot)
mu1<-0
mu2<-0
sigma1<-1
sigma2<-1
m <- 1000
k<-3
p<-2
mu <- 0.3
n1 <- 10
n2 <- 100
R<-999
Tn <- function(z, ix, sizes,k) {
  n1 <- sizes[1]; 
  n2 <- sizes[2]; 
  n <- n1 + n2
  if(is.vector(z)) z <- data.frame(z,0);
  z <- z[ix, ];
  NN <- nn2(data=z, k=k+1) 
  block1 <- NN$nn.idx[1:n1,-1]
  block2 <- NN$nn.idx[(n1+1):n,-1]
  i1 <- sum(block1 < n1 + .5); i2 <- sum(block2 > n1+.5)
  (i1 + i2) / (3 * n)
}
  N = c(n1,n2)
eqdist.nn <- function(z,sizes,k){
  boot.obj <- boot(data=z,statistic=Tn,R=R,
                   sim = "permutation", sizes = sizes,k=k)
  tb <- c(boot.obj$t0,boot.obj$t)
  p.value <- mean(tb>=tb[1])
  list(statistic=tb[1],p.value=p.value)
}
p.values <- matrix(NA,m,3)
for(i in 1:m){
x <- matrix(rnorm(n1*p,mu1,sigma1),ncol=p);
y <- matrix(rnorm(n2*p,mu2,sigma2),ncol=p);
z <- rbind(x,y)
p.values[i,1] <- eqdist.nn(z,N,k)$p.value
p.values[i,2] <- eqdist.etest(z,sizes=N,R=R)$p.value
p.values[i,3] <- bd.test(x=x,y=y,num.permutations=999,seed=i*12345)$p.value
}
alpha <- 0.05;
pow <- colMeans(p.values<alpha)
pow

## -----------------------------------------------------------------------------
set.seed(7)
f <- function(x) {
  if (any(x < 0)) 
    return (exp(x)/2)
  else
    return(exp(-x)/2)
}

rw.Metropolis <- function(sigma, x0, N) {
  x <- numeric(N)
  x[1] <- x0
  u <- runif(N)
  k <- 0
  for (i in 2:N) {
    y <- rnorm(1, x[i-1], sigma)
    if (u[i] <= (f(y) / f(x[i-1])))
      x[i] <- y else {
        x[i] <- x[i-1]
        k <- k + 1
      }
  }
  return(list(x=x, k=k))
}

N <- 5000
sigma <- c(0.01, 0.1, 1, 10)
x0 <- 10
rw1 <- rw.Metropolis(sigma[1], x0, N)
rw2 <- rw.Metropolis(sigma[2], x0, N)
rw3 <- rw.Metropolis(sigma[3], x0, N)
rw4 <- rw.Metropolis(sigma[4], x0, N)
print(c(rw1$k, rw2$k, rw3$k, rw4$k))

p1<-rw1$k/N
p2<-rw2$k/N
p3<-rw3$k/N
p4<-rw4$k/N
print(c(p1, p2, p3, p4))

## -----------------------------------------------------------------------------
Gelman.Rubin <- function(psi) {
# psi[i,j] is the statistic psi(X[i,1:j])
# for chain in i-th row of X
psi <- as.matrix(psi)
n <- ncol(psi)
k <- nrow(psi)
psi.means <- rowMeans(psi) #row means
B <- n * var(psi.means) #between variance est.
psi.w <- apply(psi, 1, "var") #within variances
W <- mean(psi.w) #within est.
v.hat <- W*(n-1)/n + (B/n) #upper variance est.
r.hat <- v.hat / W #G-R statistic
return(r.hat)
}

k <- 4    # four chains
x0 <- c(-10,-5,5,10)    # overdispersed initial values
N <- 10000    # length of chains
b <- 200    # burn-in length


X <- matrix(nrow=k,ncol=N)
for (i in 1:k)
  X[i,] <- rw.Metropolis(0.5,x0[i],N)$x
psi <- t(apply(X, 1, cumsum))
for (i in 1:nrow(psi))
psi[i,] <- psi[i,] / (1:ncol(psi))
rhat <- rep(0, N)
for (j in (1000+1):N)
rhat[j] <- Gelman.Rubin(psi[,1:j])
plot(rhat[(1000+1):N], type="l", xlab="sigma=0.5", ylab="R_hat")
abline(h=1.2, lty=2)

X <- matrix(nrow=k,ncol=N)
for (i in 1:k)
  X[i,] <- rw.Metropolis(1,x0[i],N)$x
psi <- t(apply(X, 1, cumsum))
for (i in 1:nrow(psi))
psi[i,] <- psi[i,] / (1:ncol(psi))
rhat <- rep(0, N)
for (j in (500+1):N)
rhat[j] <- Gelman.Rubin(psi[,1:j])
x2 <- min(which(rhat>0 & rhat<1.2))
plot(rhat[(500+1):N], type="l", xlab="sigma=1", ylab="R_hat")
abline(h=1.2, lty=2)

X <- matrix(nrow=k,ncol=N)
for (i in 1:k)
  X[i,] <- rw.Metropolis(4,x0[i],N)$x
psi <- t(apply(X, 1, cumsum))
for (i in 1:nrow(psi))
psi[i,] <- psi[i,] / (1:ncol(psi))
rhat <- rep(0, N)
for (j in (b+1):N)
rhat[j] <- Gelman.Rubin(psi[,1:j])
x3 <- min(which(rhat>0 & rhat<1.2))
plot(rhat[(b+1):N], type="l", xlab="sigma=4", ylab="R_hat")
abline(h=1.2, lty=2)

X <- matrix(nrow=k,ncol=N)
for (i in 1:k)
  X[i,] <- rw.Metropolis(16,x0[i],N)$x
psi <- t(apply(X, 1, cumsum))
for (i in 1:nrow(psi))
psi[i,] <- psi[i,] / (1:ncol(psi))
rhat <- rep(0, N)
for (j in (b+1):N)
rhat[j] <- Gelman.Rubin(psi[,1:j])
x4 <- min(which(rhat>0 & rhat<1.2))
plot(rhat[(b+1):N], type="l", xlab="sigma=16", ylab="R_hat")
abline(h=1.2, lty=2)

c(x2,x3,x4)

## -----------------------------------------------------------------------------
set.seed(7)
k=c(4:25,100,500,1000)
Ak=numeric()
N=length(k)
for (i in 1:N) {
  solution=uniroot(
    function(a) {pt(sqrt(a^2*(k[i]-1)/(k[i]-a^2)),df=k[i]-1)-pt(sqrt(a^2*(k[i])/(k[i]+1-a^2)),df=k[i])
    },
    lower = 1e-5,upper = sqrt(k[i])-1e-5)
  solution <- as.numeric(unlist(solution)[1])
  if(solution>0 & solution<sqrt(k[i])) Ak[i] <- solution 
  else Ak[i] <- NA
}
matrix(Ak,ncol=1,dimnames = list(paste("k=",k,sep=""),"A(k)"))

## -----------------------------------------------------------------------------
set.seed(7)
nA <- 444
nB <- 132
nOO <- 361
nAB <- 63
N<-1000
func<-function(p,q,n){
  func<-nA*(log(p^2+2*p*(1-p-q)))+nB*(log(q^2+2*q*(1-p-q)))+2*nOO*log(1-p-q)+nAB*(log(2*p*q))
  return(func)
}

p<-0.3
q<-0.1
k<-100
for (step in 1:k) {
  nAA<-rbinom(1,nA,p/(2-p-2*q))
  nBB<-rbinom(1,nB,q/(2-q-2*p))
  p<-(nA+nAA+nAB)/(2*N)
  q<-(nB+nBB+nAB)/(2*N)
  cat("step",step,"p",p,"q",q,"func",func(p,q,N),"\n")
}

## -----------------------------------------------------------------------------
formulas <- list(
mpg ~ disp,
mpg ~ I(1 / disp),
mpg ~ disp + wt,
mpg ~ I(1 / disp) + wt
)
f<-as.character(formulas)
out <- vector("list", length(f))
for (i in seq_along(formulas)) {
out[[i]] <- lm(f[[i]],mtcars)
}
print(out)
out1<-lapply(formulas, function(x) lm(x,mtcars))
print(out1)

## -----------------------------------------------------------------------------
trials <- replicate(
100,
t.test(rpois(10, 10), rpois(7, 10)),
simplify = FALSE
)
p <- sapply(trials, function(x) x[["p.value"]] )
print(p)

## -----------------------------------------------------------------------------
mapvapply <- function(x,f,f_value){
  g <- function(x) Map(f,x)
  vapply(x,g,f_value)
}

## -----------------------------------------------------------------------------
library(Rcpp) 
cppFunction('NumericVector rw_Metropolis(double sigma,double x0,int N) {
  NumericVector x(N);
  x[0]=x0;
  NumericVector u(N);
  u=runif(N);
  for(int i=1;i<N;i++){
    NumericVector y(1);
    y=rnorm(1, x[i-1], sigma);
    if(u[i] <= ((exp(-abs(y[0]))) / (exp(-abs(x[i-1]))))){
      x[i] = y[0];
    }
    else{
      x[i] = x[i-1];
    }
  }
  return x;
}')
sigma <- c(.05, .5, 2, 16)
x0 <- 25
N <- 2000
rw1 = rw_Metropolis(sigma[1],x0,N)
rw2 = rw_Metropolis(sigma[2],x0,N)
rw3 = rw_Metropolis(sigma[3],x0,N)
rw4 = rw_Metropolis(sigma[4],x0,N)
rw = cbind(rw1, rw2, rw3,  rw4)
for (j in 1:4) {
  plot(rw[,j], type="l",
       xlab=bquote(sigma == .(round(sigma[j],3))),
       ylab="X", ylim=range(rw[,j]))
}

## -----------------------------------------------------------------------------
for (j in 1:4) {
  qqplot(1:2000,rw[,j], type="l",
       xlab=bquote(sigma == .(round(sigma[j],3))),
       ylab="X", ylim=range(rw[,j]))
}

## -----------------------------------------------------------------------------
set.seed(3000)

lap_f = function(x) exp(-abs(x))

rw.Metropolis = function(sigma, x0, N){
 x = numeric(N)
 x[1] = x0
 u = runif(N)
 k = 0
 for (i in 2:N) {
  y = rnorm(1, x[i-1], sigma)
  if (u[i] <= (lap_f(y) / lap_f(x[i-1]))) x[i] = y 
  else {
  x[i] = x[i-1]
  k = k+1
  }
 }
 return(list(x = x, k = k))
}

N = 2000
sigma = c(.05, .5, 2, 16)
x0 = 25
rw1 = rw.Metropolis(sigma[1],x0,N)
rw2 = rw.Metropolis(sigma[2],x0,N)
rw3 = rw.Metropolis(sigma[3],x0,N)
rw4 = rw.Metropolis(sigma[4],x0,N)
    rw = cbind(rw1$x, rw2$x, rw3$x,  rw4$x)
    for (j in 1:4) {
        qqplot(1:2000,rw[,j], type="l",
             xlab=bquote(sigma == .(round(sigma[j],3))),
             ylab="X", ylim=range(rw[,j]))
    }

## -----------------------------------------------------------------------------
library(microbenchmark)
sigma0 <- 2
ts <- microbenchmark(
  rw_Metropolis(sigma0, x0, N),
  rw.Metropolis(sigma0, x0, N)
)
summary(ts)[,c(1,3,5,6)]

