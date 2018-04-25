## all of this is the same

y = rnorm(n = 10000, mean = 0, sd = 1)
x1 = rnorm(n = 10000, mean = 0, sd = 1)
x2 = rnorm(n = 10000, mean = 0, sd = 1)
x3 = rnorm(n = 10000, mean = 0, sd = 1)
x4 = rnorm(n = 10000, mean = 0, sd = 1)
x5 = rnorm(n = 10000, mean = 0, sd = 1)


gradientR<-function(y, X, epsilon,eta, iters){
  epsilon = 0.0001
  X = as.matrix(data.frame(rep(1,length(y)),X))
  N= dim(X)[1]
  print("Initialize parameters...")
  theta.init = as.matrix(rnorm(n=dim(X)[2], mean=0,sd = 1)) # Initialize theta
  
  theta.init = t(theta.init)
  e = t(y) - theta.init%*%t(X)
  grad.init = -(2/N)%*%(e)%*%X
  theta = theta.init - eta*(1/N)*grad.init
  l2loss = c()
  for(i in 1:iters){
    l2loss = c(l2loss,sqrt(sum((t(y) - theta%*%t(X))^2)))
    e = t(y) - theta%*%t(X)
    grad = -(2/N)%*%e%*%X
    theta = theta - eta*(2/N)*grad
    print(sqrt(sum(grad^2)))
    if(sqrt(sum(grad^2)) <= epsilon){
      break
    }
  }
  print("Algorithm converged")
  print(paste("Final gradient norm is",sqrt(sum(grad^2))))
  values<-list("coef" = t(theta), "l2loss" = l2loss)
  return(values)
}

normalest <- function(y, X){
  X = data.frame(rep(1,length(y)),X)
  X = as.matrix(X)
  theta = solve(t(X)%*%X)%*%t(X)%*%y
  return(theta)
}

ptm <- proc.time()
speedglm()
gdec.eta1 = gradientR(y = y, X = data.frame(x1,x2,x3, x4,x5), eta = 100, iters = 1000)
gdec.eta1 = SGD(y = y, X = data.frame(x1,x2,x3, x4,x5),n=10000, eta = 100, iters = 1000)



SGD <- function(y, X, n, eta,iters) {
  idx <- sample(nrow(X), n)
  y_m=as.matrix(y)
  y_idx <- y_m[idx, , drop = FALSE]
  y<-c(y_idx)
  x <- X[idx, , drop = FALSE]
  gradientR(y, x, eta=100,iters=1000)
}
typeof(X)
t(x)
nrow(y)
as.matrix(y)
y_m<-as.matrix(y)
y_m_idx<-y_m[3,]
y_d<-data.frame(y)
gdec.eta1$coef #Coefficients from gradient descent

plot(1:length(gdec.eta1$l2loss),gdec.eta1$l2loss,xlab = "Epoch", ylab = "L2-loss")
lines(1:length(gdec.eta1$l2loss),gdec.eta1$l2loss)