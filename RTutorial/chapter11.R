
sigmoid <- function(x){
  return(1.0/(1.0+exp(-x)))
}

softmax <- function(x){
  return(exp(x)/sum(exp(x)))
}

train_x <- c(0.5, 0.8, 0.2)
train_y = 1
distance = 1
alpah = 0.5
w <- c(0.4, 0.7, 0.8)
for(i in 1:5){
  print(paste("============= repeate",i,"=============="))
  error = distance - sum(w*train_x)
  print(paste("error",error))
  delta_w = alpha * error * train_x
  print(paste("delta_w",delta_w))
  w = w + delta_w
  print(paste("w",delta_w))
}

w <- c(0.5, 0.8, 0.2)
alpha = 1
for(i in 1:5000){
  # origin_w = sigmoid(sum(w*train_x))
  # get dw/dx
  dw_per_dx = sigmoid(sum(w*train_x)) * (1- sigmoid(sum(w*train_x)))
  error = train_y - sigmoid(sum(w*train_x))
  dx = (alpha * error * train_x)
  delta_w = dw_per_dx * dx
  # add delta w every training
  w = w + delta_w
  print(error)
}
w

# dot product
a = matrix(1:6, nrow=2,byrow=T)
b = matrix(7:12, nrow=3,byrow=T)
a %*% b

# one-hot encoding
library(nnet)
iris_x = iris[,-5]
iris_x
iris_y_one_hot = class.ind(iris[,5])
iris_y_one_hot

SLP.SGD <- function(data, y_one_hot, alpha=0.05, epoch=1000){
  # init weight
  random_nums <- ncol(data) * ncol(y_one_hot)
  w <- runif(random_nums, min = -0.5, max = 0.5)
  w <- matrix(w, nrow=ncol(data))
  # update w
  for (i in 1:epoch){
    for(k in 1:nrow(data)){
      train_x = rbind(data[k,])
      dw_per_dx = sigmoid(sum(train_x %*% w)) * (1- sigmoid(sum(train_x %*% w)))
      error = y_one_hot[k,] - sigmoid(train_x %*% w)
      
      dx <- c()
      for(a in 1:length(train_x)){
        dx = rbind(dx ,(alpha * train_x[a] * error))
      }
      
      w = w + dw_per_dx * dx
    }
    cat("error",i,mean(error), "\n")
  }
  return(w)
}
runif(ncol(iris_train_x) * ncol(iris_train_y_one_hot))
matrix(iris_train_x,nrow())
length(rbind(iris_train_x[1,]))
dx <- c()
dx <- rbind(dx, iris_train_y_one_hot[1,])
dx
iris_train_y_one_hot[1,]
rbind(iris_train_x[1,])
# train and make model

# exercise 01
iris_train_x <- as.matrix(iris[,-5])
iris_train_x
iris_train_y_one_hot <- rbind(class.ind(iris[,5]))
SLP.SGD <- function(ds,cl,alpha=0.05,rep=1000) {
  #initialize w
  n <- ncol(ds)*ncol(cl)
  w = runif(n, min= -0.5, max= 0.5)
  w = matrix(w, nrow=ncol(ds))
  # update w
  plot_e <- c()
  for (i in 1:rep) {
    for(k in 1:nrow(ds)) {
      x = rbind(ds[k,])
      v = x %*% w
      y = sigmoid(v)
      e = cl[k,] - y
      w = w + alpha * cbind(ds[k,])%*%(y*(1-y)*e)
    }
    cat("error",i,mean(e), "\n")
    plot_e <- c(plot_e, mean(e))
  }
  return(plot_e)
}

error_plot <- c()
error_plot_0.05 <- SLP.SGD(iris_train_x, iris_train_y_one_hot, alpha=0.05, rep=1000)
error_plot_0.1 <- SLP.SGD(iris_train_x, iris_train_y_one_hot, alpha=0.1, rep=1000)
error_plot_0.5 <- SLP.SGD(iris_train_x, iris_train_y_one_hot, alpha=0.5, rep=1000)
plot(error_plot_0.05, type='l', pch=2, col="green")
lines(error_plot_0.1, type="l", pch=2, col="red")
lines(error_plot_0.5, type='l', pch=2, col="blue")
legend("bottomright", c("0.05","0.1","0.5"), col=c("green","red","blue"), cex=0.8, lty=1)

come <- c()
come <- rbind(come, c(1,2,3))
come <- rbind(come, c(2,3,4))
come

lines(c(4:8), type="l", pch=22, col="red")
lines(c(1:5), type='l', pch=2, col="blue")
plot(c(1:5), type='l', pch=2, col="green")
alpah = 1
for(i in 1:50){
  dw_per_dx = sigmoid(sum(w*iris_x)) * (1-sigmoid(sum(w*iris_x)))
  
}
