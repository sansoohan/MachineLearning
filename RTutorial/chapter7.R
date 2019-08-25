#kmeans(x, centers, iter.max = 10, nstart=1, algorithm = c("Hartigan-Wong", "Lloyd", "Forgy, "MacQueen"))

require(graphics)
x<-rbind(matrix(rnorm(100,sd=0.3),ncol=2), matrix(rnorm(100,mean=1,sd=0.3),ncol=2))
x
matrix(rnorm(100,sd=0.3),ncol=2)

colnames(x) <- c("x", "y")
kmeans(x,2)

plot(x)
plot(x, col = kmeans(x,2)$cluster)
points(kmeans(x,2)$center, col = 1:2, pch =8)

# exam01
iris_x <- matrix()
iris_x <- cbind(iris[,1])
iris_x <- cbind(iris_x, iris[,2])
kmeans(iris_x,3)
plot(iris_x, col=kmeans(iris_x,3)$cluster)
points(kmeans(iris_x,3)$center, col = 1:5)

state_x <- cbind(state.x77[,"Income"])
state_x <- cbind(state_x, state.x77[,"Population"])
state_x
cluster <- kmeans(state_x,5)$cluster
center <- kmeans(state_x,5)$center
plot(state_x, col=cluster)
points(center, col = 1:8, pch = 8)
# exam01 end

require("class")
train.idx <- c(1:25, 51:75, 101:125)
test.idx <- c(26:50, 76:100, 126:150)
train.data <- iris[train.idx, 1:4] # 1:25, 51:75, 101:125
train.data
test.data <- iris[test.idx,1:4] # 26:50, 76:100, 126:150
test.data
train.class <- factor(iris[train.idx, 5])
train.class
test.class <- factor(iris[test.idx, 5])
test.class

pred <- knn(train.data, test.data, train.class, k=3, prob=T)
pred

mean(pred == test.class)
table(pred, test.class)


# exam04
require("class")
wdbc <- read.csv('wdbc.data', header = FALSE, sep=',', encoding='utf-8')[,-1]
nrow(wdbc)
wdbc.train_y <- wdbc[seq(1,nrow(wdbc),2),1]
wdbc.train_x <- wdbc[seq(1,nrow(wdbc),2),-1]
wdbc.test_y <- wdbc[seq(2,nrow(wdbc),2),1]
wdbc.test_x <- wdbc[seq(2,nrow(wdbc),2),-1]
for(i in 1:3){
  k=2*i+1
  k_string01 = "k="
  k_string02 = " : "
  print(paste(k_string01,k,k_string02,mean(knn(wdbc.train_x, wdbc.test_x, wdbc.train_y,k , prob=T) == wdbc.test_y),sep=""))
}
# exam04 end

# batch
cut(seq(75*(0)+1,75*1),breaks=2,labels=F)
group <- c()
group <- rbind(group, cut(seq(75*(0)+1,75*1),breaks=2,labels=F))
group

# exam04
require("class")
library("caret")
kfold_cross_validation1 <- function(data,y_col,K,fold_num){
  index_group <- c()
  for(i in 1:fold_num){
    sep_num = nrow(data)/fold_num;
    index_group <- c(index_group, cut(seq(sep_num*(i-1)+1,sep_num*i),breaks=fold_num,labels=F))
  }
  acc <- c()
  for(i in 1:fold_num){
    train_x <- data[index_group != i, -y_col]
    test_x <- data[index_group == i, -y_col]
    train_y <- factor(data[index_group != i, y_col])
    test_y <- factor(data[index_group == i, y_col])
    pred <- knn(train_x, test_x, train_y, k=fold_num)
    acc[i] <- mean(pred == test_y)
  }
  return(mean(acc))
}
kfold_cross_validation1(iris,5,3,5)
# exam04 end


library("class")
library("caret")
kfold_cross_validation <- function(data, y_col, K, fold_num){
  fld <- createFolds(data[, y_col], k=fold_num)
  acc <- c()
  for(i in 1:fold_num){
    train_x <- data[-fld[[i]], -y_col]
    test_x <- data[fld[[i]], -y_col]
    train_y <- factor(data[-fld[[i]], y_col])
    test_y <- factor(data[fld[[i]], y_col])
    pred <- knn(train_x, test_x, train_y, k=K)
    acc[i] <- mean(pred == test_y)
  }
  return(acc)
}
kfold_cross_validation(iris,5,3,5)



# exam05
library(mlbench)
data(BreastCancer)
head(BreastCancer)
breastCancer_data <- BreastCancer[,-c(1,7)]
library("caret")
kfold_cross_validation <- function(data, y_col, K, fold_num){
  fld <- createFolds(data[, y_col], k=fold_num)
  acc <- c()
  for(i in 1:fold_num){
    train_x <- data[-fld[[i]], -y_col]
    test_x <- data[fld[[i]], -y_col]
    train_y <- factor(data[-fld[[i]], y_col])
    test_y <- factor(data[fld[[i]], y_col])
    pred <- knn(train_x, test_x, train_y, k=K)
    acc[i] <- mean(pred == test_y)
  }
  return(acc)
}
mean(kfold_cross_validation(breastCancer_data, length(breastCancer_data), 5, 10))
# exam05 end

# exam06
library(mlbench)
data(PimaIndiansDiabetes)
dim(PimaIndiansDiabetes)
head(PimaIndiansDiabetes)
library("caret")
kfold_cross_validation <- function(data, y_col, K, fold_num){
  fld <- createFolds(data[, y_col], k=fold_num)
  acc <- c()
  for(i in 1:fold_num){
    train_x <- data[-fld[[i]], -y_col]
    test_x <- data[fld[[i]], -y_col]
    train_y <- factor(data[-fld[[i]], y_col])
    test_y <- factor(data[fld[[i]], y_col])
    pred <- knn(train_x, test_x, train_y, k=K)
    acc[i] <- mean(pred == test_y)
  }
  return(acc)
}
mean(kfold_cross_validation(PimaIndiansDiabetes, length(PimaIndiansDiabetes), 5, 10))
# exam06 end