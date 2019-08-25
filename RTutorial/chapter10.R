require(nnet)
myNN <- function(x){
  W <- c(0.4, -0.1, 0.5)
  b <- -0.5
  theta <- 0
  return(sum(x*W)+b)
}
data_set <- matrix(c(0.3,0.1,0.8,
                     0.5,0.6,0.3,
                     0.1,0.2,0.1,
                     0.8,0.7,0.7,
                     0.5,0.5,0.6), ncol=3, byrow=T)
apply(data_set, 1, myNN)



set.seed(1000)
test_idx <- sample (1:nrow(iris), 50, replace=F)

train_x <- iris[-test_idx, -5]
train_y <- as.numeric(iris[-test_idx, 5])

test_x <- iris[test_idx, -5]
test_y <- as.numeric(iris[test_idx, 5])
model <- nnet(train_x, train_y, size=4, rang=sqrt(nrow(train_x/2)), decay=5e-4, maxit=200)
model$wts

summary(model)
str(model)