head(cars)
plot(dist~speed, data=cars)

cars

dist
speed

model <- lm(dist~speed, cars)
model
coef(model)[1]
coef(model)[2]

speed = 40
dist <- 3.932 * speed - 17.579
dist

speed <- cars[,1]
pred <- 3.932 * speed - 17.579
pred

cars
pred-cars
plot(pred-cars)
plot(pred-cars[,2])

# simple plot1
compare <- cbind(3.932*cars[,1] - 17.579, cars[,2], abs(pred-cars[,2]))
compare
plot(dist~speed, data=cars)
# linear regression1
abline(coef(lm(dist~speed, cars)))

# simple plot2
plot(Sepal.Length~Sepal.Width, data=iris)
# linear regression2
abline(coef(lm(Sepal.Length~Sepal.Width, iris)))

iris
# exam01
state_model <- coef(lm(Murder~Illiteracy, data=data.frame(state.x77)))
state_model

state_model_b = state_model["(Intercept)"]
state_model_w = state_model["Illiteracy"]
for(i in 1:3){
  illiteracy_x = 0.5*i
  state_model_h = state_model_w * illiteracy_x + state_model_b
  names(state_model_h) <- c("Murder")
  print(state_model_h)
}

# multiple matrix plot
library(carData)
head(Prestige)
plot(Prestige[,c(1:4)], pch=16, col="blue", main="Matrix Scatterplot")



# get Influence of each factor
mod1 <- lm(income ~ education + prestige + women, data=Prestige[,c(1:4)])
mod1 <- lm(income ~ ., data=Prestige[,c(1:4)])
# show Influence of each factor
mod1

# use Influence of each factor for prediction
summary(mod1)
summary(mod1)$coefficient["(Intercept)","Estimate"]
mod1$coefficient["(Intercept)"]
education = 9.5
prestige = 80
women = 20
predict_income <- function(education, prestige, women){
  income <-
  ((summary(mod1)$coefficients)[,"Estimate"])["(Intercept)"] +
  ((summary(mod1)$coefficients)[,"Estimate"])["education"]*education +
  ((summary(mod1)$coefficients)[,"Estimate"])["prestige"]*prestige -
  ((summary(mod1)$coefficients)[,"Estimate"])["women"]*women
  return(income)
}
predict_income(education,prestige,women)


library(MASS)
mod2 <- lm(income ~ education + prestige + women + census, data=Prestige[,c(1:5)])
mod2 <- lm(income ~ ., data=Prestige[,c(1:5)])
step <- stepAIC(mod2, direction = "both")
mod3 <- lm(income ~ prestige + women, data=newdata2)
summary(mod3)

# exam02
library(MASS)
library(mlbench)
boston_model<- lm(medv ~ .,BostonHousing)
boston_model$coefficients
boston_step<- stepAIC(boston_model, direction="both")
boston_step$coefficients
summary(boston_step$coefficients)
boston_step$coefficients
boston_model<- lm(medv ~ crim + zn + chas + nox + rm + dis + rad + tax + ptratio + 
                    b + lstat, BostonHousing)
summary(boston_model)$coefficients

predict_boston <- function(weight_vector, x, bias){
  res <- matrix()
  colnames(res) <- c("medv")
  for(i in 1:nrow(x)){
    res <- rbind(res, sum(weight_vector*x[i,]) + bias)
  }
  return(res)
}
boston_weight <- summary(boston_model)$coefficients[2:12,"Estimate"]
boston_x <- matrix(runif(6*11, 0.0, 1.0), nrow=6, ncol=11,byrow=T)
colnames(boston_x) <- c("crim", "zn", "chas", "nox", "rm", "dis", "rad", "tax", "ptratio", "b", "lstat")
boston_bias <- summary(boston_model)$coefficients[1,"Estimate"]
predict_boston(boston_weight, boston_x, boston_bias)
# exam02 end



predict_iris <- function(weight_vector, x, bias){
  res <- matrix()
  colnames(res) <- c("result")
  for(i in 1:nrow(x)){
    res<-rbind(res, sum(weight_vector*x[i,]) + bias)
  }
  return(res)
}
iris_model <- glm(as.integer(Species) ~ ., data=iris)
iris_weight <- iris_model$coefficients[2:5]
iris_x <- matrix(runif(4*2, 0.0, 1.0), nrow=2, ncol=4, byrow=T)
iris_bias <- iris_model$coefficients[1]
predict_iris(iris_weight, iris_x, iris_bias)



iris_x2 <- data.frame(rbind(c(runif(4*1, 0.0, 1.0))))
names(iris_x2) <- names(iris)[1:4]
iris_x2
predict(iris_model, iris_x2)
predict(iris_model, iris[,1:4])
round(predict(iris_model, iris[,1:4]))

iris[,5]
as.integer(iris[,5])
mean(c(1,1,1,1,0))
mean(round(predict(iris_model, iris[,1:4])) == as.integer(iris[,5]))



# exam03
ucla_data <- read.csv('ucla_admit.csv', header = TRUE, sep=',', encoding='utf-8')
ucla_data

ucla_model <- glm(admit ~., data=ucla_data)
ucla_model
predict(ucla_model, ucla_data)
mean(round(predict(ucla_model, ucla_data[,-1])) == ucla_data[,1])
