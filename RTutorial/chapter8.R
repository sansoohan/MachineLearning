library(C50)
library(gmodels)
credit <- read.csv('credit.csv', header = TRUE, sep=',', encoding='utf-8')
str(credit)

set.seed(12345)
order(runif(1000))
credit_rearange <- credit[order(runif(1000)),]
credit_train <- credit_rearange[1:900,]
credit_test <- credit_rearange[901:1000,]
credit_train
credit_test[,17]


credit_model <- C5.0(credit_train[-17], credit_test$default)
str(credit_model)

# low accuracy 76%
credit_pred <- predict(credit_model, credit_test)
credit_pred
mean(credit_pred == credit_test$default)

# high accuracy 98.9%
# make 10 models as random forest
trail_num = 10
credit_boost10 <- C5.0(credit_train[-17], credit_train$default, trials=trail_num)
str(credit_boost10)
summary(credit_boost10)
credit_boost10_pred <- predict(credit_boost10,credit_test)
str(credit_boost10_pred)

credit_boost10$boostResults[1,]
str(summary(credit_boost10))
mean(credit_boost10_pred==credit_test[,17])

str(credit_boost10)
credit_boost10$predictors

result <- stringi::stri_extract_all(summary(credit_boost10)$output, regex = "\\??[0-9.]+[0-9]%")[[1]][1+trail_num]
as.numeric(substr(result,1,nchar(result)-1))



credit_boost10$boostResults
credit_boost10


# show TFPN table
CrossTable(credit_test$default, credit_pred, prop.chisq=F, prop.c=F, prop.r=F, dnn=c('actual default', 'predicted default'))

# Redude FP in TFPN table(false positive)   !!!!!!!!!!!! You need to know how it works !!!!!!!!!!!!!!!!!!!!
error_cost <- matrix(c(0,1,4,0), nrow=2, ncol=2)
error_cost

credit_cost <- C5.0(credit_train[-17], credit_train$default, costs = error_cost)
credit_cost

credit_cost_pred <- predict(credit_cost, credit_test)
credit_cost_pred

CrossTable(credit_test$default, credit_cost_pred, prop.chisq=F, prop.c=F, prop.r=F, dnn=c('actual default', 'predicted default'))



# re-rullable decision tree

# 1. make rerullable tree
credit_rule <- C5.0(credit_train[-17], credit_train$default, rules=T)
credit_rule


# 2. find where you want to rerull
credit_rule$rules

# 3. modify rerullable tree and save
ruleText = credit_rule$rules
write(ruleText, file="ruleText.txt")

# 4. read rerulled tree
ruleText=paste(readLines("ruleText.txt"),collapse="\n")

# 5. insert rule
credit_rule$rules = ruleText
credit_rule$rules
summary(credit_rule)




library(randomForest)
data(iris)
set.seed(71)
ind <- sample(2, nrow(iris), replace = T, prob=c(0.8,0.2)) # random generate ratio : train 0.8, test 0.2
train_length = sum(ind==1)
test_length = sum(ind==2)

iris.rf <- randomForest(ncol(iris) ~ ., data=iris[ind==1,])
iris.rf
iris.pred <- predict(iris.rf, iris[ind==2,])
iris.pred

table(observed=iris[ind==2, "Species"],predicted=iris.pred)
acc <- mean(iris.pred==iris[c(ind==2), 5])
acc

# Exercise
library(C50)
library(randomForest)
mushrooms <- read.csv('mushrooms.csv', header = TRUE, sep=',', encoding='utf-8')

ind <- sample(2, nrow(iris), replace = T, prob=c(0.8,0.2)) # random generate ratio : train 0.8, test 0.2
train_length = sum(ind==1)
test_length = sum(ind==2)

mushrooms.rf <- randomForest(habitat ~ ., data=mushrooms[ind==1,])
mushrooms.rf
mushrooms.pred <- predict(mushrooms.rf, mushrooms[ind==2,])
mushrooms.pred

table(observed=mushrooms[ind==2, "habitat"],predicted=mushrooms.pred)
acc <- mean(mushrooms.pred==mushrooms[c(ind==2), ncol(mushrooms)])
acc

ncol(mushrooms)
ncol(mushrooms_mat)

mushrooms_mat <- c()
for(i in 1:length(mushrooms)){
  mushrooms_mat <- cbind(mushrooms_mat, factor(mushrooms[,i]))
}
mushrooms_mat
colnames(mushrooms_mat) <- colnames(mushrooms)

set.seed(12345)

mushrooms_train <- mushrooms_mat[c(mushroom_index==1),]
mushrooms_train_x <- mushrooms_train[,-c(ncol(mushrooms_mat))]
mushrooms_train_y <- factor(mushrooms_train[,ncol(mushrooms_mat)])

mushrooms_test <- mushrooms_mat[c(mushroom_index==2),]
mushrooms_test_x <- mushrooms_test[,-c(ncol(mushrooms_mat))]
mushrooms_test_y <- factor(mushrooms_test[,ncol(mushrooms_mat)])
trail_num = 10
mushrooms_boost10 <- C5.0(mushrooms_train_x, mushrooms_train_y, trials=trail_num)
summary(mushrooms_boost10)


result <- stringi::stri_extract_all(summary(mushrooms_boost10)$output, regex = "\\??[0-9.]+[0-9]%")[[1]][1+trail_num]
as.numeric(substr(result,1,nchar(result)-1))

library("class")
library("caret")
kfold_cross_validation <- function(data, y_col, K, fold_num){
  set.seed(12345)
  mushrooms_index <- sample(2, nrow(mushrooms),replace = T, prob=c(0.6,0.4))
  for(i in 1:fold_num){
    train_x <- data[-fld[[i]], -y_col]
    test_x <- data[fld[[i]], -y_col]
    train_y <- factor(data[-fld[[i]], y_col])
    test_y <- factor(data[fld[[i]], y_col])
    pred <- C5.0(train_x, train_y, trials=10)
    acc[i] <- mean(pred == test_y)
  }
  return(acc)
}
kfold_cross_validation(iris,5,3,5)


mushrooms_train

mushrooms_boost10

summary(mushrooms_boost10)

# SVM?— factorë¡? ë°”ê¿”?„œ ?„£?„ ?•„?š”ê°€ ?žˆ?‹¤.

set.seed(1)



