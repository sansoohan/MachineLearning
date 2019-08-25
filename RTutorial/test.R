iris <- read.csv('iris_A.csv', header = TRUE, sep=',', encoding='utf-8')
library(mlbench)
library(e1071)
library(caret)
library(Species)
library(randomForest)
train_algo <- function(data, method, y_col, hyper_parameter, fold_num){
  fld <- createFolds(data[, y_col], k=fold_num)
  acc <- c()
  max_acc = 0;
  ensemble_test_y <- c()
  ret_model = NULL;
  model
  for(i in 1:fold_num){
    train_x <- data[-fld[[i]], -y_col]
    test_x <- data[fld[[i]], -y_col]
    train_y <- factor(data[-fld[[i]], y_col])
    test_y <- factor(data[fld[[i]], y_col])
    ensemble_test_y <- cbind(ensemble_test_y, test_y)
    pred <- c()
    # Logistic regression 
    if(method == 1){
      model <- glm(Species ~., data=data[-fld[[i]],], family = hyper_parameter)
      mean(round(predict(model, data[-fld[[i]],])) == data[-fld[[i]], y_col])
    }
    # KNN
    else if(method==2){
      pred <- knn(train_x, test_x, train_y, k=hyper_parameter)
      acc[i] <- mean(pred == test_y)
    }
    # C5.0
    else if(method==3){
      model = C5.0(train_x, train_y, trials=hyper_parameter)
      pred <- predict(model, test_x)
      acc[i] <- mean(pred == test_y)
    }
    # randomForest
    else if(method==4){
      model <- randomForest(Species ~ ., data=data[-fld[[i]],])
      pred <- predict(model, data[fld[[i]],])
      acc[i] <- mean(pred == data[fld[[i]],y_col])
    }
    # SVM
    else if(method==5){
      model <- svm(train_x, train_y, kernel=hyper_parameter)
      pred <- predict(model, test_x)
      acc[i] <- mean(pred == test_y)
    }
  }
  if(max_acc < acc[i]){
    max_acc <- acc[i]
    ret_model <- model
  }
  print(acc)
  print(paste('average :',mean(acc)))
  return(ret_model)
}


write.table(state_date, paste('result',i,'.csv', sep=''),row.names=FALSE,sep="\n")

# Logistic regression
#Family	Default Link Function
#binomial	(link = "logit")
#gaussian	(link = "identity")
#Gamma	(link = "inverse")
#inverse.gaussian	(link = "1/mu^2")
#poisson	(link = "log")
#quasi	(link = "identity", variance = "constant")
#quasibinomial	(link = "logit")
#quasipoisson	(link = "log"

# Generalized Linear model
model <- train_algo(iris, method=1 ,y_col=ncol(iris), hyper_parameter="binomial", fold_num=5)
# KNN 에서는 루트n값이 이상적 
model <- train_algo(iris, method=2 ,y_col=ncol(iris), hyper_parameter=12, fold_num=5)
# C 5.0
model <- train_algo(iris, method=3 ,y_col=ncol(iris), hyper_parameter=25, fold_num=5)
# randomForest
model <- train_algo(iris, method=4 ,y_col=ncol(iris), hyper_parameter=12, fold_num=5)
# SVM
model <- train_algo(iris, method=5 ,y_col=ncol(iris), hyper_parameter="linear", fold_num=5)
model <- train_algo(iris, method=5 ,y_col=ncol(iris), hyper_parameter="polynomial", fold_num=5)
model <- train_algo(iris, method=5 ,y_col=ncol(iris), hyper_parameter="radial", fold_num=5)
model <- train_algo(iris, method=5 ,y_col=ncol(iris), hyper_parameter="sigmoid", fold_num=5)


predict_data <- read.csv('iris_B.csv', header = TRUE, sep=',', encoding='utf-8')

predict_model<-function(model, data){
  write.table(predict(model,data),'32091357_한산수.csv',col.names=F, row.names=FALSE,sep=",")
}
predict_model(model, iris[,-5])
