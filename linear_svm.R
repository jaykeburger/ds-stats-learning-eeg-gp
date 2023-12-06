library(e1071)
eyeData <- read.csv("train.csv")
eyeData[,"eyeDetection"] <- as.factor(eyeData[,"eyeDetection"])

# Printing function
printPlots <- function(svmBestModel, data){
  # Plots with AF3
  plot(svmBestModel, data, AF3 ~ F7)
  plot(svmBestModel, data, AF3 ~ F3)
  plot(svmBestModel, data, AF3 ~ FC5)
  plot(svmBestModel, data, AF3 ~ T7)
  plot(svmBestModel, data, AF3 ~ P7)
  plot(svmBestModel, data, AF3 ~ O1)
  plot(svmBestModel, data, AF3 ~ O2)
  plot(svmBestModel, data, AF3 ~ P8)
  plot(svmBestModel, data, AF3 ~ T8)
  plot(svmBestModel, data, AF3 ~ FC6)
  plot(svmBestModel, data, AF3 ~ F4)
  plot(svmBestModel, data, AF3 ~ F8)
  plot(svmBestModel, data, AF3 ~ AF4)
  
  # Plots with F7
  plot(svmBestModel, data, F7 ~ F3)
  plot(svmBestModel, data, F7 ~ FC5)
  plot(svmBestModel, data, F7 ~ T7)
  plot(svmBestModel, data, F7 ~ P7)
  plot(svmBestModel, data, F7 ~ O1)
  plot(svmBestModel, data, F7 ~ O2)
  plot(svmBestModel, data, F7 ~ P8)
  plot(svmBestModel, data, F7 ~ T8)
  plot(svmBestModel, data, F7 ~ FC6)
  plot(svmBestModel, data, F7 ~ F4)
  plot(svmBestModel, data, F7 ~ F8)
  plot(svmBestModel, data, F7 ~ AF4)
  
  # Plots with F3
  plot(svmBestModel, data, F3 ~ FC5)
  plot(svmBestModel, data, F3 ~ T7)
  plot(svmBestModel, data, F3 ~ P7)
  plot(svmBestModel, data, F3 ~ O1)
  plot(svmBestModel, data, F3 ~ O2)
  plot(svmBestModel, data, F3 ~ P8)
  plot(svmBestModel, data, F3 ~ T8)
  plot(svmBestModel, data, F3 ~ FC6)
  plot(svmBestModel, data, F3 ~ F4)
  plot(svmBestModel, data, F3 ~ F8)
  plot(svmBestModel, data, F3 ~ AF4)
  
  # Plots with FC5
  plot(svmBestModel, data, FC5 ~ T7)
  plot(svmBestModel, data, FC5 ~ P7)
  plot(svmBestModel, data, FC5 ~ O1)
  plot(svmBestModel, data, FC5 ~ O2)
  plot(svmBestModel, data, FC5 ~ P8)
  plot(svmBestModel, data, FC5 ~ T8)
  plot(svmBestModel, data, FC5 ~ FC6)
  plot(svmBestModel, data, FC5 ~ F4)
  plot(svmBestModel, data, FC5 ~ F8)
  plot(svmBestModel, data, FC5 ~ AF4)
  
  # Plots with T7
  plot(svmBestModel, data, T7 ~ P7)
  plot(svmBestModel, data, T7 ~ O1)
  plot(svmBestModel, data, T7 ~ O2)
  plot(svmBestModel, data, T7 ~ P8)
  plot(svmBestModel, data, T7 ~ T8)
  plot(svmBestModel, data, T7 ~ FC6)
  plot(svmBestModel, data, T7 ~ F4)
  plot(svmBestModel, data, T7 ~ F8)
  plot(svmBestModel, data, T7 ~ AF4)
  
  # Plots with P7
  plot(svmBestModel, data, P7 ~ O1)
  plot(svmBestModel, data, P7 ~ O2)
  plot(svmBestModel, data, P7 ~ P8)
  plot(svmBestModel, data, P7 ~ T8)
  plot(svmBestModel, data, P7 ~ FC6)
  plot(svmBestModel, data, P7 ~ F4)
  plot(svmBestModel, data, P7 ~ F8)
  plot(svmBestModel, data, P7 ~ AF4)
  
  # Plots with O1
  plot(svmBestModel, data, O1 ~ O2)
  plot(svmBestModel, data, O1 ~ P8)
  plot(svmBestModel, data, O1 ~ T8)
  plot(svmBestModel, data, O1 ~ FC6)
  plot(svmBestModel, data, O1 ~ F4)
  plot(svmBestModel, data, O1 ~ F8)
  plot(svmBestModel, data, O1 ~ AF4)
  
  # Plots with O2
  plot(svmBestModel, data, O2 ~ P8)
  plot(svmBestModel, data, O2 ~ T8)
  plot(svmBestModel, data, O2 ~ FC6)
  plot(svmBestModel, data, O2 ~ F4)
  plot(svmBestModel, data, O2 ~ F8)
  plot(svmBestModel, data, O2 ~ AF4)
  
  # Plots with P8
  plot(svmBestModel, data, P8 ~ T8)
  plot(svmBestModel, data, P8 ~ FC6)
  plot(svmBestModel, data, P8 ~ F4)
  plot(svmBestModel, data, P8 ~ F8)
  plot(svmBestModel, data, P8 ~ AF4)
  
  # Plots with T8
  plot(svmBestModel, data, T8 ~ FC6)
  plot(svmBestModel, data, T8 ~ F4)
  plot(svmBestModel, data, T8 ~ F8)
  plot(svmBestModel, data, T8 ~ AF4)
  
  # Plots with FC6
  plot(svmBestModel, data, FC6 ~ F4)
  plot(svmBestModel, data, FC6 ~ F8)
  plot(svmBestModel, data, FC6 ~ AF4)
  
  # Plots with F4
  plot(svmBestModel, data, F4 ~ F8)
  plot(svmBestModel, data, F4 ~ AF4)
  
  # Plots with F8
  plot(svmBestModel, data, F8 ~ AF4)
}

# Sampling
n <- nrow(eyeData)
train <- sample(1:n, 0.8*n)

train_set <- eyeData[train,]
test_set <- eyeData[-train,]

## Tuning WITHOUT scaling
set.seed(1)
svm.tune <- tune(METHOD = svm,
                 eyeDetection~.,
                 data = train_set,
                 type = "C-classification",
                 kernel = "linear",
                 ranges = list(cost = c(0.01, 0.1, 1, 5, 10, 100, 1000)))
svm.tune
summary(svm.tune)
plot(svm.tune$performances$error, type = "b", ylab = "Error Rate", main = "Error Rates of Scaled Tune Models")

# Error rates

train.yPred <- predict(svm.tune$best.model, train_set)  # training error rate
train.errorrate <- mean(train.yPred != train_set[,"eyeDetection"])
print(train.errorrate)

test.yPred <- predict(svm.tune$best.model, test_set)  # test error rate
test.errorrate <- mean(test.yPred != test_set[,"eyeDetection"])
print(test.errorrate)

# Confusion Matrix for train_set
eyeDetection_pred_train <- predict(svm.tune$best.model, truth = train_set$eyeDetection)
table(predict = eyeDetection_pred_train,
      truth = train_set$eyeDetection)

#Printing the plots
printPlots(svmBestModel = svm.tune$best.model, data = train_set)

#################################################################################################

## Tuning WITH scaling
train_set.scaled <- scale(train_set[,-15])
test_set.scaled <- data.frame(scale(test_set[,-15],
                                    center = attr(train_set.scaled, "scaled:center"),
                                    scale = attr(train_set.scaled , "scaled:scale")))

# Turning scaled train data into dataFrame
train_set.scaled <- data.frame(train_set.scaled)
train_set.scaled[,"eyeDetection"] <- train_set[,"eyeDetection"]

set.seed(1)
svm.tune.scale <- tune(METHOD = svm,
                       eyeDetection~.,
                       data = train_set.scaled,
                       type = "C-classification",
                       kernel = "linear",
                       ranges = list(cost = c(0.01, 0.1, 1, 5, 10, 100, 1000)))
svm.tune.scale
summary(svm.tune.scale)
plot(svm.tune.scale$performances$error, type = "b", ylab = "Error Rate", main = "Error Rates of Unscaled Tune Models")

# Error rates (Scaled)
train.yPred.scaled <- predict(svm.tune.scale$best.model, train_set.scaled)  # training error rate
train.errorrate.scaled <- mean(train.yPred.scaled != train_set.scaled[,"eyeDetection"])
print(train.errorrate.scaled)

test.yPred.scaled <- predict(svm.tune.scale$best.model, test_set.scaled)  # test error rate
test.errorrate.scaled <- mean(test.yPred.scaled != test_set.scaled[,"eyeDetection"])
print(test.errorrate.scaled)

# Confusion Matrix for train_set.scaled
eyeDetection_pred_train.scaled <- predict(svm.tune.scale$best.model, truth = train_set.scaled$eyeDetection)
table(predict = eyeDetection_pred_train.scaled,
      truth = train_set.scaled$eyeDetection)

# Printing the scaled plots
printPlots(svmBestModel = svm.tune.scale$best.model, data = train_set.scaled)