install.packages("caret")
library(caret)

data <- read.csv("C:/Users/Joseph/Downloads/school/Fall/MATH 4323/Project/train.csv")

data$eyeDetection <- as.factor(data$eyeDetection)
n <- nrow(data)
#train set
train <- sample(1:n, 0.8*n)
training <- data[train,]
#test set
testing <- data[-train,]

X.train <- training[,-15]
y.train <- training[,15]

X.test <- testing[,-15]
y.test <- testing[,15]

#hyper parameter tuning for KNN model using caret library

set.seed(1)
ctrl <- trainControl(method="cv", number=10)
fit.knn <- train(eyeDetection ~ . , data = training, method = "knn", trControl = ctrl, 
              tuneGrid= expand.grid(k=seq(3,85, by =2)), preProcess = c("center", "scale")
              , metric = "Accuracy")

fit.knn
#best result was when k=3

knn.pred = knn(X.train, X.test, y.train, k=3)

#confusion matrix
print(table(knn.pred, y.test))
#error rate
err <- mean(knn.pred != y.test)
print(err)

#correct prediction rate
print(1-err)
