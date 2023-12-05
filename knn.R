library(class)
library(caret)

#change path to wherever train.csv is located
data <- read.csv("C:/Users/Joseph/Downloads/school/Fall/MATH 4323/Project/train.csv")

data$eyeDetection <- as.factor(data$eyeDetection)
n <- nrow(data)
set.seed(1)
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
ctrl <- trainControl(method="cv", number=10)
fit.knn <- train(eyeDetection ~ . , data = training, method = "knn", trControl = ctrl, 
              tuneGrid= expand.grid(k=seq(3,85, by =2)), preProcess = c("center", "scale")
              , metric = "Accuracy")

#best result was when k=3


knn.pred = knn(X.train, X.test, y.train, k=3)

# Run KNN on scaled data
scaled_X_train <- scale(X.train)
scaled_X_test <- scale(X.test, center = attr(scaled_X_train, "scaled:center"), 
                       scale = attr(scaled_X_train, "scaled:scale"))

knn.pred <- knn(scaled_X_train, scaled_X_test, y.train, k = 3)

#confusion matrix
print(table(knn.pred, y.test))
#error rate
err <- mean(knn.pred != y.test)
print(err)

#correct prediction rate
print(1-err)

#creating plot for analysis portion of KNN
error_rates <- numeric()

# Loop through k values from 1 to 12 of unscaled data
for (k in 1:12) {
  knn.pred <- knn(X.train, X.test, y.train, k = k)
  
  # Calculate error rate for each k value
  error <- mean(knn.pred != y.test)
  error_rates <- c(error_rates, error)
}

# Plotting error rates for different k values
plot(1:12, error_rates, type = 'b', xlab = 'k values', ylab = 'Error Rate', 
     main = 'Unscaled Error Rates for Different k values in KNN')

#need to reset the error_rates for 2nd plot to work
error_rates <- numeric()

#Loop through k values from 1 to 12 of scaled data
for (k in 1:12) {
  scaled_X_train <- scale(X.train)
  scaled_X_test <- scale(X.test, center = attr(scaled_X_train, "scaled:center"), 
                         scale = attr(scaled_X_train, "scaled:scale"))
  
  knn.pred <- knn(scaled_X_train, scaled_X_test, y.train, k = k)
  
  # Calculate error rate for each k value
  error <- mean(knn.pred != y.test)
  error_rates <- c(error_rates, error)
}

# Plotting error rates for different k values
plot(1:12, error_rates, type = 'b', xlab = 'k values', ylab = 'Error Rate', 
     main = 'Scaled Error Rates for Different k values in KNN')


# SVM models & PCA analysis

X <- data[,-15]
y <- data[,15]

pca_model <- prcomp(X, scale=TRUE)

summary(pca_model)

#first 5 PCAs account for 96% of the variance, we will use these to train a model

#creating SVM models, using PCA

#new PCA dataset - hyperparameter tuning

library(e1071)

PCA_X <- pca_model$x[,1:5]
PCA_X <- data.frame(PCA_X, eyeDetection = data$eyeDetection)

#80% training, 20% testing
training.PCA <- PCA_X[train,]
testing.PCA <- PCA_X[-train,]

PCA.SVm.tune <- tune(svm, eyeDetection ~ ., data=training.PCA,
                     ranges = list(kernel = c("linear", "radial", "polynomial", "sigmoid"),
                     cost = 10^(-1:2))
                    )

summary(PCA.SVm.tune)



ctrl <- trainControl(method="cv", number=10)
fit.knn <- train(eyeDetection ~ . , data = training.PCA, method = "knn", trControl = ctrl, 
                 tuneGrid= expand.grid(k=seq(3,85, by =2)), preProcess = c("center", "scale")
                 , metric = "Accuracy")

fit.knn








