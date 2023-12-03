

train$eyeDetection <- as.factor(train$eyeDetection)
set.seed(4323)
library(e1071)
n = nrow(train)
t.train = sample(1:n,.8*n)
t.train



nums = 1:n
subsset.size = 2000
set.seed(1)
sub = sample(nums,subsset.size)
eye.sub = train[sub,]

train.sample = sample(1:subsset.size, .8 * subsset.size)
eye.train.sub = eye.sub[train.sample,]
eye.test.sub = eye.sub[-train.sample,-15]
eye.test.sub.y = eye.sub[-train.sample,15]


#PCA analysis for graphing?
eye.PCA.out.scale = prcomp(train[,-15], scale = T)
eye.PCA.out.scale
summary(eye.PCA.out.scale)
#first 3 PC explain 87.77% proportional variance



##no scale first##
#subset#
#checking code
library(e1071)
set.seed(4323)
tune.eye.outR.notS =tune(svm,
                 eyeDetection~., data=eye.train.sub,
                 kernel="radial",
                 ranges=list(cost=c(0.1,1,10,100,1000),
                             gamma=c(0.5,1,2,3,4)))
tune.eye.outR.notS
summary(tune.eye.outR.notS)

##scaled this time##
train.scale = scale(eye.train.sub[,-15])
train.scale.y =eye.train.sub$eyeDetection
test.scale = scale(eye.test.sub[,-15],
                   center = attr(train.scale, "scaled:center"),
                   scale = attr(train.scale, "scaled:scale"))
eye.test.sub.y
?tune
library(e1071)
set.seed(4323)
tune.eye.outR.S =tune(svm,train.scale, train.y = train.scale.y, 
                      kernel="radial",
                      ranges=list(cost=c(0.1,1,10,100),
                                  gamma=c(0.5,1,2,3)))
tune.eye.outR.S


#compare scaled and not scaled
tune.eye.outR.notS
tune.eye.outR.S

#no difference between scaled and unscaled data by error rate

#unscaled confusion matric
predict.sub.notS = predict(tune.eye.outR.notS$best.model, newdata = eye.test.sub)
predict.sub.notS
table(true = eye.test.sub.y, pred = predict.sub.notS)

#scaled confusion matrix
#reminder on variables
#eye.test.sub = eye.sub[-train.sample,-15]
#eye.test.sub.y = eye.sub[-train.sample,15]
predict.sub.scale = predict(tune.eye.outR.S$best.model, newdata = test.scale)
predict.sub.scale
table(true = eye.test.sub.y, pred = predict.sub.scale)


#side by side confusion matrixes
#not scaled
table(true = eye.test.sub.y, pred = predict.sub.notS)
#scaled
table(true = eye.test.sub.y, pred = predict.sub.scale)

#both confusion matrixes have the same result






#all data#
#got an error originally when cost 1000 and polynomial 4 included
#have not been able to run it yet 
library(e1071)
set.seed(4323)
tune.eye.outR.notS =tune(svm,
                         eyeDetection~., data=train[t.train,],
                         kernel="radial",
                         ranges=list(cost=c(.1,1,10,100),
                                     gamma=c(0.5,1,2,3)))
tune.eye.outR.notS
summary(tune.eye.outR.notS)


eye.rad.svm.notS = svm(eyeDetection~., data=train[t.train,],kernel="radial",cost=10,gamma = 1)
summary(eye.rad.svm.notS)
#plot(tune.eye.outR.notS$best.model,dat[t.train,])


