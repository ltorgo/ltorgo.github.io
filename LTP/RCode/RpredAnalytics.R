#############
##  Code of Slides:  Predictive Analytics in R
#############




####### Section:  Evaluation Metrics

##
trueVals <- c("c1","c1","c2","c1","c3","c1","c2","c3","c2","c3")
preds <- c("c1","c2","c1","c3","c3","c1","c1","c3","c1","c2")
confMatrix <- table(trueVals,preds)
confMatrix
errorRate <- 1-sum(diag(confMatrix))/sum(confMatrix)
errorRate

##
trueVals <- c("c1","c1","c2","c1","c3","c1","c2","c3","c2","c3")
preds <- c("c1","c2","c1","c3","c3","c1","c1","c3","c1","c2")
confMatrix <- table(trueVals,preds)
costMatrix <- matrix(c(10,-2,-4,-2,30,-3,-5,-6,12),ncol=3)
colnames(costMatrix) <- c("predC1","predC2","predC3")
rownames(costMatrix) <- c("obsC1","obsC2","obsC3")
costMatrix
utilityPreds <- sum(confMatrix*costMatrix)
utilityPreds

##
trueVals <- c(10.2,-3,5.4,3,-43,21,
              32.4,10.4,-65,23)
preds <-  c(13.1,-6,0.4,-1.3,-30,1.6,
            3.9,16.2,-6,20.4)
mse <- mean((trueVals-preds)^2)
mse
rmse <- sqrt(mse)
rmse
mae <- mean(abs(trueVals-preds))
mae
nmse <- sum((trueVals-preds)^2) / 
        sum((trueVals-mean(trueVals))^2)
nmse

##
nmae <- sum(abs(trueVals-preds)) / 
        sum(abs(trueVals-mean(trueVals)))
nmae
mape <- mean(abs(trueVals-preds)/trueVals)
mape
smape <- 1/length(preds) * sum(abs(preds - trueVals) /
                        (abs(preds)+abs(trueVals)))
smape
corr <- cor(trueVals,preds)
corr


####### Section:  Linear Discriminant

##
library(MASS)
data(iris)
lda(Species ~ ., iris)

##
sp <- sample(1:150,100)
tr <- iris[sp,]
ts <- iris[-sp,]
l <- lda(Species ~ ., tr)
preds <- predict(l,ts)
(mtrx <- table(preds$class,ts$Species))
(err <- 1-sum(diag(mtrx))/sum(mtrx))


####### Section:  Multiple Linear Regression

##
library(DMwR2)
data(algae)
algae <- algae[-c(62,199),]   # the 2 incomplete samples
clean.algae <- knnImputation(algae) # lm() does not handle NAs!
la1 <- lm(a1 ~ .,clean.algae[,1:12])
la1

##
summary(la1)

##
final.la1 <- step(la1)

##
summary(final.la1)

##
clean.test.algae <- knnImputation(test.algae, 
    k = 10, distData = clean.algae[, 1:11])
preds <- predict(final.la1,clean.test.algae)
mean((preds-algae.sols$a1)^2)

##
plot(algae.sols$a1,preds,main='Errors Scaterplot', 
     ylab='Predicted Values',xlab='True Values')
abline(0,1,col='red',lty=2)


####### Section:  Support Vector Machines (SVMs)

##
library(e1071)
data(Glass,package='mlbench')
tr <- Glass[1:200,]
ts <- Glass[201:214,]
s <- svm(Type ~ .,tr)
predict(s,ts)

##
ps <- predict(s,ts)
table(ps,ts$Type)
mc <- table(ps,ts$Type)
error <- 100*(1-sum(diag(mc))/sum(mc))
error

##
library(e1071)
data(Boston,package='MASS')
set.seed(1234)
sp <- sample(1:nrow(Boston),354)
tr <- Boston[sp,]
ts <- Boston[-sp,]
s <- svm(medv ~ .,tr,cost=10,epsilon=0.02)
preds <- predict(s,ts)
mean((ts$medv-preds)^2)

##
plot(ts$medv,preds,main='Errors Scaterplot',
      ylab='Predictions',xlab='True')
abline(0,1,col='red',lty=2)


####### Section:  Tree-based Models

##
library(DMwR2)
library(rpart.plot)
data(Glass,package='mlbench')
ac <- rpartXse(Type  ~ .,Glass)
prp(ac,type=4,extra=101)

##
tr <- Glass[1:200,]
ts <- Glass[201:214,]
ac <- rpartXse(Type  ~ .,tr)
predict(ac,ts)

##
predict(ac,ts,type='class')
ps <- predict(ac,ts,type='class')
table(ps,ts$Type)
mc <- table(ps,ts$Type)
err <- 100*(1-sum(diag(mc))/sum(mc))
err

##
library(DMwR2)
library(rpart.plot)
load('carInsurance.Rdata')
d <- ins[,-1]
ar <- rpartXse(normLoss ~ .,d)
prp(ar,type=4,extra=101)

##
library(DMwR2)
library(rpart.plot)
data(algae)
d <- algae[,1:12]
ar <- rpartXse(a1 ~ .,d)

##
prp(ar,type=4,extra=101)

##
tr <- d[1:150,]
ts <- d[151:205,]
arv <- rpartXse(normLoss ~ .,tr)
preds <- predict(arv,ts)
mae <- mean(abs(preds-ts$normLoss),na.rm=T)
mae
mape <- mean(abs(preds-ts$normLoss)/ts$normLoss,na.rm=T)
mape

##
tr <- d[1:150,]
ts <- d[151:200,]
ar <- rpartXse(a1 ~ .,tr)
preds <- predict(ar,ts)
mae <- mean(abs(preds-ts$a1))
mae
cr <- cor(preds,ts$a1)
cr


####### Section:  Model Ensembles


####### Section:  Model Ensembles and Random Forests

##
simpleBagging <- function(form,data,model='rpartXse',nModels=100,...) {
    ms <- list()
    n <- nrow(data)
    for(i in 1:nModels) {
        tr <- sample(n,n,replace=T)
        ms[[i]] <- do.call(model,c(list(form,data[tr,]),...))
    }
    ms
}

predict.simpleBagging <- function(models,test) {
   ps <- sapply(models,function(m) predict(m,test))
   apply(ps,1,mean)
}

##
data(Boston,package='MASS')
library(DMwR)
set.seed(123)
trPerc <- 0.7
sp <- sample(1:nrow(Boston),as.integer(trPerc*nrow(Boston)))
tr <- Boston[sp,]
ts <- Boston[-sp,]

m <- simpleBagging(medv ~ .,tr,nModels=300,se=0.5)
ps <- predict.simpleBagging(m,ts)
mean(abs(ps-ts$medv))

##
library(ipred)
data(Boston,package='MASS')
set.seed(123)
trPerc <- 0.7
sp <- sample(1:nrow(Boston),as.integer(trPerc*nrow(Boston)))
tr <- Boston[sp,]
ts <- Boston[-sp,]

m <- bagging(medv ~ .,tr,nbagg=100)
ps <- predict(m,ts)
mean(abs(ps-ts$medv))

##
randPreds <- function(tgtName,data,model='rpartXse',
                       nVars=(ncol(data)-1)%/%2,nModels=20,...) {
 
   np <- ncol(data)-1
   if (np <= nVars)
     stop(paste("Nro de colunas nos dados insuficiente para escolher",
                nVar,"variáveis"))
   
   tgtCol <- which(colnames(data) == tgtName)
   preds <- (1:ncol(data))[-tgtCol]
 
   ms <- list()
   for(i in 1:nModels) {
     cols <- sample(preds,nVars)
     form <- as.formula(paste(paste(names(data)[tgtCol],'~'),
                              paste(names(data)[cols],collapse='+')))
     ms[[i]] <- do.call(model,c(list(form,data),...))
   }
   ms
}
 
predict.randPreds <- function(models,test) {
   ps <- sapply(models,function(m) predict(m,test))
   apply(ps,1,mean)
}

##
data(Boston,package='MASS')
library(DMwR)
set.seed(123)
trPerc <- 0.7
sp <- sample(1:nrow(Boston),as.integer(trPerc*nrow(Boston)))
tr <- Boston[sp,]
ts <- Boston[-sp,]

m <- randPreds("medv",tr,nModels=300,se=0.5)
ps <- predict.randPreds(m,ts)
mean(abs(ps-ts$medv))

##
library(randomForest)
data(Boston,package="MASS")
samp <- sample(1:nrow(Boston),354)
tr <- Boston[samp,]
ts <- Boston[-samp,]
m <- randomForest(medv ~ ., tr)
ps <- predict(m,ts)
mean(abs(ts$medv-ps))

##
data(Glass,package='mlbench')
set.seed(1234)
sp <- sample(1:nrow(Glass),150)
tr <- Glass[sp,]
ts <- Glass[-sp,]
m <- randomForest(Type ~ ., tr,ntree=3000)
ps <- predict(m,ts)
table(ps,ts$Type)
mc <- table(ps,ts$Type)
err <- 100*(1-sum(diag(mc))/sum(mc))
err

##
data(Boston,package='MASS')
library(randomForest)
m <- randomForest(medv ~ ., Boston, 
                  importance=T)
importance(m)

##
varImpPlot(m,main="Feature Relevance Scores")


####### Section:  Hands on Random Forests

##
library(randomForest)
library(DMwR)
data(algae)
algae <- algae[-c(62,199),]
algae <- knnImputation(algae)
rf.a4 <- randomForest(a4 ~.,algae[,c(1:11,15)])

##
lm.a4 <- lm(a4 ~ .,algae[,c(1:11,15)])

##
lm.a4 <- step(lm.a4)

##
lm.a4

##
psrf <- predict(rf.a4,algae)
pslm <- predict(lm.a4,algae)
plot(psrf,pslm,xlab="Random forest predictions",ylab="lm predictions")
abline(0,1,col="green")

##
data(testAlgae)
prevs.rf <- predict(rf.a4,test.algae)
prevs.lm <- predict(lm.a4,test.algae)
summary(prevs.rf)
summary(prevs.lm) # notice the difference in the number of NA's. Why?

##
test.algae <- knnImputation(test.algae,distData=algae[,1:11])
prevs.rf <- predict(rf.a4,test.algae)
prevs.lm <- predict(lm.a4,test.algae)

##
library(adabag)
data(iris)
set.seed(1234)
trPerc <- 0.7
sp <- sample(1:nrow(iris),as.integer(trPerc*nrow(iris)))
 
tr <- iris[sp,]
ts <- iris[-sp,]
 
m <- boosting(Species ~ ., tr)
ps <- predict(m,ts)
 
ps$confusion
ps$error

##
library(adabag)
data(iris)
set.seed(1234)
trPerc <- 0.7
sp <- sample(1:nrow(iris),as.integer(trPerc*nrow(iris)))
 
tr <- iris[sp,]
ts <- iris[-sp,]
 
m <- bagging(Species ~ ., tr,mfinal=50)
ps <- predict(m,ts)
 
ps$confusion
ps$error

##
library(adabag)
data(BreastCancer,package="mlbench")
set.seed(1234)
trPerc <- 0.7
sp <- sample(1:nrow(BreastCancer),as.integer(trPerc*nrow(BreastCancer)))
 
tr <- BreastCancer[sp,-1]
ts <- BreastCancer[-sp,-1]
 
m <- bagging(Class ~ ., tr,mfinal=100)
ps <- predict(m,ts)
 
ptr <- errorevol(m,tr)
pts <- errorevol(m,ts)

##
plot(ptr$error,type="l",xlab="nr.models",ylab="error",ylim=c(0,0.1)) 
lines(pts$error,col="red")

##
library(gbm)
data(Boston,package='MASS')
 
set.seed(1234)
trPerc <- 0.7
sp <- sample(1:nrow(Boston),as.integer(trPerc*nrow(Boston)))
tr <- Boston[sp,]
ts <- Boston[-sp,]
 
m <- gbm(medv ~ .,distribution='gaussian',data=tr,
         n.trees=20000,verbose=F)
ps <- predict(m,ts,type='response',n.trees=20000)
mean(abs(ps-ts$medv))


####### Section:  Hands on Boosting

##
library(randomForest)
library(DMwR)
data(algae)
algae <- algae[-c(62,199),]
algae <- knnImputation(algae)
rf.a4 <- randomForest(a4 ~.,algae[,c(1:11,15)])

##
lm.a4 <- lm(a4 ~ .,algae[,c(1:11,15)])

##
lm.a4 <- step(lm.a4)

##
lm.a4

##
psrf <- predict(rf.a4,algae)
pslm <- predict(lm.a4,algae)
plot(psrf,pslm,xlab="Random forest predictions",ylab="lm predictions")
abline(0,1,col="green")

##
data(testAlgae)
prevs.rf <- predict(rf.a4,test.algae)
prevs.lm <- predict(lm.a4,test.algae)
summary(prevs.rf)
summary(prevs.lm) # notice the difference in the number of NA's. Why?

##
test.algae <- knnImputation(test.algae,distData=algae[,1:11])
prevs.rf <- predict(rf.a4,test.algae)
prevs.lm <- predict(lm.a4,test.algae)

##
library(UBL) # Loading our infra-structure
library(e1071) # package containing the svm we will use
data(ImbC) # The synthetic data set we are going to use
summary(ImbC) # Summary of the ImbC data
table(ImbC$Class)

##
set.seed(123)
samp <- sample(1:nrow(ImbC), nrow(ImbC)*0.7)
train <- ImbC[samp,]
test <- ImbC[-samp,]
model <- svm(Class~., train)
preds <- predict(model,test)
table(preds, test$Class) # confusion matrix

##
# not using the default distance (Eucledian) because of the nominal feature
newtrain <- SmoteClassif(Class~., train, C.perc="balance", dist="HEOM")
# generate a new model with the changed data
newmodel <- svm(Class~., newtrain)
preds <- predict(newmodel,test)
table(preds, test$Class)

##
newtrain2 <- RandOverClassif(Class~., train, C.perc="balance")
#generate a new model with the modified data set
newmodel2 <- svm(Class~., newtrain2)
preds <- predict(newmodel2, test)
table(preds, test$Class)

##
data(ImbR)
summary(ImbR)
set.seed(123)
samp <- sample(1:nrow(ImbR), as.integer(0.7*nrow(ImbR)))
trainD <- ImbR[samp,]
testD <- ImbR[-samp,]

##
library(randomForest)
model <- randomForest(Tgt~., trainD)
preds <- predict(model, testD)

##
# using the Introduction of Gaussian Noise with the default parameters
newTrain <- GaussNoiseRegress(Tgt~., trainD)
newModel <-randomForest(Tgt~., newTrain)
newPreds <- predict(newModel, testD)

##
res <- data.frame(true=rep(testD$Tgt,2),preds=c(preds,newPreds),
                  model=c(rep("original",nrow(testD)),rep("resampled",nrow(testD))))
library(ggplot2)
ggplot(res, aes(x=true,y=preds,color=model)) + geom_point() + geom_abline(slope=1,intercept=0)
