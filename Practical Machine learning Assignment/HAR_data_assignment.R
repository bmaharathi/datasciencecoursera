library(caret)

training <- read.csv("pml-training.csv", header = TRUE, na.strings = c("NA",""))
testing <- read.csv("pml-testing.csv", header = TRUE,na.strings = c("NA",""))



nacols <- colSums(is.na(training))
#remove the colums with NA values in all rows 
train2 <- training[,colSums(is.na(training)) ==0]
test2 <- testing[,colSums(is.na(training)) ==0]


#remove unnecessary columns like x, name and timestamp.
train3 <- train2[, -grep("X|user_name|timestamp|new_window", colnames(train2))]
test3 <- test2[, -grep("X|user_name|timestamp|new_window", colnames(train2))]

#create data partition to train and test 
traind <- createDataPartition(train3$classe,p=0.7,list=FALSE)
trainf <- train3[traind,]
testf <- train3[-traind,]

dim(trainf); dim(testf)
#create the model fit  RF
control_rf <- trainControl(method="oob",number=10,repeats=5,p=0.80)
fit_rf <- train(classe~.,data=trainf,trControl=control_rf)
#plot final model for error curve 
plot(fit_rf$finalModel)

predict_trainf <- predict(fit_rf,trainf)

confusionMatrix(predict_trainf,trainf$classe)


predict_testf <- predict(fit_rf,testf)
confusionMatrix(predict_testf,testf$classe)


predict20 <-predict(fit_rf, test3)
test_20_predict <- data.frame(predict20)


# cross validate with GBM
control_gbm <- trainControl(method="cv",number=3, allowParallel=TRUE, verbose=TRUE)
fit_gbm <- train(classe~., data=trainf, method="gbm",trControl=control_gbm, verbose=FALSE)
fit_gbm
plot(fit_gbm$finalModel)


predict_gbm_trainf <- predict(fit_gbm,trainf)
confusionMatrix(predict_gbm_trainf, trainf$classe)

predict_gbm_test<- predict(fit_gbm, testf)
confusionMatrix(predict_gbm_test, testf$classe)

confusionMatrix(predict_testf,predict_gbm_test)
