# Prepare
library(caret)
library(gbm)

set.seed(3433)
library(AppliedPredictiveModeling)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

set.seed(62433)
fit.raf <- train( diagnosis ~ ., method="rf", prox=F, data=training)
fit.gbm <- train( diagnosis ~ ., method="gbm", verbose=F, data=training)
fit.lda <- train( diagnosis ~ ., method="lda", verbose=F, data=training)

# Measure accuracy of prediction on test set
pred.test.raf <- predict(fit.raf, testing)
pred.test.gbm <- predict(fit.gbm, testing)
pred.test.lda <- predict(fit.lda, testing)

result.raf <- confusionMatrix(testing$diagnosis, pred.test.raf)
result.gbm <- confusionMatrix(testing$diagnosis, pred.test.gbm)
result.lda <- confusionMatrix(testing$diagnosis, pred.test.lda)


### Train a combined model, on training set!

#first create predictions on training set:
pred.train.raf <- predict(fit.raf, training)
pred.train.gbm <- predict(fit.gbm, training)
pred.train.lda <- predict(fit.lda, training)

# create a combined data frame from these predictions:
combi.train <- data.frame(pred.raf=pred.train.raf, pred.gbm=pred.train.gbm, pred.lda=pred.train.lda, diagnosis=training$diagnosis)
fit.combi <- train( diagnosis ~ ., method="rf", prox=F, data=combi.train)

### Predict on testing set

# first create combined data set from the predictions on the testing dataset
combi.test <- data.frame(pred.raf=pred.test.raf, pred.gbm=pred.test.gbm, pred.lda=pred.test.lda, diagnosis=testing$diagnosis)
# then predict
pred.combi <- predict( fit.combi, combi.test )
result.combi <- confusionMatrix(testing$diagnosis, pred.combi)

result.raf$overall[1]
result.gbm$overall[1]
result.lda$overall[1]
result.combi$overall[1]
