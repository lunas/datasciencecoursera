set.seed(3523)
library(AppliedPredictiveModeling)
data(concrete)
inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training = concrete[ inTrain,]
testing = concrete[-inTrain,]

library(e1071)
set.seed(325)
fit <- svm( CompressiveStrength ~ ., data=training)
pred.test <- predict(fit, testing)
confusionMatrix(testing$CompressiveStrength, pred.test)
