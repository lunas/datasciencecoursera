library(lubridate)  # For year() function below
setwd("~/Documents/ml/datascience/courses/08_PracticalMachineLearning/quizzes/")
dat = read.csv("gaData.csv")
training = dat[year(dat$date) < 2012,]
testing = dat[(year(dat$date)) > 2011,]
tstrain = ts(training$visitsTumblr)
tstest  = ts(training$visitsTumblr)
library(forecast)
fit <- bats(training$visitsTumblr, num.cores=NULL)
forecasted <- forecast(fit, h=dim(testing)[1], level=95)

plot(testing$X, testing$visitsTumblr, )
lines(testing$X, forecasted$upper)
lines(testing$X, forecasted$lower)

length(testing$visitsTumblr[ testing$visitsTumblr < forecasted$upper ]) / length(testing$visitsTumblr) * 100
