# Kaggle Competition - Bicycle Sharing

setwd('../kaggle/bikeshare/')

library("DAAG")
library(caret)
library(caTools)
library(ggplot2)
library(rpart.plot)
library(randomForest)

bs <- read.csv('./data/train.csv')
ts <- read.csv('./data/test.csv')

str(ts)
# Convert datetime to a datetime data type from a factor

bs$datetime=strptime(as.character(bs$datetime), format="%Y-%m-%d %H:%M:%S")

bs$Weekday = weekdays(bs$datetime)

# Extract Hour from datetime variable
bs$Hour = as.factor(format(bs$datetime, '%H'))
bs$Weekday = factor(bs$Weekday, ordered=T, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

weekdayHourPlot <- ggplot(bs, aes(x=Hour, y=Weekday))

weekdayHourPlot + geom_tile(aes(fill=count)) + scale_fill_gradient(low="white", high="red")

weekdayHourLine <- ggplot(bs, aes(x=Hour, y=count))
weekdayHourLine + geom_line(aes(group=Weekday, color = Weekday), size=1.5, alpha=0.5) 

str(bs)

bs.lm <- lm(count ~ season + workingday + weather + atemp + humidity + windspeed + registered + Weekday + Hour, data=bs)
summary(bs.lm)

?cv.lm
a = cv.lm(df=bs, bs.lm, seed=100, m=10)

summary(a)

set.seed(111)

testSpl = sample(nrow(bs)/10)

bs.test = bs[testSpl,]
bs.train = bs[-testSpl,]

bs.lm <- lm(count ~ season + workingday + weather + atemp + humidity + windspeed + Weekday + Hour, data=bs.train)

summary(bs.lm)

pred.lm1 = predict(bs.lm, newdata = bs.test)

sse = sum((pred.lm1 - bs.test$count) ^ 2)
sst = sum((bs.test$count - mean(bs.train$count)) ^ 2)


kaggle.test = read.csv('./data/test.csv')
kaggle.test$datetime=strptime(as.character(kaggle.test$datetime), format="%Y-%m-%d %H:%M:%S")

kaggle.test$Weekday = weekdays(kaggle.test$datetime)

# Extract Hour from datetime variable
kaggle.test$Hour = as.factor(as.numeric(as.factor(format(kaggle.test$datetime, '%H'))))
kaggle.test$Weekday = factor(kaggle.test$Weekday, ordered=T, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

pred.kaggle = predict(bs.lm, newdata = kaggle.test)
summary(kaggle.test)
summary(bs.train)
summary(bs)

str(bs.train)
str(kaggle.test)

submitKag <- data.frame(datetime = kaggle.test$datetime, count = pred.kaggle)
str(submitKag)
write.csv(submitKag, './data/submission.csv')

#************ Trees and Random Forest ***************

tree1 <- rpart(count ~ season + workingday + weather + atemp + humidity + windspeed + Weekday + Hour, data=bs.train)
rf1 <- randomForest(count ~ season + workingday + weather + atemp + humidity + windspeed + Weekday + Hour, data=bs.train, nodesize=25, ntree=200)
prp(tree1)
summary(rf1)
pred.tr1 = predict(tree1, newdata=bs.test)
pred.rf1 = predict(rf1, newdata=bs.test)
sse.tr = sum((pred.tr1 - bs.test$count) ^ 2)
sse.rf = sum((pred.rf1 - bs.test$count) ^ 2)

pred.k.rf = predict(rf1, kaggle.test)
pred.k.rf <- round(pred.k.rf)


submitRF <- data.frame(UserID=testHappy$UserID, Probability1=FinalPredrf[,2])
submitKag <- data.frame(datetime = as.factor(as.character(kaggle.test$datetime)), count = pred.k.rf)
rownames(submitKag) = NULL
write.csv(submitKag, './data/submission.csv', row.names=F)

diff = pred.rf1 - bs.test$count
j = sort(diff, decreasing=T)
p = data.frame(dif = diff)
rownames(p) = NULL
p[1:20,]
head(p)
