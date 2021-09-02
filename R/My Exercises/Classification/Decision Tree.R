df_raw<-read.csv("C:/Users/farha/Documents/GitHub/Data Science Projects/R/Data Files/Decision Tree Dataset/Movie_classification.csv")
summary(df_raw)
df<-df_raw

### DATA PREPROCESSING ###

#missing values handling using mean
df$Time_taken[is.na(df$Time_taken)]<- mean(df$Time_taken,na.rm = TRUE)

### DATA SPLIT ###
#using catools
set.seed(0)
split=sample.split(df,SplitRatio = 0.8)
train = subset(df, split==TRUE)
test = subset(df, split==FALSE)

### Decision Tree ###
#using rpart, rpart.plot

#run classification tree model on train set
classtree <-rpart(formula = Start_Tech_Oscar~., data = train,method = 'class', 
                  control = rpart.control(maxdepth = 3))

#plot decision tree
rpart.plot(classtree, box.palette = "RdBu", digits = -2)
#Predict value at any point
test$pred <- predict(classtree, test, type = 'class')

table(test$Start_Tech_Oscar, test$pred)
64/107

### Ada Boosting ###
#install.packages('adabag')
library(adabag)

train$Start_Tech_Oscar <-as.factor(train$Start_Tech_Oscar)

adaboost <- boosting(Start_Tech_Oscar~., data=train, boos = T)
predada <- predict(adaboost,test)
table(predada$class,test$Start_Tech_Oscar)

t1<-adaboost$trees[[1]]
plot(t1)
text(t1,pretty = 10)
68/107


### Ada Boosting ###
install.packages("xgboost")
library(xgboost)

trainY = train$Start_Tech_Oscar == "1"


trainX <- model.matrix(Start_Tech_Oscar ~ .-1, data = train)
trainX <- trainX[,-12]

testY = test$Start_Tech_Oscar == "1"

testX <- model.matrix(Start_Tech_Oscar ~ .-1, data = test)
testX <- testX[,-12]
#delete additional variable

Xmatrix <- xgb.DMatrix(data = trainX, label= trainY)
Xmatrix_t <- xgb.DMatrix(data = testX, label = testY)

Xgboosting <- xgboost(data = Xmatrix, # the data   
                      nround = 50, # max number of boosting iterations
                      objective = "multi:softmax",eta = 0.3, num_class = 2, max_depth = 10)

xgpred <- predict(Xgboosting, Xmatrix_t)
table(testY, xgpred)
69/107
