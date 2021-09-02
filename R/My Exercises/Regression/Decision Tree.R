df_raw<-read.csv("C:/Users/farha/Documents/GitHub/Data Science Projects/R/Data Files/Decision Tree Dataset/Movie_Regression.csv")
summary(df_raw)
df<-df_raw

### DATA PREPROCESSING ###

#missing values handling using mean
df$Time_taken[is.na(df$Time_taken)]<- mean(df$Time_taken,na.rm = TRUE)
df <- dummy.data.frame(df)

df <- df[,-12]
### DATA SPLIT ###
#using catools
set.seed(0)
split=sample.split(df,SplitRatio = 0.8)
train = subset(df, split==TRUE)
test = subset(df, split==FALSE)
summary(train)
### Decision Tree ###
#using rpart, rpart.plot

#run regression tree model on train set
regtree <-rpart(formula = Collection~., data = train, control = rpart.control(maxdepth = 5))

#plot decision tree
rpart.plot(regtree, box.palette = "RdBu", digits = -2)
#Predict value at any point
test$pred <- predict(regtree, test, type = 'vector')

MSE2<-mean((test$pred -test$Collection)^2)

### Decision Tree with Pruning ###
fulltree <- rpart(formula = Collection~., data = train, control = rpart.control(cp=0))
rpart.plot(fulltree,box.palette = "RdBu", digits = -3)

printcp(fulltree)
plotcp(regtree)

mincp <- regtree$cptable[which.min(regtree$cptable[,"xerror"]),"CP"]

prunedtree <- prune(fulltree, cp=mincp)
rpart.plot(prunedtree, box.palette = "RdBu", digits = -3)

test$fulltree <- predict(fulltree, test,type = 'vector')

MSE2full <-mean((test$fulltree - test$Collection)^2)

test$pruned<- predict(prunedtree, test,type = 'vector')
MSE2pruned <-mean((test$pruned - test$Collection)^2)

### Bagging###
#using randomforest
set.seed(0)
bagging = randomForest(formula=Collection~., data=train, mtry=17)
test$bagging <- predict(bagging, test)

MSE2bagging <-mean((test$bagging-test$Collection)^2)


### Random Forest ###
#using randomforest
set.seed(0)
randomfor = randomForest(formula=Collection~., data=train, ntree=500)
test$random <- predict(randomfor, test)

MSE2random <-mean((test$random-test$Collection)^2)

### Gradient Boosting ###
#using gbm 

#install.packages('gbm')
library(gbm)
set.seed(0)
boosting = gbm(Collection~., data=train, distribution = 'gaussian', n.trees=5000,interaction.depth = 4,shrinkage = 0.2)
#distribution 'gaussian' for regression, 'bernoulli' for classification

test$boost = predict(boosting, test, n.trees=5000)
MSE2boost<-mean((test$boost - test$Collection)^2)

