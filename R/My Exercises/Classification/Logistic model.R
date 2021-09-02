df_raw<-read.csv("C:/Users/farha/Documents/GitHub/Data Science Projects/R/Data Files/Logistic Reg Dataset/House-Price.csv")

summary(df_raw)
df <- df_raw
hist(df$crime_rate)

pairs(~price+crime_rate+n_hot_rooms+rainfall, data=df)
barplot(table(df$airport))
barplot(table(df$waterbody))
barplot(table(df$bus_ter)) #ignore this

### Observations ###
#n_hot_rooms and rainfall has outliers
# n_hos beds has missing values
# bus_term is a useless variable
# crime_rate has some other functional relationship with price

### DATA CLEANING ###

#handle outliers
quantile(df$n_hot_rooms, 0.99)
uv = 3*quantile(df$n_hot_rooms, 0.99)
df$n_hot_rooms[df$n_hot_rooms>uv] <-uv
summary(df$n_hot_rooms)

lv = 0.3*quantile(df$rainfall, 0.01)
df$rainfall[df$rainfall<lv] <-lv
summary(df$rainfall)

#handle missing values
mean(df$n_hos_beds)
mean(df$n_hos_beds,na.rm=TRUE)
which(is.na(df$n_hos_beds))
df$n_hos_beds[is.na(df$n_hos_beds)] <-mean(df$n_hos_beds,na.rm=TRUE)

summary(df$n_hos_beds)

which(is.na(df$n_hos_beds))

#variable transformation
pairs(~price+crime_rate,data=df)
plot(df$price,df$crime_rate)

df$crime_rate=log(1+df$crime_rate)
plot(df$price,df$crime_rate)

df$avg_dist = (df$dist1+df$dist2+df$dist3+df$dist4)/4

df2 <-df[,-6:-9]
df <-df2
rm(df2)
#remove bus feature
df <- df[,-13]

#Dummy variable
install.packages("dummies")

df <- dummy.data.frame(df)
#airport no and none
df <- df[,-8]
df <- df[,-14]

#correlation matrix
cor(df)

round(cor(df),2)
#df <- df[,-16] #remove parks

### DATA EXPLORATORY & MODELING PRACTICE ###

# Logistic Regression with single Predictor
glm.fit = glm(Sold~price, data =df, family = binomial)
summary(glm.fit)

#p-value should be < 0.05 to establlish relationship

# Logistic Regression with multiple Predictor

glm.fit = glm(Sold~., data =df, family = binomial)
summary(glm.fit)

#making confusion matrix

glm.probs = predict(glm.fit, type = "response")
glm.probs[1:10]

glm.pred = rep("NO",506)
glm.pred[glm.probs>0.5] = "YES"

table(glm.pred, df$Sold)

#LDA
lda.fit =lda(Sold~., data =df)
lda.pred = predict(lda.fit, df)
lda.pred$posterior

lda.class = lda.pred$class
table(lda.class, df$Sold)
sum(lda.pred$posterior [,1]>0.8)

### SPLIT DATASET ###

set.seed(0)
split = sample.split(df, SplitRatio = 0.8)
train_set = subset(df, split ==TRUE)
test_set = subset(df, split==FALSE)


### ML MODELING ###

# Logistic Regression
train.fit = glm(Sold~., data = train_set, family = binomial)
test_probs = predict(train.fit, test_set, type='response')
test_probs
test.pred = rep('NO',120)
test.pred[test_probs>0.5]='YES'
table(test.pred, test_set$Sold)

# KNN
trainX = train_set[,-16]
testX = test_set[,-16]
trainy = train_set$Sold
testy = test_set$Sold

k=2
trainX_s = scale(trainX)
testX_s = scale(testX)

set.seed(1)
knn.pred = knn(trainX_s, testX_s, trainy, k = k)
table(knn.pred, testy)
