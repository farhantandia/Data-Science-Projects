df_raw<-read.csv("C:/Users/farha/Documents/GitHub/Data Science Projects/R/Data Files/Linear Regression Dataset/House_Price.csv")

summary(df)
hist(df$crime_rate)

pairs(~price+crime_rate+n_hot_rooms+rainfall, data=df)
barplot(table(df$airport))
barplot(table(df$waterbody))
barplot(table(df$bus_ter)) #ignore this

#observations
#n_hot_rooms and rainfall has outliers
# n_hos beds has missing values
# bus_term is a useless variable
# crime_rate has some other functional relationship with price

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

df2 <-df[,-7:-10]
df <-df2
rm(df2)
#remove bus feature
df <- df[,-14]

#Dummy variable
install.packages("dummies")

df <- dummy.data.frame(df)
#airport no and none
df <- df[,-9]
df <- df[,-14]

#correlation matrix
cor(df)

round(cor(df),2)

df <- df[,-16] 

#Linear regression model (simple 1st order)

simple_model <-lm(price~room_num,data=df)

summary(simple_model)

plot(df$room_num,df$price)
abline((simple_model))

#Multi Linear regression model (multi order)
multiple_model <-lm(price~.,data=df)
summary(multiple_model) #* means its impacting the price variable
#greater R mean  good model (>0.5) but cant tell about variable importance

#Data split
install.packages("caTools")

set.seed(8)
split = sample.split(df,SplitRatio = 0.8)
training_set = subset(df,split==TRUE)
test_set = subset(df,split==FALSE)

lm_a = lm(price~.,data=training_set) #training
train_a = predict(lm_a,training_set)
test_a = predict(lm_a,test_set) #testing

mean((training_set$price-train_a)^2)
mean((test_set$price-test_a)^2)

#Subset selection

install.packages("leaps")
lm_best = regsubsets(price~.,data=df, nvmax=15) #best selection
summary(lm_best)
summary(lm_best)$adjr2
coef(lm_best,8)

lm_forward = regsubsets(price~.,data=df, nvmax=15, method ="forward") 
summary(lm_forward)
summary(lm_forward)$adjr2
coef(lm_forward,8)

lm_backward = regsubsets(price~.,data=df, nvmax=15, method ="backward") 
summary(lm_backward )
summary(lm_backward )$adjr2
coef(lm_backward ,8)

#Ridge Regression

x=model.matrix(price~.,data=df)[,-1]
y=df$price
grid = 10^seq(10,-2,length=100)

lm_ridge = glmnet(x,y,alpha=0, lambda = grid)
summary(lm_ridge)
cv_fit = cv.glmnet(x,y,alpha=0, lambda = grid)
cv_fit
plot(cv_fit)

opt_lambda = cv_fit$lambda.min
tss = sum((y-mean(y))^2)
y_a = predict(lm_ridge, s=opt_lambda, newx = x) #predicted values

rss = sum((y_a-y)^2) #rss
rsq = 1 - rss/tss #R2

#lasso regression
lm_lasso = glmnet(x,y,alpha=1, lambda = grid)
cv_fit_lasso = cv.glmnet(x,y,alpha=1, lambda = grid)
plot(cv_fit_lasso)

opt_lambda_lasso = cv_fit_lasso$lambda.min
tss_lasso = sum((y-mean(y))^2)
y_a_lasso = predict(lm_lasso, s=opt_lambda_lasso, newx = x) #predicted values

rss_lasso = sum((y_a_lasso-y)^2) #rss
rsq_lasso = 1 - rss_lasso/tss_lasso #R2

