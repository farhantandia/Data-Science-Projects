Product<-read.table("C:/Users/farha/Documents/GitHub/Data Science Projects/R/Data Files/Product.txt", header=TRUE, sep="\t")
str(Product)

Customer<-read.csv("C:/Users/farha/Documents/GitHub/Data Science Projects/R/Data Files/Customer.csv")
Customer
View(Customer)

y<- table(Customer$Region)
barplot(y)
barplot(y[order(y)])
barplot(y[order(-y)])
barplot(y[order(-y)],horiz = FALSE, col = c('blue','red','green','yellow'), border = NA,
        main = 'Frequency of \n Region',ylab = "Number of customers")

hist(Customer$Age, breaks = 10)
hist(Customer$Age, breaks = c(0,40,60,100), freq = TRUE)

