
library(ISLR)
library(ggplot2)
?Wage
View(Wage)

df = Wage
head(df)
dim(df)
colSums(is.na(df))




error=c()
test_error = c()
samplesize = c(10,20,50,100,500,1000,2000)
for (n in samplesize){
  set.seed(10)
  rand = sample(1:nrow(df),n)
  randt = sample(1:nrow(df),3000)
  #rand
  train = df[rand, c("age","wage")]
  test = df[randt, c("age","wage")]
  
  #train
  #test
  #view(diamonds)
  
  # polynomial regression for order 7
  a <- lm(wage ~ age + I(age^2) + I(age^3) + I(age^4) + I(age^5) + I(age^6) + I(age^7), train)
  a
  
  #plotting model
  fname=paste("Graph of Sample Size ",paste(n,".jpg"))
  jpeg(fname)
  plot(train$age,train$wage, pch=19, cex=0.5, main = paste("Wage vs Age for sample size ", n), sub = "Degree = 7", xlab = "Age", ylab = "Wage")
  lines(sort(train$age), fitted(a)[order(train$age)], col='blue', type='l',pch=20)
  dev.off()
  
  
  #accuracy
  sum(a$residuals^2)
  pred = predict(a, newdata=test)
  error = sum((pred-test$wage)^2)
  test_error = append(test_error, error)
  test_error
  #error
  
  #saving the plots
  #getwd()
  #setwd("C:\\Users\\chira\\Desktop\\R plots")
}

