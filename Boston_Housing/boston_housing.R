library(MASS)
data(Boston) #Boston Housing Data
str(Boston) #요약
names(Boston) #변수명
help(Boston) #데이터 설명

attach(Boston)

hist(medv, main="Boston") #중간 주택 가격 히스토그램, right skewed
par(mfrow=c(1,3))
boxplot(medv, main="raw")
boxplot(sqrt(medv), main="sqrt") #제곱근 변환 가장 symmetric
boxplot(log(medv,10),main="log_10") #로그 변환

medv.1 <- sqrt(medv)

par(mfrow=c(1,2))
plot(age, medv.1, xlab="age", ylab="sqrt medv", main="Boston")
lines(lowess(age,medv.1),col="red",lty="dotted")
plot(tax,medv.1,xlab="tax",ylab="sqrt medv", main="Boston")
lines(lowess(tax,medv.1), col="red", lty="dotted")

p <- 13; importance <- rep(0,p)
for (j in 1:p){
  lowess.fit <- lowess(Boston[,j], medv.1)$y
  importance[j] <- max(lowess.fit) - min(lowess.fit)
}

names(importance) <- colnames(Boston)[-14]
round(sort(importance,decreasing=T),1) #중요도 순으로 정렬, 중간값이 1.6임을 고려하여 상위 3개 변수가 중요하다고 판단

var.order <- order(importance, decreasing=T)
var.order #6번 변수가 가장 중요

library(DAAG); x11()
for(j in 1:p){
  j.1 <- var.order[j]
  plot(Boston[,j.1], medv.1, xlab=colnames(Boston)[j.1], ylab="sqrt medv")
  lines(lowess(Boston[,j.1],medv.1),col="red",lty="dotted")
  pause()
}

#rm,lstat,crim과 mdev.1간 산점도가 유의해보임

final <- lm(medv.1~rm+lstat+log(crim,2))
summary(final)

plot(medv.1~final$fit, xlab="fitted", ylab="sqrt medv", main="linear regression")
