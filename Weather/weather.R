install.packages("rattle")
install.packages("DAAG")

library(rattle)
library(DAAG)

data(weather)
str(weather) #데이터 요약
names(weather) #변수명 확인
help(weather) #weather 데이터 정보
attach(weather)

table(RainTomorrow) #목표의 범주 별 빈도수(factor)

RainTomorrow.1 <- as.numeric(RainTomorrow) - 1
table(RainTomorrow.1)

plot(MinTemp, RainTomorrow.1+rnorm(nrow(weather),0,0.05),col=c("red","blue")[unclass(RainTomorrow)],pch=20,ylab="RainTomorrow",main="weather")
#rnorm은 임의로 더해진 오차. unlcass를 이용해 yes/no를 기준으로 색 칠함
abline(h=c(0,1), lty="dotted")

#연속 공변량 MinTemp와 더미형 반응변수 y간 관계를 보는 데 산점도는 한계가 있음

par(mfrow=c(2,1))
windows(200,100)
hist(MinTemp[RainTomorrow.1==0],main="No Rain",xlab="MinTemp",xlim=c(-10,30),breaks=seq(-10,30,2.5))
hist(MinTemp[RainTomorrow.1==1],main="Rain",xlab="MinTemp",xlim=c(-10,30),breaks=seq(-10,30,2.5))
#비가 온 날에 MinTemp가 큰 경향이 있음(그림1)

MinTemp.c <- cut(MinTemp, quantile(MinTemp, prob=c(0,0.25,0.5,0.75,1),na.rm=T),include.lowest=T) #이산변수로 코딩

mosaicplot(~ MinTemp.c + RainTomorrow, color = c("white","gray"),xlab = "MinTemp", main=paste("weather",3)) #Q4에서 비온날의 비율이 높음

cut.x <- function(var) cut(var,unique(quantile(var,prob=c(0,0.25,0.5,0.75,1),na.rm=T)),include.lowest = T)

detach(weather)
attach(weather2)

for (j in c(3:22)){
  if (class(weather2[,j])[1] == "numeric" | class(weather2[,j])[1] == "integer")
    mosaicplot(~ cut.x(weather2[,j]) + RainTomorrow, color = c("white","gray"),xlab=colnames(weather2)[j], main=paste("weather",j))
  else mosaicplot(~ weather2[,j]+RainTomorrow, color = c("white","gray"),xlab=colnames(weather2)[j], main = paste("weather",j))
  pause()
} #변수가 numeric일시 이산화, 아닐시 그대로 mosaic plot에 투입
#weather7(Sunshine), weather15(Humidity3pm), weather 17(Pressure3pm), weather 19(Cloud3pm) 모자이크 그림이 유의해보임

pairs(cbind(Sunshine, Humidity3pm,Pressure3pm,Cloud3pm=Cloud3pm+runif(nrow(weather2),-0.5,0.5)),col=c("#FF000055","#0000FF99")[RainTomorrow],pch=19,main="weather") #유의해보이는 변수들 간의 산점도 행렬 그렇게 유의한 상관성은 보이지 않음

#로지스틱 회귀
logistic <- glm(RainTomorrow ~ Sunshine + Humidity3pm + Pressure3pm + Cloud3pm, family = "binomial")

fitted <- predict(logistic, weather2[,c("Sunshine","Humidity3pm","Pressure3pm","Cloud3pm")])

predicted <- ifelse(rank(fitted) <= 300, "no rain", "rain")

addmargins(table(predicted, RainTomorrow)) #예측분류표
