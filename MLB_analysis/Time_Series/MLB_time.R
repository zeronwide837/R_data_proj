##메이저리그의 추세
library(Lahman)
data(Teams)
teams <- subset(Teams, yearID >= 1965)
teams.1 <- subset(teams, select = c(yearID, lgID, teamID, G, W, L, attendance))
plot(with(teams.1, table(yearID)), type="s", ylim=c(0,33), xlab="year",ylab="number of teams", main="Major League Baseball") #1965년 이후 메이저리그 팀 수

yearly <- with(teams.1, aggregate(attendance, by=list(year=yearID), sum)) #연도별 총계 계산
attend <- ts(yearly$x, start = 1965) #시계열 선언

plot(attend/10000, xlim=c(1965, 2015), ylim=c(0,10000),type="o",xlab="year", ylab="attendance(10,100)", main = "MLB Attendance Record")

abline(v=seq(1970,2010,10), lty="dotted", col="blue")

trend.all <- lm(attend[1:45]/10000 ~ yearly$year[1:45])
abline(coef(trend.all), lty="dotted", col="red", lwd=2) #회귀선

#2009년 이후 최근 6년에 걸쳐 실적의 추세선 이탈이 명확함

teams.LAN <- subset(teams.1, teamID == "LAN")
attend.LAN <- ts(teams.LAN$attendance, start=1965)
plot(attend.LAN/10000, xlim=c(1965,2015), ylim=c(0,500), type="o", xlab="year", ylab="attendance(10,000)", main="LA Dogers")
abline(v=seq(1970,2010,10), lty="dotted", col="blue")
trend.LAN <- lm(attend.LAN[1:45]/10000 ~ teams.LAN$yearID[1:45])
abline(coef(trend.LAN), lty="dotted", col="red", lwd=2)
#LA Dodgers의 연도별 관중 수:1965년-2009년 적합선 추가

teams.NYA <- subset(teams.1, teamID == "NYA")
attend.NYA <- ts(teams.NYA$attendance, start=1965)
plot(attend.NYA/10000, xlim=c(1965,2015), ylim=c(0,500), type="o", xlab="year", ylab="attendance(10,000)", main="NY Yankees")
abline(v=seq(1970,2010,10), lty="dotted", col="blue")
trend.NYA <- lm(attend.NYA[1:45]/10000 ~ teams.NYA$yearID[1:45])
abline(coef(trend.NYA), lty="dotted", col="red", lwd=2)
#NY Yankees의 연도별 관중 수:1965년-2009년 적합선 추가

##팀의 승률과 관중 수
teams.2013 <- subset(teams.1, yearID == 2013)
with(teams.2013, cor(I(W/G), attendance/10000))
with(teams.2013, plot(attendance/10000 ~ I(W/G), type = "n", main = "Year 2013", xlim=c(0.3,0.7), ylim=c(100,400), xlab="win probability", ylab="attendance in 10000"))
with(teams.2013, text(I(W/G), attendance/10000, teamID, cex=0.8, col=c("red","red","red","blue","blue")[unclass(lgID)]))
#2013년 메이저 리그의 팀 승률과 팀별 관중 수

teams.2014 <- subset(teams.1, yearID==2014)
with(teams.2014, cor(I(W/G), attendance/10000))
with(teams.2014, plot(attendance/10000 ~ I(W/G), type = "n", main = "Year 2014", xlim=c(0.3,0.7), ylim=c(100,400), xlab="win probability", ylab="attendance in 10000"))
with(teams.2014, text(I(W/G), attendance/10000, teamID, cex=0.8, col=c("red","red","red","blue","blue")[unclass(lgID)]))
#2014년 메이저 리그의 팀 승률과 팀별 관중 수

win.2013 <- teams.2013$W/teams.2013$G
names(win.2013) <- teams.2013$teamID

win.2014 <- teams.2014$W/teams.2014$G
names(win.2014) <- teams.2014$teamID

identical(names(win.2013),names(win.2014))
cor(win.2013,win.2014)

plot(win.2013, win.2014, pch=20, xlim=c(0.3,0.7), ylim=c(0.3,0.7), main="Win Percents of 2013 and 2014")
text(win.2013+0.02, win.2014+0.015, names(win.2013), cex=0.8, col=c("red","red","red","blue","blue")[unclass(teams.2014$lgID)])
#2013년과 2014년 메이저 리그의 팀 승률

##게임 당 득점
teams.1 <- subset(teams, select=c(yearID,lgID,teamID,G,R,ER,AB,H,HR,SO,BB))
yearly <- with(teams.1,aggregate(teams.1[,c("G","R","ER","AB","H","HR","SO","BB")], by = list(year=yearID), sum))
R.average <- ts(yearly$R/yearly$G, start=1965)
mean(R.average)
plot(R.average, xlim=c(1965, 2015), ylim=c(3,6), type="o", xlab="year",ylab="run average", main = "MLB Run Average since 1965")
abline(v=seq(1970,2010,10),lty="dotted",col="blue")

yearly.1 <- with(teams.1, aggregate(teams.1[,c("G","R","ER","AB","H","HR","SO","BB")], by=list(year=yearID, league=lgID), sum))
head(yearly.1,5)
tail(yearly.1, 5)
R.average.AL <- ts(yearly.1$R[yearly.1$league=="AL"]/yearly.1$G[yearly.1$league=="AL"], start=1965)
R.average.NL <- ts(yearly.1$R[yearly.1$league=="NL"]/yearly.1$G[yearly.1$league=="NL"], start=1965)
plot(R.average.AL, xlim=c(1965,2015), ylim=c(3,6), type="o",col="red",xlab="year",ylab="runs", main="Average Runs by League since 1965")
abline(v=seq(1970,2010,10), lty="dotted")
par(new=T)
plot(R.average.NL, xlim=c(1965,2015), ylim=c(3,6), type="o",col="blue",xlab="year",ylab="runs", main="")
legend(1964, 6, legend = c("AL","NL"), col=c("red","blue"),lty=1,cex=0.8)
#메이저 리그의 게임당 평균 득점, 1965년 이후 : AL과 NL의 비교

teams.LAN <- subset(teams.1, teamID == "LAN")
R.average.LAN <- ts(teams.LAN$R/teams.LAN$G, start=1965)
plot(R.average.LAN, xlim=c(1965,2015), ylim=c(3,6), type="o", xlab="year", ylab="run average", main="LA Dodgers")
abline(v=seq(1970,2010,10), lty="dotted", col="blue")
#LA Dodgers팀의 게임당 평균 득점, 1965년 이후

teams.NYA <- subset(teams.1, teamID == "NYA")
R.average.NYA <- ts(teams.NYA$R/teams.NYA$G, start=1965)
plot(R.average.NYA, xlim=c(1965,2015), ylim=c(3,6), type="o", xlab="year", ylab="run average", main="NY Yankees")
abline(v=seq(1970,2010,10), lty="dotted", col="blue")
#NY Yankees팀의 게임당 편귱 득점, 1965년 이후

#평균 실점(RA), 평균 실책수(E), 평균 안타수(H), 평균 홈런수(HR)도 확인

##메이저 리그 타자 기록
library(Lahman)
data(Batting)
str(Batting)
Batting[Batting$playerID=="choosh01", 1:12] #추신수 선수의 기록

length(unique(Batting$playerID))#데이터가 쥰내 많다

library(plyr)
yy.players <- ddply(Batting, .(playerID), summarise, year.1=min(yearID), year.2=max(yearID)) #데이터 프레임을 분할하고 함수를 적용한 뒤 결과를 데이터 프레임으로 반환
str(yy.players)
head(yy.players, 5)
tail(yy.players, 5)
duration <- with(yy.players, year.2-year.1+1) #선수 별 활동 년 수
table(duration[yy.players$year.1 >= 1965]) #빈도표
hist(duration[yy.players$year.1 >= 1965], breaks=0:30, xlab="years", ylab="frequency", main="ML Playing Years")
quantile(duration[yy.players$year.1 >= 1965], prob=c(0,0.05,0.1,0.25,0.5,0.75,0.9,0.95,1))

duration <- with(yy.players, ifelse(year.2 < 2014, year.2-year.1+1,NA)) #2014년에 데뷔한 선수는 결측값으로 처리

which.max(duration[yy.players$year.1 >= 1965])
yy.players$playerID[yy.players$year.1 >= 1965][8091] #최장 플레이 선수
Batting[Batting$playerID == "ryanno01", 1:10]

data(People)
People[People$playerID=="ryanno01",]

totalHR.player <- ddply(Batting, .(playerID), summarise, HomeRuns = sum(HR, na.rm=TRUE), Games=sum(G,na.rm=TRUE))
max(totalHR.player$HomeRuns)
with(totalHR.player, playerID[which.max(HomeRuns)])
People[People$playerID=="bondsba01",c("nameFirst","nameLast")]
#최고기록은 762 홈런의 Barry Bonds

nn <- order(totalHR.player$HomeRuns, decreasing=T)
head(totalHR.player[nn,],10)

plot(cumsum(Batting$HR[Batting$playerID=="bondsba01"]), type="s", xlim=c(0,25), ylim=c(0,800), xlab="career year", ylab="cummulative home runs", main="Home Run Records")
par(new=T)

plot(cumsum(Batting$HR[Batting$playerID=="aaronha01"]), type="s",col=2,lty="dotted",xlim=c(0,25), ylim=c(0,800), xlab="", ylab="", main="")
par(new=T)

plot(cumsum(Batting$HR[Batting$playerID=="ruthba01"]), type="s",col=3,lty="dotted",xlim=c(0,25), ylim=c(0,800), xlab="", ylab="", main="")
par(new=T)

legend("topleft", legend=c("Bonds","Aaron","Ruth"),lty=c(1,2,2),col=1:3)

max(totalHR.player$Games)
with(totalHR.player, playerID[which.max(Games)]) #출전 수 1위 선수
People[People$playerID=="rosepe01", c("nameFirst","nameLast")]

nn <- order(totalHR.player$Games, decreasing=T) #출전 수 상위 10명
head(totalHR.player[nn,], 10)
