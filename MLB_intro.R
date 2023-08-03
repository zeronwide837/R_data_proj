setwd("E:/R/project")

parkch <- read.csv("parkch.csv",header=T)
head(parkch,3)
str(parkch)
names(parkch)

with(parkch, range(yearID))
range(parkch$yearID) #yearID 는 integer(32bit)
class(parkch$yearID)

with(parkch, table(teamID))

parkch$teamID.1 <- factor(parkch$teamID, levels = c("LAN","TEX","SDN","PHI","NYA"))
with(parkch, table(teamID.1))
with(parkch, barplot(table(teamID.1), beside=F, main="Park Chan Ho's Team History"))
#barplot(table(parkch$teamID.1),beside=F, main="Park Chan Ho's Team History")도 똑같이 작용

attach(parkch)
barplot(salary, names.arg=yearID, main="Park Chan Ho's salary")

barplot(salary/1000, names.arg=yearID, las=2, main="Park Chan Ho's Salary (in 1000)") #las=2로 x축 레이블을 세로로 세움

##벡터
year <- c(2001, 2002, 2003, 2004)
y <- c(2002,2001,2003,2004)
length(year) #벡터 길이(인자 갯수)
is.vector(year) #벡터 자료형 여부
identical(year,y) #두 벡터가 일치하는 지 판별

tf <- c(T,F,F,F) #논리형 벡터
team <- c("LAN","LAN","TEX","NYA") #문자형 벡터

class(tf) #logical
class(team) #character
class(year) #numeric

z <- c(2001L, 2002L, 2003L, 2004L)
class(z) #integer

year-z #0 0 0 0
identical(year,z) #false, 자료형이 다름

year[3] #인덱스로 요소 조회 가능
team[3]

team.1 <- factor(team) #문자형 벡터를 인자형으로 전환
team.1

levels(team.1)
as.numeric(team.1) #인자형 벡터는 숫자형으로 변환 가능

mode(team.1)
mode(team)

#어느 변수가 범주형이면 factor로 변환하는 것을 추천

#벡터 생성 방식
#c() 함수 사용
#rep() 함수 사용
rep("LAN",3)
rep(c("LAN","TEX","SDN","PHI","NYA"), c(8,4,1,1,1))

#seq() 함수 사용
seq(from=1, to=10, by=3)
seq(from=1, to=10, length.out=5) #by == (to - from) / (length.out-1)
seq(from=1, to=10, by=1)
seq(1,10)
sum(rep(1:9, 9:1))
rep(1:9,9:1) #1이 9번, 2가 8번, ... 9가 1번 반복됨

##논리 연산
#부등호
parkch$salary > 1.0e+7 #박찬호 선수의 연봉이 1천만 불을 넘은 해
with(parkch, yearID[salary > 1.0e+7])
with(parkch, teamID[salary > 1.0e+7])

#등호
parkch$teamID == "NYA"
with(parkch, teamID == "NYA")
with(parkch, yearID[teamID == "NYA"])

#not equal
with(parkch, teamID != "LAN")
with(parkch, yearID[teamID != "LAN"])

#AND OR
with(parkch, teamID == "LAN" | teamID == "SDN" | teamID == "PHI")
year.NL <- with(parkch, teamID == "LAN" | teamID == "SDN" | teamID == "PHI")
parkch$yearID[year.NL]

#집합 연산
NL <- c("LAN", "SDN","PHI")
parkch$teamID %in% NL

year.NL <- parkch$teamID %in% NL #앞 코드와 결과는 같지만 코드가 더 간결함
parkch$yearID[year.NL]

year.NL.1 <- c(rep(T,8), rep(F,4), rep(T,2), F)
identical(year.NL, year.NL.1)

#T를 True로, F를 False로 사용할 수 있으나 다른 변수로 사용할 수도 있으니 가급적 사용 자제

##산술 연산
5+4
"+"(5,4)
#두 연산 같음

8 %/% 3 #몫연산
8 %% 3 #나머지 연산

4.3 %/% 2.1
4.3 %% 2.1

year <- 1:2016
k1 <- sum(year %% 4 == 0)
k2 <- sum(year %% 100 == 0)
k3 <- sum(year %% 400 == 0)
c(k1,k2,k3)
k1-k2+k3 #윤년의 수

0.7 - 0.4
0.7 - 0.4 == 0.3
print(0.7-0.4, 16)
#유동 수를 처리할 때는 동일여부를 묻는 건 곤란

10 ^ (308:309)
10 ^ -(323:324)

10^309 / 10^309
10^309 - 10^309

##Data Frame
library(Lahman)
data(Pitching)
attach(Pitching)
pitch <- Pitching[playerID == "parkch01",]
detach(Pitching)
pitch <- pitch[,c(2,4,6,7,8,20)]
str(pitch)
#G는 게임 수, W는 승리 수, L은 패배 수, ERA는 평균자책점

table(pitch$yearID)[table(pitch$yearID) > 1]
#2005년과 2010에는 2개의 팀에서 활동해서 한 해의 데이터가 두 줄에 있음

pitch.1 <- pitch
pitch.1[12,3:4] <- pitch[12,3:4] + pitch[13,3:4] #12행과 13행의 승,패를 병합해 12행에 기록
pitch.1[18,3:4] <- pitch[18,3:4] + pitch[19,3:4] #18행과 19행의 승,패를 병합해 12행에 기록

pitch.1[12,6] <- (pitch[12,5]*pitch[12,6] + pitch[13,5]*pitch[13,6])/(pitch[12,5]+pitch[13,5]) #12행의 ERA 추가
pitch.1[18,6] <- (pitch[18,5]*pitch[18,6] + pitch[19,5]*pitch[19,6])/(pitch[18,5]+pitch[19,5]) #18행의 ERA 추가

pitch.1[13,] <- NA
pitch.1[19,] <- NA
pitch.2 <- pitch.1[c(1:12,14:18),] #결측치 제거
str(pitch.2)

str(parkch)
parkch <- parkch[,c(1,3,4)] #chr 변수 제거
parkch <- parkch[,c(1,3,2)] #열 순서 바꿈
parkch
names(parkch)[2] <- "teamID"

parkch.1 <- data.frame(yearID=2007:2008, teamID = c("NYA","LAN"),salary=c(NA,NA))
parkch.2 <- rbind(parkch, parkch.1)

parkch.master <- merge(parkch.2, pitch.2, by = "yearID") #yearID를 기준으로 병합
parkch.master

par(mfrow=c(2,2))
parkch.master$salary <- round(parkch.master$salary / 1000, 0) 
attach(parkch.master)
plot(salary, G, xlab="salary in 1000", ylab = "G")
text(salary, G, labels = substring(yearID,3,4), pos = 3.5, cex = 0.8)

plot(salary, ERA, xlab="salary in 1000", ylab = "ERA")
text(salary, ERA, labels = substring(yearID,3,4), pos = 3.5, cex = 0.8)

plot(salary, W, xlab="salary in 1000", ylab = "W")
text(salary, W, labels = substring(yearID,3,4), pos = 3.5, cex = 0.8)

plot(salary, W-L, xlab="salary in 1000", ylab = "(W-L)")
text(salary, W-L, labels = substring(yearID,3,4), pos = 3.5, cex = 0.8)

par(mfrow = c(1,1))
with(parkch.master, barplot(salary, names.arg = substring(yearID,3,4),main="Park Chan Ho's Salary (in 1000)"))