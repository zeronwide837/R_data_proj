##메이저리그의 2001년 투수 기록
library(Lahman)
data(Pitching)
class(Pitching)
nrow(Pitching)
ncol(Pitching)

names(Pitching)
help(Pitching) #변수 도움말

pitch.01 <- subset(Pitching, yearID == 2001)#2001년 기록 추출
nrow(pitch.01) 

hist(pitch.01$G, xlab="Games", right=F,nclass=20,main="Number of Games")
hist(pitch.01$GS, xlab="Games",right=F,nclass=20,main="Number of Games Started") #GS는 선발 게임수

pitch.01.s <- subset(pitch.01,GS >= 10) #선발 게임 수가 10 이상인 선수로 제한
nrow(pitch.01.s)

hist.1 <- hist(pitch.01$G, xlab="Games", right=F,nclass=20, main="Number of Games")
str(hist.1)

hist.1$breaks #구간 경계점
hist.1$counts #빈도

##평균자책점의 점도표
attach(pitch.01.s)
x <- ER / IPouts * 27
head(round(x,3),10)
head(ERA,10)
#ERA는 투수별로 ER을 IPouts/27로 나누어 산출

order.1 <- order(ERA, decreasing=F) #오름차순으로 정렬
dotchart(ERA[order.1][30:1],labels=playerID[order.1][30:1], pch=20, xlim=c(0,3.5),xlab="ERA", cex=0.8, main="Major League Baseball, 2001") #평균자책점이 낮은 상위 30명의 투수

order(c(4,2,1,5,3))
x <- c(4,2,1,5,3)
x[order(x)]

pitch.01.s[playerID == "pineijo01" | playerID == "parkch01",] #pineijo01 과 parkch01 비교

library(Lahman)
data(People) #Master 데이터셋의 이름이 People로 변경됨
People[People$playerID == "pineijo01",]
rank(ERA)[playerID == "parkch01"]

##스트라이크 아웃과 피홈런의 관계
plot(HR ~ SO, main="MLB 2001, Pitching")
lines(lowess(HR~SO), col="red",lwd=2)

HRA <- HR / IPouts
SOA <- SO / IPouts
windows(100,100)
plot(HRA ~ SOA, xlab = "SO average", ylab = "HR average", main="MLB 2001, Pitching")
lines(lowess(HRA~SOA), col = "red", lwd=2)
#평균 스트라이크 아웃이 클수록 평균 피홈런이 작아지는 경향을 보임

identify(SOA, HRA, labels=playerID, cex=0.8, col="blue")
#windows()를 이용하여 윈도우 창을 새로 띄루고 점을 찍는 걸 권장함. R studio 내에 있는 plot은 조준점이 이상하게 찍힘

plot(HRA ~ SOA, xlab = "SO average", ylab = "HR average", main="MLB 2001, Pitching")
lines(lowess(HRA~SOA), sol="red",lwd=1)
#평균 스트라이크 아웃과 평균 피홈런의 관계

text(SOA[playerID == "parkch01"], HRA[playerID=="parkch01"], "*", col="blue", cex=4)
#박찬호 선수의 위치

BBA <- BB / IPouts
HA <- H / IPouts
plot(BBA ~ HA, xlab="H average", ylab="BB average", main = "MLB 2001, Pitching")
lines(lowess(BBA ~ HA), col = "red", lwd = 2)
#평균 피안타와 평균 볼넷의 관계

text(HA[playerID == "parkch01"], BBA[playerID == "parkch01"], "*", col="blue", cex=4)
#박찬호 선수의 위치

##American League와 National League
table(lgID) #각 리그 별 10경기 이상 선발 출전한 MLB 투수의 수
#AL과 NL만 있음

boxplot(ERA ~lgID, xlab="ERA", horizontal=T)
levels(lgID)
#리그 간 ERA비교

league <- factor(lgID, levels=c("AL","NL"))
boxplot(ERA ~ league, xlab="ERA", horizontal=T)
#AL과 NL만 표시
#ERA가 NL이 AL보다 상대적으로 작음

windows(100,100)
par(mfrow=c(4,1))

##투수 그룹 간 비교
library(Lahman)
pitch.01 <- subset(Pitching, yearID == 2001)
nrow(pitch.01)
with(pitch.01, plot(G,GS, ylim=c(0,40), main="MLB 2001, Pitching"))
#GS <= G, GS >= 0, G >= 0

e1 <- runif(nrow(pitch.01), -0.5, 0.5) #균일분포 난수 생성
e2 <- runif(nrow(pitch.01), -0.5, 0.5)
with(pitch.01, plot(G+e1, GS+e2, ylim=c(0,40), main="MLB 2001, Pitching"))

with(pitch.01, plot(G+e1, GS+e2, ylim=c(0,40), main="MLB 2001, Pitching"))
abline(h=10, col="red", lwd=2) #가로 구분선
segments(20,0,20,10,col="red",lwd=2) #세로 구분선

gr <- with(pitch.01, ifelse(GS >= 10, 1, ifelse(G>=20,2,3)))
table(gr)
#그룹1(GS>=10)의 투수는 191명, 그룹2(GS<10,G>=20) 의 투수는 219, 그룹3(GS<10, G<20)의 투수는 242명

with(pitch.01, boxplot(ERA ~ gr, names = c("grp.1", "grp.2", "grp.3"), main = "MLB 2001, by Pitcher Group")) #투수 그룹별 ERA

with(pitch.01, boxplot(ERA ~ gr,ylim=c(0,20), names = c("grp.1", "grp.2", "grp.3"), main = "MLB 2001, by Pitcher Group")) #가시성을 위해 수직축 제한

with(pitch.01, boxplot(IPouts/G ~ gr, names=c("grp.1", "grp.2", "grp.3"), main = "MLB 2001, by Pitcher Group")) #투수 그룹 간 평균 IPouts 비교
