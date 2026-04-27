# 정보요약과 범주형
library(dplyr)
library(ggplot2)
summary(mpg) # 요약
mpg |> group_by(class) |> summarise(
  mean_cty = mean(cty),
  mean_hwy = mean(hwy),
  n = n()
)
table(mpg$class) # 도수 요약
ggplot(mpg, aes(class, hwy)) + geom_boxplot() # 3사분위수, 중위수, 1사분위수, 하한선, 상한선

data(cars) # 로딩
str(cars) # 확인
head(cars)
plot(cars$speed, type="l") # y 축, x 축은 자동으로 순서, line type
plot(cars$dist, type="l") # line
plot(cars, type="l")
plot(cars, type="p") # point
plot(cars, type="b") # both (line + point)
plot(cars, type="b", pch=2) # 0 ~ 25 / pch : 점 모양
plot(cars, type="b", pch=22)
plot(cars, type="b", pch="+")
plot(cars, type="b", pch=2, cex=2)
plot(cars, type="l", pch="+", cex=2, col="red", lty="dotdash")
# xlab : x축 이름 / ylab : y축 이름
# limits 값이 데이터를 한정
plot(cars, main="그래픽", sub="옵션확인을 위해서", col="red", xlab="speed", ylab="distance",
     ylim=c(0,40), xlim=c(1,10), type="l", asp=1)
# aspect는 가로, 세로 비율
plot(cars, cex=0.5)
identify(cars$speed, cars$dist)
text(cars$speed, cars$dist, pos=1) # 보조 시각화함수

# 동전 -> 앞면이 나온 횟수
coin<-c(2, 2, 0, 1, 1, 1, 2, 3, 1, 2, 2, 3, 3, 4, 1, 3, 2, 3, 3, 1, 2, 2, 1, 2, 2)
(frequency <- table(coin)) # 도수분포표 (5가지)
(relative <- frequency / length(coin)) # 상대도수분포표
(cumulative <- cumsum(relative)) # 누적도수분포표
opar <- par(mfrow=c(1,4)) # 화면 분할 (행수, 열수)
plot(frequency, xlab="값", ylab="도수", type="b", col="red", main="도수",
     sub="순수도수", frame.plot=F)
plot(1:5, frequency, xlab="값", ylab="도수", type="b", col="red", frame.plot=F)
plot(round(relative,2), type="b", pch=23, col="red")
plot(round(cumulative, 2), type="b", col="red", axes=F)
par(opar) # 화면이 한개로
par(mfrow=c(1,1)) # 단일 화면 복원
plot(round(cumulative, 2), type="b", col="red", axes=F)
opar <- par(mfrow=c(1,1))
plot(round(cumulative,2), type="b", col="red", axes=F)

# 삼각함수 시각화
library(NISTunits)
x = 1:1440
x <- NISTdegTOradian(x) # 각도 -> 라디안 값으로 변환
plot(x, cos(x), # 1부터 시작 : 0도 일 때 1
     main="사인함수",
     ylab="sin(x)", type="l", col="blue")
plot(x, sin(x), # 0부터 시작 : 0도 일 때 0
     main="사인함수",
     ylab="sin(x)", type="l", col="blue")

# 진폭과 주기
x = 1:60
amp.1 <- 2 # 진폭
amp.2 <- 2
amp.3 <- 5
amp.4 <- 5
wav.1 <- 1 # 주기
wav.2 <- 2
wav.3 <- 3
wav.4 <- 7
# sin함수 : 물결모양을 만드는 함수(-1 ~ 1)
signal.1 <- amp.1*sin(wav.1*x) # -1 ~ 1 => -2 ~ 2 값
signal.2 <- amp.2*sin(wav.2*x)
signal.3 <- amp.3*sin(wav.3*x)
signal.4 <- amp.4*sin(wav.4*x)
par(mfrow = c(1,4))
# 보조그래프 함수 선을 작도 : horizontal(수평선), vertical(수직선)
plot(x, signal.1, type="l", ylim=c(-5,5)); abline(h=0, v=0, lty=3)
plot(x, signal.2, type="l", ylim=c(-5,5)); abline(h=0, lty=3)
plot(x, signal.3, type="l", ylim=c(-5,5)); abline(h=0, lty=3)
plot(x, signal.4, type="l", ylim=c(-5,5)); abline(h=0, lty=3)
par(mfrow = c(1,1))

library(MASS)
data("Boston") # 집값 예측
str(Boston)
par(mfrow=c(1,2))
Boston$tax # 세금
# bandwidth가 결정 : 부드러움과 뾰족함을 결정
# 보간법
plot(density(Boston$tax, bw=10)) # band width 대역폭 (주변의 데이터를 얼마나 고려)
# random + normal (정규분포)
# 중복 문제 해결
rug(Boston$tax + rnorm(length(Boston$tax), sd=5), col=2, lwd=3.5)
# bandwidth 클 때 : 부드러움
# bandwidth 작을 때 : 뾰족해짐
plot(density(Boston$tax, bw=2))
rug(Boston$tax + rnorm(length(Boston$tax), sd=5), col=2, lwd=3.5)
par(mfrow=c(1,1))

library(mlbench)
data(Ozone) # 태양, 습도, 풍속
str(Ozone)
head(Ozone)
opar <- par(mfrow=c(1,2))
# 중복
plot(Ozone$V6, Ozone$V7, xlab="Windspeed", ylab="Humidity", main="Ozone")
# 지터 잡음
plot(jitter(Ozone$V6), jitter(Ozone$V7), xlab="Windspeed", ylab="Humidity", main="Ozone")
par(opar)

# 고주파(복잡)를 저주파(단순화)로 변환
par(mfrow = c(1,2)) # 제동거리
plot(cars$dist, type="o", cex=0.5, xlab="speed", ylab="dist") # 중복값 때문에 그래프 복잡
tapply(cars$dist, cars$speed, mean) # speed로 군집화 평균 => 단일값 표현
plot(tapply(cars$dist, cars$speed, mean), type="o", cex=0.5,
     xlab="speed", ylab="dist")
par(mfrow = c(1,1))

# 신뢰구간 시각화
# 회귀분석(linear regression) 함수
# 회귀분석 => 직선의 기울기(3.932)와 절편(-17.579)
# 회귀식
dist = 3.932 * speed - 17.579
# 종속변수 : dist / 독립변수 : speed
(m <- lm(dist ~ speed, data=cars)) # : formula
(m <- lm(dist ~ speed, cars))
str(cars) # 실제 값
plot(cars) # 산포도 (speed(x), dist(y)) 순서에 따라 결정
abline(m) # 회귀선을 그림
(p <- predict(m, interval="confidence")) # 학습한 speed를 넣고 회귀식에 예측
head(p)
x <- c(cars$speed, tail(cars$speed, 1), rev(cars$speed), cars$speed[1])
y <- c(p[, "lwr"], tail(p[, "upr"],1), rev(p[, "upr"]), p[, "lwr"][1])
polygon(x,y,col=rgb(.7,.7,.7,.5))

opar <- par(mfrow=c(1,1))
plot(cars)
lines(lowess(cars)) # 비선형회귀

# 문제
# 선형회귀 후 시각화
str(women)
head(women)
summary(women)
(wm <- lm(weight ~ height, women))
plot(women)
abline(wm) # 우상향 : 선형회귀 (직선) / 완전상관

# 커널에 대한 이해 (density는 커널과 선형회귀에 의해 보관)
opar <- par(mfrow=c(1,2))
# 밀도함수 (연속적 데이터 : 적용되는 함수를 커널)
d <- density(mtcars$mpg, kernel="rectangular")
plot(d)
polygon(d, col="red", border="blue")

d <- density(mtcars$mpg, kernel="cosine") # 비현실적
plot(d)
polygon(d, col="red", border="blue")
opar <- par(mfrow=c(1,1))

# attach / detach
# 선형회귀로 비선형회귀 하는 방법
str(trees)
summary(trees)
Height2 <- trees$Height^2 # 파생변수
(trees2 <- cbind(trees, Height2))
attach(trees2) # 데이터를 마치 패키지처럼 사용하도록 로딩
search() # 로딩된 패키지 확인함수
(test2 <- lm(Girth ~ Height + Height2)) # 데이터원본 적용없이 변수처럼 데이터 열 사용
plot(Girth ~ Height)
fitted(test2) # 예측값
# sort나 order는 정렬
# sort는 값을 직접 정렬 / order는 정렬된 인덱스만 제공
lines(sort(Height), fitted(test2)[order(Height)], col="red", type="l")
detach(trees2) # 메모리 해제
search()

# 간단한 버전 qplot
library(ggplot2)
data(mpg)
str(mpg)
table(mpg$drv) # 4: 4륜구동 / f : 전륜구동 / r : 후륜구동
# stat이 붙으면 통계함수 : 구간범주화
qplot(hwy, data=mpg) + stat_bin(bins=30, binwidth=2)
qplot(hwy, data=mpg, fill=drv) # fill : 색을 채움
qplot(displ, hwy, data=mpg)
qplot(displ, hwy, data=mpg, color=drv) # color : 외곽선
qplot(hwy, data=mpg, facets=.~drv, binwidth=4) # 화면분할 : facets에서의 . : 열 분할
qplot(hwy, data=mpg, facets=drv~., binwidth=2)
qplot(hwy, data=mpg, fill=drv, facets=drv~., binwidth=2)
# 다차원으로 표현할 때 factor형이 아니면 그래프가 이상이 있을 수 있음
# 범주형 변수는 factor로 변환
qplot(wt, mpg, data=mtcars, size=qsec, color=factor(carb), shape=factor(cyl))
# 데이터 + aes + geom 순으로 배치
# layer 표현
# aes 미적 매핑
## ggplot은 선언형으로 작동 (통계처리 후 자동 시각화)
mpg_boxplot <- ggplot(mpg, aes(class, hwy)) +
  geom_boxplot(fill="lightblue") +
  labs(title="자동차 class별 고속도로 연비 분포 요약")
mpg_boxplot

# 중량과 연비
p <- ggplot(mtcars, aes(wt, mpg))
p + geom_point()
p + geom_point(aes(colour = factor(cyl))) # 실린더 개수 별로
p + geom_point(aes(shape = factor(cyl)))
p + geom_point(aes(size = qsec))
p + geom_point(aes(size = qsec, shape = factor(cyl)))
p + geom_point(aes(size = qsec, shape = factor(cyl), colour = factor(cyl)))

# 문제
iris_p = ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width))
iris_p + geom_point()
# 문제 1) geom_point를 사이즈 3으로 해서 출력하시오
iris_p + geom_point(size = 3)
# 문제 2) Species를 모양으로 구분해서 출력하시오
iris_p + geom_point(aes(shape = factor(Species)))
# 문제 3) Species로 화면을 분할해서 출력하시오
# iris_p + geom_point() + facet_wrap(~Species)
res <- iris_p + geom_point(aes(shape = Species, color = Species, size = 3))
res + facet_wrap(~Species, ncol=3)

ggplot(iris, aes(x=Petal.Width, fill=Species, color=Species)) + # 범주 지정
  geom_density(alpha=0.4, linewidth=1) + # 범주별 밀도함수(연속된 함수)
  scale_fill_brewer(palette="Set2")+scale_color_brewer(palette="Set2") +
  labs(title="붓꽃 종별 꽃잎 너비 밀도")+theme_classic()
# 누적 분포 함수
ggplot(iris, aes(x=Sepal.Length, color=Species)) +
  stat_ecdf(geom="step", linewidth=1.2) + # 누적 데이터, step : 계단형
  scale_color_brewer(palette="Set1")+
  labs(y="누적 확률", title="종별 ECDF")+theme_bw() # black / white (흰 배경)
# 회귀분석 (배기량과 연비)
ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(color = class)) + # 차종 타입별로
  geom_smooth(se = TRUE) + # 회귀분석 (비선형회귀) : 표준오차 - 신뢰구간
  labs(title = "연료효율")
# 범주형 변수로 정렬
ggplot(mpg, aes(x=reorder(class, hwy, median), y=hwy, fill=class)) +
  geom_violin(alpha=0.5, trim=FALSE) + # 데이터 분포, 좌우 대칭, 끝을 자름(데이터 있는데까지만)
  geom_boxplot(width=0.15, fill="white", alpha=0.9) + # 데이터 있는데까지만
  geom_jitter(width=0.08, alpha=0.3, size=1.5) +
  coord_flip() + scale_fill_viridis_d() + theme_minimal() # 행으로 표현
#
install.packages("gridExtra")
library(ggplot2)
library(gridExtra) # 화면 배치 라이브러리
g1=ggplot(mtcars, aes(x=qsec)) + geom_density(fill="slateblue")
g2=ggplot(mtcars, aes(x=drat, y=qsec, color=cyl)) + geom_point(size = 5) +
  theme(legend.position="none")
g3=ggplot(mtcars, aes(x=factor(cyl), y=qsec, fill=cyl)) + geom_boxplot() +
  theme(legend.position="none")
g4=ggplot(mtcars, aes(x=factor(cyl), fill=factor(cyl))) + geom_bar()
# 격자
grid.arrange(g1, g2, g3, g4, ncol=2, nrow=2)
# arrangeGrob(: Box화)
grid.arrange(g2, arrangeGrob(g3, g4, ncol=2), nrow=2)
grid.arrange(g1, g2, g3, nrow=3)
grid.arrange(g2, arrangeGrob(g3, g4, ncol=2), nrow=1)
grid.arrange(g2, arrangeGrob(g3, g4, nrow=2), nrow=1)

# 시각화 객체 + 테이블 데이터 객체
p1 <- ggplot(mtcars, aes(mpg, wt)) + geom_point()
p2 <- ggplot(mtcars, aes(mpg, cyl)) + geom_point()
grid.arrange(p1, p2, ncol = 2)
tbl <- tableGrob(head(mtcars))
grid.arrange(p1, tbl, ncol = 2)

# 문제
# 다음 데이터를 다차원으로 시각화하시오
fruit_data <- data.frame(
  과일 = c("사과", "배", "포도", "딸기", "수박", "참외", "키위", "귤"),
  당도 = c(12, 15, 18, 10, 14, 13, 11, 12),
  산도 = c(5, 2, 8, 7, 1, 2, 9, 6),
  가격 = c(1000, 3000, 2000, 1500, 8000, 2500, 1200, 800),
  판매량 = c(500, 200, 300, 450, 100, 250, 150, 600),
  계절 = c("가을", "가을", "여름", "봄", "여름", "여름", "겨울", "겨울")
)
# x축 : 당도
# y축 : 산도
# 색상 : 계절
# 크기 : 사이즈
# 모양 : shape로 다차원 시각화
ggplot(fruit_data, aes(x=당도, y=산도)) +
  geom_point(aes(color=계절, size=판매량, fill=계절, shape=과일),
             alpha=0.7, stroke=1.5) +
  labs(title="과일 특성 다차원 분석", 
       x="당도 (Sugar Content)",
       y="산도 (Acidity)") +
  theme_minimal()

plot(1:10, 1:10,
     xlab = expression(paste("배기량 (", cm^3, ")")),
     ylab = expression(frac(delta, theta)),
     main = expression(alpha + beta^2 >= sqrt(x)))

sprintf("%f", pi) # format 문자열
sprintf("%.3f", pi)
format(c("A", "BB", "CCC"), width = 5, justify = "centre")
head(USArrests)
(states = rownames(USArrests)) # 행이름만 -> 주이름
length(states)
grep(pattern = "k", x = states) # 탐색
class(grep(pattern = "k", x = states))
grep(pattern = "k", x = states, value = TRUE) # value=TRUE : 값으로 출력
grep(pattern = "[wW]", x = states, value = TRUE) # 선택
grep(pattern = "W", x = toupper(states), value = TRUE) # 문자열 함수             

# Anchor Sequences 의 의미
# \\d 숫자
# \\D 숫자 제외
# \\w 문자/숫자/언더바
# \\W 문자 제외
sub("\\$", "", "$Peace-Love")
sub("\\.", "", "Peace.Love")
sub("\\+", "", "Peace+Love")
sub("\\d", "_", "the dandelion war 2010") # 숫자면
gsub("\\d", "_", "the dandelion war 2010")
sub("\\D", "_", "the dandelion war 2010") # 숫자가 아니면
gsub("\\w", "_", "the dandelion war 2010")
