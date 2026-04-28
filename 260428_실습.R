# 정규분포 - 밀도함수 (연속적 데이터)
# rnorm(random), pnorm(distribution:누적확률), qnorm(quantile), dnorm(density:밀도)
# 분위수 한점에서의 확률은 0
rnorm(1, 64.5, 2.5) # 평균과 표준편차
rnorm(5, 64.5, 2.5) # 

(x <- rnorm(1000, 64.5, 2.5))
(x <- sort(x))
(d <- dnorm(x, 64.5, 2.5)) # 높이값 - 밀도함수값
# 전체합이 확률이 1이 되도록
hist(x, probability=TRUE, main="한국 남자들 몸무게 분포")
plot(x, d, main="한국남자들 몸무게 분포", xlab="몸무게")
curve(dnorm(x), -3, 3)
plot(density(rnorm(10000, 0, 1))) # 표준 정규분포

# 누적확률
pnorm(0) # 분위수 (가운데)
pnorm(1) # 표준편차 1배수
pnorm(1) - pnorm(-1) # 0.68
(pnorm(1) - pnorm(0)) * 2
(pnorm(2) - pnorm(0)) * 2 # 0.95
# 표준편차 +- 3의 범위의 확률을 구하시오
(pnorm(3) - pnorm(-3))
(pnorm(3) - pnorm(0)) * 2
# 정확하게 95%가 되도록 설계하시오
(pnorm(1.96) - pnorm(0)) * 2
# 문제
# 평균이 100이고 표준편차가 10인 정규분포에서 50이 나올 확률밀도 값은?
# 표준정규분포
# z점수 = (x-100) / 10
z1 <- (50-100) / 10
dnorm(z1, 0, 1)
dnorm(50, mean=100, sd=10)
# 평균이 90이고 표준편차가 12인 정규분포에서 98이 나올 확률밀도 값은?
z2 <- (98-90) / 12
dnorm(z2, 0, 1)
dnorm(98, mean=90, sd=12)

par(mfrow=c(1,1))
plot(seq(-3.2, 3.2, length=50), dnorm(seq(-3, 3, length=50), 0, 1), type="l", 
     xlab="", ylab="", ylim=c(0, 0.5))
      # 시작점 2개 (-3, -1), (3, -1) # 끝점 (-3, 1), (3, 1)
segments(x0=c(-3, 3), y0=c(-1, -1), x1=c(-3, 3), y1=c(1,1))
text(x=0, y=0.45, labels=expression("99.7% 가 표준편차 3배수 안에 3" ~ sigma))
      # 시작점 2개 (-2, 0.45), (2, 0.45) # 끝점 (-3, 0.45), (3, 0.45)
arrows(x0=c(-2,2), y=c(0.45, 0.45), x1=c(-3,3), y1=c(0.45, 0.45))
segments(x0=c(-2,2), y0=c(-1,-1), x1=c(-2,2), y1=c(0.4,0.4))
text(x=0, y=0.3, labels=expression("95.4% 가 표준편차 2배수 안에 2" ~ sigma))
arrows(x0=c(-1.5,1.5), y0=c(0.3,0.3), x1=c(-2,2), y1=c(0.3,0.3))
segments(x0=c(-1,1), y0=c(-1,-1), x1=c(-1,1), y1=c(0.25, 0.25))
text(x=0, y=0.15, labels=expression("68.3% 가 표준편차 1배수 안에 1" * sigma), cex=0.9)

# 범위 표현
# 정규분포
curve(dnorm(x,0,1), -4,4, xlab="z", ylab="f(z)") # 높이값, x축의 범위
z=seq(0,4,0.02)
lines(z,dnorm(z),type="h",col="grey")

# 문제
# -1.96 ~ 1.96 까지 라인으로 구분하시오
curve(dnorm(x,0,1), -4, 4, xlab="z", ylab="f(z)")
z=seq(-1.96, 1.96, 0.02)
lines(z,dnorm(z),type="h",col="grey")

# 표준오차 = 표준편차 / sqrt(n)
stderr = function(x) sd(x, na.rm=T) / sqrt(length(na.omit(x))) # 결측치 고려
# 기술통계 : 평균, 분산, 표준편차, 표준오차, 변동계수(표준편차/평균)
# 모집단 -> 표본을 뽑아서 분석
# 표본이 많아지면 모집단 => 평균이 안정화
# 표본이 많아지면 표준오차는 작아짐
# 비행기의 평균 비행시간은 120시간이고 비행시간은 정규분포
# 표준편차가 30시간
t9=rnorm(9, 120, 30)
t36=rnorm(36, 120, 30)
t100=rnorm(100, 120, 30)
t1000=rnorm(1000, 120, 30) # 표본 숫자를 늘림
mean(t9) # 평균이 120에 수렴
var(t9)
mean(t36)
var(t36)
mean(t100)
var(t100)
mean(t1000)
var(t1000)
stderr(t9) # 표준오차는 표본이 많아지면 점점 작아진다
stderr(t36)
stderr(t100)
stderr(t1000)
# 문제
# x ~ (300, 50^2) 인 정규분포에서 p(x >= 370)일 확률을 구하여라
# z점수 사용
(z3 <- (370-300) / 50)
(d1 <- 1 - pnorm(z3)) # 표준정규분포(평균:0, 표준편차:1)
# 이를 시각적으로 표현해 보시오
curve(dnorm(x, 0, 1), -4, 4, xlab="z", ylab="f(z)")
z=seq(z3, 4, 0.01)
lines(z, dnorm(z), type="h", col="grey")
points(z3, dnorm(z3))

# 문제
# 백열전구 수명이 1500시간의 평균값과 75시간의 표준편차로 정규적으로 분포되어 있다
# 1) 백열 전구가 1410시간보다 덜 오래갈 확률을 구하시오
(zpoint <- (1410-1500) / 75)
(d2 <- pnorm(zpoint))
(x <- rnorm(1000, 0, 1))
curve(dnorm(x, 0, 1), -4, 4, xlab="z", ylab="f(z)")
z <- seq(-4, z4, 0.01)
lines(z, dnorm(z), type="h", col="grey")
# 2) 백열 전구가 1563시간에서 1641시간 사이의 오래갈 확률은 얼마인가
(z5 <- (1563-1500) / 75)
(z6 <- (1641-1500) / 75)
(d3 <- pnorm(z6) - pnorm(z5))
curve(dnorm(x, 0, 1), -4, 4, xlab="z", ylab="f(z)")
z <- seq(z5, z6, 0.01)
lines(z, dnorm(z), type="h", col="grey")
# 3) 위의 구간을 정규분포 곡선으로 그려 그래프 위에 표시하시오

# 문제
# 우리나라에서 사육하고 있는 생후 18개월 이상된 황소의 무게는
# 평균이 500kg이고 표준편차가 50kg인 정규분포이다.
# 이제 우량 한우를 집중 육성 관리하기 위해서 무게가 무거운 순서대로
# 5%에 해당하는 황소를 선발하고자 한다.
# 그렇다면 무게가 몇 kg 이상인 황소를 선발해야 되는가
?qnorm
(qpoint <- qnorm(0.95, 0, 1)) # 표준정규분포
# (x-500) / 50 = qpoint
# 역변환
(kgpoint <- qpoint * 50 + 500)
# 시각화
(x <- rnorm(1000, 0, 1))
curve(dnorm(x, 0, 1), -4, 4, xlab="z", ylab="f(z)")
z=seq(qpoint, 4, 0.01)
lines(z, dnorm(z), type="h", col="grey")
points(qpoint, dnorm(qpoint))

# 적률
# 2차 중심 적률 : ((데이터 - 평균)^2) / (n - 1) = 분산
# 3차 중심 적률 : ((데이터 - 평균)^3) / (n - 1) = 왜도 : 대칭(0 : 정규분포)
# 4차 중심 적률 : ((데이터 - 평균)^4) / (n - 1) = 첨도 : 뾰족함(3 : 정규분포)
library(moments)
set.seed(1234)
data_normal <- rnorm(n=1000, mean=55, sd=4.5)
skewness(data_normal) # 왜도
kurtosis(data_normal) # 첨도

# 지수분포
data_skewed <- rexp(1000) # pexp, qexp 한쪽으로 치우친 지수 분포
data_outlier <- c(rnorm(995), 10, 15, 20, 30, 50)
skewness(data_skewed)
kurtosis(data_skewed)
skewness(data_outlier)
kurtosis(data_outlier)
library(ggplot2)
df <- data.frame(
  value = c(data_normal, data_skewed, data_outlier),
  type = rep(c("Normal", "Skewed", "Outlier"), each = 1000)
)
ggplot(df, aes(x = value)) +
  geom_histogram(bins = 30, fill = "skyblue") +
  facet_wrap(~type, scales = "free") +
  theme_minimal()
# 통계분석의 대상은 정규성, 독립성, 등분산성(분산이 일정)
# 비정규성 : 대표적 소득 (income : exp 형태) => 변환 (정규분포)
# log, sqrt 등으로 정규분포로 변환
install.packages("UsingR")
library(UsingR)
data(cfb)
head(cfb)
summary(cfb$INCOME) # 소득 데이터
hist(cfb$INCOME, breaks=500, req=TRUE) # 고액연봉자는 숫자 적음
cfb <- transform(cfb, INCOME_log=log(INCOME + 1)) # log0은 무한대
hist(cfb$INCOME_log, breaks=500, freq=TRUE)
cfb <- transform(cfb, INCOME_sqrt=sqrt(INCOME + 1))
hist(cfb$INCOME_sqrt, breaks=500, freq=TRUE)
par(mfrow=c(1,3))
# 정규성확인 - 시각화를 이용
qqnorm(cfb$INCOME, main="INCOME의 Q-Q도")
qqline(cfb$INCOME)
qqnorm(cfb$INCOME_log, main="INCOM_log")
qqline(cfb$INCOME_log)
qqnorm(cfb$INCOME_sqrt, main="INCOM_sqrt")
qqline(cfb$INCOME_sqrt)
par(mfrow=c(1,1))

# 카이제곱 분석
# 자유도에 따라 분포가 달라짐
# 자유도가 커지면 중심이 오른쪽으로 이동
# chisq 분포의 매개변수는 데이터와 자유도
# density dt df
x <- seq(1, 10, .1)
par(mfrow=c(2,3))
plot(x, dchisq(x, 20), type="l")
plot(x, dchisq(x, 9), type="l")
plot(x, dchisq(x, 7), type="l")
plot(x, dchisq(x, 5), type="l")
plot(x, dchisq(x, 3), type="l")
plot(x, dchisq(x, 1), type="l")

# 범주별로 교차표를 작성
# 경우의 수 일치
x=c(1,2,3,4,5,6)
y=c(7,8,9,10,11,12)
z=10:15
(ta = table(x))
(tb = table(x,y))
(tc = table(x,y,z)) # z은 페이지

df <- data.frame(
  Gender=c("남", "남", "여", "여", "남"),
  Fruit=c("사과", "포도", "사과", "포도", "사과")
)
table(df$Gender, df$Fruit)
xtabs(~ Gender + Fruit, data=df) # formula
ftable(xtabs(~ Gender + Fruit, data=df)) # 계층적 인덱스

(d <- data.frame(x=c("1", "2", "2", "1"), y=c("A", "B", "A", "B"),
                 num=c(3,5,8,7)))
# 확률
class(d)
str(d)
(xt = xtabs(num ~ x+y, data=d))
class(xt)
margin.table(xt,1) # 행별로 주변확률, 한계확률
margin.table(xt,2) # 열별로
margin.table(xt)
prop.table(xt,1)
prop.table(xt,2)
prop.table(xt)
addmargins(xt) # 행과 열의 한계확률

Titanic
str(Titanic)
class(Titanic)
# 4차원 데이터
margin.table(Titanic,1)
margin.table(Titanic,2)
margin.table(Titanic,3)
margin.table(Titanic,4)
barplot(margin.table(Titanic, 1))
(titanic.data = xtabs(~Survived + Sex, data=Titanic))
ftable(Titanic, row.vars=1:3) # pandas에서 지원
ftable(Titanic, row.vars=1:2, col.vars="Survived")
# 문제제
# ftable 성별로 생존여부를 확인하시오
ftable(Titanic, row.vars="Sex", col.vars="Survived")
xtabs(Freq ~ Sex+Survived, data=Titanic) # Freq : frequence
# 나이별로 선실등급의 구성을 확인하시오
ftable(Titanic, row.vars="Age", col.vars="Class")
xtabs(Freq ~ Age + Class, data=Titanic)
# 확률 (이산적 데이터)
fruit_table <- matrix(
  c(9, 1, 15, 5),
  nrow=2,
  byrow=TRUE
)
rownames(fruit_table) <- c("남", "여")
colnames(fruit_table) <- c("사과", "포도")
fruit_table
(total <- sum(fruit_table))
# 과일별 확률
(p_apple <- sum(fruit_table[,"사과"]) / total)
(p_graph <- sum(fruit_table[,"포도"]) / total)
# 성별 확률
(p_male <- sum(fruit_table["남",]) / total)
(p_female <- sum(fruit_table["여",]) / total)
# 결합확률
# 남자이면서 사과를 좋아할 확률
(p_male_apple <- fruit_table["남","사과"] / total)
(p_female_graph <- fruit_table["여","포도"] / total)
# 조건부 확률
# 남자 중에서 사과를 좋아할 확률
(p_apple_given <- fruit_table["남","사과"] / sum(fruit_table["남",]))
# 문제
# 사과를 좋아하는 사람 중에서 남자일 확률
(p_male_given <- fruit_table["남", "사과"] / sum(fruit_table[,"사과"]))

# 동일성검증
data <- textConnection(
  "사이다종류 관측도수
  1 14
  2 32
  3 17
  4 9
  5 18"
)
(x <- read.table(data, header=T))
class(x)
# 기대 도수
(14+32+17+9+18)/5
# 카이제곱분석의 검정통계량 = sigma((관측도수 - 기대도수)^2) / 기대도수
res <- (14-18)^2 + (32-18)^2 + (17-18)^2 + (9-18)^2 + (18-18)^2
re <- (res / 18) # 검정통계량
dchisq(re, 4) # 밀도 함수의 높이값
pchisq(re, 4) # 확률
x$관측도수
chisq.test(x$관측도수) # X-Squared = 16.333 / p-value = 0.002603
# 귀무가설 : 선호도 차이가 없다.
# 대립가설 : 선호도 차이가 있다.
# p-value = 0.002603
# 귀무가설을 기각하고 대립가설을 채택한다.
# 카이제곱분석 - 독립성 검증
# install.packages("gmodels")
library(gmodels)
library(MASS)
str(survey)
sum(is.na(survey))
sapply(survey, function(x) sum(is.na(x)))
survey <- na.omit(survey)
head(survey[c("Sex", "Exer")])
unique(survey[c("Sex", "Exer")])
unique(survey[c("Sex")])
unique(survey[c("Exer")]) # Some(약간), None(0), Freq(자주)
(xt=xtabs(~ Sex+Exer, data=survey)) # 교차표
chisq.test(xtabs(~ Sex+Exer, data=survey))
CrossTable(survey$Sex, survey$Exer, expected=TRUE) # 기대도수도 출력
# 가설 : 성별 운동량의 차이
# 귀무가설 : 성별로 운동량 차이가 없다.
# 대립가설 : 성별로 운동량 차이가 있다.
# 검정통계량 : 4.1585
# p-value : 0.125
# 결론 : 귀무가설 채택

# 독립성 검증
xtabs(~ W.Hnd+Clap, data=survey)
chisq.test(xtabs(~ W.Hnd + Clap, data=survey))
# 귀무가설 : 주로 쓰는 손과 박수치는 손은 관계가 없다.
cot=85+14+69
84 * (85/cot) # 42.5
84 * (14/cot) # 7
84 * (69/cot) # 34.5
# 대립가설 : 주로 쓰는 손과 박수치는 손은 관계가 있다.
# 검정통계량 : 9.1166
# p-value : 0.01048
# 결론 : 귀무가설을 기각하고 대립가설을 채택 : 주로 쓰는 손과 박수치는 손은 관계가 있다.
fisher.test(xtabs(~ W.Hnd + Clap, data=survey))
# 기대도수가 5미만인 셀의 숫자가 전체의 25%를 넘어서면 fisher.test를 해야 정확하다
# 소량의 데이터에 적용

# 적합성 검증
table(survey$W.Hnd)
chisq.test(table(survey$W.Hnd), p=c(.3, .7))
