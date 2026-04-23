# 기술 통계
x <- c(10, 20, 30, 40, 50)
# 합계 - 매출액
sum(x)
# 평균 - 대표
mean(x)
# 분산 - 변동성
var(x)
# 표준편차 - 변동
sd(x)
# 표준오차 - 신뢰
sd(x) / sqrt(length(x))
# 변동계수 - 비교
sd(x) / mean(x)

str(mtcars)
# 연비, 실린더수, 배기량, 마력, 후륜비, 무게, 100km 도달 시간
# 엔진(기통), am(변속기), 기어수, 기화기수
mtcars$cyl <- factor(mtcars$cyl)
mtcars$vs <- factor(mtcars$vs)
mtcars$am <- factor(mtcars$am)
mtcars$gear <- factor(mtcars$gear)
mtcars$carb <- factor(mtcars$carb)
summary(mtcars$mpg) # 자동차 연비 분석
mean(mtcars$mpg) # 평균
median(mtcars$mpg) # 중위수
var(mtcars$mpg) # 분산
sd(mtcars$mpg) # 표준편차
range(mtcars$mpg) # 범위 10.4 33.9
quantile(mtcars$mpg, probs=c(0.1, 0.25, 0.5, 0.75, 0.9)) # 100분위수
install.packages("moments")
library(moments)
skewness(mtcars$mpg) # 정규분포 왜도의 기준은 0 : 좌우 대칭 여부
kurtosis(mtcars$mpg) # 첨도의 기준값은 3 : 뾰족한 정도

# 자동 구간 범주화하고 도수 분포표
hist(mtcars$mpg,
     breaks = 10,
     main = "MPG 분포",
     xlab = "mpg",
     col = "lightblue",
     border = "white",
     freq = FALSE
     )
lines(density(mtcars$mpg), col = "red", lwd = 2)
# 밀도함수 - 보간법을 적용 (없는 부분은 보간법으로 채움)
# spline 보간법

iris
head(iris)
tail(iris)
dim(iris) # 차수
length(iris) # 데이터프레임 : 열 개수 / 벡터 : 요소 수
NROW(iris) # 행 개수
ncol(iris) # 열 개수
names(iris) # 열 이름
summary(iris) # 통계 요약
str(iris) # 차수와 변수 설명
class(iris) # 어떤 클래스
# 뒤에 있는 함수를 열 별로 적용 - 데이터 프레임은 열 기준
# 열 내 동질 / 열 간 이질
sapply(iris, class)
boxplot(iris) # 데이터가 있는데까지만 표현


data(airquality)
head(airquality)
str(airquality)
is.na(airquality$Ozone)
sum(is.na(airquality))
colSums(is.na(airquality))
colMeans(is.na(airquality))
barplot(colSums(is.na(airquality)),
        main = "열별 결측값 계수",
        col = "steelblue",
        ylab = "NA 개수"
          )
aq_complete <- airquality[complete.cases(airquality),]
nrow(aq_complete) # 111 / 153

aq_omit <- na.omit(airquality)
nrow(aq_omit)
# identical(aq_complete, aq_omit)
# ?identical

aq_filled <- airquality # 백업
# 중위수로 결측치 값 채움
aq_filled$Ozone[is.na(aq_filled$Ozone)] <- median(aq_filled$Ozone, na.rm = TRUE)
# 평균으로 결측치 값 채움
aq_filled$Solar.R[is.na(aq_filled$Solar.R)] <- mean(aq_filled$Solar.R, na.rm = TRUE)
sum(is.na(aq_filled))

install.packages("mlbench")
library(mlbench)
data("BostonHousing", package="mlbench")
str(BostonHousing)
??BostonHousing
# na를 확인하고 omit 하시오
str(BostonHousing)
dim(BostonHousing)
sum(is.na(BostonHousing))
# 난수 - 의사난수(이미 정해져 있음)
set.seed(100) # 시작점을 변경
BostonHousing[sample(1:nrow(BostonHousing), 40), "rad"] <- NA
BostonHousing[sample(1:nrow(BostonHousing), 40), "ptratio"] <- NA
sum(is.na(BostonHousing))

sapply(BostonHousing, function(x) sum(is.na(x)))
# 종속변수 ~ formula + 독립변수수
lm(medv ~ ptratio + rad, data=BostonHousing, na.action=na.omit)
# 선형회귀식
medv <- -1.7691 * ptratio - 0.2032 * rad + 60.1767
BostonHousing[1, ]
(medv <- -1.7691 * 15.3 - 0.2032 * 1 + 60.1767)
error = 30.62705 - 24

install.packages("Hmisc")
library(Hmisc)
impute(BostonHOusing$rad, mean)
BostonHousing$rad <- impute(BostonHousing$rad, median) # 수정 후 대입
sapply(BostonHousing, function(x) sum(is.na(X)))

install.packages("ggplot2")
install.packages("MASS")
library(ggplot2)
library(MASS)
str(Cars93)
?Cars93
hist(Cars93$MPG.highway)
disc_1 <- Cars93[, c("Model", "MPG.highway")] # 데이터 Subset
head(disc_1)
within(Cars93, {MPG.highway >= 20 & MPG.highway < 25})
range(disc_1["MPG.highway"])
# 구간 범주화
disc_1 <- within(disc_1, {
  MPG.highway_cd = character(0) # 초기화
  MPG.highway_cd[ MPG.highway >= 20 & MPG.highway < 25] = "20~25" #문자열
  MPG.highway_cd[ MPG.highway >= 25 & MPG.highway < 30] = "25~30"
  MPG.highway_cd[ MPG.highway >= 30 & MPG.highway < 35] = "30~35"
  MPG.highway_cd[ MPG.highway >= 35 & MPG.highway < 40] = "35~40"
  MPG.highway_cd[ MPG.highway >= 40 & MPG.highway < 45] = "40~45"
  MPG.highway_cd[ MPG.highway >= 45 & MPG.highway <= 50] = "45~50"
  # 범주화
  MPG.highway_cd = factor(MPG.highway_cd, level=c("20~25", "25~30", "30~35", "35~40", "40~45", "45~50"))
})
disc_1
table(disc_1$MPG.highway_cd) # 도수 분포표
tapply(disc_1$MPG.highway, disc_1$MPG.highway_cd, mean)
tapply(disc_1$MPG.highway, disc_1$MPG.highway_cd, sd)
ggplot(disc_1, aes(x=MPG.highway_cd, fill=MPG.highway_cd)) + geom_bar()
ggplot(disc_1, aes(x=MPG.highway_cd, fill=MPG.highway_cd)) + geom_dotplot(binwidth = 0.3)

# one-hot-encoding : 범주형 데이터 정규화
cust_id <- c("대한", "민국", "만세", "영원", "무궁", "발전", "행복")
cust_id <- factor(cust_id)
age <- c(25, 45, 31, 30, 49, 53, 27) # 구간 범주화 -> 4가지
cust_profile <- data.frame(cust_id, age, stringAsFactors = F)
cust_profile
# 파생변수
# 회귀 분석 -> 정규화 (0 ~ 1, -1 ~ 1) # 범주형 변수의 정규화
cust_profile <- transform(cust_profile, # 파생변수
                          # 3항 연산자
                          # 변수 4가지를 생성 (0과 1로 데이터 표현)
                          age_20 = ifelse(age >= 20 & age < 30, 1, 0),
                          age_30 = ifelse(age >= 30 & age < 40, 1, 0),
                          age_40 = ifelse(age >= 40 & age < 50, 1, 0),
                          age_50 = ifelse(age >= 50 & age < 60, 1, 0)
                          )
cust_profile

##### 정규화
# Z점수 정규화
data(mtcars)
std_scale <- function(x) {
  (x - mean(x)) / sd(x)
}
std_scale(mtcars$mpg)

(z_mpg <- scale(mtcars$mpg))
typeof(z_mpg)
class(z_mpg)
round(mean(z_mpg), 10)
round(sd(z_mpg), 10)

scaled_mtcars <- as.data.frame(scale(mtcars))
round(colMeans(scaled_mtcars), 10)

# minmax 정규화 : 0 ~ 1 사이 값으로 정규화
minmax <- function(x) {
  (x-min(x)) / (max(x) - min(x))
}

(mtcars$mpg_norm <- minmax(mtcars$mpg))
range(mtcars$mpg_norm)

iris_norm <- as.data.frame(lapply(iris[, 1:4], minmax)) # 열에 적용
apply(iris_norm, 2, range) #check : 행과 열로 선택적 적용이 가능

# robust : 이상치에 강건하다
robust_scale = function(x) {
  (x - median(x)) / IQR(x) # (quantile(x, 0.75) - quantile(x, 0.25))
}
(robust_mpg <- robust_scale(mtcars$mpg))

# 문제
# iris 데이터에 대해서 min_max 정규화를 적용한 data.frame으로 작성하시오
# 정규화는 수치데이터에 적용
str(iris)
minmax <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}
new_iris <- as.data.frame(apply(iris[, 1:4], 2, minmax))
new_iris$Species <- iris$Species
head(new_iris)

# 이상 탐지
data(mtcars)
Q1 <- quantile(mtcars$mpg, 0.25) # 1사분위수
Q3 <- quantile(mtcars$mpg, 0.75) # 3사분위수
IQR_val <- IQR(mtcars$mpg)
lower <- Q1 - 1.5 * IQR_val
upper <- Q3 + 1.5 * IQR_val
cat("하한 :", lower, " 상한 :", upper, "\n")
outliers_iqr <- mtcars$mpg[mtcars$mpg < lower | mtcars$mpg > upper]
cat("IQR 이상치 :", outliers_iqr, "\n")

# 3-sigma 방법
# 정규분포 1: 68%, 표준편차 2배수: 95.4%, 표준편차 3배수: 99.7%
mu <- mean(mtcars$mpg)
sig <- sd(mtcars$mpg)
outliers_3s <- mtcars$mpg[abs(mtcars$mpg - mu) > 3 * sig]
cat("3σ 이상치 :", outliers_3s, "\n")

# 데이터 5가지 요약값
# 최소값, Q1, 중앙값, Q3, 최대값
fivenum(iris[,1], na.rm = TRUE)

mt_five <- fivenum(mtcars$cyl, na.rm = TRUE)
mt_IQR <- mt_five[4] - mt_five[2]
mt_lower <- mt_five[2] - 1.5 * mt_IQR
mt_upper <- mt_five[4] + 1.5 * mt_IQR
cat("하한 :", mt_lower, " 상한 :", mt_upper, "\n")
outliers_mt_iqr <- mtcars$cyl[mtcars$cyl < mt_lower | mtcars$cyl > mt_upper]
cat("IQR 이상치 :", outliers_mt_iqr, "\n")

state_table <- data.frame(key=c("SE", "DJ", "DG", "SH", "QD"),
                          name=c("서울", "대전", "대구", "상해", "칭따오"),
                          country=c("한국", "한국", "한국", "중국", "중국"))
state_table

month_table <- data.frame(key=1:12, desc=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul",
                                          "Aug", "Sep", "Oct", "Nov", "Dec"),
                         quarter=c("Q1", "Q1", "Q1", "Q2", "Q2", "Q2", "Q3", "Q3", "Q3", "Q4", "Q4", "Q4"))
month_table

prod_table <- data.frame(key=c("Printer", "Tablet", "Laptop"), price=c(225, 570, 1120))

prod_table
(prod_table1 <- data.frame(Printer=225, Tablet=570, Laptop=1120))
(prod_table1 <- t(prod_table1))

# 복원 추출
gen_sales <- function(no_of_recs) {
  loc <- sample(state_table$key, size=no_of_recs, replace=T, prob=c(2,2,1,1,1))
  time_month <- sample(month_table$key, no_of_recs, replace=T)
  time_year <- sample(c(2012, 2013), no_of_recs, replace=T)
  prod <- sample(prod_table$key, no_of_recs, replace=T, prob=c(1,3,2))
  unit <- sample(c(1,2), no_of_recs, replace=T, prob=c(10, 3))
  amount <- unit*prod_table1[prod,1] # 제품 가격
  sales <- data.frame(month=time_month,
                      year=time_year,
                      loc=loc,
                      prod=prod,
                      unit=unit,
                      amount=amount)
  # sort는 직접 값을 정렬 / order는 순서를 정렬
  sales <- sales[order(sales$year, sales$month),] # 년월 별 정렬
  row.names(sales) <- NULL
  return(sales)
}

(sales_fact <- gen_sales(500))
str(sales_fact)
head(sales_fact)
tail(sales_fact)
dim(sales_fact) # 500 x 6

(revenue_cube <- tapply(sales_fact$amount, # group by
                        sales_fact[,c("prod", "month", "year", "loc")], # group
                        FUN=function(x){
                          return (sum(x))
                        }))
dimnames(revenue_cube)
revenue_cube

tapply(sales_fact$amount,
       sales_fact[, c("prod")],
       FUN=function(x){
         return(sum(x))
       }
       )

# 문제
# 지역별 판매액을 확인하시오
region_fact <- tapply(sales_fact$amount,
                      sales_fact[, c("loc")],
                      FUN=function(x) {
                        return (sum(x))
                      })
region_fact
# 연도별 지역별 판매액을 확인하시오
year_region_fact <- tapply(sales_fact$amount,
                           sales_fact[, c("year", "loc")],
                           FUN=function(x) {
                             return (sum(x))
                           })
year_region_fact
# 지역별, 월별 판매액을 확인하시오
region_month_fact <- tapply(sales_fact$amount,
                            sales_fact[, c("loc", "month")],
                            FUN=function(x) {
                              return (sum(x))
                            })
region_month_fact

# 문제
str(sales_fact)
head(sales_fact)
all_fact <- tapply(sales_fact$amount,
                   sales_fact[, c("prod", "month", "year", "loc")],
                   FUN=function(x) {
                     return (sum(x))
                   })
all_fact
# 2013년도 서울 지역 판매액을 출력하시오
year_region_fact <- tapply(sales_fact$amount,
                     sales_fact[, c("year", "loc")],
                     FUN=function(x) {
                       return (sum(x))
                     })
year_region_fact["2013","SE"]
# 대전지역 12월 판매액을 출력하시오
month_region_fact <- tapply(sales_fact$amount,
                            sales_fact[, c("month", "loc")],
                            FUN=function(x) {
                              return (sum(x))
                            })
month_region_fact["12", "DJ"]
# 서울지역 4분기 판매현황을 출력하시오
all_fact[,c("10", "11", "12"),,"SE"]
# Tablet 제품에 대한 월별 연도별 지역별 현황을 출력하시오
all_fact["Tablet",,,]
# 2012년도 1월의 판매현황
all_fact[,"1","2012",]
(sales201201 <- data.frame(all_fact, year="2012", month="1"))
# 서울에서 Printer의 2013년도 총 판매액
seoul_printer_13_fact <- all_fact["Printer",,"2013","SE"]
sum(seoul_printer_13_fact, na.rm = TRUE)
# 모든 지역에서 Tablet의 2012년 총 판매량
tablet_2012_fact <- all_fact["Tablet", ,"2012",]
sum(tablet_2012_fact, na.rm = TRUE)
# 대구에서 가장 많이 팔린 상품과 그 판매 금액
# daegu_fact <- all_fact[,,,"DG"]
# daegu_max <- max(daegu_fact, na.rm=TRUE)
# daegu_most <- which(daegu_fact == daegu_max, arr.ind = TRUE)
# daegu_most
(unit_table <- tapply(sales_fact$unit, sales_fact$loc, sum))
(amount_table <- tapply(sales_fact$amount, sales_fact$loc, sum))
result <- data.frame(
  loc=names(unit_table),
  total_unit = unit_table,
  total_amount = amount_table
)
result
result[which.max(result$total_unit),]

# sales_fact 데이터에서 판매액(amount)의 총합, 평균, 중앙값, 표준편차, 표준오차, 변동계수를 구하시오
sum(sales_fact$amount)
mean(sales_fact$amount)
median(sales_fact$amount)
sd(sales_fact$amount)
se <- function(x) {
  sd(sales_fact$amount) / length(sales_fact$amount)
}
se(sales_fact$amount)
cv <- function(x) {
  sd(sales_fact$amount) / mean(sales_fact$amount)
}
cv(sales_fact$amount)
