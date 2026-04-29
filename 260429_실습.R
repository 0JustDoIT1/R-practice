# 평균분석 (One-sample t-test)
# 평균차이가 있는가
x = c(65, 78, 88, 55, 48, 95, 66, 57, 79, 81)
t.test(x, mu=75) # mu=75 모집단의 평균
# t검정통계량 : t = -0.78303
# df = 9
# p-value = 0.4537
# 귀무가설 : 평균차이가 없다
# 대립가설 : 평균차이가 있다
# 귀무가설을 기각할 수 없다
# 표준오차 = 표준편차 / sqrt(n)
(sem <- sd(x) / sqrt(length(x)))
# 신뢰구간
# c(mean(x) - 1.96 * sem, mean(x) + 1.96 * sem) # 1.96은 정규분포 신뢰 구간 (95%)
# t분포 사용(자유도 따라서)
(t_crit <- qt(0.975, df=length(x)-1))
c(mean(x) - t_crit * sem,
  mean(x) + t_crit * sem)

# 검정 통계량 = 평균차 / 표준오차
x_bar <- mean(x) # 표본의 평균
mu_0 <- 75 # 모집단의 평균
s <- sd(x) # 표준편차
n <- length(x) # n
(x_bar - mu_0) / (s / sqrt(n))

# 폐암발생률 평균이 20이라는 가설을 검증하라
# 신뢰수준은 0.01
(lung <- runif(20, min=19, max=20)) # 균등 분포
t.test(lung, mu=20, conf.level=.99)
# 귀무가설 : 폐암발생률 평균이 20이다
# 대립가설 : 폐암발생률 평균은 20이 아닌다
# 검정 통계량 : t = -5.9646
# p-value : 9.682e-06
# 귀무가설을 기각하고 대립가설을 채택한다

# 이항분포 (성공, 실패)
binom.test(c(125, 9), p=0.7)
# 귀무가설 : 비율이 같을 것이다 (차이가 없다)
# 대립가설 : 비율이 0.7이 아닐 것이다
# p-value = 4.204e-11
# 귀무가설을 기각하고 대립가설을 채택한다

# 비율검정
# A회사 직장인 500명과 B회사 직장인 600명을 대상으로 조사한 결과
# A회사 직장인의 흡연율은 33%, B회사의 직장인의 흡연율은 41%로 나타남
# 그러면 A회사와 B회사의 직장인의 흡연율에는 차이가
# 있다고 할 수 있는지 유의수준 5%에서 검정하시오
# 문제 A회사가 B회사보다 흡연율이 큰 것을 검정하라(단측검정)
# 귀무가설 : A회사와 B회사의 흡연율에는 차이가 없다.
# 대립가설 : A회사와 B회사의 흡연율에는 차이가 있다.
prop <- c(0.33, 0.41)
n <- c(500, 600)
(x <- prop*n) # 실제 인원 수
prop.test(x=x,
          n=n,
          alternative=c("two.sided"),
          conf.level=0.95)
# X-squared = 7.1203
# p-value = 0.007622
# 귀무가설을 기각하고 대립가설을 채택 (유의미하게)

# 맥나마 검정
# 사건 전후에 관찰자의 성향이 어떻게 달라지는지를 보는 검정
# 투표권이 있는 나이의 미국인 1600명을 대해 대통령 지지율을 조사를 한 것으로
# 1차 조사 2차 조사는 한달 간격으로 수행
# 전체 인원 수는 같고 어떤 변화가 있었는지 확인하시오
Performance <-
  matrix(c(794, 86, 150, 570),
         nrow=2,
         dimnames=list("1st Survey" = c("Approve", "Disapprove"),
                       "2nd Survey" = c("Approve", "Disapprove")))
Performance
mcnemar.test(Performance)
# 귀무가설은 Approve, Disapprove 차이가 없다
# 대립가설은 Approve, Disapprove 차이가 있다
# chi-squared = 16.818
# p-value = 4.115e-05
# 귀무가설을 기각하고 대립가설을 채택 => 변화가 있다

library(ggplot2)
ggplot(data.frame(x=c(-3,3)), aes(x=x)) +
  stat_function(fun=dnorm, colour="blue", size=1) +
  stat_function(fun=stats::dt, args=list(df=3), colour="red", size=2) +
  stat_function(fun=stats::dt, args=list(df=1), colour="yellow", size=3) +
  annotate("segment", x=1.5, xend=2, y=0.4, yend=0.4, colour="blue", size=1) +
  annotate("segment", x=1.5, xend=2, y=0.37, yend=0.37, colour="red", size=2) +
  annotate("segment", x=1.5, xend=2, y=0.34, yend=0.34, colour="yellow", size=3) +
  annotate("text", x=2.4, y=0.4, label="N(0,1)") +
  annotate("text", x=2.4, y=0.37, label="t(3)") +
  annotate("text", x=2.4, y=0.34, label="t(1)") +
  ggtitle("정규분포와 t분포")

str(sleep) # 초과 수면량 (수면제 먹은 집단, 먹지 않은 집단)
attach(sleep)
plot(extra ~ group, data = sleep)
var.test(extra[group == 1], extra[group == 2], data = sleep)
# var.test의 귀무가설 : 등분산
# var.test의 대립가설 : 이분산(등분산이 아니다)
# p-value = 0.7427 => 등분산
with(sleep, t.test(extra[group == 1], extra[group == 2], var.equal = T))
detach(sleep)
# 귀무가설 : 두 집단의 평균이 동일하다
# 대립가설 : 두 집단의 평균이 차이가 있다
# 검정 통계량 t = -1.8608
# 자유도 df = 18
# p-value = 0.07919
# 귀무가설을 기각할 수 없다
# 평균이 동일하기 때문에 수면제의 약효가 없다

# 문제
# 남학생과 여학생의 영어 성적
boy <- c(46,47,58,47,27,58,56,26,47,25)
girl <- c(78,57,31,28,67,77,36,57,36,57)
# 남학생과 여학생의 평균 차이가 있는지 검정하시오
# 귀무가설 : 평균차이가 없다
# 대립가설 : 평균차이가 있다
var.test(boy, girl)
# p-value = 0.306
# 등분산
t.test(boy, girl, var.equal = T)
# p-value = 0.2427
# 귀무가설을 기각할 수 없다 => 평균차이가 없다

# 정규분포 테스트
shapiro.test(rnorm(1000))
# 귀무가설 : 정규분포이다
# 대립가설 : 정규분포가 아니다
# p-value : 0.2389
# 귀무가설을 기각하지 못한다 (정규분포이다)
set.seed(450)
x <- runif(300, min=2, max=4)
shapiro.test(x)
# p-value : 8.497e-09
# 귀무가설을 기각하고 대립가설을 채택 => 정규분포가 아니다
# 정규분포 테스트 여부에 따라
# 정규분포이면 t.test
# 정규분포가 아니면 wilcox.test

x <- rnorm(1000, 5.0, 0.5)
mean(x)
t.test(x, mu=5.2, alter="two.sided", conf.level=0.95) # 양측검정
# 귀무가설 : 데이터 평균이 5.2와 같다
# 대립가설 : 데이터 평균이 5.2와 다르다
(result <- t.test(x, mu=5.2, alter="greater", conf.level=0.95)) # 오른쪽 단측
# 귀무가설 : 데이터 x가 5.2보다 작다
# 대립가설 : 데이터 x가 5.2보다 크다
(result <- t.test(x, mu=5.2, alter="less", conf.level=0.95)) # 왼쪽 단측
# 귀무가설 : 데이터 x가 5.2보다 크다
# 대립가설 : 데이터 x가 5.2보다 작다
# paired인 경우 (표본이 동일한 경우 : 같은 표본)
# 한번은 수면제, 한번은 일상 : 동일 표본을 대상으로 관측
with(sleep, t.test(extra[group==1], extra[group==2], paired=TRUE))

# 문제
# iris 데이터의 Sepal.Width와 Sepal.Length의 등분산성을 테스트하시오
var.test(iris$Sepal.Width, iris$Sepal.Length)
# 귀무가설 : 등분산이다
# 대립가설 : 등분산이 아니다
# p-value = 3.595e-14
# 귀무가설을 기각하고 대립가설 채택 : 등분산이 아니다

x1 <- c(51.4, 58.0, 45.5, 55.5, 52.3, 50.9, 52.7, 50.3, 53.8, 53.1)
x2 <- c(50.1, 51.5, 45.9, 53.1, 51.8, 50.3, 52.0, 49.9, 52.5, 53.0)
# 문제
# 다음은 당뇨병 환자 10명을 선발해서 짝을 이루어 위약과 신약을
# 투여했을 때의 혈당 데이터이다
# 이 때 혈당 차이가 유의미한 차이가 있는지 검정하시오
# 유의 수준 5%에서 검정하시오
# 정규분포인지, paired = TRUE, 등분산인지, 양측검정/단측검정
var.test(x1, x2)
# x1이 위약, x2가 신약 # 신약이 좋다
t.test(x1, x2,
       alternative=c("greater"),
       paired=TRUE,
       var.equal=T,
       conf.level=0.95)
shapiro.test(x1) # p-value = 0.7659 : 정규분포이다
shapiro.test(x2) # p-value = 0.04861 : 정규분포가 아니다
wilcox.test(x1, x2,
            alternative=c("greater"),
            paired=TRUE,
            conf.int=F, # 신뢰구간은 출력하지 말 것
            var.equal=T,
            exact=FALSE, # tie가 있어 정확한 p값을 계산할 수 없습니다
            conf.level=0.95)
# V = 52.5
# p-value = 0.006201
# 귀무가설 : x1이 x2보다 작다
# 대립가설 : x1이 x2보다 크다
# 귀무가설을 기각하고 대립가설 채택 : 약효가 있다
par(mfrow=c(1,1))
boxplot(x1, x2,
        names=c("Placebo", "Drug"),
        col=c("lightgray", "lightblue"),
        main="혈당 비교")
d= x1 - x2
plot(d, type="b", pch=19,
     main="개별 환자 차이 (위약 - 신약)",
     ylab="차이값")
abline(h=0, col="red", lty=2)

# 문제
# MASS 패키지에서 내장된 Cars93 데이터프레임의 가격(Price)와 생산국가
# (Origin) 데이터에서 생산국이 USA vs non-USA 2개의 그룹에 대해서 차 가격의
# 평균차이가 있는지 5% 유의 수준에서 검정해 보시오
# 다음과 같은 형식으로 보고서를 제출하시오

# 1) 가설검정
    # 귀무가설 : 생산국인 USA와 non-USA 그룹 간에 가격 차이는 없다
    # 대립가설 : 생산국인 USA와 non-USA 그룹 간에 가격 차이는 있다
# 2) 연구환경
    # 두 집단 (USA, non-USA)
    # 서로 독립 표본
    # 연속형 데이터 (price)
# 3) 유의수준 : 0.05
# 4) 분석방법
    # 4-1) shapiro.test : 각 그룹 데이터의 정규분포 검증
    # 4-2) wilcox.test (두 그룹 데이터 모두 정규분포가 아님)
    # 4-3) 차이만 비교하니까 양측검정
# 5) 검정통계량 : W = 1024.5
# 6) 유의 확률(p-value) : p-value = 0.6724
# 7) 결과해석
    # 귀무가설을 기각할 수 없다 => 생산국인 USA와 non-USA 그룹 간에 가격 차이는 없다

library(MASS)
data("Cars93")

# 내 풀이
# price <- Cars93$Price
# origin <- Cars93$Origin
# head(price)
# head(origin)
# 
# usa_price <- price[origin=="USA"]
# nusa_price <- price[origin=="non-USA"]
# 
# shapiro.test(usa_price) # p-value = 0.0002006 : 정규분포 아님 -> wilcox.test
# shapiro.test(nusa_price) # p-value = 0.0002036 : 정규분포 아님 -> wilcox.test
# 
# result <- wilcox.test(usa_price, nusa_price,
#             alternative = "two.sided",
#             exact=FALSE,
#             conf.level = 0.95)
# # W = 1024.5
# # p-value = 0.6724

# 강사님 풀이
str(Cars93)
sum(is.na(Cars93))
Cars93 <- na.omit(Cars93)
str(Cars93)
Cars93$Price
class(Cars93$Origin)
levels(Cars93$Origin)
table(Cars93$Origin)
with(Cars93, tapply(Price, Origin, summary))
boxplot( Price ~ Origin,
         data = Cars93,
         main = "원산지별 가격",
         xlab = "원산지",
         ylab = "가격"
         
)

with(Cars93, tapply(Price, Origin, shapiro.test))
# 비모수인 경우는 등분산성 테스트 안함
result <- wilcox.test(Price ~ Origin,
                      data = Cars93,
                      alternative = c("two.sided"),
                      exact = F,
                      conf.level = 0.95
                      
)

result$statistic # 검정 통계량
result$p.value # 확률값
result$method # 분석 방법

### modern 한 방법
library(tidyverse)
library(broom)
cars_df <- Cars93 %>% 
  drop_na(Price, Origin) %>% 
  mutate(
    Origin = factor(Origin)
  )
glimpse(cars_df) # str
cars_df %>% 
  summarise(
    n_total = n(),
    missing_total = sum(is.na(.))
  )
normality_result <- cars_df %>% 
  group_by(Origin) %>% 
  summarise(
    shapiro_statistic = shapiro.test(Price)$statistic,
    shapiro_p_value = shapiro.test(Price)$p.value,
    normality = if_else(shapiro_p_value > 0.05,
                        "정규성 만족",
                        "정규성 위반"),
    .groups = "drop"
  )
normality_result

wilcox_result <- wilcox.test(
  Price ~ Origin,
  data = cars_df,
  alternative = "two.sided",
  exact = FALSE,
  conf.level = 0.95
)
wilcox_tidy <- tidy(wilcox_result) # 정돈된 스타일로 출력
wilcox_tidy
(test_statistic <- wilcox_tidy$statistic)
(p_value <- wilcox_tidy$p.value)
(method <- wilcox_tidy$method)
(alternative <- wilcox_tidy$alternative)

# 효과크기 : 0.05를 기준
# 차이를 구분하는 것이 불가능
# 효과크기 : conhen's d ( 평균 차이값 / 표준편차 ) # 차이가 표준편차의 몇 배수
# install.packages("effsize")
library(effsize)
group_A <- c(50, 55, 60, 65, 70, 75, 80, 85, 90, 95)
group_B <- c(45, 50, 55, 60, 65, 70, 75, 80, 85, 90)
cohen_d <- cohen.d(group_A, group_B)
cohen_d
# d estimate: 0.3302891 / 평가 기준 : 0.5 중간 효과, 0.8 이상이면 큰 효과

# 표준편차
sd_A <- sd(group_A)
sd_B <- sd(group_B)
# 평균
mean_A <- mean(group_A)
mean_B <- mean(group_B)
# 개수
n_A <- length(group_A)
n_B <- length(group_B)
# 합동 표준편차
# (A의 자유도 * A표준편차^2 + B의 자유도 * B표준편차^2) / A와 B의 통합자유도
pooled_sd <- sqrt(((n_A - 1) * sd_A^2 + (n_B - 1) * sd_B^2) / (n_A + n_B - 2))
(cohen_d_manual <- (mean_A - mean_B) / pooled_sd)

# ANOVA
data(InsectSprays) # 곤충스프레이 6개 제품
attach(InsectSprays)
str(InsectSprays) # 범주가 6개 있는 변수
InsectSprays
xtabs(InsectSprays)
table(InsectSprays)
table(InsectSprays$spray) # 각 회사별로 12개의 spray
with(InsectSprays, oneway.test(count~spray)) # 분산분석
# F = 36.065
# p-value = 7.999e-12
# 귀무가설 : 성능 차이가 없다
# 대립가설 : 스프레이 간 성능차이가 있다
# 귀무가설을 기각하고 대립가설을 채택한다 -> 스프레이 간 성능 차이가 있다
aov.out = aov(count ~ spray, data=InsectSprays)
aov.out
summary(aov.out)
TukeyHSD(aov.out) # 사후분석
summary.lm(aov.out) # 선형회귀 해석
plot(aov.out) # 선형회귀 분석 결과 확인
detach(InsectSprays)
