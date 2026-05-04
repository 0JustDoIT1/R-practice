# 선형회귀 (linear regression) - 단순선형회귀 (독립 변수가 1개) : Bias 커짐
# y = ax + b 에서 x,y는 데이터관측 결정
# 기울기와 절편을 구하는 것이 회귀 분석
data(mtcars)
(x <- mtcars$hp) # 마력
(y <- mtcars$mpg) # 연비

x_mean <- mean(x)
y_mean <- mean(y)
# 공분산 (y와 x 간의 공분산 / x의 분산)
# x,y의 공분산을 x의 분산이 얼마나 설명하고 있는가 -> 기울기
(beta1 <- sum((x - x_mean) * (y - y_mean)) / sum((x - x_mean)^2))
# 절편
(beta0 <- y_mean - beta1 * x_mean)
cat("회귀선 방정식 : Y =", beta0, "+", beta1, "* X\n")
y_pred <- beta0 + beta1 * x # 회귀식

plot(x, y, main = "최소제곱법 회귀", xlab = "마력", ylab = "연비")
abline(a = beta0, b = beta1, col = "blue", lwd = 2)

# 상관계수로 설명하는 회귀식
# lineal model : 기울기와 절편
model <- lm(y~x)
plot(x, y, main = "최소제곱법 회귀", xlab="마력", ylab="연비")
abline(model, col = "blue", lwd = 2)

# 다중회귀 : 설명변수가 여러 개 (독립 변수가 여러 개)
# 변수가 늘어남 : Bias가 작아지고, Variance는 아직 적절
y <- c(1,2,3,8,5,3,10,7) # 실제값
x1 <- c(2,4,5,6,8,10,11,13) # 10
x2 <- c(1,2,2,4,4,4,6,6) # 5
opar = par(mfrow=c(1,3))
plot(x1, y)
plot(x2, y)
plot(x1, x2)
summary(lm(y~x1))
summary(lm(y~x2))
summary(lm(y~x1+x2))

# 회귀식 (무한대까지 가능) 10000, 34
(yhat <- -0.1041 + (-1.1566 * x1) + (3.7267 * x2)) # 예측값
y - yhat # 잔차 (residual)
(sse <- sum((y - yhat)^2)) # sum of square error : 잔차 제곱합
(ybar <- mean(y))
(ssr <- sum((yhat - ybar)^2)) # 회귀 제곱합
ssr/(sse+ssr) # 0.9532009 # Multiple R-squared : 0.9532
# F 검정 통계량 MSR / MSE (mean 평균 - 개수)
(MSR <- ssr / 2) # 변수가 2개이므로 # 회귀 제곱합
(MSE <- sse / (8-2-1)) # 잔차 # 잔차 제곱합
MSR/MSE # 50.91979 # F-statistic: 50.92
sd(y - yhat) # 잔차의 표준편차
sd(yhat - y) / sqrt(length(y)) # 잔차의 표준오차

###################### 다중회귀
# install.packages("car")
library(car)
data(mtcars)
fit <- lm(mpg ~ wt + hp + disp, data = mtcars)
# 선형성, 정규성, 등분산성, 독립성
plot(fit, which = 1)

# 선형성 체크
car::residualPlots(fit)

# 독립성 체크 : 자기상관성 : 2가 기준
# 2 : 자기상관성이 없다
# 2 -> 0으로 가면 자기 상관성 강해짐
# 2 -> 4으로 가면 음의 자기 상관성이 강해짐
# p-value가 0.05보다 작으면 자기상관성이 있다.
car::durbinWatsonTest(fit)

# 정규성 체크
plot(fit, which = 2)
shapiro.test(residuals(fit))
# 귀무가설 : 정규성을 따른다
# 대립가설 : 정규성을 따르지 않는다
# 결론 : 귀무가설을 기각하고, 대립가설을 채택한다
car::vif(fit) # 다중공선성 체크 : 변수 상호 간 영향력이 있는가
# vif 값이 10을 기준으로 10을 넘어가면 위험
# 5를 기준으로 주의해서 점검

# 등분산성 체크
plot(fit, which = 3)
lmtest::bptest(fit)
# 귀무가설 : 등분산성이 존재한다
# 대립가설 : 등분산성이 존재하지 않는다 (이분산성이 존재)
# 결론 : 귀무가설을 기각하지 못한다 -> 등분산성이다

plot(fit, which = 4)
car::influencePlot(fit)
# 특별한 point (데이터 한 개가) 모델에 얼마나 영향을 미치는가
# 이상치
# Cook's Distance : 특정 데이터를 제외했을 때 모델의 계수가 얼마나 변화가 있는지를 체크
#                 : 값이 크면 모델을 자기 방향으로 유도 하는 성향
# outlier 이상치를 발견하는 방법


# 다음 데이터에 대해서 회귀식을 작성해 보시오
str(women)
(fit <- lm(weight ~ height, data=women))
plot(women$weight, women$height)
summary(fit)
women.fitted_weight <- -87.52 + (3.45 * women$height)
# 잔차를 구하시오 (min, 1사분위수(quantile), median, 3사분위수, max)
(residual <- women$weight - women.fitted_weight)
summary(women_residual)
min(residual)
quantile(residual, 0.25)
median(residual)
quantile(residual, 0.5)
quantile(residual, 0.75)
max(residual)
# 상관계수를 구해서 회귀 모델의 정당성을 부여하시오
cor(women$weight, women$height)
cor(women)
cor.test(~ weight + height, data = women)
# 귀무가설 : 상관이 없다
# 대립가설 : 상관이 있다
# 잔차 제곱합과 회귀 제곱합을 구하고 결정계수를 구하시오
sse <- sum(residual^2) # 잔차제곱합
ssr <- sum((women.fitted_weight - mean(women$weight)) ^2) #회귀제곱합
ssr / (ssr + sse) # 결정계수 R^2
# F 검정 통계량
(MSR <- (ssr/1))
(MSE <- (sse/(15-1-1)))
MSR / MSE
# 잔차의 표준편차를 구하시오
sd(residual)

# cars 데이터에 대해서
# 모델이 통계적으로 유의미한가 : p-value: 1.49e-12 -> 모델이 유의미하다
# 계수가 유의미한가 : 1.49e-12 -> 계수도 유의미하다
# 모형의 설명력은 : Adjusted R-squared:  0.6438
# 데이터에 적용된 선형회귀는 적정한가
# 에 대해서 해석하시오
opar=par(mfrow=c(1,1))
data(cars)
head(cars)
summary(cars)
summary(cars$speed)
plot(cars$speed, cars$dist)
# speed에 따른 제동거리 회귀 예측 모델
res.lm <- lm(dist ~ speed, data=cars)
summary(res.lm)
abline(res.lm)
# 선형회귀는 적정한가 : 0.8068949 -> 적절하다
yhat <- -17.5791 + (3.9324 * cars$speed)
cor(cars$dist, yhat)
#########################
(cof=coef(res.lm))
cof["speed"]
cof["(Intercept)"]
coef(res.lm)["speed"]
(dist_pred = cof["(Intercept)"] + cof["speed"] * cars$speed)
fitted(res.lm) # 각 데이터에 대한 예측값
residuals(res.lm) # 잔차
sum(cars$dist - fitted(res.lm))
sum((cars$dist - fitted(res.lm))^2)
var(residuals(res.lm)) # 잔차 분산
sd(residuals(res.lm)) # 잔차 표준편차
plot(res.lm)
# 표준화 잔차 : 잔차 / 표준편차
# 29, 39, 49 이 Cook's distance 값이 크고
# 이 중 49번이 위험 : 임계선
# 위험하다 -> 모델에 영향을 크게 미치는 점
library(lmtest)
bptest(cars$dist ~ cars$speed) # 등분산이다
dwtest(res.lm) # 2는 자기상관성이 없다. -> 변수를 제거하고 시계열분석
# DW = 1.6762 는 2를 기준으로 양의 자기 상관성이 약간 있다
predict(res.lm, newdata=data.frame(speed=10)) # 점 추정
# 구간예측
predict(res.lm, newdata=data.frame(speed=c(4.0, 25.0, .21)), interval="confidence")
# 실제상황을 고려해서 예측하라 (돌발변수)
predict(res.lm, newdata=data.frame(speed=10), interval="prediction")

#################### 모델 비교
fit1 <- lm(mpg ~ wt, data = mtcars)
fit2 <- lm(mpg ~ wt + hp, data = mtcars)

logLik(fit1) # 모델 적합도
logLik(fit2)
# 숫자가 작은 것이 좋은 모델이다
# 다음 모델 중 fit2를 선택
AIC(fit1, fit2) # 2개값을 고려 : Bias (logLik), Variance (2k : 변수의 개수)
# AIC보다 수정해서 변수가 많으면 더 큰 페널티를 부여
BIC(fit1, fit2)

#################### 보스턴 집값예측
library(MASS)
data("Boston")
str(Boston) # 506 x 14
boston_df <- as.data.frame(scale(Boston)) # 다중회귀에서 정규화가 중요
# (데이터 관측값 - 평균) / 표준편차 -> 정규화 (Z 점수 정규화)
# 역정규화가 필요 -> 예측값 * 표준편차 + 평균 -> 원래 scale로 복원
head(boston_df, 3)
set.seed(234)

# 학습 데이터 : 테스트 (7:3 분리)
# 테스트에 정당성 부여 (전데이터를 학습하면 테스트할 데이터의 특성이 모델에 영향을 미침)
idx <- sample(1:nrow(boston_df), 300) # 행으로 분리
trainDF <- boston_df[idx,]
testDF <- boston_df[-idx,]
dim(trainDF); dim(testDF)
set.seed(100)
head(trainDF, 2)
formula <- medv ~ crim + factor(chas) + zn + indus + nox + rm + age + dis + rad + tax + ptratio + black + lstat
lm_model <- lm(formula = formula, data = trainDF)
lm_model
summary(lm_model)

(all_vars <- names(trainDF))
target <- "medv"
(features <- all_vars[all_vars != target])
custom_formula <- as.formula(
  paste(target, "~", paste(features, collapse = " + "))
)
print(custom_formula)
formula <- zn + nox + rm + dis + rad + tax + ptratio + black + lstat
lm_model <- lm(formula = formula, data = trainDF)
lm_model
summary(lm_model) # Multiple R-squared:  0.7847
pred <- predict(lm_model, trainDF)
# 예측의 평가는 상관계수 : 일반화
cor(pred, trainDF$medv) # 0.8858279 정확도
pred <- predict(lm_model, testDF) # 학습에 참여하지 않은 데이터
cor(pred, testDF$medv) # 0.8137324 정확도 : 일반화된 모델
# 만약 test 데이터 평가 0.5라면 train 데이터에 과대 적합
# 만약에 train 데이터에 대해서 모델 테스트를 했는데 0.5 : 과소적합
# 과소적합이면 변수를 추가해서 다시 업데이트
dwtest(lm_model) # DW = 2.1179
library(car)
sqrt(vif(lm_model)) > 2
cor(trainDF)
# nox, rad, tax
cor(trainDF[c('nox', 'rad', 'tax')])
form <- medv ~ crim + zn + indus + rm + age + dis + rad + ptratio + black + lstat
lm_model <- lm(formula = form, data = trainDF)
(pred <- predict(lm_model, testDF))
cor(pred, testDF$medv) # 0.7954335

# scale -> 역표준화 (표준편차, 평균)
pred * sd(Boston$medv) + mean(Boston$medv)

########################## 회귀 분석은 종속변수나 독립변수나 연속적 데이터
str(mtcars) # 연비 예측
# 제거해야할 변수를 선택하시오 : cyl, vs, am, gear, carb
(mydata <- mtcars[-c(2,8,9,10,11)])
# 문제 formula를 작성해 보시오
# formula <- mpg ~ disp + hp + drat + wt + qsec
(vari <- colnames(mydata))
(form <- as.formula(paste(vari[1], paste(vari[-1], collapse=" + "),
                          sep=" ~ ")))
form
# 문제 train / test 데이터를 7:3의 비율로 분리하시오
idx <- sample(1:nrow(mydata), 0.7*nrow(mydata))
train_data <- mydata[idx, ]
test_data <- mydata[-idx, ]

fit <- lm(form, data=train_data)
summary(fit)
par(mfrow=c(2,2))
plot(fit)
par(mfrow=c(1,1))
pred <- predict(fit, test_data[-1]) # 종속변수를 예측(따라서, 종속변수 제거)
cor(pred, test_data$mpg) # 0.9237757
# 문제 : -1이 들어간 이유
# 변수 선택법
library(MASS)
form
# 비교를 하는 AIC를 이용해서 모델을 비교
# 작은 값이 좋다
(form3 <- step(fit)) # 변수 선택 : 모델을 만들고 변수를 후진선택법(변수를 제거하면서 모델 작성)
fit <- lm(form3, data=train_data)
summary(fit) # 차원감소하면 추론 속도가 빨라짐

# 전진선택법은 1개부터 시작해서 점차 추가
form2 <- stepAIC(fit) # AIC, BIC 보정이 가능
fit <- lm(form2, data=train_data)
summary(fit)

pred <- predict(fit, test_data[-1])
cor(pred, test_data$mpg) # 0.9215776

# install.packages("gvlma")
library(gvlma)
summary(gvlma(fit))
# ml, gml (logistic 회귀)
# ml은 정규분포 회귀, 연속형 : 연속형
# gml은 family (다양한 확률 분포와 결합한 모델) : 이항(true/false), 포아송, 감마
# logistic : 범주형 : 연속형
# 확률 = 회귀분석 mapping -> link function
# 레버리지 값 확인
(leverage_values <- hatvalues(fit))
cooks_distance <- cooks.distance(fit)
n <- nrow(mtcars)
p <- length(coef(fit)) - 1
leverage_threshold <- 2 * (p + 1) / n
filtered_data <- mtcars[leverage_values > leverage_threshold | cooks_distance > 1,]
clean_data <- mtcars[!(leverage_values > leverage_threshold | cooks_distance > 1),]
# 이상치가 제거된 데이터와 일반 데이터로 학습된 모델 비교
model <- lm(mpg ~ hp + wt, data = train_data)
diagnos1 <- summary(model)
model <- lm(mpg ~ hp + wt, data = clean_data)
diagnos2 <- summary(model)
diagnos1$adj.r.squared # 0.7623299
diagnos2$adj.r.squared # 0.8407457
diagnos1$coefficients
diagnos2$coefficients # 절편까지도 유의미하게 변화
