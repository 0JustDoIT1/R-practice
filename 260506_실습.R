# entropy 불순도 계산
-0.5 * log2(0.5) # log는 확률 0~1 사이값이 -라서
-0.1 * log2(0.1)
-0.9 * log2(0.9)

# 동전의 앞뒤면
baseEntropy = 1
# 동전의 앞면, 동전의 뒷면 => 확률표현
x1 <- 0.4; x2 <- 0.6
e <- -x1 * log2(x1) - x2 * log2(x2)
e # 0.9709506
x1 <- 0.2; x2 <- 0.8
e2 <- -x1 * log2(x1) - x2 * log2(x2)
e2 # 0.7219281
e-e2 # 0.2490225, 정보 이득 (복잡도가 줄어듬)

# 불순도 그래프
p <- seq(0, 1, length.out = 501)
gini <- 2*p*(1-p) # 지니 계수 계산식
# log에서는 0이 무한대
entropy <- {pp <- p; pp[pp==0] <- 1e-12; pp[pp==1] <- 1-1e-12;
-(pp*log2(pp) + (1-pp)*log2(1-pp))}
misclass <- 1 - pmax(p, 1-p) # 오분류

plot(p, gini, type="l", lwd=2, xlab="Class proportion p", ylab="Impurity", main="Impurity indices(binary)")
lines(p, entropy, lwd=2)
lines(p, misclass, lwd=2)
legend("top", legend=c("Gini", "Entropy", "Misclassification"), lty=1, lwd=2, bty="n")

# 0.5가 peak가 됨

# rpart DT 모델
# install.packages("rpart.plot")
# install.packages("rattle")
library(rpart)
library(rpart.plot)
library("rattle") # fancyRpartPlot
str(iris) # 150x5(독립변수 : 4개, 종속변수 : 1개(Species: 범주 3개))

result <- sample(1 : nrow(iris), nrow(iris) * 0.7)
train <- iris[result,]
test <- iris[-result,]
dim(train); dim(test) # 105x5 , 45x5

table(train$Species)
formula <- Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width
model <- rpart(formula = formula, data = train)
model
plot(model)
text(model)
prp(model)
rpart.plot(model)
fancyRpartPlot(model)
# class 예측
(pred2 <- predict(model, test, type="class"))
# 확률값 예측
pred <- predict(model, test)
pred
cpred <- ifelse(pred[,1] >= 0.5, "setosa",
                ifelse(pred[,2] >= 0.5, "versicolor", "virginica"))
(tb=table(cpred, test$Species))
# cpred        setosa versicolor virginica
# setosa         17          0         0
# versicolor      0         12         1
# virginica       0          3        12

# 정분류율
(17 + 12 + 12) / (17 + 12 + 3 + 12 + 1) # 0.9111111
(17 + 12 + 12) / length(test$Species)
sum(diag(tb)) / nrow(test)

# 오분류율
1 - sum(diag(tb)) / nrow(test) # 0.08888889
# 정분류율 + 오분류율 = 1
(3 + 1)/nrow(test)
(tb - diag(diag(tb)))

              # 정밀도(precision), recall 또는 민감도
# setosa        17/17               17/17
# versicolor    12/13               12/15
# virginica     12/15               12/13
prp(model, type = 1, extra = 1)
prp(model, type = 2, extra = 106)
fancyRpartPlot(model, palettes = c("Greens", "Oranges", "Blues"))
# install.packages("partykit")
library(partykit)
plot(as.party(model), tp_args = list(id = FALSE))

# 유방암 데이터
wdbc <- read.csv("wdbc_data.csv", stringsAsFactors = FALSE)
str(wdbc) # Benign(양성), Malignant(악성)
# 암세포의 반지름, 둘레, 질감, 면적, 대칭 등을 이용해서 양성, 악성을 판별하는 분류 모델
sum(is.na(wdbc))
wdbc <- wdbc[-1]
wdbc_box <- wdbc[c(-1, -2)] # diagnos를 제거 (boxplot은 수치 데이터만)
boxplot(wdbc_box)
head(wdbc)
head(wdbc[, c('diagnosis')], 10)
# 종속변수를 factor
wdbc$diagnosis <- factor(wdbc$diagnosis, levels = c("B", "M"))
wdbc$diagnosis[1:10]
normalize <- function(x) { # min_max 정규화 0 ~ 1 사이값으로
  return ((x - min(x)) / (max(x) - min(x)))
}
wdbc_x <- as.data.frame(lapply(wdbc[2:31], normalize)) # 열로 적용
wdbc_x
summary(wdbc_x)
# 종속변수 + 독립변수
wdbc_df <- data.frame(wdbc$diagnosis, wdbc_x)
dim(wdbc_df) # 569 31 (독립변수 30)
idx = sample(nrow(wdbc_df), 0.7 * nrow(wdbc_df))
wdbc_train = wdbc_df[idx, ]
wdbc_test = wdbc_df[-idx, ]
dim(wdbc_train)
dim(wdbc_test)
model2 <- rpart(wdbc.diagnosis ~ ., data = wdbc_train)
model2
rpart.plot(model2)
# 문제 : 정분류율과 오분류율을 계산하시오
wdbc_pred <- predict(model2, wdbc_test, type="class")
(res <- table(wdbc_pred, wdbc_test$wdbc.diagnosis))
# res   B   M
# B     113   8
# M       3  47

# 정분류율
# (113 + 47) / nrow(wdbc_test)
acc <- round(sum(diag(res)) / nrow(wdbc_test), 2)
# 오분류율
# 1 - ((113 + 47) / nrow(wdbc_test))
malacc <- 1 - acc
print(paste("정분류율:", acc*100, "%, 오분류율:", malacc*100, "%"))
# 정밀도, 민감도, 특이도를 구하시오
# 정밀도
113 / (113 + 8)
# 민감도
113 / (113 + 3)
# 특이도
47 / (8 + 47)
# install.packages("caret")
library(caret)
confusionMatrix(wdbc_pred, factor(wdbc_test$wdbc.diagnosis))
# 정보가 없더라도 accuracy : 0.6784
# Kappa 우연히 맞출 확률을 제외하고 Kappa : 0.8849
# Pos Pred Value : 0.9339     정밀도 (precision)
# Neg Pred Value : 0.9400     음성 환자에 대한 정밀도
# Balanced Accuracy : 0.9143  양성/음성 숫자가 다른 것을 고려한 가중된 accuracy

# 문제 : 이상치 제거 코드를 추가하시오
remove_outliers <- function(x) {
  qnt <- quantile(x, probs = c(.25, .75), na.rm = TRUE)
  caps <- quantile(x, probs = c(.05, .95), na.rm = TRUE)
  H <- 1.5 * IQR(x, na.rm = TRUE)
  x[x < (qnt[1] - H)] <- caps[1] # 이상치를 삭제하는 것이 아니라 대체
  x[x > (qnt[2] + H)] <- caps[2]
  return(x)
}
wdbc_cleaned <- as.data.frame(lapply(wdbc[, -1], remove_outliers))
wdbc_cleaned$diagnosis <- wdbc$diagnosis
str(wdbc_cleaned)
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
wdbc_x <- as.data.frame(lapply(wdbc_cleaned[1:30], normalize))
wdbc_x
summary(wdbc_x)
wdbc_df <- data.frame(wdbc$diagnosis, wdbc_x)

############################ weather.csv
# RainTomorrow를 예측 (내일 비가 올 것인가)
weather = read.csv("weather.csv", header=TRUE)
str(weather)
names(weather)
head(weather)
c <- which(colnames(weather) == "Date") # 인덱스
d <- which(colnames(weather) == "RainToday")
weather <- weather[,c(-c,-d)]
sum(is.na(weather))
weather <- na.omit(weather)
str(weather)
weather$RainTomorrow <- factor(weather$RainTomorrow)
weather$WindGustDir <- factor(weather$WindGustDir)
weather$WindDir <- factor(weather$WindDir)
idx <- sample(1:nrow(weather), 0.7*nrow(weather))
train <- weather[idx,]
test <- weather[-idx,]
tree <- rpart(RainTomorrow~., data = train) # 일반 파라미터를 이용한 모델
# 가지치기를 적용 : cp(complexity parameter) : 개선효과가 이것보다 크지 않으면 분할 중지
# 정보이득이 있어야 분할 : cp보다 커야 분할 -> 분할하지 않으면 가지치기
weather.df <- rpart(RainTomorrow~., data = train, cp=0.048)
# control을 이용해서 여러가지 제약조건 부가
# cross validation 데이터를 10개 그룹으로 나누어서 테스트 (xval)
# leaf가 5개는 되어야 분할 한다
my.control <- rpart.control(xval=10, cp=0.048, minsplit=5)
tree3 <- rpart(RainTomorrow ~ ., data = train, method = "class",
               parms = list(split = "information"), control = my.control)
# list(split = "information") : entropy를 기준으로 분할
rpart.plot(tree) # 기본 모델
rpart.plot(weather.df) # 가지 치기
rpart.plot(tree3) # entropy, cross validation, regulaization규제, 가지치기
plotcp(tree)
pred <- predict(tree, test, type="class") # 기본
res <- xtabs(~pred + test$RainTomorrow)
round(sum(diag(res)) / nrow(test),2)
pred <- predict(tree3, test, type="class") # 복잡
res <- xtabs(~pred + test$RainTomorrow) # formula
round(sum(diag(res)) / nrow(test),2)
pred <- predict(weather.df, test, type="class") # 가지치기
res <- xtabs(~pred + test$RainTomorrow) #formula
round(sum(diag(res)) / nrow(test),2)
c <- confusionMatrix(pred, factor(test$RainTomorrow))
# 프로그램 내에서 참조 가능능
c$overall[1] # Accuracy
c$overall[2] # Kappa
c$overall[3] # AccuracyLower
c$table
c$byClass["Sensitivity"] # 민감도
c$byClass["Specifity"] # 특이도
c$byClass["Precision"] # 정밀도
c$byClass["Recall"] # 민감도도

library(rpart)
str(kyphosis) # 척추측만증 81 x 4
# absent(치료) present(진행 중)
# number : 몇 개를 수술, Start : 몇 번째 척추 뼈에서 시작
kyphosis$Kyphosis
sum(is.na(kyphosis))
kyphosis <- na.omit(kyphosis)
str(kyphosis)
idx <- sample(1:nrow(kyphosis), 0.7 * nrow(kyphosis))
train <- kyphosis[idx,]
test <- kyphosis[-idx,]
dim(train)
fit <- rpart(Kyphosis ~ Age + Number + Start, method="class", data=train)
names(fit)
fit$frame
# 정보기반 모델들은 변수 중요도를 출력하고 변수 선택을 가능하게 함
fit$variable.importance # 변수 중요도 - 전처리로 사용 가능 (변수 선택 가능)
fit$cptable
fit$parms
# $prior : 데이터의 사전확률
# $loss : 오분류시 벌점의 가중치
# $split : 분할의 알고리즘 지표
predict(fit, type = "prob") # 학습한 데이터에 대한 예측
predict(fit, type = "class")
predict(fit, type = "matrix")
pred <- predict(fit, test, type="class") # 테스트 데이터
xtabs(~pred + test$Kyphosis)
res <- xtabs(~pred + test$Kyphosis)
round(sum(diag(res)) / nrow(test), 2) # accuracy

# 문제
# 가지치기의 기준값을 확인하고 가지치기 적용된 모델을 구성한 후
# 가지치기 이전의 결과와 비교하시오
# plotcp
plotcp(fit) # Inf : 가지치기를 하지 마라
fit2 <- rpart(Kyphosis ~ Age + Number + Start, method="class", data=train, cp=1)
pred2 <- predict(fit2, test, type="class")
xtabs(~pred + test$Kyphosis)
res <- xtabs(~pred + test$Kyphosis)
round(sum(diag(res)) / nrow(test), 2)
my.control <- rpart.control(xval=10, cp=0.048, minsplit=3) # 과적합방지
fit3 <- rpart(Kyphosis ~ Age + Number + Start,
              method="class", data=train, control=my.control)
pred <- predict(fit3, test, type="class")
res <- xtabs(~pred + test$Kyphosis)
round(sum(diag(res)) / nrow(test), 2) # 0.8
fit3$cptable
fit3$cptable[which.min(fit$cptable[,"xerror"]), "CP"] # 0.2083333
summary(fit3) # 회귀 분석의 summary 처럼 모델 요약

# randomForest : ensemble
# bootstrap
# bagging
# ensemble : 여러개의 모델이 합동으로 예측(평균), 분류(투표)
# voting, stacking
# install.packages("randomForest")
library(randomForest)
fit <- randomForest(Kyphosis ~ Age + Number + Start, data=kyphosis)
print(fit) # Number of trees: 500개의 DT로 구성
# OOB 학습에서 제외된 데이터
varImpPlot(fit, main="Variable Importance for Kyphosis")
prob_preds <- predict(fit, kyphosis, type="prob")
head(prob_preds)

# 문제 iris를 randomForest로 트리 모델을 구성하시오
str(iris)
# ntree= 300 모델 개수, mtry=4 : 트리를 분할할 때 몇 개의 변수를 고려할 것인가
# iris의 독립변수 4 -> 4 : 랜덤성이 없음
# sqrt(n:변수개수) : 2
model <- randomForest(Species ~ ., data=iris, ntree=300, mtry=2, na.action=na.omit)
model
# 2/3는 학습에 참여하고 1/3남겨서 (OOB)
# OOB estimate of error rate : 18.52% 훈련에 참여하지 않은 데이터
# accuracy 100-18.52 정확도 => 82.48%
plot(model)
print(model)
importance(model)

# 최적의 파라미터 tunning
ntree <- c(400, 500, 600)
mtry <- c(2:4)
param <- data.frame(n=ntree, m=mtry)
param

for(i in param$n) {
  cat('ntree = ', i, '\n')
  for(j in param$m) {
    cat('mtry = ', j, '\n')
    model = randomForest(Species~., data=iris,
                         ntree=i, mtry=j,
                         na.action=na.omit)
    print(model)
  }
}
model3 = randomForest(Species ~ ., data=iris, ntree=500, mtry=2,
                      importance = T, na.action=na.omit)
model3
importance(model3)
print(model3)
model3$confusion
model3$votes
model3$ntree
