# two_sample : 평균분석을 하고 효과차이 분석을 실시하시오
# method 교육방법, score에는 점수
# 가설 : 교육 방법에는 차이가 있다
# 귀무가설 : 교육방법 별 점수 차이가 없다
# 대립가설 : 교육방법 별 점수 차이가 있다
two_sample <- read.csv("two_sample.csv", header=TRUE)
str(two_sample)
head(two_sample)
sum(is.na(two_sample)) # 결측치 개수 계산
two_sample <- na.omit(two_sample) # 결측치 처리 -> 제거
two_sample$method <- as.factor(two_sample$method) # 교육방법 그룹을 범주형으로 변환
# 1) 평균차이가 있는지 검정하시오
tapply(two_sample$score, two_sample$method, shapiro.test)
  # p-value = 0.7242, p-value = 0.008546 : 그룹 하나가 정규분포가 아니므로 비모수 검정(wilcox)
two_result <- wilcox.test(score ~ method,
            data=two_sample,
            alternative="two.sided",
            conf.level = 0.95)
  # W = 5628, p-value = 0.1042
two_result$statistic # 검정 통계량 : 5628
two_result$p.value # 확률값 : 0.104235
two_result$method # 분석 방법

  # 결론
  # 귀무가설을 기각할 수 없음 -> 즉, 교육방법 별 점수 차이가 없다

# 2) 방법1이 방법2보다 더 나은지를 검정하시오
wilcox.test(score ~ method,
                          data=two_sample,
                          alternative="greater",
                          conf.level = 0.95)
  # p-value = 0.9481 : 방법1이 방법2보다 더 낫다고 할 수 없음
# 3) 방법2가 방법1보다 더 나은지를 검정하시오
wilcox.test(score ~ method,
                          data=two_sample,
                          alternative="less",
                          conf.level = 0.95)
  # p-value = 0.05212 : 방법2가 방법1보다 더 낫지만 유의하지 않음

# 효과분석
# install.packages("effsize")
library(effsize)
cohen.d(score ~ method,
        data=two_sample)
# d estimate: -0.2740223 (small) -> 방법2가 방법1보다 낫지만 차이가 거의 없음


# three_sample : 분산 분석을 실시하시오
three_sample <- read.csv("three_sample.csv", header=TRUE)
str(three_sample)
sum(is.na(three_sample)) # 결측치 개수 계산
three_sample <- na.omit(three_sample) # 결측치 처리 -> 제거
three_sample$method <- as.factor(three_sample$method) # 교육방법 그룹을 범주형으로 변환

aov.out <- aov(score ~ method, data=three_sample) # 분산분석
aov.out
summary(aov.out) # p-value = 0.285 : 그룹 간 평균 차이가 유의미하지 않음
TukeyHSD(aov.out)
summary.lm(aov.out)
