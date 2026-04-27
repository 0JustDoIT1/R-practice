# 과제 - 파일 없이 코드 직접 복사 제출
# 문제
# weatherAUS.csv를 로딩하여 다음 과정을 처리하시오
getwd()
library(dplyr)
weather <- read.csv(choose.files())
nrow(weather)
length(weather)
# 수치 데이터 + 수치가 아닌 데이터
data <- select_if(weather, is.numeric) # 열별 적용
length(data)
boxplot(data)
nrow(weather)
# 1-1) NA수를 세고 결측치를 처리하시오
colSums(is.na(weather)) # 열별 NA 합계
weather <- na.omit(weather)
head(weather)

# 1-2) 이상치를 확인하고 처리하시오
new_weather <- weather %>% 
  mutate(
    across(
      where(is.numeric),
      ~ {
        Q1 <- quantile(.x, 0.25, na.rm=TRUE) # 1사분위수
        Q3 <- quantile(.x, 0.75, na.rm=TRUE) # 3사분위수
        IQR_VAL <- IQR(.x, na.rm=TRUE) # Q3 - Q1
        
        lower <- Q1 - 1.5 * IQR_VAL # 하한선
        upper <- Q3 + 1.5 * IQR_VAL # 상한선
        
        outlier <- .x < lower | .x > upper # 이상치 구간
        
        .x[outlier] <- median(.x[!outlier], na.rm=TRUE) # 이상치 데이터를 중앙값으로 대체
        
        .x
      }
    )
  )
head(new_weather)

# 1-3) 데이터를 7:3으로 샘플링하시오
n <- nrow(new_weather) # 전체 데이터 개수
sampling <- sample(1:n, 0.7*n) # 70% 추출
n70 <- new_weather[sampling, ] # 데이터니까 행에만 적용하고 모든 컬럼 출력
n30 <- new_weather[-sampling, ] # -를 통해서 그 외 30퍼 추출

# 1-4) 위의 데이터에서 각자 3가지 정보를 찾아내고 이를 시각화하시오

# ex) 오후 3시 습도와 내일 비의 관계는?
# ggplot(df, aes(x = Humidity3pm, fill = RainTomorrow)) +
#   geom_histogram(position = "dodge", bins = 30) +
#   theme_minimal()

library(ggplot2)
# 오후3시 구름량과 다음 날 비의 관계
ggplot(n30, aes(x=RainTomorrow, y=Cloud3pm)) +
  geom_boxplot(aes(fill=RainTomorrow)) +
  labs(title="구름량과 비 분석",
       x="다음 날 비",
       y="오후3시 구름량") +
  theme_minimal()

# 오후 3시 구름량과 오후 3시 습도의 관계 (+ 오늘 비)
ggplot(n30, aes(x=Cloud3pm, y=Humidity3pm)) +
  geom_point(aes(color=RainToday), alpha=0.5, size=3) +
  labs(title="구름량과 습도 관계",
       x="오후3시 구름량",
       y="오후3시 습도") +
  theme_minimal()

# 햇볕량과 최고 온도 관계(지역별)
ggplot(n30, aes(x=Sunshine, y=MaxTemp)) +
  geom_point(aes(color=MinTemp), alpha=0.2, size=3) +
  geom_smooth(method="lm", color="red") +
  labs(title="햇볕과 최고 온도 관계(지역별)",
       x="햇볕량",
       y="최고 온도") +
  facet_wrap(~Location) +
  theme_minimal()
