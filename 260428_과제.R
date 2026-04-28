# 부모 학력수준(level2) 자녀대학진학여부(pass2) 간에 관련이 있는지 검정하시오 (독립성 검정)
# 범주형 데이터 확인
# 귀무가설 : 부모의 학력과 자녀의 대학진학은 관계가 없다.
# 대립가설 : 부모의 학력과 자녀의 대학진학은 관계가 있다.
# 유의수준 : 0.05에서 검정
# 과제 : R 파일로 날짜_이름.R 제출
# 카이제곱분석 : 범주형(factor)
# 결측치, 이상치, 범주화, 정규화
getwd()
data <- read.csv("cleanDescriptive.csv", header=TRUE, fileEncoding="CP949", encoding="CP949")
head(data)
str(data)
class(data)

sum(is.na(data)) # 결측치 개수 계산
data <- na.omit(data) # 결측치 처리 -> 제거
data$level2 <- as.factor(data$level2) # chr 형태로 안전하게 범주형 변환
data$pass2 <- as.factor(data$pass2) # chr 형태로 안전하게 범주형 변환
(xt <- xtabs(~ level2 + pass2, data=data)) # xtabs으로 교차표 만들기(level2, pass2)
chisq.test(xt) # 카이스트제곱분석 -> 지금은 정확하지 않다고 경고
# 기대빈도가 5미만인 셀 존재 -> Fisher test
fisher.test(xt) # p-value : 0.01116
0.05 > 0.01116
# 결론 : 유의수준보다 p-value가 더 작으므로 귀무가설 기각
# => 즉, 부모의 학력과 자녀의 대학진학은 관계가 있다.