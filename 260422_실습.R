# 리스트 (데이터 타입 상이, 개수도 상이)
student <- list(
  name = "홍길동",
  score = c(85, 92, 78), # 벡터를 만드는 함수
  passed = TRUE,
  info = list(grade = 3, major = '통계학') # 리스트 안에 리스트
)

student$name
student[["score"]] # 값으로 데이터 추출 -> R : vector
student["score"] # 열이름 => 결과도 리스트
# mean(student["score"]) # 에러, vector 기준으로 연산
mean(student[["score"]])
student[[2]][1]
student$info$major # chaining으로 접근이 가능

student$score[2] <- 95
student[["email"]] <- "hong@naver.com" # 추가 (Python, Javascript, R 이나 똑같음)

# vector, matrix, data.frame, list, array
# 데이터 프레임은 기본적으로 데이터를 문자 취급하려는 경향이 있음
# 문자를 범주형으로 변화시키는 것을 금지
# R의 자료구조의 기본은 데이터프레임
# 데이터 프레임 -> 2차원 : 열 내 동질적, 열 간 이질적
df <- data.frame(
  name = c("대한", "민국", "만세", "진정성"),
  score = c(88, 75, 92, 65),
  grade = factor(c("B", "C", "A", "D")), # 범주화
  pass = c(TRUE, TRUE, TRUE, FALSE), # Boolean
  stringsAsFactors = FALSE # 입력되는 것을 모두 문자취급하는 것을 방지
)
str(df) # 데이터의 구조를 보여주는 함수
summary(df) # 요약 통계 -> 데이터 전체 상태
head(df, 3) # 앞에서 n개만 보기
dim(df) # 전체 크기 (행, 열)
nrow(df); ncol(df) # nrow : 행 개수 / ncol : 열 개수

df$score
df[["grade"]]
df[, "name"]
df[df$score >= 80,] # if 문이 [] 안에 정의
df[df$grade == "A", c("name", "score")]

# 데이터 프레임은 자료구조의 기본
data(iris)
head(iris, 6)
str(iris)
summary(iris) # 분석은 범주형데이터를 중심하고 실시
dim(iris) # 150 x 5
iris

(L3 = LETTERS[1:3])

d.f <- data.frame()
d.f <- edit(d.f)
d.f

# 데이터 샘플링
# cbind : column 기준
# recycling : 자동으로 길이 맞춤
# replace=TRUE : 복원추출 (항상 3개)
d <- data.frame(cbind(x=1, y=1:10), fac=sample(L3, 10, replace=TRUE))
d
d$fac
names(d) # 열 이름
(d$yhat <- d$fac)
str(d)
head(d)
d$fac = factor(d$fac)
rownames(d) = c("일", "이", "삼", "사", "오", "육", "칠", "팔", "구", "십")
d
mode(d$fac)
class(d$fac)

# 나무 성장
str(trees)
head(trees, n=3)
tail(trees, n=3)
trees[2:3,]
trees[trees$Height > 82,]
trees[10:12,2] # 리스트는 기본이 리스트
trees[10:12,2,drop=FALSE] # 데이터프레임은 빼면 벡터
summary(trees)
# IQR(Inter Quantile Range) : (3사분위수 - 1사분위수)
# IQR * 1.5배 : 이상치 판단 기준
# 하한 = Q1 - 1.5 * IQR
# 상한 = Q3 + 1.5 * IQR
boxplot(trees)
pairs(trees) # 상관도

getwd()
(data <- read.csv("input.csv", blank.lines.skip=T, stringsAsFactors=F, encoding="UTF-8"))
install.packages("stringr")
library(stringr)
head(data)
print(is.data.frame(data))
print(length(data)) # 열의 개수
sal <- max(data$salary) # vector로 리턴
print(sal)
info <- subset(data, salary >= 600 & str_trim(dept) == "IT")
print(info)
# 문자열 -> 날짜
retval <- subset(data, as.Date(start_date) > as.Date("2014-01-01"))
print(retval)
write.csv(retval, "output.csv")
newdata <- read.csv("output.csv")
print(newdata)

# 문제
# 문제 1) 데이터 프레임에 영어 점수의 퀴즈, 중간, 기말점수를 입력
#     영어 grade
# 퀴즈 67 "C"
# 중간 92 "A"
# 기말 89 "B"
# 문제 2) 수학점수를 열로 입력하고 (50,100,80) # 열추가
# 문제 3) 한 사람에 대한 점수를 rbind시키시오 # 행추가
# (영어점수 90, 등급 "A", 수학점수 100)
# 문제 4) 영어의 중간점수를 100점으로 수정하시오
score <- data.frame(
  eng = c(67, 92, 89),
  grade = c("C", "A", "B"),
  row.names= c("퀴즈", "중간", "기말")
)
print(score)
score$math <- c(50,100,80)
print(score)
score <- rbind(score, data.frame(eng=90, grade="A", math=100, row.names="보충"))
print(score)
score["중간", "eng"] = 100
print(score)

# 영어, 수학 점수를 합계해서 추가
# score <- cbind(score, sum = score$eng + score$math)
# score
(sum <- colSums(score[, c("eng", "math")]))
(da <- data.frame("eng"=sum[1], "grade" = 0, "math" = sum[2], row.names="합계"))
score2 <- rbind(score, da)
(kor <- c(90,80,100,77))
(kor <- c(kor, sum(kor)))
score2 <- cbind(score2, kor)
score2
write.table(score2, file="sungjuk.csv", sep=",")
(data3 <- read.table("sungjuk.csv", header = TRUE, sep = ",", row.names = 1))
write.csv(score2, file = "sungjuk2.csv")
data4<-read.csv("sungjuk2.csv")
data4

# 평균이 50이고 표준편차가 10인 정규분포에서 400개 추출
x <- rnorm(4000, mean=50, sd=10) # random + normal
hist(x) # 자동으로 도수 분포 생성 (구간값으로)
(x <- floor(runif(4000, min=0, max=100))) # random + uniform 균등 분포
plot(x)
x <- runif(4000, min=0, max=100)
hist(x)

# x,y좌표, y값만 제공, x값은 자동 결정
plot(c(data3$eng[1:4]), main="그래픽", col="red",
     ylab="영어점수", ylim=c(0,100), type="b") # type="b" -> both : point + line
# 구간 범주화를 자동으로 실행, 분포를 출력
hist(as.numeric(data3$eng[1:4]), main="영어점수", xlab="점수",
     ylab="도수", col="lightblue", breaks=5) # 균등구간값

mean(data3$kor[1:4])
median(data3$kor[1:4])
var(data3$kor[1:4])
sd(data3$kor[1:4])

# 02. 다음과 같은 벡터를 칼럼으로 갖는 데이터프레임을 생성하시오.
name <-c("대한이","민국이", "만세야","희망이","다함께")
age <-c(55,45,45,53,15)
gender <-c(1,1,1,2,1)
job <-c("연예인","주부","군인","직장인","학생")
sat <-c(3,4,2,5,5)
grade <- c("C","C","A","D","A")
total <-c(44.4,28.5,43.5,NA,27.1)

# <조건1> 위 7개 벡터를 user 이름으로 데이터 프레임 생성
# <조건2> 성별(gender) 변수를 이용하여 히스토그램 그리기
# <조건3> 만족도(sat) 변수를 이용하여 산점도 그리기
# <조건4> 총구매금액에 대하여 평균과 분산 그리고 표준편차를 출력하시오
user <- data.frame(
  name, age, gender, job, sat, grade, total
)
str(user)
user
user$gender <- as.factor(user$gender) # 형변환
unique(user$grade)
user$grade <- factor(user$grade, levels=c("A", "B", "C", "D", "E"))
user$sat <- factor(sat, ordered=TRUE) # 조심해야 할 점 1,2,3,4,5(1의 차이가 큼)

hist(as.integer(user$gender), main="성별분포", xlab="성별(남자,여자)", xlim=c(1,2), ylab="인원 수", col=c("orange", "yellow"), breaks=2)
plot(user$gender)

plot(user$sat, main="만족도 산점도", col="red", ylab="만족도", ylim=c(0,5), type="b")
(user_total <- sum(user$total, na.rm=TRUE))
(user_mean <- mean(user$total, na.rm=TRUE))
(user_var <- var(user$total, na.rm=TRUE))
(user_sd <- sd(user$total, na.rm=TRUE))

# 내적 - a dot b = |a| |b| cos theta
a <- c(1,1,1,1,1)
b <- c(1,1,1,1,1)
sum(a*b)
(dotresult <- a%*%b) # 매출액 계산
(anorm <- sqrt(sum(a^2))) # 피타고라스
(bnorm <- sqrt(sum(b^2)))
(costheta <- dotresult / (anorm * bnorm)) # 코사인 theta 값 # 유사도 비교
# cos theta 값의 범위(-1 ~ 1)
# 0 직교, 1이면 같다, -1 서로 반대

# 문제
a_3d <- c(3,1,2)
b_3d <- c(1,4,5)
# 1. 내적
# 2. 코사인 유사도
# 3. 각도 (사람이 인식하기 좋게)
# 함수화 하시오
(dot_product <- sum(a_3d*b_3d)) # 내적
a_3d_norm <- sqrt(sum(a_3d^2)) # norm
b_3d_norm <- sqrt(sum(b_3d^2)) # norm
costheta <- dot_product / (a_3d_norm * b_3d_norm) # 코사인 유사도
radian_angle_3d <- acos(costheta) # 각도 (라디안)
degree_angle_3d <- radian_angle_3d * 180 / pi # 각도 (도 단위 - 사람이 보기 좋게)

install.packages("NISTunits")
library(NISTunits)
(degree_angle_3d <- NISTradianTOdeg(radian_angle_3d))
dot_product <- function(vec1, vec2) {
  sum(vec1 * vec2)
}
calculate_norm <- function(vec) {
  sqrt(sum(vec^2))
}
calculate_cosine_similarity <- function(vec1, vec2) {
  dot_product <- sum(vec1 * vec2)
  norm1 <- calculate_norm(vec1)
  norm2 <- calculate_norm(vec2)
  dot_product / (norm1 * norm2)
}
dot_product(a_3d, b_3d)
calculate_cosine_similarity(a_3d, b_3d)

# 투영
# 그래프 문법을 이용한 패키지 + 레이어를 이용
install.packages("ggplot2")
library(ggplot2)
x <- c(3,4) # original
u <- c(1,0) # 기준벡터
(proj_scalar <- sum(x * u)) # 3
(proj <- proj_scalar * u) # 투영된 값 projection (차원 축소)

# 회전 - 매트릭스 (행렬) MRS(move, rotate, scale)
R <- matrix(c(0,-1, 1, 0), nrow=2) # 90도 돌림
(rot <- R %*% x)
# 반사
Ref <- matrix(c(-1, 0, 0, 1), nrow=2)
(ref <- Ref %*% x)

df <- data.frame(
  x = c(0,0,0,0),
  y = c(0,0,0,0),
  xend = c(x[1], proj[1], rot[1], ref[1]),
  yend = c(x[2], proj[2], rot[2], ref[2]),
  type = c("Original", "Projection", "Rotation", "Reflection")
)
# aes 미적요소
ggplot(df) + geom_segment(aes(x=x, y=y, xend=xend, yend=yend, color=type),
                          arrow=arrow(length=unit(0.3, "cm")),
                          linewidth=1.2) +
  xlim(-4,4) + ylim(-4,4) +
  coord_equal() + # x,y 축 비율을 동일
  theme_minimal() +
  labs(title="행렬변환")

# 매출액 -> 내적활용
qty <- c(10, 5, 8)
price <- c(1000, 2000, 1500)
total_sales <- sum(qty * price)
total_sales
10*1000 + 5*2000 + 8*1500

# 성적 가중평가
score <- c(80,90,70)
weight <- c(0.3, 0.2, 0.5) # 가중치
(weighted_score <- sum(score*weight)) # 내적
############## 복잡한 매출액 (지점별 매출액)
qty_matrix <- matrix(c(
  10,5,8,
  7,9,6
  ), nrow=2, byrow=TRUE)
price <- c(1000, 2000, 1500)
# [ 1000,
#   2000,
#   1500 ]

# 앞에 있는 행렬의 열의 수와 뒤에 있는 행렬의 행의 수가 같아야 계산 가능
(store_sales <- qty_matrix %*% price)

# array 다차원배열
column.names <- c("COL1", "COL2", "COL3")
row.names <- c("ROW1", "ROW2", "ROW3")
matrix.names <- c("Matrix1", "Matrix2", "m3", "m4", "m5")

# 행 , 열, 면으로 이름 지정
result <- array(c(11:19, 21:29, 31:39, 41:49, 51:59),
                dim = c(3,3,5),
                dimnames = list(row.names, column.names, matrix.names))
print(result)
print(result[3,,2])

# 2면에 있는 데이터를 모두 출력하시오
# 2번 매트릭스의 3행 2열에 있는 데이터를 추상해 보고 확인하시오
# 1번 매트릭스의 2행 3열에 있는 데이터를 추상해 보고 확인하시오
# 2번 매트릭스의 1행 3열에 있는 데이터를 추상해 보고 확인하시오
# 3번 매트릭스의 2행 2열에 있는 데이터를 추상해 보고 확인하시오
# 3번 매트릭스의 2행 3열에 있는 데이터를 추상해 보고 확인하시오
# 4번 매트릭스의 3행 2열에 있는 데이터를 추상해 보고 확인하시오
# 5번 매트릭스의 3행 1열에 있는 데이터를 추상해 보고 확인하시오
