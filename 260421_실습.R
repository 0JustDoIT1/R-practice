#기본 데이터 타입 (5개) 
# R은 객체지향 프로그램으로 데이터 타입을 지정하지 않음
# 기본 데이터타입 : number(double), 기본 데이터 단위 : vector
# 기본자료구조 : Data.Frame

# 앞뒤에 괄호를 붙이면 결과도 출력해준다
# 실행명령은 ctrl + enter
(x_num <- 3.14) # number(double)
(x_int <- 5L) # integer 형
(x_char <- "Hello R") # 문자열 ""
(x_lgl <- TRUE) # logical Boolean
(x_cpx <- 2 + 3i) # complex 복소수

# 데이터 타입 확인함수
class(x_num) #객체 지향 프로그래밍 -> 어떤 클래스인지 확인한다
typeof(x_int) # 기본 데이터 타입 확인
is.numeric(x_num) #TRUE
is.character(x_char)
is.logical(x_lgl)

# 타입 변환
as.integer(3.7)
as.numeric("3.14")
as.character(100)
as.logical(0)
as.logical(1)
as.logical("TRUE")

# 연산자 (계산기처럼 사용가능)
5 + 3
10 / 4
2^8
sqrt(144) #바로 루트 씌우는거 가능

# log 사용 이유 : 비선형을 선형으로 변환한다 (1,2,3)
log(100000, base = 10) #복잡한걸 단순화하기 위해(큰수를 작은수로)

# 연산은 동일 데이터타입만 가능 - 자동 형변환
x <- 42 #type: double / 기본이 numeric
t <- 1.23
x + t 
s = 1.01
x + s 
typeof(x)
typeof(t)

#관계 연산자
x <- 5 #하나를 집어넣어도 vector
y <- 16
x < y
x > y
x <= 5
y >= 20
y == 16
x != 5

# 벡터를 명시화 : 벡터에는 동질적 데이터가 저장되어야 한다
# 이질적 데이터가 들어갈 수가 없어서 자동 형변환 된다.
(x <- c(TRUE, FALSE, 0 ,6)) # 데이터 타입은 더 큰쪽으로 자동 형변환 된다
# bool은 TRUE, FALSE 두가지만 있고 숫자는 많아서 numeric 승리
mode(x) #거의 안쓴다
typeof(x) #데이터 타입 확인
class(x) #클래스 확인
x+1 #모든 요소를 1씩 증가시킨다
x #원본을 보존한다
x <- x+1 #이렇게 대입을 해야 값이 변경된다
!x #벡터 부정정
y <- 16
x&y #and 연산자 (두개가 참이면 참이다)
x
y # broadcasting -> x개수에 맞춰서 y되 똑같이 된다 (개수를 자동으로 맞춘다)
#NA가 있으면 연산이 안되기 때문에, 개수를 맞춘다
x|y # or연산자 = |, 요소에 적용한다

# R은 인덱스가 1부터 시작한다 -> 뒤에것도 포함한다
v <- 2:8 #범위 연산자 사용
print(v)

#포함연산자 #R에서는 연산자 정의가 가능하다, 정의하는 연산자는 %%를 사용한다
v1 <- 8
v2 <- 12
t <- 1:10
v1 %in% t
print(v2 %in% t)

# 행렬만들기 (matrix) -> 2차원 (행row, 열column) #byrow = 행이 우선이냐?
(M = matrix( c(2,6,5,1,10,4), nrow = 2, ncol = 3, byrow = TRUE))
# 2 by 3 (2행 3열)
t(M) # transpose 전치행렬 (3행 2열로 바뀜) / 행과 열을 바꿔준다
# %*%  행렬 곱
t <- M %*% t(M) # =가 이상하게 동작할때가 많아서 alt - <-사용 
# 행렬은 데이터를 표현하고 변환하고, 구조분석을 하기 위함함
# 내적 : 두 벡터의 같은 요소를 곱해서 다 더하면 내적 (크기값이 고려된 사이각)
# 벡터와 벡터의 내적을 내려면 사이즈가 동일해야함
# 행렬곱은 벡터 내적 연산의 연속이다
dim(M) #2 3
dim(t(M)) # 3 2#행렬수 보는거
print(t)
# 행렬제곱을 할때, 행렬과 행렬을 직접 곱할 수 없어서, 전치를 해줘야한다 
# 본인 행렬 * 전치행렬로 곱한다
# 중요!!!! 이유 : 행렬곱 연산은 내적연산의 연속이다
# 내적연산 : vector가 두개있을때 내적을 구할때, 관계 : 크기가 고려된 사이각
# 복잡함을 단순화함
# 앞에있는 열수와 뒤에 있는 행렬의 행수가 일치해야 행렬곱이 가능하다
# ex 2x3 * 3x2
# 자기자신을 제곱하면 행렬은 항상 정사각행렬이 된다

# recycling (앞뒤가 같지 않아도 알아서 부족한게 다섯개가 된다)
c(1,3,5,7,9) * 2 #벡터화 연산 #(내부적으로 알아서 2,2,2,2,2 로 개수를 맞춰준다다)
c(1,3,5,7,9) * c(2,4) #배수관계가 아니라는 경고가 뜬다
c(1,3,5,7,9,10) * c(2,4) # 두개씩 갯수를 맞춰줘야한다
c(2,4) * c(1,3,5,7,9,10) # 앞뒤로 자리가 바뀌어도 동일하다

(data = 1:5) # 1,2,3,4,5
factorial(1:5) # 1, 2, 6, 24, 120 = cumprod
1*2*3*4*5
sum(data)
prod(data)
mean(data) # 평균
median(data) # 중위수 (중요!! 이상치를 고려) 통계에서는 중위수를 중요하게 여긴다다
cumsum(data) #누적합 1, 3, 6, 10, 15
cumprod(data)

# 수학함수의 초월함수(비선형) : 삼각함수, 지수함수, 로그함수
# 삼각함수 - 회전이나 주기를 가진 데이터 분석 시 사용
# 지수함수 - 자연적으로 증가하는 데이터 분석 시 사용
# 로그함수 - 복잡한 데이터를 단순화, 비선형을 선형으로 매핑함
# exponent 
exp(1) # 지수함수 2.718282(오일러 상수) / 복리가 자연적으로 증가할때 얼마나 증가하나?
exp(2) 
exp(2:10)

# 원주율 3.141592 : 컴퓨터에서는 각도를 사용하지 않고 라디안을 사용한다

cos(c(0, pi)) #180도를 4로 나누면 45도 / 0을 0이라고 하지 않고 e-17 0.0000000000000
sqrt(c(1, 4, 9, 16)) # 비선형을 선형으로 바꾸는데 사용 - 데이터 규칙을 찾기 위함함
# -> 분석하기가 쉬워진다다

#범주형 데이터(정성적 데이터) -복잡함을 단순화시키기 위함함
seasons <- factor(c("봄", "여름", "가을", "겨울", "봄", "여름")) #factor는 범주형데이터
levels(seasons) #범주의 종류
nlevels(seasons) # 범주 개수
table(seasons) # 도수분포표 만들어줌

# 순서가 있는 범주형 데이터 ordered factor (비교가 가능해진다)
size <- factor(c("소","중","대","소"),
               levels = c("소", "중", "대"),
               ordered = TRUE)
size[1] < size[3]
size[2] > size[1]

#범주형 데이터 변환
gender <- c('M','F','M','F','M') #문자 MALE, FEMALE
gender
mode(gender);
class(gender)
#문자를 시각화하려면 안된다
plot(gender)
fgender <- as.factor(gender) #범주형 변환
fgender
fgender[3]
levels(fgender)
plot(fgender)



#결측치, NA
sum(c(1,2,NA,3))
1 / 0 #Inf  : infinite 무한대
0 / 0 #NaN : not a number 숫자가 아니다다
Inf / NaN # NaN
Inf / Inf # NaN

# 연산자
# 변수 이름 정의시 특수문자는 사용이 불가하다다
'%divisible%' <- function(x,y)
{
  if (x%%y == 0) return (TRUE) #나머지 연산자 : 배수인 경우
  else           return (FALSE)
}
10 %divisible% 3
10 %divisible% 2
'%divisible%'(10,5) #연산자를 함수처럼 사용

# 간단한 기술통계
score <- c(85, 95, 75, 65)
score
mean(score) #평균
sum(score) #합계

var(score) #variance 분산
sd(score) # standard deviation 표준편차
score - mean(score)
sum(score - mean(score))
(score - mean(score))
sum((score - mean(score))^2)/3 # = 분산 구하는 수식
# 4가아닌 3으로 나눈 이유는 데이터를 표본으로 인식했기때문
# 예) 우리나라 인구의 키 평균 (전체인구/n) =>시간과 비용이 많이 든다
# 일부분을 뽑아서(표본)으로 모집단을 추정한다
# 모집단 - 표본(n-1)


score <- c(85, 85, NA, 75, 65)
score
sum(score) #na가 하나라도 들어있을때 연산을 하면 NA가 나온다
mean(score, na.rm = TRUE) #na.rm -> 제거하고 계산
sum(score, na.rm = T) #TRUE는 T로만 써도 된다

x <- c(10, 20, 30, '40') #문자로 데이터 입력
x
typeof(x)
(xx <- as.numeric(x))
typeof(xx)


# 날짜 데이터 (문자, 숫자 아님)
# 패키지 : (설치O, 로딩o), (설치o, 미로딩), (설치x, 로딩x)
Sys.Date()
Sys.time()

today <- '2017-03-11 10:15:24'
today
class(today)
today2 <- as.Date(today) #날짜데이터로 변환
today2
class(today2)

#도움말보기
help("mean")
?sum
#사용법
args(sum)
example(sum)
?typeof

getwd() #get working directory 현재 작업 드렉토리
setwd('C:\\work') #현재 작업 디렉토리
getwd()
ls() #현재 메모리에 있는 변수들 출력

#데이터 많을때 한번씩 삭제 필요함함
rm(list=ls()) #메모리에서 변수 제거
ls()

#데이터 저장하고 제거했다가 다시 로드하는 법법
x <-  1:10
x
y <-  100:200
y
setwd('c:\\work')
save(x, y, file='xy.RData')
rm(list=ls())
ls()
load('xy.RData')
ls()
x
y

# indexing
a = c(1,2,3)
a[2]
(b=1:10)
b[2]
b[c(2,3,4)] #인덱스에 벡터 입력

x <- c(1:9)
x
x[3]
x[c(2,4)]
x[-1] # 마이너스 인덱스 (-의 의미는 : 제외하고) #2,3,4,5,6,7,8,9

#x[c(2, -4)] #정수와 마이너스는 혼용이 안된다 / 정수면 정수, 마이너스면 마이너스
x[c(2.4, 3.54)] #소수점은 내림한다
x[c(TRUE, FALSE, FALSE, TRUE)] #boolean indexing
x[x < 3]
x[x > 3]

x <- c(1, 5, 4, 9, 0)
length(x)
NROW(x) #열 개수
NCOL(x) #행 개수

#sequence / 연속된 수치 만들기
seq(1, 3.2, by=0.2) #1부터 3.2까지 간격을 0.2로 해서 연속된 수치 만들어라
seq(1, 5, length.out=4) # 1부터 5사이에 전체 출력개수가 4개가 되게 해라
seq(1, 6, length.out=4) # 1부터 6사이에 전체 출력개수가 4개가 되게 해라라

#문제
# 3.2에서 1사이의 값을 0.2 간격으로 출력하시오
seq(3.2, 1, by = -0.2)

#repeat 반복
(b <- rep(1:4, 2)) # 1에서 4까지 두번반복
(d <- rep(1:3, each=3)) # 1에서 3까지 각각 세번 반복
(d <- rep(1:3, 2, each=3)) # 1에서 3까지 각각 세번을 두번 반복 #each가 먼저 적용된다


#which 위치를 찾는다
x <- c(10, 20 , 30, 40)
which(x >20)
x[which(x > 20)] #인덱스화
which.max(x) # python에서는 클래스.함수인데 / R에서는 .도 이름이다
which.min(x)

#any, all
v1 <- c(10, 20, 30, 40, 50)
length(v1) #5개
names(v1) <- c("a","b","c","d","e") #인덱스 이름 지정정
v1["a"]
any(v1 > 45) # 하나라도 조건을 만족하면 TRUE
all(v1 > 0) # 모든게 조건을 만족하면 TRUE
which(v1 > 25) #인덱스로 출력된다

#문제

#1) Vector1 벡터 변수를 만들고, "R" 문자가 5회 반복되도록 하시오.
#2) Vector2 벡터 변수에 1~20까지 3간격으로 연속된 정수를 만드시오.
#3) Vector3에는 1~10까지 3간격으로 연속된 정수가 3회 반복되도록 만드시오.
#4) Vector4에는 Vector2~Vector3가 모두 포함되는 벡터를 만드시오.
#5) 25~ -15까지 5간격으로 벡터 생성- seq()함수 이용

#1) Vector1 벡터 변수를 만들고, "R" 문자가 5회 반복되도록 하시오.
(vector1 <- rep("R", 5))


#2) Vector2 벡터 변수에 1~20까지 3간격으로 연속된 정수를 만드시오.
(vector2 <- seq(1, 20, by=3))

#3) Vector3에는 1~10까지 3간격으로 연속된 정수가 3회 반복되도록 만드시오.
(vector3 <- rep(seq(1, 10, 3), 3))

#4) Vector4에는 Vector2~Vector3가 모두 포함되는 벡터를 만드시오.
(vector4 <- append(vector2, vector3))

#5) 25~ -15까지 5간격으로 벡터 생성- seq()함수 이용
(vector <- seq(25, -15, by=-5))

data() #교육용 자체 데이터 로딩할때 dataset
Nile #Flow of the River Nile, 나일강 강수량
length(Nile)
plot(Nile)

#vector는 names, matrix =  colnames, rownames
#매트릭스
x <-  matrix(1:9, nrow=3, byrow=TRUE) #자동 계산해줘서 col알아서 계산해준다다
colnames(x)
colnames(x) <- c("c1","c2","c3") 
rownames(x) <- c("r1","r2","r3")
x
#인덱스 줄때
x['r1', 'c1'] #행, 열로 지정하고
x["r1",] #하나만 하면 다 나옴

(x <- matrix(1:9, nrow=3, dimnames = list(c("X","Y","Z"), c("A","B","C")))) #개수가 다를 경우 리스트로 묶어서 데이터를 보낸다

y <-  matrix(nrow=2, ncol=2)
y[1,2]
y[1,1] <- 1 #열중심
y[2,1] <- 2
y[1,2] <- 3
y[2,2] <- 4
y

#column bind, row bind
cbind(c(1,2,3), c(4,5,6)) #열로 묶어서
rbind(c(1,2,3), c(4,5,6)) #행으로 묶어서

#matrix index
(x = matrix(1:9, nrow = 3, byrow = TRUE))
x[c(1,2), c(2,3)] #1행, 2행 / 2열 3열이 교차되는 지점점
#,가 붙으면 2차원으로 해석해라
x[c(2,3),] 
x[,] #아무것도 없으면 전체 다 보여주기
x[-1,] #1행을 제외하고고
(a=x[1,]) #1행만 보이기
class(a)
mode(a)
(a=x[1,,drop=FALSE]) #한행, drop=FALSE는 차원을 유지해라라
mode(a)
class(a)

#, 없이 -> 1차원 해석 (2차원도 메모리에서는 1차원으로 저장) 1차원 자체로 인덱싱해라

x[1:4]
x[c(3,5,7)]


x[c(TRUE,FALSE,TRUE), c(TURE, TRUE, FALSE)]
x[c(TRUE, FALSE), c(2,3)]
x[c(TRUE, FALSE)]

(x[2,2] <- 10)
(x[x<5] <- 0)
print(x)

#문제
#행렬제곱하시오
(x = matrix(1:12, nrow = 3, ncol = 4))
# 3X4 - 4X3
t(x)
x * x
x %*% x #에러 자기꺼 같이 못곱함
xt <- t(x)
x %*% xt
x[1,] %*% xt[,1] #내적 기호 # 내적 166
x[1,] %*% xt[,2]
x[1,] %*% xt[,3]
x[2,] %*% xt[,c(1,2,3)] #행렬곱, 내적값
#행렬곱은 내적의 연속이다 -> 특징추출출
x[3,] %*% xt[,c(1,2,3)] #특징 추출

#      [,1] [,2] [,3]
#[1,]  166  188  210
#[2,]  188  214  240
#[3,]  210  240  270
#행렬을 제곱하면 대칭 행렬이 만들어진다

#정방행렬 (행수와 열수가 같은 행렬)
#비정방행렬 (행수와 열수가 다른 행렬)
#대칭행렬 (대각에 있는 요소를 제외하고 상삼각과 하삼각이 대칭)
(mdat <- matrix(seq(20, 4, -2), nrow=3, ncol=3, byrow=TRUE, dimnames = list(c('a','b','c'), c('x','y','z'))))
t(mdat)
nrow(mdat)
ncol(mdat)
dim(mdat) # 차수

rowSums(mdat)  # 행합계
rowMeans(mdat) # 행평균
colSums(mdat)  # 열합계
colMeans(mdat) # 열평균

diag(mdat) #대각요소 : 크기에 영향을 미침
diag(20, 12, 4) # 20이 대각요소, 12행수, 4열수
diag(diag(mdat)) #상삼각 0, 하삼각 0, -> 대각행렬 -> 방정식의 해를 구한 값


#행렬분해 -> 구조파악
#!중요! 고유값 분해 (eigen 분해) :정방행렬에만 해당된다다
# vector는 크기와 방향을 표현
# 행렬은 vector로 구성 -> 크기와 방향으로 분해 가능
# 데이터의 노이즈 제거를 위함
# 고유벡터들은 크기가 1이다, 그리고 서로 직교좌표계 -> 그 축의 방향으로 얼마나 흐트러졌는데 -> 분산의 크기
# 고유치 : 중요한걸 표현
# 노이즈 제거를 위한 PCA principle component analysis 주성분 분석 / 제1주성분, 제2주성분

(mdat2 <- mdat %*% t(mdat)) #정방행렬 + 대칭행렬
mdatEigen <- eigen(mdat2) #행렬을 간단하게 -> 구조파악을 위함
mode(eigen(mdat2))
mdatEigen$values
mdatEigen$vectors[1,]
mdatEigen$vectors[2,]
mdatEigen$vectors[3,]
mdatEigen$vectors[1,] %*% mdatEigen$vectors[2,]
mdatEigen$vectors[1,] %*% mdatEigen$vectors[3,] # 0은 직교
mdatEigen$vectors[2,] %*% mdatEigen$vectors[3,] 

#문제
#열별 내적을 확인해 보시오

mdatEigne$vectors[,1]
mdatEigne$vectors[,2]
mdatEigne$vectors[,3]
mdatEigne$vectors[,1] %*% mdatEigen$vectors[,2]
mdatEigne$vectors[,1] %*% mdatEigen$vectors[,3]
mdatEigne$vectors[,2] %*% mdatEigen$vectors[,3]
#행간 열간 내적이 모두 0인 정직교하는 고유벡터 축을 발견
#고유치의 중요한 변수순으로 선택하면 주성분 분석 (PAC)