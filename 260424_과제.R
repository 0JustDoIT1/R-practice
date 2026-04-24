###############################
# 1. 다음 중 R에서 가장 기본이 되는 자료구조의 이름을 선택하시오.
# 답 : 2번
#  ① list ② vector  ③ union   ④ character

# 2. 다음에 주어진 변수의 합계와 평균을 출력하시오.
pay <- c(250,180,200,300,1000)

sum(pay)
mean(pay)

# 3. 다음은 무슨 함수의 실행 결과인지 실행 함수 이름을 작성하시오.
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
# 180     200     250     386     300    1000
summary(pay)

# 아래의 data.frame이 완성일 될 수 있게 4번,5번을 완성하시오.
# empno   pay   bonus
# 101     250   0.10
# 102     180   0.10
# 103     200   0.12
# 104     300   0.15
# 105    1000   0.00

# 4.  empno 변수, pay,  bonus 변수 생성
empno = c(101:105)
pay = c(250, 180, 200, 300, 1000)
bonus = c(0.10, 0.10, 0.12, 0.15, 0.00)
# 5. 위의 표처럼 데이터 프레임을 이용해서 만드시오.
pay201801 <- data.frame(
  empno = empno,
  pay = pay,
  bonus = bonus
)
pay201801
# 6. 다음의 출력처럼 total 컬럼을 추가 하려고 한다. 아래의 순번에 내용을 작성하시오.
#  1) 계산  > total <-
total <- pay201801$pay + (pay201801$pay * pay201801$bonus)
#  2) 함수  > pay201801 <-           (pay201801,total)
pay201801 <- transform(pay201801, total=total)
#  7. 위의 자료를 이용하여 아래의 그래프를 만드시오. ( x축 : empno, y축 : pay )
plot(x=pay201801$empno, y=pay201801$pay, main="201801", xlab="사원번호", ylab="급여", col="red", type="b")
#  8. dplyr 패키지 관련 함수를 이용하여 총 급여 300이상의 사원번호와 총 급여를 출력하시오.
# install.packages("tidyverse")
library(tidyverse)
result <- pay201801 %>% 
  mutate(total = total) %>% 
  select(c("empno", "total")) %>% 
  filter(pay >= 300)
result
#  9. 다음의 출력처럼 부서번호(deptno)를 추가한다.
#  empno   pay   bonus  deptno
#  101     250   0.10      1
#  102     180   0.10      2
#  103     200   0.12      1
#  104     300   0.15      2
#  105    1000   0.00      2
dept_result <- pay201801 %>% 
  mutate(deptno = c(1,2,1,2,2)) %>% 
  select(c("empno", "pay", "bonus", "deptno"))
dept_result
# 10. 아래의 출력 내용처럼 부서별 급여 평균을 출력하시오.
# A tibble: 2 x 2
#  deptno          mean_total
#  <dbl>            <dbl>
#    1      1        250.
#    2      2        514.
dept_group <- dept_result %>% 
  group_by(deptno) %>% 
  mutate(mean_total = mean(total)) %>% 
  select("deptno", "mean_total") %>% 
  head(2)
  
dept_group
##########################################################################################
