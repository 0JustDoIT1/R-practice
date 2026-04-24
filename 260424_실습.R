# remove.packages("tidyverse")
install.packages("tidyverse")
library(tidyverse)
head(mtcars)
as_tibble(mtcars) # 내부적으로 속도가 빠름
tibble(x=1:3, y='a') # R의 기본데이터타입 (Double 형) : 자동 형변환이 없음
df <- tibble(mpg = 1:3)
# df$m # 출력이 안됨
df$mpg
# dataframe하고 tibble의 차이점
mtcars %>% # 로딩하지 않고 사용, 패키지를 명시해야할 때때
  tibble::rownames_to_column("car") # tibble에서는 행이름을 열로 놓고 처리
tibble(
  id=1:3,
  data=list(1:5, 6:10, 11:15) # tibble에서 list로 데이터 지정가능
)

x <- c(10, 20)
x %>% 
  sqrt() %>% 
  log() %>% 
  mean()
x %>% sin(.) # 명시적으로 위치 지정 가능
x |> sqrt() |> log() # R base 에서도 가능

# tibble의 정의
df <- tibble(
  name=c("Kim", "Lee"),
  math_mid=c(80, 70),
  math_final=c(90, 85),
  eng_mid = c(75, 88),
  eng_final = c(82, 91)
)
df

df_long <- df %>% # long
  pivot_longer(
    cols = -name, # 제외하고
    names_to = "subject_exam",
    values_to = "score"
  )
df_long

df_sep <- df_long %>% # 정보 분리
  separate(
    col=subject_exam,
    into=c("subject", "exam"),
    sep="_"
  )
df_sep

df_unite <- df_sep %>% 
  unite(
    col="subject_exam",
    subject, exam,
    sep="_"
  )
df_unite

df_wide <- df_sep %>% 
  pivot_wider(
    names_from = exam,
    values_from = score
  )
df_wide

df_sep %>% filter(subject=="math") # where
df_sep %>% 
  group_by(name) %>% # 표시
  summarise(avg = mean(score)) # 개인별 과목 평균
# mtcars
head(mtcars)
df <- mtcars %>% 
  rownames_to_column("car") # 맨 마지막 결과값을 변수에 대입
head(df)
df %>% 
  select(car, mpg, hp, wt) %>% # 필드 선택 (열이름)
  arrange(desc(mpg)) %>% 
  head(5)
df %>% 
  filter(car %in% c("Mazda RX4", "Datsun 710", "Merc 240D")) %>% # 행선택
  select(car, mpg, hp, wt) # 열선택

df %>% 
  summarise(
    avg_mpg = mean(mpg),
    avg_hp = mean(hp),
    avg_wt = mean(wt)
  )

df_long <- df %>% 
  select(car, mpg, hp, wt) %>% 
  pivot_longer(
    cols = c(mpg, hp, wt), # 하나의 열에 포함될 데이터를 지정 : 3개열 -> 3개행
    names_to = "feature", # 열 이름은 feature
    values_to = "value" # 값들은 value
  )
head(df_long, 10)
df_long %>% 
  group_by(feature) %>% 
  summarise(avg_value = mean(value), .groups="drop") # 작업

df_long %>% 
  group_by(feature) %>% 
  summarise(
    min_value = min(value),
    mean_value = mean(value),
    max_value = max(value),
    .groups = "drop"
  )
# longer로 하면 시각화에 편리 (group화 해서 시각화)
ggplot(df_long, aes(x = feature, y = value)) + geom_boxplot() # group별로 시각화

df_wide_again <- df_long %>% 
  pivot_wider(
    names_from = feature,
    values_from = value
  )
head(df_wide_again)

data <- data.frame(x1 = 1:6,                
                   x2 = c(1, 2, 2, 3, 1, 2),
                   x3 = c("F", "B", "C", "E", "A", "D"))
data
dplyr::select(data, x1, x2)
dplyr::select(data, -x1) # 제외하고
arrange(data, x3)
filter(data, x2 == 2)
filter(data, x1 > 4, x2 > 1) # & | 연산자 없이 조건 추가 가능
data <- mutate(data, x4 = x1 + x2) # transform과 같이 파생변수
mutate(data, x5 = x4 / 100, x6 = x4 * x2) # 대입이 없어 확인만
data
rename(data, new_name = x3)
summarise(data, avg = mean(x2))
grouped_data = group_by(data, x2) # 표시만 함
summarise(grouped_data, # 집계함수
          average_height = mean(x1),
          people = n()) # count
set.seed(765)
sample_n(data, 3) # random하게 3개 출력력
select(data, c(x2, x3))
top_n(data, 1, x1) # 기준 점
top_n(data, 2, -x1)
(sorted = arrange(data, -x1)) # 역순으로
filter(sorted, x2 == 1)

mtcars %>% 
  filter(mpg > 20) %T>% 
  print() %>% # print는 연산을 안함 : 뒤로 보낼 것이 없음
  summarise(avg_hp = mean(hp))
x <- c(-3, 2, -1, 4)
x %<>% abs %>% sort # 앞으로도 전송해서 값을 바꿈
print(x)

# 문제
# 나이가 30이상인 사람만 선택(filter)해서 내림차순으로 정렬(arrange)해서 출력하시오
df <- tibble(
  id = 1:5,
  name = c("대한", "민국", "만세", "영원", "무궁"),
  age = c(25, 30, 35, 40, 45),
  gender = c("Male", "Male", "Male", "Female", "Female")
)
df %>% 
  filter(age >= 30) %>% 
  arrange(-age) -> filtered
filtered
# 데이터를 성별로 그룹핑한 다음, 평균 나이를 svg_age 변수로 추가하고
# df_summary 변수에 대입하시오
df_summaray <- df %>%
  group_by(gender) %>% 
  mutate(svg_age = mean(age))
df_summaray
# original 데이터의 나이에 모두 12를(mutate) 곱한 다음
# pension_in_months 변수 입력하시오
df %>% 
  mutate(pension_in_months = age * 12)

# Oncology(종양학), Cardio(심장학), Neuro(신경과), Pulmo(호흡기)
df <- tibble( # cohort 환자군
  cohort = c("Oncology","Cardio","Neuro","Pulmo","Cardio","Neuro","Oncology"),
  value  = c(10, 20, 15, 5, 12, 18, 25)
)
df
unique(df$cohort)

# factor # frequency 도수
df %>% 
  mutate(cohort_f = fct_infreq(cohort)) %>% 
  ggplot(aes(x = cohort_f)) +
  geom_bar() # 막대 그래프

df %>% 
  group_by(cohort) %>% 
  summarise(mean_val = mean(value)) %>% 
  mutate(cohort_f = fct_reorder(cohort, mean_val)) %>% 
           ggplot(aes(x = cohort_f, y = mean_val)) +
           geom_col()
library(purrr)
nums <- list(1,2,3,4)
# ~ : 함수 축약 (무명함수)
map(nums, ~ .x^2) # 함수형 프로그래밍 nums에 있는 각 데이터에 함수를 적용
map_dbl(nums, ~ .x^2) # 결과를 벡터로
class(map(nums, ~ .x^2)) # "list"
class(map_dbl(nums, ~.x^2)) # vector

df <- mtcars %>%
  rownames_to_column("car") %>%
  as_tibble()
head(df)
df_raw <- df %>%
  # row_number: 행 위치
  mutate(
    mpg = if_else(row_number() %in% c(2, 8), NA_real_, mpg), # 결측치
    hp  = if_else(row_number() %in% c(5, 12), NA_real_, hp), # 결측치
    wt  = if_else(row_number() == 3, 10, wt),  # 이상치
    mpg = if_else(row_number() == 10, 100, mpg) # 이상치
  )
# map은 행에 함수를 적용
# across는 열에 함수를 적용
df_raw %>% 
  summarise(
    across(
      everything(), # 모든 열
      ~ sum(is.na(.)) # 함수 축약 / . : 전달된 데이터
    )
  )
# 숫자 변수만 결측치를 확인하고자 할 때
df_raw %>% 
  summarise(
    across( # 열에 함수 적용
      where(is.numeric), # 수치가 숫자인 열
      ~ sum(is.na(.)),
      .names = "na_{.col}" # .names : 열 이름 = 들어온 이름
    )
  )

# na를 평균으로 대체
df_na_mean <- df_raw %>% 
  mutate(
    mpg = replace_na(mpg, mean(mpg, na.rm = TRUE)), # 수정 후 복사
    hp = replace_na(hp, mean(hp, na.rm = TRUE))
  )

detect_outlier_iqr <- function(x) {
  q1 <- quantile(x, 0.25, na.rm = TRUE) # 1사분위수
  q3 <- quantile(x, 0.75, na.rm = TRUE) # 3사분위수
  iqr <- q3 - q1 # IQR
  x < q1 - 1.5 * iqr | x > q3 + 1.5 * iqr # 이상치 구간
}
df_outlier_check <- df_na_mean %>% 
  mutate(
    mpg_outlier = detect_outlier_iqr(mpg),
    hp_outlier = detect_outlier_iqr(hp),
    wt_outlier = detect_outlier_iqr(wt)
  )
df_outlier_check %>% 
  select(car, mpg, hp, wt, mpg_outlier, hp_outlier, wt_outlier) %>% 
  filter(mpg_outlier | hp_outlier | wt_outlier)

install.packages("RMariaDB")
library(RMariaDB)
library(DBI)
library(dbplyr)
con <- dbConnect(
  RMariaDB::MariaDB(),
  dbname = "daejeon",
  host = "104.198.27.181",
  port = 3306,
  user = "hbc3869",
  password = "Zmfjszl123!"
)
dbListTables(con)
student=dbReadTable(con, "student")
print(student)
dbWriteTable(con, "mtcars_table", mtcars, overwrite = TRUE)
dbListTables(con)
mtcars_table=dbReadTable(con, "mtcars_table")
print(mtcars_table)
result <- mtcars_table %>% 
  filter(mpg > 20) %>% 
  summarise(mean_hp = mean(hp))
print(result)
dbDisconnect(con)

# 문제
# student 테이블을 로딩한 다음 국어, 영어, 수학 점수에 대한 평균을 출력하시오
student=dbReadTable(con, "student")
# avg_result <- student %>% 
#   summarise(kor_avg = mean(kor, na.rm=TRUE), eng_avg = mean(eng, na.rm=TRUE), mat_avg = mean(mat, na.rm=TRUE))
# avg_result
student %>% 
  select(c(kor, mat, eng)) %T>% 
  colSums() %T>% 
  print() %>% 
  colMeans()
student %>% 
  dplyr::select(kor, mat, eng) %>% 
  summarise(across(c(kor, mat, eng), ~ round(mean(.x, na.rm = TRUE), 2)))
dbDisconnect(con)