#excel data 복붙 바로 하는 방법#####
#install.package(datapasta)
#library(datapasta)  
#그리고 옮기려는 데이터를 복사하고
#addins에 data를 검색 후 tribble란을 클릭.
tab_a <- tibble::tribble(
 ~id, ~score,
    1L,    35L,
    3L,    70L,
    4L,    63L,
    5L,    80L
 )
tab_a
tab_b <- tibble::tribble(
    ~id, ~name,
          1L, "issac",
          2L, "jelly",
          3L, "soony",
          4L,  "bomi"
    )
tab_b
library(tidyverse)
#left_join(a,b,by=) : a라는 df에 b가 달라붙는 형태.
#아래는 기준열이 유일하게 id로 존재하기에 기준열 지정안해도 워킹 됨.

left_join(tab_a, tab_b)
#~기준열이 다를때~####
#예로들어 a라는 df에는 id1, b라는 df에는 id2라고 네이밍이 되어있다면
#by를 vector로 받게되므로 c('id1'='id2')로 하면 된다.
tab_a <- tibble::tribble(
    ~id1, ~score,
    1L,    35L,
    3L,    70L,
    4L,    63L,
    5L,    80L
)
tab_a
tab_b <- tibble::tribble(
    ~id2, ~name,
    1L, "issac",
    2L, "jelly",
    3L, "soony",
    4L,  "bomi"
)
tab_b
left_join(tab_a,tab_b,
          by=c('id1' = 'id2'))




#Mutating joins[두 테이블이 하나의 테이블로 합체]####
#left_join(): 가장 중요 (위에서 함.)
#right_join()
#inner_join(): 유용함
#full_join(): 유용함

#right_join() : 오른쪽기준.
#이 경우는 id=5가 남지 않는다.
right_join(tab_a, tab_b)

#inner_join() : 교집합만 빼서 테이블 생성.
inner_join(tab_a, tab_b)
#full_join() : 모든 데이터 보존 테이블 생성.
#우선 선 매칭시키고 tab_a부터 데이터 처리되는 방식.
full_join(tab_a, tab_b) 

#Filtering joins : 하나의 테이블을 사용해서 다른 테이블로 필터.####
#semi_join(): 조건 선택
#anti_join(): 조건 제거

#semi_join()
#tab_a를 필터링을 하는데 tab_b를 이용해서 하는 방식
#tab_b에는 id가 1,3,4가 있으므로 이들로 필터링 한다는 생각임.
#id=5가 없는 이유는 필터링 테이블(tab_b)에 id=5가 없기 때문
semi_join(tab_a, tab_b)

#anti_join() 
#tab_a를 필터링 하는데 tab_b에 있는 것을 차집합으로 나타낸 것.
anti_join(tab_a, tab_b)
#아래와 같이 쓰면 필터링 느낌이 확 난다.
tab_a
tab_a %>% 
    anti_join(tab_b)

tab_a
tab_a %>% 
    semi_join(tab_b)

#SQL에서는 이렇게 다른 테이블로 불러와서 id가 분류된 경우가 많음.
#이런 경우 두 테이블을 불러와서 하나씩 filter()를 쓰는것은 매우 힘든작업이므로
#두 테이블을 이용해서 하는게 바람직한 방법임.

#Q. 대칭 차집합은 어떻게?? 즉, 공통인 부분 아예 없애버리는 것.
#(A-B) U (B-A)
#output : id=2,5
#내풀이
tab_a
tab_b
tab_c <- (tab_a %>% 
    full_join(tab_b))
tab_c[apply(tab_c, MARGIN = 1, FUN = function(x) any(is.na(x))),]

#강사님 풀이 : full_join, inner_join을 가지고 !!두테이블을 이용해
#!!필터링을 하자.
tab_c <- tab_a %>% 
    full_join(tab_b)

tab_d <- tab_a %>% 
    inner_join(tab_b)
tab_c;tab_d 

tab_c %>% 
    anti_join(tab_d)


#실제 데이터 이용####
# 데이터 로드
moving_data <- read_csv("./data/seoul_moving_202107_09_hr.csv")
reference_data <- readxl::read_excel("./data/reference.xlsx")
# 변수 이름 수정
moving_data <- janitor::clean_names(moving_data)
reference_data <- janitor::clean_names(reference_data)

moving_data %>%
    glimpse()

#문제1. moving_data에 시도정보와 
#name, full_name 붙이기.
#hint. sigungu코드를 이용할 것.
moving_data %>% glimpse()
reference_data %>% glimpse()

moving_data %>% 
    left_join(reference_data, by= c("chulbal_sigungu_kodeu" = 'sigungu')) %>% 
    glimpse()

#문제2. 서울시에서 출발한 trip만 필터링 해보시오.
#hint. sido 코드를 이용하시오.
reference_data %>% 
    select(sido,full_name)
moving_data %>% 
    left_join(reference_data, by= c("chulbal_sigungu_kodeu" = 'sigungu')) %>% 
    filter(sido == 11000)

    

#mutate 심화학습.#####
library(magrittr)
#랭크라는 새로운 컬럼을 가지고 컬럼 생성 위치조정
rank(tab_a$score)
tab_a %<>% 
    mutate(ranking = rank(score), 
           .after = "id")
tab_a

#그러나 보통 점수가 높을 수록 순위를 높게쳐주니까.. 
order(tab_a$score,decreasing = T)
tab_a %<>% 
    mutate(ranking = order(tab_a$score, 
                           decreasing = T), 
           .after = "id") %>% 
    arrange(ranking)
tab_a

#또는 다음처럼 깔끔하게 할 수 있다. score는 스칼라 실수값이므로 -를
#붙여주면 반대 랭크가 될 것임을 이용한다.
rank(-tab_a$score)
tab_a %<>% 
    mutate(ranking = rank(-score), 
           .after = "id")
tab_a


#빈칸()을 NA로 처리하고 싶을 때 관련한 학습####
y <- c("a", "b", "", "c")
y_na <- na_if(y, "")
y_na
#NA를 다른값으로 대치할 때
#물론, 벡터로 확장가능하다.
coalesce(y_na, "hi")
replace_na(y_na, "hello")

y_na[1] <- NA
y_na
coalesce(y_na, c('hi','hello'))
#위 코드를 보면 모두 매칭이 되어야 한다. 총 4개의 벡터를써줘야함. 
#나는 불편하게 느껴짐.
coalesce(y_na, c('hi', 'hello', 'pizza', 'pasta'))




#ifelse() ####
rank(-tab_a$score)
tab_a %<>% 
    mutate(ranking = rank(-score), 
           .after = "id")
tab_a

tab_a %<>% 
    mutate(
        status = if_else(
            score < 40,
            "low", if_else((score >=40) &(score <=70), 'middle', 
                           'high')),
        .after = "ranking"
    )
tab_a


#case_when() 학습 : 다중 ifelse문 작성####
tab_a %>% 
    mutate(
        status = if_else(ranking ==1, 'leader',
                         if_else(ranking<3, 'followers',
                                 'third')),
        .after = 'ranking'
    )
#위코드로 되는데 왜 아래코드를??
#어느정도 직관적이고 SQL문법과 아래코드가 유사하기때문.
tab_a %>% 
    mutate(
        status = case_when(
            ranking == 1 ~ "leader",
            ranking < 3 ~ "followers",
            ranking == 3 ~ "third",
            TRUE ~ "else"),
        .after = "ranking"
    )

#문제####
#moving_data에서 평균이동시간을 시간단위로 바꿔 새로운 변수를 만들어보세요.
#trip_time_hr 이름 사용
#trip_time_hr을 이용해서 새로운 컬럼trip_time_class를 
#short(<=30m), middle(30m~1h), long(1h 이상)으로 
#분류하는 컬럼을 만들어라.
moving_data %>% 
    mutate(trip_time_hr = pyeong_gyun_idong_sigan_bun/60) %>% 
    mutate(trip_time_class = case_when(
        trip_time_hr <=0.5 ~ 'short',
        (0.5<trip_time_hr) & (trip_time_hr<1) ~ 'middle',
        trip_time_hr >=1 ~ 'long'
    ),
    .after = 'trip_time_hr')






####group_by####
#grouping된 tribble은 프린트하면 표시가 된다.
moving_data <- read_csv("./data/seoul_moving_202107_09_hr.csv")
reference_data <- readxl::read_excel("./data/reference.xlsx")

moving_data <- janitor::clean_names(moving_data)
reference_data <- janitor::clean_names(reference_data)

moving_data %<>% 
    left_join(reference_data, by= c("chulbal_sigungu_kodeu" = 'sigungu')) %>% 
    filter(sido == 11000)
moving_data %<>% 
    mutate(trip_time_hr = pyeong_gyun_idong_sigan_bun/60) %>% 
    mutate(trip_time_class = case_when(
        trip_time_hr <=0.5 ~ 'short',
        (0.5<trip_time_hr) & (trip_time_hr<1) ~ 'middle',
        trip_time_hr >=1 ~ 'long'
    ),
    .after = 'trip_time_hr')
moving_data %>% dim()


tab_a<- tibble(id = c(1,3,4,5),
               ranking = c(1,3,2,4),
               status = c('high','low','high','low'),
               score = c(35,70,63,80))
tab_a %>% arrange(ranking, .by_group = T) # grouping 표시 생략.

tab_a %<>% group_by(status)
#그룹별로 카운팅 해줌.
tab_a %>% 
    group_by(status) %>% 
    tally() #tally? : "바를 정"를 사용한 카운팅.

tab_a %>% 
    ungroup() %>% 
    count(status)
# count() == group_by() + tally()
####실습#####
#Q. 서울시에서 short trip이 가장 많이 일어나는 구는?
#moving_data를 이용할 것. 
moving_data %<>%
    group_by(name)

moving_data %>% 
    filter(trip_time_class == 'long') %>% 
    group_by(name) %>% 
    tally() %>% 
    arrange(desc(n))
 
moving_data %>% 
    group_by(trip_time_class, name) %>% 
    tally() %>% 
    slice_max(n, n = 3) #group_by로 묶여있어서 slice_max도 이렇게 출력 됨. 
#각 그룹별 3/3/3출력되는 모양임.



