library(tidyverse)
library(magrittr)
library(DBI)

# DB-SQLite : 설치완료
# DBI 
# RSQLite : R~SQLite 연결
#dbconnect : DBI에 속한 함수
getwd()
setwd("C:/Users/PJS/Documents/study/tidyverse in R/data")
con <- dbConnect(RSQLite::SQLite(), 
                 "./movingdata.db")
dbListTables(con) #.tables
#CREATE TABLE = dbWriteTable()
dbWriteTable(con, 'mtcars_tbl', 
             mtcars)
dbListTables(con)
dbGetQuery(con,
           "SELECT * from mtcars_tbl limit 5")

#install.packages("dbplyr")
library(dbplyr)
library(dplyr) #tbl()
mtcars_db <- tbl(con, 'mtcars_tbl')
mtcars_db #is not tribble
mtcars_filter <- mtcars_db %>% 
    select(mpg:hp) %>% 
    filter(mpg > 20)

mtcars_filter %>% #SQL을 잘하는 분에게 질문할 때 좋다.
    show_query() #SQL_QUERY를 알려줌.
#최적화 된 답변을 받았으면 dbGetQuery()에 받아서 쓰면 됨.

mtcars_filter %>% 
    slice(2:6) 
#Error in `slice()`:
#    ! `slice()` is not supported on
#database backends.
#아니 그러면 뭐가 적용되는지 어떻게 암????
#dbplyr reference 참고..
#slice_min,max는 다 있는데 그냥 slice가 없음. -> head()로 대체.

mtcars_tibble <- mtcars_filter %>% #as_tibble()도 가능.
    collect() #collect : database backends -> tibble로 변환
mtcars_tibble

library(tidyverse)
# 데이터 로드
moving_data <- read_csv("./seoul_moving_202107_09_hr.csv")
reference_data <- readxl::read_excel("./reference.xlsx")

names(moving_data)  <- gsub(" ", "", names(moving_data)) #공백, 괄호 없애기.
names(moving_data)[9:10] <- c("평균이동시간_분", "이동인구_합")
names(reference_data) <- c("시도코드", "시군구코드", "시군구이름", "전체이름")

#Q. 두 데이터를 DB 파일에 등록해라.
# moving_data_0709 tablename
# reference_data tablename

dbWriteTable(con, 'reference_data', 
             reference_data)
dbWriteTable(con, 'moving_data_0709', 
             moving_data)
dbListTables(con)

copy_to(con, moving_data, "moving_data_0709",
        temporary = FALSE, #집어넣을 때 임시 메모리 쓸려면 True
        indexes = list( #책갈피 역할 
            #책갈피가 없어도 찾는데는 오래걸리나 찾긴함.
            #인덱스 지정(책갈피)해두면 금방 찾음.
            #단, 인덱스가 많아지면 테이블에 추가,수정에
            #쓰기속도가 느려짐. 
            #인덱스는 일종의 추가 테이블처럼 따로 저장됨.
            #디스크 용량이 늘어남.
            "대상연월", "요일", "도착시간",
            "출발시군구코드", "도착시군구코드"),
        overwrite = TRUE) #덮어쓰기를 하려면 TRUE를 써줘야 함. 아니면 애러.
dbListTables(con)
tbl(con, 'sqlite_stat4') 
#copy_to()를 돌리면서 지우고 다시 썼기 때문에 
#'sqlite_stat4' 같은 것들이 생기는 것임.

copy_to(con, reference_data, "reference_data",
        temporary = FALSE,
        indexes = list("시군구코드"),
        overwrite = TRUE)

dbListTables(con)

moving_db <- tbl(con, "moving_data_0709")
moving_db %>% head(6)

reference_db <- tbl(con, "reference_data")
reference_db


moving_db %<>% 
    mutate(평균이동시간_시 = 평균이동시간_분 / 60) %>% 
    mutate(여행타입 = 
               case_when(
                   between(평균이동시간_시, 0, 0.5) ~ "단기",
                   between(평균이동시간_시, 0.5, 1) ~ "중기",
                   평균이동시간_시 >= 1 ~ "장기",
                   TRUE ~ as.character(평균이동시간_시)
               )) %>% 
    relocate(여행타입)

moving_db %>% colnames()
moving_db %>% relocate(이동인구_합)
#이동인구_합이 chr이므로 mutate로 변경


reference_db %>% colnames()
moving_db %>% colnames()

as.integer(moving_data$이동인구_합)


top_six_list <- moving_db %>%
    mutate(이동인구_합 = as.integer(이동인구_합)) %>% 
    group_by(출발시군구코드) %>% 
    summarize(이동인구_합_구 = sum(이동인구_합)) %>% #출발 시군구코드 별로 이동인구 합 구한것임.
    slice_max(이동인구_합_구, n = 6) %>%
    select(출발시군구코드) %>%
    left_join(reference_db,
              by = c("출발시군구코드" = "시군구코드")) %>% 
    select("시군구이름") %>% 
    collect() %>% pull()

#위 코드가 어렵다면? 
# 예시 데이터프레임
df <- tibble( 지역 = c("서울", "서울", "부산", "부산", "부산"),
              인구 = c(950, 1020, 340, 360, 390))
df %>%
    group_by(지역) %>%
    summarize(평균인구 = mean(인구))


moving_db %>% colnames()

cummute_seoul <- moving_db %>% 
    mutate(나이 = as.integer(나이)) %>% 
    filter(나이 >= 70) %>% 
    filter(이동유형 %in% c("HH", "HW", "WH", "WE")) %>% 
    mutate(그룹이동시간 = 평균이동시간_분 * 이동인구_합) %>% 
    group_by(출발시군구코드, 이동유형) %>% 
    summarize(이동인구 = sum(이동인구_합, na.rm = TRUE),
            총이동시간 = sum(그룹이동시간, na.rm = TRUE)) %>% 
    mutate(평균이동시간 = 총이동시간 / 이동인구) %>% 
    left_join(reference_db,
              by = c("출발시군구코드" = "시군구코드")) %>% 
    filter(시도코드 == 11000) %>% 
    filter(시군구이름 %in% top_six_list) %>% 
    ungroup() %>% 
    select(시군구이름, 이동유형, 이동인구, 평균이동시간) %>% 
    collect()


cummute_seoul %>%
    group_by(시군구이름) %>% 
    mutate(이동유형 = factor(이동유형),
           시군구이름 = factor(시군구이름,
                               levels = top_six_list)) %>% 
    mutate(percentage = 이동인구 / sum(이동인구),
           .after = "이동인구") %>% 
    ggplot(aes(x = 이동유형, y = percentage * 100)) +
    geom_bar(stat = "identity",
             aes(fill = 이동유형)) +
    geom_label(aes(x = 이동유형, 
                   label = paste0(round(percentage * 100, digits = 2), "%") )) +
    facet_wrap(~시군구이름) +
    labs(title = "서울시 구별 아침 출근 시간 분포도",
         subtitle = "이동 빈도 상위 6개구 (70세 이상)") +
    ylab("퍼센트 (%)") +
    xlab("이동 유형별 분류") +
    theme_bw()

