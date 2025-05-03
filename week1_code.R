library(tidyverse)
library(magrittr)

#read_csv() tidyverse
#read.csv() base
#tidyverse에서 함수이름에는 . 을 쓰지않는다.
#tidyverser패키지 내 또 다른 readxl의 패키지가 있음. read_excel은 readxl안의 함수임.
#잘 쓰이지 않는 것은 ::을 또 써줘야 하긴함. -> 중요패키지만 로드 됨.
#Q :: 두개의 의미는? 패키지내 함수 호출 기능.
moving_data <- read_csv("./seoul_moving_202107_09_hr.csv")
reference_data <- readxl::read_excel("./reference.xlsx")

moving_data %>% dim()
reference_data %>% dim()

moving_data %>% head()
moving_data %>% names()

moving_data %>% glimpse()
reference_data %>% names
reference_data %>% glimpse()

#library(janitor)
# reference_data
origin_name_reference <- names(reference_data)
#clean_names : 자동으로 영문네임으로 익스체인지 해준다.
#디폴트 값이 영문 첫글자가 소문자라서 코드 실수 줄여줌.
reference_data <- janitor::clean_names(reference_data)
reference_data %>% names()


# moving_data
origin_name_moving <- names(moving_data)
moving_data <- janitor::clean_names(moving_data)
moving_data %>% names()

#unique(reference_data$sido)
#output -> vector form
#distinct : data.frame(tibble) form
reference_data %>%
    distinct(sido, .keep_all = T)

reference_data %>%
    distinct(sido) %>%
    count()

#Q. moving_data에는 어떤 나이대가 있는지 체크해보시오.
moving_data %>% 
    distinct(nai)

moving_data %>%
    filter(yoil == "일") %>% 
    count()

#Q. moving_data에서 화요일 이동한 표본 중 이동시간이
#   30분이상인 데이터의 개수는?
moving_data %>% 
    filter((yoil == '화') & (pyeong_gyun_idong_sigan_bun >= 30 )) %>% 
    count()

#Q. 이동시간이 30분이상 데이터 중
#   중랑구와 성북구에서 출발한 데이터는?
moving_data %>% glimpse()
reference_data %>% glimpse()
reference_data %>% 
    filter(name %in% c('중랑구','성북구'))

moving_data %>% 
    filter((moving_data$pyeong_gyun_idong_sigan_bun >= 30) & (chulbal_sigungu_kodeu %in% c(11070,11080))) %>% 
    count()

#다른풀이
moving_data %>% 
filter((moving_data$pyeong_gyun_idong_sigan_bun >= 30) & (chulbal_sigungu_kodeu == 11070 | 
                                                              chulbal_sigungu_kodeu == 11080)) %>% 
    count()

moving_data %>% 
    filter(between(nai, 10, 20)) %>% 
    count()

moving_data %>% 
    filter((nai >= 10) & (nai <= 20)) %>% 
    count()
#moving_data %>% glimpse()
#moving_data$nai <- as.numeric(moving_data$nai)

#slice() : head()라는 base function의 확장버전.
reference_data %>% 
    slice(1:6) #head(1:6)랑 같음 

reference_data %>% 
    slice(10:n()) #10~마지막까지 짤라올때 이렇게 진행. n() : 마지막을 의미.

moving_data %>% glimpse()

#나이대 중에서 탑5만 출력해보자.
moving_data %>%
    mutate(nai = as.numeric(nai)) %>% 
distinct(nai) %>%
    slice_max(nai, n = 5)

#select : 열 선택법
reference_data %>% 
    select(sido, sigungu)

#아래와 같이 묶음[vec]으로도 가져올 수 있음.
reference_data %>% 
    select(sido:name)

#컬럼 위치 조정
reference_data %>% 
    select(full_name, everything())

reference_data %>% 
    select(sigungu :last_col())

#컬럼 첫 문자를 가지고 추출하는 방법
reference_data %>% 
    select(starts_with("s"))

#나만의 열을 생성(mutate)하는 방법
# new = old 라는 문법을 따름.

moving_data %>% 
    mutate(idong_sigan_hr =
           pyeong_gyun_idong_sigan_bun/60) %>% 
    select(idong_sigan_hr, everything())

#Q. 년도정보만 빼내오시오.
#   새로운 년도 열 생성하기.
#   year라는 컬럼을 하나 만들자.
substr(moving_data$daesang_yeon_wol,start = 1,stop = 4) %>%  as.numeric()
moving_data %>% 
    mutate(year = substr(daesang_yeon_wol, start=1 ,stop = 4) %>% 
               as.numeric() ) %>% 
    select(year, everything())

#새로운 컬럼에 대한 조정은 반드시 다시 대입을 해줘야 한다.

moving_data %<>% 
    mutate(year = substr(daesang_yeon_wol, start=1 ,stop = 4) %>% 
               as.numeric() ) %>% 
    select(year, everything())
moving_data %>% slice(1:6)

moving_data %>% 
    mutate(year_plus1 = year +1, 
           .keep = 'used') %>% 
select(year_plus1,everything())#mutate에 사용된 컬럼 변수들만 가져와줘. : used

moving_data %>% 
    mutate(year_plus1 = year +1, 
           .keep = 'unused') %>% 
    select(year_plus1,everything())#mutate에 사용된 컬럼 변수들은 이제 필요없어.
#그러니까 기존꺼는 삭제하고 신규 컬럼을 대신 사용해줘. : unused

moving_data %>% 
    mutate(year_plus1 = year +1, 
           .keep = 'none') %>% 
    select(year_plus1,everything())#나는 그냥 그 새로운 하나의 컬럼만 원해. : none

#Stingr 패키지
reference_data$full_name %>% 
    str_split_fixed(pattern = " ", 2) #행렬형태로 " "(공백)을 기준으로 2개로 나눠 줘.

#예로들어, 시,도에 대한 데이터만 가져오려면 위에처럼 분할시키고
str_split_fixed(reference_data$full_name,pattern = " ", 2)[,1]

#시도이름과 코드만으로 이루어진 데이터프레임(tibble)을 생성하려면
reference_data %>% 
    mutate(sido_name = 
               str_split_fixed(full_name, 
                               pattern = " ", 2)[ ,1]) %>% 
    select(sido_name, sido) %>% 
    distinct()

#아래도 가능하나 좀 번잡함.
data.frame(sido = unique(reference_data$sido),
           sido_name = (unique(str_split_fixed((reference_data$full_name),
                                               pattern = ' ',n = 2)[,1])))

#rename
reference_data %<>% 
    rename(sido_code = sido)
 
reference_data %>% 
    rename_with(toupper)

reference_data %>% names() %>% toupper() #toupper는 모두 대문자로 바꾸는 코드

#컬럼 위치 조정
#앞서, select로 조정하는 방법을 배웠다.
moving_data %>% 
    select(yoil,everything())
#아래는 relocate로 바꾸는 방식이다. 같은 방식이 존재하나 왜 배우냐면
#before,after,last_col(),where(is.charater)[변수타입으로 조정가능]을 사용 할 수 있음.

moving_data %>% 
    relocate(yoil, .after = daesang_yeon_wol) %>% head(2) #대상연월 뒤로 보낼 때

moving_data %>% 
    relocate(yoil, .before = daesang_yeon_wol) %>% head(2) #대상연월 앞으로 보낼 때

moving_data %>% 
    relocate(yoil, .before = last_col()) %>% head(2) %>% 
    names()#마지막 컬럼 앞으로 보낼 때 

moving_data %>% 
    relocate(yoil, where(is.character))  #캐릭터인 애들만 앞으로 뺄 수 있음.

#arrange
moving_data$yoil %>% as.factor() # Levels: 금 목 수 월 일 토 화

#levels 지정하고 그 순서대로 정렬하기.
moving_data %>% 
    mutate(yoil= as.factor(yoil) %>% 
    lvls_revalue(c('월', '화', '수', '목',
                   '금', '토', '일'))) %>% 
    arrange(yoil) %>% 
    relocate(yoil)

#수치형에서는 아래와 같이 간단.
reference_data %>% 
    arrange(desc(sigungu))

#full_name(ㄱ,ㄴ,ㄷ,..)으로 시작하되, desc을 지키면서 정렬
reference_data %>% 
    arrange(full_name, desc(sigungu))
