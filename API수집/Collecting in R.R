---
  title: "01_데이터 수집"
output: html_document
---
  
#{r echo=FALSE}
rm(list = ls())

library(tidyverse)
library(jsonlite)
library(httr)

"
추가파라미터 유무로 구분하여 수집

* API_LIST : 추가파라미터 없음
* API_LIST_ADD_PARAMS : 추가파라미터 존재

"
# {r}
# 요청 URL 데이터셋
API_LIST_ALL <- read_tsv("data/API_LIST.tsv") %>%
  mutate(
    제공_시작 = str_replace(제공_시작, "년", "") %>% as.numeric(),
    제공_끝   = str_replace(제공_끝  , "년", "") %>% as.numeric()
  )

API_LIST <- API_LIST_ALL 

"
# API 호출 명세

변수명    | 요청인자타입    | 설명                 |비고
----------|-----------------|----------------------|------------------------
  Key       | STRING(필수)    | 인증키               |기본값 : sample key
Type      | STRING(필수)    | 호출 문서(xml, json) |기본값 : XML
pIndex    | INTEGER(필수)   | 페이지 위치          |기본값 : 1(sample key 1 고정)
pSize     | INTEGER(필수)   | 페이지 당 요청 숫자	 |기본값 : 100(sample key 5고정)


"
{r}
# request parameter 구조
# http://openapi.openfiscaldata.go.kr/ExpenditureBudgetInit3

# 고정
BASE_URL = "http://openapi.openfiscaldata.go.kr/"
Key  = "RAQNB1000078820210822054406IDJYX"
Type = "json"


### 호출테스트

{r}
# 가변
pIndex = 1      # 페이지번호
pSize  = 1000    # 한페이지당 최대 허용 instance 개수 1000개

# 회계년도
# i <- 1
REQ_ID <- API_LIST[5,]$REQ_ID
FSCL_YY_ST <- API_LIST[5,]$제공_시작
FSCL_YY_ED <- API_LIST[5,]$제공_끝
FSCL_YY_LIST <- FSCL_YY_ST:FSCL_YY_ED

FSCL_YY = 2013  # 회계기준년도

# API 요청
URL <- paste0(  BASE_URL, REQ_ID , "?" , "Key=" , Key, "&Type=" , Type, "&FSCL_YY=", FSCL_YY, "&pIndex=", pIndex, "&pSize=", pSize )

fromJSON(URL) 




# 데이터 수집


# {r echo=FALSE, eval=FALSE, render=FALSE}
# parameter
BASE_URL = "http://openapi.openfiscaldata.go.kr/"
Key  = "RAQNB1000078820210822054406IDJYX"
Type = "json"


# API 리스트별
for (i in 1:nrow(API_LIST)) {
  # i 번째 API에 대하여,
  REQ_ID <- API_LIST[i,]$REQ_ID
  FSCL_YY_ST <- API_LIST[i,]$제공_시작
  FSCL_YY_ED <- API_LIST[i,]$제공_끝
  
  # 기준년도 별
  for (FSCL_YY in FSCL_YY_ST:FSCL_YY_ED) {
    pIndex   <- 1       # 페이지번호
    pSize    <- 1000    # 한페이지당 최대 허용개수 1000개
    
    # 데이터셋 저장 파일 경로 설정
    file_path <- paste0("data/API_RESPONSE/", REQ_ID, "_", FSCL_YY, ".tsv") 
    
    # 1000건씩 반복실행
    repeat{
      
      
      # 실행문 
      URL <- NULL
      
      # 추가 파라미터에 따라 url 분기
      if (REQ_ID == "TotalExpenditure3") {
        URL <- paste0(  BASE_URL, REQ_ID , "?" , "Key=" , Key, "&Type=" , Type, "&FSCL_YY=", FSCL_YY, "&pIndex=", pIndex, "&pSize=", pSize, "&ANEXP_INQ_STND_CD=1", "&BDG_FND_DIV_CD=0" )
      } else if(REQ_ID == "TotalExpenditure4"){
        URL <- paste0(  BASE_URL, REQ_ID , "?" , "Key=" , Key, "&Type=" , Type, "&FSCL_YY=", FSCL_YY, "&pIndex=", pIndex, "&pSize=", pSize, "&ANEXP_INQ_STND_CD=1", "&BDG_FND_DIV_CD=0"  )
      } else if(REQ_ID == "TotalRevenue"){
        URL <- paste0(  BASE_URL, REQ_ID , "?" , "Key=" , Key, "&Type=" , Type, "&FSCL_YY=", FSCL_YY, "&pIndex=", pIndex, "&pSize=", pSize, "&REVN_INQ_STND_CD=1", "&BDG_FND_DIV_CD=0" )
      } else {
        URL <- paste0(  BASE_URL, REQ_ID , "?" , "Key=" , Key, "&Type=" , Type, "&FSCL_YY=", FSCL_YY, "&pIndex=", pIndex, "&pSize=", pSize )
      }
      
      response <- fromJSON(URL)
      
      
      # 응답결과확인. 결과코드 INFO-000 이 정상
      response_code     <- response[[1]]$head[[1]][2][2,1][1,1]  
      response_totalCnt <- response[[1]]$head[[1]][1,1]      
      
      
      # response result logging
      paste(REQ_ID,"_",FSCL_YY,",", URL,",", response_code, "[",pIndex*pSize,"/",response_totalCnt,"]" ,sep = "") %>%
        write(., file="data/API_RESPONSE_LOG.txt", append=TRUE)
      
      
      # 응답결과가 null인 경우 out of loop
      if (is.null(response_code) ) {
        break
      }
      
      
      # 성공 file write
      if (response_code == "INFO-000") {
        response_payload <- response[[1]]$row[[2]] # dataset
        is_file_exists <- file.exists(file_path)
        write_tsv(response_payload , file = file_path, append = is_file_exists, col_names = !is_file_exists)
      }
      
      
      # 마지막 페이징일때 out of loop
      if( response_totalCnt < (pIndex*pSize) ){
        break
      }else{
        pIndex <- pIndex+1
      }
    }
  }
  
  
  # console logging
  paste0("[",i,"] ",REQ_ID, "_", FSCL_YY ," complete..!") %>% print()
}


# 파일병합
# {r echo=FALSE, eval=FALSE, render=FALSE}
rm(list = ls())
folder_path <- "data/API_RESPONSE/"

api_list <- read_tsv( "data/API_LIST.tsv" ) %>%
  select(REQ_ID)

file_list <- tibble(FILE_NM = list.files(folder_path))

for (req_id in api_list$REQ_ID) {
  
  subset <- file_list %>% 
    filter(  str_detect(FILE_NM, req_id)  ) %>%
    mutate(FILE_NM = paste0(folder_path, FILE_NM))
  
  save_path <- paste0("data/",req_id,".tsv" )
  
  for (fnm in subset$FILE_NM) {
    
    is_file_exists <- save_path %>% file.exists()
    fnm %>% read_tsv() %>% write_tsv( ., file = save_path, append = is_file_exists, col_names = !is_file_exists)
  }
}

rm(list = ls())
gc()
```

















