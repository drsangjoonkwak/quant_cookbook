# http://comp.fnguide.com/SVO2/ASP/SVD_Finance.asp?pGB=1&gicode=A005930

library(httr)
library(rvest)

ifelse(dir.exists('data/KOR_fs'), FALSE,
       dir.create('data/KOR_fs'))

Sys.setlocale("LC_ALL", "English")

url = paste0('http://comp.fnguide.com/SVO2/ASP/SVD_Finance.asp?pGB=1&gicode=A005930')

data = GET(url,
           user_agent('Mozilla/5.0 (Windows NT 10.0; Win64; x64)
                      AppleWebKit/537.36 (KHTML, like Gecko) Chrome/70.0.3538.77 Safari/537.36'))
data = data %>%
  read_html() %>%
  html_table()

Sys.setlocale("LC_ALL", "Korean")

lapply(data, function(x) {
  head(x, 3)})

data_IS = data[[1]]
data_BS = data[[3]]
data_CF = data[[5]]

print(names(data_IS))

data_IS = data_IS[, 1:(ncol(data_IS)-2)]

data_fs = rbind(data_IS, data_BS, data_CF) %>% data.frame()
data_fs[, 1] = gsub('계산에 참여한 계정 펼치기',
                    '', data_fs[, 1])
data_fs = data_fs[!duplicated(data_fs[, 1]), ]

rownames(data_fs) = NULL
rownames(data_fs) = data_fs[, 1]
data_fs[, 1] = NULL

data_fs = data_fs[, substr(colnames(data_fs), 7, 8) == '12']

print(head(data_fs))

sapply(data_fs, typeof)

# change character to numbers
library(stringr)

data_fs = sapply(data_fs, function(x) {
  str_replace_all(x, ',', '') %>%
    as.numeric()
}) %>%
  data.frame(., row.names = rownames(data_fs))

print(head(data_fs))

sapply(data_fs, typeof)

#####################
for(i in 1 : nrow(KOR_ticker) ) {
  
  price = xts(NA, order.by = Sys.Date()) # 빈 시계열 데이터 생성
  name = KOR_ticker$'종목코드'[i] # 티커 부분 선택
  
  from = (Sys.Date() - years(3)) %>% str_remove_all('-') # 시작일
  to = Sys.Date() %>% str_remove_all('-') # 종료일
  
  # 오류 발생 시 이를 무시하고 다음 루프로 진행
  tryCatch({
    # url 생성
    url = paste0('https://fchart.stock.naver.com/siseJson.nhn?symbol=', name,
                 '&requestType=1&startTime=', from, '&endTime=', to, '&timeframe=day')
    
    # 이 후 과정은 위와 동일함
    # 데이터 다운로드
    data = GET(url)
    data_html = data %>% read_html %>%
      html_text() %>%
      read_csv()
    
    # 필요한 열만 선택 후 클렌징
    price = data_html[c(1, 5)]
    colnames(price) = (c('Date', 'Price'))
    price = na.omit(price)
    price$Date = parse_number(price$Date)
    price$Date = ymd(price$Date)
    price = tk_xts(price, date_var = Date)
    
  }, error = function(e) {
    
    # 오류 발생시 해당 종목명을 출력하고 다음 루프로 이동
    warning(paste0("Error in Ticker: ", name))
  })
  
  # 다운로드 받은 파일을 생성한 폴더 내 csv 파일로 저장
  write.csv(data.frame(price),
            paste0('data/KOR_price/', name, '_price.csv'))
  
  # 타임슬립 적용
  Sys.sleep(2)
}