# 금융 데이터 수집하기 (기본)

API와 크롤링을 이용한다면 비용을 지불하지 않고 얼마든지 금융 데이터를 수집할 수있습니다. 이 CHAPTER에서는 금융 데이터를 받기 위해 필요한 주식티커를 구하는 방법과 섹터별 구성종목을 크롤링하는 방법을 알아보겠습니다.

## 한국거래소의 산업별 현황 및 개별지표 크롤링

앞 CHAPTER의 예제를 통해 네이버 금융에서 주식티커를 크롤링하는 방법을 살펴보았습니다. 그러나 이 방법은 지나치게 복잡하고 시간이 오래 걸립니다. 반면 한국거래소에서 제공하는 업종분류 현황과 개별종목 지표 데이터를 이용하면 훨씬 간단하게 주식티커 데이터를 수집할 수 있습니다.

- KRX 정보데이터시스템 http://data.krx.co.kr/ 에서 [기본통계 → 주식 → 세부안내] 부분
- [12025] 업종분류 현황 
- [12021] 개별종목

해당 데이터들을 크롤링이 아닌 [Excel] 버튼을 클릭해 엑셀 파일로 받을 수도 있습니다. 그러나 매번 엑셀 파일을 다운로드하고 이를 R로 불러오는 작업은 상당히 비효율적이며, 크롤링을 이용한다면 해당 데이터를 R로 직접 불러올 수 있습니다.

### 업종분류 현황 크롤링

먼저 업종분류 현황에 해당하는 페이지에 접속한 후 개발자 도구 화면을 열고 [다운로드] 버튼을 클릭한 후 [CSV]를 누릅니다. [Network] 탭에는 generate.cmd와 download.cmd 두 가지 항목이 있습니다. 거래소에서 엑셀 데이터를 받는 과정은 다음과 같습니다.

1. http://data.krx.co.kr/comm/fileDn/download_excel/download.cmd 에 원하는 항목을 쿼리로 발송하면 해당 쿼리에 해당하는 OTP(generate.cmd)를 받게 됩니다.

2. 부여받은 OTP를 **http://data.krx.co.kr/**에 제출하면 이에 해당하는 데이터(download.cmd)를 다운로드하게 됩니다.

먼저 1번 단계를 살펴보겠습니다.

<div class="figure" style="text-align: center">
<img src="images/crawl_practice_krx_sector.png" alt="OTP 생성 부분" width="100%" />
<p class="caption">(\#fig:unnamed-chunk-1)OTP 생성 부분</p>
</div>

General 항목의 Request URL의 앞부분이 원하는 항목을 제출할 주소입니다. Form Data에는 우리가 원하는 항목들이 적혀 있습니다. 이를 통해 POST 방식으로 데이터를 요청함을 알 수 있습니다.

다음으로 2번 단계를 살펴보겠습니다.

<div class="figure" style="text-align: center">
<img src="images/crawl_practice_krx_sector2.png" alt="OTP 제출 부분" width="100%" />
<p class="caption">(\#fig:unnamed-chunk-2)OTP 제출 부분</p>
</div>

General 항목의 Request URL은 OTP를 제출할 주소입니다. Form Data의 OTP는 1번 단계에서 부여받은 OTP에 해당합니다. 이 역시 POST 방식으로 데이터를 요청합니다.

위 과정을 코드로 나타내면 다음과 같습니다.


```r
library(httr)
library(rvest)
library(readr)

gen_otp_url =
  'http://data.krx.co.kr/comm/fileDn/GenerateOTP/generate.cmd'
gen_otp_data = list(
  mktId = 'STK',
  trdDd = '20210108',
  money = '1',
  csvxls_isNo = 'false',
  name = 'fileDown',
  url = 'dbms/MDC/STAT/standard/MDCSTAT03901'
)
otp = POST(gen_otp_url, query = gen_otp_data) %>%
  read_html() %>%
  html_text()
```

1. gen_otp_url에 원하는 항목을 제출할 URL을 입력합니다.
2. 개발자 도구 화면에 나타는 쿼리 내용들을 리스트 형태로 입력합니다. 이 중 mktId의 STK는 코스피에 해당하는 내용이며, 코스닥 데이터를 받고자 할 경우 KSQ를 입력해야 합니다.
3. `POST()` 함수를 통해 해당 URL에 쿼리를 전송하면 이에 해당하는 데이터를 받게 됩니다.
4. `read_html()`함수를 통해 HTML 내용을 읽어옵니다.
5. `html_text()` 함수는 HTML 내에서 텍스트에 해당하는 부분만을 추출합니다. 이를 통해 OTP 값만 추출하게 됩니다.

위의 과정을 거쳐 생성된 OTP를 제출하면, 우리가 원하는 데이터를 다운로드할 수 있습니다.


```r
down_url = 'http://data.krx.co.kr/comm/fileDn/download_csv/download.cmd'
down_sector_KS = POST(down_url, query = list(code = otp),
                   add_headers(referer = gen_otp_url)) %>%
  read_html(encoding = 'EUC-KR') %>%
  html_text() %>%
  read_csv()
```

1. OTP를 제출할 URL을 down_url에 입력합니다.
2. `POST()` 함수를 통해 위에서 부여받은 OTP 코드를 해당 URL에 제출합니다.
3. `add_headers()` 구문을 통해 리퍼러(referer)를 추가해야 합니다. 리퍼러란 링크를 통해서 각각의 웹사이트로 방문할 때 남는 흔적입니다. 거래소 데이터를 다운로드하는 과정을 살펴보면 첫 번째 URL에서 OTP를 부여받고, 이를 다시 두번째 URL에 제출했습니다. 그런데 이러한 과정의 흔적이 없이 OTP를 바로 두번째 URL에 제출하면 서버는 이를 로봇으로 인식해 데이터를 반환하지 않습니다. 따라서 `add_headers()` 함수를 통해 우리가 거쳐온 과정을 흔적으로 남겨
야 데이터를 반환하게 되며 첫 번째 URL을 리퍼러로 지정해줍니다.
4. `read_html()`과 `html_text()` 함수를 통해 텍스트 데이터만 추출합니다. EUC-KR로 인코딩이 되어 있으므로 `read_html()` 내에 이를 입력해줍니다.
5. `read_csv()` 함수는 csv 형태의 데이터를 불러옵니다. 


```r
print(down_sector_KS)
```

```
## # A tibble: 917 x 8
##    종목코드 종목명 시장구분 업종명   종가  대비 등락률
##    <chr>    <chr>  <chr>    <chr>   <dbl> <dbl>  <dbl>
##  1 095570   AJ네트웍… KOSPI    서비스업…   4540  -155  -3.3 
##  2 006840   AK홀딩스… KOSPI    기타금융…  25350   150   0.6 
##  3 027410   BGF    KOSPI    기타금융…   4905   -25  -0.51
##  4 282330   BGF리테… KOSPI    유통업 141000  4500   3.3 
##  5 138930   BNK금융… KOSPI    기타금융…   5780     0   0   
##  6 001460   BYC    KOSPI    섬유의복… 324500 10500   3.34
##  7 001465   BYC우  KOSPI    섬유의복… 157500 10000   6.78
##  8 001040   CJ     KOSPI    기타금융… 102500  7600   8.01
##  9 079160   CJ CGV KOSPI    서비스업…  26150   300   1.16
## 10 00104K   CJ4우(… KOSPI    기타금융…  81400  5300   6.96
## # … with 907 more rows, and 1 more variable:
## #   시가총액 <dbl>
```

위 과정을 통해 down_sector 변수에는 산업별 현황 데이터가 저장되었습니다. 코스닥 시장의 데이터도 다운로드 받도록 하겠습니다.


```r
gen_otp_data = list(
  mktId = 'KSQ', # 코스닥으로 변경
  trdDd = '20210108',
  money = '1',
  csvxls_isNo = 'false',
  name = 'fileDown',
  url = 'dbms/MDC/STAT/standard/MDCSTAT03901'
)
otp = POST(gen_otp_url, query = gen_otp_data) %>%
  read_html() %>%
  html_text()

down_sector_KQ = POST(down_url, query = list(code = otp),
                   add_headers(referer = gen_otp_url)) %>%
  read_html(encoding = 'EUC-KR') %>%
  html_text() %>%
  read_csv()
```

코스피 데이터와 코스닥 데이터를 하나로 합치도록 합니다.


```r
down_sector = rbind(down_sector_KS, down_sector_KQ)
```


이를 csv 파일로 저장하겠습니다.


```r
ifelse(dir.exists('data'), FALSE, dir.create('data'))
write.csv(down_sector, 'data/krx_sector.csv')
```

먼저 `ifelse()` 함수를 통해 data라는 이름의 폴더가 있으면 FALSE를 반환하고, 없으면 해당 이름으로 폴더를 생성해줍니다. 그 후 앞서 다운로드한 데이터를 data 폴더 안에 krx_sector.csv 이름으로 저장합니다. 해당 폴더를 확인해보면 데이터가 csv 형태로 저장되어 있습니다.

### 개별종목 지표 크롤링

개별종목 데이터를 크롤링하는 방법은 위와 매우 유사하며, 요청하는 쿼리 값에만 차이가 있습니다. 개발자 도구 화면을 열고 [CSV] 버튼을 클릭해 어떠한 쿼리를 요청하는지 확인합니다.

<div class="figure" style="text-align: center">
<img src="images/crawl_practice_krx_ind.png" alt="개별지표 OTP 생성 부분" width="100%" />
<p class="caption">(\#fig:unnamed-chunk-9)개별지표 OTP 생성 부분</p>
</div>

이 중 tboxisuCd_finder_stkisu0_6, isu_Cd, isu_Cd2 등의 항목은 조회 구분의 개별추이 탭에 해당하는 부분이므로 우리가 원하는 전체 데이터를 받을 때는 필요하지 않은 요청값입니다. 이를 제외한 요청값을 산업별 현황 예제에 적용하면 해당 데이터 역시 손쉽게 다운로드할 수 있습니다.


```r
library(httr)
library(rvest)
library(readr)

gen_otp_url =
  'http://data.krx.co.kr/comm/fileDn/GenerateOTP/generate.cmd'
gen_otp_data = list(
  searchType = '1',
  mktId = 'ALL',
  trdDd = '20210108',
  csvxls_isNo = 'false',
  name = 'fileDown',
  url = 'dbms/MDC/STAT/standard/MDCSTAT03501'
)
otp = POST(gen_otp_url, query = gen_otp_data) %>%
  read_html() %>%
  html_text()

down_url = 'http://data.krx.co.kr/comm/fileDn/download_csv/download.cmd'
down_ind = POST(down_url, query = list(code = otp),
                add_headers(referer = gen_otp_url)) %>%
  read_html(encoding = 'EUC-KR') %>%
  html_text() %>%
  read_csv()
```


```r
print(down_ind)
```

```
## # A tibble: 2,345 x 11
##    종목코드 종목명   종가  대비 등락률   EPS    PER
##    <chr>    <chr>   <dbl> <dbl>  <dbl> <dbl>  <dbl>
##  1 060310   3S       2245   -45  -1.97    NA  NA   
##  2 095570   AJ네트웍…   4540  -155  -3.3    982   4.62
##  3 006840   AK홀딩스…  25350   150   0.6   2168  11.7 
##  4 054620   APS홀딩…   7500  -150  -1.96    NA  NA   
##  5 265520   AP시스템…  26000  -100  -0.38   671  38.8 
##  6 211270   AP위성   8100  -250  -2.99    51 159.  
##  7 027410   BGF      4905   -25  -0.51   281  17.5 
##  8 282330   BGF리테… 141000  4500   3.3   8763  16.1 
##  9 138930   BNK금융…   5780     0   0     1647   3.51
## 10 001460   BYC    324500 10500   3.34 33265   9.75
## # … with 2,335 more rows, and 4 more variables:
## #   BPS <dbl>, PBR <dbl>, 주당배당금 <dbl>,
## #   배당수익률 <dbl>
```

위 과정을 통해 down_ind 변수에는 개별종목 지표 데이터가 저장되었습니다. 해당 데이터 역시 csv 파일로 저장하겠습니다.


```r
write.csv(down_ind, 'data/krx_ind.csv')
```

### 최근 영업일 기준 데이터 받기

위 예제의 쿼리 항목 중 date와 schdate 부분을 원하는 일자로 입력하면(예: 20190104) 해당일의 데이터를 다운로드할 수 있으며, 전 영업일 날짜를 입력하면 가장 최근의 데이터를 받을 수 있습니다. 그러나 매번 해당 항목을 입력하기는 번거로우므로 자동으로 반영되게 할 필요가 있습니다.

네이버 금융의 [국내증시 → 증시자금동향]에는 이전 2영업일에 해당하는 날짜가 있으며, 자동으로 날짜가 업데이트되어 편리합니다. 따라서 해당 부분을 크롤링해 쿼리 항목에 사용할 수 있습니다.

<div class="figure" style="text-align: center">
<img src="images/crawl_practice_recentdate.png" alt="최근 영업일 부분" width="70%" />
<p class="caption">(\#fig:unnamed-chunk-13)최근 영업일 부분</p>
</div>

크롤링하고자 하는 데이터가 하나거나 소수일때는 HTML 구조를 모두 분해한 후 데이터를 추출하는 것보다 Xpath를 이용하는 것이 훨씬 효율적입니다. Xpath란 XML 중 특정 값의 태그나 속성을 찾기 쉽게 만든 주소라 생각하면 됩니다. 예를 들어 R 프로그램이 저장된 곳을 윈도우 탐색기를 이용해 이용하면 C:\\Program Files\\R\\R-3.4.2 형태의 주소를 보이는데 이것은 윈도우의 path 문법입니다. XML 역시 이와 동일한 개념의 Xpath가 있습니다. 웹페이지에서 Xpath를 찾는 법은 다음과 같습니다.

<div class="figure" style="text-align: center">
<img src="images/crawl_practice_xpath.png" alt="Xpath 복사하기" width="70%" />
<p class="caption">(\#fig:unnamed-chunk-14)Xpath 복사하기</p>
</div>

먼저 크롤링하고자 하는 내용에 마우스 커서를 올린 채 마우스 오른쪽 버튼을 클릭한 후 [검사]를 선택합니다. 그러면 개발자 도구 화면이 열리며 해당 지점의 HTML 부분이 선택됩니다. 그 후 HTML 화면에서 마우스 오른쪽 버튼을 클릭하고 [Copy → Copy Xpath]를 선택하면 해당 지점의 Xpath가 복사됩니다.


```css
//*[@id="type_0"]/div/ul[2]/li/span
```


<style type="text/css">
//*[@id="type_0"]/div/ul[2]/li/span
</style>

위에서 구한 날짜의 Xpath를 이용해 해당 데이터를 크롤링하겠습니다.


```r
library(httr)
library(rvest)
library(stringr)

url = 'https://finance.naver.com/sise/sise_deposit.nhn'

biz_day = GET(url) %>%
  read_html(encoding = 'EUC-KR') %>%
  html_nodes(xpath =
               '//*[@id="type_1"]/div/ul[2]/li/span') %>%
  html_text() %>%
  str_match(('[0-9]+.[0-9]+.[0-9]+') ) %>%
  str_replace_all('\\.', '')

print(biz_day)
```

```
## [1] "20210113"
```

1. 페이지의 url을 저장합니다.
2. `GET()` 함수를 통해 해당 페이지 내용을 받습니다.
3. `read_html()` 함수를 이용해 해당 페이지의 HTML 내용을 읽어오며, 인코딩은 EUC-KR로 설정합니다.
4. `html_node()` 함수 내에 위에서 구한 Xpath를 입력해서 해당 지점의 데이터를 추출합니다.
5. `html_text()` 함수를 통해 텍스트 데이터만을 추출합니다.
6. `str_match()` 함수 내에서 정규표현식^[특정한 규칙을 가진 문자열의 집합을 표현하는데 사용하는 형식 언어]을 이용해 숫자.숫자.숫자 형식의 데이터를 추출합니다.
7. `str_replace_all()` 함수를 이용해 마침표(.)를 모두 없애줍니다.

이처럼 Xpath를 이용하면 태그나 속성을 분해하지 않고도 원하는 지점의 데이터를 크롤링할 수 있습니다. 위 과정을 통해 yyyymmdd 형태의 날짜만 남게 되었습니다. 이를 위의 date와 schdate에 입력하면 산업별 현황과 개별종목 지표를 최근일자 기준으로 다운로드하게 됩니다. 전체 코드는 다음과 같습니다.


```r
library(httr)
library(rvest)
library(stringr)
library(readr)

# 최근 영업일 구하기
url = 'https://finance.naver.com/sise/sise_deposit.nhn'

biz_day = GET(url) %>%
  read_html(encoding = 'EUC-KR') %>%
  html_nodes(xpath =
               '//*[@id="type_1"]/div/ul[2]/li/span') %>%
  html_text() %>%
  str_match(('[0-9]+.[0-9]+.[0-9]+') ) %>%
  str_replace_all('\\.', '')

# 코스피 업종분류 OTP 발급
gen_otp_url =
    'http://data.krx.co.kr/comm/fileDn/GenerateOTP/generate.cmd'
gen_otp_data = list(
  mktId = 'STK',
  trdDd = biz_day, # 최근영업일로 변경
  money = '1',
  csvxls_isNo = 'false',
  name = 'fileDown',
  url = 'dbms/MDC/STAT/standard/MDCSTAT03901'
)
otp = POST(gen_otp_url, query = gen_otp_data) %>%
  read_html() %>%
  html_text()

# 코스피 업종분류 데이터 다운로드
down_url = 'http://data.krx.co.kr/comm/fileDn/download_csv/download.cmd'
down_sector_KS = POST(down_url, query = list(code = otp),
                   add_headers(referer = gen_otp_url)) %>%
  read_html(encoding = 'EUC-KR') %>%
  html_text() %>%
  read_csv()

# 코스닥 업종분류 OTP 발급
gen_otp_data = list(
  mktId = 'KSQ',
  trdDd = biz_day, # 최근영업일로 변경
  money = '1',
  csvxls_isNo = 'false',
  name = 'fileDown',
  url = 'dbms/MDC/STAT/standard/MDCSTAT03901'
)
otp = POST(gen_otp_url, query = gen_otp_data) %>%
  read_html() %>%
  html_text()

# 코스닥 업종분류 데이터 다운로드
down_sector_KQ = POST(down_url, query = list(code = otp),
                   add_headers(referer = gen_otp_url)) %>%
  read_html(encoding = 'EUC-KR') %>%
  html_text() %>%
  read_csv()

down_sector = rbind(down_sector_KS, down_sector_KQ)

ifelse(dir.exists('data'), FALSE, dir.create('data'))
write.csv(down_sector, 'data/krx_sector.csv')

# 개별종목 지표 OTP 발급
gen_otp_url =
  'http://data.krx.co.kr/comm/fileDn/GenerateOTP/generate.cmd'
gen_otp_data = list(
  searchType = '1',
  mktId = 'ALL',
  trdDd = biz_day, # 최근영업일로 변경
  csvxls_isNo = 'false',
  name = 'fileDown',
  url = 'dbms/MDC/STAT/standard/MDCSTAT03501'
)
otp = POST(gen_otp_url, query = gen_otp_data) %>%
  read_html() %>%
  html_text()

# 개별종목 지표 데이터 다운로드
down_url = 'http://data.krx.co.kr/comm/fileDn/download_csv/download.cmd'
down_ind = POST(down_url, query = list(code = otp),
                add_headers(referer = gen_otp_url)) %>%
  read_html(encoding = 'EUC-KR') %>%
  html_text() %>%
  read_csv()

write.csv(down_ind, 'data/krx_ind.csv')
```

### 거래소 데이터 정리하기

위에서 다운로드한 데이터는 중복된 열이 있으며, 불필요한 데이터 역시 있습니다. 따라서 하나의 테이블로 합친 후 정리할 필요가 있습니다. 먼저 다운로드한 csv 파일을 읽어옵니다.


```r
down_sector = read.csv('data/krx_sector.csv', row.names = 1,
                       stringsAsFactors = FALSE)
down_ind = read.csv('data/krx_ind.csv',  row.names = 1,
                    stringsAsFactors = FALSE)
```

`read.csv()` 함수를 이용해 csv 파일을 불러옵니다. `row.names = 1`을 통해 첫 번째 열을 행 이름으로 지정하고, `stringsAsFactors = FALSE`를 통해 문자열 데이터가 팩터 형태로 변형되지 않게 합니다.


```r
intersect(names(down_sector), names(down_ind))
```

```
## [1] "종목코드" "종목명"   "종가"     "대비"    
## [5] "등락률"
```

먼저 `intersect()` 함수를 통해 두 데이터 간 중복되는 열 이름을 살펴보면 종목코드와 종목명 등이 동일한 위치에 있습니다.


```r
setdiff(down_sector[, '종목명'], down_ind[ ,'종목명'])
```

```
##  [1] "ESR켄달스퀘어리츠"  "NH프라임리츠"      
##  [3] "롯데리츠"           "맥쿼리인프라"      
##  [5] "맵스리얼티1"        "모두투어리츠"      
##  [7] "미래에셋맵스리츠"   "바다로19호"        
##  [9] "베트남개발1"        "신한알파리츠"      
## [11] "에이리츠"           "엘브이엠씨홀딩스"  
## [13] "이리츠코크렙"       "이지스레지던스리츠"
## [15] "이지스밸류리츠"     "제이알글로벌리츠"  
## [17] "케이탑리츠"         "코람코에너지리츠"  
## [19] "하이골드12호"       "하이골드3호"       
## [21] "한국ANKOR유전"      "한국패러랠"        
## [23] "GRT"                "JTC"               
## [25] "SBI핀테크솔루션즈"  "SNK"               
## [27] "골든센츄리"         "글로벌에스엠"      
## [29] "넥스틴"             "뉴프라이드"        
## [31] "로스웰"             "미투젠"            
## [33] "소마젠(Reg.S)"      "씨케이에이치"      
## [35] "에스앤씨엔진그룹"   "엑세스바이오"      
## [37] "오가닉티코스메틱"   "윙입푸드"          
## [39] "이스트아시아홀딩스" "잉글우드랩"        
## [41] "컬러레이"           "코오롱티슈진"      
## [43] "크리스탈신소재"     "헝셩그룹"
```

`setdiff()` 함수를 통해 두 데이터에 공통적으로 없는 종목명, 즉 하나의 데이터에만 있는 종목을 살펴보면 위와 같습니다. 해당 종목들은 선박펀드, 광물펀드, 해외종목 등 일반적이지 않은 종목들이므로 제외하는 것이 좋습니다. 따라서 둘 사이에 공통적으로 존재하는 종목을 기준으로 데이터를 합쳐주겠습니다.


```r
KOR_ticker = merge(down_sector, down_ind,
                   by = intersect(names(down_sector),
                                  names(down_ind)),
                   all = FALSE
                   )
```

`merge()` 함수는 by를 기준으로 두 데이터를 하나로 합치며, 공통으로 존재하는 종목코드, 종목명, 종가, 대비, 등락률을 기준으로 입력해줍니다. 또한 all 값을 TRUE로 설정하면 합집합을 반환하고, FALSE로 설정하면 교집합을 반환합니다. 공통으로 존재하는 항목을 원하므로 여기서는 FALSE를 입력합니다.


```r
KOR_ticker = KOR_ticker[order(-KOR_ticker['시가총액']), ]
print(head(KOR_ticker))
```

```
##      종목코드           종목명    종가  대비 등락률
## 327    005930         삼성전자   89700  -900  -0.99
## 45     000660       SK하이닉스  133000  4000   3.10
## 1076   051910           LG화학 1000000 38000   3.95
## 328    005935       삼성전자우   78600 -1400  -1.75
## 299    005380           현대차  259000 -2000  -0.77
## 1928   207940 삼성바이오로직스  830000 12000   1.47
##      시장구분   업종명  시가총액   EPS    PER    BPS
## 327     KOSPI 전기전자 5.355e+14  3166  28.33  37528
## 45      KOSPI 전기전자 9.682e+13  2943  45.19  65836
## 1076    KOSPI     화학 7.059e+13  4085 244.80 217230
## 328     KOSPI 전기전자 6.468e+13    NA     NA     NA
## 299     KOSPI 운수장비 5.534e+13 11310  22.90 253001
## 1928    KOSPI   의약품 5.492e+13  3067 270.62  65812
##        PBR 주당배당금 배당수익률
## 327   2.39       1416       1.58
## 45    2.02       1000       0.75
## 1076  4.60       2000       0.20
## 328     NA       1417       1.80
## 299   1.02       4000       1.54
## 1928 12.61          0       0.00
```

데이터를 시가총액 기준으로 내림차순 정렬할 필요도 있습니다. `order()` 함수를 통해 상대적인 순서를 구할 수 있습니다. R은 기본적으로 오름차순으로 순서를 구하므로 앞에 마이너스(-)를 붙여 내림차순 형태로 바꿉니다. 결과적으로 시가총액 기준 내림차
순으로 해당 데이터가 정렬됩니다.

마지막으로 스팩, 우선주 종목 역시 제외해야 합니다.


```r
library(stringr)

KOR_ticker[grepl('스팩', KOR_ticker[, '종목명']), '종목명']  
```

```
##  [1] "엔에이치스팩14호"     "유안타제3호스팩"     
##  [3] "케이비제18호스팩"     "삼성스팩2호"         
##  [5] "교보8호스팩"          "엔에이치스팩17호"    
##  [7] "유안타제6호스팩"      "미래에셋대우스팩3호" 
##  [9] "케이비제20호스팩"     "DB금융스팩8호"       
## [11] "유안타제5호스팩"      "SK6호스팩"           
## [13] "대신밸런스제8호스팩"  "케이비17호스팩"      
## [15] "유안타제7호스팩"      "교보10호스팩"        
## [17] "IBKS제13호스팩"       "대신밸런스제7호스팩" 
## [19] "한화에스비아이스팩"   "미래에셋대우스팩 5호"
## [21] "하이제5호스팩"        "SK4호스팩"           
## [23] "신한제6호스팩"        "에이치엠씨제5호스팩" 
## [25] "한국제7호스팩"        "하나금융15호스팩"    
## [27] "유안타제4호스팩"      "한화플러스제1호스팩" 
## [29] "하나머스트제6호스팩"  "상상인이안1호스팩"   
## [31] "엔에이치스팩18호"     "IBKS제14호스팩"      
## [33] "하나금융14호스팩"     "엔에이치스팩16호"    
## [35] "미래에셋대우스팩4호"  "SK5호스팩"           
## [37] "케이비제19호스팩"     "신영스팩6호"         
## [39] "에이치엠씨제4호스팩"  "대신밸런스제9호스팩" 
## [41] "엔에이치스팩13호"     "하나금융16호스팩"    
## [43] "유진스팩5호"          "교보9호스팩"         
## [45] "상상인이안제2호스팩"  "키움제5호스팩"       
## [47] "이베스트스팩5호"      "신영스팩5호"         
## [49] "유진스팩4호"          "한국제8호스팩"       
## [51] "IBKS제12호스팩"       "이베스트이안스팩1호"
```

```r
KOR_ticker[str_sub(KOR_ticker[, '종목코드'], -1, -1) != 0, '종목명']
```

```
##   [1] "삼성전자우"         "현대차2우B"        
##   [3] "LG화학우"           "현대차우"          
##   [5] "LG생활건강우"       "LG전자우"          
##   [7] "삼성SDI우"          "아모레퍼시픽우"    
##   [9] "미래에셋대우2우B"   "삼성화재우"        
##  [11] "한국금융지주우"     "신영증권우"        
##  [13] "CJ4우(전환)"        "삼성전기우"        
##  [15] "한화3우B"           "아모레G3우(전환)"  
##  [17] "현대차3우B"         "신풍제약우"        
##  [19] "대신증권우"         "SK케미칼우"        
##  [21] "CJ제일제당 우"      "SK이노베이션우"    
##  [23] "LG우"               "삼성물산우B"       
##  [25] "대림산업우"         "두산퓨얼셀1우"     
##  [27] "금호석유우"         "S-Oil우"           
##  [29] "NH투자증권우"       "두산우"            
##  [31] "SK우"               "CJ우"              
##  [33] "아모레G우"          "솔루스첨단소재1우" 
##  [35] "녹십자홀딩스2우"    "대신증권2우B"      
##  [37] "유한양행우"         "한화솔루션우"      
##  [39] "SK디스커버리우"     "미래에셋대우우"    
##  [41] "호텔신라우"         "코오롱인더우"      
##  [43] "롯데지주우"         "두산퓨얼셀2우B"    
##  [45] "부국증권우"         "두산2우B"          
##  [47] "GS우"               "솔루스첨단소재2우B"
##  [49] "대교우B"            "대한항공우"        
##  [51] "롯데칠성우"         "유화증권우"        
##  [53] "삼성중공우"         "LG하우시스우"      
##  [55] "BYC우"              "유안타증권우"      
##  [57] "티와이홀딩스우"     "일양약품우"        
##  [59] "남양유업우"         "세방우"            
##  [61] "한진칼우"           "대상우"            
##  [63] "하이트진로2우B"     "코리아써우"        
##  [65] "한화우"             "대덕전자1우"       
##  [67] "SK증권우"           "덕성우"            
##  [69] "현대건설우"         "한화투자증권우"    
##  [71] "태영건설우"         "넥센타이어1우B"    
##  [73] "삼양사우"           "코오롱우"          
##  [75] "삼양홀딩스우"       "유유제약1우"       
##  [77] "DB하이텍1우"        "남선알미우"        
##  [79] "NPC우"              "SK네트웍스우"      
##  [81] "루트로닉3우C"       "서울식품우"        
##  [83] "넥센우"             "성신양회우"        
##  [85] "대덕1우"            "계양전기우"        
##  [87] "금호산업우"         "대한제당우"        
##  [89] "태양금속우"         "코오롱글로벌우"    
##  [91] "한양증권우"         "동원시스템즈우"    
##  [93] "크라운제과우"       "CJ씨푸드1우"       
##  [95] "크라운해태홀딩스우" "대상홀딩스우"      
##  [97] "현대비앤지스틸우"   "대원전선우"        
##  [99] "흥국화재우"         "깨끗한나라우"      
## [101] "금강공업우"         "하이트진로홀딩스우"
## [103] "JW중외제약우"       "KG동부제철우"      
## [105] "대호피앤씨우"       "노루페인트우"      
## [107] "코리아써키트2우B"   "진흥기업우B"       
## [109] "동부건설우"         "성문전자우"        
## [111] "JW중외제약2우B"     "유유제약2우B"      
## [113] "동양우"             "소프트센우"        
## [115] "동양2우B"           "진흥기업2우B"      
## [117] "신원우"             "노루홀딩스우"      
## [119] "흥국화재2우B"       "동양3우B"
```

`grepl()` 함수를 통해 종목명에 ‘스팩’이 들어가는 종목을 찾고, `stringr` 패키지의 `str_sub()` 함수를 통해 종목코드 끝이 0이 아닌 우선주 종목을 찾을 수 있습니다.


```r
KOR_ticker = KOR_ticker[!grepl('스팩', KOR_ticker[, '종목명']), ]  
KOR_ticker = KOR_ticker[str_sub(KOR_ticker[, '종목코드'], -1, -1) == 0, ]
```

마지막으로 행 이름을 초기화한 후 정리된 데이터를 csv 파일로 저장합니다.


```r
rownames(KOR_ticker) = NULL
write.csv(KOR_ticker, 'data/KOR_ticker.csv')
```

## WICS 기준 섹터정보 크롤링

일반적으로 주식의 섹터를 나누는 기준은 MSCI와 S&P가 개발한 GICS^[https://en.wikipedia.org/wiki/Global_Industry_Classification_Standard]를 가장 많이 사용합니다. 국내 종목의 GICS 기준 정보 역시 한국거래소에서 제공하고 있으나, 이는 독점적 지적재산으로 명시했기에 사용하는 데 무리가 있습니다. 그러나 지수제공업체인 와이즈인덱스^[http://www.wiseindex.com/]에서는 GICS와 비슷한 WICS 산업분류를 발표하고 있습니다. WICS를 크롤링해 필요한 정보를 수집해보겠습니다.

먼저 웹페이지에 접속해 [Index → WISE SECTOR INDEX → WICS → 에너지]를 클릭합니다. 그 후 [Components] 탭을 클릭하면 해당 섹터의 구성종목을 확인할 수 있습니다.

<div class="figure" style="text-align: center">
<img src="images/crawl_practice_wics.png" alt="WICS 기준 구성종목" width="100%" />
<p class="caption">(\#fig:unnamed-chunk-26)WICS 기준 구성종목</p>
</div>

개발자도구 화면(그림 \@ref(fig:wicurl))을 통해 해당 페이지의 데이터전송 과정을 살펴보도록 하겠습니다.

<div class="figure" style="text-align: center">
<img src="images/crawl_practice_wics2.png" alt="WICS 페이지 개발자도구 화면" width="100%" />
<p class="caption">(\#fig:wicurl)WICS 페이지 개발자도구 화면</p>
</div>

일자를 선택하면 [Network] 탭의 GetIndexComponets 항목을 통해 데이터 전송 과정이 나타납니다. Request URL의 주소를 살펴보면 다음과 같습니다.

1. http://www.wiseindex.com/Index/GetIndexComponets: 데이터를 요청하는 url 입니다.
2. ceil_yn = 0: 실링 여부를 나타내며, 0은 비실링을 의미합니다.
3. dt=20190607: 조회일자를 나타냅니다.
4. sec_cd=G10: 섹터 코드를 나타냅니다.

이번엔 위 주소의 페이지를 열어보겠습니다.

<div class="figure" style="text-align: center">
<img src="images/crawl_practice_wics3.png" alt="WICS 데이터 페이지" width="100%" />
<p class="caption">(\#fig:unnamed-chunk-27)WICS 데이터 페이지</p>
</div>

글자들은 페이지에 출력된 내용이지만 매우 특이한 형태로 구성되어 있는데 이것은 JSON 형식의 데이터입니다. 기존에 우리가 살펴보았던 대부분의 웹페이지는 XML 형식으로 표현되어 있습니다. XML 형식은 문법이 복잡하고 표현 규칙이 엄격해 데이터의 용량이 커지는 단점이 있습니다. 반면 JSON 형식은 문법이 단순하고 데이터의 용량이 작아 빠른 속도로 데이터를 교환할 수 있습니다. R에서는 jsonlite 패키지의 `fromJSON()` 함수를 사용해 매우 손쉽게 JSON 형식의 데이터를 크롤링할 수 있습니다.


```r
library(jsonlite)

url = 'http://www.wiseindex.com/Index/GetIndexComponets?ceil_yn=0&dt=20190607&sec_cd=G10'
data = fromJSON(url)

lapply(data, head)
```

```
## $info
## $info$TRD_DT
## [1] "/Date(1559833200000)/"
## 
## $info$MKT_VAL
## [1] 19850082
## 
## $info$TRD_AMT
## [1] 70030
## 
## $info$CNT
## [1] 23
## 
## 
## $list
##   IDX_CD  IDX_NM_KOR ALL_MKT_VAL CMP_CD
## 1    G10 WICS 에너지    19850082 096770
## 2    G10 WICS 에너지    19850082 010950
## 3    G10 WICS 에너지    19850082 267250
## 4    G10 WICS 에너지    19850082 078930
## 5    G10 WICS 에너지    19850082 067630
## 6    G10 WICS 에너지    19850082 006120
##              CMP_KOR MKT_VAL   WGT S_WGT CAL_WGT SEC_CD
## 1       SK이노베이션 9052841 45.61 45.61       1    G10
## 2              S-Oil 3403265 17.14 62.75       1    G10
## 3     현대중공업지주 2873204 14.47 77.23       1    G10
## 4                 GS 2491805 12.55 89.78       1    G10
## 5 에이치엘비생명과학  624986  3.15 92.93       1    G10
## 6       SK디스커버리  257059  1.30 94.22       1    G10
##   SEC_NM_KOR SEQ TOP60 APT_SHR_CNT
## 1     에너지   1     2    56403994
## 2     에너지   2     2    41655633
## 3     에너지   3     2     9283372
## 4     에너지   4     2    49245150
## 5     에너지   5     2    39307272
## 6     에너지   6     2    10470820
## 
## $sector
##   SEC_CD         SEC_NM_KOR SEC_RATE IDX_RATE
## 1    G25     경기관련소비재    16.05        0
## 2    G35           건강관리     9.27        0
## 3    G50 커뮤니케이션서비스     2.26        0
## 4    G40               금융    10.31        0
## 5    G10             에너지     2.37      100
## 6    G20             산업재    12.68        0
## 
## $size
##   SEC_CD    SEC_NM_KOR SEC_RATE IDX_RATE
## 1 WMI510 WMI500 대형주    69.40    89.78
## 2 WMI520 WMI500 중형주    13.56     4.44
## 3 WMI530 WMI500 소형주    17.04     5.78
```

\$list 항목에는 해당 섹터의 구성종목 정보가 있으며, \$sector 항목을 통해 다른 섹터의 코드도 확인할 수 있습니다. for loop 구문을 이용해 URL의 sec_cd=에 해당하는 부분만 변경하면 모든 섹터의 구성종목을 매우 쉽게 얻을 수 있습니다.


```r
sector_code = c('G25', 'G35', 'G50', 'G40', 'G10',
                'G20', 'G55', 'G30', 'G15', 'G45')
data_sector = list()

for (i in sector_code) {
  
  url = paste0(
    'http://www.wiseindex.com/Index/GetIndexComponets',
    '?ceil_yn=0&dt=',biz_day,'&sec_cd=',i)
  data = fromJSON(url)
  data = data$list
  
  data_sector[[i]] = data
  
  Sys.sleep(1)
}

data_sector = do.call(rbind, data_sector)
```

해당 데이터를 csv 파일로 저장해주도록 합니다.


```r
write.csv(data_sector, 'data/KOR_sector.csv')
```
