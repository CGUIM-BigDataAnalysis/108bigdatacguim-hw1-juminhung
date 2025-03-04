108-2 大數據分析方法 作業一
================
ju-min hung

搞不清楚各行各業的薪資差異嗎? 念研究所到底對第一份工作的薪資影響有多大? CP值高嗎? 透過分析**初任人員平均經常性薪資**-
（107年）<https://data.gov.tw/dataset/6647>
（104-105年）<http://ipgod.nchc.org.tw/dataset/a17000000j-020066>
，可初步了解台灣近幾年各行各業、各學歷的起薪。

## 比較104年度和107年度大學畢業者的薪資資料

### 資料匯入與處理

``` r
library(readr)
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
#匯入104年度的資料
hw104 <- read_csv("C:/Users/Emily/Desktop/hw104.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   年度 = col_double(),
    ##   大職業別 = col_character(),
    ##   `經常性薪資-薪資` = col_double(),
    ##   `經常性薪資-女/男` = col_character(),
    ##   `國中及以下-薪資` = col_character(),
    ##   `國中及以下-女/男` = col_character(),
    ##   `高中或高職-薪資` = col_character(),
    ##   `高中或高職-女/男` = col_character(),
    ##   `專科-薪資` = col_character(),
    ##   `專科-女/男` = col_character(),
    ##   `大學-薪資` = col_character(),
    ##   `大學-女/男` = col_character(),
    ##   `研究所及以上-薪資` = col_character(),
    ##   `研究所及以上-女/男` = col_character()
    ## )

``` r
#匯入107年度的資料
hw107 <- read_csv("C:/Users/Emily/Desktop/hw107.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   年度 = col_double(),
    ##   大職業別 = col_character(),
    ##   `經常性薪資-薪資` = col_character(),
    ##   `經常性薪資-女/男` = col_character(),
    ##   `國中及以下-薪資` = col_character(),
    ##   `國中及以下-女/男` = col_character(),
    ##   `高中或高職-薪資` = col_character(),
    ##   `高中或高職-女/男` = col_character(),
    ##   `專科-薪資` = col_character(),
    ##   `專科-女/男` = col_character(),
    ##   `大學-薪資` = col_character(),
    ##   `大學-女/男` = col_character(),
    ##   `研究所-薪資` = col_character(),
    ##   `研究所-女/男` = col_character()
    ## )

``` r
#將大學-薪資的值為—或…，取代為空值，並將其資料類型轉換為數值
hw104$`大學-薪資`<-gsub("—|…","",hw104$`大學-薪資`)
hw104$`大學-薪資`<-as.numeric(hw104$`大學-薪資`)
hw107$`大學-薪資`<-gsub("—|…","",hw107$`大學-薪資`)
hw107$`大學-薪資`<-as.numeric(hw107$`大學-薪資`)
#將大學-女/男的值為—或…，取代為空值，並將其資料類型轉換為數值
hw104$`大學-女/男`<-gsub("—|…","",hw104$`大學-女/男`)
hw104$`大學-女/男`<-as.numeric(hw104$`大學-女/男`)
hw107$`大學-女/男`<-gsub("—|…","",hw107$`大學-女/男`)
hw107$`大學-女/男`<-as.numeric(hw107$`大學-女/男`)
#將研究所-薪資的值為—或…，取代為空值，並將其資料類型轉換為數值
hw107$`研究所-薪資`<-gsub("—|…","",hw107$`研究所-薪資`)
hw107$`研究所-薪資`<-as.numeric(hw107$`研究所-薪資`)
#因為兩個檔案的大職業別相同，只是名稱有些不一樣，所以直接取代為一樣名稱，才能JOIN在一起
hw107$"大職業別"<-hw104$"大職業別"
```

### 請問107年度薪資較104年度薪資高的職業有哪些? 請按照提高比例由大到小排序?呈現前十名的資料

``` r
#Join 104及107年度表格為AllData
AllData<-inner_join(hw104,hw107,by="大職業別")
#選取所需要的欄位變成一個新表格AllDataClean
AllDataClean<-AllData[c(2,1,11,15,24)]
#計算薪資比
AllDataClean$salary_ratio<-hw107$`大學-薪資`/hw104$`大學-薪資`
#篩選薪資比大於1的資料並由大到小排序得到107年度薪資較104年度薪資高的資料
salary_higher<-AllDataClean%>%filter(salary_ratio>1)%>%arrange(desc(salary_ratio))
#呈現前十名的資料
knitr::kable(head(salary_higher,10))
```

| 大職業別                  | 年度.x | 大學-薪資.x | 年度.y | 大學-薪資.y | salary\_ratio |
| :-------------------- | :--: | :-----: | :--: | :-----: | ------------: |
| 專業、科學及技術服務業-服務及銷售工作人員 | 2015 |  24423  | 2018 |  28304  |      1.158908 |
| 教育服務業-服務及銷售工作人員       | 2015 |  24324  | 2018 |  27543  |      1.132338 |
| 不動產業-技術員及助理專業人員       | 2015 |  27338  | 2018 |  30914  |      1.130807 |
| 住宿及餐飲業-服務及銷售工作人員      | 2015 |  23548  | 2018 |  26111  |      1.108842 |
| 藝術、娛樂及休閒服務業-事務支援人員    | 2015 |  24244  | 2018 |  26611  |      1.097632 |
| 金融及保險業-技藝、機械設備操作及組裝人員 | 2015 |  30074  | 2018 |  32997  |      1.097194 |
| 教育服務業                 | 2015 |  25162  | 2018 |  27582  |      1.096177 |
| 教育服務業-事務支援人員          | 2015 |  23643  | 2018 |  25908  |      1.095800 |
| 用水供應及污染整治業-專業人員       | 2015 |  32179  | 2018 |  34957  |      1.086330 |
| 教育服務業-專業人員            | 2015 |  26615  | 2018 |  28911  |      1.086267 |

前10名的職業包含：專業、科學及技術服務業、教育服務業、不動產業、住宿及餐飲業、藝術、娛樂及休閒服務業、金融及保險業、用水供應及污染整治業等職業

### 提高超過5%的的職業有哪些?

``` r
#篩選薪資比超過5%的資料
higher_career<-filter(AllDataClean,salary_ratio>1.05)
knitr::kable(higher_career)
```

| 大職業別                       | 年度.x | 大學-薪資.x | 年度.y | 大學-薪資.y | salary\_ratio |
| :------------------------- | :--: | :-----: | :--: | :-----: | ------------: |
| 工業及服務業部門-服務及銷售工作人員         | 2015 |  25831  | 2018 |  27551  |      1.066587 |
| 工業部門-專業人員                  | 2015 |  30546  | 2018 |  32145  |      1.052347 |
| 工業部門-技術員及助理專業人員            | 2015 |  27429  | 2018 |  28894  |      1.053411 |
| 工業部門-技藝、機械設備操作及組裝人員        | 2015 |  25890  | 2018 |  27193  |      1.050328 |
| 製造業-專業人員                   | 2015 |  30377  | 2018 |  31972  |      1.052507 |
| 製造業-技術員及助理專業人員             | 2015 |  27247  | 2018 |  28746  |      1.055015 |
| 電力及燃氣供應業-技藝、機械設備操作及組裝人員    | 2015 |  28039  | 2018 |  30031  |      1.071044 |
| 用水供應及污染整治業                 | 2015 |  28464  | 2018 |  29943  |      1.051960 |
| 用水供應及污染整治業-專業人員            | 2015 |  32179  | 2018 |  34957  |      1.086330 |
| 用水供應及污染整治業-技藝、機械設備操作及組裝人員  | 2015 |  27887  | 2018 |  29909  |      1.072507 |
| 營造業                        | 2015 |  27748  | 2018 |  29154  |      1.050670 |
| 營造業-專業人員                   | 2015 |  31014  | 2018 |  32897  |      1.060715 |
| 營造業-事務支援人員                 | 2015 |  25281  | 2018 |  26721  |      1.056960 |
| 營造業-服務及銷售工作人員              | 2015 |  26782  | 2018 |  28707  |      1.071877 |
| 服務業部門-服務及銷售工作人員            | 2015 |  24812  | 2018 |  26527  |      1.069120 |
| 批發及零售業-服務及銷售工作人員           | 2015 |  24329  | 2018 |  25614  |      1.052818 |
| 運輸及倉儲業                     | 2015 |  28404  | 2018 |  30002  |      1.056260 |
| 運輸及倉儲業-技術員及助理專業人員          | 2015 |  29670  | 2018 |  31747  |      1.070003 |
| 運輸及倉儲業-事務支援人員              | 2015 |  26079  | 2018 |  27650  |      1.060240 |
| 運輸及倉儲業-服務及銷售工作人員           | 2015 |  27757  | 2018 |  29262  |      1.054221 |
| 運輸及倉儲業-技藝、機械設備操作及組裝人員      | 2015 |  29168  | 2018 |  31085  |      1.065723 |
| 住宿及餐飲業                     | 2015 |  25167  | 2018 |  27213  |      1.081297 |
| 住宿及餐飲業-事務支援人員              | 2015 |  24786  | 2018 |  26538  |      1.070685 |
| 住宿及餐飲業-服務及銷售工作人員           | 2015 |  23548  | 2018 |  26111  |      1.108842 |
| 住宿及餐飲業-技藝、機械設備操作及組裝人員      | 2015 |  26276  | 2018 |  27913  |      1.062300 |
| 資訊及通訊傳播業                   | 2015 |  27478  | 2018 |  29143  |      1.060594 |
| 資訊及通訊傳播業-專業人員              | 2015 |  29352  | 2018 |  31763  |      1.082141 |
| 資訊及通訊傳播業-技術員及助理專業人員        | 2015 |  27470  | 2018 |  28910  |      1.052421 |
| 資訊及通訊傳播業-事務支援人員            | 2015 |  25728  | 2018 |  27464  |      1.067475 |
| 資訊及通訊傳播業-服務及銷售工作人員         | 2015 |  26210  | 2018 |  27586  |      1.052499 |
| 金融及保險業                     | 2015 |  30787  | 2018 |  32448  |      1.053951 |
| 金融及保險業-專業人員                | 2015 |  33706  | 2018 |  35490  |      1.052928 |
| 金融及保險業-技術員及助理專業人員          | 2015 |  30495  | 2018 |  32602  |      1.069093 |
| 金融及保險業-服務及銷售工作人員           | 2015 |  28213  | 2018 |  30134  |      1.068089 |
| 金融及保險業-技藝、機械設備操作及組裝人員      | 2015 |  30074  | 2018 |  32997  |      1.097194 |
| 不動產業                       | 2015 |  27657  | 2018 |  29494  |      1.066421 |
| 不動產業-技術員及助理專業人員            | 2015 |  27338  | 2018 |  30914  |      1.130807 |
| 不動產業-事務支援人員                | 2015 |  25632  | 2018 |  27001  |      1.053410 |
| 不動產業-服務及銷售工作人員             | 2015 |  25806  | 2018 |  27409  |      1.062117 |
| 不動產業-技藝、機械設備操作及組裝人員        | 2015 |  25204  | 2018 |  26802  |      1.063403 |
| 專業、科學及技術服務業-服務及銷售工作人員      | 2015 |  24423  | 2018 |  28304  |      1.158908 |
| 支援服務業-服務及銷售工作人員            | 2015 |  24810  | 2018 |  26321  |      1.060903 |
| 教育服務業                      | 2015 |  25162  | 2018 |  27582  |      1.096177 |
| 教育服務業-專業人員                 | 2015 |  26615  | 2018 |  28911  |      1.086267 |
| 教育服務業-事務支援人員               | 2015 |  23643  | 2018 |  25908  |      1.095800 |
| 教育服務業-服務及銷售工作人員            | 2015 |  24324  | 2018 |  27543  |      1.132338 |
| 醫療保健服務業-服務及銷售工作人員          | 2015 |  25106  | 2018 |  26549  |      1.057476 |
| 藝術、娛樂及休閒服務業                | 2015 |  25615  | 2018 |  27147  |      1.059809 |
| 藝術、娛樂及休閒服務業-專業人員           | 2015 |  28351  | 2018 |  29955  |      1.056577 |
| 藝術、娛樂及休閒服務業-技術員及助理專業人員     | 2015 |  26523  | 2018 |  27887  |      1.051427 |
| 藝術、娛樂及休閒服務業-事務支援人員         | 2015 |  24244  | 2018 |  26611  |      1.097632 |
| 藝術、娛樂及休閒服務業-技藝、機械設備操作及組裝人員 | 2015 |  25739  | 2018 |  27180  |      1.055985 |
| 其他服務業-服務及銷售工作人員            | 2015 |  22179  | 2018 |  23443  |      1.056991 |

### 主要的職業種別是哪些種類呢?

``` r
career<-strsplit(higher_career$"大職業別","-")%>%lapply( "[", 1)
knitr::kable(table(unlist(career)))
```

| Var1        | Freq |
| :---------- | ---: |
| 工業及服務業部門    |    1 |
| 工業部門        |    3 |
| 不動產業        |    5 |
| 支援服務業       |    1 |
| 用水供應及污染整治業  |    3 |
| 住宿及餐飲業      |    4 |
| 批發及零售業      |    1 |
| 其他服務業       |    1 |
| 服務業部門       |    1 |
| 金融及保險業      |    5 |
| 專業、科學及技術服務業 |    1 |
| 教育服務業       |    4 |
| 資訊及通訊傳播業    |    5 |
| 運輸及倉儲業      |    5 |
| 電力及燃氣供應業    |    1 |
| 製造業         |    2 |
| 營造業         |    4 |
| 醫療保健服務業     |    1 |
| 藝術、娛樂及休閒服務業 |    5 |

主要的職業類別為上述表格出現的職業，其中出現次數最多為不動產業、金融及保險業、資訊及通訊傳播業、運輸及倉儲業、藝術、娛樂及休閒服務業。

## 男女同工不同酬現況分析

男女同工不同酬一直是性別平等中很重要的問題，分析資料來源為104和107年度的大學畢業薪資。

### 104和107年度的大學畢業薪資資料，哪些行業男生薪資比女生薪資多?依照差異大小由大到小排序，呈現前十名的資料

``` r
#104年度大學畢業薪資，男生薪資比女生薪資多的職業
hw104_malehigher<-hw104[c(1,2,12)]
University104_asc<-filter(hw104_malehigher,hw104_malehigher$`大學-女/男`<100) %>%arrange(`大學-女/男`) 
knitr::kable(head(University104_asc,10))
```

|  年度  | 大職業別                      | 大學-女/男 |
| :--: | :------------------------ | :----: |
| 2015 | 電力及燃氣供應業-技藝、機械設備操作及組裝人員   | 91.69  |
| 2015 | 教育服務業-服務及銷售工作人員           | 91.90  |
| 2015 | 礦業及土石採取業-技術員及助理專業人員       | 92.42  |
| 2015 | 礦業及土石採取業-技藝、機械設備操作及組裝人員   | 93.10  |
| 2015 | 礦業及土石採取業                  | 95.28  |
| 2015 | 其他服務業-事務支援人員              | 95.47  |
| 2015 | 營造業-技藝、機械設備操作及組裝人員        | 95.64  |
| 2015 | 用水供應及污染整治業-技藝、機械設備操作及組裝人員 | 95.90  |
| 2015 | 營造業                       | 96.35  |
| 2015 | 教育服務業                     | 96.44  |

``` r
#107年度大學畢業薪資，男生薪資比女生薪資多的職業
hw107_malehigher<-hw107[c(1,2,12)]
University107_asc<-filter(hw107_malehigher,hw107_malehigher$`大學-女/男`<100) %>%arrange(`大學-女/男`) 
knitr::kable(head(University107_asc,10))
```

|  年度  | 大職業別                 | 大學-女/男 |
| :--: | :------------------- | :----: |
| 2018 | 礦業及土石採取業-專業人員        | 96.02  |
| 2018 | 電力及燃氣供應業-事務支援人員      | 96.96  |
| 2018 | 礦業及土石採取業             | 97.11  |
| 2018 | 營造業                  | 97.52  |
| 2018 | 教育服務業-事務支援人員         | 97.61  |
| 2018 | 電力及燃氣供應業-服務及銷售工作人員   | 97.73  |
| 2018 | 營造業-技術員及助理專業人員       | 97.78  |
| 2018 | 用水供應及污染整治業-專業人員      | 97.81  |
| 2018 | 用水供應及污染整治業-服務及銷售工作人員 | 97.94  |
| 2018 | 電力及燃氣供應業-專業人員        | 98.23  |

資料中，若女生薪資/男生薪資\<100，代表男生薪資較高，若女生薪資/男生薪資=100，代表男女薪資相等，所以數字小於100越多，代表男女薪資差異越大。

在104年度的資料中，男女薪資差異最大的職業為電力及燃氣供應業-技藝、機械設備操作及組裝人員，其中，前10名中出現最多次的職業為礦業及土石採取業。

在107年度的資料中，男女薪資差異最大的職業為礦業及土石採取業-專業人員，其中，前10名中出現最多次的職業為電力及燃氣供應業。

三年中，第一名出現了改變，可能跟大家用電力及燃氣更多，所以需要更多人員來工作。

### 請問那些行業女生薪資比男生薪資多? 依據差異大小由大到小排序，呈現前十名的資料

``` r
#104年度大學畢業薪資，女生薪資比男生薪資多的職業
hw104_femalehigher<-hw104[c(1,2,12)]
University104_desc<-filter(hw104_femalehigher,hw104_femalehigher$`大學-女/男`>100) %>%arrange(`大學-女/男`) 
knitr::kable(head(University104_desc,10))
```

|  年度  | 大職業別                       | 大學-女/男 |
| :--: | :------------------------- | :----: |
| 2015 | 專業、科學及技術服務業-技藝、機械設備操作及組裝人員 | 100.26 |

資料中，若女生薪資/男生薪資\>100，代表女生薪資較高，若女生薪資/男生薪資=100，代表男女薪資相等，所以數字大於100越多，代表男女薪資差異越大。

在104年的資料表中，只有“專業、科學及技術服務業-技藝、機械設備操作及組裝人員”是女生薪資大於男生薪資的職業。

``` r
#107年度大學畢業薪資，女生薪資比男生薪資多的職業
hw107_femalehigher<-hw107[c(1,2,12)]
University107_desc<-filter(hw107_femalehigher,hw107_femalehigher$`大學-女/男`>100) %>%arrange(`大學-女/男`) 
knitr::kable(head(University107_desc,10))
```

| 年度 | 大職業別 | 大學-女/男 |
| -: | :--- | -----: |

資料中，若女生薪資/男生薪資\>100，代表女生薪資較高，若女生薪資/男生薪資=100，代表男女薪資相等，所以數字大於100越多，代表男女薪資差異越大。

在104年的資料表中，沒有職業是女生薪資大於男生薪資的。

## 研究所薪資差異

以107年度的資料來看，哪個職業別念研究所最划算 (研究所學歷薪資與大學學歷薪資增加比例最多)?
請按照薪資差異比例由大到小排序，呈現前十名的資料

``` r
#篩選出大學薪資欄位與研究所薪資欄位
grad107<-hw107[c(2,11,13)]
#計算研究所跟大學的薪資比
grad107$GUsalary_ratio<-grad107$`研究所-薪資`/grad107$`大學-薪資`
#按照薪資差異比例由大到小排序
grad107<-grad107[order(grad107$GUsalary_ratio,decreasing = T),]
#呈現前十名的資料
knitr::kable(head(grad107,10))
```

| 大職業別             | 大學-薪資 | 研究所-薪資 | GUsalary\_ratio |
| :--------------- | :---: | :----: | --------------: |
| 其他服務業            | 25781 | 31909  |        1.237694 |
| 專業、科學及技術服務業      | 29353 | 35381  |        1.205362 |
| 專業、科學及技術服務業-專業人員 | 32460 | 39103  |        1.204652 |
| 教育服務業-事務支援人員     | 25908 | 30827  |        1.189864 |
| 資訊及通訊傳播業         | 29143 | 34503  |        1.183921 |
| 其他服務業-事務支援人員     | 26115 | 30830  |        1.180548 |
| 資訊及通訊傳播業-專業人員    | 31763 | 37479  |        1.179958 |
| 製造業              | 28777 | 33916  |        1.178580 |
| 工業部門             | 28850 | 33893  |        1.174801 |
| 工業及服務業部門         | 28849 | 33880  |        1.174391 |

從此表格中可以看出，其他服務業念研究所最划算，因為薪資差異比最大。

## 我有興趣的職業別薪資狀況分析

### 請列出自己有興趣的職業別 (至少一個至多五個)，並呈現相對應的大學畢業薪資與研究所畢業薪資，請問此薪資與妳想像中的一樣嗎? 研究所薪資與大學薪資差多少呢? 會因為這樣改變心意，決定念/不念研究所嗎?

``` r
#篩選出大學薪資欄位與研究所薪資欄位
grad107<-hw107[c(2,11,13)]
#計算研究所跟大學的薪資比
grad107$GUsalary_ratio<-grad107$`研究所-薪資`/grad107$`大學-薪資`
#把NA值清乾淨
grad107Clean<-filter(grad107,grad107$GUsalary_ratio>0)
```

根據107年度的資料來做。

``` r
#篩選教育服務業
edu<-grad107Clean[grepl("教育服務業",grad107Clean$"大職業別"),]
knitr::kable(edu)
```

| 大職業別         | 大學-薪資 | 研究所-薪資 | GUsalary\_ratio |
| :----------- | :---: | :----: | --------------: |
| 教育服務業        | 27582 | 30509  |        1.106120 |
| 教育服務業-專業人員   | 28911 | 30025  |        1.038532 |
| 教育服務業-事務支援人員 | 25908 | 30827  |        1.189864 |

``` r
#教育服務業薪資差
edu$GUsalary_diff<-(edu$`研究所-薪資`)-(edu$`大學-薪資`)
knitr::kable(edu)
```

| 大職業別         | 大學-薪資 | 研究所-薪資 | GUsalary\_ratio | GUsalary\_diff |
| :----------- | :---: | :----: | --------------: | -------------: |
| 教育服務業        | 27582 | 30509  |        1.106120 |           2927 |
| 教育服務業-專業人員   | 28911 | 30025  |        1.038532 |           1114 |
| 教育服務業-事務支援人員 | 25908 | 30827  |        1.189864 |           4919 |

我對於教育服務業有興趣，他的薪資跟我預想的差不多，因為老師的基本薪水都是27000-30000元左右，如果是職員的話就會再少一點，大概25000-30000元左右，研究所薪資與大學薪資平均差3000元，薪資比其實差異不大，對於我來說，如果需要進修自己才會去唸研究所，不會因為薪資差異而影響我要不要念研究所。

``` r
#篩選資訊及通訊傳播業
tech<-grad107Clean[grepl("資訊及通訊傳播業",grad107Clean$"大職業別"),]
knitr::kable(tech)
```

| 大職業別                | 大學-薪資 | 研究所-薪資 | GUsalary\_ratio |
| :------------------ | :---: | :----: | --------------: |
| 資訊及通訊傳播業            | 29143 | 34503  |        1.183921 |
| 資訊及通訊傳播業-專業人員       | 31763 | 37479  |        1.179958 |
| 資訊及通訊傳播業-技術員及助理專業人員 | 28910 | 33643  |        1.163715 |
| 資訊及通訊傳播業-事務支援人員     | 27464 | 30950  |        1.126930 |

``` r
#資訊及通訊傳播業薪資差
tech$GUsalary_diff<-(tech$`研究所-薪資`)-(tech$`大學-薪資`)
knitr::kable(tech)
```

| 大職業別                | 大學-薪資 | 研究所-薪資 | GUsalary\_ratio | GUsalary\_diff |
| :------------------ | :---: | :----: | --------------: | -------------: |
| 資訊及通訊傳播業            | 29143 | 34503  |        1.183921 |           5360 |
| 資訊及通訊傳播業-專業人員       | 31763 | 37479  |        1.179958 |           5716 |
| 資訊及通訊傳播業-技術員及助理專業人員 | 28910 | 33643  |        1.163715 |           4733 |
| 資訊及通訊傳播業-事務支援人員     | 27464 | 30950  |        1.126930 |           3486 |

對於念資管系的我來說，資訊及通訊傳播業當然會是有興趣的職業之一，
資訊領域的專業人員大學薪資就有31763元，如果念研究所就有37479元，跟我預想的薪水差不多，薪水差了5716元，薪資比為1.18，是我有興趣的職業中薪資差異最大的，如果想走資訊及通訊傳播業的專業人員的話，薪水的差異可能會影響我是否要念研究所。

``` r
#篩選醫療保健服務業
med<-grad107Clean[grepl("醫療保健服務業",grad107Clean$"大職業別"),]
knitr::kable(med)
```

| 大職業別               | 大學-薪資 | 研究所-薪資 | GUsalary\_ratio |
| :----------------- | :---: | :----: | --------------: |
| 醫療保健服務業            | 29590 | 34436  |        1.163771 |
| 醫療保健服務業-專業人員       | 34270 | 38677  |        1.128596 |
| 醫療保健服務業-技術員及助理專業人員 | 30556 | 33798  |        1.106100 |
| 醫療保健服務業-事務支援人員     | 26177 | 29573  |        1.129732 |

``` r
#醫療保健服務業薪資差
med$GUsalary_diff<-(med$`研究所-薪資`)-(med$`大學-薪資`)
knitr::kable(med)
```

| 大職業別               | 大學-薪資 | 研究所-薪資 | GUsalary\_ratio | GUsalary\_diff |
| :----------------- | :---: | :----: | --------------: | -------------: |
| 醫療保健服務業            | 29590 | 34436  |        1.163771 |           4846 |
| 醫療保健服務業-專業人員       | 34270 | 38677  |        1.128596 |           4407 |
| 醫療保健服務業-技術員及助理專業人員 | 30556 | 33798  |        1.106100 |           3242 |
| 醫療保健服務業-事務支援人員     | 26177 | 29573  |        1.129732 |           3396 |

醫療保健服務業也是我有興趣的職業之一，雖然不能做專業人員或技術人員，但對於事物支援人員也是有興趣，大學畢業有26177元，研究所畢業有29573元，跟我預想的差了一點，不過還是可以接受的範圍，薪資差3396元，不一定會因為這個差異而去念研究所。

``` r
#篩選藝術、娛樂及休閒服務業
art<-grad107Clean[grepl("藝術、娛樂及休閒服務業",grad107Clean$"大職業別"),]
knitr::kable(art)
```

| 大職業別                   | 大學-薪資 | 研究所-薪資 | GUsalary\_ratio |
| :--------------------- | :---: | :----: | --------------: |
| 藝術、娛樂及休閒服務業            | 27147 | 29780  |        1.096990 |
| 藝術、娛樂及休閒服務業-專業人員       | 29955 | 32049  |        1.069905 |
| 藝術、娛樂及休閒服務業-技術員及助理專業人員 | 27887 | 28599  |        1.025532 |
| 藝術、娛樂及休閒服務業-事務支援人員     | 26611 | 28833  |        1.083499 |

``` r
#藝術、娛樂及休閒服務業薪資差
art$GUsalary_diff<-(art$`研究所-薪資`)-(art$`大學-薪資`)
knitr::kable(art)
```

| 大職業別                   | 大學-薪資 | 研究所-薪資 | GUsalary\_ratio | GUsalary\_diff |
| :--------------------- | :---: | :----: | --------------: | -------------: |
| 藝術、娛樂及休閒服務業            | 27147 | 29780  |        1.096990 |           2633 |
| 藝術、娛樂及休閒服務業-專業人員       | 29955 | 32049  |        1.069905 |           2094 |
| 藝術、娛樂及休閒服務業-技術員及助理專業人員 | 27887 | 28599  |        1.025532 |            712 |
| 藝術、娛樂及休閒服務業-事務支援人員     | 26611 | 28833  |        1.083499 |           2222 |

對於藝術、娛樂及休閒服務業也有一點興趣，因為對音樂藝術有一些專長，這類的薪資有些人其實很不固定，有些人可以很高，有些人卻收入很低，所以這個平均薪水跟我預想的差不多，但他們的薪資差大約在712\~2633元不等，對於我來說其實薪資差異不大，所以不會因為這個原因來影響我要不要念研究所，通常做這個職業的人都是為了精進自己才念研究所的。

``` r
#篩選住宿及餐飲業
room<-grad107Clean[grepl("住宿及餐飲業",grad107Clean$"大職業別"),]
knitr::kable(room)
```

| 大職業別              | 大學-薪資 | 研究所-薪資 | GUsalary\_ratio |
| :---------------- | :---: | :----: | --------------: |
| 住宿及餐飲業            | 27213 | 30079  |        1.105317 |
| 住宿及餐飲業-專業人員       | 29669 | 32103  |        1.082038 |
| 住宿及餐飲業-技術員及助理專業人員 | 28140 | 29908  |        1.062829 |
| 住宿及餐飲業-事務支援人員     | 26538 | 28480  |        1.073178 |

``` r
#住宿及餐飲業薪資差
room$GUsalary_diff<-(room$`研究所-薪資`)-(room$`大學-薪資`)
knitr::kable(room)
```

| 大職業別              | 大學-薪資 | 研究所-薪資 | GUsalary\_ratio | GUsalary\_diff |
| :---------------- | :---: | :----: | --------------: | -------------: |
| 住宿及餐飲業            | 27213 | 30079  |        1.105317 |           2866 |
| 住宿及餐飲業-專業人員       | 29669 | 32103  |        1.082038 |           2434 |
| 住宿及餐飲業-技術員及助理專業人員 | 28140 | 29908  |        1.062829 |           1768 |
| 住宿及餐飲業-事務支援人員     | 26538 | 28480  |        1.073178 |           1942 |

我對於住宿及餐飲業有一點點的興趣，雖然是種服務業類型的工作會比較累，但是還是有一些興趣，其實這行業的大學薪資與研究所薪資差異也不大，大約在1768\~2866元之間，除非要當主管，否則這樣的薪資並不會影響我是否要念研究所。
