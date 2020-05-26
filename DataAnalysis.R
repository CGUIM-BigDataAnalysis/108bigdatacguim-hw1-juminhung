library(jsonlite)
library(dplyr)
library(readr)
hw104 <- read_csv("C:/Users/Emily/Desktop/hw104.csv")
View(hw104)
hw107 <- read_csv("C:/Users/Emily/Desktop/hw107.csv")
View(hw107)
hw104$`大學-薪資`<-gsub("—|…","",hw104$`大學-薪資`)
hw104$`大學-薪資`<-as.numeric(hw104$`大學-薪資`)
hw107$`大學-薪資`<-gsub("—|…","",hw107$`大學-薪資`)
hw107$`大學-薪資`<-as.numeric(hw107$`大學-薪資`)
hw104$`大學-女/男`<-gsub("—|…","",hw104$`大學-女/男`)
hw104$`大學-女/男`<-as.numeric(hw104$`大學-女/男`)
hw107$`大學-女/男`<-gsub("—|…","",hw107$`大學-女/男`)
hw107$`大學-女/男`<-as.numeric(hw107$`大學-女/男`)
hw107$`研究所-薪資`<-gsub("—|…","",hw107$`研究所-薪資`)
hw107$`研究所-薪資`<-as.numeric(hw107$`研究所-薪資`)
hw107$"大職業別"<-hw104$"大職業別"

#Q1
library(dplyr)
AllData<-inner_join(hw104,hw107,by="大職業別")
AllDataClean<-AllData[c(2,1,11,15,24)]
AllDataClean$salary_ratio<-hw107$`大學-薪資`/hw104$`大學-薪資`
salary_higher<-AllDataClean%>%filter(salary_ratio>1)%>%arrange(desc(salary_ratio))
knitr::kable(head(salary_higher,10))

head(AllData,10)
higher_career<-filter(AllDataClean,salary_ratio>1.05)
career<-strsplit(higher_career$大職業別,"-")%>%lapply( "[", 1)
View(table(unlist(career)))


#Q2
#女生薪資/男生薪資 如果>100女生薪資多 如果=100則相等 如果<100男生薪資高
#男生是分母 分母大於分子就會算出小於100
#男生薪資高
#104年度大學畢業薪資，男生薪資比女生薪資多的職業
hw104_malehigher<-hw104[c(1,2,12)]
University104_asc<-filter(hw104_malehigher,hw104_malehigher$`大學-女/男`<100) %>%arrange(`大學-女/男`) 
knitr::kable(head(University104_asc,10))
#107年度大學畢業薪資，男生薪資比女生薪資多的職業
hw107_malehigher<-hw107[c(1,2,12)]
University107_asc<-filter(hw107_malehigher,hw107_malehigher$`大學-女/男`<100) %>%arrange(`大學-女/男`) 
knitr::kable(head(University107_asc,10))
#女生薪資高
#104年度大學畢業薪資，女生薪資比男生薪資多的職業
hw104_femalehigher<-hw104[c(1,2,12)]
University104_desc<-filter(hw104_femalehigher,hw104_femalehigher$`大學-女/男`>100) %>%arrange(`大學-女/男`) 
knitr::kable(head(University104_desc,10))
#107年度大學畢業薪資，女生薪資比男生薪資多的職業
hw107_femalehigher<-hw107[c(1,2,12)]
University107_desc<-filter(hw107_femalehigher,hw107_femalehigher$`大學-女/男`>100) %>%arrange(`大學-女/男`) 
knitr::kable(head(University107_desc,10))

#Q3
#篩選出大學薪資欄位與研究所薪資欄位
grad107<-hw107[c(2,11,13)]
#計算研究所跟大學的薪資比
grad107$GUsalary_ratio<-grad107$`研究所-薪資`/grad107$`大學-薪資`
#按照薪資差異比例由大到小排序
grad107<-grad107[order(grad107$GUsalary_ratio,decreasing = T),]
#呈現前十名的資料
knitr::kable(head(grad107,10))


#Q4
grad107<-hw107[c(2,11,13)]
grad107$GUsalary_ratio<-grad107$`研究所-薪資`/grad107$`大學-薪資`
grad107$GUsalary_diff<-(grad107$`研究所-薪資`)-(grad107$`大學-薪資`)
grad107Clean<-filter(grad107,grad107$GUsalary_ratio>0)
edu<-grad107Clean[grepl("教育服務業",grad107Clean$"大職業別"),]

tech<-grad107Clean[grepl("資訊及通訊傳播業",grad107Clean$"大職業別"),]

med<-grad107Clean[grepl("醫療保健服務業",grad107Clean$"大職業別"),]

art<-grad107Clean[grepl("藝術、娛樂及休閒服務業",grad107Clean$"大職業別"),]

room<-grad107Clean[grepl("住宿及餐飲業",grad107Clean$"大職業別"),]
  