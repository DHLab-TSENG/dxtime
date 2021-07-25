dxtime\_CAD
================

## set environment

### load library

``` r
# install.packages("devtools")
# devtools::install_github("DHLab-TSENG/dxtime")
# devtools::install_github("DHLab-TSENG/dxpr")
library(dxpr)
library(dxtime)
```

    ## Warning: replacing previous import 'data.table::transpose' by 'purrr::transpose'
    ## when loading 'dxtime'

``` r
library(dplyr)
library(tidyr)
library(stringr)
library(data.table)
library(purrr)
library(RODBC)
```

## CAD data preparation

### connect to database

``` r
SQLIP<-"192.168.1.6"
DBName<-"CGRD_CAD" 
id<-"********"
pw<-"********" 
dbhandle<-
  odbcDriverConnect(paste0('driver={ODBC Driver 13 for SQL Server};',
                           'server=',SQLIP,';', 
                           'database=',DBName,';',
                           'uid=',id,';',
                           'pwd=',pw))

#門診
resPDAF <- sqlQuery(dbhandle, 'SELECT  LOC, IDCODE, IPDAT,DSSID FROM RSASOPDAF ')
#急診
resRDAF <- sqlQuery(dbhandle, 'SELECT  LOC, IDCODE, IPDAT,DSSID FROM RSASERDAF ')
#住院
resICDSUM <- sqlQuery(dbhandle, 'SELECT LOC, IDCODE,DXKD1, DXKD2, DXKD3,DXKD4, DXKD5, DXKD6,DXKD7, DXKD8, DXKD9,DXKD10, IPDAT FROM RSASICDSUM ')
#歸戶
respat <- sqlQuery(dbhandle, 'SELECT LOC,IDCODE,SEX,BIRTHDAY FROM RSASHOSP')

close(dbhandle)
```

### merge diagnosis data

``` r
resPDAF <- resPDAF %>% mutate(origi="O")
resRDAF <- resRDAF %>% mutate(origi="E")
resPDAF <- resPDAF %>% mutate(seq="")
resRDAF <- resRDAF %>% mutate(seq="")
resICDSUM <- unique(resICDSUM)
resICDSUM <- gather(resICDSUM,key="seq",value=ICD,DXKD1,DXKD2,DXKD3)
resICDSUM <- resICDSUM %>% mutate(origi="I")
resPDAF <- resPDAF %>% select(LOC,ID=IDCODE,date=IPDAT,ICD=DSSID,origi,seq)
resRDAF <- resRDAF %>% select(LOC,ID=IDCODE,date=IPDAT,ICD=DSSID,origi,seq)
resICDSUM <- resICDSUM %>% select(LOC,ID=IDCODE,date=IPDAT,ICD,origi,seq)
res <- rbind(resPDAF,resRDAF,resICDSUM)
res$date<-as.Date(as.character(res$date),"%Y%m%d")
res$ID <- as.character(res$ID)
res$ID <- str_trim(res$ID)
res <- res[complete.cases(res),]
```

### 歸戶檔pre-process

``` r
respat <- readRDS("data/respat.rds")
pat <- setDT(respat)
pat <- pat %>% unique() %>% rename(c(ID=IDCODE,gender=SEX,birthday=BIRTHDAY))
pat$birthday <- as.Date(as.character(pat$birthday),"%Y%m%d")
pat$gender <- as.character(pat$gender)
pat$ID <- str_trim(pat$ID)
pat$gender <- str_trim(pat$gender)
pat$LOC <- str_trim(pat$LOC)

# gender
patgender <- pat %>% select(ID,gender) %>% unique() %>% 
  filter(gender %in% "F"|gender %in% "M") %>% group_by(ID) %>%
  mutate(numgender=n_distinct(gender)) %>% filter(numgender==1) %>% 
  select(ID,gender)  %>% unique()
setDT(patgender)
# birthday
# 日期沒問題的人
patbir <- pat %>% select(LOC,ID,birthday) %>% unique() %>% 
  filter(! year(birthday) == 9999) %>% 
  group_by(ID) %>% mutate(numbir=n_distinct(birthday))
patbir_B <- patbir %>% filter(numbir==1) %>% select(ID,birthday)  %>% unique() 
setDT(patbir_B)
# 日期有問題，但可判斷出生日
patbir_C <- patbir %>% filter(! numbir==1) %>% select(LOC,ID,birthday,numbir) %>% 
  arrange(ID) %>% group_by(ID) %>% mutate(numbir2=duplicated(birthday)) %>% 
  group_by(ID,numbir2) %>% mutate(numtrue=length(numbir2)) %>% 
  filter(numbir2==TRUE ) %>% select(-LOC) %>% unique() %>% 
  group_by(ID) %>% mutate(numbir=n_distinct(birthday)) %>% 
  filter(numbir==1) %>% select(ID,birthday) %>% unique()
setDT(patbir_C)
# # 無法辨別出生日者(病例組以第一次看診為準)
# undefined <- patbir %>% filter(! ID %in% patbir_B$ID) %>% filter(! ID %in% patbir_C$ID) %>% select(LOC,ID,birthday)
# setDT(undefined)
# # CAD者以第一次CAD確診時的院區的生日資料為準
# temp <- res %>% filter(ID %in% CAD$ID)
# setDT(temp)
# temp <- temp[grep("^410|^411|^412|^413|^414|^I20|^I21|^I22|^I23|^I24|^I25",temp$ICD)]
# temp <- temp %>% arrange(ID,date) %>% group_by(ID) %>% slice(1) %>%
# select(LOC,ID) %>% rename("firLOC"="LOC")
# patbir_D <- left_join(undefined,temp,by="ID") %>% filter(LOC==firLOC) %>% select(ID,birthday)
# setDT(patbir_D)
# # 非CAD者以第一次看診時的資料為準
# undefined <- undefined %>% filter(! ID %in% patbir_D$ID)
# temp <- controlrec %>% arrange(ID,desc(date)) %>% group_by(ID) %>%
# slice(1) %>% select(LOC,ID) %>% rename("firLOC"="LOC")
# patbir_E <- left_join(undefined,temp,by="ID") %>% filter(LOC==firLOC) %>% select(ID,birthday)
# # 合併可辨別出生日的個案 
# patbir <- rbind(patbir_B,patbir_C,patbir_D,patbir_E)
patbir <- rbind(patbir_B,patbir_C)
# 合併性別和生日
pat <- full_join(patbir,patgender)
pat <- pat[is.na(pat$birthday)==FALSE & is.na(pat$gender)==FALSE,]
head(pat)
```

    ##                                          ID   birthday gender
    ## 1: DE3BCB948E4DBE921A88FF6F4AD4E2F3511810DA 1930-05-19      M
    ## 2: EDDB8545AE16FA2A754DB043408114F20B616554 1929-02-05      F
    ## 3: 3F36E0511AED65E94DFC0A3FCCEB8EFFC33F313B 1945-03-08      F
    ## 4: 68D517CBF4C04DB004B6BEF17F35388D43E024E8 1980-12-23      M
    ## 5: 9D4D8EC493D25D8AC1C1DB109078CC31CE48C9A5 1959-03-14      F
    ## 6: 71C7D5636ECC0B3415352013B3980B19E7778155 1940-05-11      M

### case and control groups

``` r
res <- readRDS("data/res.rds")
res_filter <- res %>% filter(ID %in% pat$ID)

groupData <- selectCases(dxDataFile = res_filter,
                    idColName = ID,           
                    icdColName = ICD,       
                    dateColName = date,
                    groupDataType = ICD,
                    icd10usingDate = "2016/01/01",
                    caseCondition = "^410|^411|^412|^413|^414|^I20|^I21|^I22|^I23|^I24|^I25",
                    caseCount = 2,
                    periodRange = c(30, 365),
                    caseName = "Selected")
```

    ## Warning: The ICD mentioned above matches to "NA" due to the format or other
    ## issues.

    ## Warning: "Wrong ICD format" means the ICD has wrong format

    ## Warning: "Wrong ICD version" means the ICD classify to wrong ICD version (cause
    ## the "icd10usingDate" or other issues)

``` r
# case group(ok) from this
CAD <- groupData %>% 
        filter(selectedCase=="Selected") %>% 
        left_join(res %>% group_by(ID) %>% arrange(date) %>% slice(1) %>% select(ID,date)) %>% 
        rename("firstdate"="date") %>% 
        filter(firstdate < firstCaseDate)

CAD <- CAD %>% left_join(pat,by="ID")
CAD$CADage <- round((unclass(CAD$firstCaseDate)-unclass(CAD$birthday))/365.25,digits=2)
CAD$firstage <- round((unclass(CAD$firstdate)-unclass(CAD$birthday))/365.25,digits=2)
CAD <- CAD %>% filter(CADage>18)
CADrec <- res %>% filter(ID %in% CAD$ID)
head(CAD)
```

    ##                                          ID selectedCase count firstCaseDate
    ## 1: 45E9799D36A0FA5653F90735DD6659BF587B83A3     Selected  1105    2007-01-24
    ## 2: 7DBB3D48001432C826FF383D9B85CE515BF00DBE     Selected   871    2001-01-12
    ## 3: E95EF85A8C2CEB65E4B98E5490AC9D126546692C     Selected   868    2005-06-23
    ## 4: A7778E4BD3924F963141CF62341EBFBF368915B9     Selected   478    2003-12-09
    ## 5: 17E0628BEE5A27187487DFDD5217BF34129EAB03     Selected   405    2005-12-23
    ## 6: 1FF228FD3020051C2688E462BF2EE387508C2C10     Selected   366    2008-04-21
    ##    endCaseDate    period MostCommonICD MostCommonICDCount  firstdate   birthday
    ## 1:  2018-10-31 4298 days          4139                931 2001-07-24 1946-02-20
    ## 2:  2015-06-08 5260 days          4148                864 2001-01-03 1953-10-15
    ## 3:  2015-06-03 3632 days          4149                845 2001-02-22 1947-06-05
    ## 4:  2013-10-25 3608 days          4149                430 2001-01-10 1950-12-22
    ## 5:  2014-06-25 3106 days          4149                390 2005-07-29 1933-12-23
    ## 6:  2016-05-14 2945 days          4149                322 2001-01-02 1929-12-04
    ##    gender CADage firstage
    ## 1:      M  60.93    55.42
    ## 2:      F  47.24    47.22
    ## 3:      M  58.05    53.72
    ## 4:      F  52.96    50.05
    ## 5:      M  72.00    71.60
    ## 6:      M  78.38    71.08

``` r
# control group
control <- groupData %>% 
  filter(selectedCase=="non-Selected") %>% 
  left_join(res %>% group_by(ID) %>% arrange(date) %>% slice(1) %>% select(ID,date)) %>% 
  rename("firstdate"="date") %>% 
  left_join(res %>% group_by(ID) %>% arrange(desc(date)) %>% slice(1) %>% select(ID,date)) %>% 
  rename("enddate"="date")

control <- control %>% left_join(pat,by="ID")
control$firstage <- round((unclass(control$firstdate)-unclass(control$birthday))/365.25,digits=2)
control$endage <- round((unclass(control$enddate)-unclass(control$birthday))/365.25,digits=2)
control <- control %>% filter(endage>18)
controlrec <- res %>% filter(ID %in% control$ID)
head(control)
```

    ##                                          ID selectedCase count firstCaseDate
    ## 1: B88B618705A3B10224F3A636E28B664B58FCCECE non-Selected    NA          <NA>
    ## 2: B06FDF1075373520E35E75C28B067B199E60F239 non-Selected    NA          <NA>
    ## 3: 9EE29E8C425BADD575E4D02E2178BAC3B7E88365 non-Selected    NA          <NA>
    ## 4: C5C61696A4563F9D43B156717C2AB985F20E6F50 non-Selected    NA          <NA>
    ## 5: 7496F7D5775027DAEE1BF7DBC5EDCEB51EC2C9FA non-Selected    NA          <NA>
    ## 6: 75FD7847EF1736CCA8E9AFB5805DD627010D9426 non-Selected    NA          <NA>
    ##    endCaseDate  period MostCommonICD MostCommonICDCount  firstdate    enddate
    ## 1:        <NA> NA days          <NA>                 NA 2005-12-21 2018-10-31
    ## 2:        <NA> NA days          <NA>                 NA 2009-04-21 2018-07-17
    ## 3:        <NA> NA days          <NA>                 NA 2002-04-24 2018-03-05
    ## 4:        <NA> NA days          <NA>                 NA 2003-01-02 2018-10-25
    ## 5:        <NA> NA days          <NA>                 NA 2001-03-30 2018-10-12
    ## 6:        <NA> NA days          <NA>                 NA 2007-08-21 2018-10-19
    ##      birthday gender firstage endage
    ## 1: 1950-04-20      M    55.67  68.53
    ## 2: 1950-12-25      F    58.32  67.56
    ## 3: 1931-09-19      M    70.60  86.46
    ## 4: 1980-03-05      F    22.83  38.64
    ## 5: 1970-08-10      F    30.64  48.17
    ## 6: 1965-01-19      M    42.58  53.75

``` r
# add label
addlabel <- function(x){
  if(x %in% CAD$ID){
    label=1
  }else if(x %in% control$ID)
  {
    label=0
  }else{
    lable=NA
  }
}
pat$lable <- unlist(map(pat$ID,addlabel))
head(pat)
```

    ##                                          ID   birthday gender lable
    ## 1: DE3BCB948E4DBE921A88FF6F4AD4E2F3511810DA 1930-05-19      M    NA
    ## 2: EDDB8545AE16FA2A754DB043408114F20B616554 1929-02-05      F     0
    ## 3: 3F36E0511AED65E94DFC0A3FCCEB8EFFC33F313B 1945-03-08      F    NA
    ## 4: 68D517CBF4C04DB004B6BEF17F35388D43E024E8 1980-12-23      M     0
    ## 5: 9D4D8EC493D25D8AC1C1DB109078CC31CE48C9A5 1959-03-14      F     0
    ## 6: 71C7D5636ECC0B3415352013B3980B19E7778155 1940-05-11      M     0

``` r
knitr::opts_current$get(c(
  "cache",
  "cache.path",
  "cache.rebuild",
  "dependson",
  "autodep"
))
```

    ## $cache
    ## [1] 3
    ## 
    ## $cache.path
    ## [1] "dxtime_CAD_cache/gfm/"
    ## 
    ## $cache.rebuild
    ## [1] FALSE
    ## 
    ## $dependson
    ## NULL
    ## 
    ## $autodep
    ## [1] FALSE

## time-series analysis

### filter
