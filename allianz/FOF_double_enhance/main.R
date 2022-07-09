library(data.table)
library(lubridate)
library(dplyr)
library(feather)
# source("utils_data.R",encoding = "UTF-8")
#以2020-8-31为例
#obs_date <- as.POSIXct("2017-03-31")

#月底日期列表----------------------------------------------
date_list <- seq(as.POSIXct("2002-01-01"), length=12*20, by="1 month")-ddays(1)
para_table <- data.table(m = seq(1,12,1),
                         report = rep(c(1,2,3),4),
                         type = c(rep("key",2),"detail",rep("key",4),
                                  "detail","detail",rep("key",3)),
                         type1 = c(rep("key",3),rep("detail",3),
                                   rep("key",3),rep("detail",3)),
                         type2 = c(rep("detail",3),rep("key",3),
                                   rep("detail",3),rep("key",3)))
#obs是指盈余公告观察期，report是指对应报告期,type对应公告种类

#读取数据------------------------------------------------------------------------
#获得总的基础基金池
MF_FundArchives <- as.data.table(readRDS("./data/MF_FundArchives.rds"))
SecuMain <- as.data.table(readRDS("./data/SecuMain.rds"))
MF_JYFundType <- as.data.table(readRDS("./data/MF_JYFundType.rds"))
#基金持仓报告
MF_KeyStockPortfolio <- as.data.table(readRDS("./data/MF_KeyStockPortfolio.rds"))
MF_StockPortfolioDetail <- as.data.table(readRDS("./data/MF_StockPortfolioDetail.rds"))
#获取业绩报告
fin_performance_express <- as.data.table(readRDS("./data/fin_performance_express.rds"))
fin_performance_forecast <- as.data.table(readRDS("./data/fin_performance_forecast.rds"))
der_prob_excess_stock <- as.data.table(readRDS("./data/der_prob_excess_stock.rds"))
#获取基金净值信息
MF_FundNetValueRe <- as.data.table(readRDS("./data/MF_FundNetValueRe.rds"))
MF_MainFinancialIndexQ <- as.data.table(readRDS("./data/MF_MainFinancialIndexQ.rds"))
MF_HolderInfo <- as.data.table(readRDS("./data/MF_HolderInfo.rds"))
#获取股票行情信息
stock_quote <- as.data.table(read_feather("./data/qt_dailyquote.feather"))
# name_list <- c("EFFECTIVEDATE","CANCELDATE")
# MF_JYFundType[,(name_list):=lapply(.SD,as.Date),.SDcols=name_list]
##不能直接转换为date类型，因为POSIX转换之后会减少一天，涉及时区问题

#1.获得基金名称(16698)-------------------------------------------------------------
Fund_tmp <- merge(MF_FundArchives[,.(INNERCODE,SECURITYCODE,LISTEDDATE,EXPIREDATE)],
                  SecuMain[,.(INNERCODE,CHINAMEABBR)],by="INNERCODE",all.x=TRUE)
#获得基金分类以及不同分类所处时间
Type_tmp <- merge(Fund_tmp,MF_JYFundType[,.(INNERCODE,EFFECTIVEDATE,CANCELDATE,
                                            SECASSETCATNAME)],by='INNERCODE',all.x=TRUE)
dim(Type_tmp)#(17107)
#2.同名只保留A类基金,选择标准股票型和偏股型------------------------------------------
Type_tmp <- Type_tmp[SECASSETCATNAME%in%c("标准股票型","偏股型"),]
dim(Type_tmp)#(3869)
Type_tmp[,`:=`(NAME=sapply(CHINAMEABBR,function(x){substr(x,1,nchar(x)-1)}),
               CLASS=sapply(CHINAMEABBR,function(x){substr(x,nchar(x),nchar(x))}))]
name_tmp <- Type_tmp[,.(NUM=.N),by=.(NAME)]#根据同名进行计数统计
num_tmp <- merge(Type_tmp,name_tmp,by="NAME",all.x=TRUE)#将计数结果加入列变量
del_list <- unlist(num_tmp[NUM!=1&CLASS%in%c("B","C","E","H","O","R","X"),.(INNERCODE)])
#删除的基金清单
pool_tmp <- Type_tmp[!(INNERCODE%in%del_list),]#保留A类基础基金池名单
dim(pool_tmp)#(2757)
pool_tmp <- pool_tmp[CLASS!='C',]#还有几支C，删除
dim(pool_tmp)#(2752)
pool_tmp <- pool_tmp[!grepl("FOF",pool_tmp[,CHINAMEABBR]),]#删除FOF基金
dim(pool_tmp)#(2685)
pool_tmp <- pool_tmp[!grepl("LOF",pool_tmp[,CHINAMEABBR]),]#删除LOF基金
dim(pool_tmp)#(2584)
filldate <- as.POSIXct('2021-08-25')#截止到取数据那天，cancel为NA，可以理解为仍然存续或者未更新
pool_tmp[is.na(CANCELDATE),CANCELDATE:=filldate]#进行空值填充



