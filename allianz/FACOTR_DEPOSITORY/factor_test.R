library(data.table)
library(GCAMCPUB)
library(WindR)
library(ggplot2)
w.start()

#分股票池检查效果
source("dev_factor.R")
source("factor_test_utils.R")
start <- as.Date("2009-01-01")
end <- as.Date("2021-12-30")
dates <- as.Date("2013-01-01")#2013年以后的因子表现
save_folder <- "./"
uqer_folder <- "uqer_basic_py"
base_data <- "base_data"
N <- 5#因子分组组数
index_list <- c("3145","4978")#沪深300：3145;中证500：4978

#读取所需数据-------------------------
#股价数据
dailyquote <- GCAMCPUB::readDtRds(file.path(base_data,"dailyquote.rds"))
kcb <- GCAMCPUB::readDtRds(file.path(base_data,"kcb_performance.rds"))
stkprice <- rbind(dailyquote,kcb)
setkey(stkprice,INNERCODE,TRADINGDAY)
index <- GCAMCPUB::readDtRds(file.path(base_data,"index.rds"))
index[,RET:=CLOSEPRICE/PREVCLOSEPRICE-1]
setkey(index,INNERCODE,TRADINGDAY)
#指数成分数据#沪深300：3145;中证500：4978
indexcomponent <- GCAMCPUB::readDtRds(file.path(base_data,"indexcomponent.rds"))
indexcomponent <- indexcomponent[INDEXINNERCODE%in%c("3145","4978"),]
#基本信息表
secumain <- GCAMCPUB::readDtRds(file.path(base_data,"SecuMain.rds"))
setkey(secumain,INNERCODE)


factor <- "func_revenuetoasset_chg"
vars <- c("EPS")

origin_raw <- factor_test$prep_data(stkprice, factor,vars,dates,start,end,save_folder,uqer_folder)
month_raw <- factor_test$day_to_month(origin_raw,start,end,stkprice)
index_select <- "4978"
factor_raw <- factor_test$index_select(indexcomponent,index_select,N,month_raw)
test_package(factor_raw,stkprice,secumain)#因子指标
yearly_check(factor_raw, stkprice, index, index_select)#分年度
# 
# for(index_select in index_list) {
#   factor_raw <- factor_test$index_select(indexcomponent,index_select,N,month_raw)
#   test_package(factor_raw,stkprice,secumain)
# }




