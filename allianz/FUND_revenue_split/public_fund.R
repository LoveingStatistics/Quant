library(data.table)
library(lubridate)
library(dplyr)
library(feather)
library(stringr)

#2021-12-21---------------
#1. 没有判断开放还是封闭
#2. 使用了年报半年报全部持仓数据
#3. 

data_fold <- './data'
#读取数据------------------------------------------------------------------------
#获得总的基础基金池
# MF_FundArchives <- GCAMCPUB::readDtRds(file.path(data_fold,'MF_FundArchives.rds'))
SecuMain <- GCAMCPUB::readDtRds(file.path(data_fold,'SecuMain.rds'))
MF_JYFundType <- GCAMCPUB::readDtRds(file.path(data_fold,'MF_JYFundType.rds'))
setkey(SecuMain,INNERCODE)

#基金指标 MF_MainFinancialIndexQ
share_index <- GCAMCPUB::readDtRds(file.path(data_fold,'share_index.rds'))
share_index <- share_index[,.(INNERCODE,ENDDATE,INFOPUBLDATE,NETASSETSVALUE,NVPERSHARE)]
setkey(share_index,INNERCODE,ENDDATE)

#基金净值数据 MF_FundNetValueRe
netvalue <- GCAMCPUB::readDtRds(file.path(data_fold,'netvalue.rds'))
netvalue <- netvalue[,.(INNERCODE,TRADINGDAY,UNITNVRESTORED)]
setkey(netvalue,INNERCODE,TRADINGDAY)
netvalue[,RET:=UNITNVRESTORED/shift(UNITNVRESTORED)-1,by=.(INNERCODE)]

#重仓股数据 MF_KeyStockPortfolio
keystock <- GCAMCPUB::readDtRds(file.path(data_fold,'keystock.rds'))
setkey(keystock,INNERCODE,REPORTDATE)

#全部持仓数据 MF_DetailStockPortfolio
detailstock <- GCAMCPUB::readDtRds(file.path(data_fold,'detailstock.rds'))
setkey(detailstock,INNERCODE,REPORTDATE)
#1.获得基金名称-------------------------------------------------------------
from <- '2010-02-01'
to <- '2021-11-30'
# date_range <- seq(as.Date(from),as.Date(to),by='1 day')

date_range <- seq(as.Date(from), as.Date(to), by="1 month")-lubridate::ddays(1)


FundType <- MF_JYFundType[THIRDASSETCATCODE%in%c('110101','120101'),]
fundtype_panel <- data.table()
for(i in 1:length(date_range)){
  fundtype_cross <- FundType[EFFECTIVEDATE<=date_range[i]
                             &(CANCELDATE>=date_range[i]|is.na(CANCELDATE)),
                             .(INNERCODE,TRADINGDAY=date_range[i],
                               THIRDASSETCATCODE,EFFECTIVEDATE,CANCELDATE)]
  fundtype_panel <- rbind(fundtype_panel,fundtype_cross)
}
setkey(fundtype_panel,INNERCODE,TRADINGDAY)

#2.同名只保留A类基金------------------------------------------------------------
fundtype_panel[,CHINAMEABBR:=SecuMain[J(fundtype_panel$INNERCODE)]$CHINAMEABBR]
fundtype_panel[,CHINAMEABBR:=lapply(.(CHINAMEABBR),trimws,which='both')]
class_rule <- "(([ABCDEFH]/[ABCDEFH])|[ABCDEFH]|[^ABCDEFH])$"

fundtype_panel[,`:=`(NAME=sapply(CHINAMEABBR,
                                 function(x){substr(x,1,regexpr(class_rule,x)[1]-1)}),
               CLASS=sapply(CHINAMEABBR,function(x){str_extract(x,class_rule)}))]

#3.选择开放式基金----------------------------------
fundtype_panel[,CAT:=SecuMain[J(fundtype_panel$INNERCODE)]$SECUCATEGORY]
fundtype_panel <- fundtype_panel[CAT==8,]

#计算基金的合并总规模
########################################################################

fundtype_panel <- share_index[fundtype_panel,
                              on=.(INNERCODE=INNERCODE,
                                   INFOPUBLDATE=TRADINGDAY),
                              roll=TRUE]
setnames(fundtype_panel,'INFOPUBLDATE','TRADINGDAY')
fundtype_panel[,SIZE:=sum(NETASSETSVALUE,na.rm = TRUE),by=.(TRADINGDAY,NAME)]

#删除同类非A基金
name_class <- fundtype_panel[,.(NUM=.N),by=.(TRADINGDAY,NAME)]#根据同名进行计数统计
setkey(name_class,TRADINGDAY,NAME)
fundtype_panel[,NUM:=name_class[J(fundtype_panel$TRADINGDAY,fundtype_panel$NAME)]$NUM]
fund_pool <- rbind(fundtype_panel[NUM==1,],fundtype_panel[NUM!=1&CLASS%in%c('A','A/B'),])
dim(fund_pool)#保留A类基金

fund_pool <- fund_pool[!grepl("FOF",fund_pool[,CHINAMEABBR]),]#删除FOF基金
fund_pool <- fund_pool[!grepl("LOF",fund_pool[,CHINAMEABBR]),]#删除LOF基金
fund_pool <- fund_pool[!CLASS%in%c('C','B','E','H'),]
dim(fund_pool)

#基金筛选-----------------------------------------------------------------------
#基金份额取对数 MF_SharesChange EndShares INNERCODE,ENDDATE,STATPERIOD
fund_pool[,SHARE:=log(SIZE/NVPERSHARE)]#基金份额取对数
#夏普比 最大回撤 MF_NetValuePerformance UnitNV
##MF_KeyStockPortfolio RatioInNV
#持股集中度 (StkConcI):基金持仓的前10大重仓股票占组合净资产值的比例之和

keystock[,CONCENTRATION:=sum(RATIOINNV,na.rm=TRUE),by=.(INNERCODE,REPORTDATE)]

stock_concentration <- keystock[,lapply(.SD,first),
                                by=.(INNERCODE,REPORTDATE),
                                .SDcols=c('CONCENTRATION','INFOPUBLDATE')]

fund_pool <- stock_concentration[fund_pool,
                                 on=.(INNERCODE=INNERCODE,
                                      INFOPUBLDATE=TRADINGDAY),
                                 roll=TRUE]
setnames(fund_pool,"INFOPUBLDATE","TRADINGDAY")

#夏普比 没有年化 目前只能做到计算一天，计算速度比较慢

#最大回撤函数
drawdown <- function(nav){
  if(length(nav)!=0){
    cum_max <- cummax(nav)
    drowdown <- 1-nav/cum_max
    return(max(drowdown))
  }else{
    return(NA)
  }
}

cal_factor <- function(DATE,net_sub,fund_pool) {
  td_fundpool <- fund_pool[TRADINGDAY==DATE,]
  td_fundpool[,MDD:=sapply(INNERCODE,function(x){
    net_sub[INNERCODE==x&TRADINGDAY<DATE&TRADINGDAY>=DATE-365,
            drawdown(UNITNVRESTORED)]
  })]
  td_fundpool[,SHARPE:=sapply(INNERCODE,function(x){
    net_sub[INNERCODE==x&TRADINGDAY<DATE&TRADINGDAY>=DATE-365,
            mean(RET,na.rm=TRUE)/sd(RET,na.rm=TRUE)]
  })]
  td_fundpool
}


#2021-12-15
#验证基金表现-------------------------------------------------------------------
fund_pool[,DURATION:=(TRADINGDAY-as.Date(EFFECTIVEDATE)-lubridate::ddays(1))]
fund_pool <- fund_pool[DURATION>=365*24*3600&SIZE>=1e8,] #成立大于一年且规模在1亿以上
setkey(fund_pool, INNERCODE,TRADINGDAY)

n <- 100
total_list <- data.table()
for(i in seq_along(date_range)){
  DATE <- date_range[i]
  
  net_sub <- netvalue[TRADINGDAY>=as.Date(DATE)-365&TRADINGDAY<=as.Date(DATE),]
  
  td_fund <- cal_factor(DATE,net_sub,fund_pool)
  
  td_fund[,c('CONCENTRATION','SHARPE','MDD','SHARE'):=lapply(.SD,GCAMCQT::normalizing_zscore),
          .SDcols=c('CONCENTRATION','SHARPE','MDD','SHARE')]
  td_fund[,SCORE:=(CONCENTRATION+SHARPE-MDD-SHARE)/4]
  td_fund <- td_fund[,.(INNERCODE,TRADINGDAY,CHINAMEABBR,SCORE)]
  td_fund[,RANK:=frank(-SCORE,na.last='keep')]
  setorder(td_fund,'RANK')
  td_fund <- td_fund[RANK<=n,]
  td_fund[,SECUCODE:=SecuMain[J(td_fund$INNERCODE)]$SECUCODE]
  
  total_list <- rbind(total_list,td_fund)
  print(paste0("已经完成:",DATE))
}

library(WindR)
w.start()
tdays_data<-w.tdays("2010-01-01","2021-12-15")$Data$DATETIME

for(i in seq_along(date_range)){
  dt_diff <- tdays_data - date_range[i]
  STARTDATE <- tdays_data[which(dt_diff>0)[1]]
  total_list[TRADINGDAY==date_range[i],STARTDAY:=STARTDATE]
}
#shift之后进行收益率匹配
total_list[,TILLDAY:=shift(STARTDAY,-n)]


#FOF组合回测------------------------

# total_list <- fread("big_list.csv")
#2021-12-24调仓时间修改------------
#换仓时点，每年1,4,8,10月
tot_list <- total_list[month(TRADINGDAY)%in%c(1,4,8,10),]
# tot_list <- tot_list[,lapply(.SD,head,30),by=.(TRADINGDAY)]
tot_list[,TILLDAY:=shift(STARTDAY,-n)]
date_range <- unique(tot_list$TRADINGDAY)
tot_nav <- data.table()
for(i in 1:(length(date_range)-1)) {
  DATE <- date_range[i]
  this_list <- tot_list[TRADINGDAY==DATE,]
  hold <- netvalue[TRADINGDAY>=unique(this_list$STARTDAY)
                   &TRADINGDAY<=unique(this_list$TILLDAY)
                   &INNERCODE%in%this_list$INNERCODE,]
  setkey(hold,INNERCODE,TRADINGDAY)
  
  hold_nav <- hold[,.(PNAV=UNITNVRESTORED/first(UNITNVRESTORED),TRADINGDAY),
                   by=.(INNERCODE)][,.(NAV=mean(PNAV)),by=.(TRADINGDAY)]
  setorder(hold_nav,"TRADINGDAY")
  hold_nav[,RET:=NAV/shift(NAV)-1]
  tot_nav <- rbind(tot_nav,hold_nav[!is.na(RET),.(TRADINGDAY,RET)])
}
tot_nav[,NAV:=cumprod(1+RET)]
#------------------------


tot_nav <- data.table()
for(i in 1:(length(date_range)-1)) {
  DATE <- date_range[i]
  this_list <- total_list[TRADINGDAY==DATE,]
  hold <- netvalue[TRADINGDAY>=unique(this_list$STARTDAY)
           &TRADINGDAY<=unique(this_list$TILLDAY)
           &INNERCODE%in%this_list$INNERCODE,]
  setkey(hold,INNERCODE,TRADINGDAY)

  hold_nav <- hold[,.(PNAV=UNITNVRESTORED/first(UNITNVRESTORED),TRADINGDAY),
                   by=.(INNERCODE)][,.(NAV=mean(PNAV)),by=.(TRADINGDAY)]
  setorder(hold_nav,"TRADINGDAY")
  hold_nav[,RET:=NAV/shift(NAV)-1]
  tot_nav <- rbind(tot_nav,hold_nav[!is.na(RET),.(TRADINGDAY,RET)])
}
tot_nav[,NAV:=cumprod(1+RET)]

#股票基金指数
w_wsd_data <- as.data.table(w.wsd("H11021.CSI","close,pre_close",
                                  "2010-02-01","2021-10-08")$Data)
w_wsd_data[,RET:=CLOSE/PRE_CLOSE-1]
w_wsd_data <- w_wsd_data[DATETIME>='2010-02-02',]
w_wsd_data[,NAV:=cumprod(1+RET)]

plot(tot_nav[,.(TRADINGDAY,NAV)],type='l')
lines(w_wsd_data[,.(DATETIME,NAV)],col='red')


tot_nav[,YEAR:=lubridate::year(TRADINGDAY)]
fof_sum <- tot_nav[,.(RET_FOF=GCAMCPUB::f_fmt_pct(last(NAV)/first(NAV)-1,2),
                      MDD_FOF=GCAMCPUB::f_fmt_pct(drawdown(NAV),2)),by=.(YEAR)]

w_wsd_data[,YEAR:=lubridate::year(DATETIME)]
base_sum <- w_wsd_data[,.(RET_BASE=GCAMCPUB::f_fmt_pct(last(NAV)/first(NAV)-1,2),
                          MDD_BASE=GCAMCPUB::f_fmt_pct(drawdown(NAV),2)),by=.(YEAR)]
setkey(base_sum,YEAR)
tot_sum <- base_sum[fof_sum]
tot_sum

annual_fof <- tot_nav[,(last(NAV)/first(NAV))^(365/.N)-1]
annual_base <- w_wsd_data[,(last(NAV)/first(NAV))^(365/.N)-1]

cat(paste0("FOF100组合年化收益为:",GCAMCPUB::f_fmt_pct(annual_fof,2),"\n",
           "股票基金组合年化收益为:",GCAMCPUB::f_fmt_pct(annual_base,2)))


#stock backtest--------------------------------
#commission no
# total_list <- fread("total_list.csv",encoding="UTF-8")

#提取给定日期对应的基金重仓股票池
get_key <- function(DATE,keystock,total_list) {
  code <- total_list[TRADINGDAY==DATE,INNERCODE]
  stock_pool <- keystock[INNERCODE%in%code
                         &INFOPUBLDATE<=as.Date(DATE),]
  stock_pool[,NEWDATE:=max(INFOPUBLDATE),by=.(INNERCODE)]
  stock_pool <- stock_pool[INFOPUBLDATE==NEWDATE,.(REPORTDATE,
                                                   SHARESHOLDING,STOCKINNERCODE,
                                                   INFOPUBLDATE),by=.(INNERCODE)]
  stock_pool[,CHINAME:=SecuMain[J(stock_pool$STOCKINNERCODE)]$CHINAMEABBR]
  stock_pool[,SECUCODE:=SecuMain[J(stock_pool$STOCKINNERCODE)]$SECUCODE]
  stock_pool <- stock_pool[,.(SECUCODE=first(SECUCODE),CHNAME=first(CHINAME),
                              HOLDING=sum(SHARESHOLDING),DATE=DATE),
                           by=.(STOCKINNERCODE)]
  stock_pool
}
get_detail <- function(DATE,detailstock,total_list) {
  code <- total_list[TRADINGDAY==DATE,INNERCODE]
  stock_pool <- detailstock[INNERCODE%in%code
                         &INFOPUBLDATE<=as.Date(DATE),]
  stock_pool[,NEWDATE:=max(INFOPUBLDATE),by=.(INNERCODE)]
  stock_pool <- stock_pool[INFOPUBLDATE==NEWDATE,.(REPORTDATE,
                                                   SHARESHOLDING,STOCKINNERCODE,
                                                   INFOPUBLDATE),by=.(INNERCODE)]
  stock_pool[,CHINAME:=SecuMain[J(stock_pool$STOCKINNERCODE)]$CHINAMEABBR]
  stock_pool[,SECUCODE:=SecuMain[J(stock_pool$STOCKINNERCODE)]$SECUCODE]
  stock_pool <- stock_pool[,.(SECUCODE=first(SECUCODE),CHNAME=first(CHINAME),
                              HOLDING=sum(SHARESHOLDING),DATE=DATE),
                           by=.(STOCKINNERCODE)]
  stock_pool
}

from <- '2010-02-01'
to <- '2021-10-31'
# date_range <- seq(as.Date(from),as.Date(to),by='1 day')

date_range <- seq(as.Date(from), as.Date(to), by="1 month")-lubridate::ddays(1)


stock_list <- data.table()
for(i in 1:(length(date_range)-1)) {
  DATE <- date_range[i]
  if(month(DATE)%in%c(1,10)){
    td_stock <- get_key(DATE,keystock,tot_list)
    td_stock[,TILLDAY:=date_range[i+1]]
  }else{
    td_stock <- get_detail(DATE,detailstock,tot_list)
    td_stock[,TILLDAY:=date_range[i+1]]
  }
  stock_list <- rbind(stock_list, td_stock)
}

for(i in 1:(length(date_range)-1)){
  dt_diff <- tdays_data - date_range[i]
  STARTDATE <- tdays_data[which(dt_diff>0)[1]]
  stock_list[DATE==date_range[i],STARTDAY:=STARTDATE]
  
  dt_diff <- tdays_data - date_range[i+1]
  ENDDATE <- tdays_data[which(dt_diff>0)[1]]
  stock_list[DATE==date_range[i],ENDDAY:=ENDDATE]
}

stock_list <- stock_list[!is.na(SECUCODE),]
setnames(stock_list,"STOCKINNERCODE","INNERCODE")

dailyquote <- GCAMCPUB::readDtRds(file.path(data_fold,"dailyquote.rds"))
kcb <- GCAMCPUB::readDtRds(file.path(data_fold,"kcb_performance.rds"))
kcb <- kcb[,.(INNERCODE,TRADINGDAY,PREVCLOSEPRICE,CLOSEPRICE)]
stock_price <- rbind(dailyquote,kcb)
setkey(stock_price,INNERCODE,TRADINGDAY)
stock_list[,PRICE:=stock_price[J(stock_list$INNERCODE,stock_list$STARTDAY)]$CLOSEPRICE]
stock_list <- stock_list[!is.na(PRICE),]
setnames(stock_list,"DATE","TRADINGDAY")

#2021-12-24
stock_list[,VALUE:=HOLDING*PRICE]
stock_list[,WEIGHT:=VALUE/sum(VALUE),by=.(TRADINGDAY)]
###

stock_nav <- data.table()
for(i in 1:(length(date_range)-1)) {
  DATE <- date_range[i]
  this_list <- stock_list[TRADINGDAY==DATE,]
  setkey(this_list,INNERCODE)
  hold <- stock_price[TRADINGDAY>=unique(this_list$STARTDAY)
                   &TRADINGDAY<=unique(this_list$ENDDAY)
                   &INNERCODE%in%this_list$INNERCODE,]
  hold[,WEIGHT:=this_list[J(hold$INNERCODE)]$WEIGHT]
  setkey(hold,INNERCODE,TRADINGDAY)
  
  hold_nav <- hold[,.(PNAV=CLOSEPRICE/first(CLOSEPRICE),WEIGHT=first(WEIGHT),TRADINGDAY),
                   by=.(INNERCODE)][,.(NAV=sum(PNAV*WEIGHT)),by=.(TRADINGDAY)]
  setorder(hold_nav,"TRADINGDAY")
  hold_nav[,RET:=NAV/shift(NAV)-1]
  stock_nav <- rbind(stock_nav,hold_nav[!is.na(RET),.(TRADINGDAY,RET)])
}
stock_nav[,NAV:=cumprod(1+RET)]

lines(stock_nav[,.(TRADINGDAY,NAV)],col='green')







