library(data.table)
library(lubridate)
library(stringr)
library(WindR)
w.start()

data_fold <- './data'
from <- '2010-02-01'
to <- '2022-02-15'
date_range <- seq(as.Date(from), as.Date(to), by="1 month")-lubridate::ddays(1)

#读取数据------------------------------------------------------------------------
#获得总的基础信息基金池
SecuMain <- GCAMCPUB::readDtRds(file.path(data_fold,'SecuMain.rds'))
MF_JYFundType <- GCAMCPUB::readDtRds(file.path(data_fold,'MF_JYFundType.rds'))
FundType <- MF_JYFundType[THIRDASSETCATCODE%in%c('110101','120101'),] #股票 偏股型基金
setkey(SecuMain,INNERCODE)
#基金指标
share_index <- GCAMCPUB::readDtRds(file.path(data_fold,'share_index.rds'))
share_index <- share_index[,.(INNERCODE,ENDDATE,INFOPUBLDATE,NETASSETSVALUE,NVPERSHARE)]
setkey(share_index,INNERCODE,ENDDATE)
#基金净值数据
netvalue <- GCAMCPUB::readDtRds(file.path(data_fold,'netvalue.rds'))
netvalue <- netvalue[,.(INNERCODE,TRADINGDAY,UNITNVRESTORED)]
setkey(netvalue,INNERCODE,TRADINGDAY)
netvalue[,RET:=UNITNVRESTORED/shift(UNITNVRESTORED)-1,by=.(INNERCODE)]
#重仓股
keystock <- GCAMCPUB::readDtRds(file.path(data_fold,'keystock.rds'))
setkey(keystock,INNERCODE,INFOPUBLDATE)


# 找出所有时间段符合的标准股票型基金基础表-------------------------------
fundtype_panel <- local({
  #1.偏股型基金
  fundtype_panel <- data.table()
  for(i in 1:length(date_range)){
    fundtype_cross <- FundType[EFFECTIVEDATE<=date_range[i]
                               &(CANCELDATE>=date_range[i]|is.na(CANCELDATE)),
                               .(INNERCODE,TRADINGDAY=date_range[i],
                                 THIRDASSETCATCODE,EFFECTIVEDATE,CANCELDATE)]
    fundtype_panel <- rbind(fundtype_panel,fundtype_cross)
  }
  setkey(fundtype_panel,INNERCODE,TRADINGDAY)
  #2.选择开放式基金
  fundtype_panel[,CAT:=SecuMain[J(fundtype_panel$INNERCODE)]$SECUCATEGORY]
  fundtype_panel <- fundtype_panel[CAT==8,]
  #3.成立时间一年以上
  fundtype_panel[,DURATION:=(TRADINGDAY-as.Date(EFFECTIVEDATE)-lubridate::ddays(1))]
  fundtype_panel <- fundtype_panel[DURATION>=365*24*3600,]
})

#在基础表中选取同名A类并且合并规模在1亿以上
fund_pool <- local({
  #3.同名不同类基金
  fundtype_panel[,CHINAMEABBR:=SecuMain[J(fundtype_panel$INNERCODE)]$CHINAMEABBR]
  fundtype_panel[,CHINAMEABBR:=lapply(.(CHINAMEABBR),trimws,which='both')]
  class_rule <- "(([ABCDEFH]/[ABCDEFH])|[ABCDEFH]|[^ABCDEFH])$"
  
  fundtype_panel[,`:=`(NAME=sapply(CHINAMEABBR,
                                   function(x){substr(x,1,regexpr(class_rule,x)[1]-1)}),
                       CLASS=sapply(CHINAMEABBR,function(x){stringr::str_extract(x,class_rule)}))]
  #4.基金的合并总规模
  fundtype_panel <- share_index[fundtype_panel,
                                on=.(INNERCODE=INNERCODE,
                                     INFOPUBLDATE=TRADINGDAY),
                                roll=TRUE]
  setnames(fundtype_panel,'INFOPUBLDATE','TRADINGDAY')
  fundtype_panel[,SIZE:=sum(NETASSETSVALUE,na.rm = TRUE),by=.(TRADINGDAY,NAME)]
  fundtype_panel <- fundtype_panel[SIZE>1e8,]
  #5.删除同类非A基金
  fundtype_panel[,NUM:=.N,by=.(TRADINGDAY,NAME)]
  fund_pool <- rbind(fundtype_panel[NUM==1,],fundtype_panel[NUM!=1&CLASS%in%c('A','A/B'),])
  #删除FOF LOF基金
  fund_pool <- fund_pool[!grepl("FOF",fund_pool[,CHINAMEABBR])
                         &(!grepl("LOF",fund_pool[,CHINAMEABBR]))
                         &(!CLASS%in%c('C','B','E','H')),]

  fund_pool[,SHARE:=log(SIZE/NVPERSHARE)]#基金份额取对数
  setkey(fund_pool, INNERCODE,TRADINGDAY)
  fund_pool[,.(INNERCODE,TRADINGDAY,SHARE)]
})


#基金筛选因子-----------------------------------------------------------------------

#持股集中度 (StkConcI):基金持仓的前10大重仓股票占比
stock_concentration <- keystock[,.(INFOPUBLDATE=first(INFOPUBLDATE),
                                   CONCENTRATION=sum(RATIOINNV,na.rm=TRUE)),
                                by=.(INNERCODE,REPORTDATE)]

fund_pool <- stock_concentration[fund_pool,
                                 on=.(INNERCODE=INNERCODE,
                                      INFOPUBLDATE=TRADINGDAY),
                                 roll=TRUE]
setnames(fund_pool,"INFOPUBLDATE","TRADINGDAY")

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
#计算给定时间点基金的最大回撤和夏普比
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


#基金筛选-------------------------------------------------------------------
n <- 100
total_list <- data.table()
for(i in seq_along(date_range)){
  DATE <- date_range[i]
  
  net_sub <- netvalue[TRADINGDAY>=as.Date(DATE)-365&TRADINGDAY<=as.Date(DATE),]
  
  td_fund <- cal_factor(DATE,net_sub,fund_pool)#计算最大回撤和夏普比因子
  #四个因子标准化
  td_fund[,c('CONCENTRATION','SHARPE','MDD','SHARE'):=lapply(.SD,GCAMCQT::normalizing_zscore),
          .SDcols=c('CONCENTRATION','SHARPE','MDD','SHARE')]
  #因子加和
  td_fund[,SCORE:=(CONCENTRATION+SHARPE-MDD-SHARE)/4]
  td_fund <- td_fund[,.(INNERCODE,TRADINGDAY,SCORE)]
  #按照因子得分排序
  td_fund[,RANK:=frank(-SCORE,na.last='keep')]
  setorder(td_fund,'RANK')
  td_fund <- td_fund[RANK<=n,]
  td_fund[,SECUCODE:=SecuMain[J(td_fund$INNERCODE)]$SECUCODE]
  
  total_list <- rbind(total_list,td_fund[,.(INNERCODE,SECUCODE,TRADINGDAY)])
  print(paste0("已经完成:",DATE))
}

GCAMCPUB::writeDtRds(total_list,file.path(data_fold,'fund_list.rds'))






