

library(data.table)
library(lubridate)
library(WindR)
w.start()

#日期
data_fold <- './data'
from <- '2010-02-01'
to <- '2020-11-30'
date_range <- seq(as.Date(from), as.Date(to), by="1 month")-lubridate::ddays(1)
date_range <- date_range[month(date_range)%in%c(1,4,8,10)]

#需要的数据
#重仓股
keystock <- GCAMCPUB::readDtRds(file.path(data_fold,'keystock.rds'))
setkey(keystock,INNERCODE,INFOPUBLDATE)
#股票日行情数据
dailyquote <- GCAMCPUB::readDtRds(file.path(data_fold,"dailyquote.rds"))
kcb <- GCAMCPUB::readDtRds(file.path(data_fold,"kcb_performance.rds"))
stock_price <- rbind(dailyquote,kcb)
setkey(stock_price,INNERCODE,TRADINGDAY)
#选出来的基金列表
total_list <- GCAMCPUB::readDtRds(file.path(data_fold,"fund_list.rds"))
keystock_info <- unique(keystock[,.(INNERCODE,INFOPUBLDATE,INFOPUBLDATE2=INFOPUBLDATE)])
setkey(keystock_info,INNERCODE,INFOPUBLDATE)


#重仓股选取
stock_list <- data.table()
for(i in seq_along(date_range)) {
  DATE <- date_range[i]
  cross_fundlist <- total_list[TRADINGDAY==DATE,.(INNERCODE,SECUCODE,TRADINGDAY)]
  cross_fundlist[,INFOPUBLDATE:=keystock_info[J(cross_fundlist$INNERCODE,cross_fundlist$TRADINGDAY),roll=T]$INFOPUBLDATE2]
  cross_fundlist <- cross_fundlist[!is.na(INFOPUBLDATE)]
  
  stock_pool <- keystock[J(cross_fundlist$INNERCODE,cross_fundlist$INFOPUBLDATE)]
  stock_pool[,DATE:=DATE]
  stock_pool <- stock_pool[,.(INNERCODE,DATE,REPORTDATE,INFOPUBLDATE,
                              STOCKINNERCODE,RATIOINNV,SHARESHOLDING,MARKETVALUE)]
  
  stock_list <- rbind(stock_list, stock_pool)
}

keystocks_pool <- stock_list[,.(SHARESHOLDING=sum(SHARESHOLDING)),by=.(STOCKINNERCODE,DATE)]
keystocks_pool[, CLOSEPRICE := stock_price[J(keystocks_pool$STOCKINNERCODE, keystocks_pool$DATE), roll = T]$CLOSEPRICE]
#重仓股列表
keystocks_pool <- keystocks_pool[!is.na(CLOSEPRICE),.(INNER_CODE=STOCKINNERCODE,DATE,SHARESHOLDING,MV=SHARESHOLDING*CLOSEPRICE)]
keystocks_pool[,WEIGHT:=MV/sum(MV),by=.(DATE)]

tdays_data<-w.tdays("2010-01-01","2021-12-15")$Data$DATETIME
tdays_data <- fread("DATE.csv")
tdays_data <- as.Date(tdays_data$DATE)
for(i in seq_along(date_range)){
  dt_diff <- tdays_data - date_range[i]
  STARTDATE <- tdays_data[which(dt_diff>0)[1]]
  keystocks_pool[DATE==date_range[i],STARTDAY:=STARTDATE]
}


keystocks_pool[,PRICE:=stock_price[J(keystocks_pool$INNER_CODE,keystocks_pool$STARTDAY), roll = T]$BACKWARDPRICE]

pos_date <- unique(keystocks_pool$STARTDAY)

origin_cash <- 100000000
pos <- data.table()
cash <- c()
for(i in seq_along(pos_date)) {
  if(i==1){
    new_cash <- origin_cash
    cash <- rbind(cash,new_cash)
  }else{
    sell_pos <- copy(pos[DATE==pos_date[i-1],])
    sell_pos[,DATE:=pos_date[i]]
    sell_pos[,PRICE:=stock_price[J(sell_pos$INNER_CODE,sell_pos$DATE), roll = T]$BACKWARDPRICE]
    new_cash <- sell_pos[,sum(NUM*PRICE)]
    cash <- rbind(cash,new_cash)
  }
  stk_list <- keystocks_pool[STARTDAY==pos_date[i],]
  hold_pos <- stk_list[,.(INNER_CODE, DATE=STARTDAY, NUM=new_cash*WEIGHT/PRICE)]
  pos <- rbind(pos,hold_pos)
}

# keystocks_pool[,SECUCODE:=SecuMain[J(keystocks_pool$INNER_CODE)]$SECUCODE]
# keystocks_pool[,MARKET:=SecuMain[J(keystocks_pool$INNER_CODE)]$SECUMARKET]
# keystocks_pool[MARKET==90,SECUCODE:=paste0(SECUCODE,'.SZ')]
# keystocks_pool[MARKET==83,SECUCODE:=paste0(SECUCODE,'.SH')]
# keystocks_pool[,DATE:=NULL]
# setnames(keystocks_pool,"STARTDAY",'DATE')

fwrite(keystocks_pool[,.(SECUCODE,DATE,WEIGHT)],'stock_list.csv')
