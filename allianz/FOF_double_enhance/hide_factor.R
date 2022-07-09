# obs_date <- as.POSIXct("2010-07-31")
# 
# df_tmp <- fund_pool(obs_date,pool_tmp)#找到给定日期对应的基础基金池
# report_index <- which(date_list==obs_date)-para_table[m==month(obs_date),report]
#2019-9-28重做了隐藏交易能力因子的计算

#####全新的隐藏交易能力------------------------------------------------
hide_factor = function(df_temp,Type_tmp,obs_date,report_index){
  key_stock <- key_stock_pool(df_tmp,Type_tmp,obs_date,report_index)#给定观察期对应基金的重仓股
  stock_temp <- stock_quote[TRADINGDAY>=(as.Date(date_list[report_index])+period(1,"day"))
                            &TRADINGDAY<=(as.Date(date_list[report_index+1])+period(1,"day")),
                            .(INNERCODE,TRADINGDAY,CLOSEPRICE)]
  setkey(stock_temp,INNERCODE,TRADINGDAY)
  #删除不满过去3个月的个股
  exclude_list <- stock_temp[,.N,by=.(INNERCODE)][N!=max(N),INNERCODE]
  stock_temp <- stock_temp[!INNERCODE%in%exclude_list,]
  #计算过去3个月的个股收益率
  stock <- stock_temp[,.(RET=last(CLOSEPRICE)/first(CLOSEPRICE)-1),by=.(INNERCODE)]
  stock[RET<median(RET),SRANK:=-1]
  stock[RET>=median(RET),SRANK:=1]
  #1:前50%，-1：后50%
  stock <- stock[order(-RET),]
  #N-0-N
  #偶数:2
  #奇数：(N-0)/((N-1)/2)
  n <- nrow(stock)
  if(n%%2==0){
    stock[1:(n/2),SR:=seq(n,2,-2)]
    stock[(n/2+1):n,SR:=seq(2,n,2)]
  }else{
    stock[1:((n-1)/2),SR:=seq(n,n/((n-1)/2),-n/((n-1)/2))]
    stock[((n+1)/2):n,SR:=seq(0,n,n/((n-1)/2))]
  }
  stock[,SW:=SR/sum(SR)]
  setkey(stock,INNERCODE)
  #obs_date <- as.POSIXct("2010-07-31")
  # Secu_temp <- SecuMain[,.(LISTEDDATE=max(LISTEDDATE,na.rm=T)),by=.(INNERCODE)]
  # setnames(Secu_temp,"INNERCODE","STOCKINNERCODE")#获取股票的成立日期
  # key_stock <- Secu_temp[key_stock,on=.(STOCKINNERCODE)]
  # key_stock <- key_stock[(date_list[report_index]-LISTEDDATE)>90,]#2424
  key_stock <- key_stock[STOCKINNERCODE%in%stock$INNERCODE,]
  key_stock[,SRANK:=stock[J(STOCKINNERCODE)]$SRANK]#匹配股票对应的过去3个月收益率排序区间
  
  for(i in unique(key_stock$INNERCODE)){
    key_stock[INNERCODE==i&SRANK==1,RANK:=frank(RATIOINNV)]
    key_stock[INNERCODE==i&SRANK==-1,RANK:=frank(-RATIOINNV)]
  }
  key_stock[,SW:=stock[J(STOCKINNERCODE)]$SW]
  factor <- key_stock[,.(STRUCT=sum(RANK*SW)),by=.(INNERCODE)]
  factor[,STRUCT:=(STRUCT-min(STRUCT))/(max(STRUCT)-min(STRUCT))]
  
  fund_t <- MF_FundNetValueRe[INNERCODE%in%df_tmp$INNERCODE
                              &TRADINGDAY>=(as.Date(date_list[report_index])+period(1,"day"))
                              &TRADINGDAY<=(as.Date(date_list[report_index+1])+period(1,"day")),]
  setkey(fund_t,INNERCODE,TRADINGDAY)
  fund_ret <- fund_t[,.(RET=last(UNITNVRESTORED)/first(UNITNVRESTORED)-1),by=.(INNERCODE)]
  fund_ret[,RK:=frank(RET)]
  fund_ret[,SRET:=RK/nrow(fund_ret)]
  fund_ret[,`:=`(RET=NULL,RK=NULL)]
  
  final <- fund_ret[factor,on=.(INNERCODE)]
  final[,HIDE:=SRET-STRUCT]
  # final[order(-HIDE),][1:30,INNERCODE]
  final[,.(INNERCODE,HIDE)]
}



