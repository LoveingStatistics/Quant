

###equity report
# source('prep_wind_data.R')

#1. return stat----------------------------------------------------------------
###index return
index_ret <- GCAMCPUB::readDtRds(file.path(jydb_path,'index_return.rds'))
setkey(index_ret,SECUCODE,TRADINGDAY)

#计算周、年收益率
week_tmp <- index_ret[TRADINGDAY>=prev_week_date,.(WEEK_RET=last(CLOSEPRICE)/first(PREVCLOSEPRICE)-1),
by=.(SECUCODE)]
year_tmp <- index_ret[TRADINGDAY>=prev_year_date,.(YEARLY_RET=last(CLOSEPRICE)/first(PREVCLOSEPRICE)-1),
by=.(SECUCODE)]
#bind
index_ret_stat <- week_tmp[year_tmp,on=.(SECUCODE=SECUCODE)]
index_ret_stat[,`:=`(SECUNAME=c('上证50','沪深300','科创50','中证500','创业板指'),
SECUCODE=NULL)]
#plot data
melt_index_ret <- melt(index_ret_stat,id.vars="SECUNAME")
melt_index_ret[,SECUNAME:=factor(SECUNAME,levels=c('上证50','沪深300',
                                                   '中证500','创业板指',
                                                   '科创50'))]
#save data
GCAMCPUB::saveDtRds(melt_index_ret,file.path(output_path,'index_ret.rds'))

###industry return
industry_name <- as.data.table(GCAMCPUB::readDtRds(file.path(wind_path,'industry_name','industry_name.rds')))

industry_name <- industry_name[,.(wind_code,sec_name)]
industry_name[,`:=`(wind_code=gsub('.WI','',wind_code),
sec_name=gsub('\\(中信\\)','',sec_name))]
industry_info <- GCAMCPUB::readDtRds(file.path(jydb_path,'industry_code.rds'))
industry_price <- GCAMCPUB::readDtRds(file.path(jydb_path,'industry_index_price.rds'))
secumain <- GCAMCPUB::readDtRds(file.path(jydb_path,'secumain.rds'))
setkey(industry_name,wind_code)
setkey(secumain,INNERCODE)
setkey(industry_info,INDEXCODE)

industry_info[,CODE:=secumain[J(industry_info$INDEXCODE)]$SECUCODE]
need_code <- paste0('CI00',seq(5001,5030,1))
index_code <- industry_info[CODE%in%need_code,INDEXCODE]
required_industry <- industry_price[INNERCODE%in%index_code,
.(TRADINGDAY,INNERCODE,CLOSEPRICE,PREVCLOSEPRICE)]
required_industry[,CODE:=industry_info[J(required_industry$INNERCODE)]$CODE]
required_industry[,NAME:=industry_name[J(required_industry$CODE)]$sec_name]
required_industry[,RET:=CLOSEPRICE/PREVCLOSEPRICE-1]
setkey(required_industry,INNERCODE,TRADINGDAY)
week_ret <- required_industry[TRADINGDAY>=prev_week_date&TRADINGDAY<=update_date,
.(WEEK_RET=last(CLOSEPRICE)/first(PREVCLOSEPRICE)-1),by=.(NAME)]
year_ret <- required_industry[TRADINGDAY>=prev_year_date&TRADINGDAY<=update_date,
.(YEAR_RET=last(CLOSEPRICE)/first(PREVCLOSEPRICE)-1),by=.(NAME)]

industry_ret_stat <- week_ret[year_ret,on=.(NAME=NAME)]
#plot data
melt_indus_ret <- melt(industry_ret_stat,id.vars='NAME')
melt_indus_ret[,NAME:=factor(NAME)]
#save data
GCAMCPUB::saveDtRds(melt_indus_ret,file.path(output_path,'industry_ret.rds'))


#2. index&industry valuation----------------------------------------------------
#index_valuation
index_valuation <- GCAMCPUB::readDtRds(file.path(jydb_path,'index_valuation.rds'))
setkey(index_valuation,SECUCODE,TRADINGDAY)
index_valuation[,`:=`(PE_RANK=frank(PE_TTM,na.last='keep')/.N,PB_RANK=frank(PB_LF,na.last='keep')/.N,
PEG_RANK=frank(PEG,na.last='keep')/.N),by=.(SECUABBR)]
index_valuation_stat <- index_valuation[TRADINGDAY==last(index_valuation$TRADINGDAY),]
index_valuation_stat[,SECUNAME:=c('上证50','沪深300','科创50','中证500','创业板指')]
#plot data
index_pb <- index_valuation_stat[,.(SECUNAME,PB_LF,PB_RANK)]
index_pb[,SECUNAME:=factor(SECUNAME,levels=c('上证50','沪深300',
                                             '中证500','创业板指',
                                             '科创50'))]
index_pe <- index_valuation_stat[,.(SECUNAME,PE_TTM,PE_RANK)]
index_pe[,SECUNAME:=factor(SECUNAME,levels=c('上证50','沪深300',
                                             '中证500','创业板指',
                                             '科创50'))]
index_peg <- index_valuation_stat[,.(SECUNAME,PEG,PEG_RANK)]
index_peg[,SECUNAME:=factor(SECUNAME,levels=c('上证50','沪深300',
                                              '中证500','创业板指',
                                              '科创50'))]
#saving data
GCAMCPUB::saveDtRds(index_pb,file.path(output_path,'index_pb.rds'))
GCAMCPUB::saveDtRds(index_pe,file.path(output_path,'index_pe.rds'))
GCAMCPUB::saveDtRds(index_peg,file.path(output_path,'index_peg.rds'))

#industry_valuation
indus_name <- as.data.table(GCAMCPUB::readDtRds(file.path(wind_path,'industry_name','industry_name.rds')))
setkey(indus_name,wind_code)

industry_valuation_cal <- function(wind_path,indus_name,type){
path <- paste0('indusry_',type,'.rds')
indus2 <- GCAMCPUB::readDtRds(file.path(wind_path,path))
indus3<-melt(indus2,id.vars=c("DATETIME"),variable.name="indus")
indus3[,rank_value:=frank(value,na.last = 'keep')/.N,by=.(indus)]
indus4<-indus3[order(DATETIME)][,.(percentile=last(value),percentrank=last(rank_value)),by=.(indus)]
indus4[,indus_name:= indus_name[J(indus4$indus)]$sec_name]
indus4
}

industry_valuation_cal <- function(wind_path,indus_name,type){
  value_type <- paste0('industry_',type)
  dfs <- prep_wind$read_all(value_type)
  
  names(dfs) <- c('CODE','VALUE','TRADINGDAY')
  dfs[,rank_value:=frank(VALUE,na.last = 'keep')/.N,by=.(CODE)]
  dfs<-dfs[order(TRADINGDAY)][,.(percentile=last(VALUE),percentrank=last(rank_value)),by=.(CODE)]
  dfs[,indus_name:= indus_name[J(dfs$CODE)]$sec_name]
  dfs
}

indus_pb <- industry_valuation_cal(wind_path,indus_name,'pb')
indus_pe <- industry_valuation_cal(wind_path,indus_name,'pe')
setkey(indus_pe,CODE)
indus_peg <- industry_valuation_cal(wind_path,indus_name,'peg')
setkey(indus_peg,CODE)
indus_pb[,`:=`(pb=percentile,pe=indus_pe[J(indus_pb$CODE)]$percentile,
peg=indus_peg[J(indus_pb$CODE)]$percentile,
pb_rank=percentrank,pe_rank=indus_pe[J(indus_pb$CODE)]$percentrank,
peg_rank=indus_peg[J(indus_pb$CODE)]$percentrank,
indus_name=gsub('\\(中信\\)','',indus_name))]
indus_valuation_stat <- indus_pb[,.(indus_name,pb,pe,peg,pb_rank,pe_rank,peg_rank)]
#plot data
indus_pb <- indus_valuation_stat[,.(indus_name,pb,pb_rank)]
indus_pe <- indus_valuation_stat[,.(indus_name,pe,pe_rank)]
indus_peg <- indus_valuation_stat[,.(indus_name,peg,peg_rank)]
#saving data
GCAMCPUB::saveDtRds(indus_pb,file.path(output_path,'industry_pb.rds'))
GCAMCPUB::saveDtRds(indus_pe,file.path(output_path,'industry_pe.rds'))
GCAMCPUB::saveDtRds(indus_peg,file.path(output_path,'industry_peg.rds'))


#3. A stock liquidity-----------------------------------------------------------
turnover <- GCAMCPUB::readDtRds(file.path(jydb_path,'a_turnover.rds'))
turnover <- turnover[,.(STOCKTURNOVER=sum(STOCKTURNOVER)),by=ENDDATE][order(ENDDATE),]
daily_value <- mean(turnover[ENDDATE > prev_week_date & ENDDATE <= update_date]$STOCKTURNOVER)
prev_daily_value <- mean(turnover[ENDDATE > prev_prev_week_date & ENDDATE <= prev_week_date]$STOCKTURNOVER)
weekly_changes <- daily_value / prev_daily_value - 1
turnover_bind <- rbind(turnover[ENDDATE >= prev_year_date & ENDDATE <= prev_week_date],
data.table(ENDDATE = update_date,STOCKTURNOVER = daily_value))
pct_to_year <- turnover_bind[,rank:=frank(STOCKTURNOVER)/.N][,last(rank)]
turnover_tbl <- data.table(DAILY_VALUE = daily_value,
CHG = weekly_changes,
POS_TO_YEAR = pct_to_year)
turnover_tbl
TURNOVER <- turnover[ENDDATE>'2017-01-01']
TURNOVER[,STOCKTURNOVER:=STOCKTURNOVER/100]
GCAMCPUB::saveDtRds(TURNOVER,file.path(output_path,'turnover.rds'))
GCAMCPUB::saveDtRds(turnover_tbl,file.path(output_path,'turnover_tbl.rds'))

#4. ETF-------------------------------------------------------------------------
#lky
#计算规模统计 相对比重 流入流出/总规模
etf_name <- c('etf_stock','etf_bond','etf_commodity','etf_monetary','etf_crossborder')
etf <- data.table()
for(j in seq_along(etf_name)){
  dfs <- prep_wind$read_all(etf_name[j])
  etf <- rbind(etf, dfs)
}

sum_etf <- etf[, .(INFLOW = sum(MF_NETINFLOW,na.rm=T)),by=.(TRADINGDAY,TYPE)]

last_week1 <- sum_etf[TRADINGDAY>=prev_week_date
&TRADINGDAY<=update_date,.(last_week=mean(INFLOW)/1e8),by=.(TYPE)]
setkey(last_week1,TYPE)
last_month <- sum_etf[TRADINGDAY<=update_date
&TRADINGDAY>=update_date-lubridate::ddays(20),
.(last_month=mean(INFLOW)/1e8),by=.(TYPE)]
setkey(last_month,TYPE)
last_three <- sum_etf[TRADINGDAY<=update_date
&TRADINGDAY>=update_date-lubridate::ddays(60),
.(last_three=mean(INFLOW)/1e8),by=.(TYPE)]
setkey(last_three,TYPE)
last_six <- sum_etf[TRADINGDAY<=update_date
&TRADINGDAY>=update_date-lubridate::ddays(120),
.(last_six=mean(INFLOW)/1e8),by=.(TYPE)]
setkey(last_six,TYPE)
last_year <- sum_etf[TRADINGDAY<=update_date
&TRADINGDAY>=prev_year_date,
.(last_year=mean(INFLOW)/1e8),by=.(TYPE)]
setkey(last_year,TYPE)

last_week1[,`:=`(last_month=last_month[J(TYPE),]$last_month,
last_three=last_three[J(TYPE),]$last_three,
last_six=last_six[J(TYPE),]$last_six,
last_year=last_year[J(TYPE),]$last_year)]

GCAMCPUB::saveDtRds(last_week1,file.path(output_path,"etf_stat.rds"))


#5. north money-----------------------------------------------------------------
#北向资金每周净流入统计
#单位：元
north_value <- GCAMCPUB::readDtRds(file.path(jydb_path,'north_value.rds'))
colnames(north_value) <- c('TRADINGDAY','BTRADEVALUE','STRADEVALUE')
a_trading_dates <- unique(turnover[TRADINGDAY>'2017-01-01'
&TRADINGDAY<=update_date,TRADINGDAY])
fill_north_value <- data.table(TRADINGDAY=a_trading_dates)
setkey(fill_north_value,TRADINGDAY)
setkey(north_value,TRADINGDAY)
north_value <- north_value[,.(BTRADEVALUE=sum(BTRADEVALUE),
STRADEVALUE=sum(STRADEVALUE)),by=.(TRADINGDAY)]

north_value[,NETVALUE:=BTRADEVALUE-STRADEVALUE]
fill_north_value[,NETVALUE:=north_value[J(fill_north_value$TRADINGDAY)]$NETVALUE]
fill_north_value[is.na(NETVALUE),NETVALUE:=0]

fill_north_value <- week_seek(fill_north_value)
north_value_stat <- fill_north_value[,.(DATE=last(TRADINGDAY),NETVALUE=sum(NETVALUE)),by=.(week)]
north_value_stat[,CUM_VALUE:=cumsum(NETVALUE)]

GCAMCPUB::saveDtRds(north_value_stat,file.path(output_path,'north_flow.rds'))

#北向资金行业占比情况
#数据基础处理
lgt <- GCAMCPUB::readDtRds(file.path(jydb_path,'lgt_holding.rds'))
main <- GCAMCPUB::readDtRds(file.path(jydb_path,'secumain.rds'))
quote <- GCAMCPUB::readDtRds(file.path(jydb_path,'daily_quote.rds'))
main <- main[,.(INNERCODE,COMPANYCODE)]
quote <- quote[,.(INNERCODE,TRADINGDAY,CLOSEPRICE,OPENPRICE,HIGHPRICE,LOWPRICE)]
lgt <- quote[lgt,on=.(INNERCODE=INNERCODE,TRADINGDAY=ENDDATE)]
lgt <- main[lgt,on=.(INNERCODE=INNERCODE)]
#companycode和innercode可能不一致

industry <- GCAMCPUB::readDtRds(file.path(jydb_path,'industry.rds'))
setkey(industry,COMPANYCODE,INFOPUBLDATE)
industry <- industry[,.(INDUSTRY=last(FIRSTINDUSTRYNAME)),by=.(COMPANYCODE)]
lgt <- industry[lgt,on=.(COMPANYCODE=COMPANYCODE)]
##north industry

mkv <- GCAMCPUB::readDtRds(file.path(jydb_path,'stockperformance.rds'))
#单位：流通市值：万元
mkv <- mkv[,.(INNERCODE,TRADINGDAY,NEGOTIABLEMV)]
mkv <- main[mkv,on=.(INNERCODE=INNERCODE)]
mkv[,NEGOTIABLEMV:=NEGOTIABLEMV*10000]
mkv <- industry[mkv,on=.(COMPANYCODE=COMPANYCODE)]
mkv <- mkv[TRADINGDAY>'2017-01-01',][order(TRADINGDAY),]
mkv <- mkv[,.(SEC_VALUE=sum(NEGOTIABLEMV)),by=.(TRADINGDAY,INDUSTRY)]
####
# north_industry <- readRDS(file.path(jydb_path,'north_industry.rds'))
north_industry <- lgt
north_industry <- north_industry[(!is.na(CLOSEPRICE))&(!is.na(INDUSTRY)),]

north_industry <- north_industry[,lapply(.SD,first),by=.(INNERCODE,TRADINGDAY)]

north_industry <- week_seek(north_industry)
week_date <- north_industry[,.(week_date=last(TRADINGDAY)),by=.(week)][,week_date]
north_tmp <- north_industry[TRADINGDAY%in%week_date,]
north_tmp <- north_tmp[,.(HOLD_VALUE=sum(CLOSEPRICE*SHARESHOLDING)),by=.(TRADINGDAY,INDUSTRY)]

# mkv_industry <- readRDS(file.path(jydb_path,'mkv_industry.rds'))
mkv_industry <- mkv
setkey(mkv_industry,TRADINGDAY,INDUSTRY)
north_tmp[,IND_VALUE:=mkv_industry[J(north_tmp$TRADINGDAY,north_tmp$INDUSTRY)]$SEC_VALUE]
north_tmp[,RATIO:=HOLD_VALUE/IND_VALUE]
north_industry_hold <- north_tmp[,.(TRADINGDAY,INDUSTRY,RATIO)]
#plot data
north_industry_hold[,INDUSTRY:=factor(INDUSTRY)]
north_week_end <- unique(north_industry_hold$TRADINGDAY)
north_industry_hold_plot <- north_industry_hold[TRADINGDAY%in%tail(north_week_end,10),]
north_industry_hold_plot[,TRADINGDAY:=strftime(TRADINGDAY,'%Y%m%d')]
#saving data
GCAMCPUB::saveDtRds(north_industry_hold_plot,file.path(output_path,'north_industry_hold.rds'))


#北向行业变动
setkey(north_industry,INNERCODE,TRADINGDAY)
used_symbol <- unique(north_industry[TRADINGDAY==max(TRADINGDAY),INNERCODE])
used_dates <- unique(north_industry$TRADINGDAY)
used_tbl <- auto_tbl(used_dates, used_symbol)
used_tbl[,HOLD_STOCK:=north_industry[J(used_tbl$INNER_CODE,used_tbl$TRADINGDAY)]$SHARESHOLDING]
used_tbl[is.na(HOLD_STOCK),HOLD_STOCK:=0]
used_tbl[,`:=`(CLOSE_PRICE=north_industry[J(used_tbl$INNER_CODE,used_tbl$TRADINGDAY)]$CLOSEPRICE,
OPEN_PRICE=north_industry[J(used_tbl$INNER_CODE,used_tbl$TRADINGDAY)]$OPENPRICE,
HIGH_PRICE=north_industry[J(used_tbl$INNER_CODE,used_tbl$TRADINGDAY)]$HIGHPRICE,
LOW_PRICE=north_industry[J(used_tbl$INNER_CODE,used_tbl$TRADINGDAY)]$LOWPRICE)]
used_tbl[,DEAL_PRICE:=(CLOSE_PRICE+OPEN_PRICE+HIGH_PRICE+LOW_PRICE)/4]
used_tbl[,DELTA_HOLD_STOCKS := HOLD_STOCK - shift(HOLD_STOCK),by = INNER_CODE]
used_tbl[,DELTA_AMOUNT:=DEAL_PRICE*DELTA_HOLD_STOCKS]
used_tbl[,INDUSTRY:=north_industry[J(used_tbl$INNER_CODE,used_tbl$TRADINGDAY)]$INDUSTRY]
net_cash_flow <- used_tbl[,.(CASH_IN=sum(DELTA_AMOUNT)),by=.(INDUSTRY,TRADINGDAY)]
net_cash_flow <- week_seek(net_cash_flow)
net_cash_flow <- net_cash_flow[,.(TRADINGDAY=last(TRADINGDAY),CASH_IN=sum(CASH_IN)),
by=.(week,INDUSTRY)]
net_cash_flow <- net_cash_flow[!is.na(INDUSTRY),]
setkey(north_tmp,TRADINGDAY,INDUSTRY)
net_cash_flow[,NEW_HOLD_VALUE:=north_tmp[J(net_cash_flow$TRADINGDAY,net_cash_flow$INDUSTRY)]$HOLD_VALUE]
net_cash_flow[,INI_HOLD_VALUE:=shift(NEW_HOLD_VALUE),by=.(INDUSTRY)]
net_cash_flow[,CHG_RATIO:=CASH_IN/INI_HOLD_VALUE]

#plot data
net_cash_flow_plot <- net_cash_flow[TRADINGDAY%in%tail(north_week_end,10),]
net_cash_flow_plot[,`:=`(INDUSTRY=factor(INDUSTRY,levels=levels(north_industry_hold_plot$INDUSTRY)),
TRADINGDAY=strftime(TRADINGDAY,format='%Y%m%d'))]
#saving data
GCAMCPUB::saveDtRds(net_cash_flow_plot,file.path(output_path,'north_net_cash_flow.rds'))


#6. credit trading--------------------------------------------------------------
#credit单位-亿元
credit <- prep_wind$read_all('credittrading')
a_turnover <- GCAMCPUB::readDtRds(file.path(jydb_path,'a_turnover.rds'))
setkey(a_turnover,ENDDATE)
#a_turnover单位-百万元
a_turnover <- a_turnover[,.(STOCKTURNOVER=sum(STOCKTURNOVER)),by=.(ENDDATE)]
a_turnover[,STOCKTURNOVER:=STOCKTURNOVER/100]
cre_turn <- a_turnover[credit,on=.(ENDDATE=DATETIME)][,.(ENDDATE,STOCKTURNOVER,CLOSE)]
cre_turn[,pct:=CLOSE/STOCKTURNOVER]
cre_pct <- cre_turn[,.(ENDDATE,CLOSE,pct)]
cre_pct#两融占全A总成交额比例

GCAMCPUB::saveDtRds(cre_pct,file.path(output_path,'credit_trading.rds'))

#7. factor return---------------------------------------------------------------

#8. stock deviation-------------------------------------------------------------
deviation <- GCAMCPUB::readDtRds(file.path(jydb_path,'stockperformance.rds'))
deviation <- deviation[,.(INNERCODE,TRADINGDAY,PREVCLOSEPRICE,CLOSEPRICE)]
deviation
deviation[,ret:=CLOSEPRICE/PREVCLOSEPRICE-1]

deviation2 <- copy(deviation)
deviation2[,MARK:=shift(TRADINGDAY,60),by=.(INNERCODE)]
deviation3 <- deviation2[!is.na(MARK),.(INNERCODE,TRADINGDAY,ret)]
sd_de <- deviation3[,.(sd=sd(ret,na.rm=T)),by=TRADINGDAY][order(TRADINGDAY),]
sd_de_oneyear <- sd_de[TRADINGDAY >= prev_oneyear_date & TRADINGDAY <= update_date]
sd_pct <- sd_de_oneyear[,rank_pct:=frank(sd)/.N]

GCAMCPUB::saveDtRds(sd_pct,file.path(output_path,'stock_deviation.rds'))





