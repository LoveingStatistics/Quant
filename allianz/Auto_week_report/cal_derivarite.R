

#1.股指期货 期权数据------------------------------------------------------------

futures_ret <- function(df,update_date){
  chg_name <- colnames(df)[-1]
  lw_name <- paste0(colnames(df)[-1],'_RET_LW')
  lm_name <- paste0(colnames(df)[-1],'_RET_LM')
  df[,(lw_name):=lapply(.SD,function(x){(x-shift(x))/shift(x)}),.SDcols=chg_name]#last week return
  df[,(lm_name):=lapply(.SD,function(x){(x-shift(x,4))/shift(x,4)}),.SDcols=chg_name]#last month return
  
  needed_data <- melt(df[Date==update_date,],
                      measure.vars=colnames(df)[-1],
                      variable.name='type',
                      value.name='return',
                      variable.factor=FALSE)[order(type),]
  needed_data <- cbind(needed_data[type%like%'LW',.(Date,type,week_return=return)],
                       needed_data[type%like%'LM',.(month_return=return)])
  #calculate difference
  df[,`:=`(IF_DIFF=(IF01.CFE-IF)/IF,
           IC_DIFF=(IC01.CFE-IC)/IC,
           IH_DIFF=(IH01.CFE-IH)/IH)]
  #把两个结果整合
  result <- list()
  result$ret <- needed_data
  
  result$difference <- df[,.(Date,IF_DIFF,IC_DIFF,IH_DIFF)]
  
  return(result)
}

#P1:futures data----------------------------------------------------------------
df <- readr::read_csv(file.path(wind_path,futures_name))
df <- as.data.table(df)
setnames(df,'DATETIME','Date')
# df[,Date:=as.Date(DATETIME)]
# df[,DATETIME:=NULL]
df[,`:=`(IF=`000300.SH`,
         IC=`000905.SH`,
         IH=`000016.SH`,
         `000300.SH`=NULL,
         `000905.SH`=NULL,
         `000016.SH`=NULL)]#simple colname treatment,去掉第一列序号列
#2021-8-21改进：将读入的第一列序号列删除
data_list <- futures_ret(df,update_date)
ret <- data_list$ret

week_ret <- dcast.data.table(ret,Date~type,value.var='week_return')
week_ret[, c(paste0('IC0',c(0,1,2,3),'.CFE_RET_LW')) := purrr::map(.SD, function(x) {x - IC_RET_LW}),
         .SDcols = c(paste0('IC0',c(0,1,2,3),'.CFE_RET_LW'))]
week_ret[, c(paste0('IF0',c(0,1,2,3),'.CFE_RET_LW')) := purrr::map(.SD, function(x) {x - IF_RET_LW}),
         .SDcols = c(paste0('IF0',c(0,1,2,3),'.CFE_RET_LW'))]
week_ret[, c(paste0('IH0',c(0,1,2,3),'.CFE_RET_LW')) := purrr::map(.SD, function(x) {x - IH_RET_LW}),
         .SDcols = c(paste0('IH0',c(0,1,2,3),'.CFE_RET_LW'))]
week_excess <- melt(week_ret,id.vars='Date',value.name = 'week_excess')
setkey(week_excess,Date,variable)

month_ret <- dcast.data.table(ret,Date~type,value.var='month_return')
month_ret[, c(paste0('IC0',c(0,1,2,3),'.CFE_RET_LW')) := purrr::map(.SD, function(x) {x - IC_RET_LW}),
          .SDcols = c(paste0('IC0',c(0,1,2,3),'.CFE_RET_LW'))]
month_ret[, c(paste0('IF0',c(0,1,2,3),'.CFE_RET_LW')) := purrr::map(.SD, function(x) {x - IF_RET_LW}),
         .SDcols = c(paste0('IF0',c(0,1,2,3),'.CFE_RET_LW'))]
month_ret[, c(paste0('IH0',c(0,1,2,3),'.CFE_RET_LW')) := purrr::map(.SD, function(x) {x - IH_RET_LW}),
         .SDcols = c(paste0('IH0',c(0,1,2,3),'.CFE_RET_LW'))]
month_excess <- melt(month_ret,id.vars='Date',value.name = 'month_excess')
setkey(month_excess,Date,variable)

ret[,`:=`(WEEK_EXCESS=week_excess[J(ret$Date,ret$type)]$week_excess,
          MONTH_EXCESS=month_excess[J(ret$Date,ret$type)]$month_excess)]
ret <- ret[type%in%c(paste0('IC0',c(0,1,2,3),'.CFE_RET_LW'),
                     paste0('IF0',c(0,1,2,3),'.CFE_RET_LW'),
                     paste0('IH0',c(0,1,2,3),'.CFE_RET_LW')),]
fut_classes <- c('当月','次月','当季','下季')
ret[,type:=c(paste0('IC',fut_classes,'合约'),
             paste0('IF',fut_classes,'合约'),
             paste0('IH',fut_classes,'合约'))]
ret[,c('week_return','month_return',
       'WEEK_EXCESS','MONTH_EXCESS'):=lapply(.SD,function(x){GCAMCPUB::f_fmt_pct(x,2)}),
    .SDcols=c('week_return','month_return',
              'WEEK_EXCESS','MONTH_EXCESS')]
names(ret) <- c('日期','期货合约',
                '周度收益率','月度收益率'
                ,'相对现货周收益率','相对现货月收益率')

GCAMCPUB::saveDtRds(ret,file.path(output_path,'futures_ret.rds'))


#P2:期限差----------------------------------------------------------------------
spread <- readr::read_csv(file.path(wind_path,spread_name))
spread <- as.data.table(spread)

lasted_spread <- spread[DATE == update_date, .(DATE, code = substr(wind_code, 1, 2), wind_code, FUTURE_PRICE, INDEX_PRICE, REMAIN_DAYS, SPREAD)]
lasted_spread[REMAIN_DAYS < 20, SPREAD := NA]
total_lasted_spread <- lasted_spread[, .(wind_code = NA, FUTURE_PRICE = NA, INDEX_PRICE = NA,
                                         REMAIN_DAYS = NA, SPREAD = mean(SPREAD, na.rm = T)), by = .(DATE, code)]
lasted_spread <- rbind(lasted_spread, total_lasted_spread)
setorder(lasted_spread, code, wind_code, na.last = TRUE)

spread <- spread[REMAIN_DAYS >= 20]
spread[, FUTURE := substr(wind_code, 1, 2)]
mean_spread <- spread[, .(SPREAD = mean(SPREAD)), by = .(DATE, FUTURE)]
mean_spread <- dcast.data.table(mean_spread, DATE~FUTURE, value.var = "SPREAD")
#画图调整
mean_spread_plot <- melt(mean_spread,id.vars='DATE')
mean_spread_plot[,variable:=factor(variable,levels=c('IC','IF','IH'))]

GCAMCPUB::saveDtRds(mean_spread_plot,file.path(output_path,'futures_spread.rds'))

#P3:期权PCR数据-----------------------------------------------------------------
pcr <- readr::read_csv(file.path(wind_path,pcr_name))
pcr <- as.data.table(pcr)
position <- pcr[, .(daily_put_position = sum(daily_put_position) / 10000,
                    daily_call_position = sum(daily_call_position) / 10000), by = DATE]
pcr <- dcast.data.table(pcr, DATE~INDEX, value.var = "PCR")
pcr <- position[pcr, on = "DATE"]
setkey(pcr, DATE)

pcr_plot <- pcr[,.(DATE,`000300.SH`,`510050.SH`)]
pcr_plot <- melt(pcr_plot,id.vars='DATE')
pcr_plot[,variable:=factor(variable,
                           levels=c('000300.SH','510050.SH'),
                           labels=c('沪深300期权','上证50ETF期权'))]

GCAMCPUB::saveDtRds(pcr_plot,file.path(output_path,'pcr.rds'))


#2.商品主力期货合约统计-----------------------------------------------------------

commodity_ret_stat <- function(com_main){
  com_main <- melt(com_main,id.vars='DATETIME')
  com_main_w <- com_main[DATETIME<=update_date&DATETIME>=prev_week_date,]
  w_temp <- com_main_w[,.(WEEK_RET=last(value)/first(value)-1),by=.(variable)]
  com_main_m <- com_main[DATETIME<=update_date&DATETIME>=update_date-20,]
  m_temp <- com_main_m[,.(MONTH_RET=last(value)/first(value)-1),by=.(variable)]
  com_main_3m <- com_main[DATETIME<=update_date&DATETIME>=update_date-60,]
  m3_temp <- com_main_3m[,.(M3_RET=last(value)/first(value)-1),by=.(variable)]
  com_main_y <- com_main[DATETIME<=update_date&DATETIME>=prev_year_date,]
  y_temp <- com_main_y[,.(YEAR_RET=last(value)/first(value)-1),by=.(variable)]
  setkey(m_temp,variable)
  setkey(m3_temp,variable)
  setkey(y_temp,variable)
  
  w_temp[,`:=`(M_RET=m_temp[J(w_temp$variable)]$MONTH_RET,
                   M3_RET=m3_temp[J(w_temp$variable)]$M3_RET,
                   Y_RET=y_temp[J(w_temp$variable)]$YEAR_RET)]
  w_temp <- w_temp[order(-Y_RET),]
  w_temp[,c('WEEK_RET','M_RET','M3_RET','Y_RET'):=lapply(.SD,function(x){GCAMCPUB::f_fmt_pct(x,2)}),
                   .SDcols=c('WEEK_RET','M_RET','M3_RET','Y_RET')]
  names(w_temp) <- c('品种名称','本周收益率','过去一月收益率',
                     '过去三月收益率','过去一年收益率')
  w_temp
}

#贵金属类
gjs_main <- GCAMCPUB::readDtRds(file.path(wind_path,'gjs.rds'))
setkey(gjs_main,DATETIME)
gjs_stat <- commodity_ret_stat(gjs_main)

gjs_ref <- data.table(CODE=c("AU.SHF","AG.SHF"),NAME=c('沪金','沪银'))
setkey(gjs_ref,CODE)
gjs_stat[,品种名称:=gjs_ref[J(gjs_stat$品种名称)]$NAME]

#黑色系
hs_main <- GCAMCPUB::readDtRds(file.path(wind_path,'hs.rds'))
setkey(hs_main,DATETIME)
hs_stat <- commodity_ret_stat(hs_main)

hs_ref <- data.table(CODE=c("RB.SHF","I.DCE",
                            "SS.SHF","HC.SHF",
                            "SF.CZC","SM.CZC"),
                     NAME=c('螺纹钢','铁矿石',
                            '不锈钢','热轧卷板',
                            '硅铁','锰硅'))
setkey(hs_ref,CODE)
hs_stat[,品种名称:=hs_ref[J(hs_stat$品种名称)]$NAME]
#有色金属
jbjs_main <- GCAMCPUB::readDtRds(file.path(wind_path,'jbjs.rds'))
setkey(jbjs_main,DATETIME)
jbjs_stat <- commodity_ret_stat(jbjs_main)

jbjs_ref <- data.table(CODE=c("CU.SHF","AL.SHF",
                              "PB.SHF","ZN.SHF",
                              "NI.SHF","SN.SHF"),
                       NAME=c('沪铜','铝',
                              '铅','锌','镍','锡'))
setkey(jbjs_ref,CODE)
jbjs_stat[,品种名称:=jbjs_ref[J(jbjs_stat$品种名称)]$NAME]

#化工类
ny_main <- GCAMCPUB::readDtRds(file.path(wind_path,'ny.rds'))
setkey(ny_main,DATETIME)
ny_stat <- commodity_ret_stat(ny_main)

ny_ref <- data.table(CODE=c("ZC.CZC","PF.CZC","FU.SHF",
                            "PG.DCE","J.DCE","JM.DCE",
                            "RU.SHF","BU.SHF","SP.SHF",
                            "L.DCE","TA.CZC","EG.DCE",
                            "MA.CZC","PP.DCE","EB.DCE",
                            "UR.CZC","SA.CZC","V.DCE","FG.CZC"),
                       NAME=c('动力煤','短纤','燃油','LPG',
                              '焦炭','焦煤','橡胶','沥青','纸浆','塑料',
                              'PTA','乙二醇','甲醇','聚丙烯',
                              '苯乙烯','尿素','纯碱','PVC','玻璃'))
setkey(ny_ref,CODE)
ny_stat[,品种名称:=ny_ref[J(ny_stat$品种名称)]$NAME]
#农产品
ncp_main <- GCAMCPUB::readDtRds(file.path(wind_path,'ncp.rds'))
setkey(ncp_main,DATETIME)
ncp_stat <- commodity_ret_stat(ncp_main)

ncp_ref <- data.table(CODE=c("LH.DCE","PK.CZC","A.DCE",
                             "C.DCE","M.DCE","Y.DCE",
                             "OI.CZC","P.DCE","CF.CZC",
                             "SR.CZC","JD.DCE","RM.CZC",
                             "CS.DCE","AP.CZC","CJ.CZC"),
                     NAME=c('生猪','花生','豆一',
                            '玉米','豆粕','豆油',
                            '菜油','棕榈油','棉花',
                            '白糖','鸡蛋','菜粕',
                            '玉米淀粉','苹果','红枣'))
setkey(ncp_ref,CODE)
ncp_stat[,品种名称:=ncp_ref[J(ncp_stat$品种名称)]$NAME]

GCAMCPUB::saveDtRds(gjs_stat,file.path(output_path,'gjs_stat.rds'))
GCAMCPUB::saveDtRds(hs_stat,file.path(output_path,'hs_stat.rds'))
GCAMCPUB::saveDtRds(jbjs_stat,file.path(output_path,'jbjs_stat.rds'))
GCAMCPUB::saveDtRds(ny_stat,file.path(output_path,'ny_stat.rds'))
GCAMCPUB::saveDtRds(ncp_stat,file.path(output_path,'ncp_stat.rds'))

