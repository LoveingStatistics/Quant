#0.更新日志---------------------------------------------------------------------
#2021-9-28更新:更新最后选基数量，灵活选取，应用重做的基金隐藏交易因子，删除小规模基金

#backtest_main
#回测使用选基因子:前瞻能力,夏普率,规模因子,关注度因子,隐藏交易能力
#调仓时间:3,6,9,12月月底
library(ggplot2)
source("main.R",encoding = "UTF-8")
source("utils_data_beta.R",encoding = "UTF-8")
source("hide_factor.R",encoding = "UTF-8")

trading_date <- unique(stock_quote[order(TRADINGDAY),TRADINGDAY])
change_index <- seq(which(date_list==as.POSIXct("2009-09-30")),length(date_list),3)
change_date <- as.Date(date_list[change_index])+period(1,units="day")

#1.优选基金组合回测-----------------------------------------------------------
simulate_result <- data.table()
fof_num <- 20

for (chg_date in 2:(length(change_date)-3)){
  obs_date <- as.POSIXct(change_date[chg_date])-period(8,units="hour")
  df_tmp <- fund_pool(obs_date,pool_tmp)#找到给定日期对应的基础基金池
  report_index <- which(date_list==obs_date)-para_table[m==month(obs_date),report]#找到相对应的基金报告期
  #A:半衰加权前瞻因子得分----------------------------------------------------
  #过去3个报告期加权 1,1/2,1/4
  fac_tmp <- foresee(df_tmp,Type_tmp,para_table,obs_date,report_index)
  foresee_fact <- fac_tmp[,.(INNERCODE,FORESEE=0.5*(num_factor+weight_factor))]
  fund_list <- fac_tmp[,INNERCODE]#基金代码
  #B:基金夏普因子---------------------------------------------------------------
  sharpe_fac <- sharpe(fund_list,MF_FundNetValueRe,obs_date)
  #C:基金规模因子----------------------------
  #这里需要做一些处理，就是把同名基金的规模进行合并
  size_fac <- size(fac_tmp,Type_tmp,MF_MainFinancialIndexQ,obs_date,report_index)
  #D:基金关注度因子-------------------------------------------------------------
  attention_fac <- attention(fac_tmp,Type_tmp,MF_MainFinancialIndexQ,MF_HolderInfo,obs_date,report_index)
  #E:基金经理隐藏交易能力-------------------------------------------------------
  if(chg_date==2){
    obs_date <- as.POSIXct(change_date[chg_date-1])-period(8,units="hour")
    df_tmp <- fund_pool(obs_date,pool_tmp)#找到给定日期对应的基础基金池
    report_index <- which(date_list==obs_date)-para_table[m==month(obs_date),report]#找到相对应的基金报告期
    fac_old <- hide_factor(df_tmp,Type_tmp,obs_date,report_index)
  }#有问题：2021-9-16：有些基金估计是匹配了港股和A股，所以出现重复
  fac_new <- hide_factor(df_tmp,Type_tmp,obs_date,report_index)
  #2期因子半衰加权
  hide_fac <- fac_old[fac_new,on=.(INNERCODE)]
  colnames(hide_fac) <- c("INNERCODE","OLD","NEW")
  hide_fac[,`:=`(HIDE=(OLD/2+NEW)*2/3)]
  hide_fac[is.na(HIDE),HIDE:=NEW]
  hide_fac[,`:=`(OLD=NULL,NEW=NULL)]
  fac_old <- fac_new#这时将本期factor作为旧的factor方便后续迭代
  #因子去极值化
  hide_fac <- hide_fac[!is.na(HIDE),]
  fill_hide <- hide_fac[,mean(HIDE)+3*sd(HIDE)]
  hide_fac[HIDE>fill_attention,HIDE:=fill_hide]

  #排序归一化，排序，因子组合得分-------------------------------------------------------
  final_fund_list <- final_score(fof_num)  # MF_FundNetValueRe[]
  start <- trading_date[min(which(trading_date>change_date[chg_date]))]#本次持仓时间点
  end <- trading_date[max(which(trading_date<=change_date[chg_date+1]))+1]#下一个调仓日
  hold <- MF_FundNetValueRe[INNERCODE%in%final_fund_list[,INNERCODE]
                      &TRADINGDAY>=start
                      &TRADINGDAY<=end,
                      .(INNERCODE,TRADINGDAY,UNITNVRESTORED)]
  NAV = hold[,.(PNAV=UNITNVRESTORED/first(UNITNVRESTORED),TRADINGDAY),
           by=.(INNERCODE)][,.(NAV=mean(PNAV)),by=.(TRADINGDAY)][order(TRADINGDAY),]
  NAV[,ret:=NAV/shift(NAV)-1]
  tmp <- NAV[!is.na(ret),.(TRADINGDAY,ret)]
  simulate_result <- rbind(simulate_result,tmp)
}

#3.回测结果展示---------------------------------------------------------
#回测期累计收益率
navs <- cumprod(simulate_result[,ret]+1)
simulate_result[,NAV:=navs]
base <- as.data.table(readr::read_csv("base.csv"))
base[,H11021.CSI:=H11021.CSI/100]
# plot(simulate_result[,.(TRADINGDAY,NAV)],type='l')
ggplot(aes(x=TRADINGDAY,y=NAV),data=simulate_result)+
  geom_line(color="blue")+
  geom_line(aes(x=as.POSIXct(Date),y=cumprod(H11021.CSI+1)),data=base)+
  theme_bw()
#分年度回测结果与基准对比
part_tmp <- simulate_result[TRADINGDAY<="2020-9-30",]
port_annual <- part_tmp[,.(RET=round(100*(last(NAV)/first(NAV)-1),2)),by=.(year(TRADINGDAY))]
base <- base[Date<=as.POSIXct("2020-09-30"),]
base_annual <- base[,.(BASE_RET=round(100*(last(cumprod(H11021.CSI+1))-1),2)),by=.(year(Date))]
port_annual[base_annual,on=.(year)]
#回测期内年化回报
tot_annual <- (simulate_result[TRADINGDAY=="2020-09-30",NAV])^(4/43)-1
print(paste0("2010-2020.9.30年化收益率为:",round(tot_annual*100,3),"%"))
base_tot_annual <- (last(cumprod(base[,H11021.CSI]+1)))^(4/43)-1
print(paste0("2010-2020.9.30年化收益率为:",round(base_tot_annual*100,3),"%"))



