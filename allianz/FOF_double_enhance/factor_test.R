###################################################
#-----本脚本测试因子的单调性等特性————————————————#
#Date:2021/9/3#
#2021-9-28重做了隐藏交易能力因子
###################################################

library(ggplot2)
source("main.R",encoding = "UTF-8")
source("utils_data_beta.R",encoding = "UTF-8")
source("hide_factor.R",encoding = "UTF-8")

size_sift <- function(fac_tmp,obs_date,report_index){
  # #:筛选出基金规模大于2亿的基金
  # #这里需要做一些处理，就是把同名基金的规模进行合并
  name_tmp <- Type_tmp[fac_tmp,on=.(INNERCODE=INNERCODE)][,.(INNERCODE,NAME,EFFECTIVEDATE,CANCELDATE)]
  name_tmp[is.na(CANCELDATE),CANCELDATE:=as.POSIXct("2021-8-25")]
  tmp <- name_tmp[,.(EFFECTIVEDATE=min(EFFECTIVEDATE),CANCELDATE=max(CANCELDATE)),by=.(INNERCODE)]
  setkey(Type_tmp,INNERCODE)
  tmp[,NAME:=unique(Type_tmp[J(tmp$INNERCODE)]$NAME)]
  
  full_list <- Type_tmp[NAME%in%tmp[,NAME],unique(INNERCODE)]
  #得到全部的基金规模
  size_tmp <- MF_MainFinancialIndexQ[INFOPUBLDATE<=obs_date
                                     &INNERCODE%in%full_list
                                     &ENDDATE==date_list[report_index],]
  size_tmp <- size_tmp[,.(INNERCODE,NETASSETSVALUE)]
  size_tmp[,NAME:=sapply(INNERCODE,function(x){Type_tmp[INNERCODE==x,unique(NAME)]})]
  sum_size <- size_tmp[,.(SIZE=sum(NETASSETSVALUE)),by=.(NAME)]
  
  size_fac <- sum_size[tmp,on=.(NAME=NAME)][,.(INNERCODE,SIZE)]
  return(size_fac[SIZE>=2*1e8&!is.na(SIZE)&SIZE<=100*1E8,INNERCODE])
}
grp_result <- function(start,end,foresee_fact){
  nav_tmp <- MF_FundNetValueRe[TRADINGDAY>=start
                               &TRADINGDAY<=end
                               &INNERCODE%in%foresee_fact[,INNERCODE],
                               .(INNERCODE,TRADINGDAY,UNITNVRESTORED)]
  setkey(foresee_fact,INNERCODE)
  nav_tmp[,RANK:=foresee_fact[J(nav_tmp$INNERCODE)]$RANK]
  
  c_result <- nav_tmp[,.(PNAV=UNITNVRESTORED/first(UNITNVRESTORED),TRADINGDAY),
                      by=.(RANK,INNERCODE)][,.(CNAV=mean(PNAV)),by=.(RANK,TRADINGDAY)][order(-RANK,TRADINGDAY),]
  group_result <- c_result[,.(RET=CNAV/shift(CNAV)-1,TRADINGDAY),by=.(RANK)][,.(M_RET=mean(RET,na.rm=TRUE)),by=.(RANK)][order(-RANK)]
  
  result_list <- list()
  result_list[[1]] <- c_result[,.(RET=CNAV/shift(CNAV)-1,TRADINGDAY),by=.(RANK)]
  result_list[[2]] <- group_result[,M_RET]
  
  return(result_list)
} #统计分组收益函数
grp_IC <- function(start,end,fac){
  if(!is.POSIXct(start)){
    start = as.POSIXct(start) - period(8,units="hour")
  }
  if(!is.POSIXct(end)){
    end = as.POSIXct(end) - period(8,units="hour")
  }
  nav_tmp <- MF_FundNetValueRe[(TRADINGDAY==start|TRADINGDAY==end)
                               &INNERCODE%in%foresee_fact[,INNERCODE],
                               .(INNERCODE,TRADINGDAY,UNITNVRESTORED)]
  fac[,RK:=frank(FORESEE)]
  setkey(fac,INNERCODE)
  
  setkey(nav_tmp,TRADINGDAY)
  ret_tmp <- nav_tmp[,.(RET=last(UNITNVRESTORED)/first(UNITNVRESTORED)-1),by=.(INNERCODE)]
  ret_tmp[,`:=`(fac_RK=fac[J(ret_tmp$INNERCODE)]$RK,
                ret_RK=frank(RET))]
  ret_tmp[,cor(fac_RK,ret_RK)]
}


trading_date <- unique(stock_quote[order(TRADINGDAY),TRADINGDAY])
change_index <- seq(which(date_list==as.POSIXct("2010-03-31")),length(date_list),3)
change_date <- as.Date(date_list[change_index])+period(1,units="day")
IC_date <- date_list[which(date_list==as.POSIXct("2010-01-31")):length(date_list)]

num_grp <- 5

#前瞻因子单调性测试--------------------------------------------
result <- c()#记录结果
result_series <- data.table()#记录收益率
IC_series <- c()

for (chg in 1:(length(change_date)-3)){
  obs_date <- as.POSIXct(change_date[chg])-period(8,units="hour")
  #A:半衰加权前瞻因子得分------------------
  #过去3个报告期加权 1,1/2,1/4
  
  df_tmp <- fund_pool(obs_date,pool_tmp)#找到给定日期对应的基础基金池
  report_index <- which(date_list==obs_date)-para_table[m==month(obs_date),report]#找到相对应的基金报告期
  
  para_tmp <- unlist(para_table[m==month(obs_date),.(type,type1,type2)])
  factor <- list()
  for(i in 1:3){
    if(para_tmp[i]=="key"){
      tmp <- key_hold_factor(df_tmp,Type_tmp,obs_date,report_index-3*(i-1))
      fill_num <- tmp[,mean(num_point)+3*sd(num_point)]
      fill_weight <- tmp[,mean(weight_point)+3*sd(weight_point)]
      tmp[num_point>fill_num,num_point:=fill_num]
      tmp[weight_point>fill_weight,weight_point:=fill_weight]
      factor[[i]] <- tmp
    }
    if(para_tmp[i]=="detail"){
      tmp <- detail_hold_factor(df_tmp,Type_tmp,obs_date,report_index-3*(i-1))
      fill_num <- tmp[,mean(num_point)+3*sd(num_point)]
      fill_weight <- tmp[,mean(weight_point)+3*sd(weight_point)]
      tmp[num_point>fill_num,num_point:=fill_num]
      tmp[weight_point>fill_weight,weight_point:=fill_weight]
      factor[[i]] <- tmp
    }
  }
  fac_tmp=merge(factor[[1]],factor[[2]],by="INNERCODE",all.x=TRUE)
  fac_tmp=merge(fac_tmp,factor[[3]],by="INNERCODE",all.x=TRUE)
  fac_tmp[,`:=`(num_factor=(1*num_point.x+1/2*num_point.y+1/4*num_point)/(7/4),
                weight_factor=(1*weight_point.x+1/2*weight_point.y+1/4*weight_point)/(7/4))]
  #fac_tmp[,.(INNERCODE,final_factor=0.5*(num_factor+weight_factor))][order(-final_factor)][1:30,]
  fac_tmp[is.na(num_factor),num_factor:=num_point.x]
  fac_tmp[is.na(weight_factor),weight_factor:=weight_point.x]
  
  # #去掉规模小于2亿或者没有规模数据的基金
  # size_limit <- size_sift(fac_tmp,obs_date,report_index)
  # fac_tmp <- fac_tmp[INNERCODE%in%size_limit,]
  #将两个前瞻因子因子进行等权组合
  foresee_fact <- fac_tmp[,.(INNERCODE,FORESEE=0.5*(num_factor+weight_factor))]
  foresee_fact[,RANK:=cut(FORESEE,
                          quantile(FORESEE,seq(0,1,1/num_grp)),
                          labels=FALSE,include.lowest=TRUE)]
  #foresee_fact[order(-FORESEE)]#第10组评分最高
  
  start <- trading_date[min(which(trading_date>change_date[chg]))]#本次持仓时间点
  end <- trading_date[max(which(trading_date<=change_date[chg+1]))+1]#下一个调仓日
  
  result_series <- rbind(result_series,grp_result(start,end,foresee_fact)[[1]])
  result <- rbind(result,grp_result(start,end,foresee_fact)[[2]])
  IC_series <- c(IC_series,grp_IC(start,end,foresee_fact))
  
}

#季均分组收益率
tmp <- data.table(RANK=c(num_grp:1),SCORE=apply(result,2,mean))
tmp[,RANK:=as.factor(RANK)]

ggplot(aes(x=RANK,y=SCORE),data=tmp)+
  geom_bar(stat="identity",fill='blue')+
  theme_bw()
#分组收益率曲线
result_series[is.na(RET),RET:=0]
NAV_tmp <- result_series[,.(NAV=cumprod(RET+1),TRADINGDAY),by=.(RANK)]
NAV_tmp[,RANK:=as.factor(RANK)]

ggplot(aes(x=TRADINGDAY,y=NAV,color=RANK),data=NAV_tmp)+
  geom_line()

#IC均值
for (chg in 1:(length(IC_date)-12)){
  obs_date <- IC_date[chg]
  #A:半衰加权前瞻因子得分------------------
  #过去3个报告期加权 1,1/2,1/4
  
  df_tmp <- fund_pool(obs_date,pool_tmp)#找到给定日期对应的基础基金池
  report_index <- which(date_list==obs_date)-para_table[m==month(obs_date),report]#找到相对应的基金报告期
  
  para_tmp <- unlist(para_table[m==month(obs_date),.(type,type1,type2)])
  factor <- list()
  for(i in 1:3){
    if(para_tmp[i]=="key"){
      tmp <- key_hold_factor(df_tmp,Type_tmp,obs_date,report_index-3*(i-1))
      fill_num <- tmp[,mean(num_point)+3*sd(num_point)]
      fill_weight <- tmp[,mean(weight_point)+3*sd(weight_point)]
      tmp[num_point>fill_num,num_point:=fill_num]
      tmp[weight_point>fill_weight,weight_point:=fill_weight]
      factor[[i]] <- tmp
    }
    if(para_tmp[i]=="detail"){
      tmp <- detail_hold_factor(df_tmp,Type_tmp,obs_date,report_index-3*(i-1))
      fill_num <- tmp[,mean(num_point)+3*sd(num_point)]
      fill_weight <- tmp[,mean(weight_point)+3*sd(weight_point)]
      tmp[num_point>fill_num,num_point:=fill_num]
      tmp[weight_point>fill_weight,weight_point:=fill_weight]
      factor[[i]] <- tmp
    }
  }
  fac_tmp=merge(factor[[1]],factor[[2]],by="INNERCODE",all.x=TRUE)
  fac_tmp=merge(fac_tmp,factor[[3]],by="INNERCODE",all.x=TRUE)
  fac_tmp[,`:=`(num_factor=(1*num_point.x+1/2*num_point.y+1/4*num_point)/(7/4),
                weight_factor=(1*weight_point.x+1/2*weight_point.y+1/4*weight_point)/(7/4))]
  #fac_tmp[,.(INNERCODE,final_factor=0.5*(num_factor+weight_factor))][order(-final_factor)][1:30,]
  fac_tmp[is.na(num_factor),num_factor:=num_point.x]
  fac_tmp[is.na(weight_factor),weight_factor:=weight_point.x]
  
  # #去掉规模小于2亿或者没有规模数据的基金
  # size_limit <- size_sift(fac_tmp,obs_date,report_index)
  # fac_tmp <- fac_tmp[INNERCODE%in%size_limit,]
  #将两个前瞻因子因子进行等权组合
  foresee_fact <- fac_tmp[,.(INNERCODE,FORESEE=0.5*(num_factor+weight_factor))]
  foresee_fact[,RANK:=cut(FORESEE,
                          quantile(FORESEE,seq(0,1,1/num_grp)),
                          labels=FALSE,include.lowest=TRUE)]
  #foresee_fact[order(-FORESEE)]#第10组评分最高
  
  start <- trading_date[min(which(trading_date>IC_date[chg]))]#本次持仓时间点
  end <- trading_date[max(which(trading_date<=IC_date[chg+3]))+1]#下一个调仓日

  result <- rbind(result,grp_result(start,end,foresee_fact)[[2]])
  IC_series <- c(IC_series,grp_IC(start,end,foresee_fact))
}
mean(IC_series)
sd(IC_series)
mean(IC_series)/sd(IC_series)

#盈利因子单调性测试--------------------------------------------
result <- c()#记录结果
result_series <- data.table()#记录收益率

for (chg in 1:(length(change_date)-3)){
  obs_date <- as.POSIXct(change_date[chg])-period(8,units="hour")
  
  df_tmp <- fund_pool(obs_date,pool_tmp)#找到给定日期对应的基础基金池
  report_index <- which(date_list==obs_date)-para_table[m==month(obs_date),report]#找到相对应的基金报告期
  
  fund_list <- df_tmp[,INNERCODE]
  sharp_tmp<-sapply(fund_list,function(x){
    tmp <- MF_FundNetValueRe[INNERCODE==x,][order(TRADINGDAY)][TRADINGDAY<=obs_date
                                                               &TRADINGDAY>=obs_date-dyears(1),]
    tmp[,mean(NVRDAILYGROWTHRATE)/sd(NVRDAILYGROWTHRATE)]
  })
  sharpe_fac <- data.table(INNERCODE=fund_list,SHARPE=sharp_tmp)
  
  # #去掉规模小于2亿或者没有规模数据的基金
  # size_limit <- size_sift(fac_tmp,obs_date,report_index)
  # fac_tmp <- fac_tmp[INNERCODE%in%size_limit,]
  #将因子排序
  sharpe_fac <- sharpe_fac[!is.na(SHARPE),]
  sharpe_fac[,RANK:=cut(SHARPE,
                          quantile(SHARPE,seq(0,1,1/num_grp)),
                          labels=FALSE,include.lowest=TRUE)]
  #foresee_fact[order(-FORESEE)]#第10组评分最高
  
  start <- trading_date[min(which(trading_date>change_date[chg]))]#本次持仓时间点
  end <- trading_date[max(which(trading_date<=change_date[chg+1]))+1]#下一个调仓日
  
  result_series <- rbind(result_series,grp_result(start,end,sharpe_fac)[[1]])
  result <- rbind(result,grp_result(start,end,sharpe_fac)[[2]])
  
}

tmp <- data.table(RANK=c(num_grp:1),SCORE=apply(result,2,mean))
tmp[,RANK:=as.factor(RANK)]
#季均分组收益率
ggplot(aes(x=RANK,y=SCORE),data=tmp)+
  geom_bar(stat="identity",fill='blue')+
  theme_bw()
#分组收益率曲线
result_series[is.na(RET),RET:=0]
NAV_tmp <- result_series[,.(NAV=cumprod(RET+1),TRADINGDAY),by=.(RANK)]
NAV_tmp[,RANK:=as.factor(RANK)]

ggplot(aes(x=TRADINGDAY,y=NAV,color=RANK),data=NAV_tmp)+
  geom_line()

#sharpe因子区分度并不高


#规模因子单调性测试--------------------------------------------
result <- c()#记录结果
result_series <- data.table()#记录收益率

for (chg in 1:(length(change_date)-3)){
  obs_date <- as.POSIXct(change_date[chg])-period(8,units="hour")
  
  df_tmp <- fund_pool(obs_date,pool_tmp)#找到给定日期对应的基础基金池
  report_index <- which(date_list==obs_date)-para_table[m==month(obs_date),report]#找到相对应的基金报告期
  
  fund_list <- df_tmp[,INNERCODE]
  name_tmp <- merge(df_tmp[,.(INNERCODE)],Type_tmp[,.(INNERCODE,NAME)],by="INNERCODE",all.x=TRUE)
  name_tmp <- name_tmp[,lapply(.SD,unique),.SDcols=c("INNERCODE","NAME")]
  full_list <- Type_tmp[NAME%in%name_tmp[,NAME],unique(INNERCODE)]#得到同名的不同类基金代码
  #得到全部的基金规模
  size_tmp <- MF_MainFinancialIndexQ[INFOPUBLDATE<=obs_date
                                     &INNERCODE%in%full_list
                                     &ENDDATE==date_list[report_index],]
  size_tmp <- size_tmp[,.(INNERCODE,NETASSETSVALUE)]
  size_tmp[,NAME:=sapply(INNERCODE,function(x){Type_tmp[INNERCODE==x,unique(NAME)]})]
  size_tmp[is.na(NETASSETSVALUE),NETASSETSVALUE:=0]#没有的规模就填成空值
  sum_size <- size_tmp[,.(SIZE=sum(NETASSETSVALUE)),by=.(NAME)]
  size_fac <- merge(name_tmp,sum_size,by="NAME",all.x=TRUE)
  
  size_fac[,NAME:=NULL]
  
  
  #将因子排序
  size_fac <- size_fac[!is.na(SIZE),]
  size_fac[,RANK:=cut(SIZE,
                        quantile(SIZE,seq(0,1,1/num_grp)),
                        labels=FALSE,include.lowest=TRUE)]
  #foresee_fact[order(-FORESEE)]#第10组评分最高
  
  start <- trading_date[min(which(trading_date>change_date[chg]))]#本次持仓时间点
  end <- trading_date[max(which(trading_date<=change_date[chg+1]))+1]#下一个调仓日
  
  result_series <- rbind(result_series,grp_result(start,end,size_fac)[[1]])
  result <- rbind(result,grp_result(start,end,size_fac)[[2]])
  
}

tmp <- data.table(RANK=c(num_grp:1),SCORE=apply(result,2,mean))
tmp[,RANK:=as.factor(RANK)]
#季均分组收益率
ggplot(aes(x=RANK,y=SCORE),data=tmp)+
  geom_bar(stat="identity",fill='blue')+
  theme_bw()
#分组收益率曲线
result_series[is.na(RET),RET:=0]
NAV_tmp <- result_series[,.(NAV=cumprod(RET+1),TRADINGDAY),by=.(RANK)]
NAV_tmp[,RANK:=as.factor(RANK)]

ggplot(aes(x=TRADINGDAY,y=NAV,color=RANK),data=NAV_tmp)+
  geom_line()
#确实呈现大规模悖论

#关注度因子单调性测试--------------------------------------------
result <- c()#记录结果
result_series <- data.table()#记录收益率

for (chg in 1:(length(change_date)-3)){
  obs_date <- as.POSIXct(change_date[chg])-period(8,units="hour")
  
  df_tmp <- fund_pool(obs_date,pool_tmp)#找到给定日期对应的基础基金池
  report_index <- which(date_list==obs_date)-para_table[m==month(obs_date),report]#找到相对应的基金报告期
  
  fund_list <- df_tmp[,INNERCODE]
  name_tmp <- merge(df_tmp[,.(INNERCODE)],Type_tmp[,.(INNERCODE,NAME)],by="INNERCODE",all.x=TRUE)
  name_tmp <- name_tmp[,lapply(.SD,unique),.SDcols=c("INNERCODE","NAME")]
  full_list <- Type_tmp[NAME%in%name_tmp[,NAME],unique(INNERCODE)]#得到同名的不同类基金代码
  
  attention_tmp <- MF_HolderInfo[INFOPUBLDATE<=obs_date
                                 &INNERCODE%in%full_list
                                 &difftime(date_list[report_index],ENDDATE,units="days")<366,]
  attention_tmp <- attention_tmp[order(ENDDATE),.SD[.N],by=.(INNERCODE)]#最近一期的报告值
  attention_tmp <- attention_tmp[,.(INNERCODE,INSTITUTIONHOLDRATIO)]
  #填充一些缺失值,空值怎么处理？用中位数填充
  mid_attention <- median(attention_tmp[,INSTITUTIONHOLDRATIO],na.rm=TRUE)
  attention_tmp <- attention_tmp[is.na(INSTITUTIONHOLDRATIO),
                                 INSTITUTIONHOLDRATIO:=mid_attention]
  attention_tmp <- merge(attention_tmp,size_tmp,by="INNERCODE",all.x=TRUE)
  sum_attention <- attention_tmp[,.(ATTENTION=sum(INSTITUTIONHOLDRATIO*NETASSETSVALUE)/sum(NETASSETSVALUE)),by=.(NAME)]
  attention_fac <- merge(name_tmp,sum_attention,by='NAME',all.x=TRUE)

  attention_fac[,NAME:=NULL]
  
  #将因子排序
  attention_fac <- attention_fac[!is.na(ATTENTION),]
  attention_fac[,RANK:=cut(ATTENTION,
                      quantile(ATTENTION,seq(0,1,1/num_grp)),
                      labels=FALSE,include.lowest=TRUE)]
  #foresee_fact[order(-FORESEE)]#第10组评分最高
  
  start <- trading_date[min(which(trading_date>change_date[chg]))]#本次持仓时间点
  end <- trading_date[max(which(trading_date<=change_date[chg+1]))+1]#下一个调仓日
  
  result_series <- rbind(result_series,grp_result(start,end,attention_fac)[[1]])
  result <- rbind(result,grp_result(start,end,attention_fac)[[2]])
  
}

tmp <- data.table(RANK=c(num_grp:1),SCORE=apply(result,2,mean))
tmp[,RANK:=as.factor(RANK)]
#季均分组收益率
ggplot(aes(x=RANK,y=SCORE),data=tmp)+
  geom_bar(stat="identity",fill='blue')+
  theme_bw()
#分组收益率曲线
result_series[is.na(RET),RET:=0]
NAV_tmp <- result_series[,.(NAV=cumprod(RET+1),TRADINGDAY),by=.(RANK)]
NAV_tmp[,RANK:=as.factor(RANK)]

ggplot(aes(x=TRADINGDAY,y=NAV,color=RANK),data=NAV_tmp)+
  geom_line()

#隐藏交易因子单调性测试--------------------------------------------
result <- c()#记录结果
result_series <- data.table()#记录收益率
ttttt<-function(){
  # for (chg in 1:(length(change_date)-3)){
  #   obs_date <- as.POSIXct(change_date[chg])-period(8,units="hour")
  #   
  #   df_tmp <- fund_pool(obs_date,pool_tmp)#找到给定日期对应的基础基金池
  #   report_index <- which(date_list==obs_date)-para_table[m==month(obs_date),report]#找到相对应的基金报告期
  #   
  #   fund_list <- df_tmp[,INNERCODE]
  #   hide_fac <- key_trade_factor(df_tmp,Type_tmp,obs_date,report_index)
  #   #有问题：2021-9-16：有些基金估计是匹配了港股和A股，所以出现重复
  #   
  #   #将因子排序
  #   hide_fac <- hide_fac[!is.na(HIDE),]
  #   hide_fac[,RANK:=cut(HIDE,
  #                            quantile(HIDE,seq(0,1,1/num_grp)),
  #                            labels=FALSE,include.lowest=TRUE)]
  #   #foresee_fact[order(-FORESEE)]#第10组评分最高
  #   
  #   start <- trading_date[min(which(trading_date>change_date[chg]))]#本次持仓时间点
  #   end <- trading_date[max(which(trading_date<=change_date[chg+1]))+1]#下一个调仓日
  #   
  #   result_series <- rbind(result_series,grp_result(start,end,hide_fac)[[1]])
  #   result <- rbind(result,grp_result(start,end,hide_fac)[[2]])
  #   
  # }
}#旧的隐藏交易测试，测出来因子反向，说明可能存在构建问题

for (chg in 2:(length(change_date)-3)){
  if(chg==2){
    obs_date <- as.POSIXct(change_date[chg-1])-period(8,units="hour")
    df_tmp <- fund_pool(obs_date,pool_tmp)#找到给定日期对应的基础基金池
    report_index <- which(date_list==obs_date)-para_table[m==month(obs_date),report]#找到相对应的基金报告期
    fac_old <- hide_factor(df_tmp,Type_tmp,obs_date,report_index)
  }
  obs_date <- as.POSIXct(change_date[chg])-period(8,units="hour")
  df_tmp <- fund_pool(obs_date,pool_tmp)#找到给定日期对应的基础基金池
  report_index <- which(date_list==obs_date)-para_table[m==month(obs_date),report]#找到相对应的基金报告期
  fac_new <- hide_factor(df_tmp,Type_tmp,obs_date,report_index)
  #有问题：2021-9-16：有些基金估计是匹配了港股和A股，所以出现重复
  #2期因子半衰加权
  hide_fac <- fac_old[fac_new,on=.(INNERCODE)]
  colnames(hide_fac) <- c("INNERCODE","OLD","NEW")
  hide_fac[,`:=`(HIDE=(OLD/2+NEW)*2/3)]
  hide_fac[is.na(HIDE),HIDE:=NEW]
  hide_fac[,`:=`(OLD=NULL,NEW=NULL)]
  fac_old <- fac_new#这时将本期factor作为旧的factor方便后续迭代
  #将因子排序
  hide_fac <- hide_fac[!is.na(HIDE),]
  hide_fac[,RANK:=cut(HIDE,
                      quantile(HIDE,seq(0,1,1/num_grp)),
                      labels=FALSE,include.lowest=TRUE)]
  #foresee_fact[order(-FORESEE)]#第10组评分最高
  
  start <- trading_date[min(which(trading_date>change_date[chg]))]#本次持仓时间点
  end <- trading_date[max(which(trading_date<=change_date[chg+1]))+1]#下一个调仓日
  
  result_series <- rbind(result_series,grp_result(start,end,hide_fac)[[1]])
  result <- rbind(result,grp_result(start,end,hide_fac)[[2]])
  
}

tmp <- data.table(RANK=c(num_grp:1),SCORE=apply(result,2,mean))
tmp[,RANK:=as.factor(RANK)]
#季均分组收益率
ggplot(aes(x=RANK,y=SCORE),data=tmp)+
  geom_bar(stat="identity",fill='blue')+
  theme_bw()
#分组收益率曲线
result_series[is.na(RET),RET:=0]
NAV_tmp <- result_series[,.(NAV=cumprod(RET+1),TRADINGDAY),by=.(RANK)]
NAV_tmp[,RANK:=as.factor(RANK)]

ggplot(aes(x=TRADINGDAY,y=NAV,color=RANK),data=NAV_tmp)+
  geom_line()









