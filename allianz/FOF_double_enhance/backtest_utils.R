######2021-9-28更新：代码规整化

#backtesk_utils-----------------------------------------------------------------
foresee = function(df_tmp,Type_tmp,para_table,obs_date,report_index){
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
  fac_tmp
}
sharpe = function(fund_list,MF_FundNetValueRe,obs_date){
  sharp_tmp<-sapply(fund_list,function(x){
    tmp <- MF_FundNetValueRe[INNERCODE==x,][order(TRADINGDAY)][TRADINGDAY<=obs_date
                                                               &TRADINGDAY>=obs_date-dyears(1),]
    tmp[,mean(NVRDAILYGROWTHRATE)/sd(NVRDAILYGROWTHRATE)]
  })
  sharpe_fac <- data.table(INNERCODE=fund_list,SHARPE=sharp_tmp)
  #去极值化
  fill_sharpe <- sharpe_fac[,mean(SHARPE)+3*sd(SHARPE)]
  sharpe_fac[SHARPE>fill_sharpe,SHARPE:=fill_sharpe]
  sharpe_fac
}
size = function(fac_tmp,Type_tmp,MF_MainFinancialIndexQ,obs_date,report_index){
  name_tmp <- merge(fac_tmp[,.(INNERCODE)],Type_tmp[,.(INNERCODE,NAME)],by="INNERCODE",all.x=TRUE)
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
  #去极值化
  fill_size <- size_fac[,mean(SIZE)+3*sd(SIZE)]
  size_fac[SIZE>fill_size,SIZE:=fill_size]
  size_fac[,NAME:=NULL]
  size_fac
}
attention = function(fac_tmp,Type_tmp,MF_MainFinancialIndexQ,MF_HolderInfo,obs_date,report_index){
  name_tmp <- merge(fac_tmp[,.(INNERCODE)],Type_tmp[,.(INNERCODE,NAME)],by="INNERCODE",all.x=TRUE)
  name_tmp <- name_tmp[,lapply(.SD,unique),.SDcols=c("INNERCODE","NAME")]
  full_list <- Type_tmp[NAME%in%name_tmp[,NAME],unique(INNERCODE)]#得到同名的不同类基金代码
  #得到全部的基金规模
  size_tmp <- MF_MainFinancialIndexQ[INFOPUBLDATE<=obs_date
                                     &INNERCODE%in%full_list
                                     &ENDDATE==date_list[report_index],]
  size_tmp <- size_tmp[,.(INNERCODE,NETASSETSVALUE)]
  size_tmp[,NAME:=sapply(INNERCODE,function(x){Type_tmp[INNERCODE==x,unique(NAME)]})]
  size_tmp[is.na(NETASSETSVALUE),NETASSETSVALUE:=0]#没有的规模就填成空值
  
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
  #去极值化
  fill_attention <- attention_fac[,mean(ATTENTION)+3*sd(ATTENTION)]
  attention_fac[ATTENTION>fill_attention,ATTENTION:=fill_attention]
  attention_fac[,NAME:=NULL]
  attention_fac
}





#backup------------------------------
# for (chg_date in 2:(length(change_date)-3)){
#   #A:半衰加权前瞻因子得分----------------------------------------------------
#   #过去3个报告期加权 1,1/2,1/4
#   obs_date <- as.POSIXct(change_date[chg_date])-period(8,units="hour")
#   df_tmp <- fund_pool(obs_date,pool_tmp)#找到给定日期对应的基础基金池
#   report_index <- which(date_list==obs_date)-para_table[m==month(obs_date),report]#找到相对应的基金报告期
#   
#   para_tmp <- unlist(para_table[m==month(obs_date),.(type,type1,type2)])
#   factor <- list()
#   for(i in 1:3){
#     if(para_tmp[i]=="key"){
#       tmp <- key_hold_factor(df_tmp,Type_tmp,obs_date,report_index-3*(i-1))
#       fill_num <- tmp[,mean(num_point)+3*sd(num_point)]
#       fill_weight <- tmp[,mean(weight_point)+3*sd(weight_point)]
#       tmp[num_point>fill_num,num_point:=fill_num]
#       tmp[weight_point>fill_weight,weight_point:=fill_weight]
#       factor[[i]] <- tmp
#     }
#     if(para_tmp[i]=="detail"){
#       tmp <- detail_hold_factor(df_tmp,Type_tmp,obs_date,report_index-3*(i-1))
#       fill_num <- tmp[,mean(num_point)+3*sd(num_point)]
#       fill_weight <- tmp[,mean(weight_point)+3*sd(weight_point)]
#       tmp[num_point>fill_num,num_point:=fill_num]
#       tmp[weight_point>fill_weight,weight_point:=fill_weight]
#       factor[[i]] <- tmp
#     }
#   }
#   fac_tmp=merge(factor[[1]],factor[[2]],by="INNERCODE",all.x=TRUE)
#   fac_tmp=merge(fac_tmp,factor[[3]],by="INNERCODE",all.x=TRUE)
#   fac_tmp[,`:=`(num_factor=(1*num_point.x+1/2*num_point.y+1/4*num_point)/(7/4),
#                 weight_factor=(1*weight_point.x+1/2*weight_point.y+1/4*weight_point)/(7/4))]
#   #fac_tmp[,.(INNERCODE,final_factor=0.5*(num_factor+weight_factor))][order(-final_factor)][1:30,]
#   fac_tmp[is.na(num_factor),num_factor:=num_point.x]
#   fac_tmp[is.na(weight_factor),weight_factor:=weight_point.x]
#   foresee_fact <- fac_tmp[,.(INNERCODE,FORESEE=0.5*(num_factor+weight_factor))]
#   #foresee_fact[order(-FORESEE)]
#   
#   fund_list <- fac_tmp[,INNERCODE]#基金代码
#   
#   #B:基金夏普因子---------------------------------------------------------------
#   
#   sharp_tmp<-sapply(fund_list,function(x){
#     tmp <- MF_FundNetValueRe[INNERCODE==x,][order(TRADINGDAY)][TRADINGDAY<=obs_date
#                                                                &TRADINGDAY>=obs_date-dyears(1),]
#     tmp[,mean(NVRDAILYGROWTHRATE)/sd(NVRDAILYGROWTHRATE)]
#   })
#   sharpe_fac <- data.table(INNERCODE=fund_list,SHARPE=sharp_tmp)
#   fill_sharpe <- sharpe_fac[,mean(SHARPE)+3*sd(SHARPE)]
#   sharpe_fac[SHARPE>fill_sharpe,SHARPE:=fill_sharpe]
#   
#   #C:基金规模因子----------------------------
#   #这里需要做一些处理，就是把同名基金的规模进行合并
#   name_tmp <- merge(fac_tmp[,.(INNERCODE)],Type_tmp[,.(INNERCODE,NAME)],by="INNERCODE",all.x=TRUE)
#   name_tmp <- name_tmp[,lapply(.SD,unique),.SDcols=c("INNERCODE","NAME")]
#   full_list <- Type_tmp[NAME%in%name_tmp[,NAME],unique(INNERCODE)]#得到同名的不同类基金代码
#   #得到全部的基金规模
#   size_tmp <- MF_MainFinancialIndexQ[INFOPUBLDATE<=obs_date
#                                      &INNERCODE%in%full_list
#                                      &ENDDATE==date_list[report_index],]
#   size_tmp <- size_tmp[,.(INNERCODE,NETASSETSVALUE)]
#   size_tmp[,NAME:=sapply(INNERCODE,function(x){Type_tmp[INNERCODE==x,unique(NAME)]})]
#   size_tmp[is.na(NETASSETSVALUE),NETASSETSVALUE:=0]#没有的规模就填成空值
#   sum_size <- size_tmp[,.(SIZE=sum(NETASSETSVALUE)),by=.(NAME)]
#   size_fac <- merge(name_tmp,sum_size,by="NAME",all.x=TRUE)
#   fill_size <- size_fac[,mean(SIZE)+3*sd(SIZE)]
#   size_fac[SIZE>fill_size,SIZE:=fill_size]
#   size_fac[,NAME:=NULL]
#   
#   #D:基金关注度因子-------------------------------------------------------------
#   attention_tmp <- MF_HolderInfo[INFOPUBLDATE<=obs_date
#                                  &INNERCODE%in%full_list
#                                  &difftime(date_list[report_index],ENDDATE,units="days")<366,]
#   attention_tmp <- attention_tmp[order(ENDDATE),.SD[.N],by=.(INNERCODE)]#最近一期的报告值
#   attention_tmp <- attention_tmp[,.(INNERCODE,INSTITUTIONHOLDRATIO)]
#   #填充一些缺失值,空值怎么处理？用中位数填充
#   mid_attention <- median(attention_tmp[,INSTITUTIONHOLDRATIO],na.rm=TRUE)
#   attention_tmp <- attention_tmp[is.na(INSTITUTIONHOLDRATIO),
#                                  INSTITUTIONHOLDRATIO:=mid_attention]
#   attention_tmp <- merge(attention_tmp,size_tmp,by="INNERCODE",all.x=TRUE)
#   sum_attention <- attention_tmp[,.(ATTENTION=sum(INSTITUTIONHOLDRATIO*NETASSETSVALUE)/sum(NETASSETSVALUE)),by=.(NAME)]
#   attention_fac <- merge(name_tmp,sum_attention,by='NAME',all.x=TRUE)
#   fill_attention <- attention_fac[,mean(ATTENTION)+3*sd(ATTENTION)]
#   attention_fac[ATTENTION>fill_attention,ATTENTION:=fill_attention]
#   attention_fac[,NAME:=NULL]
#   #E:基金经理隐藏交易能力-------------------------------------------------------
#   if(chg_date==2){
#     obs_date <- as.POSIXct(change_date[chg_date-1])-period(8,units="hour")
#     df_tmp <- fund_pool(obs_date,pool_tmp)#找到给定日期对应的基础基金池
#     report_index <- which(date_list==obs_date)-para_table[m==month(obs_date),report]#找到相对应的基金报告期
#     fac_old <- hide_factor(df_tmp,Type_tmp,obs_date,report_index)
#   }
#   obs_date <- as.POSIXct(change_date[chg_date])-period(8,units="hour")
#   df_tmp <- fund_pool(obs_date,pool_tmp)#找到给定日期对应的基础基金池
#   report_index <- which(date_list==obs_date)-para_table[m==month(obs_date),report]#找到相对应的基金报告期
#   fac_new <- hide_factor(df_tmp,Type_tmp,obs_date,report_index)
#   #有问题：2021-9-16：有些基金估计是匹配了港股和A股，所以出现重复
#   #2期因子半衰加权
#   hide_fac <- fac_old[fac_new,on=.(INNERCODE)]
#   colnames(hide_fac) <- c("INNERCODE","OLD","NEW")
#   hide_fac[,`:=`(HIDE=(OLD/2+NEW)*2/3)]
#   hide_fac[is.na(HIDE),HIDE:=NEW]
#   hide_fac[,`:=`(OLD=NULL,NEW=NULL)]
#   fac_old <- fac_new#这时将本期factor作为旧的factor方便后续迭代
#   #将因子排序
#   hide_fac <- hide_fac[!is.na(HIDE),]
#   fill_hide <- hide_fac[,mean(HIDE)+3*sd(HIDE)]
#   hide_fac[HIDE>fill_attention,HIDE:=fill_hide]
#   
#   #排序归一化，排序，因子组合得分-----------------------------------------------
#   final_fund_list <- final_score(fof_num)
#   # MF_FundNetValueRe[]
#   start <- trading_date[min(which(trading_date>change_date[chg_date]))]#本次持仓时间点
#   end <- trading_date[max(which(trading_date<=change_date[chg_date+1]))+1]#下一个调仓日
#   hold <- MF_FundNetValueRe[INNERCODE%in%final_fund_list[,INNERCODE]
#                             &TRADINGDAY>=start
#                             &TRADINGDAY<=end,
#                             .(INNERCODE,TRADINGDAY,UNITNVRESTORED)]
#   NAV = hold[,.(PNAV=UNITNVRESTORED/first(UNITNVRESTORED),TRADINGDAY),
#              by=.(INNERCODE)][,.(NAV=mean(PNAV)),by=.(TRADINGDAY)][order(TRADINGDAY),]
#   NAV[,ret:=NAV/shift(NAV)-1]
#   tmp <- NAV[!is.na(ret),.(TRADINGDAY,ret)]
#   simulate_result <- rbind(simulate_result,tmp)
# }