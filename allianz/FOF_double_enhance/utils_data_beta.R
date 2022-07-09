
#0.更新日志--------------------------------------------------------
#2021-09-17 beta版clean
#function used to calculated factors realted to public funds
#2021-9-27更新
#fund_Pool 不应该进行canceldate筛选
#2021-9-28更新
#fund_pool存在类型切换时，进行去重处理
#2021-10-28
#删除规模过小的基金 再进行回测

#1.给定观察日对应的基金池-----------------------------------------
#后续每个时点选择基础基金池时可以利用分类的,后续改编为函数，获取给定时间的基础股票池
#difftime>15months(65weeks) 并且canceldate大于给定日期进行筛选
fund_pool = function(obs_date,pool_tmp){
  df_tmp <- pool_tmp[difftime(obs_date,EFFECTIVEDATE,units='weeks')>65,]#65周对应15个月
  # &difftime(CANCELDATE,obs_date,units="day")>93
  df_tmp[,`:=`(CLASS=NULL,LISTEDDATE=NULL,EXPIREDATE=NULL)]#(980)
  setkey(df_tmp,INNERCODE,EFFECTIVEDATE)
  df_tmp <- df_tmp[,.(SECURITYCODE=first(SECURITYCODE),CHINAMEABBR=first(CHINAMEABBR),
                      EFFECTIVEDATE=min(EFFECTIVEDATE),CANCELDATE=max(CANCELDATE),
                      SECASSETCATNAME=first(SECASSETCATNAME),NAME=first(NAME)),by=.(INNERCODE)]
    return(df_tmp)
}

#2.基金重仓股票匹配---------------------------------------------------------
key_stock_pool = function(df_tmp,Type_tmp,obs_date,report_index){
  #基金和仓位的匹配
  key_tmp <- MF_KeyStockPortfolio[INNERCODE%in%unlist(df_tmp$INNERCODE),]
  #可能存在少数基金匹配遗失问题，有些定期报告只收录C类基金
  main_info <- df_tmp[key_tmp,on=.(INNERCODE)]
  # merge(key_tmp,df_tmp,by="INNERCODE",all.x=TRUE)
  #进行重新匹配寻找
  lost_code <- setdiff(unique(df_tmp$INNERCODE),unique(key_tmp$INNERCODE))
  if (length(lost_code)!=0){
    lost_part <- df_tmp[INNERCODE%in%lost_code,]
    
    l <- lapply(lost_part[,NAME],function(x){
      Type_tmp[NAME==x&CLASS!='A',.(INNERCODE,NAME)]
    })#找出可能匹配到的代码（同名的非A基金）
    name_list <- colnames(MF_KeyStockPortfolio)
    sup_part <- lapply(l,function(x){
      MF_KeyStockPortfolio[INNERCODE%in%unlist(x[,INNERCODE]),
                           .(.SD,NAME=unique(x[,NAME])),
                           .SDcols=name_list]#拿到需要的数据
    })
    sup_info <- data.table()
    for(i in 1:length(sup_part)){
      sup_info <- rbind(sup_info,sup_part[[i]])
    }
    colnames(sup_info) <- c(name_list,'NAME')
    sup_info[,INNERCODE:=NULL]
    sup_info <- merge(sup_info,df_tmp,by="NAME",all.x=TRUE)
    full_info <- rbind(main_info[,.(INNERCODE,REPORTDATE,STOCKINNERCODE,
                                    RATIOINNV,INFOPUBLDATE,CHINAMEABBR)],
                       sup_info[,.(INNERCODE,REPORTDATE,STOCKINNERCODE,
                                   RATIOINNV,INFOPUBLDATE,CHINAMEABBR)])
  } else{
    print("No need to supplement more infomation")
    full_info <- main_info[,.(INNERCODE,REPORTDATE,STOCKINNERCODE,
                              RATIOINNV,INFOPUBLDATE,CHINAMEABBR)]
  }#如果没有缺失的匹配基金，那就直接返回，否则进行拼接
  
  #获取应该参照的基金持仓报告
  
  # reference <- full_info[REPORTDATE==(obs_date%m+%months(-1))&
  #                          INFOPUBLDATE<=obs_date,]
  reference <- full_info[REPORTDATE==date_list[report_index]&
                           INFOPUBLDATE<=obs_date,]#对应的报告期以及信息发布日期规则
  reference <- merge(reference,SecuMain[,.(INNERCODE,SECUCODE,SECUABBR)],
                     by.x="STOCKINNERCODE",by.y="INNERCODE",all.x=TRUE)
  reference <- reference[!is.na(SECUCODE),]
  
  reference
}

detail_stock_pool = function(df_tmp,Type_tmp,obs_date,report_index){
  #基金和仓位的匹配
  key_tmp <- MF_StockPortfolioDetail[INNERCODE%in%unlist(df_tmp$INNERCODE),]
  #可能存在少数基金匹配遗失问题，有些定期报告只收录C类基金
  main_info <- merge(key_tmp,df_tmp,by="INNERCODE",all.x=TRUE)
  #进行重新匹配寻找
  lost_code <- setdiff(unique(df_tmp$INNERCODE),unique(key_tmp$INNERCODE))
  if (length(lost_code)!=0){
    lost_part <- df_tmp[INNERCODE%in%lost_code,]
    
    l <- lapply(lost_part[,NAME],function(x){
      Type_tmp[NAME==x&CLASS!='A',.(INNERCODE,NAME)]
    })#找出可能匹配到的代码（同名的非A基金）
    name_list <- colnames(MF_StockPortfolioDetail)
    sup_part <- lapply(l,function(x){
      MF_StockPortfolioDetail[INNERCODE%in%unlist(x[,INNERCODE]),
                              .(.SD,NAME=unique(x[,NAME])),
                              .SDcols=name_list]#拿到需要的数据
    })
    sup_info <- data.table()
    for(i in 1:length(sup_part)){
      sup_info <- rbind(sup_info,sup_part[[i]])
    }
    colnames(sup_info) <- c(name_list,'NAME')
    sup_info[,INNERCODE:=NULL]
    sup_info <- merge(sup_info,df_tmp,by="NAME",all.x=TRUE)
    full_info <- rbind(main_info[,.(INNERCODE,REPORTDATE,STOCKINNERCODE,
                                    RATIOINNV,INFOPUBLDATE,CHINAMEABBR)],
                       sup_info[,.(INNERCODE,REPORTDATE,STOCKINNERCODE,
                                   RATIOINNV,INFOPUBLDATE,CHINAMEABBR)])
  } else{
    print("No need to supplement more infomation")
    full_info <- main_info[,.(INNERCODE,REPORTDATE,STOCKINNERCODE,
                              RATIOINNV,INFOPUBLDATE,CHINAMEABBR)]
  }#如果没有缺失的匹配基金，那就直接返回，否则进行拼接
  
  
  #获取应该参照的基金持仓报告
  reference <- full_info[REPORTDATE==date_list[report_index]&
                           INFOPUBLDATE<=obs_date,]
  reference <- merge(reference,SecuMain[,.(INNERCODE,SECUCODE,SECUABBR)],
                     by.x="STOCKINNERCODE",by.y="INNERCODE",all.x=TRUE)
  reference <- reference[!is.na(SECUCODE),]
  
  reference
}

#3.前瞻能力----------------------------------------------------------
#3a.重仓股埋伏名单
key_hold_factor = function(df_tmp,Type_tmp,obs_date,report_index){
  
  reference <- key_stock_pool(df_tmp,Type_tmp,obs_date,report_index)
  
  #提取发布相应报告期业绩报告和快报的公司名单,以及有分析师点评的超预期公司
  ref_year <- unique(year(reference[,REPORTDATE]))
  ref_period <- unique(quarter(reference[,REPORTDATE]))
  ref_date <- unique(reference[,REPORTDATE])
  ref_express <- fin_performance_express[REPORT_YEAR==ref_year&
                                           REPORT_PERIOD==ref_period&
                                           DECLARE_DATE<=obs_date&
                                           DECLARE_DATE>=ref_date,.(STOCK_CODE,DECLARE_DATE)]
  ref_forecast <- fin_performance_forecast[REPORT_YEAR==ref_year&
                                             REPORT_PERIOD==ref_period&
                                             DECLARE_DATE<=obs_date&
                                             DECLARE_DATE>=ref_date,.(STOCK_CODE,DECLARE_DATE)]
  ref_analyst <- der_prob_excess_stock[REPORT_YEAR==ref_year&
                                         REPORT_QUARTER==ref_period&
                                         DECLARE_DATE<=obs_date&
                                         DECLARE_DATE>=ref_date,.(STOCK_CODE,DECLARE_DATE)]
  
  
  ref_express_tmp <-merge(ref_express,ref_analyst,by="STOCK_CODE",all.x=TRUE)
  ref_express_tmp[,DIFF:=difftime(DECLARE_DATE.y,DECLARE_DATE.x,units="days")]
  exp <- ref_express_tmp[!is.na(DIFF)&DIFF<=5,.(STOCK_CODE)]#得到业绩快报的超预期公司代码
  
  ref_forecast_tmp <- merge(ref_forecast,ref_analyst,by="STOCK_CODE",all.x=TRUE)
  ref_forecast_tmp[,DIFF:=difftime(DECLARE_DATE.y,DECLARE_DATE.x,units="days")]
  fore <- ref_forecast_tmp[!is.na(DIFF)&DIFF<=5,.(STOCK_CODE)]#得到业绩快报的超预期公司代码
  
  #funion(exp,fore)
  reference[,GOOD:=SECUCODE%in%unlist(funion(exp,fore))]
  
  forward<- reference[!is.na(SECUCODE),.(num_point=sum(GOOD)/.N,
                         weight_point=sum(GOOD*RATIOINNV,na.rm=T)),
                      by=.(INNERCODE)]
  forward[is.na(num_point),num_point:=0]
  forward[is.na(weight_point),num_point:=0]
  #forward[,sum(num_point==0)/nrow(forward)]

}

#3b.全部持仓埋伏名单
detail_hold_factor = function(df_tmp,Type_tmp,obs_date,report_index){
  
  reference <- detail_stock_pool(df_tmp,Type_tmp,obs_date,report_index)
  
  ref_year <- unique(year(reference[,REPORTDATE]))
  ref_period <- unique(quarter(reference[,REPORTDATE]))
  ref_date <- unique(reference[,REPORTDATE])
  ref_express <- fin_performance_express[REPORT_YEAR==ref_year&
                                           REPORT_PERIOD==ref_period&
                                           DECLARE_DATE<=obs_date&
                                           DECLARE_DATE>=ref_date,.(STOCK_CODE,DECLARE_DATE)]
  ref_forecast <- fin_performance_forecast[REPORT_YEAR==ref_year&
                                             REPORT_PERIOD==ref_period&
                                             DECLARE_DATE<=obs_date&
                                             DECLARE_DATE>=ref_date,.(STOCK_CODE,DECLARE_DATE)]
  ref_analyst <- der_prob_excess_stock[REPORT_YEAR==ref_year&
                                         REPORT_QUARTER==ref_period&
                                         DECLARE_DATE<=obs_date&
                                         DECLARE_DATE>=ref_date,.(STOCK_CODE,DECLARE_DATE)]
  
  
  ref_express_tmp <-merge(ref_express,ref_analyst,by="STOCK_CODE",all.x=TRUE)
  ref_express_tmp[,DIFF:=difftime(DECLARE_DATE.y,DECLARE_DATE.x,units="days")]
  exp <- ref_express_tmp[!is.na(DIFF)&DIFF<=5,.(STOCK_CODE)]#得到业绩快报的超预期公司代码
  
  ref_forecast_tmp <- merge(ref_forecast,ref_analyst,by="STOCK_CODE",all.x=TRUE)
  ref_forecast_tmp[,DIFF:=difftime(DECLARE_DATE.y,DECLARE_DATE.x,units="days")]
  fore <- ref_forecast_tmp[!is.na(DIFF)&DIFF<=5,.(STOCK_CODE)]#得到业绩快报的超预期公司代码
  
  #计算当期的前瞻能力得分
  reference[,GOOD:=SECUCODE%in%unlist(funion(exp,fore))]
  reference[,GOOD:=as.numeric(GOOD)]
  
  forward <- reference[!is.na(SECUCODE),.(num_point=sum(GOOD)/.N,
                         weight_point=sum(GOOD*RATIOINNV,na.rm=T)),
                      by=.(INNERCODE)]
  #0值比率
  # nrow(forward[num_point==0,])/nrow(forward)
  
}

#4.基金隐藏交易-------------------------------------------------------

#定义隐藏交易因子计算函数
hide_function <- function(code){
  tmp <- reference[INNERCODE==code,]
  tmp_list <- unique(tmp[,STOCKINNERCODE])
  stock_tmp <- stock_quote[INNERCODE%in%tmp_list,.(INNERCODE,
                                                   TRADINGDAY,
                                                   CLOSEPRICE)]
  #stock_tmp[,DATE:=as.POSIXct(TRADINGDAY)-period(8,units="hour")]
  stock_tmp <- stock_tmp[,DATE:=as.POSIXct(TRADINGDAY)][DATE>(date_list[report_index]+period(8,units="hour"))
                                                        &DATE<=(obs_date+period(8,units="hour")),]
  
  tmp <- tmp[,.(RATIOINNV=sum(RATIOINNV)),by=.(STOCKINNERCODE)]
  setkey(tmp,STOCKINNERCODE)
  stock_tmp[,RATIOINNV:=tmp[J(stock_tmp$INNERCODE)]$RATIOINNV]
  hold_tmp <- stock_tmp
  # hold_tmp <- merge(stock_tmp,tmp[,.(STOCKINNERCODE,RATIOINNV)],
  #                   by.x="INNERCODE",by.y="STOCKINNERCODE",all.x=TRUE)
  hold_tmp <- hold_tmp[,.(NAV=sum(CLOSEPRICE*RATIOINNV)/sum(RATIOINNV)),by=.(DATE)]
  hold_ret <- hold_tmp[,mean(NAV/shift(NAV)-1,na.rm=TRUE)]
  fund_ret <- MF_FundNetValueRe[INNERCODE==code
                                &TRADINGDAY>date_list[report_index]
                                &TRADINGDAY<=obs_date,mean(NVRDAILYGROWTHRATE)/100]
  hide_fac <- fund_ret-hold_ret
  return(hide_fac)
}

key_trade_factor = function(df_tmp,Type_tmp,obs_date,report_index){
  
  reference <- key_stock_pool(df_tmp,Type_tmp,obs_date,report_index)
  
  unique_code <- unique(reference[,INNERCODE])
  return(data.table(INNERCODE=unique_code,
                    HIDE=sapply(unique_code,hide_function)))
}

detail_trade_factor = function(df_tmp,Type_tmp,obs_date,report_index){
  
  reference <- detail_stock_pool(df_tmp,Type_tmp,obs_date,report_index)
  
  unique_code <- unique(reference[,INNERCODE])
  return(data.table(INNERCODE=unique_code,
                    HIDE=sapply(unique_code,hide_function)))
}

#5.得到因子得分----------------------------------------------------
final_score = function(fof_num){
  final_fac <- foresee_fact[sharpe_fac,on=.(INNERCODE=INNERCODE)]%>%
    .[size_fac,on=.(INNERCODE=INNERCODE)]%>%
    .[attention_fac,on=.(INNERCODE=INNERCODE)]%>%
    .[hide_fac,on=.(INNERCODE=INNERCODE)]

  final_fac[,SIZE:=-SIZE]
  #正向因子：前瞻因子，夏普因子，隐藏交易因子，机构关注因子
  #负向因子：规模因子
  final_fac[,lapply(.SD,function(x){
    GCAMCQT::normalizing_zscore(frank(x))
  }),.SDcols=c("FORESEE","SHARPE","SIZE","ATTENTION","HIDE")]%>%
    .[,INNERCODE:=final_fac[,INNERCODE]]->norm_fac#因子排序归一化
  norm_fac[,total_fac:=(FORESEE+SHARPE+SIZE+ATTENTION+HIDE)/5]#因子平均加权
  #2021-10-28添加 删除规模过小的基金
  size_limit <- size_sift(norm_fac,obs_date,report_index)#筛出2亿-100亿的基金
  norm_fac <- norm_fac[INNERCODE%in%size_limit,]
  
  final_fund_list <- norm_fac[order(-total_fac),][1:fof_num,.(INNERCODE,total_fac)]#选出总因子得分最高的前30支基金
  final_fund_list <- merge(final_fund_list,df_tmp,by="INNERCODE",all.x=TRUE)
  return(final_fund_list)
}

#6.最后结果处理--------------------------------------------------------
get_result = function(final_fund_list,Type_tmp,obs_date,report_index){
  detail_match = function(final_fund_list,Type_tmp,obs_date,report_index){
    #基金和仓位的匹配
    key_tmp <- MF_StockPortfolioDetail[INNERCODE%in%unlist(final_fund_list$INNERCODE),]
    #可能存在少数基金匹配遗失问题，有些定期报告只收录C类基金
    main_info <- merge(key_tmp,final_fund_list,by="INNERCODE",all.x=TRUE)
    #进行重新匹配寻找
    lost_code <- setdiff(unique(final_fund_list$INNERCODE),unique(key_tmp$INNERCODE))
    if (length(lost_code)!=0){
      lost_part <- final_fund_list[INNERCODE%in%lost_code,]
      
      l <- lapply(lost_part[,NAME],function(x){
        Type_tmp[NAME==x&CLASS!='A',.(INNERCODE,NAME)]
      })#找出可能匹配到的代码（同名的非A基金）
      name_list <- colnames(MF_StockPortfolioDetail)
      sup_part <- lapply(l,function(x){
        MF_StockPortfolioDetail[INNERCODE%in%unlist(x[,INNERCODE]),
                                .(.SD,NAME=unique(x[,NAME])),
                                .SDcols=name_list]#拿到需要的数据
      })
      sup_info <- data.table()
      for(i in 1:length(sup_part)){
        sup_info <- rbind(sup_info,sup_part[[i]])
      }
      colnames(sup_info) <- c(name_list,'NAME')
      sup_info[,INNERCODE:=NULL]
      sup_info <- merge(sup_info,final_fund_list,by="NAME",all.x=TRUE)
      full_info <- rbind(main_info[,.(INNERCODE,REPORTDATE,STOCKINNERCODE,
                                      RATIOINNV,INFOPUBLDATE,CHINAMEABBR)],
                         sup_info[,.(INNERCODE,REPORTDATE,STOCKINNERCODE,
                                     RATIOINNV,INFOPUBLDATE,CHINAMEABBR)])
    } else{
      print("No need to supplement more infomation")
      full_info <- main_info[,.(INNERCODE,REPORTDATE,STOCKINNERCODE,
                                RATIOINNV,INFOPUBLDATE,CHINAMEABBR)]
    }#如果没有缺失的匹配基金，那就直接返回，否则进行拼接
    
    #获取应该参照的基金持仓报告
    reference <- full_info[REPORTDATE==date_list[report_index]&
                             INFOPUBLDATE<=obs_date,]
    reference <- merge(reference,SecuMain[,.(INNERCODE,SECUCODE,SECUABBR)],
                       by.x="STOCKINNERCODE",by.y="INNERCODE",all.x=TRUE)
    reference <- reference[!is.na(SECUCODE),]
    return(reference)
  }
  key_match = function(final_fund_list,Type_tmp,obs_date,report_index){
    #基金和仓位的匹配
    key_tmp <- MF_KeyStockPortfolio[INNERCODE%in%unlist(final_fund_list$INNERCODE),]
    #可能存在少数基金匹配遗失问题，有些定期报告只收录C类基金
    main_info <- merge(key_tmp,final_fund_list,by="INNERCODE",all.x=TRUE)
    #进行重新匹配寻找
    lost_code <- setdiff(unique(final_fund_list$INNERCODE),unique(key_tmp$INNERCODE))
    if (length(lost_code)!=0){
      lost_part <- final_fund_list[INNERCODE%in%lost_code,]
      
      l <- lapply(lost_part[,NAME],function(x){
        Type_tmp[NAME==x&CLASS!='A',.(INNERCODE,NAME)]
      })#找出可能匹配到的代码（同名的非A基金）
      name_list <- colnames(MF_KeyStockPortfolio)
      sup_part <- lapply(l,function(x){
        MF_KeyStockPortfolio[INNERCODE%in%unlist(x[,INNERCODE]),
                             .(.SD,NAME=unique(x[,NAME])),
                             .SDcols=name_list]#拿到需要的数据
      })
      sup_info <- data.table()
      for(i in 1:length(sup_part)){
        sup_info <- rbind(sup_info,sup_part[[i]])
      }
      colnames(sup_info) <- c(name_list,'NAME')
      sup_info[,INNERCODE:=NULL]
      sup_info <- merge(sup_info,final_fund_list,by="NAME",all.x=TRUE)
      full_info <- rbind(main_info[,.(INNERCODE,REPORTDATE,STOCKINNERCODE,
                                      RATIOINNV,INFOPUBLDATE,CHINAMEABBR)],
                         sup_info[,.(INNERCODE,REPORTDATE,STOCKINNERCODE,
                                     RATIOINNV,INFOPUBLDATE,CHINAMEABBR)])
    } else{
      print("No need to supplement more infomation")
      full_info <- main_info[,.(INNERCODE,REPORTDATE,STOCKINNERCODE,
                                RATIOINNV,INFOPUBLDATE,CHINAMEABBR)]
    }#如果没有缺失的匹配基金，那就直接返回，否则进行拼接
    
    #获取应该参照的基金持仓报告
    reference <- full_info[REPORTDATE==date_list[report_index]&
                             INFOPUBLDATE<=obs_date,]#对应的报告期以及信息发布日期规则
    reference <- merge(reference,SecuMain[,.(INNERCODE,SECUCODE,SECUABBR)],
                       by.x="STOCKINNERCODE",by.y="INNERCODE",all.x=TRUE)
    reference <- reference[!is.na(SECUCODE),]
    return(reference)
  }
  type <- para_table[m==month(obs_date),type]
  if(type=="detail"){
    display <- detail_match(final_fund_list,Type_tmp,obs_date,report_index)
  }
  if(type=="key"){
    display <- key_match(final_fund_list,Type_tmp,obs_date,report_index)
  }
  
  display <- merge(display,size_fac,by="INNERCODE",all.x=TRUE)
  display[,OBS_DATE:=obs_date]
  display[,.(OBS_DATE,INFOPUBLDATE,REPORTDATE,
             INNERCODE,CHINAMEABBR,
             STOCKINNERCODE,SECUCODE,SECUABBR,
             RATIOINNV,SIZE)]
}

#7.基金规模筛选--------------------------------------------------------
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



