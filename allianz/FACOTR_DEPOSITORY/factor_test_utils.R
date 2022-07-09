#因子检验框架-----------------------------------------------------

factor_test <- new.env()

factor_test$prep_data <- function(stkprice,factor,vars,dates,start,end,save_folder,uqer_folder){
  expr <- parse(text = paste0(factor, "(var=vars,dates=dates,
                              start=start,end=end,
                              save_folder=save_folder,
                              uqer_folder=uqer_folder)"))
  raw <- eval(expr,envir=list(vars=vars,dates=dates,start=start,end=end,save_folder=save_folder,uqer_folder=uqer_folder))
  raw <- raw[DATE>=dates]
  print(paste0("缺失值比例为:",nrow(raw[is.na(RAW),])/nrow(raw)))
  raw
}

factor_test$day_to_month <- function(raw,start,end,stkprice) {
  raw <- raw[!is.na(RAW),]
  adjust_date <- data.table(TDAY=w.tdays(format(start,"%Y%m%d"),format(end+30,"%Y%m%d"),"Period=M")$Data$DATETIME)
  adjust_date[,DELAY_TDAY:=shift(TDAY,-1)]
  setkey(adjust_date,TDAY)
  
  #计算持仓起止日-------------------------------------------------
  factor_raw <- raw[DATE%in%adjust_date[,TDAY],]
  factor_raw[,DELAY_TDAY:=adjust_date[J(factor_raw$DATE)]$DELAY_TDAY]
  tradingdays <- w.tdays(start,end+100)$Data$DATETIME
  next_trading_day <- function(dates,tradingdays) {
    tdays <- tradingdays[which(tradingdays==dates)+1]
    return(tdays)
  }
  factor_raw[,`:=`(STARTDAY=as.Date(sapply(DATE,next_trading_day,tradingdays),origin='1970-01-01'),
                   ENDDAY=as.Date(sapply(DELAY_TDAY,next_trading_day,tradingdays),origin='1970-01-01'))]
  #计算区间收益率---------------------------
  period <- sort(unique(factor_raw$STARTDAY))
  for(i in seq_along(period)) {
    period_factor <- factor_raw[STARTDAY==period[i],]
    stk_ret <- stkprice[INNERCODE%in%period_factor$INNER_CODE
                        &TRADINGDAY>=unique(period_factor$STARTDAY)
                        &TRADINGDAY<=unique(period_factor$ENDDAY),
                        .(RET=last(BACKWARDPRICE)/first(BACKWARDPRICE)-1),by=.(INNERCODE)]
    factor_raw[STARTDAY==period[i]&INNER_CODE%in%stk_ret$INNERCODE,RET:=stk_ret$RET]
  }
  factor_raw <- factor_raw[!is.na(RET),]
  factor_raw
}

factor_test$index_select <- function(indexcomponent, index_select, N, factor_raw) {
  #分不同的股票池进行因子测试-----
  # factor_raw <- factor_test$prep_data(stkprice, factor,vars,dates,start,end,save_folder,uqer_folder)
  pool_period <- sort(unique(factor_raw$DATE))
  index_raw <- data.table()
  for(i in seq_along(pool_period)) {
    judge_date <- pool_period[i]
    tot_list <- indexcomponent[INDEXINNERCODE==index_select
                               &INDATE<=judge_date
                               &(OUTDATE>judge_date|is.na(OUTDATE)),SECUINNERCODE]
    period_raw <- factor_raw[DATE==judge_date&INNER_CODE%in%tot_list,]
    index_raw <- rbind(index_raw, period_raw)
  }
  factor_raw <- index_raw
  eligible_date <- factor_raw[,.(NUM=.N),by=.(DATE)][NUM>=2*N,DATE]
  factor_raw <- factor_raw[DATE%in%eligible_date,]
  factor_raw
}

test_package <- function(factor_raw, stkprice, secumain) {
  #2.IC 均值以及分年度IC---------------------------------------------------
  factor_raw[,YEAR:=year(DATE)]
  ic_temp <- factor_raw[,.(IC=cor(RAW,RET,method='spearman')),by=.(YEAR,DATE)]
  yearly_ic <- ic_temp[,.(IC_MEAN=mean(IC)),by=.(YEAR)]
  yearly_ic[,IC_MEAN:=GCAMCPUB::f_fmt_pct(IC_MEAN,3)]
  print(yearly_ic)
  
  IC <- factor_raw[,.(rank_IC=cor(RAW,RET,method='spearman')),
                   by=.(DATE)]
  #IC均值------------
  rank_IC_MEAN <- mean(IC$rank_IC)
  
  #ICIR--------------
  rank_ICIR <- mean(IC$rank_IC)/sd(IC$rank_IC)
  #3.多空组合-----------------
  period <- sort(unique(factor_raw$STARTDAY))
  
  #3.1根据rankIC因子和股票收益间应该是正相关，所以做多最高组，做空最低组
  factor_ret <- data.table()
  period_group_ret <- data.table()
  for(i in seq_along(period)) {
    period_factor <- factor_raw[STARTDAY==period[i],]
    period_factor[,GROUP:=cut(RAW,quantile(RAW,seq(0,1,1/N)),
                              labels=FALSE,include.lowest = TRUE)]
    setkey(period_factor,INNER_CODE)
    stk_nav <- stkprice[INNERCODE%in%period_factor$INNER_CODE
                        &TRADINGDAY>=unique(period_factor$STARTDAY)
                        &TRADINGDAY<=unique(period_factor$ENDDAY),
                        .(TRADINGDAY,NAV=BACKWARDPRICE/first(BACKWARDPRICE)),by=.(INNERCODE)]
    stk_nav[,GROUP:=period_factor[J(stk_nav$INNERCODE)]$GROUP]
    group_nav <- stk_nav[,.(NAV=mean(NAV)),by=.(GROUP,TRADINGDAY)]
    group_ret <- group_nav[,.(TRADINGDAY,RET=NAV/shift(NAV)-1),by=.(GROUP)]
    group_ret <- group_ret[!is.na(RET)]
    setkey(group_ret,GROUP,TRADINGDAY)
    period_ret <- group_nav[,.(TRADINGDAY=period[i],PERIOD_RET=last(NAV)-1),by=.(GROUP)]
    period_group_ret <- rbind(period_group_ret,period_ret)
    factor_ret <- rbind(factor_ret,group_ret)
  }
  
  #3.2分组净值-------------
  factor_nav <- copy(factor_ret)
  factor_nav[,NAV:=cumprod(1+RET),by=.(GROUP)]
  factor_nav[,GROUP:=as.factor(GROUP)]
  
  #3.3分组月平均收益------------
  period_ret <- period_group_ret[,.(MEAN_RET=mean(PERIOD_RET)),by=.(GROUP)]
  period_ret[,GROUP:=as.factor(GROUP)]
  setkey(period_ret,GROUP)
  
  
  #3.4多空组合净值------------
  if(rank_IC_MEAN>0) {
    direction <- "+"
  }else{
    direction <- '-'
  }
  if(direction=='-'){
    long <- factor_nav[GROUP==1,]
    short <- factor_nav[GROUP==N,]
  }else{
    long <- factor_nav[GROUP==N,]
    short <- factor_nav[GROUP==1,]
  }
  
  setkey(short,TRADINGDAY)
  long[,SHORT_RET:=short[J(long$TRADINGDAY)]$RET]
  long[,LS_RET:=RET-SHORT_RET]
  long[,LS_NAV:=cumprod(1+LS_RET)]
  
  #3.5多空策略最大回撤--------------
  drawdown <- function(nav){
    if(length(nav)!=0){
      cum_max <- cummax(nav)
      drowdown <- 1-nav/cum_max
      return(max(drowdown))
    }else{
      return(NA)
    }
  }
  MDD <- long[,.(DRAWDOWN=drawdown(LS_NAV))]
  #3.6多空每期平均收益率---------
  period_LS <- data.table()
  for(i in seq_along(period)) {
    if(i==length(period)){
      ls_period <- long[TRADINGDAY>=period[i],
                        .(TRADINGDAY,NAV=cumprod(1+LS_RET))]
    }else{
      ls_period <- long[TRADINGDAY>=period[i]
                        &TRADINGDAY<=period[i+1],
                        .(TRADINGDAY,NAV=cumprod(1+LS_RET))]
    }
    period_LS <- rbind(period_LS,ls_period[,.(RET=last(NAV)-1)])
    
  }
  MEAN_RET <- period_LS[,mean(RET)]
  #3.7多空胜率----------
  WIN_RATIO <- period_LS[,sum(RET>0)/.N]
  #3.8多空IR---------
  LS_IR <- long[,sqrt(252)*mean(LS_RET)/sd(LS_RET)]
  
  #4.plot-------------
  IC_plot <- ggplot(data=IC,aes(x=DATE,y=rank_IC))+geom_bar(stat='identity')+
    geom_hline(yintercept=rank_IC_MEAN,color='blue')+
    theme_bw()+
    theme(panel.grid.major.y = element_line(linetype = 2, color = "black"),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank())
  
  group_nav_plot <- ggplot(data=factor_nav,aes(x=TRADINGDAY,y=NAV))+geom_line(aes(color=GROUP))+
    theme_bw()+
    theme(panel.grid.major.y = element_line(linetype = 2, color = "black"),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank())
  
  group_ret_plot <- ggplot(data=period_ret,aes(x=GROUP,y=MEAN_RET))+geom_bar(stat='identity',fill='blue')+
    theme_bw()+
    theme(panel.grid.major.y = element_line(linetype = 2, color = "black"),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank())
  
  ls_plot <- ggplot(data=long,aes(x=TRADINGDAY,y=LS_NAV))+geom_line(color='red')+
    theme_bw()+
    theme(panel.grid.major.y = element_line(linetype = 2, color = "black"),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank())
  
  GCAMCPUB:::multiplot(IC_plot,group_nav_plot,group_ret_plot,ls_plot,cols=2)
  
  #统计数据--------------
  
  result <- data.table(IC=rank_IC_MEAN,ICIR=rank_ICIR,
                       MEAN_RET=MEAN_RET,
                       MDD=MDD,
                       WIN_RATIO=WIN_RATIO,
                       IR=LS_IR)
  result <- result[,lapply(.SD,f_fmt_pct,digits=3)]
  result[,NAME:=secumain[J(as.numeric(index_select))]$CHINAMEABBR]
  
  print(result)
}

yearly_check <- function(factor_raw, stkprice, index, index_select) {
  
  base_index <- index[INNERCODE==index_select,.(TRADINGDAY,RET)]
  setkey(base_index,TRADINGDAY)#引入基准指数
  
  series <- seq(2013,2021)
  plots <- list()
  yearly_ret <- data.table()
  for(single in series) {
    single_raw <- copy(factor_raw[year(STARTDAY)==single,])
    
    factor_ret <- data.table()
    period_group_ret <- data.table()
    period <- sort(unique(single_raw$STARTDAY))
    for(i in seq_along(period)) {
      period_factor <- single_raw[STARTDAY==period[i],]
      period_factor[,GROUP:=cut(RAW,quantile(RAW,seq(0,1,1/N)),
                                labels=FALSE,include.lowest = TRUE)]
      setkey(period_factor,INNER_CODE)
      stk_nav <- stkprice[INNERCODE%in%period_factor$INNER_CODE
                          &TRADINGDAY>=unique(period_factor$STARTDAY)
                          &TRADINGDAY<=unique(period_factor$ENDDAY),
                          .(TRADINGDAY,NAV=BACKWARDPRICE/first(BACKWARDPRICE)),by=.(INNERCODE)]
      stk_nav[,GROUP:=period_factor[J(stk_nav$INNERCODE)]$GROUP]
      group_nav <- stk_nav[,.(NAV=mean(NAV)),by=.(GROUP,TRADINGDAY)]
      group_ret <- group_nav[,.(TRADINGDAY,RET=NAV/shift(NAV)-1),by=.(GROUP)]
      group_ret <- group_ret[!is.na(RET)]
      setkey(group_ret,GROUP,TRADINGDAY)
      period_ret <- group_nav[,.(TRADINGDAY=period[i],PERIOD_RET=last(NAV)-1),by=.(GROUP)]
      period_group_ret <- rbind(period_group_ret,period_ret)
      factor_ret <- rbind(factor_ret,group_ret)
    }#计算分组收益率
    factor_ret[,BASE_RET:=base_index[J(factor_ret$TRADINGDAY)]$RET]
    factor_ret[,EXCESS_RET:=RET-BASE_RET]#计算分组超额收益率
    
    factor_nav <- copy(factor_ret)
    factor_nav[,NAV:=cumprod(1+EXCESS_RET),by=.(GROUP)]
    factor_nav[,GROUP:=as.factor(GROUP)]#计算分组累计超额净值
    
    year_nav <- factor_nav[,last(NAV)/first(NAV)-1,by=.(GROUP)]
    year_nav[,year:=single]
    year_nav <- dcast(year_nav,year~GROUP,value.var="V1")
    yearly_ret <- rbind(yearly_ret,year_nav)
    
    plots[[single]] <- ggplot(data=factor_nav,aes(x=TRADINGDAY,y=NAV))+geom_line(aes(color=GROUP))+
      theme_bw()+
      theme(panel.grid.major.y = element_line(linetype = 2, color = "black"),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank())
  }
  GCAMCPUB:::multiplot(plots[[2013]],plots[[2014]],plots[[2015]],
                       plots[[2016]],plots[[2017]],plots[[2018]],
                       plots[[2019]],plots[[2020]],plots[[2021]],cols=3)
  yearly_ret[,c("1",'2','3','4','5'):=lapply(.SD,GCAMCPUB::f_fmt_pct,digits=2),
             .SDcols=c("1",'2','3','4','5')]
  print(yearly_ret)
  
}
