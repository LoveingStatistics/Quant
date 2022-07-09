library(data.table)
library(lubridate)

#月底日期列表-----
date_list <- seq(as.POSIXct("2002-01-01"), length=12*20, by="1 month")-ddays(1)
para_table <- data.table(m = seq(1,12,1),
                         report = rep(c(1,2,3),4),
                         type = c(rep("key",2),"detail",rep("key",4),
                                  "detail","detail",rep("key",3)),
                         type1 = c(rep("key",3),rep("detail",3),
                                   rep("key",3),rep("detail",3)),
                         type2 = c(rep("detail",3),rep("key",3),
                                   rep("detail",3),rep("key",3)))
#obs是指盈余公告观察期，report是指对应报告期,type对应公告种类

#读取数据---------------------------------
#获得总的基础基金池
MF_FundArchives <- as.data.table(readRDS("./data/MF_FundArchives.rds"))
SecuMain <- as.data.table(readRDS("./data/SecuMain.rds"))
MF_JYFundType <- as.data.table(readRDS("./data/MF_JYFundType.rds"))
#基金持仓报告
MF_KeyStockPortfolio <- as.data.table(readRDS("./data/MF_KeyStockPortfolio.rds"))
MF_StockPortfolioDetail <- as.data.table(readRDS("./data/MF_StockPortfolioDetail.rds"))
#获取业绩报告
fin_performance_express <- as.data.table(readRDS("./data/fin_performance_express.rds"))
fin_performance_forecast <- as.data.table(readRDS("./data/fin_performance_forecast.rds"))
der_prob_excess_stock <- as.data.table(readRDS("./data/der_prob_excess_stock.rds"))

# name_list <- c("EFFECTIVEDATE","CANCELDATE")
# MF_JYFundType[,(name_list):=lapply(.SD,as.Date),.SDcols=name_list]
##不能直接转换为date类型，因为POSIX转换之后会减少一天，涉及时区问题

#1.获得基金名称(16698)------------------------------------------
Fund_tmp <- merge(MF_FundArchives[,.(INNERCODE,SECURITYCODE,LISTEDDATE,EXPIREDATE)],
                  SecuMain[,.(INNERCODE,CHINAMEABBR)],by="INNERCODE",all.x=TRUE)
#获得基金分类以及不同分类所处时间
Type_tmp <- merge(Fund_tmp,MF_JYFundType[,.(INNERCODE,EFFECTIVEDATE,CANCELDATE,
                                            SECASSETCATNAME)],by='INNERCODE',all.x=TRUE)
dim(Type_tmp)#(17107)
#2.同名只保留A类基金,选择标准股票型和偏股型
Type_tmp <- Type_tmp[SECASSETCATNAME%in%c("标准股票型","偏股型"),]
dim(Type_tmp)#(3869)
Type_tmp[,`:=`(NAME=sapply(CHINAMEABBR,function(x){substr(x,1,nchar(x)-1)}),
               CLASS=sapply(CHINAMEABBR,function(x){substr(x,nchar(x),nchar(x))}))]
name_tmp <- Type_tmp[,.(NUM=.N),by=.(NAME)]#根据同名进行计数统计
num_tmp <- merge(Type_tmp,name_tmp,by="NAME",all.x=TRUE)#将计数结果加入列变量
del_list <- unlist(num_tmp[NUM!=1&CLASS%in%c("B","C","E","H","O","R","X"),.(INNERCODE)])#删除的基金清单
pool_tmp <- Type_tmp[!(INNERCODE%in%del_list),]#保留A类基础基金池名单
dim(pool_tmp)#(2757)
pool_tmp <- pool_tmp[CLASS!='C',]#还有几支C，删除
dim(pool_tmp)#(2752)
pool_tmp <- pool_tmp[!grepl("FOF",pool_tmp[,CHINAMEABBR]),]#删除FOF基金
dim(pool_tmp)#(2685)
pool_tmp <- pool_tmp[!grepl("LOF",pool_tmp[,CHINAMEABBR]),]#删除LOF基金
dim(pool_tmp)#(2584)
filldate <- as.POSIXct('2021-08-25')#截止到取数据那天，cancel为NA，可以理解为仍然存续或者未更新
pool_tmp[is.na(CANCELDATE),CANCELDATE:=filldate]#进行空值填充

#3.后续每个时点选择基础基金池时可以利用分类的,后续改编为函数，获取给定时间的基础股票池
#difftime>15months(65weeks) 并且canceldate大于给定日期进行筛选
fund_pool = function(obs_date,pool_tmp){
  df_tmp <- pool_tmp[difftime(obs_date,EFFECTIVEDATE,units='weeks')>65&
                       difftime(CANCELDATE,obs_date)>0,]#65周对应15个月
  df_tmp[,`:=`(CLASS=NULL,LISTEDDATE=NULL,EXPIREDATE=NULL)]#(980)
  
  return(df_tmp)
}
#重仓股持仓报告处理---------------------------------------------
key_hold_factor = function(df_tmp,Type_tmp,obs_date,report_index){
  #基金和仓位的匹配
  key_tmp <- MF_KeyStockPortfolio[INNERCODE%in%unlist(df_tmp$INNERCODE),]
  #可能存在少数基金匹配遗失问题，有些定期报告只收录C类基金
  main_info <- merge(key_tmp,df_tmp,by="INNERCODE",all.x=TRUE)
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
                         weight_point=sum(GOOD*RATIOINNV)),
                      by=.(INNERCODE)]
  forward[is.na(num_point),num_point:=0]
  forward[is.na(weight_point),num_point:=0]
  #forward[,sum(num_point==0)/nrow(forward)]

}

#全部持仓报告处理(与重仓股几乎一致)------------------------------------
detail_hold_factor = function(df_tmp,Type_tmp,obs_date,report_index){
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
                         weight_point=sum(GOOD*RATIOINNV)),
                      by=.(INNERCODE)]
  #0值比率
  # nrow(forward[num_point==0,])/nrow(forward)
  
}


#A:半衰加权前瞻因子得分------------------
#过去3个报告期加权 1,1/2,1/4

#以2020-8-31为例
obs_date <- as.POSIXct("2020-08-31")
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
foresee_fact <- fac_tmp[,.(INNERCODE,FORESEE=0.5*(num_factor+weight_factor))]
#foresee_fact[order(-FORESEE)]

#补充持仓名单
# LC_MainSHListNew <- as.data.table(readRDS("./data/LC_MainSHListNew.rds"))
# tmp<-LC_MainSHListNew[INFOPUBLDATE<=obs_date&
#                    ENDDATE==ref_date&
#                    INFOTYPECODE%in%c(1,2,4)&
#                    !is.na(SECUINNERCODE)&
#                    SECUINNERCODE%in%unique(reference[,INNERCODE]),.(SECUINNERCODE,COMPANYCODE,
#                                                                     SECUABBR,HOLDASHARESUM)]

fund_list <- fac_tmp[,INNERCODE]

#其他因子-----------------------------------------------------------------
#B:基金夏普因子------------------------------

MF_FundNetValueRe <- as.data.table(readRDS("./data/MF_FundNetValueRe.rds"))
sharp_tmp<-sapply(fund_list,function(x){
  tmp <- MF_FundNetValueRe[INNERCODE==x,][order(TRADINGDAY)][TRADINGDAY<=obs_date
                                                             &TRADINGDAY>=obs_date-dyears(1),]
  tmp[,mean(NVRDAILYGROWTHRATE)/sd(NVRDAILYGROWTHRATE)]
})
sharpe_fac <- data.table(INNERCODE=fund_list,SHARPE=sharp_tmp)
fill_sharpe <- sharpe_fac[,mean(SHARPE)+3*sd(SHARPE)]
sharpe_fac[SHARPE>fill_sharpe,SHARPE:=fill_sharpe]

#C:基金规模因子----------------------------
MF_MainFinancialIndexQ <- as.data.table(readRDS("./data/MF_MainFinancialIndexQ.rds"))
#这里需要做一些处理，就是把同名基金的规模进行合并
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
fill_size <- size_fac[,mean(SIZE)+3*sd(SIZE)]
size_fac[SIZE>fill_size,SIZE:=fill_size]
size_fac[,NAME:=NULL]

#D:基金关注度因子--------------------------------------------
MF_HolderInfo <- as.data.table(readRDS("./data/MF_HolderInfo.rds"))
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
fill_attention <- attention_fac[,mean(ATTENTION)+3*sd(ATTENTION)]
attention_fac[ATTENTION>fill_attention,ATTENTION:=fill_attention]
attention_fac[,NAME:=NULL]

#E:基金经理隐藏交易能力---------------------------------------
library(feather)
stock_quote <- as.data.table(read_feather("./data/qt_dailyquote.feather"))
#stock_quote[,DATE:=as.POSIXct(TRADINGDAY)-period(8,units="hour")]

key_trade_factor = function(df_tmp,Type_tmp,obs_date,report_index){
  #基金和仓位的匹配
  key_tmp <- MF_KeyStockPortfolio[INNERCODE%in%unlist(df_tmp$INNERCODE),]
  #可能存在少数基金匹配遗失问题，有些定期报告只收录C类基金
  main_info <- merge(key_tmp,df_tmp,by="INNERCODE",all.x=TRUE)
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
  #定义隐藏交易因子计算函数
  hide_function <- function(code){
    tmp <- reference[INNERCODE==code,]
    tmp_list <- tmp[,STOCKINNERCODE]
    stock_tmp <- stock_quote[INNERCODE%in%tmp_list,.(INNERCODE,
                                                     TRADINGDAY,
                                                     CLOSEPRICE)]
    #stock_tmp[,DATE:=as.POSIXct(TRADINGDAY)-period(8,units="hour")]
    stock_tmp <- stock_tmp[,DATE:=as.POSIXct(TRADINGDAY)][DATE>(date_list[report_index]+period(8,units="hour"))
                                                          &DATE<=(obs_date+period(8,units="hour")),]
    hold_tmp <- merge(stock_tmp,tmp[,.(STOCKINNERCODE,RATIOINNV)],
                      by.x="INNERCODE",by.y="STOCKINNERCODE",all.x=TRUE)
    hold_tmp <- hold_tmp[,.(NAV=sum(CLOSEPRICE*RATIOINNV)/sum(RATIOINNV)),by=.(DATE)]
    hold_ret <- hold_tmp[,mean(NAV/shift(NAV)-1,na.rm=TRUE)]
    fund_ret <- MF_FundNetValueRe[INNERCODE==code
                                  &TRADINGDAY>date_list[report_index]
                                  &TRADINGDAY<=obs_date,mean(NVRDAILYGROWTHRATE)/100]
    hide_fac <- fund_ret-hold_ret
    return(hide_fac)
  }
  unique_code <- unique(reference[,INNERCODE])
  return(data.table(INNERCODE=unique_code,
                    HIDE=sapply(unique_code,hide_function)))
}
detail_trade_factor = function(df_tmp,Type_tmp,obs_date,report_index){
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
  #定义隐藏交易因子计算函数
  hide_function <- function(code){
    tmp <- reference[INNERCODE==code,]
    tmp_list <- tmp[,STOCKINNERCODE]
    stock_tmp <- stock_quote[INNERCODE%in%tmp_list,.(INNERCODE,
                                                     TRADINGDAY,
                                                     CLOSEPRICE)]
    #stock_tmp[,DATE:=as.POSIXct(TRADINGDAY)-period(8,units="hour")]
    stock_tmp <- stock_tmp[,DATE:=as.POSIXct(TRADINGDAY)][DATE>(date_list[report_index]+period(8,units="hour"))
                                                          &DATE<=(obs_date+period(8,units="hour")),]
    hold_tmp <- merge(stock_tmp,tmp[,.(STOCKINNERCODE,RATIOINNV)],
                      by.x="INNERCODE",by.y="STOCKINNERCODE",all.x=TRUE)
    hold_tmp <- hold_tmp[,.(NAV=sum(CLOSEPRICE*RATIOINNV)/sum(RATIOINNV)),by=.(DATE)]
    hold_ret <- hold_tmp[,mean(NAV/shift(NAV)-1,na.rm=TRUE)]
    fund_ret <- MF_FundNetValueRe[INNERCODE==code
                                  &TRADINGDAY>date_list[report_index]
                                  &TRADINGDAY<=obs_date,mean(NVRDAILYGROWTHRATE)/100]
    hide_fac <- fund_ret-hold_ret
    return(hide_fac)
  }
  unique_code <- unique(reference[,INNERCODE])
  return(data.table(INNERCODE=unique_code,
                    HIDE=sapply(unique_code,hide_function)))
}

type <- para_table[m==month(obs_date),type]
if(type=="detail"){
  hide_fac <- detail_trade_factor(df_tmp,Type_tmp,obs_date,report_index)
}
if(type=="key"){
  hide_fac <- key_trade_factor(df_tmp,Type_tmp,obs_date,report_index)
}

fill_hide <- hide_fac[,mean(HIDE)+3*sd(HIDE)]
hide_fac[HIDE>fill_hide,HIDE:=fill_hide]#去极值

#排序归一化，排序，因子组合得分-------------------------
library(dplyr)
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
final_fund_list <- norm_fac[order(-total_fac),][1:30,.(INNERCODE,total_fac)]#选出总因子得分最高的前30支基金
final_fund_list <- merge(final_fund_list,df_tmp,by="INNERCODE",all.x=TRUE)

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
best_hold <- get_result(final_fund_list,Type_tmp,obs_date,report_index)
