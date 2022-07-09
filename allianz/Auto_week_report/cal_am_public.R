#指数增强和对冲
#同一支的话去掉C类
#统计方法仿照私募基金script
#沪深300 中证500 对冲表格展示收益
#从JY分类中 按照公司进行收益平均

###Asset Management report


#1. public find-----------------------------------------------------------------
#主动型 指数增强
#另类：量化对冲
#债基：纯债 二级债基
#今年以来（多个区间段） 各类基金 中位数 平均数 上下四分位数 样本量
###代表性基金统计表
fundtype <- GCAMCPUB::readDtRds(file.path(jydb_path,'MF_JYFundType.rds'))
nav <- GCAMCPUB::readDtRds(file.path(jydb_path,'MF_FundNetValueRe.rds'))
secumain <- GCAMCPUB::readDtRds(file.path(jydb_path,'secumain.rds'))
index <- GCAMCPUB::readDtRds(file.path(jydb_path, "index_return.rds"))


fundtype <- fundtype[,.(INNERCODE,THIRDASSETCATCODE,EFFECTIVEDATE,CANCELDATE)]
setkey(fundtype,INNERCODE)
setkey(secumain,INNERCODE)

type_match <- function(start, end, fundnav, fundtype){
fundnav <- fundnav[TRADINGDAY>=start&TRADINGDAY<=end,
.(INNERCODE,TRADINGDAY,UNITNVRESTORED)]
fund_list <- unique(fundnav$INNERCODE)
type_search = function(x){
if(nrow(fundtype[INNERCODE==x,])==1){
fundnav[INNERCODE==x,TYPE:=fundtype[INNERCODE==x,THIRDASSETCATCODE]]
}else if(nrow(fundtype[INNERCODE==x,])==2){
change_date <- min(fundtype[INNERCODE==x,CANCELDATE])
fundnav[INNERCODE==x&TRADINGDAY<=change_date,
TYPE:=fundtype[INNERCODE==x&CANCELDATE==change_date,THIRDASSETCATCODE]]
fundnav[INNERCODE==x&TRADINGDAY>change_date,
TYPE:=fundtype[INNERCODE==x,last(THIRDASSETCATCODE)]]
}
}
for(i in fund_list){
type_search(i)
}
fundnav
}

start <- prev_year_date
end <- update_date

fundnav <- type_match(start, end, nav, fundtype)


#calculation--------------------------------------------------------------------
#长债130101 中短债130102+130103 二级债基130202 普通股票110101 偏股混合120101 指增110202 对冲120602  FOF999999
fundnav[,CHINABBR:=secumain[J(fundnav$INNERCODE)]$CHINAMEABBR]
fundnav[grepl("FOF",CHINABBR)&!grepl("QDII",CHINABBR),TYPE:=999999]
target_fund_type <- c(130101,130102,130103,130202,110101,120101,110202,120602,999999)
required_fund <- fundnav[TYPE%in%target_fund_type,]
setkey(required_fund,INNERCODE,TRADINGDAY)

bmk_index <- index[INNERCODE == 4978] #zz500 index
setkey(bmk_index, TRADINGDAY)
len_trading <- nrow(bmk_index[TRADINGDAY>=start&TRADINGDAY<=end,])#区间交易日天数


#分组统计结果

subtype <- function(required_fund,code,len_trading, start){
  fund <- required_fund[TYPE%in%code,]
  qualified_code <- fund[,.N,by=.(INNERCODE)][N==len_trading,INNERCODE]
  fund <- fund[INNERCODE%in%qualified_code,]
  
  fund[,RET:=UNITNVRESTORED/shift(UNITNVRESTORED)-1,by=.(INNERCODE)]
  fund_ret <- fund[,.(INNERCODE,TRADINGDAY,RET)]
  fund_ret <- fund_ret[!is.na(RET),]
  fund_ret[,INNERCODE:=as.character(INNERCODE)]
  
  fund_rtn <- dcast.data.table(fund_ret,TRADINGDAY~INNERCODE,value.var='RET')
  var <- names(fund_rtn)[!names(fund_rtn)%in%'TRADINGDAY']
  
  perf <- new_perf(as.xts.data.table(fund_rtn), var)
  perf$param_index <- paste0(format(start, "%Y%m%d"), "/")
  period_rtn <- as.data.table(summary(perf, "cr_abs"))
  period_rtn <- melt.data.table(period_rtn, id.vars = "PERIOD") %>% .[, .(NAME = variable, PERIOD_ABS = value)] %>% setkey(NAME)
  
}

long_period_rtn <- subtype(required_fund,130101,len_trading,start)
med_period_rtn <- subtype(required_fund,c(130102,130103),len_trading,start)
second_period_rtn <- subtype(required_fund,130202,len_trading,start)
stock_period_rtn <- subtype(required_fund,110101,len_trading,start)
stock_mix_period_rtn <- subtype(required_fund,120101,len_trading,start)
hedge_period_rtn <- subtype(required_fund,120602,len_trading,start)
enhance_period_rtn <- subtype(required_fund,110202,len_trading,start)
fof_period_rtn <- subtype(required_fund,999999,len_trading,start)

long_period_rtn[,`:=`(TYPE='长债基金',NAME=NULL)]
med_period_rtn[,`:=`(TYPE='中短债基金',NAME=NULL)]
second_period_rtn[,`:=`(TYPE='二级债基',NAME=NULL)]
stock_period_rtn[,`:=`(TYPE='标准股票型基金',NAME=NULL)]
stock_mix_period_rtn[,`:=`(TYPE='偏股混合基金',NAME=NULL)]
hedge_period_rtn[,`:=`(TYPE='对冲型基金',NAME=NULL)]
enhance_period_rtn[,`:=`(TYPE='指数增强型基金',NAME=NULL)]
fof_period_rtn[,`:=`(TYPE='FOF型基金',NAME=NULL)]

public_fund_result <- dplyr::bind_rows(list(long_period_rtn,med_period_rtn,second_period_rtn,
stock_period_rtn,stock_mix_period_rtn,hedge_period_rtn,
enhance_period_rtn,fof_period_rtn))

public_fund_result[,TYPE:=factor(TYPE,levels=c("长债基金","中短债基金",
"二级债基","标准股票型基金",
"偏股混合基金","对冲型基金","指数增强型基金",
"FOF型基金"))]

GCAMCPUB::saveDtRds(public_fund_result,file.path(output_path,'public_fund_ret.rds'))


#2. 展示量化公募基金表现按照公司划分----------------------------------------------
#>90% 按照每家统计每日基金收益的平均值，然后绘出平均净值曲线，从而以此计算相关指标
#500----------------------------------------------------------------------------
fund_archive <- GCAMCPUB::readDtRds(file.path(jydb_path,'MF_FundArchives.rds'))
fund_manager <- GCAMCPUB::readDtRds(file.path(jydb_path,'MF_InvestAdvisorOutline.rds'))
setkey(fund_manager,INVESTADVISORCODE)

#中证500对应的基金
temp <- fund_archive[grepl('90.0％×中证小盘500指数|95.0％×中证小盘500指数|100.0％×中证小盘500指数',PERFORMANCEBENCHMARK),
.(INNERCODE,SECURITYCODE,
INVESTADVISORCODE,PERFORMANCEBENCHMARK)]
temp[,MANAGER:=fund_manager[J(temp$INVESTADVISORCODE)]$INVESTADVISORABBRNAME]
setkey(temp,INNERCODE)

#需要统计的公司
hs500 <- nav[INNERCODE%in%unique(temp$INNERCODE),]
hs500_company <- readxl::read_xlsx(file.path(download_path, "公募基金公司.xlsx"), sheet = "zz500") %>% as.data.table()
hs500[,RET:=UNITNVRESTORED/shift(UNITNVRESTORED)-1,by=.(INNERCODE)]
hs500[,MANAGER:=temp[J(hs500$INNERCODE)]$MANAGER]
hs500 <- hs500[MANAGER%in%hs500_company$NAME,]
#每日收益率的均值
hs500_ret <- hs500[,.(M_RET=mean(RET,na.rm=T)),by=.(MANAGER,TRADINGDAY)]
hs500_ret <- hs500_ret[!is.na(M_RET)]
hs500_ret[,NAV:=cumprod(1+M_RET),by=.(MANAGER)]
hs500_ret_dcast <- dcast.data.table(hs500_ret, TRADINGDAY~MANAGER, value.var = "M_RET")
#benchmark index
bmk_index <- index[INNERCODE == 4978]
bmk_index[, RTN := CLOSEPRICE / PREVCLOSEPRICE - 1]
setkey(bmk_index, TRADINGDAY)
hs500_ret_dcast[,BMK:=bmk_index[J(hs500_ret_dcast$TRADINGDAY)]$RTN]
hs500_ret_dcast <- hs500_ret_dcast[TRADINGDAY<=update_date,]
#performance index
hs500_ret_vars <- names(hs500_ret_dcast)[!names(hs500_ret_dcast)=='TRADINGDAY']
hs500_perf <- new_perf(as.xts.data.table(hs500_ret_dcast), hs500_ret_vars)
hs500_perf$set_bmk('BMK')

#收益率-------------------------------------------------------------------------
#本周
hs500_perf$param_index <- paste0(format(prev_week_date, "%Y%m%d"), "/")
hs500_week_rtn <- as.data.table(summary(hs500_perf, "cr_abs"))
hs500_week_rtn <- melt.data.table(hs500_week_rtn, id.vars = "PERIOD") %>%
.[, .(NAME = variable, WEEK_ABS = value)] %>% setkey(NAME)
hs500_week_excess <- as.data.table(summary(hs500_perf, "cr_rel"))
hs500_week_excess <- melt.data.table(hs500_week_excess, id.vars = "PERIOD") %>%
.[, .(NAME = variable, WEEK_EXCESS = value)] %>% setkey(NAME)
#近三月
hs500_perf$param_index <- paste0(format(update_date - 90, "%Y%m%d"), "/")
hs500_three_rtn <- as.data.table(summary(hs500_perf, "cr_abs"))
hs500_three_rtn <- melt.data.table(hs500_three_rtn, id.vars = "PERIOD") %>%
.[, .(NAME = variable, THREE_ABS = value)] %>% setkey(NAME)
hs500_three_excess <- as.data.table(summary(hs500_perf, "cr_rel"))
hs500_three_excess <- melt.data.table(hs500_three_excess, id.vars = "PERIOD") %>%
.[, .(NAME = variable, THREE_EXCESS = value)] %>% setkey(NAME)
#今年来
hs500_perf$param_index <- paste0(format(prev_year_date, "%Y%m%d"), "/")
hs500_year_rtn <- as.data.table(summary(hs500_perf, "cr_abs"))
hs500_year_rtn <- melt.data.table(hs500_year_rtn, id.vars = "PERIOD") %>%
.[, .(NAME = variable, YEAR_ABS = value)] %>% setkey(NAME)
hs500_year_excess <- as.data.table(summary(hs500_perf, "cr_rel"))
hs500_year_excess <- melt.data.table(hs500_year_excess, id.vars = "PERIOD") %>%
.[, .(NAME = variable, YEAR_EXCESS = value)] %>% setkey(NAME)
#今年最大回撤-------------------------------------------------------------------
hs500_year_excess_mdd <- as.data.table(summary(hs500_perf, "mdd_rel"))
hs500_year_excess_mdd <- melt.data.table(hs500_year_excess_mdd, id.vars = "PERIOD") %>%
.[, .(NAME = variable, YEAR_EXCESS_MDD = value)] %>% setkey(NAME)

hs500_stat <- hs500_week_rtn[hs500_week_excess][hs500_three_rtn][hs500_three_excess][hs500_year_rtn][hs500_year_excess][hs500_year_excess_mdd]

#300----------------------------------------------------------------------------
temp <- fund_archive[grepl('90.0％×沪深300指数|95.0％×沪深300指数|100.0％×沪深300指数',PERFORMANCEBENCHMARK),
.(INNERCODE,SECURITYCODE,
INVESTADVISORCODE,PERFORMANCEBENCHMARK)]
temp[,MANAGER:=fund_manager[J(temp$INVESTADVISORCODE)]$INVESTADVISORABBRNAME]
setkey(temp,INNERCODE)
#沪深300对应的基金
hs300 <- nav[INNERCODE%in%unique(temp$INNERCODE),]
hs300_company <- readxl::read_xlsx(file.path(download_path, "公募基金公司.xlsx"), sheet = "hs300") %>% as.data.table()

hs300[,RET:=UNITNVRESTORED/shift(UNITNVRESTORED)-1,by=.(INNERCODE)]
hs300[,MANAGER:=temp[J(hs300$INNERCODE)]$MANAGER]
hs300 <- hs300[MANAGER%in%hs300_company$NAME,]
hs300_ret <- hs300[,.(M_RET=mean(RET,na.rm=T)),by=.(MANAGER,TRADINGDAY)]
hs300_ret <- hs300_ret[!is.na(M_RET)]
hs300_ret[,NAV:=cumprod(1+M_RET),by=.(MANAGER)]
hs300_ret_dcast <- dcast.data.table(hs300_ret, TRADINGDAY~MANAGER, value.var = "M_RET")
#指数收益率
bmk_index <- index[INNERCODE == 3145]
bmk_index[, RTN := CLOSEPRICE / PREVCLOSEPRICE - 1]
setkey(bmk_index, TRADINGDAY)
hs300_ret_dcast[,BMK:=bmk_index[J(hs300_ret_dcast$TRADINGDAY)]$RTN]
hs300_ret_dcast <- hs300_ret_dcast[TRADINGDAY<=update_date,]

#performance index
hs300_ret_vars <- names(hs300_ret_dcast)[!names(hs300_ret_dcast)=='TRADINGDAY']
hs300_perf <- new_perf(as.xts.data.table(hs300_ret_dcast), hs300_ret_vars)
hs300_perf$set_bmk('BMK')

#收益率------------------------------------------------------------------------
#本周
hs300_perf$param_index <- paste0(format(prev_week_date, "%Y%m%d"), "/")
hs300_week_rtn <- as.data.table(summary(hs300_perf, "cr_abs"))
hs300_week_rtn <- melt.data.table(hs300_week_rtn, id.vars = "PERIOD") %>%
.[, .(NAME = variable, WEEK_ABS = value)] %>% setkey(NAME)
hs300_week_excess <- as.data.table(summary(hs300_perf, "cr_rel"))
hs300_week_excess <- melt.data.table(hs300_week_excess, id.vars = "PERIOD") %>%
.[, .(NAME = variable, WEEK_EXCESS = value)] %>% setkey(NAME)
#三个月
hs300_perf$param_index <- paste0(format(update_date - 90, "%Y%m%d"), "/")
hs300_three_rtn <- as.data.table(summary(hs300_perf, "cr_abs"))
hs300_three_rtn <- melt.data.table(hs300_three_rtn, id.vars = "PERIOD") %>%
.[, .(NAME = variable, THREE_ABS = value)] %>% setkey(NAME)
hs300_three_excess <- as.data.table(summary(hs300_perf, "cr_rel"))
hs300_three_excess <- melt.data.table(hs300_three_excess, id.vars = "PERIOD") %>%
.[, .(NAME = variable, THREE_EXCESS = value)] %>% setkey(NAME)
#本年
hs300_perf$param_index <- paste0(format(prev_year_date, "%Y%m%d"), "/")
hs300_year_rtn <- as.data.table(summary(hs300_perf, "cr_abs"))
hs300_year_rtn <- melt.data.table(hs300_year_rtn, id.vars = "PERIOD") %>%
.[, .(NAME = variable, YEAR_ABS = value)] %>% setkey(NAME)
hs300_year_excess <- as.data.table(summary(hs300_perf, "cr_rel"))
hs300_year_excess <- melt.data.table(hs300_year_excess, id.vars = "PERIOD") %>%
.[, .(NAME = variable, YEAR_EXCESS = value)] %>% setkey(NAME)
#今年最大回撤-------------------------------------------------------------------
hs300_year_excess_mdd <- as.data.table(summary(hs300_perf, "mdd_rel"))
hs300_year_excess_mdd <- melt.data.table(hs300_year_excess_mdd, id.vars = "PERIOD") %>%
.[, .(NAME = variable, YEAR_EXCESS_MDD = value)] %>% setkey(NAME)
hs300_stat <- hs300_week_rtn[hs300_week_excess][hs300_three_rtn][hs300_three_excess][hs300_year_rtn][hs300_year_excess][hs300_year_excess_mdd]


#ALPHA--------------------------------------------------------------------------
temp <- fund_archive[INNERCODE%in%fundtype[THIRDASSETCATCODE==120602,unique(INNERCODE)]]
temp[,MANAGER:=fund_manager[J(temp$INVESTADVISORCODE)]$INVESTADVISORABBRNAME]
setkey(temp,INNERCODE)
#对冲基金品种
alpha <- nav[INNERCODE%in%unique(temp$INNERCODE),]
alpha_company <- readxl::read_xlsx(file.path(download_path, "公募基金公司.xlsx"), sheet = "alpha") %>% as.data.table()
#对冲基金公司
alpha[,RET:=UNITNVRESTORED/shift(UNITNVRESTORED)-1,by=.(INNERCODE)]
alpha[,MANAGER:=temp[J(alpha$INNERCODE)]$MANAGER]

alpha <- alpha[MANAGER%in%alpha_company$NAME,]
alpha_ret <- alpha[,.(M_RET=mean(RET,na.rm=T)),by=.(MANAGER,TRADINGDAY)]
alpha_ret <- alpha_ret[!is.na(M_RET)]
alpha_ret[,NAV:=cumprod(1+M_RET),by=.(MANAGER)]
alpha_ret_dcast <- dcast.data.table(alpha_ret, TRADINGDAY~MANAGER, value.var = "M_RET")

alpha_ret_vars <- names(alpha_ret_dcast)[!names(alpha_ret_dcast)=='TRADINGDAY']
alpha_perf <- new_perf(as.xts.data.table(alpha_ret_dcast), alpha_ret_vars)

#收益率-------------------------------------------------------------------------
#本周
alpha_perf$param_index <- paste0(format(prev_week_date, "%Y%m%d"), "/")
alpha_week_rtn <- as.data.table(summary(alpha_perf, "cr_abs"))
alpha_week_rtn <- melt.data.table(alpha_week_rtn, id.vars = "PERIOD") %>%
.[, .(NAME = variable, WEEK_ABS = value)] %>% setkey(NAME)
#三月
alpha_perf$param_index <- paste0(format(update_date - 90, "%Y%m%d"), "/")
alpha_three_rtn <- as.data.table(summary(alpha_perf, "cr_abs"))
alpha_three_rtn <- melt.data.table(alpha_three_rtn, id.vars = "PERIOD") %>%
.[, .(NAME = variable, THREE_ABS = value)] %>% setkey(NAME)
#今年
alpha_perf$param_index <- paste0(format(prev_year_date, "%Y%m%d"), "/")
alpha_year_rtn <- as.data.table(summary(alpha_perf, "cr_abs"))
alpha_year_rtn <- melt.data.table(alpha_year_rtn, id.vars = "PERIOD") %>%
.[, .(NAME = variable, YEAR_ABS = value)] %>% setkey(NAME)
#中邮和中金是周度数据所以没有收益率结果
#年最大回撤
alpha_year_excess_mdd <- as.data.table(summary(alpha_perf, "mdd_abs"))
alpha_year_excess_mdd <- melt.data.table(alpha_year_excess_mdd, id.vars = "PERIOD") %>%
.[, .(NAME = variable, YEAR_RTN_MDD = value)] %>% setkey(NAME)

alpha_stat <- alpha_week_rtn[alpha_three_rtn][alpha_year_rtn][alpha_year_excess_mdd]

#hs500公募TOP8
hs500_select_name <- as.character(hs500_stat[order(-YEAR_EXCESS),head(NAME,8)])
hs500_select_comp <- hs500_ret[MANAGER%in%hs500_select_name
&TRADINGDAY>=prev_year_date
&TRADINGDAY<=update_date,]
bmk_index <- index[INNERCODE == 4978]
bmk_index[, RTN := CLOSEPRICE / PREVCLOSEPRICE - 1]
setkey(bmk_index, TRADINGDAY)
bmk_index <- bmk_index[TRADINGDAY>=prev_year_date
&TRADINGDAY<=update_date,]
hs500_select_comp[,EXCESS_RET:=M_RET-bmk_index$RTN,by=.(MANAGER)]

hs500_select_comp[,NAV:=cumprod(1+EXCESS_RET),by=.(MANAGER)]
hs500_select_comp[,MANAGER:=factor(MANAGER,levels=hs500_select_name)]

#hs300公募TOP8
hs300_select_name <- as.character(hs300_stat[order(-YEAR_EXCESS),head(NAME,8)])
hs300_select_comp <- hs300_ret[MANAGER%in%hs300_select_name
&TRADINGDAY>=prev_year_date
&TRADINGDAY<=update_date,]
bmk_index <- index[INNERCODE == 3145]
bmk_index[, RTN := CLOSEPRICE / PREVCLOSEPRICE - 1]
setkey(bmk_index, TRADINGDAY)
bmk_index <- bmk_index[TRADINGDAY>=prev_year_date
&TRADINGDAY<=update_date,]
hs300_select_comp[,EXCESS_RET:=M_RET-bmk_index$RTN,by=.(MANAGER)]

hs300_select_comp[,NAV:=cumprod(1+EXCESS_RET),by=.(MANAGER)]
hs300_select_comp[,MANAGER:=factor(MANAGER,levels=hs300_select_name)]

#alpha公募TOP8
alpha_select_name <- as.character(alpha_stat[order(-YEAR_ABS),head(NAME,8)])
alpha_select_comp <- alpha_ret[MANAGER%in%alpha_select_name
&TRADINGDAY>=prev_year_date
&TRADINGDAY<=update_date,]
alpha_select_comp[,NAV:=NAV/first(NAV),by=.(MANAGER)]
alpha_select_comp[,MANAGER:=factor(MANAGER,levels=alpha_select_name)]

GCAMCPUB::saveDtRds(hs500_select_comp,file.path(output_path,'public_fund_500.rds'))
GCAMCPUB::saveDtRds(hs300_select_comp,file.path(output_path,'public_fund_300.rds'))
GCAMCPUB::saveDtRds(alpha_select_comp,file.path(output_path,'public_fund_alpha.rds'))


hs500_stat[order(-WEEK_EXCESS),][,`:=`(WEEK_ABS=GCAMCPUB::f_fmt_pct(WEEK_ABS,2),WEEK_EXCESS=GCAMCPUB::f_fmt_pct(WEEK_EXCESS,2),
THREE_ABS=GCAMCPUB::f_fmt_pct(THREE_ABS,2),THREE_EXCESS=GCAMCPUB::f_fmt_pct(THREE_EXCESS,2),
YEAR_ABS=GCAMCPUB::f_fmt_pct(YEAR_ABS,2),YEAR_EXCESS=GCAMCPUB::f_fmt_pct(YEAR_EXCESS,2),
YEAR_EXCESS_MDD=GCAMCPUB::f_fmt_pct(YEAR_EXCESS_MDD,2))]
hs300_stat[,`:=`(WEEK_ABS=GCAMCPUB::f_fmt_pct(WEEK_ABS,2),WEEK_EXCESS=GCAMCPUB::f_fmt_pct(WEEK_EXCESS,2),
THREE_ABS=GCAMCPUB::f_fmt_pct(THREE_ABS,2),THREE_EXCESS=GCAMCPUB::f_fmt_pct(THREE_EXCESS,2),
YEAR_ABS=GCAMCPUB::f_fmt_pct(YEAR_ABS,2),YEAR_EXCESS=GCAMCPUB::f_fmt_pct(YEAR_EXCESS,2),
YEAR_EXCESS_MDD=GCAMCPUB::f_fmt_pct(YEAR_EXCESS_MDD,2))]
alpha_stat[,`:=`(WEEK_ABS=GCAMCPUB::f_fmt_pct(WEEK_ABS,2),THREE_ABS=GCAMCPUB::f_fmt_pct(THREE_ABS,2),
YEAR_ABS=GCAMCPUB::f_fmt_pct(YEAR_ABS,2),YEAR_RTN_MDD=GCAMCPUB::f_fmt_pct(YEAR_RTN_MDD,2))]

GCAMCPUB::saveDtRds(hs500_stat,file.path(output_path,'public_fund_500_stat.rds'))
GCAMCPUB::saveDtRds(hs300_stat,file.path(output_path,'public_fund_300_stat.rds'))
GCAMCPUB::saveDtRds(alpha_stat,file.path(output_path,'public_fund_alpha_stat.rds'))




#3. new fund--------------------------------------------------------------------
start_date <- as.Date("2021-01-01")
#两张表都需要手动下载
#1.新发基金数据统计
df <- data.table::fread(file.path(download_path,"基金发行明细.csv"))
#在wind下载
df <- df[!grepl("被动",unlist(df[,"投资类型(二级分类)"])),]
df <- df[!grepl("指数",unlist(df[,"投资类型(二级分类)"])),]
df <- df[!grepl("QDII",unlist(df[,"投资类型(二级分类)"]))]
#2.df数据处理
data <- df[,c("代码","基金成立日","投资类型(二级分类)","合并发行份额(亿份)")]
colnames(data) <- c("CODE","ESTABLISHDATE","TYPE","SIZE")
data[,`:=`(ESTABLISHDATE=as.Date(ESTABLISHDATE,'%Y/%m/%d'))]
data <- data[!is.na(SIZE),]#删除没有规模的基金(不符合入池标准)
#3.读取交易日数据,可以替换成更为完善的交易日数据
turnover <- readRDS(file.path(jydb_path,'a_turnover.rds'))
turnover <- turnover[order(ENDDATE),]
#4.找出今年的trading_date
td <- unique(turnover[ENDDATE>start_date,ENDDATE])
wk <- rep(0,length(td))
st <- 1
wk[1] <- st
for(i in 2:length(td)){
if ((td[i]-td[i-1])>2){
st <- st + 1
wk[i] <- st
}else{
wk[i] <- st
}
}
week_para <- data.table(t_d=as.Date(td),wk=wk)#所在周
tmp <- data[ESTABLISHDATE>=start_date,]
tmp <- week_para[tmp,on=.(t_d=ESTABLISHDATE)]
tmp <- tmp[order(t_d),]
tmp$wk <- fill_na(tmp$wk)#填充周末成立的周数为空的基金

tmp[, NEW_TYPE := NA_character_]
tmp[TYPE %in% c("普通股票型基金", "偏股混合型基金"), NEW_TYPE := "股票型基金"]
tmp[TYPE %in% c("中长期纯债型基金", "偏债混合型基金", "混合债券型二级基金", "短期纯债型基金"), NEW_TYPE := "债券型基金"]
tmp[TYPE %in% c("平衡混合型基金", "灵活配置型基金"), NEW_TYPE := "股债平衡型基金"]
tmp[TYPE %in% c("REITs"), NEW_TYPE := "REITs"]
tmp[TYPE %in% c("债券型FOF基金", "混合型FOF基金"), NEW_TYPE := "FOF基金"]
tmp[TYPE %in% c("商品型基金"), NEW_TYPE := "商品型基金"]

result <- tmp[,.(TOTAL=.N,SIZE=sum(SIZE,na.rm=TRUE)),by=.(wk)][order(wk),]
result[,wk:=factor(as.character(result$wk))]

result_size <- tmp[, .(SIZE = sum(SIZE, na.rm = TRUE)), by = .(wk, NEW_TYPE)]

#5.近两周发行统计
last_two_week <- tmp[wk<=max(wk)&wk>=(max(wk)-1),]
last_result <- last_two_week[,.(.N,TOT_SIZE=sum(SIZE,na.rm=TRUE)),by=.(TYPE)][order(-N)]

#6.近两周新发基金明细
info <- df[,c("代码","名称","基金经理","管理公司","基金托管人")]
setnames(info,c("CODE","NAME","MANAGER","COMPANY","CUSTODIAN"))
detail <- info[last_two_week,on=.(CODE=CODE)]

stock_ten <- detail[grepl("股",TYPE),][order(-SIZE)][1:10][,.(DATE=t_d,CODE,NAME,MANAGER,COMPANY,TYPE,SIZE)]
nonstock_ten <- detail[!grepl("股",TYPE),][order(-SIZE)][1:10][,.(DATE=t_d,CODE,NAME,MANAGER,COMPANY,TYPE,SIZE)]

#4. securities AM---------------------------------------------------------------
df <- data.table::fread(file.path(download_path,"产品发行统计.csv"),skip=1,encoding='UTF-8')
security <- df[2:nrow(df),1:4]
colnames(security) <- c("日期", "数量", "规模(亿元)", "平均规模(亿元)")


#资管产品保存
GCAMCPUB::saveDtRds(result,file.path(output_path,'public_fund_new_stat.rds'))
GCAMCPUB::saveDtRds(result_size,file.path(output_path,'public_fund_type_stat.rds'))
GCAMCPUB::saveDtRds(stock_ten,file.path(output_path,'public_fund_new_stock.rds'))
GCAMCPUB::saveDtRds(nonstock_ten,file.path(output_path,'public_fund_new_nonstock.rds'))
GCAMCPUB::saveDtRds(security,file.path(output_path,'security_asset_management_product.rds'))


