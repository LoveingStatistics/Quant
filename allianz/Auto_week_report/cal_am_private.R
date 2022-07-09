
# 私募基金评价体系
# 私募基金产品表现，包含指数增强，对冲和CTA

# index data
index <- GCAMCPUB::readDtRds(file.path(jydb_path, "index_return.rds"))
bmk_index <- index[INNERCODE == 4978]
bmk_index[, RTN := CLOSEPRICE / PREVCLOSEPRICE - 1]
setkey(bmk_index, TRADINGDAY)

# proddata
index_prod <- readxl::read_xlsx(file.path(download_path, "private_fund.xlsx"), sheet = "指增") %>% as.data.table()
hedge_prod <- readxl::read_xlsx(file.path(download_path, "private_fund.xlsx"), sheet = "对冲") %>% as.data.table()
cta_prod <- readxl::read_xlsx(file.path(download_path, "private_fund.xlsx"), sheet = "CTA") %>% as.data.table()
index_vars <- names(index_prod)[!names(index_prod) %in% "日期"]
hedge_vars <- names(hedge_prod)[!names(hedge_prod) %in% "日期"]
cta_vars <- names(cta_prod)[!names(cta_prod) %in% "日期"]

setnames(index_prod, "日期", "DATE")
setnames(hedge_prod, "日期", "DATE")
setnames(cta_prod, "日期", "DATE")
index_prod[, DATE := as.Date(DATE)]
hedge_prod[, DATE := as.Date(DATE)]
cta_prod[, DATE := as.Date(DATE)]
setkey(index_prod, DATE)
setkey(hedge_prod, DATE)
setkey(cta_prod, DATE)

# perf
melt_index_prod <- melt.data.table(index_prod, id.vars = "DATE") %>% setkey(DATE, variable)
melt_hedge_prod <- melt.data.table(hedge_prod, id.vars = "DATE") %>% setkey(DATE, variable)
melt_cta_prod <- melt.data.table(cta_prod, id.vars = "DATE") %>% setkey(DATE, variable)
melt_index_prod[, value := as.numeric(value)]
melt_hedge_prod[, value := as.numeric(value)]
melt_cta_prod[, value := as.numeric(value)]

melt_index_prod[, RTN := value / shift(value) - 1, by = variable]
melt_hedge_prod[, RTN := value / shift(value) - 1, by = variable]
melt_cta_prod[, RTN := value / shift(value) - 1, by = variable]
melt_index_prod <- melt_index_prod[!is.na(RTN)]
melt_hedge_prod <- melt_hedge_prod[!is.na(RTN)]
melt_cta_prod <- melt_cta_prod[!is.na(RTN)]

index_prod_rtn <- dcast.data.table(melt_index_prod, DATE~variable, value.var = "RTN")
index_prod_rtn[, BMK := bmk_index[J(index_prod_rtn$DATE)]$RTN]
hedge_prod_rtn <- dcast.data.table(melt_hedge_prod, DATE~variable, value.var = "RTN")
cta_prod_rtn <- dcast.data.table(melt_cta_prod, DATE~variable, value.var = "RTN")


index_perf <- new_perf(as.xts.data.table(index_prod_rtn), c(index_vars, "BMK"))
hedge_perf <- new_perf(as.xts.data.table(hedge_prod_rtn), hedge_vars)
cta_perf <- new_perf(as.xts.data.table(cta_prod_rtn), cta_vars)

# stat
# 针对指数增强需要计算相对中证500的收益率
# 超额收益率 本年超额回撤
# 收益率 回撤 夏普
# 统计每周，过去3个月，今年以来的情况

# 指数增强基金统计 --------------------------------------------------------------------------------------
index_perf$set_bmk("BMK")
# 周度
index_perf$param_index <- paste0(format(prev_week_date, "%Y%m%d"), "/")
index_week_rtn <- as.data.table(summary(index_perf, "cr_abs"))
index_week_rtn <- melt.data.table(index_week_rtn, id.vars = "PERIOD") %>% .[, .(NAME = variable, WEEK_ABS = value)] %>% setkey(NAME)
index_week_excess <- as.data.table(summary(index_perf, "cr_rel"))
index_week_excess <- melt.data.table(index_week_excess, id.vars = "PERIOD") %>% .[, .(NAME = variable, WEEK_EXCESS = value)] %>% setkey(NAME)

# 过去三个月
index_perf$param_index <- paste0(format(update_date - 90, "%Y%m%d"), "/")
index_month3_rtn <- as.data.table(summary(index_perf, "cr_abs"))
index_month3_rtn <- melt.data.table(index_month3_rtn, id.vars = "PERIOD") %>% .[, .(NAME = variable, M3_ABS = value)] %>% setkey(NAME)
index_month3_excess <- as.data.table(summary(index_perf, "cr_rel"))
index_month3_excess <- melt.data.table(index_month3_excess, id.vars = "PERIOD") %>% .[, .(NAME = variable, M3_EXCESS = value)] %>% setkey(NAME)

# 今年以来
index_perf$param_index <- paste0(format(prev_year_date, "%Y%m%d"), "/")
index_year_rtn <- as.data.table(summary(index_perf, "cr_abs"))
index_year_rtn <- melt.data.table(index_year_rtn, id.vars = "PERIOD") %>% .[, .(NAME = variable, YEAR_ABS = value)] %>% setkey(NAME)
index_year_excess <- as.data.table(summary(index_perf, "cr_rel"))
index_year_excess <- melt.data.table(index_year_excess, id.vars = "PERIOD") %>% .[, .(NAME = variable, YEAR_EXCESS = value)] %>% setkey(NAME)
index_year_excess_mdd <- as.data.table(summary(index_perf, "mdd_rel"))
index_year_excess_mdd <- melt.data.table(index_year_excess_mdd, id.vars = "PERIOD") %>%
.[, .(NAME = variable, YEAR_EXCESS_MDD = value)] %>% setkey(NAME)

index_prod_stat <- index_week_rtn[index_week_excess][index_month3_rtn][index_month3_excess][index_year_rtn][index_year_excess][index_year_excess_mdd]

# 量化对冲基金统计 ----------------------------------------------------------------------------------------------------
# 周度
hedge_perf$param_index <- paste0(format(prev_week_date, "%Y%m%d"), "/")
hedge_week_rtn <- as.data.table(summary(hedge_perf, "cr_abs"))
hedge_week_rtn <- melt.data.table(hedge_week_rtn, id.vars = "PERIOD") %>% .[, .(NAME = variable, WEEK_ABS = value)] %>% setkey(NAME)

# 过去三个月
hedge_perf$param_index <- paste0(format(update_date - 90, "%Y%m%d"), "/")
hedge_month3_rtn <- as.data.table(summary(hedge_perf, "cr_abs"))
hedge_month3_rtn <- melt.data.table(hedge_month3_rtn, id.vars = "PERIOD") %>% .[, .(NAME = variable, M3_ABS = value)] %>% setkey(NAME)

# 今年以来
hedge_perf$param_index <- paste0(format(prev_year_date, "%Y%m%d"), "/")
hedge_year_rtn <- as.data.table(summary(hedge_perf, "cr_abs"))
hedge_year_rtn <- melt.data.table(hedge_year_rtn, id.vars = "PERIOD") %>% .[, .(NAME = variable, YEAR_ABS = value)] %>% setkey(NAME)
hedge_year_mdd <- as.data.table(summary(hedge_perf, "mdd_abs"))
hedge_year_mdd <- melt.data.table(hedge_year_mdd, id.vars = "PERIOD") %>%
.[, .(NAME = variable, YEAR_MDD = value)] %>% setkey(NAME)
hedge_prod_stat <- hedge_week_rtn[hedge_month3_rtn][hedge_year_rtn][hedge_year_mdd]


# CTA基金统计 ----------------------------------------------------------------------------------------------------
# 周度
cta_perf$param_index <- paste0(format(prev_week_date, "%Y%m%d"), "/")
cta_week_rtn <- as.data.table(summary(cta_perf, "cr_abs"))
cta_week_rtn <- melt.data.table(cta_week_rtn, id.vars = "PERIOD") %>% .[, .(NAME = variable, WEEK_ABS = value)] %>% setkey(NAME)

# 过去三个月
cta_perf$param_index <- paste0(format(update_date - 90, "%Y%m%d"), "/")
cta_month3_rtn <- as.data.table(summary(cta_perf, "cr_abs"))
cta_month3_rtn <- melt.data.table(cta_month3_rtn, id.vars = "PERIOD") %>% .[, .(NAME = variable, M3_ABS = value)] %>% setkey(NAME)

# 今年以来
cta_perf$param_index <- paste0(format(prev_year_date, "%Y%m%d"), "/")
cta_year_rtn <- as.data.table(summary(cta_perf, "cr_abs"))
cta_year_rtn <- melt.data.table(cta_year_rtn, id.vars = "PERIOD") %>% .[, .(NAME = variable, YEAR_ABS = value)] %>% setkey(NAME)
cta_year_mdd <- as.data.table(summary(cta_perf, "mdd_abs"))
cta_year_mdd <- melt.data.table(cta_year_mdd, id.vars = "PERIOD") %>%
.[, .(NAME = variable, YEAR_MDD = value)] %>% setkey(NAME)
cta_prod_stat <- cta_week_rtn[cta_month3_rtn][cta_year_rtn][cta_year_mdd]

#画图数据
#指增
index_select_name <- as.character(index_prod_stat[order(-YEAR_EXCESS),head(NAME,8)])
index_select_comp <- melt_index_prod[variable%in%index_select_name
&DATE<=update_date,]
bmk_index <- index[INNERCODE == 4978]
bmk_index <- bmk_index[TRADINGDAY%in%unique(index_prod$DATE),]
bmk_index[, RTN := CLOSEPRICE / shift(CLOSEPRICE) - 1]
setkey(bmk_index, TRADINGDAY)
bmk_index <- bmk_index[!is.na(RTN),]

index_select_comp[,EXCESS_RET:=RTN-bmk_index$RTN,by=.(variable)]

index_select_comp[,NAV:=cumprod(1+EXCESS_RET),by=.(variable)]
index_select_comp[,variable:=factor(variable,levels=index_select_name)]

#对冲
hedge_select_name <- as.character(hedge_prod_stat[order(-YEAR_ABS),NAME])
hedge_select_comp <- melt_hedge_prod[variable%in%hedge_select_name
&DATE>=prev_year_date
&DATE<=update_date,]

hedge_select_comp[,NAV:=cumprod(1+RTN),by=.(variable)]
hedge_select_comp[,variable:=factor(variable,levels=hedge_select_name)]

#CTA
cta_select_name <- as.character(cta_prod_stat[order(-YEAR_ABS),NAME])
cta_select_comp <- melt_cta_prod[variable%in%cta_select_name
&DATE>=prev_year_date
&DATE<=update_date,]

cta_select_comp[,NAV:=cumprod(1+RTN),by=.(variable)]
cta_select_comp[,variable:=factor(variable,levels=cta_select_name)]


GCAMCPUB::saveDtRds(index_select_comp,file.path(output_path,'private_fund_index.rds'))
GCAMCPUB::saveDtRds(hedge_select_comp,file.path(output_path,'private_fund_hedge.rds'))
GCAMCPUB::saveDtRds(cta_select_comp,file.path(output_path,'private_fund_cta.rds'))

#做表
index_prod_stat <- index_prod_stat[order(-WEEK_EXCESS),][!is.na(WEEK_EXCESS),]
hedge_prod_stat <- hedge_prod_stat[order(-WEEK_ABS),][!is.na(WEEK_ABS),]
cta_prod_stat <- cta_prod_stat[order(-WEEK_ABS),][!is.na(WEEK_ABS),]

index_prod_stat[,`:=`(WEEK_ABS=GCAMCPUB::f_fmt_pct(WEEK_ABS,2),WEEK_EXCESS=GCAMCPUB::f_fmt_pct(WEEK_EXCESS,2),
M3_ABS=GCAMCPUB::f_fmt_pct(M3_ABS,2),M3_EXCESS=GCAMCPUB::f_fmt_pct(M3_EXCESS,2),
YEAR_ABS=GCAMCPUB::f_fmt_pct(YEAR_ABS,2),YEAR_EXCESS=GCAMCPUB::f_fmt_pct(YEAR_EXCESS,2),
YEAR_EXCESS_MDD=GCAMCPUB::f_fmt_pct(YEAR_EXCESS_MDD,2))]
hedge_prod_stat[,`:=`(WEEK_ABS=GCAMCPUB::f_fmt_pct(WEEK_ABS,2),M3_ABS=GCAMCPUB::f_fmt_pct(M3_ABS,2),
YEAR_ABS=GCAMCPUB::f_fmt_pct(YEAR_ABS,2),YEAR_MDD=GCAMCPUB::f_fmt_pct(YEAR_MDD,2))]
cta_prod_stat[,`:=`(WEEK_ABS=GCAMCPUB::f_fmt_pct(WEEK_ABS,2),M3_ABS=GCAMCPUB::f_fmt_pct(M3_ABS,2),
YEAR_ABS=GCAMCPUB::f_fmt_pct(YEAR_ABS,2),YEAR_MDD=GCAMCPUB::f_fmt_pct(YEAR_MDD,2))]

setnames(index_prod_stat,c('产品名称','本周绝对收益','本周超额收益','前三个月绝对收益','前三个月超额收益','今年来绝对收益','今年来超额收益','今年来最大回撤'))
setnames(hedge_prod_stat,c('产品名称','本周绝对收益','前三个月绝对收益','今年来绝对收益','今年来最大回撤'))
setnames(cta_prod_stat,c('产品名称','本周绝对收益','前三个月绝对收益','今年来绝对收益','今年来最大回撤'))

GCAMCPUB::saveDtRds(index_prod_stat,file.path(output_path,'private_fund_detail_index.rds'))
GCAMCPUB::saveDtRds(hedge_prod_stat,file.path(output_path,'private_fund_detail_hedge.rds'))
GCAMCPUB::saveDtRds(cta_prod_stat,file.path(output_path,'private_fund_detail_cta.rds'))

