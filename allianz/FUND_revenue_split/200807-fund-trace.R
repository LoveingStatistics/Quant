library(data.table)
library(lubridate)
library(magrittr)
library(WindR)
w.start()
# 加上季度报的，回测结果比只使用年报&半年报更好一些
# 考虑仓位的，回测结果比不考虑仓位的稍差；下跌年份下跌的比全仓更低，上涨年份上涨的比全仓也低

data_fold <- './data'

start <- ymd(20100101)
# end <- core::tlast()
end <- ymd(20200807)

get_trade_day <- function(dates, method = 'f') {
  #  b: 获取当日或当日之后的交易日；f：获取当日或当日之前的交易日
  dates <- lubridate::ymd(dates)
  tradingdays <- data.table(DATE = w.tdays(min(dates) - 10, max(dates) + 10)$Data$DATETIME)
  if (method == 'b') {
    tradingdays[J(dates), x.DATE, on = 'DATE', roll = -Inf]
  }
  if (method == 'f') {
    tradingdays[J(dates), x.DATE, on = 'DATE', roll = Inf]
  }
}

# 找出所有时间段符合的标准股票型基金
jy_stkfunds <- local({
  fundtype <- GCAMCPUB::readDtRds(file.path(data_fold,"MF_JYFundType.rds"))
  fundarch <- GCAMCPUB::readDtRds(file.path(data_fold,"MF_FundArchives.rds"))
  secumain <- GCAMCPUB::readDtRds(file.path(data_fold,"SecuMain.rds"))
  fundtype <- fundtype[,.(INNERCODE,EFFECTIVEDATE,CANCELDATE,THIRDASSETCATNAME)]
  fundarch <- fundarch[,.(INNERCODE,MAINCODE)]
  secumain <- secumain[,.(INNERCODE,SECUCODE,SECUCATEGORY)]
  
  setkey(secumain,INNERCODE)
  setkey(fundarch,INNERCODE)
  fundtype[,`:=`(MAINCODE=fundarch[J(fundtype$INNERCODE)]$MAINCODE,
                 SECUCATEGORY=secumain[J(fundtype$INNERCODE)]$SECUCATEGORY,
                 SECUCODE=secumain[J(fundtype$INNERCODE)]$SECUCODE)]
  jy_stkfunds <- fundtype[THIRDASSETCATNAME%in%c('标准股票型', '股债平衡型', '偏股型', '其他混合型')
                          &SECUCATEGORY==8]
  jy_stkfunds <- jy_stkfunds[SECUCODE == MAINCODE]
  
  jy_stkfunds <- jy_stkfunds[, .(EFFECTIVEDATE = min(EFFECTIVEDATE),
                                 CANCELDATE = max(CANCELDATE)), by = .(INNERCODE, SECUCODE, MAINCODE)]
})

stkfunds <- local({
  stkfunds <- CJ(INNERCDOE = jy_stkfunds[, unique(INNERCODE)], 
                 DATE = w.tdays(start, end)$Data$DATETIME)
  stkfunds[, `:=` (EFFECTIVEDATE = jy_stkfunds[J(stkfunds$INNERCDOE), EFFECTIVEDATE, on = 'INNERCODE'],
                   CANCELDATE = jy_stkfunds[J(stkfunds$INNERCDOE), CANCELDATE, on = 'INNERCODE'])]
  stkfunds[is.na(CANCELDATE), CANCELDATE := as.POSIXct(end + 1)]
  stkfunds[, IS_EFF := (DATE >= EFFECTIVEDATE & DATE < CANCELDATE)]
  stkfunds <- stkfunds[IS_EFF == TRUE, .(INNERCDOE, DATE)]
  stkfunds[]
})#选出在日期内生效的基金

funds_ptfdetail <- local({
  # 年报 & 半年报
  allinners <- stkfunds[, unique(INNERCDOE)]
  detailstock <- GCAMCPUB::readDtRds(file.path(data_fold,"detailstock.rds"))
  res <- detailstock[INNERCODE%in%allinners,]
})

funds_keystks <- local({
  # 十大重仓股（主要看季报）
  allinners <- stkfunds[, unique(INNERCDOE)]
  keystock <- GCAMCPUB::readDtRds(file.path(data_fold,"keystock.rds"))
  res <- keystock[INNERCODE%in%allinners,]
  res <- res[(month(REPORTDATE) == 3 & day(REPORTDATE) == 31) | (month(REPORTDATE) == 9 & day(REPORTDATE) == 30)]
})

stock_allc <- local({
  # 基金持有股票的仓位
  allinners <- stkfunds[, unique(INNERCDOE)]
  position <- GCAMCPUB::readDtRds(file.path(data_fold,"stockposition.rds"))
  res <- position[INNERCODE%in%allinners&ASSETTYPECODE==10020,]
  res[, NETASSET := MARKETVALUE / RATIOINNV][]
})

# 4月底披露年报和一季度报，8月底披露半年报，10月披露三季度报
rep_pub_dates <- local({
  yearrange <- c(year(start), 2020)
  rep_enddates <- ymd(unlist(purrr::map((yearrange[1]-1):yearrange[2], function(x) {
    paste0(x, c('-03-31', '-06-30', '09-30', '-12-31'))
  })))
  rep_enddates <- rep_enddates[rep_enddates >= make_date(year = yearrange[1]-1, month = 12, day = 31)]
  
  cal_end_publdate <- function(dates) {
    tdates <- copy(dates)
    stopifnot(all(unique(month(tdates)) %in% c(6, 12, 3, 9)))
    tdates[month(tdates) == 6] <- tdates[month(tdates) == 6] %m+% months(2)
    tdates[month(tdates) == 12] <- tdates[month(tdates) == 12] %m+% months(4)
    tdates[month(tdates) == 3] <- tdates[month(tdates) == 3] %m+% months(1)
    tdates[month(tdates) == 9] <- tdates[month(tdates) == 9] %m+% months(1)
    tdates
  }
  pub_enddates <- cal_end_publdate(rep_enddates)
  data.table(REP = rep_enddates, PUB = pub_enddates)
})

pos <- local({
  stock_allc[, ENDPUBDAY := rep_pub_dates[J(REPORTDATE), PUB, on = 'REP']]
  stock_allc <- stock_allc[!is.na(ENDPUBDAY)]
  stock_allc <-  stock_allc[INFOPUBLDATE <= ENDPUBDAY]
  stock_allc[, `:=` (REP_TRADEDAY = get_trade_day(REPORTDATE, 'f'),
                     PUB_TRADEDAY = get_trade_day(ENDPUBDAY, 'f'))]
  pos <- stock_allc[, .(POS = sum(MARKETVALUE) / sum(NETASSET)), by = .(REPORTDATE, REP_TRADEDAY, PUB_TRADEDAY)]
  setorder(pos, PUB_TRADEDAY, REPORTDATE)
  pos <- pos[!duplicated(pos[, .(PUB_TRADEDAY)], fromLast = TRUE)]
  pos[]
})

funds_stkdetail <- rbind(funds_keystks, funds_ptfdetail)
# funds_stkdetail <- copy(funds_ptfdetail)
funds_stkdetail[, ENDPUBDAY := rep_pub_dates[J(REPORTDATE), PUB, on = 'REP']]
funds_stkdetail <- funds_stkdetail[!is.na(ENDPUBDAY)]
funds_stkdetail <- funds_stkdetail[INFOPUBLDATE <= ENDPUBDAY]
funds_stkdetail[, `:=` (REP_TRADEDAY = get_trade_day(REPORTDATE, 'f'),
                        PUB_TRADEDAY = get_trade_day(ENDPUBDAY, 'f'))]
setorder(funds_stkdetail, INNERCODE, ENDPUBDAY, STOCKINNERCODE, REPORTDATE)
funds_stkdetail <- funds_stkdetail[
  !duplicated(funds_stkdetail[, .(INNERCODE, ENDPUBDAY, STOCKINNERCODE)], fromLast = TRUE)
]


dailyquote <- GCAMCPUB::readDtRds(file.path(data_fold,"dailyquote.rds"))
kcb <- GCAMCPUB::readDtRds(file.path(data_fold,"kcb_performance.rds"))
kcb <- kcb[,.(INNERCODE,TRADINGDAY,PREVCLOSEPRICE,CLOSEPRICE)]
qtquote <- rbind(dailyquote,kcb)
setkey(qtquote,INNERCODE,TRADINGDAY)

# funds_stkdetail[!STOCKINNERCODE %in% qtquote$INNERCODE]

funds_stkdetail[, `:=` (
  REPORT_CLOSE = qtquote[J(STOCKINNERCODE, REP_TRADEDAY), CLOSEPRICE, on = c('INNERCODE', 'TRADINGDAY')],
  PUB_CLOSE = qtquote[J(STOCKINNERCODE, PUB_TRADEDAY), CLOSEPRICE, on = c('INNERCODE', 'TRADINGDAY')]
)]
funds_stkdetail <- funds_stkdetail[!is.na(REPORT_CLOSE) & !is.na(PUB_CLOSE)]
funds_stkdetail[, VALUE := SHARESHOLDING * PUB_CLOSE]

stocks_value <- funds_stkdetail[, .(VALUE = sum(VALUE, na.rm = TRUE)), by = .(STOCKINNERCODE, PUB_TRADEDAY)]
stocks_value[, WEIGHT := VALUE / sum(VALUE, na.rm = TRUE), by = .(PUB_TRADEDAY)]

tradeday_list <- w.tdays('2009-01-01', '2023-12-31')$Data$DATETIME
pub_tradeday <- unique(stocks_value$PUB_TRADEDAY)

for(i in seq_along(pub_tradeday)) {
  dt_diff <- tradeday_list - pub_tradeday[i]
  NEXTDATE <- tradeday_list[which(dt_diff>0)[1]]
  stocks_value[PUB_TRADEDAY==pub_tradeday[i],NEXT_TRADINGDAY:=NEXTDATE]
}
stocks_value[,NEXT_TRADINGDAY:=as.Date(NEXT_TRADINGDAY,origin='1970-01-01')]

# stocks_value[, NEXT_TRADINGDAY := GCAMCQT::next_trading_date(PUB_TRADEDAY, mkt = 'SH')]
setkey(stocks_value, STOCKINNERCODE, PUB_TRADEDAY)
stocks_value[, POS := pos[J(stocks_value$PUB_TRADEDAY), POS, on = 'PUB_TRADEDAY', roll = Inf]]

stock_weight_pos <- stocks_value[, .(INNER_CODE = STOCKINNERCODE, 
                                     DATE = NEXT_TRADINGDAY, 
                                     WEIGHT = WEIGHT * POS)]
stock_weight <- stocks_value[, .(INNER_CODE = STOCKINNERCODE, 
                                 DATE = NEXT_TRADINGDAY, 
                                 
                                 WEIGHT = WEIGHT)]
# 停牌股票
stocks_value_del_suspended <- copy(stocks_value)
setnames(stocks_value_del_suspended, c("STOCKINNERCODE", "NEXT_TRADINGDAY"), c("INNER_CODE", "DATE"))
setkey(stocks_value_del_suspended, INNER_CODE, DATE)
GCAMCQT::attach_suspended_flag(stocks_value_del_suspended, 'daily', 1L)
stocks_value_del_suspended <- stocks_value_del_suspended[SUSPENDED_FLAG == FALSE]
stocks_value_del_suspended[, WEIGHT := VALUE / sum(VALUE), by = .(DATE)] # 暂时对于停牌的股票没处理
stocks_value_del_suspended[, POS := pos[J(stocks_value_del_suspended$DATE), POS, on = 'PUB_TRADEDAY', roll = Inf]]
stock_weight_pos <- stocks_value_del_suspended[, .(INNER_CODE, DATE, WEIGHT = WEIGHT * POS)]
stock_weight <- stocks_value_del_suspended[, .(INNER_CODE, DATE, WEIGHT)]


# BACKTEST
library(GCAMCBT)
library(GCAMCQT)
from_to <- as_from_to(c(start, end))
# candle <- candle_ashare(from_to)
td <- trading_desk()
# load_candle(td, candle)
config_trading(td, trading_consider_lot = FALSE)

ptf <- backtest_by_weight(
  td, stock_weight, as_from_to(c(stock_weight[, min(DATE)] - 10, end)), 
  exec_prices = c('vwap', 'vwap'), record_trade = TRUE,
  record_order = TRUE, record_pos = TRUE, init_capital = 1e8)

ptf_pos <- backtest_by_weight(
  td, stock_weight_pos, as_from_to(c(stock_weight[, min(DATE)] - 10, end)), 
  exec_prices = c('vwap', 'vwap'), record_trade = TRUE,
  record_order = TRUE, record_pos = TRUE, init_capital = 1e8)

dates <- tradeday_list[tradeday_list>=stock_weight[, min(DATE)] - 1& tradeday_list<=end]
zdgj <- w.wsd("930890.CSI","pct_chg",stock_weight[, min(DATE)] - 1,end)$Data %>% as.data.table()
zdgj <- zdgj[,.(PCT_CHG=PCT_CHG/100)]
# dates <- core::tdates(stock_weight[, min(DATE)] - 1, end)
# zdgj <- core::bchmk_dr("930890.CSI", dates)
zdgj <- xts::xts(zdgj, order.by = dates)
names(zdgj) <- "ZDGJ"
data <- cbind(xts::as.xts(ptf$nav[DATE %in% dates, .(DATE, DR)]), zdgj)
data <- cbind(data, DR_POS = xts::as.xts(ptf_pos$nav[DATE %in% dates, .(DATE, DR_POS = DR)]))

perf <- Xts_perf$new('PTF' = data[, "DR"], PTF_POS = data[, "DR_POS"], ZDGJ = data[, "ZDGJ"])
perf$set_bmk("ZDGJ")
plot(perf, "xts_cr_abs_rel")
plot(perf, "xts_cr_abs_rel", freq = "y")

perf$param_index <- '20110101/'
perf$param_index <- ''

perf$param_freq = 'year'
perf$param_freq = 'month'
perf$param_freq = NULL

summary(perf, 'cr_abs')
summary(perf, 'cr_rel')


