source('uqer_data.R')

fill_code <- function(code) {
  while(nchar(code)!=6){
    code <- paste0("0",code)
  }
  return(code)
}

overlapping <- function(x, y, lag_cycle) {
  
  tbl <- data.table(QUARTER = unique(y$QUARTER), START = shift(unique(y$QUARTER), lag_cycle - 1))
  x <- tbl[x, on = "QUARTER"]
  x$START[is.na(x$START)] <- tbl$QUARTER[1]
  setkeyv(x, c("SYMBOL", "START", "QUARTER"))
  y[, START := QUARTER]
  setkeyv(y, c("SYMBOL", "START", "QUARTER"))
  mapped <- foverlaps(x, y, type = "any", mult = "all")
  mapped[, c("QUARTER", "i.QUARTER", "i.START") := list(i.QUARTER, NULL, NULL)][]
}

lm_growth_acce <- function(y) {
  
  x <- seq(length(y))
  lm_regression <- lm(y ~ x + I(x^2))
  z <- as.numeric(lm_regression$coefficients[3])
}

lm_res <- function(y, x1, x2=0) {
  y <- GCAMCQT::normalizing_zscore(y)
  x1 <- GCAMCQT::normalizing_zscore(x1)
  if(length(x2)>1) {
    x2 <- GCAMCQT::normalizing_zscore(x2)
    x <- cbind(x1, x2)
  }else{
   x <- x1 
  }
  # x <- cbind(1, x)
  # ybar <- x%*%(solve(t(x)%*%x)%*%t(x)%*%y)
  # res <- y - ybar
  tryCatch({res <- lm(y~x,na.action=na.omit)$residuals
                    return(res[length(res)])},
                  error=function(e){return(NA)})
}

# 涉及到环比和同比计算
# 环比的话直接采用原始数据shift计算
# 同比数据采用－365计算


func_eps_growth_3y <- function(var, dates, start, end, save_folder, uqer_folder) {
  
  eps <- read_raw_data(start, end, c("EPS"), uqer_folder) 
  
  eps[is.na(RAW), RAW := NA_real_]
  eps <- eps[!is.na(RAW)]
  
  quarter_eps <- eps[, .(DATE = head(DATE, 1)), by = .(SYMBOL, RAW)]
  quarter_eps <- quarter_eps[!is.na(RAW)]
  setkey(quarter_eps, SYMBOL, DATE)
  
  quarter_eps[, PREV_YDATE := DATE - 365.5]
  quarter_eps[, PREV_YDATE2 := DATE - 365.5*2]
  quarter_eps[, PREV_YRAW := quarter_eps[J(quarter_eps$SYMBOL, quarter_eps$PREV_YDATE), roll = T]$RAW]
  quarter_eps[, PREV_YRAW2 := quarter_eps[J(quarter_eps$SYMBOL, quarter_eps$PREV_YDATE2), roll = T]$RAW]
  
  overlap_q_eps <- melt.data.table(quarter_eps[, .(SYMBOL, DATE, RAW, PREV_YRAW, PREV_YRAW2)], id.vars = c("SYMBOL", "DATE"))
  overlap_q_eps[, RK := seq(.N), by = .(SYMBOL, DATE)]
  setorder(overlap_q_eps, SYMBOL, DATE, -RK)
  
  growth_eps_beta <- overlap_q_eps[, .(BETA = ifelse(.N > 1, as.numeric(GCAMCQT:::lm_growth(value)), NA_real_)), by = .(SYMBOL, DATE)]
  growth_eps_beta[,SYMBOL:=as.character(SYMBOL)]
  growth_eps_beta[,SYMBOL:=sapply(SYMBOL,fill_code)]
  setkey(growth_eps_beta, SYMBOL, DATE)
  
  #normal
  univ <- stk_universe(start, end)
  univ[,SYMBOL := to_jydb_symbol(INNER_CODE)]
  univ[, SYMBOL := substr(SYMBOL, 1, 6)]
  univ[, RAW:=growth_eps_beta[J(univ$SYMBOL,univ$DATE),roll=T]$BETA]
  raw <- univ[, .(INNER_CODE, DATE, RAW)]
}

func_roa <- function(var, dates, start, end, save_folder, uqer_folder) {
  
  raw <- read_raw_data(start, end, c("NetProfitTTM","TotalAssets"), uqer_folder)
  raw <- dcast(raw,DATE+SYMBOL~variable,value.var="RAW",fun.aggregate=max)
  raw[,RAW:=NetProfitTTM/TotalAssets]
  roe <- raw[,.(SYMBOL,DATE,RAW)]
  
  roe[is.na(RAW), RAW := NA_real_]
  roe <- roe[!is.na(RAW)]
  roe[,SYMBOL:=as.character(SYMBOL)]
  roe[,SYMBOL:=sapply(SYMBOL,fill_code)]
  setkey(roe, SYMBOL, DATE)
  
  #normal
  univ <- stk_universe(start, end)
  univ[,SYMBOL := to_jydb_symbol(INNER_CODE)]
  univ[, SYMBOL := substr(SYMBOL, 1, 6)]
  univ[, RAW:=roe[J(univ$SYMBOL,univ$DATE),roll=T]$RAW]
  raw <- univ[, .(INNER_CODE, DATE, RAW)]
}

func_roa_growth_3y <- function(var, dates, start, end, save_folder, uqer_folder) {
  
  raw <- read_raw_data(start, end, c("EBIT","TotalAssets"), uqer_folder)
  raw <- dcast(raw,DATE+SYMBOL~variable,value.var="RAW",fun.aggregate=max)
  raw[,RAW:=EBIT/TotalAssets]
  roe <- raw[,.(SYMBOL,DATE,RAW)]
  
  roe[is.na(RAW), RAW := NA_real_]
  roe <- roe[!is.na(RAW)]
  
  quarter_roe <- roe[, .(DATE = head(DATE, 1)), by = .(SYMBOL, RAW)]
  quarter_roe <- quarter_roe[!is.na(RAW)]
  setkey(quarter_roe, SYMBOL, DATE)
  
  quarter_roe[, PREV_YDATE := DATE - 365.5]
  quarter_roe[, PREV_YDATE2 := DATE - 365.5*2]
  quarter_roe[, PREV_YRAW := quarter_roe[J(quarter_roe$SYMBOL, quarter_roe$PREV_YDATE), roll = T]$RAW]
  quarter_roe[, PREV_YRAW2 := quarter_roe[J(quarter_roe$SYMBOL, quarter_roe$PREV_YDATE2), roll = T]$RAW]
  
  overlap_q_roe <- melt.data.table(quarter_roe[, .(SYMBOL, DATE, RAW, PREV_YRAW, PREV_YRAW2)], id.vars = c("SYMBOL", "DATE"))
  overlap_q_roe[, RK := seq(.N), by = .(SYMBOL, DATE)]
  setorder(overlap_q_roe, SYMBOL, DATE, -RK)
  
  growth_roe_beta <- overlap_q_roe[, .(BETA = ifelse(.N > 1, as.numeric(GCAMCQT:::lm_growth(value)), NA_real_)), by = .(SYMBOL, DATE)]
  growth_roe_beta[,SYMBOL:=as.character(SYMBOL)]
  growth_roe_beta[,SYMBOL:=sapply(SYMBOL,fill_code)]
  setkey(growth_roe_beta, SYMBOL, DATE)
  
  #normal
  univ <- stk_universe(start, end)
  univ[,SYMBOL := to_jydb_symbol(INNER_CODE)]
  univ[, SYMBOL := substr(SYMBOL, 1, 6)]
  univ[, RAW:=growth_roe_beta[J(univ$SYMBOL,univ$DATE),roll=T]$BETA]
  raw <- univ[, .(INNER_CODE, DATE, RAW)]
}

func_roa_growth_12q <- function(var, dates, start, end, save_folder, uqer_folder) {
  
  raw <- read_raw_data(start, end, c("EBIT","TotalAssets"), uqer_folder)
  raw <- dcast(raw,DATE+SYMBOL~variable,value.var="RAW",fun.aggregate=max)
  raw[,RAW:=EBIT/TotalAssets]
  roe <- raw[,.(SYMBOL,DATE,RAW)]
  
  roe[is.na(RAW), RAW := NA_real_]
  roe <- roe[!is.na(RAW)]
  
  quarter_roe <- roe[, .(DATE = head(DATE, 1)), by = .(SYMBOL, RAW)]
  quarter_roe <- quarter_roe[!is.na(RAW)]
  setkey(quarter_roe, SYMBOL, DATE)
  quarter_roe[, QUARTER := seq(.N), by = SYMBOL]
  
  overlap_q_roe <- overlapping(quarter_roe[, .(SYMBOL, QUARTER)], quarter_roe, 12)
  
  growth_roe_beta <- overlap_q_roe[, .(DATE = tail(DATE, 1),
                                       BETA = ifelse(.N > 6, as.numeric(GCAMCQT:::lm_growth(RAW)), NA_real_)), by = .(SYMBOL, QUARTER)]
  growth_roe_beta[,SYMBOL:=as.character(SYMBOL)]
  growth_roe_beta[,SYMBOL:=sapply(SYMBOL,fill_code)]
  setkey(growth_roe_beta, SYMBOL, DATE)
  
  #normal
  univ <- stk_universe(start, end)
  univ[,SYMBOL := to_jydb_symbol(INNER_CODE)]
  univ[, SYMBOL := substr(SYMBOL, 1, 6)]
  univ[, RAW:=growth_roe_beta[J(univ$SYMBOL,univ$DATE),roll=T]$BETA]
  raw <- univ[, .(INNER_CODE, DATE, RAW)]
}

func_roa_growth_acce <- function(var, dates, start, end, save_folder, uqer_folder) {
  
  raw <- read_raw_data(start, end, c("EBIT","TotalAssets"), uqer_folder)
  raw <- dcast(raw,DATE+SYMBOL~variable,value.var="RAW",fun.aggregate=max)
  raw[,RAW:=EBIT/TotalAssets]
  roe <- raw[,.(SYMBOL,DATE,RAW)]
  
  roe[is.na(RAW), RAW := NA_real_]
  roe <- roe[!is.na(RAW)]
  roe <- roe[!RAW==Inf]
  
  quarter_roe <- roe[, .(DATE = head(DATE, 1)), by = .(SYMBOL, RAW)]
  quarter_roe <- quarter_roe[!is.na(RAW)]
  setkey(quarter_roe, SYMBOL, DATE)
  quarter_roe[, QUARTER := seq(.N), by = SYMBOL]
  
  overlap_q_roe <- overlapping(quarter_roe[, .(SYMBOL, QUARTER)], quarter_roe, 12)
  
  growth_roe_beta <- overlap_q_roe[, .(DATE = tail(DATE, 1),
                                       BETA = ifelse(.N > 6, as.numeric(lm_growth_acce(RAW)), NA_real_)), by = .(SYMBOL, QUARTER)]
  growth_roe_beta[,SYMBOL:=as.character(SYMBOL)]
  growth_roe_beta[,SYMBOL:=sapply(SYMBOL,fill_code)]
  setkey(growth_roe_beta, SYMBOL, DATE)
  
  #normal
  univ <- stk_universe(start, end)
  univ[,SYMBOL := to_jydb_symbol(INNER_CODE)]
  univ[, SYMBOL := substr(SYMBOL, 1, 6)]
  univ[, RAW:=growth_roe_beta[J(univ$SYMBOL,univ$DATE),roll=T]$BETA]
  raw <- univ[, .(INNER_CODE, DATE, RAW)]
}

func_workingcapital_growth_1y <- function(var, dates, start, end, save_folder, uqer_folder) {
  
  raw <- read_raw_data(start, end, c("WorkingCapital"), uqer_folder)
  workingcapital <- raw[,.(SYMBOL,DATE,RAW)]
  
  workingcapital[is.na(RAW), RAW := NA_real_]
  workingcapital <- workingcapital[!is.na(RAW)]
  
  quarter_wc <- workingcapital[, .(DATE = head(DATE, 1)), by = .(SYMBOL, RAW)]
  quarter_wc <- quarter_wc[!is.na(RAW)]
  setkey(quarter_wc, SYMBOL, DATE)
  
  quarter_wc[, PREV_YDATE := DATE - 365.5]
  quarter_wc[, PREV_YRAW := quarter_wc[J(quarter_wc$SYMBOL, quarter_wc$PREV_YDATE), roll = T]$RAW]
  quarter_wc[,ABS_PREV_YRAW:=abs(PREV_YRAW)]
  
  quarter_wc[,RATE:=(RAW-PREV_YRAW)/ABS_PREV_YRAW]
  quarter_wc <- quarter_wc[RATE!=Inf]
  
  quarter_wc[,SYMBOL:=as.character(SYMBOL)]
  quarter_wc[,SYMBOL:=sapply(SYMBOL,fill_code)]
  setkey(quarter_wc, SYMBOL, DATE)
  
  #normal
  univ <- stk_universe(start, end)
  univ[,SYMBOL := to_jydb_symbol(INNER_CODE)]
  univ[, SYMBOL := substr(SYMBOL, 1, 6)]
  univ[, RAW:=quarter_wc[J(univ$SYMBOL,univ$DATE),roll=T]$RATE]
  raw <- univ[, .(INNER_CODE, DATE, RAW)]
}

func_cash_ratio <- function(var, dates, start, end, save_folder, uqer_folder) {
  
  raw <- read_raw_data(start, end, c("CashEquivalentPS","NetAssetPS"), uqer_folder)
  raw <- dcast(raw,DATE+SYMBOL~variable,value.var="RAW",fun.aggregate=max)
  raw[,RAW:=CashEquivalentPS/NetAssetPS]
  cash_ratio <- raw[,.(SYMBOL,DATE,RAW)]
  
  cash_ratio[is.na(RAW), RAW := NA_real_]
  cash_ratio <- cash_ratio[!is.na(RAW)]
  cash_ratio <- cash_ratio[RAW!=Inf]
  
  quarter_cr <- cash_ratio[, .(DATE = head(DATE, 1)), by = .(SYMBOL, RAW)]
  quarter_cr <- cash_ratio[!is.na(RAW)]
  quarter_cr[,SYMBOL:=as.character(SYMBOL)]
  quarter_cr[,SYMBOL:=sapply(SYMBOL,fill_code)]
  setkey(quarter_cr, SYMBOL, DATE)
  
  #normal
  univ <- stk_universe(start, end)
  univ[,SYMBOL := to_jydb_symbol(INNER_CODE)]
  univ[, SYMBOL := substr(SYMBOL, 1, 6)]
  univ[, RAW:=quarter_cr[J(univ$SYMBOL,univ$DATE),roll=T]$RAW]
  raw <- univ[, .(INNER_CODE, DATE, RAW)]
}

func_revenue_chg <- function(var, dates, start, end, save_folder, uqer_folder) {
  
  raw <- read_raw_data(start, end, c("RevenueTTM","CostTTM"), uqer_folder)
  raw <- dcast(raw,DATE+SYMBOL~variable,value.var="RAW",fun.aggregate=max)
  
  raw[is.na(RevenueTTM), RevenueTTM := NA_real_]
  raw[is.na(CostTTM), CostTTM := NA_real_]
  raw <- raw[!(is.na(CostTTM)|is.na(RevenueTTM))]
  
  quarter_revenue_chg <- raw[, .(DATE = head(DATE, 1)), by = .(SYMBOL, RevenueTTM, CostTTM)]
  quarter_revenue_chg <- quarter_revenue_chg[!(is.na(CostTTM)|is.na(RevenueTTM))]
  quarter_revenue_chg[, QUARTER := seq(.N), by = SYMBOL]
  
  overlap_revenue_chg <- overlapping(quarter_revenue_chg[, .(SYMBOL, QUARTER)], quarter_revenue_chg, 6)
  
  revenue_chg_res <- overlap_revenue_chg[,.(DATE = tail(DATE, 1),
                                            RES = ifelse(.N == 6, as.numeric(lm_res(RevenueTTM,CostTTM)), NA_real_)),by=.(SYMBOL,QUARTER)]
  
  revenue_chg_res[,SYMBOL:=as.character(SYMBOL)]
  revenue_chg_res[,SYMBOL:=sapply(SYMBOL,fill_code)]
  setkey(revenue_chg_res, SYMBOL, DATE)
  
  #normal
  univ <- stk_universe(start, end)
  univ[,SYMBOL := to_jydb_symbol(INNER_CODE)]
  univ[, SYMBOL := substr(SYMBOL, 1, 6)]
  univ[, RAW:=revenue_chg_res[J(univ$SYMBOL,univ$DATE),roll=T]$RES]
  raw <- univ[, .(INNER_CODE, DATE, RAW)]
}

func_netprofitratio <- function(var, dates, start, end, save_folder, uqer_folder) {
  
  raw <- read_raw_data(start, end, c("NetProfitAPTTM","TEAP"), uqer_folder)
  raw <- dcast(raw,DATE+SYMBOL~variable,value.var="RAW",fun.aggregate=max)
  
  quarter_profitratio <- raw[, .(DATE = head(DATE, 1)), by = .(SYMBOL, TEAP, NetProfitAPTTM)]
  quarter_profitratio <- quarter_profitratio[!(is.na(TEAP)|is.na(NetProfitAPTTM))]
  setkey(quarter_profitratio, SYMBOL, DATE)
  
  quarter_profitratio[, PREV_TEAP := shift(TEAP), by=.(SYMBOL)]
  quarter_profitratio[,MTEAP:=(PREV_TEAP+TEAP)/2]
  quarter_profitratio <- quarter_profitratio[is.na(MTEAP),MTEAP:=TEAP]
  
  quarter_profitratio[,RAW:=NetProfitAPTTM/MTEAP]
  quarter_profitratio <- quarter_profitratio[RAW!=Inf]
  
  quarter_profitratio[,SYMBOL:=as.character(SYMBOL)]
  quarter_profitratio[,SYMBOL:=sapply(SYMBOL,fill_code)]
  setkey(quarter_profitratio, SYMBOL, DATE)
  
  #normal
  univ <- stk_universe(start, end)
  univ[,SYMBOL := to_jydb_symbol(INNER_CODE)]
  univ[, SYMBOL := substr(SYMBOL, 1, 6)]
  univ[, RAW:=quarter_profitratio[J(univ$SYMBOL,univ$DATE),roll=T]$RAW]
  raw <- univ[, .(INNER_CODE, DATE, RAW)]
}

func_totalasset_growth_5y <- function(var, dates, start, end, save_folder, uqer_folder) {
  
  raw <- read_raw_data(start, end, c("TotalAssets"), uqer_folder)
  raw <- raw[,.(SYMBOL,DATE,RAW)]
  
  raw[is.na(RAW), RAW := NA_real_]
  raw <- raw[!is.na(RAW)]
  
  quarter_raw <- raw[, .(DATE = head(DATE, 1)), by = .(SYMBOL, RAW)]
  quarter_raw <- quarter_raw[!is.na(RAW)]
  setkey(quarter_raw, SYMBOL, DATE)
  
  quarter_raw[, PREV_5YDATE := DATE - 365.5 * 5]
  quarter_raw[, PREV_YRAW := quarter_raw[J(quarter_raw$SYMBOL, quarter_raw$PREV_5YDATE), roll = T]$RAW]
  
  quarter_raw[,RATE:=RAW/PREV_YRAW-1]
  quarter_raw <- quarter_raw[RATE!=Inf]
  
  quarter_raw[,SYMBOL:=as.character(SYMBOL)]
  quarter_raw[,SYMBOL:=sapply(SYMBOL,fill_code)]
  setkey(quarter_raw, SYMBOL, DATE)
  
  #normal
  univ <- stk_universe(start, end)
  univ[,SYMBOL := to_jydb_symbol(INNER_CODE)]
  univ[, SYMBOL := substr(SYMBOL, 1, 6)]
  univ[, RAW:=quarter_raw[J(univ$SYMBOL,univ$DATE),roll=T]$RATE]
  raw <- univ[, .(INNER_CODE, DATE, RAW)]
}

func_revenue_growth_1y <- function(var, dates, start, end, save_folder, uqer_folder) {
  
  raw <- read_raw_data(start, end, c("RevenueTTM"), uqer_folder)
  revenue <- raw[,.(SYMBOL,DATE,RAW)]
  
  revenue[is.na(RAW), RAW := NA_real_]
  revenue <- revenue[!is.na(RAW)]
  
  quarter_rev <- revenue[, .(DATE = head(DATE, 1)), by = .(SYMBOL, RAW)]
  quarter_rev <- quarter_rev[!is.na(RAW)]
  setkey(quarter_rev, SYMBOL, DATE)
  
  quarter_rev[, PREV_YDATE := DATE - 365.5]
  quarter_rev[, PREV_YRAW := quarter_rev[J(quarter_rev$SYMBOL, quarter_rev$PREV_YDATE), roll = T]$RAW]
  quarter_rev[,ABS_PREV_YRAW:=abs(PREV_YRAW)]
  
  quarter_rev[,RATE:=(RAW-PREV_YRAW)/ABS_PREV_YRAW]
  quarter_rev <- quarter_rev[RATE!=Inf]
  
  quarter_rev[,SYMBOL:=as.character(SYMBOL)]
  quarter_rev[,SYMBOL:=sapply(SYMBOL,fill_code)]
  setkey(quarter_rev, SYMBOL, DATE)
  
  #normal
  univ <- stk_universe(start, end)
  univ[,SYMBOL := to_jydb_symbol(INNER_CODE)]
  univ[, SYMBOL := substr(SYMBOL, 1, 6)]
  univ[, RAW:=quarter_rev[J(univ$SYMBOL,univ$DATE),roll=T]$RATE]
  raw <- univ[, .(INNER_CODE, DATE, RAW)]
}

func_stockholder_ep <- function(var, dates, start, end, save_folder, uqer_folder) {
  
  raw <- read_raw_data(start, end, c("NetProfitTTM","AssetImpairLossTTM"), uqer_folder)
  raw <- dcast(raw,DATE+SYMBOL~variable,value.var="RAW",fun.aggregate=max)
  raw[,RAW:=NetProfitTTM+AssetImpairLossTTM]
  profit <- raw[,.(SYMBOL,DATE,RAW)]
  
  profit[is.na(RAW), RAW := NA_real_]
  profit <- profit[!is.na(RAW)]
  
  quarter_rev <- profit[, .(DATE = head(DATE, 1)), by = .(SYMBOL, RAW)]
  quarter_rev <- quarter_rev[!is.na(RAW)]
  
  quarter_rev[,SYMBOL:=as.character(SYMBOL)]
  quarter_rev[,SYMBOL:=sapply(SYMBOL,fill_code)]
  setkey(quarter_rev, SYMBOL, DATE)
  
  #normal
  univ <- stk_universe(start, end)
  univ[,SYMBOL := to_jydb_symbol(INNER_CODE)]
  univ[, SYMBOL := substr(SYMBOL, 1, 6)]
  univ <- stk_mv(universe=univ)
  univ[, PROFIT:=quarter_rev[J(univ$SYMBOL,univ$DATE),roll=T]$RAW]
  univ[, RAW:=PROFIT/MV]
  raw <- univ[, .(INNER_CODE, DATE, RAW)]
}

func_bp <- function(var, dates, start, end, save_folder, uqer_folder) {
  
  raw <- read_raw_data(start, end, c("TEAP"), uqer_folder)
  book <- raw[,.(SYMBOL,DATE,RAW)]
  
  book[is.na(RAW), RAW := NA_real_]
  book <- book[!is.na(RAW)]
  
  quarter_book <- book[, .(DATE = head(DATE, 1)), by = .(SYMBOL, RAW)]
  quarter_book <- quarter_book[!is.na(RAW)]
  
  quarter_book[,SYMBOL:=as.character(SYMBOL)]
  quarter_book[,SYMBOL:=sapply(SYMBOL,fill_code)]
  setkey(quarter_book, SYMBOL, DATE)
  
  #normal
  univ <- stk_universe(start, end)
  univ[,SYMBOL := to_jydb_symbol(INNER_CODE)]
  univ[, SYMBOL := substr(SYMBOL, 1, 6)]
  univ <- stk_mv(universe=univ)
  univ[, BOOK:=quarter_book[J(univ$SYMBOL,univ$DATE),roll=T]$RAW]
  univ[, RAW:=BOOK/MV]
  raw <- univ[, .(INNER_CODE, DATE, RAW)]
}

func_teap_ratio <- function(var, dates, start, end, save_folder, uqer_folder) {
  
  raw <- read_raw_data(start, end, c("TotalAssets","TEAP"), uqer_folder)
  raw <- dcast(raw,DATE+SYMBOL~variable,value.var="RAW",fun.aggregate=max)
  
  quarter_ratio <- raw[, .(DATE = head(DATE, 1)), by = .(SYMBOL, TEAP, TotalAssets)]
  quarter_ratio <- quarter_ratio[!(is.na(TEAP)|is.na(TotalAssets))]
  setkey(quarter_ratio, SYMBOL, DATE)
  
  quarter_ratio[,RAW:=TEAP/TotalAssets]
  quarter_ratio <- quarter_ratio[RAW!=Inf]
  
  quarter_ratio[,SYMBOL:=as.character(SYMBOL)]
  quarter_ratio[,SYMBOL:=sapply(SYMBOL,fill_code)]
  setkey(quarter_ratio, SYMBOL, DATE)
  
  #normal
  univ <- stk_universe(start, end)
  univ[,SYMBOL := to_jydb_symbol(INNER_CODE)]
  univ[, SYMBOL := substr(SYMBOL, 1, 6)]
  univ[, RAW:=quarter_ratio[J(univ$SYMBOL,univ$DATE),roll=T]$RAW]
  raw <- univ[, .(INNER_CODE, DATE, RAW)]
}

func_netprofit_acce <- function(var, dates, start, end, save_folder, uqer_folder) {
  
  raw <- read_raw_data(start, end, c("NetProfitTTM"), uqer_folder)
  profit <- raw[,.(SYMBOL,DATE,RAW)]
  
  profit[is.na(RAW), RAW := NA_real_]
  profit <- profit[!is.na(RAW)]

  
  quarter_profit <- profit[, .(DATE = head(DATE, 1)), by = .(SYMBOL, RAW)]
  quarter_profit <- quarter_profit[!is.na(RAW)]
  setkey(quarter_profit, SYMBOL, DATE)
  quarter_profit[, QUARTER := seq(.N), by = SYMBOL]
  
  overlap_q_profit <- overlapping(quarter_profit[, .(SYMBOL, QUARTER)], quarter_profit, 12)
  
  growth_profit_beta <- overlap_q_profit[, .(DATE = tail(DATE, 1),
                                       BETA = ifelse(.N > 6, as.numeric(lm_growth_acce(RAW)), NA_real_)), by = .(SYMBOL, QUARTER)]
  growth_profit_beta[,SYMBOL:=as.character(SYMBOL)]
  growth_profit_beta[,SYMBOL:=sapply(SYMBOL,fill_code)]
  setkey(growth_profit_beta, SYMBOL, DATE)
  
  #normal
  univ <- stk_universe(start, end)
  univ[,SYMBOL := to_jydb_symbol(INNER_CODE)]
  univ[, SYMBOL := substr(SYMBOL, 1, 6)]
  univ[, RAW:=growth_profit_beta[J(univ$SYMBOL,univ$DATE),roll=T]$BETA]
  raw <- univ[, .(INNER_CODE, DATE, RAW)]
}

func_cashdebt_ratio <- function(var, dates, start, end, save_folder, uqer_folder) {
  
  raw <- read_raw_data(start, end, c("NetOperateCFTTM","IntFreeCL","IntCL"), uqer_folder)
  raw <- dcast(raw,DATE+SYMBOL~variable,value.var="RAW",fun.aggregate=max)
  
  quarter_ratio <- raw[, .(DATE = head(DATE, 1)), by = .(SYMBOL, NetOperateCFTTM, IntFreeCL, IntCL)]
  quarter_ratio <- quarter_ratio[!(is.na(NetOperateCFTTM)|is.na(IntFreeCL)|is.na(IntCL))]
  setkey(quarter_ratio, SYMBOL, DATE)
  
  quarter_ratio[,CL:=IntFreeCL+IntCL]#流动负债总计
  quarter_ratio[, PREV_CL := shift(CL), by=.(SYMBOL)]
  quarter_ratio[,MCL:=(PREV_CL+CL)/2]
  quarter_ratio <- quarter_ratio[is.na(MCL),MCL:=CL]
  
  quarter_ratio[,RAW:=NetOperateCFTTM/MCL]
  quarter_ratio <- quarter_ratio[RAW!=Inf]
  
  quarter_ratio[,SYMBOL:=as.character(SYMBOL)]
  quarter_ratio[,SYMBOL:=sapply(SYMBOL,fill_code)]
  setkey(quarter_ratio, SYMBOL, DATE)
  
  #normal
  univ <- stk_universe(start, end)
  univ[,SYMBOL := to_jydb_symbol(INNER_CODE)]
  univ[, SYMBOL := substr(SYMBOL, 1, 6)]
  univ[, RAW:=quarter_ratio[J(univ$SYMBOL,univ$DATE),roll=T]$RAW]
  raw <- univ[, .(INNER_CODE, DATE, RAW)]
}

func_revenue_growth_1y <- function(var, dates, start, end, save_folder, uqer_folder) {
  
  raw <- read_raw_data(start, end, c("RevenueTTM"), uqer_folder)
  revenue <- raw[,.(SYMBOL,DATE,RAW)]
  
  revenue[is.na(RAW), RAW := NA_real_]
  revenue <- revenue[!is.na(RAW)]
  
  quarter_revenue <- revenue[, .(DATE = head(DATE, 1)), by = .(SYMBOL, RAW)]
  quarter_revenue <- quarter_revenue[!is.na(RAW)]
  setkey(quarter_revenue, SYMBOL, DATE)
  
  quarter_revenue[, PREV_YDATE := DATE - 365.5]
  quarter_revenue[, PREV_YRAW := quarter_revenue[J(quarter_revenue$SYMBOL, quarter_revenue$PREV_YDATE), roll = T]$RAW]
  quarter_revenue[,ABS_PREV_YRAW:=abs(PREV_YRAW)]
  
  quarter_revenue[,RATE:=(RAW-PREV_YRAW)/ABS_PREV_YRAW]
  quarter_revenue <- quarter_revenue[RATE!=Inf]
  
  quarter_revenue[,SYMBOL:=as.character(SYMBOL)]
  quarter_revenue[,SYMBOL:=sapply(SYMBOL,fill_code)]
  setkey(quarter_revenue, SYMBOL, DATE)
  
  #normal
  univ <- stk_universe(start, end)
  univ[,SYMBOL := to_jydb_symbol(INNER_CODE)]
  univ[, SYMBOL := substr(SYMBOL, 1, 6)]
  univ[, RAW:=quarter_revenue[J(univ$SYMBOL,univ$DATE),roll=T]$RATE]
  raw <- univ[, .(INNER_CODE, DATE, RAW)]
}

func_asset_growth <- function(var, dates, start, end, save_folder, uqer_folder) {
  
  raw <- read_raw_data(start, end, c("TotalAssets"), uqer_folder)
  asset <- raw[,.(SYMBOL,DATE,RAW)]
  
  asset[is.na(RAW), RAW := NA_real_]
  asset <- asset[!is.na(RAW)]
  
  quarter_asset <- asset[, .(DATE = head(DATE, 1)), by = .(SYMBOL, RAW)]
  quarter_asset <- quarter_asset[!is.na(RAW)]
  setkey(quarter_asset, SYMBOL, DATE)
  
  quarter_asset[, PREV_RAW := shift(RAW), by=.(SYMBOL)]
  
  quarter_asset[,RATE:=(RAW-PREV_RAW)/PREV_RAW]
  quarter_asset <- quarter_asset[RATE!=Inf]
  
  quarter_asset[,SYMBOL:=as.character(SYMBOL)]
  quarter_asset[,SYMBOL:=sapply(SYMBOL,fill_code)]
  setkey(quarter_asset, SYMBOL, DATE)
  
  #normal
  univ <- stk_universe(start, end)
  univ[,SYMBOL := to_jydb_symbol(INNER_CODE)]
  univ[, SYMBOL := substr(SYMBOL, 1, 6)]
  univ[, RAW:=quarter_asset[J(univ$SYMBOL,univ$DATE),roll=T]$RATE]
  raw <- univ[, .(INNER_CODE, DATE, RAW)]
}

func_purify_revenue <- function(var, dates, start, end, save_folder, uqer_folder) {
  
  raw <- read_raw_data(start, end, c("NetProfitTTM","NonOperatingNPTTM","AdminExpenseTTM"), uqer_folder)
  raw <- dcast(raw,DATE+SYMBOL~variable,value.var="RAW",fun.aggregate=max)
  
  raw[is.na(NetProfitTTM), NetProfitTTM := NA_real_]
  raw[is.na(NonOperatingNPTTM), NonOperatingNPTTM := NA_real_]
  raw[is.na(AdminExpenseTTM), AdminExpenseTTM := NA_real_]
  raw <- raw[!(is.na(NetProfitTTM)|is.na(AdminExpenseTTM)|is.na(NonOperatingNPTTM))]
  
  quarter_revenue_chg <- raw[, .(DATE = head(DATE, 1)), by = .(SYMBOL, NetProfitTTM, NonOperatingNPTTM, AdminExpenseTTM)]
  quarter_revenue_chg <- quarter_revenue_chg[!(is.na(NetProfitTTM)|is.na(AdminExpenseTTM)|is.na(NonOperatingNPTTM))]
  quarter_revenue_chg[, QUARTER := seq(.N), by = SYMBOL]
  
  overlap_revenue_chg <- overlapping(quarter_revenue_chg[, .(SYMBOL, QUARTER)], quarter_revenue_chg, 8)
  
  revenue_chg_res <- overlap_revenue_chg[,.(DATE = tail(DATE, 1),
                                            RES = ifelse(.N == 8, as.numeric(lm_res(NetProfitTTM,NonOperatingNPTTM, AdminExpenseTTM)), NA_real_)),
                                         by=.(SYMBOL,QUARTER)]
  
  revenue_chg_res[,SYMBOL:=as.character(SYMBOL)]
  revenue_chg_res[,SYMBOL:=sapply(SYMBOL,fill_code)]
  setkey(revenue_chg_res, SYMBOL, DATE)
  
  #normal
  univ <- stk_universe(start, end)
  univ[,SYMBOL := to_jydb_symbol(INNER_CODE)]
  univ[, SYMBOL := substr(SYMBOL, 1, 6)]
  univ[, RAW:=revenue_chg_res[J(univ$SYMBOL,univ$DATE),roll=T]$RES]
  raw <- univ[, .(INNER_CODE, DATE, RAW)]
}

func_NIAPCut_ratio <- function(var, dates, start, end, save_folder, uqer_folder) {
  
  raw <- read_raw_data(start, end, c("NIAPCut","TEAP"), uqer_folder)
  raw <- dcast(raw,DATE+SYMBOL~variable,value.var="RAW",fun.aggregate=max)
  
  quarter_ratio <- raw[, .(DATE = head(DATE, 1)), by = .(SYMBOL, NIAPCut, TEAP)]
  quarter_ratio <- quarter_ratio[!(is.na(NIAPCut)|is.na(TEAP))]
  setkey(quarter_ratio, SYMBOL, DATE)
  
  quarter_ratio[, PREV_TEAP := shift(TEAP), by=.(SYMBOL)]
  quarter_ratio[,MTEAP:=(PREV_TEAP+TEAP)/2]
  quarter_ratio[is.na(MTEAP),MTEAP:=TEAP]
  
  quarter_ratio[,RAW:=NIAPCut/MTEAP]
  quarter_ratio <- quarter_ratio[RAW!=Inf]
  
  quarter_ratio[,SYMBOL:=as.character(SYMBOL)]
  quarter_ratio[,SYMBOL:=sapply(SYMBOL,fill_code)]
  setkey(quarter_ratio, SYMBOL, DATE)
  
  #normal
  univ <- stk_universe(start, end)
  univ[,SYMBOL := to_jydb_symbol(INNER_CODE)]
  univ[, SYMBOL := substr(SYMBOL, 1, 6)]
  univ[, RAW:=quarter_ratio[J(univ$SYMBOL,univ$DATE),roll=T]$RAW]
  raw <- univ[, .(INNER_CODE, DATE, RAW)]
}

func_interest_cover <- function(var, dates, start, end, save_folder, uqer_folder) {
  
  raw <- read_raw_data(start, end, c("EBIT","FinanExpenseTTM"), uqer_folder)
  raw <- dcast(raw,DATE+SYMBOL~variable,value.var="RAW",fun.aggregate=max)
  
  quarter_ratio <- raw[, .(DATE = head(DATE, 1)), by = .(SYMBOL, EBIT, FinanExpenseTTM)]
  quarter_ratio <- quarter_ratio[!(is.na(EBIT)|is.na(FinanExpenseTTM))]
  setkey(quarter_ratio, SYMBOL, DATE)
  
  quarter_ratio[,RAW:=EBIT/FinanExpenseTTM]
  quarter_ratio <- quarter_ratio[RAW!=Inf]
  
  quarter_ratio[,SYMBOL:=as.character(SYMBOL)]
  quarter_ratio[,SYMBOL:=sapply(SYMBOL,fill_code)]
  setkey(quarter_ratio, SYMBOL, DATE)
  
  #normal
  univ <- stk_universe(start, end)
  univ[,SYMBOL := to_jydb_symbol(INNER_CODE)]
  univ[, SYMBOL := substr(SYMBOL, 1, 6)]
  univ[, RAW:=quarter_ratio[J(univ$SYMBOL,univ$DATE),roll=T]$RAW]
  raw <- univ[, .(INNER_CODE, DATE, RAW)]
}

func_equity_growth_5y <- function(var, dates, start, end, save_folder, uqer_folder) {
  
  raw <- read_raw_data(start, end, c("TEAP"), uqer_folder)
  revenue <- raw[,.(SYMBOL,DATE,RAW)]
  
  revenue[is.na(RAW), RAW := NA_real_]
  revenue <- revenue[!is.na(RAW)]
  
  quarter_revenue <- revenue[, .(DATE = head(DATE, 1)), by = .(SYMBOL, RAW)]
  quarter_revenue <- quarter_revenue[!is.na(RAW)]
  setkey(quarter_revenue, SYMBOL, DATE)
  
  quarter_revenue[, PREV_YDATE := DATE - 365.5 * 5]
  quarter_revenue[, PREV_YRAW := quarter_revenue[J(quarter_revenue$SYMBOL, quarter_revenue$PREV_YDATE), roll = T]$RAW]
  quarter_revenue[,ABS_PREV_YRAW:=abs(PREV_YRAW)]
  
  quarter_revenue[,RATE:=(RAW-PREV_YRAW)/ABS_PREV_YRAW]
  quarter_revenue <- quarter_revenue[RATE!=Inf]
  
  quarter_revenue[,SYMBOL:=as.character(SYMBOL)]
  quarter_revenue[,SYMBOL:=sapply(SYMBOL,fill_code)]
  setkey(quarter_revenue, SYMBOL, DATE)
  
  #normal
  univ <- stk_universe(start, end)
  univ[,SYMBOL := to_jydb_symbol(INNER_CODE)]
  univ[, SYMBOL := substr(SYMBOL, 1, 6)]
  univ[, RAW:=quarter_revenue[J(univ$SYMBOL,univ$DATE),roll=T]$RATE]
  raw <- univ[, .(INNER_CODE, DATE, RAW)]
}

func_production_improve <- function(var, dates, start, end, save_folder, uqer_folder) {
  
  raw <- read_raw_data(start, end, c("TCostTTM","TotalFixedAssets"), uqer_folder)
  raw <- dcast(raw,DATE+SYMBOL~variable,value.var="RAW",fun.aggregate=max)
  
  raw[is.na(TCostTTM), TCostTTM := NA_real_]
  raw[is.na(TotalFixedAssets), TotalFixedAssets := NA_real_]
  raw <- raw[!(is.na(TCostTTM)|is.na(TotalFixedAssets))]
  
  quarter_revenue_chg <- raw[, .(DATE = head(DATE, 1)), by = .(SYMBOL, TCostTTM, TotalFixedAssets)]
  quarter_revenue_chg <- quarter_revenue_chg[!(is.na(TCostTTM)|is.na(TotalFixedAssets))]
  quarter_revenue_chg[, QUARTER := seq(.N), by = SYMBOL]
  
  overlap_revenue_chg <- overlapping(quarter_revenue_chg[, .(SYMBOL, QUARTER)], quarter_revenue_chg, 8)
  
  revenue_chg_res <- overlap_revenue_chg[,.(DATE = tail(DATE, 1),
                                            RES = ifelse(.N == 8, as.numeric(lm_res(TCostTTM,TotalFixedAssets)), NA_real_)),
                                         by=.(SYMBOL,QUARTER)]
  
  revenue_chg_res[,SYMBOL:=as.character(SYMBOL)]
  revenue_chg_res[,SYMBOL:=sapply(SYMBOL,fill_code)]
  setkey(revenue_chg_res, SYMBOL, DATE)
  
  #normal
  univ <- stk_universe(start, end)
  univ[,SYMBOL := to_jydb_symbol(INNER_CODE)]
  univ[, SYMBOL := substr(SYMBOL, 1, 6)]
  univ[, RAW:=revenue_chg_res[J(univ$SYMBOL,univ$DATE),roll=T]$RES]
  raw <- univ[, .(INNER_CODE, DATE, RAW)]
}

func_netprofit_growth_1y <- function(var, dates, start, end, save_folder, uqer_folder) {
  
  raw <- read_raw_data(start, end, c("NetProfitTTM"), uqer_folder)
  revenue <- raw[,.(SYMBOL,DATE,RAW)]
  
  revenue[is.na(RAW), RAW := NA_real_]
  revenue <- revenue[!is.na(RAW)]
  
  quarter_rev <- revenue[, .(DATE = head(DATE, 1)), by = .(SYMBOL, RAW)]
  quarter_rev <- quarter_rev[!is.na(RAW)]
  setkey(quarter_rev, SYMBOL, DATE)
  
  quarter_rev[, PREV_YDATE := DATE - 365.5]
  quarter_rev[, PREV_YRAW := quarter_rev[J(quarter_rev$SYMBOL, quarter_rev$PREV_YDATE), roll = T]$RAW]
  quarter_rev[,ABS_PREV_YRAW:=abs(PREV_YRAW)]
  
  quarter_rev[,RATE:=(RAW-PREV_YRAW)/ABS_PREV_YRAW]
  quarter_rev <- quarter_rev[RATE!=Inf]
  
  quarter_rev[,SYMBOL:=as.character(SYMBOL)]
  quarter_rev[,SYMBOL:=sapply(SYMBOL,fill_code)]
  setkey(quarter_rev, SYMBOL, DATE)
  
  #normal
  univ <- stk_universe(start, end)
  univ[,SYMBOL := to_jydb_symbol(INNER_CODE)]
  univ[, SYMBOL := substr(SYMBOL, 1, 6)]
  univ[, RAW:=quarter_rev[J(univ$SYMBOL,univ$DATE),roll=T]$RATE]
  raw <- univ[, .(INNER_CODE, DATE, RAW)]
}

func_profitap_ep <- function(var, dates, start, end, save_folder, uqer_folder) {
  #归母净利润比市值
  raw <- read_raw_data(start, end, c("NetProfitAPTTM"), uqer_folder)
  profit <- raw[,.(SYMBOL,DATE,RAW)]
  
  profit[is.na(RAW), RAW := NA_real_]
  profit <- profit[!is.na(RAW)]
  
  quarter_rev <- profit[, .(DATE = head(DATE, 1)), by = .(SYMBOL, RAW)]
  quarter_rev <- quarter_rev[!is.na(RAW)]
  
  quarter_rev[,SYMBOL:=as.character(SYMBOL)]
  quarter_rev[,SYMBOL:=sapply(SYMBOL,fill_code)]
  setkey(quarter_rev, SYMBOL, DATE)
  
  #normal
  univ <- stk_universe(start, end)
  univ[,SYMBOL := to_jydb_symbol(INNER_CODE)]
  univ[, SYMBOL := substr(SYMBOL, 1, 6)]
  univ <- stk_mv(universe=univ)
  univ[, PROFIT:=quarter_rev[J(univ$SYMBOL,univ$DATE),roll=T]$RAW]
  univ[, RAW:=PROFIT/MV]
  raw <- univ[, .(INNER_CODE, DATE, RAW)]
}

func_roe_equity <- function(var, dates, start, end, save_folder, uqer_folder) {
  
  raw <- read_raw_data(start, end, c("NIAP","TEAP"), uqer_folder)
  raw <- dcast(raw,DATE+SYMBOL~variable,value.var="RAW",fun.aggregate=max)
  raw[,ROE:=NIAP/TEAP]
  
  raw <- raw[!(is.na(ROE)|is.na(TEAP)|is.na(NIAP))]
  
  quarter_rev <- raw[, .(DATE = head(DATE, 1)), by = .(SYMBOL, ROE, TEAP)]
  quarter_rev <- quarter_rev[!(is.na(ROE)|is.na(TEAP))]
  setkey(quarter_rev, SYMBOL, DATE)
  
  quarter_rev[, PREV_YDATE:= DATE - 365.5]
  quarter_rev[, PREV_TEAP:=quarter_rev[J(quarter_rev$SYMBOL, quarter_rev$PREV_YDATE), roll = T]$TEAP]
  quarter_rev[, PREV_ROE:=shift(ROE),by=.(SYMBOL)]
  
  quarter_rev[, `:=`(ROE_CHG=(ROE-PREV_ROE)/abs(PREV_ROE),TEAP_CHG=(TEAP-PREV_TEAP)/PREV_TEAP)]
  quarter_rev[,RAW:=ROE_CHG-TEAP_CHG]
  
  quarter_rev[,SYMBOL:=as.character(SYMBOL)]
  quarter_rev[,SYMBOL:=sapply(SYMBOL,fill_code)]
  setkey(quarter_rev, SYMBOL, DATE)
  
  #normal
  univ <- stk_universe(start, end)
  univ[,SYMBOL := to_jydb_symbol(INNER_CODE)]
  univ[, SYMBOL := substr(SYMBOL, 1, 6)]
  univ[, RAW:=quarter_rev[J(univ$SYMBOL,univ$DATE),roll=T]$RAW]
  raw <- univ[, .(INNER_CODE, DATE, RAW)]
}

func_asset_turnover <- function(var, dates, start, end, save_folder, uqer_folder) {
  
  raw <- read_raw_data(start, end, c("RevenueTTM","TotalAssets"), uqer_folder)
  raw <- dcast(raw,DATE+SYMBOL~variable,value.var="RAW",fun.aggregate=max)
  
  raw <- raw[!(is.na(TotalAssets)|is.na(RevenueTTM))]
  
  quarter_cr <- raw[, .(DATE = head(DATE, 1)), by = .(SYMBOL, TotalAssets, RevenueTTM)]
  quarter_cr[, PREV_ASSET:=shift(TotalAssets),by=.(SYMBOL)]
  quarter_cr[,MASSET:=(TotalAssets+PREV_ASSET)/2]
  quarter_cr[is.na(MASSET),MASSET:=TotalAssets]
  quarter_cr[,RAW:=RevenueTTM/MASSET]
  
  quarter_cr[,SYMBOL:=as.character(SYMBOL)]
  quarter_cr[,SYMBOL:=sapply(SYMBOL,fill_code)]
  setkey(quarter_cr, SYMBOL, DATE)
  
  #normal
  univ <- stk_universe(start, end)
  univ[,SYMBOL := to_jydb_symbol(INNER_CODE)]
  univ[, SYMBOL := substr(SYMBOL, 1, 6)]
  univ[, RAW:=quarter_cr[J(univ$SYMBOL,univ$DATE),roll=T]$RAW]
  raw <- univ[, .(INNER_CODE, DATE, RAW)]
}

func_profitap_growth_1y <- function(var, dates, start, end, save_folder, uqer_folder) {
  
  raw <- read_raw_data(start, end, c("NIAP"), uqer_folder)
  revenue <- raw[,.(SYMBOL,DATE,RAW)]
  
  revenue[is.na(RAW), RAW := NA_real_]
  revenue <- revenue[!is.na(RAW)]
  
  quarter_rev <- revenue[, .(DATE = head(DATE, 1)), by = .(SYMBOL, RAW)]
  quarter_rev <- quarter_rev[!is.na(RAW)]
  setkey(quarter_rev, SYMBOL, DATE)
  
  quarter_rev[, PREV_YDATE := DATE - 365.5]
  quarter_rev[, PREV_YRAW := quarter_rev[J(quarter_rev$SYMBOL, quarter_rev$PREV_YDATE), roll = T]$RAW]
  quarter_rev[,ABS_PREV_YRAW:=abs(PREV_YRAW)]
  
  quarter_rev[,RATE:=(RAW-PREV_YRAW)/ABS_PREV_YRAW]
  quarter_rev <- quarter_rev[RATE!=Inf]
  
  quarter_rev[,SYMBOL:=as.character(SYMBOL)]
  quarter_rev[,SYMBOL:=sapply(SYMBOL,fill_code)]
  setkey(quarter_rev, SYMBOL, DATE)
  
  #normal
  univ <- stk_universe(start, end)
  univ[,SYMBOL := to_jydb_symbol(INNER_CODE)]
  univ[, SYMBOL := substr(SYMBOL, 1, 6)]
  univ[, RAW:=quarter_rev[J(univ$SYMBOL,univ$DATE),roll=T]$RATE]
  raw <- univ[, .(INNER_CODE, DATE, RAW)]
}

func_operatecash_ratio <- function(var, dates, start, end, save_folder, uqer_folder) {
  
  raw <- read_raw_data(start, end, c("NetOperateCFTTM","TotalAssets","TEAP"), uqer_folder)
  raw <- dcast(raw,DATE+SYMBOL~variable,value.var="RAW",fun.aggregate=max)
  raw[,LIABILITY:=TotalAssets-TEAP]
  raw <- raw[!(is.na(LIABILITY)|is.na(NetOperateCFTTM))]
  
  quarter_cr <- raw[, .(DATE = head(DATE, 1)), by = .(SYMBOL, LIABILITY, NetOperateCFTTM)]
  quarter_cr[, PREV_LIABILITY:=shift(LIABILITY),by=.(SYMBOL)]
  quarter_cr[,MLIABILITY:=(LIABILITY+PREV_LIABILITY)/2]
  quarter_cr[is.na(MLIABILITY),MLIABILITY:=LIABILITY]
  quarter_cr[,RAW:=NetOperateCFTTM/MLIABILITY]
  
  quarter_cr[,SYMBOL:=as.character(SYMBOL)]
  quarter_cr[,SYMBOL:=sapply(SYMBOL,fill_code)]
  setkey(quarter_cr, SYMBOL, DATE)
  
  #normal
  univ <- stk_universe(start, end)
  univ[,SYMBOL := to_jydb_symbol(INNER_CODE)]
  univ[, SYMBOL := substr(SYMBOL, 1, 6)]
  univ[, RAW:=quarter_cr[J(univ$SYMBOL,univ$DATE),roll=T]$RAW]
  raw <- univ[, .(INNER_CODE, DATE, RAW)]
}

func_operatingprofit_growth_1y <- function(var, dates, start, end, save_folder, uqer_folder) {
  
  raw <- read_raw_data(start, end, c("OperateProfitTTM"), uqer_folder)
  revenue <- raw[,.(SYMBOL,DATE,RAW)]
  
  revenue[is.na(RAW), RAW := NA_real_]
  revenue <- revenue[!is.na(RAW)]
  
  quarter_rev <- revenue[, .(DATE = head(DATE, 1)), by = .(SYMBOL, RAW)]
  quarter_rev <- quarter_rev[!is.na(RAW)]
  setkey(quarter_rev, SYMBOL, DATE)
  
  quarter_rev[, PREV_YDATE := DATE - 365.5]
  quarter_rev[, PREV_YRAW := quarter_rev[J(quarter_rev$SYMBOL, quarter_rev$PREV_YDATE), roll = T]$RAW]
  quarter_rev[,ABS_PREV_YRAW:=abs(PREV_YRAW)]
  
  quarter_rev[,RATE:=(RAW-PREV_YRAW)/ABS_PREV_YRAW]
  quarter_rev <- quarter_rev[RATE!=Inf]
  
  quarter_rev[,SYMBOL:=as.character(SYMBOL)]
  quarter_rev[,SYMBOL:=sapply(SYMBOL,fill_code)]
  setkey(quarter_rev, SYMBOL, DATE)
  
  #normal
  univ <- stk_universe(start, end)
  univ[,SYMBOL := to_jydb_symbol(INNER_CODE)]
  univ[, SYMBOL := substr(SYMBOL, 1, 6)]
  univ[, RAW:=quarter_rev[J(univ$SYMBOL,univ$DATE),roll=T]$RATE]
  raw <- univ[, .(INNER_CODE, DATE, RAW)]
}

func_netprofit_growth_1q <- function(var, dates, start, end, save_folder, uqer_folder) {
  
  raw <- read_raw_data(start, end, c("NetProfitTTM"), uqer_folder)
  revenue <- raw[,.(SYMBOL,DATE,RAW)]
  
  revenue[is.na(RAW), RAW := NA_real_]
  revenue <- revenue[!is.na(RAW)]
  
  quarter_rev <- revenue[, .(DATE = head(DATE, 1)), by = .(SYMBOL, RAW)]
  quarter_rev <- quarter_rev[!is.na(RAW)]
  setkey(quarter_rev, SYMBOL, DATE)
  
  quarter_rev[, PREV_YRAW := shift(RAW), by=.(SYMBOL)]
  quarter_rev[,ABS_PREV_YRAW:=abs(PREV_YRAW)]
  
  quarter_rev[,RATE:=(RAW-PREV_YRAW)/ABS_PREV_YRAW]
  quarter_rev <- quarter_rev[RATE!=Inf]
  
  quarter_rev[,SYMBOL:=as.character(SYMBOL)]
  quarter_rev[,SYMBOL:=sapply(SYMBOL,fill_code)]
  setkey(quarter_rev, SYMBOL, DATE)
  
  #normal
  univ <- stk_universe(start, end)
  univ[,SYMBOL := to_jydb_symbol(INNER_CODE)]
  univ[, SYMBOL := substr(SYMBOL, 1, 6)]
  univ[, RAW:=quarter_rev[J(univ$SYMBOL,univ$DATE),roll=T]$RATE]
  raw <- univ[, .(INNER_CODE, DATE, RAW)]
}

func_accrued_surplus <- function(var, dates, start, end, save_folder, uqer_folder) {
  
  raw <- read_raw_data(start, end, c("TProfitTTM","NetOperateCFTTM","TotalAssets"), uqer_folder)
  raw <- dcast(raw,DATE+SYMBOL~variable,value.var="RAW",fun.aggregate=max)
  raw[,ACCURED:=TProfitTTM-NetOperateCFTTM]
  
  raw <- raw[!(is.na(TotalAssets)|is.na(ACCURED))]
  
  quarter_cr <- raw[, .(DATE = head(DATE, 1)), by = .(SYMBOL, TotalAssets, ACCURED)]
  quarter_cr[, PREV_ASSET:=shift(TotalAssets),by=.(SYMBOL)]
  quarter_cr[,MASSET:=(TotalAssets+PREV_ASSET)/2]
  quarter_cr[is.na(MASSET),MASSET:=TotalAssets]
  quarter_cr[,RAW:=ACCURED/MASSET]
  
  quarter_cr[,SYMBOL:=as.character(SYMBOL)]
  quarter_cr[,SYMBOL:=sapply(SYMBOL,fill_code)]
  setkey(quarter_cr, SYMBOL, DATE)
  
  #normal
  univ <- stk_universe(start, end)
  univ[,SYMBOL := to_jydb_symbol(INNER_CODE)]
  univ[, SYMBOL := substr(SYMBOL, 1, 6)]
  univ[, RAW:=quarter_cr[J(univ$SYMBOL,univ$DATE),roll=T]$RAW]
  raw <- univ[, .(INNER_CODE, DATE, RAW)]
}

func_ebit_growth_1y <- function(var, dates, start, end, save_folder, uqer_folder) {
  
  raw <- read_raw_data(start, end, c("EBIT"), uqer_folder)
  revenue <- raw[,.(SYMBOL,DATE,RAW)]
  
  revenue[is.na(RAW), RAW := NA_real_]
  revenue <- revenue[!is.na(RAW)]
  
  quarter_rev <- revenue[, .(DATE = head(DATE, 1)), by = .(SYMBOL, RAW)]
  quarter_rev <- quarter_rev[!is.na(RAW)]
  setkey(quarter_rev, SYMBOL, DATE)
  
  quarter_rev[, PREV_YDATE := DATE - 365.5]
  quarter_rev[, PREV_YRAW := quarter_rev[J(quarter_rev$SYMBOL, quarter_rev$PREV_YDATE), roll = T]$RAW]
  quarter_rev[,ABS_PREV_YRAW:=abs(PREV_YRAW)]
  
  quarter_rev[,RATE:=(RAW-PREV_YRAW)/ABS_PREV_YRAW]
  quarter_rev <- quarter_rev[RATE!=Inf]
  
  quarter_rev[,SYMBOL:=as.character(SYMBOL)]
  quarter_rev[,SYMBOL:=sapply(SYMBOL,fill_code)]
  setkey(quarter_rev, SYMBOL, DATE)
  
  #normal
  univ <- stk_universe(start, end)
  univ[,SYMBOL := to_jydb_symbol(INNER_CODE)]
  univ[, SYMBOL := substr(SYMBOL, 1, 6)]
  univ[, RAW:=quarter_rev[J(univ$SYMBOL,univ$DATE),roll=T]$RATE]
  raw <- univ[, .(INNER_CODE, DATE, RAW)]
}

func_tprofit_growth_1y <- function(var, dates, start, end, save_folder, uqer_folder) {
  
  raw <- read_raw_data(start, end, c("TProfitTTM"), uqer_folder)
  revenue <- raw[,.(SYMBOL,DATE,RAW)]
  
  revenue[is.na(RAW), RAW := NA_real_]
  revenue <- revenue[!is.na(RAW)]
  
  quarter_rev <- revenue[, .(DATE = head(DATE, 1)), by = .(SYMBOL, RAW)]
  quarter_rev <- quarter_rev[!is.na(RAW)]
  setkey(quarter_rev, SYMBOL, DATE)
  
  quarter_rev[, PREV_YDATE := DATE - 365.5]
  quarter_rev[, PREV_YRAW := quarter_rev[J(quarter_rev$SYMBOL, quarter_rev$PREV_YDATE), roll = T]$RAW]
  quarter_rev[,ABS_PREV_YRAW:=abs(PREV_YRAW)]
  
  quarter_rev[,RATE:=(RAW-PREV_YRAW)/ABS_PREV_YRAW]
  quarter_rev <- quarter_rev[RATE!=Inf]
  
  quarter_rev[,SYMBOL:=as.character(SYMBOL)]
  quarter_rev[,SYMBOL:=sapply(SYMBOL,fill_code)]
  setkey(quarter_rev, SYMBOL, DATE)
  
  #normal
  univ <- stk_universe(start, end)
  univ[,SYMBOL := to_jydb_symbol(INNER_CODE)]
  univ[, SYMBOL := substr(SYMBOL, 1, 6)]
  univ[, RAW:=quarter_rev[J(univ$SYMBOL,univ$DATE),roll=T]$RATE]
  raw <- univ[, .(INNER_CODE, DATE, RAW)]
}

func_tcost_growth_1y <- function(var, dates, start, end, save_folder, uqer_folder) {
  
  raw <- read_raw_data(start, end, c("TCostTTM"), uqer_folder)
  revenue <- raw[,.(SYMBOL,DATE,RAW)]
  
  revenue[is.na(RAW), RAW := NA_real_]
  revenue <- revenue[!is.na(RAW)]
  
  quarter_rev <- revenue[, .(DATE = head(DATE, 1)), by = .(SYMBOL, RAW)]
  quarter_rev <- quarter_rev[!is.na(RAW)]
  setkey(quarter_rev, SYMBOL, DATE)
  
  quarter_rev[, PREV_YDATE := DATE - 365.5]
  quarter_rev[, PREV_YRAW := quarter_rev[J(quarter_rev$SYMBOL, quarter_rev$PREV_YDATE), roll = T]$RAW]
  quarter_rev[,ABS_PREV_YRAW:=abs(PREV_YRAW)]
  
  quarter_rev[,RATE:=(RAW-PREV_YRAW)/ABS_PREV_YRAW]
  quarter_rev <- quarter_rev[RATE!=Inf]
  
  quarter_rev[,SYMBOL:=as.character(SYMBOL)]
  quarter_rev[,SYMBOL:=sapply(SYMBOL,fill_code)]
  setkey(quarter_rev, SYMBOL, DATE)
  
  #normal
  univ <- stk_universe(start, end)
  univ[,SYMBOL := to_jydb_symbol(INNER_CODE)]
  univ[, SYMBOL := substr(SYMBOL, 1, 6)]
  univ[, RAW:=quarter_rev[J(univ$SYMBOL,univ$DATE),roll=T]$RATE]
  raw <- univ[, .(INNER_CODE, DATE, RAW)]
}

func_operatingcash_growth_1y <- function(var, dates, start, end, save_folder, uqer_folder) {
  
  raw <- read_raw_data(start, end, c("NetOperateCFTTM"), uqer_folder)
  revenue <- raw[,.(SYMBOL,DATE,RAW)]
  
  revenue[is.na(RAW), RAW := NA_real_]
  revenue <- revenue[!is.na(RAW)]
  
  quarter_rev <- revenue[, .(DATE = head(DATE, 1)), by = .(SYMBOL, RAW)]
  quarter_rev <- quarter_rev[!is.na(RAW)]
  setkey(quarter_rev, SYMBOL, DATE)
  
  quarter_rev[, PREV_YDATE := DATE - 365.5]
  quarter_rev[, PREV_YRAW := quarter_rev[J(quarter_rev$SYMBOL, quarter_rev$PREV_YDATE), roll = T]$RAW]
  quarter_rev[,ABS_PREV_YRAW:=abs(PREV_YRAW)]
  
  quarter_rev[,RATE:=(RAW-PREV_YRAW)/ABS_PREV_YRAW]
  quarter_rev <- quarter_rev[RATE!=Inf]
  
  quarter_rev[,SYMBOL:=as.character(SYMBOL)]
  quarter_rev[,SYMBOL:=sapply(SYMBOL,fill_code)]
  setkey(quarter_rev, SYMBOL, DATE)
  
  #normal
  univ <- stk_universe(start, end)
  univ[,SYMBOL := to_jydb_symbol(INNER_CODE)]
  univ[, SYMBOL := substr(SYMBOL, 1, 6)]
  univ[, RAW:=quarter_rev[J(univ$SYMBOL,univ$DATE),roll=T]$RATE]
  raw <- univ[, .(INNER_CODE, DATE, RAW)]
}

func_operatingcash_ep <- function(var, dates, start, end, save_folder, uqer_folder) {
  #归母净利润比市值
  raw <- read_raw_data(start, end, c("NetOperateCFTTM"), uqer_folder)
  profit <- raw[,.(SYMBOL,DATE,RAW)]
  
  profit[is.na(RAW), RAW := NA_real_]
  profit <- profit[!is.na(RAW)]
  
  quarter_rev <- profit[, .(DATE = head(DATE, 1)), by = .(SYMBOL, RAW)]
  quarter_rev <- quarter_rev[!is.na(RAW)]
  
  quarter_rev[,SYMBOL:=as.character(SYMBOL)]
  quarter_rev[,SYMBOL:=sapply(SYMBOL,fill_code)]
  setkey(quarter_rev, SYMBOL, DATE)
  
  #normal
  univ <- stk_universe(start, end)
  univ[,SYMBOL := to_jydb_symbol(INNER_CODE)]
  univ[, SYMBOL := substr(SYMBOL, 1, 6)]
  univ <- stk_mv(universe=univ)
  univ[, PROFIT:=quarter_rev[J(univ$SYMBOL,univ$DATE),roll=T]$RAW]
  univ[, RAW:=PROFIT/MV]
  raw <- univ[, .(INNER_CODE, DATE, RAW)]
}

func_tcost_growth_3y <- function(var, dates, start, end, save_folder, uqer_folder) {
  
  eps <- read_raw_data(start, end, c("TCostTTM"), uqer_folder) 
  
  eps[is.na(RAW), RAW := NA_real_]
  eps <- eps[!is.na(RAW)]
  
  quarter_eps <- eps[, .(DATE = head(DATE, 1)), by = .(SYMBOL, RAW)]
  quarter_eps <- quarter_eps[!is.na(RAW)]
  setkey(quarter_eps, SYMBOL, DATE)
  
  quarter_eps[, PREV_YDATE := DATE - 365.5]
  quarter_eps[, PREV_YDATE2 := DATE - 365.5*2]
  quarter_eps[, PREV_YRAW := quarter_eps[J(quarter_eps$SYMBOL, quarter_eps$PREV_YDATE), roll = T]$RAW]
  quarter_eps[, PREV_YRAW2 := quarter_eps[J(quarter_eps$SYMBOL, quarter_eps$PREV_YDATE2), roll = T]$RAW]
  
  overlap_q_eps <- melt.data.table(quarter_eps[, .(SYMBOL, DATE, RAW, PREV_YRAW, PREV_YRAW2)], id.vars = c("SYMBOL", "DATE"))
  overlap_q_eps[, RK := seq(.N), by = .(SYMBOL, DATE)]
  setorder(overlap_q_eps, SYMBOL, DATE, -RK)
  
  growth_eps_beta <- overlap_q_eps[, .(BETA = ifelse(.N > 1, as.numeric(GCAMCQT:::lm_growth(value)), NA_real_)), by = .(SYMBOL, DATE)]
  growth_eps_beta[,SYMBOL:=as.character(SYMBOL)]
  growth_eps_beta[,SYMBOL:=sapply(SYMBOL,fill_code)]
  setkey(growth_eps_beta, SYMBOL, DATE)
  
  #normal
  univ <- stk_universe(start, end)
  univ[,SYMBOL := to_jydb_symbol(INNER_CODE)]
  univ[, SYMBOL := substr(SYMBOL, 1, 6)]
  univ[, RAW:=growth_eps_beta[J(univ$SYMBOL,univ$DATE),roll=T]$BETA]
  raw <- univ[, .(INNER_CODE, DATE, RAW)]
}

func_netprofit_growth_3y <- function(var, dates, start, end, save_folder, uqer_folder) {
  
  eps <- read_raw_data(start, end, c("NetProfitTTM"), uqer_folder) 
  
  eps[is.na(RAW), RAW := NA_real_]
  eps <- eps[!is.na(RAW)]
  
  quarter_eps <- eps[, .(DATE = head(DATE, 1)), by = .(SYMBOL, RAW)]
  quarter_eps <- quarter_eps[!is.na(RAW)]
  setkey(quarter_eps, SYMBOL, DATE)
  
  quarter_eps[, PREV_YDATE := DATE - 365.5]
  quarter_eps[, PREV_YDATE2 := DATE - 365.5*2]
  quarter_eps[, PREV_YRAW := quarter_eps[J(quarter_eps$SYMBOL, quarter_eps$PREV_YDATE), roll = T]$RAW]
  quarter_eps[, PREV_YRAW2 := quarter_eps[J(quarter_eps$SYMBOL, quarter_eps$PREV_YDATE2), roll = T]$RAW]
  
  overlap_q_eps <- melt.data.table(quarter_eps[, .(SYMBOL, DATE, RAW, PREV_YRAW, PREV_YRAW2)], id.vars = c("SYMBOL", "DATE"))
  overlap_q_eps[, RK := seq(.N), by = .(SYMBOL, DATE)]
  setorder(overlap_q_eps, SYMBOL, DATE, -RK)
  
  growth_eps_beta <- overlap_q_eps[, .(BETA = ifelse(.N > 1, as.numeric(GCAMCQT:::lm_growth(value)), NA_real_)), by = .(SYMBOL, DATE)]
  growth_eps_beta[,SYMBOL:=as.character(SYMBOL)]
  growth_eps_beta[,SYMBOL:=sapply(SYMBOL,fill_code)]
  setkey(growth_eps_beta, SYMBOL, DATE)
  
  #normal
  univ <- stk_universe(start, end)
  univ[,SYMBOL := to_jydb_symbol(INNER_CODE)]
  univ[, SYMBOL := substr(SYMBOL, 1, 6)]
  univ[, RAW:=growth_eps_beta[J(univ$SYMBOL,univ$DATE),roll=T]$BETA]
  raw <- univ[, .(INNER_CODE, DATE, RAW)]
}

func_cash_cover <- function(var, dates, start, end, save_folder, uqer_folder) {
  
  raw <- read_raw_data(start, end, c("NetOperateCFTTM","FinanExpenseTTM"), uqer_folder)
  raw <- dcast(raw,DATE+SYMBOL~variable,value.var="RAW",fun.aggregate=max)
  
  quarter_ratio <- raw[, .(DATE = head(DATE, 1)), by = .(SYMBOL, NetOperateCFTTM, FinanExpenseTTM)]
  quarter_ratio <- quarter_ratio[!(is.na(NetOperateCFTTM)|is.na(FinanExpenseTTM))]
  setkey(quarter_ratio, SYMBOL, DATE)
  
  quarter_ratio[,RAW:=NetOperateCFTTM/FinanExpenseTTM]
  quarter_ratio <- quarter_ratio[RAW!=Inf]
  
  quarter_ratio[,SYMBOL:=as.character(SYMBOL)]
  quarter_ratio[,SYMBOL:=sapply(SYMBOL,fill_code)]
  setkey(quarter_ratio, SYMBOL, DATE)
  
  #normal
  univ <- stk_universe(start, end)
  univ[,SYMBOL := to_jydb_symbol(INNER_CODE)]
  univ[, SYMBOL := substr(SYMBOL, 1, 6)]
  univ[, RAW:=quarter_ratio[J(univ$SYMBOL,univ$DATE),roll=T]$RAW]
  raw <- univ[, .(INNER_CODE, DATE, RAW)]
}

func_tdebt_growth_1y <- function(var, dates, start, end, save_folder, uqer_folder) {
  
  raw <- read_raw_data(start, end, c("TotalAssets","TEAP"), uqer_folder)
  raw <- dcast(raw,DATE+SYMBOL~variable,value.var="RAW",fun.aggregate=max)
  raw[,RAW:=TotalAssets-TEAP]
  debt <- raw[,.(SYMBOL,DATE,RAW)]
  
  debt[is.na(RAW), RAW := NA_real_]
  debt <- debt[!is.na(RAW)]
  
  quarter_rev <- debt[, .(DATE = head(DATE, 1)), by = .(SYMBOL, RAW)]
  quarter_rev <- quarter_rev[!is.na(RAW)]
  setkey(quarter_rev, SYMBOL, DATE)
  
  quarter_rev[, PREV_YDATE := DATE - 365.5]
  quarter_rev[, PREV_YRAW := quarter_rev[J(quarter_rev$SYMBOL, quarter_rev$PREV_YDATE), roll = T]$RAW]
  
  quarter_rev[,RATE:=log(RAW/PREV_YRAW)]
  quarter_rev <- quarter_rev[RATE!=Inf]
  
  quarter_rev[,SYMBOL:=as.character(SYMBOL)]
  quarter_rev[,SYMBOL:=sapply(SYMBOL,fill_code)]
  setkey(quarter_rev, SYMBOL, DATE)
  
  #normal
  univ <- stk_universe(start, end)
  univ[,SYMBOL := to_jydb_symbol(INNER_CODE)]
  univ[, SYMBOL := substr(SYMBOL, 1, 6)]
  univ[, RAW:=quarter_rev[J(univ$SYMBOL,univ$DATE),roll=T]$RATE]
  raw <- univ[, .(INNER_CODE, DATE, RAW)]
}

func_profitap_growth_3y <- function(var, dates, start, end, save_folder, uqer_folder) {
  
  eps <- read_raw_data(start, end, c("NIAP"), uqer_folder) 
  
  eps[is.na(RAW), RAW := NA_real_]
  eps <- eps[!is.na(RAW)]
  
  quarter_eps <- eps[, .(DATE = head(DATE, 1)), by = .(SYMBOL, RAW)]
  quarter_eps <- quarter_eps[!is.na(RAW)]
  setkey(quarter_eps, SYMBOL, DATE)
  
  quarter_eps[, PREV_YDATE := DATE - 365.5]
  quarter_eps[, PREV_YDATE2 := DATE - 365.5*2]
  quarter_eps[, PREV_YRAW := quarter_eps[J(quarter_eps$SYMBOL, quarter_eps$PREV_YDATE), roll = T]$RAW]
  quarter_eps[, PREV_YRAW2 := quarter_eps[J(quarter_eps$SYMBOL, quarter_eps$PREV_YDATE2), roll = T]$RAW]
  
  overlap_q_eps <- melt.data.table(quarter_eps[, .(SYMBOL, DATE, RAW, PREV_YRAW, PREV_YRAW2)], id.vars = c("SYMBOL", "DATE"))
  overlap_q_eps[, RK := seq(.N), by = .(SYMBOL, DATE)]
  setorder(overlap_q_eps, SYMBOL, DATE, -RK)
  
  growth_eps_beta <- overlap_q_eps[, .(BETA = ifelse(.N > 1, as.numeric(GCAMCQT:::lm_growth(value)), NA_real_)), by = .(SYMBOL, DATE)]
  growth_eps_beta[,SYMBOL:=as.character(SYMBOL)]
  growth_eps_beta[,SYMBOL:=sapply(SYMBOL,fill_code)]
  setkey(growth_eps_beta, SYMBOL, DATE)
  
  #normal
  univ <- stk_universe(start, end)
  univ[,SYMBOL := to_jydb_symbol(INNER_CODE)]
  univ[, SYMBOL := substr(SYMBOL, 1, 6)]
  univ[, RAW:=growth_eps_beta[J(univ$SYMBOL,univ$DATE),roll=T]$BETA]
  raw <- univ[, .(INNER_CODE, DATE, RAW)]
}

func_fixasset_turnover <- function(var, dates, start, end, save_folder, uqer_folder) {
  
  raw <- read_raw_data(start, end, c("TProfitTTM","TotalFixedAssets"), uqer_folder)
  raw <- dcast(raw,DATE+SYMBOL~variable,value.var="RAW",fun.aggregate=max)
  
  raw <- raw[!(is.na(TotalFixedAssets)|is.na(TProfitTTM))]
  
  quarter_cr <- raw[, .(DATE = head(DATE, 1)), by = .(SYMBOL, TotalFixedAssets, TProfitTTM)]
  quarter_cr[, PREV_ASSET:=shift(TotalFixedAssets),by=.(SYMBOL)]
  quarter_cr[,MASSET:=(TotalFixedAssets+PREV_ASSET)/2]
  quarter_cr[is.na(MASSET),MASSET:=TotalFixedAssets]
  quarter_cr[,RAW:=TProfitTTM/MASSET]
  
  quarter_cr[,SYMBOL:=as.character(SYMBOL)]
  quarter_cr[,SYMBOL:=sapply(SYMBOL,fill_code)]
  setkey(quarter_cr, SYMBOL, DATE)
  
  #normal
  univ <- stk_universe(start, end)
  univ[,SYMBOL := to_jydb_symbol(INNER_CODE)]
  univ[, SYMBOL := substr(SYMBOL, 1, 6)]
  univ[, RAW:=quarter_cr[J(univ$SYMBOL,univ$DATE),roll=T]$RAW]
  raw <- univ[, .(INNER_CODE, DATE, RAW)]
}

func_oprevenue_mix_growth_3y <- function(var, dates, start, end, save_folder, uqer_folder) {
  
  eps <- read_raw_data(start, end, c("OperateProfitTTM"), uqer_folder) 
  eps[is.na(RAW), RAW := NA_real_]
  eps <- eps[!is.na(RAW)]
  
  quarter_eps<- eps[, .(DATE = head(DATE, 1)), by = .(SYMBOL, RAW)]
  quarter_eps <- quarter_eps[!is.na(RAW)]
  setkey(quarter_eps, SYMBOL, DATE)
  
  quarter_eps[, PREV_YDATE := DATE - 365.5*3]
  quarter_eps[, PREV_YRAW := quarter_eps[J(quarter_eps$SYMBOL, quarter_eps$PREV_YDATE), roll = T]$RAW]
  quarter_eps[, RAW := ((RAW - PREV_YRAW) / abs(PREV_YRAW) + 1) ^ (1/3) - 1]
  setkey(quarter_eps, SYMBOL, DATE)
  
  # normal 
  quarter_eps[is.na(RAW) | is.infinite(RAW), RAW := NA_real_]
  quarter_eps[,SYMBOL:=as.character(SYMBOL)]
  quarter_eps[,SYMBOL:=sapply(SYMBOL,fill_code)]
  setkey(quarter_eps, SYMBOL, DATE)
  
  #normal
  univ <- stk_universe(start, end)
  univ[,SYMBOL := to_jydb_symbol(INNER_CODE)]
  univ[, SYMBOL := substr(SYMBOL, 1, 6)]
  univ[, RAW:=quarter_eps[J(univ$SYMBOL,univ$DATE),roll=T]$RAW]
  raw <- univ[, .(INNER_CODE, DATE, RAW)]
}

func_fixasset_ratio <- function(var, dates, start, end, save_folder, uqer_folder) {
  
  raw <- read_raw_data(start, end, c("TotalFixedAssets","TotalAssets"), uqer_folder)
  raw <- dcast(raw,DATE+SYMBOL~variable,value.var="RAW",fun.aggregate=max)
  raw <- raw[!(is.na(TotalFixedAssets)|is.na(TotalAssets))]
  
  quarter_cr <- raw[, .(DATE = head(DATE, 1)), by = .(SYMBOL, TotalFixedAssets, TotalAssets)]
  quarter_cr[,RAW:=TotalFixedAssets/TotalAssets]
  quarter_cr[is.na(RAW) | is.infinite(RAW), RAW := NA_real_]
  
  quarter_cr[,SYMBOL:=as.character(SYMBOL)]
  quarter_cr[,SYMBOL:=sapply(SYMBOL,fill_code)]
  setkey(quarter_cr, SYMBOL, DATE)
  
  #normal
  univ <- stk_universe(start, end)
  univ[,SYMBOL := to_jydb_symbol(INNER_CODE)]
  univ[, SYMBOL := substr(SYMBOL, 1, 6)]
  univ[, RAW:=quarter_cr[J(univ$SYMBOL,univ$DATE),roll=T]$RAW]
  raw <- univ[, .(INNER_CODE, DATE, RAW)]
}

func_tprofit_mix_growth_3y <- function(var, dates, start, end, save_folder, uqer_folder) {
  
  eps <- read_raw_data(start, end, c("TProfitTTM"), uqer_folder) 
  eps[is.na(RAW), RAW := NA_real_]
  eps <- eps[!is.na(RAW)]
  
  quarter_eps<- eps[, .(DATE = head(DATE, 1)), by = .(SYMBOL, RAW)]
  quarter_eps <- quarter_eps[!is.na(RAW)]
  setkey(quarter_eps, SYMBOL, DATE)
  
  quarter_eps[, PREV_YDATE := DATE - 365.5*3]
  quarter_eps[, PREV_YRAW := quarter_eps[J(quarter_eps$SYMBOL, quarter_eps$PREV_YDATE), roll = T]$RAW]
  quarter_eps[, RAW := ((RAW - PREV_YRAW) / abs(PREV_YRAW) + 1) ^ (1/3) - 1]
  setkey(quarter_eps, SYMBOL, DATE)
  
  # normal 
  quarter_eps[is.na(RAW) | is.infinite(RAW), RAW := NA_real_]
  quarter_eps[,SYMBOL:=as.character(SYMBOL)]
  quarter_eps[,SYMBOL:=sapply(SYMBOL,fill_code)]
  setkey(quarter_eps, SYMBOL, DATE)
  
  #normal
  univ <- stk_universe(start, end)
  univ[,SYMBOL := to_jydb_symbol(INNER_CODE)]
  univ[, SYMBOL := substr(SYMBOL, 1, 6)]
  univ[, RAW:=quarter_eps[J(univ$SYMBOL,univ$DATE),roll=T]$RAW]
  raw <- univ[, .(INNER_CODE, DATE, RAW)]
}

func_dilutedeps <- function(var, dates, start, end, save_folder, uqer_folder) {
  
  eps <- read_raw_data(start, end, c("DilutedEPS"), uqer_folder) 
  eps[is.na(RAW), RAW := NA_real_]
  eps <- eps[!is.na(RAW)]
  
  quarter_eps<- eps[, .(DATE = head(DATE, 1)), by = .(SYMBOL, RAW)]
  quarter_eps <- quarter_eps[!is.na(RAW)]
  setkey(quarter_eps, SYMBOL, DATE)
  
  # normal 
  quarter_eps[is.na(RAW) | is.infinite(RAW), RAW := NA_real_]
  quarter_eps[,SYMBOL:=as.character(SYMBOL)]
  quarter_eps[,SYMBOL:=sapply(SYMBOL,fill_code)]
  setkey(quarter_eps, SYMBOL, DATE)
  
  #normal
  univ <- stk_universe(start, end)
  univ[,SYMBOL := to_jydb_symbol(INNER_CODE)]
  univ[, SYMBOL := substr(SYMBOL, 1, 6)]
  univ[, RAW:=quarter_eps[J(univ$SYMBOL,univ$DATE),roll=T]$RAW]
  raw <- univ[, .(INNER_CODE, DATE, RAW)]
}

func_grossprofitratio_mix_growth_3y <- function(var, dates, start, end, save_folder, uqer_folder) {
  
  raw <- read_raw_data(start, end, c("RevenueTTM","GrossProfitTTM"), uqer_folder) 
  raw <- dcast(raw,DATE+SYMBOL~variable,value.var="RAW",fun.aggregate=max)
  raw[,RAW:=GrossProfitTTM/RevenueTTM]
  raw[is.na(RAW) | is.infinite(RAW), RAW := NA_real_]
  raw <- raw[!is.na(RAW)]
  
  quarter_eps<- raw[, .(DATE = head(DATE, 1)), by = .(SYMBOL, RAW)]
  quarter_eps <- quarter_eps[!is.na(RAW)]
  setkey(quarter_eps, SYMBOL, DATE)
  
  quarter_eps[, PREV_YDATE := DATE - 365.5*3]
  quarter_eps[, PREV_YRAW := quarter_eps[J(quarter_eps$SYMBOL, quarter_eps$PREV_YDATE), roll = T]$RAW]
  quarter_eps[, RAW := ((RAW - PREV_YRAW) / abs(PREV_YRAW) + 1) ^ (1/3) - 1]
  setkey(quarter_eps, SYMBOL, DATE)
  
  # normal 
  quarter_eps[is.na(RAW) | is.infinite(RAW), RAW := NA_real_]
  quarter_eps[,SYMBOL:=as.character(SYMBOL)]
  quarter_eps[,SYMBOL:=sapply(SYMBOL,fill_code)]
  setkey(quarter_eps, SYMBOL, DATE)
  
  #normal
  univ <- stk_universe(start, end)
  univ[,SYMBOL := to_jydb_symbol(INNER_CODE)]
  univ[, SYMBOL := substr(SYMBOL, 1, 6)]
  univ[, RAW:=quarter_eps[J(univ$SYMBOL,univ$DATE),roll=T]$RAW]
  raw <- univ[, .(INNER_CODE, DATE, RAW)]
}

func_assetfixation_ratio <- function(var, dates, start, end, save_folder, uqer_folder) {
  
  raw <- read_raw_data(start, end, c("TotalFixedAssets","TEAP"), uqer_folder)
  raw <- dcast(raw,DATE+SYMBOL~variable,value.var="RAW",fun.aggregate=max)
  raw <- raw[!(is.na(TotalFixedAssets)|is.na(TEAP))]
  
  quarter_cr <- raw[, .(DATE = head(DATE, 1)), by = .(SYMBOL, TotalFixedAssets, TEAP)]
  quarter_cr[,RAW:=TotalFixedAssets/TEAP]
  quarter_cr[is.na(RAW) | is.infinite(RAW), RAW := NA_real_]
  
  quarter_cr[,SYMBOL:=as.character(SYMBOL)]
  quarter_cr[,SYMBOL:=sapply(SYMBOL,fill_code)]
  setkey(quarter_cr, SYMBOL, DATE)
  
  #normal
  univ <- stk_universe(start, end)
  univ[,SYMBOL := to_jydb_symbol(INNER_CODE)]
  univ[, SYMBOL := substr(SYMBOL, 1, 6)]
  univ[, RAW:=quarter_cr[J(univ$SYMBOL,univ$DATE),roll=T]$RAW]
  raw <- univ[, .(INNER_CODE, DATE, RAW)]
}

func_totalprofit_costratio <- function(var, dates, start, end, save_folder, uqer_folder) {
  
  raw <- read_raw_data(start, end, c("NetProfitTTM","TCostTTM"), uqer_folder)
  raw <- dcast(raw,DATE+SYMBOL~variable,value.var="RAW",fun.aggregate=max)
  raw <- raw[!(is.na(NetProfitTTM)|is.na(TCostTTM))]
  
  quarter_cr <- raw[, .(DATE = head(DATE, 1)), by = .(SYMBOL, NetProfitTTM, TCostTTM)]
  quarter_cr[,RAW:=NetProfitTTM/TCostTTM]
  quarter_cr[is.na(RAW) | is.infinite(RAW), RAW := NA_real_]
  
  quarter_cr[,SYMBOL:=as.character(SYMBOL)]
  quarter_cr[,SYMBOL:=sapply(SYMBOL,fill_code)]
  setkey(quarter_cr, SYMBOL, DATE)
  
  #normal
  univ <- stk_universe(start, end)
  univ[,SYMBOL := to_jydb_symbol(INNER_CODE)]
  univ[, SYMBOL := substr(SYMBOL, 1, 6)]
  univ[, RAW:=quarter_cr[J(univ$SYMBOL,univ$DATE),roll=T]$RAW]
  raw <- univ[, .(INNER_CODE, DATE, RAW)]
}

func_operatingprofit_mix_growth_3y <- function(var, dates, start, end, save_folder, uqer_folder) {
  
  eps <- read_raw_data(start, end, c("TRevenueTTM"), uqer_folder) 
  eps[is.na(RAW), RAW := NA_real_]
  eps <- eps[!is.na(RAW)]
  
  quarter_eps<- eps[, .(DATE = head(DATE, 1)), by = .(SYMBOL, RAW)]
  quarter_eps <- quarter_eps[!is.na(RAW)]
  setkey(quarter_eps, SYMBOL, DATE)
  
  quarter_eps[, PREV_YDATE := DATE - 365.5*3]
  quarter_eps[, PREV_YRAW := quarter_eps[J(quarter_eps$SYMBOL, quarter_eps$PREV_YDATE), roll = T]$RAW]
  quarter_eps[, RAW := ((RAW - PREV_YRAW) / abs(PREV_YRAW) + 1) ^ (1/3) - 1]
  setkey(quarter_eps, SYMBOL, DATE)
  
  # normal 
  quarter_eps[is.na(RAW) | is.infinite(RAW), RAW := NA_real_]
  quarter_eps[,SYMBOL:=as.character(SYMBOL)]
  quarter_eps[,SYMBOL:=sapply(SYMBOL,fill_code)]
  setkey(quarter_eps, SYMBOL, DATE)
  
  #normal
  univ <- stk_universe(start, end)
  univ[,SYMBOL := to_jydb_symbol(INNER_CODE)]
  univ[, SYMBOL := substr(SYMBOL, 1, 6)]
  univ[, RAW:=quarter_eps[J(univ$SYMBOL,univ$DATE),roll=T]$RAW]
  raw <- univ[, .(INNER_CODE, DATE, RAW)]
}

func_cashrevenue_costratio <- function(var, dates, start, end, save_folder, uqer_folder) {
  
  raw <- read_raw_data(start, end, c("NetOperateCFTTM","RevenueTTM"), uqer_folder)
  raw <- dcast(raw,DATE+SYMBOL~variable,value.var="RAW",fun.aggregate=max)
  raw <- raw[!(is.na(NetOperateCFTTM)|is.na(RevenueTTM))]
  
  quarter_cr <- raw[, .(DATE = head(DATE, 1)), by = .(SYMBOL, NetOperateCFTTM, RevenueTTM)]
  quarter_cr[,RAW:=NetOperateCFTTM/RevenueTTM]
  quarter_cr[is.na(RAW) | is.infinite(RAW), RAW := NA_real_]
  
  quarter_cr[,SYMBOL:=as.character(SYMBOL)]
  quarter_cr[,SYMBOL:=sapply(SYMBOL,fill_code)]
  setkey(quarter_cr, SYMBOL, DATE)
  
  #normal
  univ <- stk_universe(start, end)
  univ[,SYMBOL := to_jydb_symbol(INNER_CODE)]
  univ[, SYMBOL := substr(SYMBOL, 1, 6)]
  univ[, RAW:=quarter_cr[J(univ$SYMBOL,univ$DATE),roll=T]$RAW]
  raw <- univ[, .(INNER_CODE, DATE, RAW)]
}

func_operatingrevenueps <- function(var, dates, start, end, save_folder, uqer_folder) {
  
  eps <- read_raw_data(start, end, c("OperatingRevenuePS"), uqer_folder) 
  eps[is.na(RAW), RAW := NA_real_]
  eps <- eps[!is.na(RAW)]
  
  quarter_eps<- eps[, .(DATE = head(DATE, 1)), by = .(SYMBOL, RAW)]
  quarter_eps <- quarter_eps[!is.na(RAW)]
  setkey(quarter_eps, SYMBOL, DATE)
  
  # normal 
  quarter_eps[is.na(RAW) | is.infinite(RAW), RAW := NA_real_]
  quarter_eps[,SYMBOL:=as.character(SYMBOL)]
  quarter_eps[,SYMBOL:=sapply(SYMBOL,fill_code)]
  setkey(quarter_eps, SYMBOL, DATE)
  
  #normal
  univ <- stk_universe(start, end)
  univ[,SYMBOL := to_jydb_symbol(INNER_CODE)]
  univ[, SYMBOL := substr(SYMBOL, 1, 6)]
  univ[, RAW:=quarter_eps[J(univ$SYMBOL,univ$DATE),roll=T]$RAW]
  raw <- univ[, .(INNER_CODE, DATE, RAW)]
}

func_operatingprofitratio_growth_1y <- function(var, dates, start, end, save_folder, uqer_folder) {
  
  raw <- read_raw_data(start, end, c("OperateProfitTTM","RevenueTTM"), uqer_folder)
  raw <- dcast(raw,DATE+SYMBOL~variable,value.var="RAW",fun.aggregate=max)
  raw <- raw[!(is.na(OperateProfitTTM)|is.na(RevenueTTM))]
  raw[, RAW := OperateProfitTTM/RevenueTTM]
  revenue <- raw[,.(SYMBOL,DATE,RAW)]
  
  revenue[is.na(RAW), RAW := NA_real_]
  revenue <- revenue[!is.na(RAW)]
  
  quarter_rev <- revenue[, .(DATE = head(DATE, 1)), by = .(SYMBOL, RAW)]
  quarter_rev <- quarter_rev[!is.na(RAW)]
  setkey(quarter_rev, SYMBOL, DATE)
  
  quarter_rev[, PREV_YDATE := DATE - 365.5]
  quarter_rev[, PREV_YRAW := quarter_rev[J(quarter_rev$SYMBOL, quarter_rev$PREV_YDATE), roll = T]$RAW]
  quarter_rev[,ABS_PREV_YRAW:=abs(PREV_YRAW)]
  
  quarter_rev[,RATE:=(RAW-PREV_YRAW)/ABS_PREV_YRAW]
  quarter_rev <- quarter_rev[RATE!=Inf]
  
  quarter_rev[,SYMBOL:=as.character(SYMBOL)]
  quarter_rev[,SYMBOL:=sapply(SYMBOL,fill_code)]
  setkey(quarter_rev, SYMBOL, DATE)
  
  #normal
  univ <- stk_universe(start, end)
  univ[,SYMBOL := to_jydb_symbol(INNER_CODE)]
  univ[, SYMBOL := substr(SYMBOL, 1, 6)]
  univ[, RAW:=quarter_rev[J(univ$SYMBOL,univ$DATE),roll=T]$RATE]
  raw <- univ[, .(INNER_CODE, DATE, RAW)]
}

func_capitalsurplus_growth_1y <- function(var, dates, start, end, save_folder, uqer_folder) {
  
  raw <- read_raw_data(start, end, c("CapitalSurplusFundPS"), uqer_folder)
  revenue <- raw[,.(SYMBOL,DATE,RAW)]
  
  revenue[is.na(RAW), RAW := NA_real_]
  revenue <- revenue[!is.na(RAW)]
  
  quarter_rev <- revenue[, .(DATE = head(DATE, 1)), by = .(SYMBOL, RAW)]
  quarter_rev <- quarter_rev[!is.na(RAW)]
  setkey(quarter_rev, SYMBOL, DATE)
  
  quarter_rev[, PREV_YDATE := DATE - 365.5]
  quarter_rev[, PREV_YRAW := quarter_rev[J(quarter_rev$SYMBOL, quarter_rev$PREV_YDATE), roll = T]$RAW]
  quarter_rev[,ABS_PREV_YRAW:=abs(PREV_YRAW)]
  
  quarter_rev[,RATE:=(RAW-PREV_YRAW)/ABS_PREV_YRAW]
  quarter_rev <- quarter_rev[RATE!=Inf]
  
  quarter_rev[,SYMBOL:=as.character(SYMBOL)]
  quarter_rev[,SYMBOL:=sapply(SYMBOL,fill_code)]
  setkey(quarter_rev, SYMBOL, DATE)
  
  #normal
  univ <- stk_universe(start, end)
  univ[,SYMBOL := to_jydb_symbol(INNER_CODE)]
  univ[, SYMBOL := substr(SYMBOL, 1, 6)]
  univ[, RAW:=quarter_rev[J(univ$SYMBOL,univ$DATE),roll=T]$RATE]
  raw <- univ[, .(INNER_CODE, DATE, RAW)]
}

func_capitalsurplus_growth_1q <- function(var, dates, start, end, save_folder, uqer_folder) {
  
  raw <- read_raw_data(start, end, c("CapitalSurplusFundPS"), uqer_folder)
  revenue <- raw[,.(SYMBOL,DATE,RAW)]
  
  revenue[is.na(RAW), RAW := NA_real_]
  revenue <- revenue[!is.na(RAW)]
  
  quarter_rev <- revenue[, .(DATE = head(DATE, 1)), by = .(SYMBOL, RAW)]
  quarter_rev <- quarter_rev[!is.na(RAW)]
  setkey(quarter_rev, SYMBOL, DATE)
  
  quarter_rev[, PREV_YRAW := shift(RAW), by=.(SYMBOL)]
  quarter_rev[,ABS_PREV_YRAW:=abs(PREV_YRAW)]
  
  quarter_rev[,RATE:=(RAW-PREV_YRAW)/ABS_PREV_YRAW]
  quarter_rev <- quarter_rev[RATE!=Inf]
  
  quarter_rev[,SYMBOL:=as.character(SYMBOL)]
  quarter_rev[,SYMBOL:=sapply(SYMBOL,fill_code)]
  setkey(quarter_rev, SYMBOL, DATE)
  
  #normal
  univ <- stk_universe(start, end)
  univ[,SYMBOL := to_jydb_symbol(INNER_CODE)]
  univ[, SYMBOL := substr(SYMBOL, 1, 6)]
  univ[, RAW:=quarter_rev[J(univ$SYMBOL,univ$DATE),roll=T]$RATE]
  raw <- univ[, .(INNER_CODE, DATE, RAW)]
}

func_revenuetoasset_chg <- function(var, dates, start, end, save_folder, uqer_folder) {
  
  raw <- read_raw_data(start, end, c("RevenueTTM","TotalAssets"), uqer_folder)
  raw <- dcast(raw,DATE+SYMBOL~variable,value.var="RAW",fun.aggregate=max)
  raw <- raw[!(is.na(RevenueTTM)|is.na(TotalAssets))]
  raw[, RAW := RevenueTTM/TotalAssets]
  revenue <- raw[,.(SYMBOL,DATE,RAW)]
  
  quarter_rev <- revenue[, .(DATE = head(DATE, 1)), by = .(SYMBOL, RAW)]
  quarter_rev <- quarter_rev[!is.na(RAW)]
  setkey(quarter_rev, SYMBOL, DATE)
  
  quarter_rev[, PREV_YDATE := DATE - 365.5]
  quarter_rev[, PREV_YRAW := quarter_rev[J(quarter_rev$SYMBOL, quarter_rev$PREV_YDATE), roll = T]$RAW]
  
  quarter_rev[,CHG:=RAW-PREV_YRAW]
  quarter_rev <- quarter_rev[CHG!=Inf]
  
  quarter_rev[,SYMBOL:=as.character(SYMBOL)]
  quarter_rev[,SYMBOL:=sapply(SYMBOL,fill_code)]
  setkey(quarter_rev, SYMBOL, DATE)
  
  #normal
  univ <- stk_universe(start, end)
  univ[,SYMBOL := to_jydb_symbol(INNER_CODE)]
  univ[, SYMBOL := substr(SYMBOL, 1, 6)]
  univ[, RAW:=quarter_rev[J(univ$SYMBOL,univ$DATE),roll=T]$CHG]
  raw <- univ[, .(INNER_CODE, DATE, RAW)]
}

