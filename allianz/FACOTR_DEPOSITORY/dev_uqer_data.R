
# function utils

# add_quarter <- function(dt) {
#   
#   dt[, YEAR := year(DATE)]
#   dt[, MMDD := month(DATE) * 100 + lubridate::day(DATE)]
#   
#   # 先确定能确定的
#   dt[MMDD > 630 & MMDD <= 930, QUARTER := paste0(YEAR, "02")]
#   dt[MMDD > 930 & MMDD <= 1231, QUARTER := paste0(YEAR, "03")]
#   dt[MMDD <= 331, QUARTER := paste0(YEAR - 1, "04")]
#   
#   # 补齐
#   dt[is.na(QUARTER), NUM := seq(.N, 1L, -1L), by = .(SYMBOL, YEAR)]
#   dt[is.na(QUARTER), N := .N, by = .(SYMBOL, YEAR)]
#   dt[N > 1 & NUM == 1, QUARTER := paste0(YEAR, "01")]
#   dt[N > 1 & NUM == 2, QUARTER := paste0(YEAR - 1, "04")]
#   
#   dt[, SHIFT_QUARTER := shift(QUARTER), by = SYMBOL]
#   dt[(N == 1) & (!substr(SHIFT_QUARTER, 5, 6) %in% "04"), QUARTER := paste0(YEAR - 1, "04")]
#   
#   dt <- dt[!is.na(QUARTER), .(SYMBOL, DATE, RAW, QUARTER = as.numeric(QUARTER))]
# }


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


lm_res <- function(y, x) {
  y <- GCAMCQT::normalizing_zscore(y)
  x <- GCAMCQT::normalizing_zscore(x)
  
  x <- cbind(1, x)
  
  tryCatch({res <- lm(y~x,na.action=na.omit)$residuals
  return(res[length(res)])},
  error=function(e){return(NA)})
}


normalize_raw <- function(raw, dates) {
  
  res <- new_factor_universe(from_to = range(dates), freq = "daily")
  res[, SYMBOL := to_jydb_symbol(INNER_CODE)]
  res[, SYMBOL := substr(SYMBOL, 1, 6)]
  
  res[, RAW := raw[J(res$SYMBOL, res$DATE), roll = T]$RAW]
  
  res[, "WINSORIZED" := GCAMCQT::winsorizing_median(RAW), by = DATE]
  res[, "NORMALIZED" := GCAMCQT::normalizing_zscore(WINSORIZED), by = DATE]
  res[, "IMPUTED" := GCAMCQT::imputing_with_mean(NORMALIZED), by = DATE]
  # 如果IMPUTED存在每天都是NA的情况，将其赋值0
  stat <- res[, .(IMPUTED_NA_PCT = sum(is.na(IMPUTED)) / .N,
                  RAW_NA_PCT = sum(is.na(RAW)) / .N), by = DATE]
  stopifnot(all(stat$PCT %in% c(0, 1)))
  if (any(stat$IMPUTED_NA_PCT == 1 & stat$RAW_NA_PCT < 1)) {
    fill_dates <- stat[IMPUTED_NA_PCT == 1 & RAW_NA_PCT < 1]$DATE
    res[DATE %in% fill_dates, IMPUTED := 0]
    GCAMCPUB::log_info("[", var, " IMPUTED]:  ", paste0(fill_dates, collapse = ","))
  }
  
  res <- res[, .(INNER_CODE, DATE, RAW, IMPUTED)]
}


# --------------------------------------------------------------------------------------------------------------------------------
# 涉及到环比和同比计算
# 环比的话直接采用原始数据shift计算
# 同比数据采用－365计算

# 因子规划
# 成长类因子：增长率采用同比 环比 区间同比 区间环比；增长加速度采用区间同比加速度 环比加速度
# 估值类因子：P/B为主进行构造
# 质量类因子：多维度综合

# 成长类 ---------------------------------------------------------------------------------------------------------------------
# 成长类考虑同比、环比、3年线性同比和环比、3年复合、3年环比增长加速度 

func_roe_growth_yoy <- function(var, dates, adj_dates, save_folder, uqer_folder, uqer_folder2) {
  
  roe <- read_raw_data(adj_dates, "ROE", uqer_folder)
  roe[is.na(RAW), RAW := NA_real_]
  roe <- roe[!is.na(RAW)]
  
  quarter_roe <- roe[, .(DATE = head(DATE, 1)), by = .(SYMBOL, RAW)]
  quarter_roe <- quarter_roe[!is.na(RAW)]
  setkey(quarter_roe, SYMBOL, DATE)
  
  quarter_roe[, PREV_YDATE := DATE - 365.5]
  quarter_roe[, PREV_YRAW := quarter_roe[J(quarter_roe$SYMBOL, quarter_roe$PREV_YDATE), roll = T]$RAW]
  quarter_roe[, RAW := RAW / PREV_YRAW - 1]
  setkey(quarter_roe, SYMBOL, DATE)
  
  # normal 
  quarter_roe[is.na(RAW) | is.infinite(RAW), RAW := NA_real_]
  raw <- normalize_raw(quarter_roe, dates)
  
  store_factor(var, from_to = range(dates), dates, raw, save_folder)
}


func_roe_growth_qoq <- function(var, dates, adj_dates, save_folder, uqer_folder, uqer_folder2) {
  
  roe <- read_raw_data(adj_dates, "ROE", uqer_folder)
  roe[is.na(RAW), RAW := NA_real_]
  roe <- roe[!is.na(RAW)]
  
  quarter_roe <- roe[, .(DATE = head(DATE, 1)), by = .(SYMBOL, RAW)]
  quarter_roe <- quarter_roe[!is.na(RAW)]
  setkey(quarter_roe, SYMBOL, DATE)
  
  quarter_roe[, PREV_QRAW := shift(RAW), by = .(SYMBOL)]
  quarter_roe[, RAW := RAW / PREV_QRAW - 1]
  setkey(quarter_roe, SYMBOL, DATE)
  
  # normal 
  quarter_roe[is.na(RAW) | is.infinite(RAW), RAW := NA_real_]
  raw <- normalize_raw(quarter_roe, dates)
  
  store_factor(var, from_to = range(dates), dates, raw, save_folder)
}


func_roe_growth_3y <- function(var, dates, adj_dates, save_folder, uqer_folder, uqer_folder2) {
  
  roe <- read_raw_data(adj_dates, "ROE", uqer_folder) 
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
  
  growth_roe_beta <- overlap_q_roe[, .(RAW = ifelse(.N > 1, as.numeric(GCAMCQT:::lm_growth(value)), NA_real_)), by = .(SYMBOL, DATE)]
  setkey(growth_roe_beta, SYMBOL, DATE)
  
  # normal 
  growth_roe_beta[is.na(RAW) | is.infinite(RAW), RAW := NA_real_]
  raw <- normalize_raw(growth_roe_beta, dates)
  
  store_factor(var, from_to = range(dates), dates, raw, save_folder)
}


func_roe_mix_growth_3y <- function(var, dates, adj_dates, save_folder, uqer_folder, uqer_folder2) {
  
  roe <- read_raw_data(adj_dates, "ROE", uqer_folder)
  roe[is.na(RAW), RAW := NA_real_]
  roe <- roe[!is.na(RAW)]
  
  quarter_roe <- roe[, .(DATE = head(DATE, 1)), by = .(SYMBOL, RAW)]
  quarter_roe <- quarter_roe[!is.na(RAW)]
  setkey(quarter_roe, SYMBOL, DATE)
  
  quarter_roe[, PREV_YDATE := DATE - 365.5*3]
  quarter_roe[, PREV_YRAW := quarter_roe[J(quarter_roe$SYMBOL, quarter_roe$PREV_YDATE), roll = T]$RAW]
  quarter_roe[, RAW := ((RAW - PREV_YRAW) / abs(PREV_YRAW) + 1) ^ (1/3) - 1]
  setkey(quarter_roe, SYMBOL, DATE)
  
  # normal 
  quarter_roe[is.na(RAW) | is.infinite(RAW), RAW := NA_real_]
  raw <- normalize_raw(quarter_roe, dates)
  
  store_factor(var, from_to = range(dates), dates, raw, save_folder)
}


func_roe_growth_12q <- function(var, dates, adj_dates, save_folder, uqer_folder, uqer_folder2) {
  
  roe <- read_raw_data(adj_dates, "ROE", uqer_folder) 
  roe[is.na(RAW), RAW := NA_real_]
  roe <- roe[!is.na(RAW)]
  
  quarter_roe <- roe[, .(DATE = head(DATE, 1)), by = .(SYMBOL, RAW)]
  quarter_roe <- quarter_roe[!is.na(RAW)]
  setkey(quarter_roe, SYMBOL, DATE)
  quarter_roe[, QUARTER := seq(.N), by = SYMBOL]
  
  overlap_q_roe <- overlapping(quarter_roe[, .(SYMBOL, QUARTER)], quarter_roe, 12)
  
  growth_roe_beta <- overlap_q_roe[, .(DATE = tail(DATE, 1),
                                       RAW = ifelse(.N > 6, as.numeric(GCAMCQT:::lm_growth(RAW)), NA_real_)), by = .(SYMBOL, QUARTER)]
  setkey(growth_roe_beta, SYMBOL, DATE)
  
  # normal 
  growth_roe_beta[is.na(RAW) | is.infinite(RAW), RAW := NA_real_]
  raw <- normalize_raw(growth_roe_beta, dates)
  
  store_factor(var, from_to = range(dates), dates, raw, save_folder)
}


func_roe_growth_acce_12q <- function(var, dates, adj_dates, save_folder, uqer_folder, uqer_folder2) {
  
  roe <- read_raw_data(adj_dates, "ROE", uqer_folder) 
  roe[is.na(RAW), RAW := NA_real_]
  roe <- roe[!is.na(RAW)]
  
  quarter_roe <- roe[, .(DATE = head(DATE, 1)), by = .(SYMBOL, RAW)]
  quarter_roe <- quarter_roe[!is.na(RAW)]
  setkey(quarter_roe, SYMBOL, DATE)
  quarter_roe[, QUARTER := seq(.N), by = SYMBOL]
  
  overlap_q_roe <- overlapping(quarter_roe[, .(SYMBOL, QUARTER)], quarter_roe, 12)
  
  growth_roe_beta <- overlap_q_roe[, .(DATE = tail(DATE, 1),
                                       RAW = ifelse(.N > 6, as.numeric(lm_growth_acce(RAW)), NA_real_)), by = .(SYMBOL, QUARTER)]
  setkey(growth_roe_beta, SYMBOL, DATE)
  
  # normal 
  growth_roe_beta[is.na(RAW) | is.infinite(RAW), RAW := NA_real_]
  raw <- normalize_raw(growth_roe_beta, dates)

  store_factor(var, from_to = range(dates), dates, raw, save_folder)
}


func_eps_growth_yoy <- function(var, dates, adj_dates, save_folder, uqer_folder, uqer_folder2) {
  
  eps <- read_raw_data(adj_dates, "EPS", uqer_folder)
  eps[is.na(RAW), RAW := NA_real_]
  eps <- eps[!is.na(RAW)]
  
  quarter_eps <- eps[, .(DATE = head(DATE, 1)), by = .(SYMBOL, RAW)]
  quarter_eps <- quarter_eps[!is.na(RAW)]
  setkey(quarter_eps, SYMBOL, DATE)
  
  quarter_eps[, PREV_YDATE := DATE - 365.5]
  quarter_eps[, PREV_YRAW := quarter_eps[J(quarter_eps$SYMBOL, quarter_eps$PREV_YDATE), roll = T]$RAW]
  quarter_eps[, RAW := RAW / PREV_YRAW - 1]
  setkey(quarter_eps, SYMBOL, DATE)
  
  # normal 
  quarter_eps[is.na(RAW) | is.infinite(RAW), RAW := NA_real_]
  raw <- normalize_raw(quarter_eps, dates)
  
  store_factor(var, from_to = range(dates), dates, raw, save_folder)
}


func_eps_growth_qoq <- function(var, dates, adj_dates, save_folder, uqer_folder, uqer_folder2) {
  
  eps <- read_raw_data(adj_dates, "EPS", uqer_folder)
  eps[is.na(RAW), RAW := NA_real_]
  eps <- eps[!is.na(RAW)]
  
  quarter_eps <- eps[, .(DATE = head(DATE, 1)), by = .(SYMBOL, RAW)]
  quarter_eps <- quarter_eps[!is.na(RAW)]
  setkey(quarter_eps, SYMBOL, DATE)
  
  quarter_eps[, PREV_QRAW := shift(RAW), by = .(SYMBOL)]
  quarter_eps[, RAW := RAW / PREV_QRAW - 1]
  setkey(quarter_eps, SYMBOL, DATE)
  
  # normal 
  quarter_eps[is.na(RAW) | is.infinite(RAW), RAW := NA_real_]
  raw <- normalize_raw(quarter_eps, dates)
  
  store_factor(var, from_to = range(dates), dates, raw, save_folder)
}


func_eps_growth_3y <- function(var, dates, adj_dates, save_folder, uqer_folder, uqer_folder2) {
  
  eps <- read_raw_data(adj_dates, "EPS", uqer_folder) 
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
  
  growth_eps_beta <- overlap_q_eps[, .(RAW = ifelse(.N > 1, as.numeric(GCAMCQT:::lm_growth(value)), NA_real_)), by = .(SYMBOL, DATE)]
  setkey(growth_eps_beta, SYMBOL, DATE)
  
  # normal 
  growth_eps_beta[is.na(RAW) | is.infinite(RAW), RAW := NA_real_]
  raw <- normalize_raw(growth_eps_beta, dates)
  
  store_factor(var, from_to = range(dates), dates, raw, save_folder)
}


func_eps_mix_growth_3y <- function(var, dates, adj_dates, save_folder, uqer_folder, uqer_folder2) {
  
  eps <- read_raw_data(adj_dates, "EPS", uqer_folder)
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
  raw <- normalize_raw(quarter_eps, dates)
  
  store_factor(var, from_to = range(dates), dates, raw, save_folder)
}


func_eps_growth_12q <- function(var, dates, adj_dates, save_folder, uqer_folder, uqer_folder2) {
  
  eps <- read_raw_data(adj_dates, "EPS", uqer_folder) 
  eps[is.na(RAW), RAW := NA_real_]
  eps <- eps[!is.na(RAW)]
  
  quarter_eps <- eps[, .(DATE = head(DATE, 1)), by = .(SYMBOL, RAW)]
  quarter_eps <- quarter_eps[!is.na(RAW)]
  setkey(quarter_eps, SYMBOL, DATE)
  quarter_eps[, QUARTER := seq(.N), by = SYMBOL]
  
  overlap_q_eps <- overlapping(quarter_eps[, .(SYMBOL, QUARTER)], quarter_eps, 12)
  
  growth_eps_beta <- overlap_q_eps[, .(DATE = tail(DATE, 1),
                                       RAW = ifelse(.N > 6, as.numeric(GCAMCQT:::lm_growth(RAW)), NA_real_)), by = .(SYMBOL, QUARTER)]
  setkey(growth_eps_beta, SYMBOL, DATE)
  
  # normal 
  growth_eps_beta[is.na(RAW) | is.infinite(RAW), RAW := NA_real_]
  raw <- normalize_raw(growth_eps_beta, dates)
  
  store_factor(var, from_to = range(dates), dates, raw, save_folder)
}


func_eps_growth_acce_12q <- function(var, dates, adj_dates, save_folder, uqer_folder, uqer_folder2) {
  
  eps <- read_raw_data(adj_dates, "EPS", uqer_folder) 
  eps[is.na(RAW), RAW := NA_real_]
  eps <- eps[!is.na(RAW)]
  
  quarter_eps <- eps[, .(DATE = head(DATE, 1)), by = .(SYMBOL, RAW)]
  quarter_eps <- quarter_eps[!is.na(RAW)]
  setkey(quarter_eps, SYMBOL, DATE)
  quarter_eps[, QUARTER := seq(.N), by = SYMBOL]
  
  overlap_q_eps <- overlapping(quarter_eps[, .(SYMBOL, QUARTER)], quarter_eps, 12)
  
  growth_eps_beta <- overlap_q_eps[, .(DATE = tail(DATE, 1),
                                       RAW = ifelse(.N > 6, as.numeric(lm_growth_acce(RAW)), NA_real_)), by = .(SYMBOL, QUARTER)]
  setkey(growth_eps_beta, SYMBOL, DATE)
  
  # normal 
  growth_eps_beta[is.na(RAW) | is.infinite(RAW), RAW := NA_real_]
  raw <- normalize_raw(growth_eps_beta, dates)
  
  store_factor(var, from_to = range(dates), dates, raw, save_folder)
}


func_totalassets_growth_yoy <- function(var, dates, adj_dates, save_folder, uqer_folder, uqer_folder2) {
  
  ta <- read_raw_data(adj_dates, "TotalAssets", uqer_folder2)
  ta[is.na(RAW), RAW := NA_real_]
  ta <- ta[!is.na(RAW)]
  
  quarter_ta <- ta[, .(DATE = head(DATE, 1)), by = .(SYMBOL, RAW)]
  quarter_ta <- quarter_ta[!is.na(RAW)]
  setkey(quarter_ta, SYMBOL, DATE)
  
  quarter_ta[, PREV_YDATE := DATE - 365.5]
  quarter_ta[, PREV_YRAW := quarter_ta[J(quarter_ta$SYMBOL, quarter_ta$PREV_YDATE), roll = T]$RAW]
  quarter_ta[, RAW := RAW / PREV_YRAW - 1]
  setkey(quarter_ta, SYMBOL, DATE)
  
  # normal 
  quarter_ta[is.na(RAW) | is.infinite(RAW), RAW := NA_real_]
  raw <- normalize_raw(quarter_ta, dates)
  
  store_factor(var, from_to = range(dates), dates, raw, save_folder)
}


func_totalassets_growth_qoq <- function(var, dates, adj_dates, save_folder, uqer_folder, uqer_folder2) {
  
  ta <- read_raw_data(adj_dates, "TotalAssets", uqer_folder2)
  ta[is.na(RAW), RAW := NA_real_]
  ta <- ta[!is.na(RAW)]
  
  quarter_ta <- ta[, .(DATE = head(DATE, 1)), by = .(SYMBOL, RAW)]
  quarter_ta <- quarter_ta[!is.na(RAW)]
  setkey(quarter_ta, SYMBOL, DATE)
  
  quarter_ta[, PREV_QRAW := shift(RAW), by = .(SYMBOL)]
  quarter_ta[, RAW := RAW / PREV_QRAW - 1]
  setkey(quarter_ta, SYMBOL, DATE)
  
  # normal 
  quarter_ta[is.na(RAW) | is.infinite(RAW), RAW := NA_real_]
  raw <- normalize_raw(quarter_ta, dates)
  
  store_factor(var, from_to = range(dates), dates, raw, save_folder)
}


func_totalassets_growth_3y <- function(var, dates, adj_dates, save_folder, uqer_folder, uqer_folder2) {
  
  ta <- read_raw_data(adj_dates, "TotalAssets", uqer_folder2) 
  ta[is.na(RAW), RAW := NA_real_]
  ta <- ta[!is.na(RAW)]
  
  quarter_ta <- ta[, .(DATE = head(DATE, 1)), by = .(SYMBOL, RAW)]
  quarter_ta <- quarter_ta[!is.na(RAW)]
  setkey(quarter_ta, SYMBOL, DATE)
  
  quarter_ta[, PREV_YDATE := DATE - 365.5]
  quarter_ta[, PREV_YDATE2 := DATE - 365.5*2]
  quarter_ta[, PREV_YRAW := quarter_ta[J(quarter_ta$SYMBOL, quarter_ta$PREV_YDATE), roll = T]$RAW]
  quarter_ta[, PREV_YRAW2 := quarter_ta[J(quarter_ta$SYMBOL, quarter_ta$PREV_YDATE2), roll = T]$RAW]
  
  overlap_q_ta <- melt.data.table(quarter_ta[, .(SYMBOL, DATE, RAW, PREV_YRAW, PREV_YRAW2)], id.vars = c("SYMBOL", "DATE"))
  overlap_q_ta[, RK := seq(.N), by = .(SYMBOL, DATE)]
  setorder(overlap_q_ta, SYMBOL, DATE, -RK)
  
  growth_ta_beta <- overlap_q_ta[, .(RAW = ifelse(.N > 1, as.numeric(GCAMCQT:::lm_growth(value)), NA_real_)), by = .(SYMBOL, DATE)]
  setkey(growth_ta_beta, SYMBOL, DATE)
  
  # normal 
  growth_ta_beta[is.na(RAW) | is.infinite(RAW), RAW := NA_real_]
  raw <- normalize_raw(growth_ta_beta, dates)
  
  store_factor(var, from_to = range(dates), dates, raw, save_folder)
}


func_totalassets_mix_growth_3y <- function(var, dates, adj_dates, save_folder, uqer_folder, uqer_folder2) {
  
  ta <- read_raw_data(adj_dates, "TotalAssets", uqer_folder2)
  ta[is.na(RAW), RAW := NA_real_]
  ta <- ta[!is.na(RAW)]
  
  quarter_ta<- ta[, .(DATE = head(DATE, 1)), by = .(SYMBOL, RAW)]
  quarter_ta <- quarter_ta[!is.na(RAW)]
  setkey(quarter_ta, SYMBOL, DATE)
  
  quarter_ta[, PREV_YDATE := DATE - 365.5*3]
  quarter_ta[, PREV_YRAW := quarter_ta[J(quarter_ta$SYMBOL, quarter_ta$PREV_YDATE), roll = T]$RAW]
  quarter_ta[, RAW := ((RAW - PREV_YRAW) / abs(PREV_YRAW) + 1) ^ (1/3) - 1]
  setkey(quarter_ta, SYMBOL, DATE)
  
  # normal 
  quarter_ta[is.na(RAW) | is.infinite(RAW), RAW := NA_real_]
  raw <- normalize_raw(quarter_ta, dates)
  
  store_factor(var, from_to = range(dates), dates, raw, save_folder)
}


func_totalassets_growth_12q <- function(var, dates, adj_dates, save_folder, uqer_folder, uqer_folder2) {
  
  ta <- read_raw_data(adj_dates, "TotalAssets", uqer_folder2) 
  ta[is.na(RAW), RAW := NA_real_]
  ta <- ta[!is.na(RAW)]
  
  quarter_ta <- ta[, .(DATE = head(DATE, 1)), by = .(SYMBOL, RAW)]
  quarter_ta <- quarter_ta[!is.na(RAW)]
  setkey(quarter_ta, SYMBOL, DATE)
  quarter_ta[, QUARTER := seq(.N), by = SYMBOL]
  
  overlap_q_ta <- overlapping(quarter_ta[, .(SYMBOL, QUARTER)], quarter_ta, 12)
  
  growth_ta_beta <- overlap_q_ta[, .(DATE = tail(DATE, 1),
                                       RAW = ifelse(.N > 6, as.numeric(GCAMCQT:::lm_growth(RAW)), NA_real_)), by = .(SYMBOL, QUARTER)]
  setkey(growth_ta_beta, SYMBOL, DATE)
  
  # normal 
  growth_ta_beta[is.na(RAW) | is.infinite(RAW), RAW := NA_real_]
  raw <- normalize_raw(growth_ta_beta, dates)
  
  store_factor(var, from_to = range(dates), dates, raw, save_folder)
}


func_totalassets_growth_acce_12q <- function(var, dates, adj_dates, save_folder, uqer_folder, uqer_folder2) {
  
  ta <- read_raw_data(adj_dates, "TotalAssets", uqer_folder2) 
  ta[is.na(RAW), RAW := NA_real_]
  ta <- ta[!is.na(RAW)]
  
  quarter_ta <- ta[, .(DATE = head(DATE, 1)), by = .(SYMBOL, RAW)]
  quarter_ta <- quarter_ta[!is.na(RAW)]
  setkey(quarter_ta, SYMBOL, DATE)
  quarter_ta[, QUARTER := seq(.N), by = SYMBOL]
  
  overlap_q_ta <- overlapping(quarter_ta[, .(SYMBOL, QUARTER)], quarter_ta, 12)
  
  growth_ta_beta <- overlap_q_ta[, .(DATE = tail(DATE, 1),
                                       RAW = ifelse(.N > 6, as.numeric(lm_growth_acce(RAW)), NA_real_)), by = .(SYMBOL, QUARTER)]
  setkey(growth_ta_beta, SYMBOL, DATE)
  
  # normal 
  growth_ta_beta[is.na(RAW) | is.infinite(RAW), RAW := NA_real_]
  raw <- normalize_raw(growth_ta_beta, dates)
  
  store_factor(var, from_to = range(dates), dates, raw, save_folder)
}


# 质量类 --------------------------------------------------------------------------------------------------------------------------------------------

# 因子来源：自行构造
func_roe_sharpe_12q <- function(var, dates, adj_dates, save_folder, uqer_folder, uqer_folder2) {
  
  roe <- read_raw_data(adj_dates, "ROE", uqer_folder) 
  roe[is.na(RAW), RAW := NA_real_]
  roe <- roe[!is.na(RAW)]
  
  quarter_roe <- roe[, .(DATE = head(DATE, 1)), by = .(SYMBOL, RAW)]
  quarter_roe <- quarter_roe[!is.na(RAW)]
  setkey(quarter_roe, SYMBOL, DATE)
  quarter_roe[, QUARTER := seq(.N), by = SYMBOL]
  
  overlap_q_roe <- overlapping(quarter_roe[, .(SYMBOL, QUARTER)], quarter_roe, 12)
  
  roe_sharpe <- overlap_q_roe[, .(DATE = tail(DATE, 1),
                                  RAW = ifelse(.N > 6, mean(RAW, na.rm = T) / sd(RAW, na.rm = T), NA_real_)), by = .(SYMBOL, QUARTER)]
  setkey(roe_sharpe, SYMBOL, DATE)
  
  # normal 
  roe_sharpe[is.na(RAW) | is.infinite(RAW), RAW := NA_real_]
  raw <- normalize_raw(roe_sharpe, dates)
  
  store_factor(var, from_to = range(dates), dates, raw, save_folder)
}

# 因子来源：自行构造
func_eps_sharpe_12q <- function(var, dates, adj_dates, save_folder, uqer_folder, uqer_folder2) {
  
  eps <- read_raw_data(adj_dates, "EPS", uqer_folder) 
  eps[is.na(RAW), RAW := NA_real_]
  eps <- eps[!is.na(RAW)]
  
  quarter_eps <- eps[, .(DATE = head(DATE, 1)), by = .(SYMBOL, RAW)]
  quarter_eps <- quarter_eps[!is.na(RAW)]
  setkey(quarter_eps, SYMBOL, DATE)
  quarter_eps[, QUARTER := seq(.N), by = SYMBOL]
  
  overlap_q_eps <- overlapping(quarter_eps[, .(SYMBOL, QUARTER)], quarter_eps, 12)
  
  eps_sharpe <- overlap_q_eps[, .(DATE = tail(DATE, 1),
                                  RAW = ifelse(.N > 6, mean(RAW, na.rm = T) / sd(RAW, na.rm = T), NA_real_)), by = .(SYMBOL, QUARTER)]
  setkey(eps_sharpe, SYMBOL, DATE)
  
  # normal 
  eps_sharpe[is.na(RAW) | is.infinite(RAW), RAW := NA_real_]
  raw <- normalize_raw(eps_sharpe, dates)
  
  store_factor(var, from_to = range(dates), dates, raw, save_folder)
}


# 因子来源：公众号 经营性现金流净额占比
# 其中最近12个月现金及现金等价物TTM 由因子现金比率 = 最近12个月现金及现金等价物TTM/（带息流动负债 + 无息流动负债）反推
func_netoperate_cf_liability <- function(var, dates, adj_dates, save_folder, uqer_folder, uqer_folder2) {
  
  netoperate_cf <- read_raw_data(adj_dates, "NetOperateCFTTM", uqer_folder2)
  cashtocurrentliability <- read_raw_data(adj_dates, "CashToCurrentLiability", uqer_folder)
  intcl <- read_raw_data(adj_dates, "IntCL", uqer_folder2)
  intfreecl <- read_raw_data(adj_dates, "IntFreeCL", uqer_folder2)
  cash <- cashtocurrentliability[intcl, on = c("SYMBOL", "DATE")][intfreecl, on = c("SYMBOL", "DATE")]
  setnames(cash, c("SYMBOL", "DATE", "CashToCurrentLiability", "IntCL", "IntFreeCL"))
  cash[, Cash := (IntCL + IntFreeCL) * CashToCurrentLiability]
  
  raw <- cash[netoperate_cf, on = c("SYMBOL", "DATE")]
  raw <- raw[, .(SYMBOL, DATE, NetOperateCFTTM = RAW, Cash)]
  setkey(raw, SYMBOL, DATE)
  
  raw[, PREV_Cash := raw[J(raw$SYMBOL, raw$DATE - 365), roll = T]$Cash]
  raw[, DIFF_Cash := Cash - PREV_Cash]
  raw[, RAW := NetOperateCFTTM / DIFF_Cash]
  setkey(raw, SYMBOL, DATE)
  
  # normal 
  raw[is.na(RAW) | is.infinite(RAW), RAW := NA_real_]
  raw <- normalize_raw(raw, dates)
  
  store_factor(var, from_to = range(dates), dates, raw, save_folder)
  
}


func_cash_ratio <- function(var, dates, adj_dates, save_folder, uqer_folder, uqer_folder2) {
  
  CashEquivalentPS <- read_raw_data(adj_dates, "CashEquivalentPS", uqer_folder2)
  NetAssetPS <- read_raw_data(adj_dates, "NetAssetPS", uqer_folder)
  
  raw <- CashEquivalentPS[NetAssetPS, on = c("SYMBOL", "DATE")]
  setnames(raw, c("SYMBOL", "DATE", "CashEquivalentPS", "NetAssetPS"))
  raw[, RAW := CashEquivalentPS / NetAssetPS]
  cash_ratio <- raw[,.(SYMBOL, DATE, RAW)]
  setkey(cash_ratio, SYMBOL, DATE)
  
  # normal 
  cash_ratio[is.na(RAW) | is.infinite(RAW), RAW := NA_real_]
  raw <- normalize_raw(cash_ratio, dates)
  
  store_factor(var, from_to = range(dates), dates, raw, save_folder)
}


func_teap_ratio <- function(var, dates, adj_dates, save_folder, uqer_folder, uqer_folder2) {
  
  TotalAssets <- read_raw_data(adj_dates, "TotalAssets", uqer_folder2)
  TEAP <- read_raw_data(adj_dates, "TEAP", uqer_folder2)
  
  raw <- TotalAssets[TEAP, on = c("SYMBOL", "DATE")]
  setnames(raw, c("SYMBOL", "DATE", "TotalAssets", "TEAP"))

  raw[, RAW := TEAP / TotalAssets]
  setkey(raw, SYMBOL, DATE)
  
  # normal 
  raw[is.na(RAW) | is.infinite(RAW), RAW := NA_real_]
  raw <- normalize_raw(raw, dates)
  
  store_factor(var, from_to = range(dates), dates, raw, save_folder)
}


func_interest_cover <- function(var, dates, adj_dates, save_folder, uqer_folder, uqer_folder2) {
  
  EBIT <- read_raw_data(adj_dates, "EBIT", uqer_folder2)
  FinanExpenseTTM <- read_raw_data(adj_dates, "FinanExpenseTTM", uqer_folder2)
  
  raw <- EBIT[FinanExpenseTTM, on = c("SYMBOL", "DATE")]
  setnames(raw, c("SYMBOL", "DATE", "EBIT", "FinanExpenseTTM"))
  
  raw[, RAW := EBIT / FinanExpenseTTM]
  setkey(raw, SYMBOL, DATE)
  
  # normal 
  raw[is.na(RAW) | is.infinite(RAW), RAW := NA_real_]
  raw <- normalize_raw(raw, dates)
  
  store_factor(var, from_to = range(dates), dates, raw, save_folder)
}


func_accrued_surplus <- function(var, dates, adj_dates, save_folder, uqer_folder, uqer_folder2) {
  
  TProfitTTM <- read_raw_data(adj_dates, "TProfitTTM", uqer_folder2)
  NetOperateCFTTM <- read_raw_data(adj_dates, "NetOperateCFTTM", uqer_folder2)
  TotalAssets <- read_raw_data(adj_dates, "TotalAssets", uqer_folder2)
  
  raw <- TProfitTTM[NetOperateCFTTM, on = c("SYMBOL", "DATE")][TotalAssets, on = c("SYMBOL", "DATE")]
  setnames(raw, c("SYMBOL", "DATE", "TProfitTTM", "NetOperateCFTTM", "TotalAssets"))
  raw[, ACCURED := TProfitTTM - NetOperateCFTTM]
  raw <- raw[!(is.na(TotalAssets)|is.na(ACCURED))]
  
  quarter_cr <- raw[, .(DATE = head(DATE, 1)), by = .(SYMBOL, TotalAssets, ACCURED)]
  quarter_cr[, PREV_ASSET := shift(TotalAssets), by=.(SYMBOL)]
  quarter_cr[, MASSET := (TotalAssets+PREV_ASSET)/2]
  quarter_cr[is.na(MASSET), MASSET := TotalAssets]
  quarter_cr[, RAW := ACCURED / MASSET]
  setkey(quarter_cr, SYMBOL, DATE)
  
  # normal 
  quarter_cr[is.na(RAW) | is.infinite(RAW), RAW := NA_real_]
  raw <- normalize_raw(quarter_cr, dates)
  
  store_factor(var, from_to = range(dates), dates, raw, save_folder)
}


# 盈利类 ----------------------------------------------------------------------------------------------------------------------------------------------------------

# 因子来源：公众号 单季度利润率
function_profit_margin_ttm <- function(var, dates, adj_dates, save_folder, uqer_folder, uqer_folder2) {
  
  raw_netprofit <- read_raw_data(adj_dates, "NetProfitTTM", uqer_folder2)
  raw_revenue <- read_raw_data(adj_dates, "RevenueTTM", uqer_folder2)
  raw <- raw_netprofit[raw_revenue, on = c("SYMBOL", "DATE")]
  setnames(raw, c("SYMBOL", "DATE", "NetProfitTTM", "RevenueTTM"))
  
  raw[is.na(NetProfitTTM), NetProfitTTM := NA_real_]
  raw[is.na(RevenueTTM), RevenueTTM := NA_real_]
  raw <- raw[!(is.na(NetProfitTTM)|is.na(RevenueTTM))]
  
  # 获取因子值
  raw[, RAW := NetProfitTTM / RevenueTTM]
  setkey(raw, SYMBOL, DATE)
  
  # normal 
  raw[is.na(RAW) | is.infinite(RAW), RAW := NA_real_]
  raw <- normalize_raw(raw, dates)
  
  store_factor(var, from_to = range(dates), dates, raw, save_folder)
}

# 因子来源：公众号 NIAPcut / TEAP
func_niapcut_ratio <- function(var, dates, adj_dates, save_folder, uqer_folder, uqer_folder2) {
  
  raw_NIAPCut <- read_raw_data(adj_dates, "NIAPCut", uqer_folder2)
  raw_TEAP <- read_raw_data(adj_dates, "TEAP", uqer_folder2)
  
  raw <- raw_NIAPCut[raw_TEAP, on = c("SYMBOL", "DATE")]
  setnames(raw, c("SYMBOL", "DATE", "NIAPCut", "TEAP"))
  
  quarter_ratio <- raw[, .(DATE = head(DATE, 1)), by = .(SYMBOL, NIAPCut, TEAP)]
  quarter_ratio <- quarter_ratio[!(is.na(NIAPCut)|is.na(TEAP))]
  setkey(quarter_ratio, SYMBOL, DATE)
  
  quarter_ratio[, PREV_TEAP := shift(TEAP), by=.(SYMBOL)]
  quarter_ratio[, MTEAP := (PREV_TEAP + TEAP) / 2]
  quarter_ratio <- quarter_ratio[is.na(MTEAP),MTEAP := TEAP]
  
  quarter_ratio[, RAW := NIAPCut / MTEAP]
  setkey(quarter_ratio, SYMBOL, DATE)
  
  # normal 
  quarter_ratio[is.na(RAW) | is.infinite(RAW), RAW := NA_real_]
  raw <- normalize_raw(quarter_ratio, dates)
  
  store_factor(var, from_to = range(dates), dates, raw, save_folder)
}


func_netprofit_ratio <- function(var, dates, adj_dates, save_folder, uqer_folder, uqer_folder2) {
  
  raw_NetProfitAPTTM <- read_raw_data(adj_dates, "NetProfitAPTTM", uqer_folder2)
  raw_TEAP <- read_raw_data(adj_dates, "TEAP", uqer_folder2)
  
  raw <- raw_NetProfitAPTTM[raw_TEAP, on = c("SYMBOL", "DATE")]
  setnames(raw, c("SYMBOL", "DATE", "NetProfitAPTTM", "TEAP"))
  
  quarter_profitratio <- raw[, .(DATE = head(DATE, 1)), by = .(SYMBOL, TEAP, NetProfitAPTTM)]
  quarter_profitratio <- quarter_profitratio[!(is.na(TEAP)|is.na(NetProfitAPTTM))]
  setkey(quarter_profitratio, SYMBOL, DATE)
  
  quarter_profitratio[, PREV_TEAP := shift(TEAP), by=.(SYMBOL)]
  quarter_profitratio[,MTEAP := (PREV_TEAP + TEAP) / 2]
  quarter_profitratio[is.na(MTEAP), MTEAP := TEAP]
  
  quarter_profitratio[,RAW := NetProfitAPTTM / MTEAP]
  setkey(quarter_profitratio, SYMBOL, DATE)
  
  # normal 
  quarter_profitratio[is.na(RAW) | is.infinite(RAW), RAW := NA_real_]
  raw <- normalize_raw(quarter_profitratio, dates)
  
  store_factor(var, from_to = range(dates), dates, raw, save_folder)
}



# 估值类 ------------------------------------------------------------------------------------------------------------------------------------------------------
# 因子来源：公众号 sale/企业价值
func_sp <- function(var, dates, adj_dates, save_folder, uqer_folder, uqer_folder2) {

  raw_sales <- read_raw_data(adj_dates, "TRevenueTTM", uqer_folder2)
  raw_mktvalue <- read_raw_data(adj_dates, "MktValue", uqer_folder2)

  raw <- raw_sales[raw_mktvalue, on = c("SYMBOL", "DATE")]
  setnames(raw, c("SYMBOL", "DATE", "REVENUE", "MKTVALUE"))
  raw[, RAW := REVENUE / MKTVALUE]
  setkey(raw, SYMBOL, DATE)
  
  # normal 
  raw[is.na(RAW) | is.infinite(RAW), RAW := NA_real_]
  raw <- normalize_raw(raw, dates)
  
  store_factor(var, from_to = range(dates), dates, raw, save_folder)
}

# 因子来源：公众号 TEAP/企业价值
func_bp <- function(var, dates, adj_dates, save_folder, uqer_folder, uqer_folder2) {
  
  raw_teap <- read_raw_data(adj_dates, "TEAP", uqer_folder2)
  raw_mktvalue <- read_raw_data(adj_dates, "MktValue", uqer_folder2)
  
  raw <- raw_teap[raw_mktvalue, on = c("SYMBOL", "DATE")]
  setnames(raw, c("SYMBOL", "DATE", "TEAP", "MKTVALUE"))
  raw[, RAW := TEAP / MKTVALUE]
  setkey(raw, SYMBOL, DATE)
  
  # normal 
  raw[is.na(RAW) | is.infinite(RAW), RAW := NA_real_]
  raw <- normalize_raw(raw, dates)
  
  store_factor(var, from_to = range(dates), dates, raw, save_folder)
}

# 因子来源：公众号 NIAP/企业价值
func_ep <- function(var, dates, adj_dates, save_folder, uqer_folder, uqer_folder2) {
  
  raw_niap <- read_raw_data(adj_dates, "NIAP", uqer_folder2)
  raw_mktvalue <- read_raw_data(adj_dates, "MktValue", uqer_folder2)
  
  raw <- raw_niap[raw_mktvalue, on = c("SYMBOL", "DATE")]
  setnames(raw, c("SYMBOL", "DATE", "NIAP", "MKTVALUE"))
  raw[, RAW := NIAP / MKTVALUE]
  setkey(raw, SYMBOL, DATE)
  
  # normal 
  raw[is.na(RAW) | is.infinite(RAW), RAW := NA_real_]
  raw <- normalize_raw(raw, dates)
  
  store_factor(var, from_to = range(dates), dates, raw, save_folder)
}


# other --------------------------------------------------------------------------------------------------------------------------------------------------------

# 因子来源：公众号 线性纯化利润
func_revenue_cost_residual <- function(var, dates, adj_dates, save_folder, uqer_folder, uqer_folder2) {
  
  raw_revenue <- read_raw_data(adj_dates, "RevenueTTM", uqer_folder2)
  raw_cost <- read_raw_data(adj_dates, "CostTTM", uqer_folder2)
  raw <- raw_revenue[raw_cost, on = c("SYMBOL", "DATE")]
  setnames(raw, c("SYMBOL", "DATE", "RevenueTTM", "CostTTM"))
  
  raw[is.na(RevenueTTM), RevenueTTM := NA_real_]
  raw[is.na(CostTTM), CostTTM := NA_real_]
  raw <- raw[!(is.na(CostTTM)|is.na(RevenueTTM))]
  
  quarter_revenue_chg <- raw[, .(DATE = head(DATE, 1)), by = .(SYMBOL, RevenueTTM, CostTTM)]
  quarter_revenue_chg <- quarter_revenue_chg[!(is.na(CostTTM)|is.na(RevenueTTM))]
  quarter_revenue_chg[, QUARTER := seq(.N), by = SYMBOL]
  
  overlap_revenue_chg <- overlapping(quarter_revenue_chg[, .(SYMBOL, QUARTER)], quarter_revenue_chg, 6)
  #获得因子值
  revenue_chg_res <- overlap_revenue_chg[,.(DATE = tail(DATE, 1),
                                            RAW = ifelse(.N == 6, as.numeric(lm_res(RevenueTTM, CostTTM)), NA_real_)),by=.(SYMBOL,QUARTER)]
  setkey(revenue_chg_res, SYMBOL, DATE)
  
  # normal 
  revenue_chg_res[is.na(RAW) | is.infinite(RAW), RAW := NA_real_]
  raw <- normalize_raw(revenue_chg_res, dates)
  
  store_factor(var, from_to = range(dates), dates, raw, save_folder)
  
}


# 因子来源：公众号 标准化营业利润
func_normlize_op <- function(var, dates, adj_dates, save_folder, uqer_folder, uqer_folder2) {
  
  raw_op <- read_raw_data(adj_dates, "OperateProfitTTM", uqer_folder2)
  raw_op[is.na(RAW), RAW := NA_real_]
  raw_op <- raw_op[!is.na(RAW)]
  
  quarter_op <- raw_op[, .(DATE = head(DATE, 1)), by = .(SYMBOL, RAW)]
  quarter_op <- quarter_op[!is.na(RAW)]
  setkey(quarter_op, SYMBOL, DATE)
  quarter_op[, QUARTER := seq(.N), by = SYMBOL]
  
  overlap_quarter_op <- overlapping(quarter_op[, .(SYMBOL, QUARTER)], quarter_op, 6)
  #获得因子值
  norm_op <- overlap_quarter_op[, .(DATE = tail(DATE, 1),
                                    RAW = ifelse(.N == 6, (tail(RAW, 1) - mean(RAW, na.rm = T)) / sd(RAW, na.rm = T), NA_real_)),by = .(SYMBOL, QUARTER)]
  setkey(norm_op, SYMBOL, DATE)
  
  # normal 
  norm_op[is.na(RAW) | is.infinite(RAW), RAW := NA_real_]
  raw <- normalize_raw(norm_op, dates)
  
  store_factor(var, from_to = range(dates), dates, raw, save_folder)
}


# 因子来源：公众号 标准化净利润
func_normlize_np <- function(var, dates, adj_dates, save_folder, uqer_folder, uqer_folder2) {
  
  raw_np <- read_raw_data(adj_dates, "NetProfitTTM", uqer_folder2)
  raw_np[is.na(RAW), RAW := NA_real_]
  raw_np <- raw_np[!is.na(RAW)]
  
  quarter_np <- raw_np[, .(DATE = head(DATE, 1)), by = .(SYMBOL, RAW)]
  quarter_np <- quarter_np[!is.na(RAW)]
  setkey(quarter_np, SYMBOL, DATE)
  quarter_np[, QUARTER := seq(.N), by = SYMBOL]
  
  overlap_quarter_np <- overlapping(quarter_np[, .(SYMBOL, QUARTER)], quarter_np, 6)
  #获得因子值
  norm_np <- overlap_quarter_np[, .(DATE = tail(DATE, 1),
                                    RAW = ifelse(.N == 6, (tail(RAW, 1) - mean(RAW, na.rm = T)) / sd(RAW, na.rm = T), NA_real_)),by = .(SYMBOL, QUARTER)]
  setkey(norm_np, SYMBOL, DATE)
  
  # normal 
  norm_np[is.na(RAW) | is.infinite(RAW), RAW := NA_real_]
  raw <- normalize_raw(norm_np, dates)
  
  store_factor(var, from_to = range(dates), dates, raw, save_folder)
}


# 因子来源：公众号 毛利率增速-营业收入增速
func_gross_yoy_to_revenue_yoy <- function(var, dates, adj_dates, save_folder, uqer_folder, uqer_folder2) {
  
  raw_gross <- read_raw_data(adj_dates, "GrossProfitTTM", uqer_folder2)
  raw_revenue <- read_raw_data(adj_dates, "RevenueTTM", uqer_folder2)
  
  raw_gross[is.na(RAW), RAW := NA_real_]
  quarter_gross <- raw_gross[, .(DATE = head(DATE, 1)), by = .(SYMBOL, RAW)]
  quarter_gross <- quarter_gross[!is.na(RAW)]
  setkey(quarter_gross, SYMBOL, DATE)
  
  quarter_gross[, PREV_YDATE := DATE - 365.5]
  quarter_gross[, PREV_YRAW := quarter_gross[J(quarter_gross$SYMBOL, quarter_gross$PREV_YDATE), roll = T]$RAW]
  quarter_gross[, RAW := RAW / PREV_YRAW - 1]
  setkey(quarter_gross, SYMBOL, DATE)
  raw_gross_yoy <- normalize_raw(quarter_gross, dates)
  
  
  raw_revenue[is.na(RAW), RAW := NA_real_]
  quarter_revenue <- raw_revenue[, .(DATE = head(DATE, 1)), by = .(SYMBOL, RAW)]
  quarter_revenue <- quarter_revenue[!is.na(RAW)]
  setkey(quarter_revenue, SYMBOL, DATE)
  
  quarter_revenue[, PREV_YDATE := DATE - 365.5]
  quarter_revenue[, PREV_YRAW := quarter_revenue[J(quarter_revenue$SYMBOL, quarter_revenue$PREV_YDATE), roll = T]$RAW]
  quarter_revenue[, RAW := RAW / PREV_YRAW - 1]
  setkey(quarter_revenue, SYMBOL, DATE)
  raw_revenue_yoy <- normalize_raw(quarter_revenue, dates)
  
  raw <- raw_gross_yoy[raw_revenue_yoy, on = c("INNER_CODE", "DATE")]
  raw <- raw[, .(SYMBOL = substr(to_jydb_symbol(INNER_CODE), 1, 6), DATE, RAW = RAW - i.RAW)]
  setkey(raw, SYMBOL, DATE)
  
  # normal 
  raw[is.na(RAW) | is.infinite(RAW), RAW := NA_real_]
  raw <- normalize_raw(raw, dates)
  
  store_factor(var, from_to = range(dates), dates, raw, save_folder)
}


# 因子来源：公众号 销售收入增速 - 成本增速
func_revenue_yoy_to_cost_yoy <- function(var, dates, adj_dates, save_folder, uqer_folder, uqer_folder2) {
  
  raw_cost <- read_raw_data(adj_dates, "CostTTM", uqer_folder2)
  raw_revenue <- read_raw_data(adj_dates, "RevenueTTM", uqer_folder2)
  
  raw_cost[is.na(RAW), RAW := NA_real_]
  quarter_cost <- raw_cost[, .(DATE = head(DATE, 1)), by = .(SYMBOL, RAW)]
  quarter_cost <- quarter_cost[!is.na(RAW)]
  setkey(quarter_cost, SYMBOL, DATE)
  
  quarter_cost[, PREV_YDATE := DATE - 365.5]
  quarter_cost[, PREV_YRAW := quarter_cost[J(quarter_cost$SYMBOL, quarter_cost$PREV_YDATE), roll = T]$RAW]
  quarter_cost[, RAW := RAW / PREV_YRAW - 1]
  setkey(quarter_cost, SYMBOL, DATE)
  raw_cost_yoy <- normalize_raw(quarter_cost, dates)
  
  
  raw_revenue[is.na(RAW), RAW := NA_real_]
  quarter_revenue <- raw_revenue[, .(DATE = head(DATE, 1)), by = .(SYMBOL, RAW)]
  quarter_revenue <- quarter_revenue[!is.na(RAW)]
  setkey(quarter_revenue, SYMBOL, DATE)
  
  quarter_revenue[, PREV_YDATE := DATE - 365.5]
  quarter_revenue[, PREV_YRAW := quarter_revenue[J(quarter_revenue$SYMBOL, quarter_revenue$PREV_YDATE), roll = T]$RAW]
  quarter_revenue[, RAW := RAW / PREV_YRAW - 1]
  setkey(quarter_revenue, SYMBOL, DATE)
  raw_revenue_yoy <- normalize_raw(quarter_revenue, dates)
  
  raw <- raw_cost_yoy[raw_revenue_yoy, on = c("INNER_CODE", "DATE")]
  raw <- raw[, .(SYMBOL = substr(to_jydb_symbol(INNER_CODE), 1, 6), DATE, RAW = i.RAW - RAW)]
  setkey(raw, SYMBOL, DATE)
  
  # normal 
  raw[is.na(RAW) | is.infinite(RAW), RAW := NA_real_]
  raw <- normalize_raw(raw, dates)
  
  store_factor(var, from_to = range(dates), dates, raw, save_folder)
}


func_roe_equity_chg <- function(var, dates, adj_dates, save_folder, uqer_folder, uqer_folder2) {
  
  raw_niap <- read_raw_data(adj_dates, "NIAP", uqer_folder2)
  raw_teap <- read_raw_data(adj_dates, "TEAP", uqer_folder2)
  
  raw <- raw_niap[raw_teap, on = c("SYMBOL", "DATE")]
  setnames(raw, c("SYMBOL", "DATE", "NIAP", "TEAP"))
  raw[, ROE := NIAP / TEAP]
  setkey(raw, SYMBOL, DATE)
  
  raw[, PREV_YDATE := DATE - 365.5]
  raw[, PREV_TEAP := raw[J(raw$SYMBOL, raw$PREV_YDATE), roll = T]$TEAP]
  raw[, PREV_ROE := raw[J(raw$SYMBOL, raw$PREV_YDATE), roll = T]$ROE]
  
  raw[, `:=` (ROE_CHG = (ROE - PREV_ROE) / abs(PREV_ROE),
                      TEAP_CHG = (TEAP - PREV_TEAP) / PREV_TEAP)]
  raw[,RAW := ROE_CHG - TEAP_CHG]
  setkey(raw, SYMBOL, DATE)
  
  # normal 
  raw[is.na(RAW) | is.infinite(RAW), RAW := NA_real_]
  raw <- normalize_raw(raw, dates)
  
  store_factor(var, from_to = range(dates), dates, raw, save_folder)
}

#2022-02-06
# 因子来源：公众号 净利润同比增长
func_netprofit_growth_yoy <- function(var, dates, adj_dates, save_folder, uqer_folder, uqer_folder2) {
  
  netprofit <- read_raw_data(adj_dates, "NetProfitTTM", uqer_folder)
  netprofit[is.na(RAW), RAW := NA_real_]
  netprofit <- netprofit[!is.na(RAW)]
  
  quarter_profit <- netprofit[, .(DATE = head(DATE, 1)), by = .(SYMBOL, RAW)]
  quarter_profit <- quarter_profit[!is.na(RAW)]
  setkey(quarter_profit, SYMBOL, DATE)
  
  quarter_profit[, PREV_YDATE := DATE - 365.5]
  quarter_profit[, PREV_YRAW := quarter_profit[J(quarter_profit$SYMBOL, quarter_profit$PREV_YDATE), roll = T]$RAW]
  quarter_profit[, RAW := RAW / PREV_YRAW - 1]
  setkey(quarter_profit, SYMBOL, DATE)
  
  #normal
  quarter_profit[is.na(RAW) | is.infinite(RAW), RAW := NA_real_]
  raw <- normalize_raw(quarter_profit, dates)
  
  store_factor(var, from_to = range(dates), dates, raw, save_folder)
}

# 因子来源：公众号 负债增速：负债=总资产-归母净资产
func_tdebt_growth_yoy <- function(var, dates, adj_dates, save_folder, uqer_folder, uqer_folder2) {
  
  raw_asset <- read_raw_data(adj_dates, "TotalAssets", uqer_folder)
  raw_TEAP <- read_raw_data(adj_dates, "TEAP", uqer_folder)
  
  raw <- raw_asset[raw_TEAP, on = c("SYMBOL", "DATE")]
  setnames(raw, c("SYMBOL", "DATE", "TotalAssets", "TEAP"))
  raw[,RAW:=TotalAssets-TEAP]
  raw[is.na(RAW), RAW := NA_real_]
  raw <- raw[!is.na(RAW)]
  
  quarter_debt <- raw[, .(DATE = head(DATE, 1)), by = .(SYMBOL, RAW)]
  quarter_debt <- quarter_debt[!is.na(RAW)]
  setkey(quarter_debt, SYMBOL, DATE)
  
  quarter_debt[, PREV_YDATE := DATE - 365.5]
  quarter_debt[, PREV_YRAW := quarter_debt[J(quarter_debt$SYMBOL, quarter_debt$PREV_YDATE), roll = T]$RAW]
  quarter_debt[, RAW := RAW / PREV_YRAW - 1]
  setkey(quarter_debt, SYMBOL, DATE)
  
  #normal
  quarter_debt[is.na(RAW) | is.infinite(RAW), RAW := NA_real_]
  raw <- normalize_raw(quarter_debt, dates)
  
  store_factor(var, from_to = range(dates), dates, raw, save_folder)
}

# 因子来源：公众号 成本费用利润率=净利润TTM/总成本TTM
func_totalprofit_costratio <- function(var, dates, adj_dates, save_folder, uqer_folder, uqer_folder2) {
  
  raw_profit <- read_raw_data(adj_dates, "NetProfitTTM", uqer_folder)
  raw_cost <- read_raw_data(adj_dates, "TCostTTM", uqer_folder)
  raw <- raw_profit[raw_cost, on = c("SYMBOL", "DATE")]
  setnames(raw, c("SYMBOL", "DATE", "NetProfitTTM", "TCostTTM"))
  raw[,RAW:=NetProfitTTM/TCostTTM]
  raw[is.na(RAW), RAW := NA_real_]
  raw <- raw[!is.na(RAW)]
  
  quarter_debt <- raw[, .(DATE = head(DATE, 1)), by = .(SYMBOL, RAW)]
  quarter_debt <- quarter_debt[!is.na(RAW)]
  setkey(quarter_debt, SYMBOL, DATE)
  
  #normal
  quarter_debt[is.na(RAW) | is.infinite(RAW), RAW := NA_real_]
  raw <- normalize_raw(quarter_debt, dates)
  
  store_factor(var, from_to = range(dates), dates, raw, save_folder)
}

# 因子来源：公众号 稀释EPS
func_dilutedeps <- function(var, dates, adj_dates, save_folder, uqer_folder, uqer_folder2) {
  
  raw_eps <- read_raw_data(adj_dates, "DilutedEPS", uqer_folder)
  raw_eps[is.na(RAW), RAW := NA_real_]
  raw_eps <- raw_eps[!is.na(RAW)]
  
  quarter_eps <- raw_eps[, .(DATE = head(DATE, 1)), by = .(SYMBOL, RAW)]
  quarter_eps <- quarter_eps[!is.na(RAW)]
  setkey(quarter_eps, SYMBOL, DATE)
  
  #normal
  quarter_eps[is.na(RAW) | is.infinite(RAW), RAW := NA_real_]
  raw <- normalize_raw(quarter_eps, dates)
  
  store_factor(var, from_to = range(dates), dates, raw, save_folder)
}

# 戴维斯双击
func_growth_value <- function(var, dates, adj_dates, save_folder, uqer_folder, uqer_folder2, uqer_folder3) {
  
  # 成长加速度
  eps <- read_raw_data(adj_dates, "EPS", uqer_folder) 
  eps[is.na(RAW), RAW := NA_real_]
  eps <- eps[!is.na(RAW)]
  
  quarter_eps <- eps[, .(DATE = head(DATE, 1)), by = .(SYMBOL, RAW)]
  quarter_eps <- quarter_eps[!is.na(RAW)]
  setkey(quarter_eps, SYMBOL, DATE)
  
  quarter_eps[, PREV_YDATE := DATE - 365.5]
  quarter_eps[, PREV_YRAW := quarter_eps[J(quarter_eps$SYMBOL, quarter_eps$PREV_YDATE), roll = T]$RAW]
  #quarter_eps[, RAW := RAW / PREV_YRAW - 1]
  quarter_eps[, RAW := (RAW - PREV_YRAW) / abs(PREV_YRAW)]
  setkey(quarter_eps, SYMBOL, DATE)
  
  quarter_eps[, SHIFT_RAW := shift(RAW), by = SYMBOL]
  quarter_eps[, RAW := (RAW - PREV_YRAW) / abs(PREV_YRAW)]
  setkey(quarter_eps, SYMBOL, DATE)
  
  # 估值
  raw_niap <- read_raw_data(adj_dates, "NIAP", uqer_folder2)
  raw_mktvalue <- read_raw_data(adj_dates, "MktValue", uqer_folder2)
  sector <- GCAMCPUB::readDtRds("D:/CLOUD/risk_factor/SECTOR/citic_sector.rds")
  sector[, SYMBOL := substr(to_jydb_symbol(INNER_CODE), 1, 6)]
  setkey(sector, SYMBOL, DATE)
  
  raw <- raw_niap[raw_mktvalue, on = c("SYMBOL", "DATE")]
  setnames(raw, c("SYMBOL", "DATE", "NIAP", "MKTVALUE"))
  raw[, RAW := NIAP / MKTVALUE]
  raw[, SECTOR_NAME := sector[J(raw$SYMBOL, raw$DATE)]$NAME]
  raw <- raw[!is.na(SECTOR_NAME)]
  
  raw[, MEAN_RAW := mean(RAW, na.rm = T), by = .(DATE, SECTOR_NAME)]
  raw[, SD_RAW := sd(RAW, na.rm = T), by = .(DATE, SECTOR_NAME)]
  raw[, EP := (RAW - MEAN_RAW) / SD_RAW]
  
  raw[, EPS_ACCE := quarter_eps[J(raw$SYMBOL, raw$DATE), roll = T]$RAW]
  raw[is.na(EP) | is.infinite(EP), EP := NA_real_]
  raw[is.na(EPS_ACCE) | is.infinite(EPS_ACCE), EPS_ACCE := NA_real_]
  
  raw[, `:=` (NORM_EP = GCAMCQT::normalizing_ranking(EP),
              NORM_EPS_ACCE = GCAMCQT::normalizing_ranking(EPS_ACCE)), by = DATE]
  raw[, RAW := NORM_EP + NORM_EPS_ACCE]
  setkey(raw, SYMBOL, DATE)
  
  # normal 
  raw[is.na(RAW) | is.infinite(RAW), RAW := NA_real_]
}
