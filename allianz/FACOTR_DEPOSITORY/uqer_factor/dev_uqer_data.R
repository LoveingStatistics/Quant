
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

# --------------------------------------------------------------------------------------------------------------------------------
fill_code <- function(code) {
  while(nchar(code)!=6){
    code <- paste0("0",code)
  }
  return(code)
}

# 涉及到环比和同比计算
# 环比的话直接采用原始数据shift计算
# 同比数据采用－365计算
start <- as.Date("2009-01-01")
end <- as.Date("2021-06-30")

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


func_roe_growth_12q <- function(var, dates, adj_dates, save_folder, uqer_folder) {
  
  roe <- read_raw_data(adj_dates, "ROE", uqer_folder)#读因子修改 
  roe[is.na(RAW), RAW := NA_real_]
  roe <- roe[!is.na(RAW)]
  
  quarter_roe <- roe[, .(DATE = head(DATE, 1)), by = .(SYMBOL, RAW)]
  quarter_roe <- quarter_roe[!is.na(RAW)]
  setkey(quarter_roe, SYMBOL, DATE)
  quarter_roe[, QUARTER := seq(.N), by = SYMBOL]
  
  overlap_q_roe <- overlapping(quarter_roe[, .(SYMBOL, QUARTER)], quarter_roe, 12)
  
  growth_roe_beta <- overlap_q_roe[, .(DATE = tail(DATE, 1),
                                       BETA = ifelse(.N > 6, as.numeric(GCAMCQT:::lm_growth(RAW)), NA_real_)), by = .(SYMBOL, QUARTER)]
  setkey(growth_roe_beta, SYMBOL, DATE)
  ######这里截至
  # normal 
  univ <- new_factor_universe(from_to = range(dates), freq = "daily")
  univ[, SYMBOL := to_jydb_symbol(INNER_CODE)]
  univ[, SYMBOL := substr(SYMBOL, 1, 6)]
  univ[, RAW := growth_roe_beta[J(univ$SYMBOL, univ$DATE), roll = T]$BETA]
  
  raw <- univ[, .(INNER_CODE, DATE, RAW)]
}


func_roe_growth_acce_12q <- function(var, dates, adj_dates, save_folder, uqer_folder) {
  
  roe <- read_raw_data(adj_dates, "ROE", uqer_folder) 
  roe[is.na(RAW), RAW := NA_real_]
  roe <- roe[!is.na(RAW)]
  
  quarter_roe <- roe[, .(DATE = head(DATE, 1)), by = .(SYMBOL, RAW)]
  quarter_roe <- quarter_roe[!is.na(RAW)]
  setkey(quarter_roe, SYMBOL, DATE)
  quarter_roe[, QUARTER := seq(.N), by = SYMBOL]
  
  overlap_q_roe <- overlapping(quarter_roe[, .(SYMBOL, QUARTER)], quarter_roe, 12)
  
  growth_roe_beta <- overlap_q_roe[, .(DATE = tail(DATE, 1),
                                       ACCE = ifelse(.N > 6, as.numeric(lm_growth_acce(RAW)), NA_real_)), by = .(SYMBOL, QUARTER)]
  setkey(growth_roe_beta, SYMBOL, DATE)
  
  # normal 
  univ <- new_factor_universe(from_to = range(dates), freq = "daily")
  univ[, SYMBOL := to_jydb_symbol(INNER_CODE)]
  univ[, SYMBOL := substr(SYMBOL, 1, 6)]
  univ[, RAW := growth_roe_beta[J(univ$SYMBOL, univ$DATE), roll = T]$ACCE]
  
  raw <- univ[, .(INNER_CODE, DATE, RAW)]
}


