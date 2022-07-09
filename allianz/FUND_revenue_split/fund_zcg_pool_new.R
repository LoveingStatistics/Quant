
# 重仓股选股代码
# 为了防止股票数量过多问题，只采用重仓股进行计算

library(data.table)
library(lubridate)

# config
start <- ymd(20100101)
end <- ymd(20211224)
dates <- GCAMCQT::factor_dates(from_to = c(start, end), freq = "daily")

MF_AssetAllocation <- GCAMCPUB::readDtRds("D:/CODE/r_codes/stock_pool/zcg_pool/MF_AssetAllocation.rds")
MF_JYFundType <- GCAMCPUB::readDtRds("D:/CODE/r_codes/stock_pool/zcg_pool/MF_JYFundType.rds")
MF_KeyStockPortfolio <- GCAMCPUB::readDtRds("D:/CODE/r_codes/stock_pool/zcg_pool/MF_KeyStockPortfolio.rds") 
MF_StockPortfolioDetail <- GCAMCPUB::readDtRds("D:/CODE/r_codes/stock_pool/zcg_pool/MF_StockPortfolioDetail.rds")
quote <- GCAMCPUB::readDtRds("D:/CODE/r_codes/stock_pool/zcg_pool/quote.rds") %>% setkey(INNERCODE, TRADINGDAY)

# MF_KeyStockPortfolio <- rbind(MF_KeyStockPortfolio[(month(REPORTDATE) == 3 & day(REPORTDATE) == 31) | (month(REPORTDATE) == 9 & day(REPORTDATE) == 30)],
#                               MF_StockPortfolioDetail)
setkey(MF_KeyStockPortfolio, INNERCODE, INFOPUBLDATE)
keystock_stamp <- unique(MF_KeyStockPortfolio[, .(INNERCODE, INFOPUBLDATE, INFOPUBLDATE2 = INFOPUBLDATE)])
setkey(keystock_stamp, INNERCODE, INFOPUBLDATE)


# 找出所有时间段符合的标准股票型基金表stkfunds
jy_stkfunds <- local({
  jy_stkfunds <- copy(MF_JYFundType)
  jy_stkfunds <- jy_stkfunds[SECUCODE == MAINCODE]
  jy_stkfunds <- jy_stkfunds[, .(EFFECTIVEDATE = min(EFFECTIVEDATE),
                                 CANCELDATE = max(CANCELDATE)), by = .(INNERCODE, SECUCODE, MAINCODE)]
})


stkfunds <- local({
  stkfunds <- CJ(INNERCODE = jy_stkfunds[, unique(INNERCODE)],
                 DATE = dates)
  stkfunds[, `:=` (EFFECTIVEDATE = jy_stkfunds[J(stkfunds$INNERCODE), EFFECTIVEDATE, on = 'INNERCODE'],
                   CANCELDATE = jy_stkfunds[J(stkfunds$INNERCODE), CANCELDATE, on = 'INNERCODE'])]
  stkfunds[is.na(CANCELDATE), CANCELDATE := end + 1]
  stkfunds[, IS_EFF := (DATE >= EFFECTIVEDATE & DATE < CANCELDATE)]
  stkfunds <- stkfunds[IS_EFF == TRUE, .(INNERCODE, DATE)]
  stkfunds[]
})


# 进行重仓股选取
keystocks <- list()
for (i in seq_along(dates)) {
  date <- dates[i]
  cross_stkfunds <- stkfunds[DATE == date]
  cross_stkfunds[, INFOPUBLDATE := keystock_stamp[J(cross_stkfunds$INNERCODE, cross_stkfunds$DATE), roll = T]$INFOPUBLDATE2]
  cross_stkfunds <- cross_stkfunds[!is.na(INFOPUBLDATE)]
  cross_keystock <- MF_KeyStockPortfolio[J(cross_stkfunds$INNERCODE, cross_stkfunds$INFOPUBLDATE)]
  cross_keystock[, DATE := date]
  keystocks[[i]] <- copy(cross_keystock)
}
keystocks <- rbindlist(keystocks)

# 获取close
last_keystocks <- keystocks[, .(SHARESHOLDING = sum(SHARESHOLDING)), by = .(STOCKINNERCODE, DATE)]
last_keystocks[, CLOSEPRICE := quote[J(last_keystocks$STOCKINNERCODE, last_keystocks$DATE), roll = T]$CLOSEPRICE]
# 剔除港股数据
last_keystocks <- last_keystocks[!is.na(CLOSEPRICE), .(INNER_CODE = STOCKINNERCODE, DATE, SHARESHOLDING, MV = SHARESHOLDING * CLOSEPRICE)]
last_keystocks[, WEIGHT := MV / sum(MV), by = DATE]


# 考虑到回测速度，采用月度换仓
month_dates <- GCAMCQT::factor_dates(from_to = c(start, end), freq = "monthly")
last_keystocks <- last_keystocks[DATE %in% month_dates]


GCAMCPUB::writeDtRds(last_keystocks, "E:/alpha_factor/gold_stock/zcg_stock_pool.rds")


