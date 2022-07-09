

###utils function
#交易日周计算
week_seek = function(df){
df <- df[order(TRADINGDAY),]
dates <- unique(df[,TRADINGDAY])
week <- c(1)
j <- 1
for(i in 2:length(dates)){
if((dates[i]-dates[i-1])>2){
j <- j + 1
}
week <- c(week, j)
}
week_ref <- data.table(dates=dates,week=week)
setkey(week_ref,dates)
df[,week:=week_ref[J(df$TRADINGDAY)]$week]
return(df)
}

#code和date的broadcast
auto_tbl <- function(dates, codes) {

res <- data.table(INNER_CODE = rep(codes, times = length(dates)),
TRADINGDAY = rep(dates, each = length(codes)))
res
}

#向前填充空值
fill_na <- function(raw) {
na_len <- vector()
i <- 1
while(!all(!is.na(raw))) {
shift_raw <- shift(raw)
raw[is.na(raw)] <- shift_raw[is.na(raw)]
na_len[i] <- sum(is.na(raw))
if (i > 1) {
if (na_len[i] == na_len[i-1]) {
break
}
}
i <- i + 1
}
raw
}


# perf
new_perf <- function(dr, vars) {

x <- seq_along(vars)
args <- purrr::map(setNames(x, vars), ~dr[, .])
do.call(GCAMCBT::Xts_perf$new, args)
}






