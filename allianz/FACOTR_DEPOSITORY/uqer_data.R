

#2022-01-04
read_raw_data <- function(start, end, var, uqer_folder) {
  
  raw <- list()
  
  data_list <- dir(uqer_folder)
  data_list <- setdiff(data_list, c("basic_factor_list.xlsx","get_uqer_data.py"))
  
  for (i in seq_along(data_list)) {
    dt <- fread(file.path(uqer_folder,data_list[i]))
    dt[,DATE:=as.Date(tradeDate)]
    dt[,`:=`(V1=NULL,tradeDate=NULL)]
    dt <- melt(dt,id.vars = c("DATE","ticker"),value.name="RAW")
    if(nrow(dt[variable%in%var,])==0){
      print("waring! invalid variables")
      break
    }else{
      dt <- dt[variable%in%var,]
    }
    raw[[i]] <- copy(dt)
  }
  raw <- rbindlist(raw)
  
  raw[DATE>=start&DATE<=end,.(DATE,SYMBOL=ticker,RAW,variable)]
}


read_factor_data <- function(dates, var, uqer_folder) {
  
  raw <- list()
  
  for (i in seq_along(dates)) {
    date <- dates[i]
    dt <- GCAMCPUB::readDtFst(file.path(uqer_folder, var, fst_file_name(var, date, date)))
    raw[[i]] <- copy(dt)
  }
  raw <- rbindlist(raw)
  
  raw
}



# new_factor_universe <- function(from_to, freq) {
#   
#   from_to <- GCAMCPUB::as_from_to(from_to)
#   max_date <- from_to$to
#   min_date <- from_to$from
#   
#   if (max_date <= as.Date("2020-03-22")) {
#     dt <- GCAMCQT::factor_universe(from_to = from_to, freq = "daily")
#   } else if (min_date > as.Date("2020-03-22")) {
#     dt <- stk_universe(from = min_date, to = max_date)
#   } else {
#     dt1 <- GCAMCQT::factor_universe(from_to = c(from_to$from, as.Date("2020-03-22")), freq = "daily")
#     dt2 <- stk_universe(from = as.Date("2020-03-23"), to = max_date)
#     dt <- rbind(dt1, dt2)
#   }
#   
#   setkey(dt, INNER_CODE, DATE)
#   dt
# }


stk_universe <- function(from, to) {
  
  trade_dates <- fetch_trading_date(from, to)
  secumain <- GCAMCPUB::readDtRds("base_data/secumain.rds") %>% setkey(INNERCODE)
  secumain <- secumain[SECUMARKET %in% c(83, 90) & SECUCATEGORY == 1 & !is.na(LISTEDDATE)]
  lc_liststatus <- GCAMCPUB::readDtRds("base_data/lc_liststatus.rds") %>% setkey(INNERCODE, CHANGEDATE)
  univ <- list()
  codes <- secumain$INNERCODE
  for (code in codes) {
    list_date <- secumain[J(code)]$LISTEDDATE + 90
    dates <- trade_dates[trade_dates >= list_date]
    if (length(dates) > 0) {
      tmp_univ <- data.table(INNER_CODE = code, DATE = dates)
      univ[[code]] <- copy(tmp_univ)
    }
  }
  univ <- rbindlist(univ)
  univ[, STATUS := lc_liststatus[J(univ$INNER_CODE, univ$DATE), roll = T]$CHANGETYPE]
  univ <- univ[STATUS %in% c(1, 3), .(INNER_CODE, DATE)]
  setkey(univ, INNER_CODE, DATE)
  univ
}


fetch_trading_date <- function(from, to) {
  
  begin_time <- format(from, "%Y%m%d")
  end_time <- format(to, "%Y%m%d")
  dates <- w.tdays(from,to)$Data$DATETIME
}

stk_mv <- function(universe) {
  mv_dt <- GCAMCPUB::readDtRds("base_data/marketvalue.rds")
  setkey(mv_dt,INNERCODE,TRADINGDAY)
  universe[,MV:=mv_dt[J(universe$INNER_CODE,universe$DATE)]$TOTALMV]#总市值
  universe
}


# is_trading_date <- function(date) {
#   
#   # trading_dates <- factor_dates(from_to = c(date - 30, date + 30), freq = "daily")
#   trading_dates <- fetch_trading_date(date - 30, date + 30)
#   
#   date %in% trading_dates
# }


gildata_param_mkt <- data.table(
  SECUMARKET = c(83L, 90L, 72L),
  SUFFIX = c("SH", "SZ", "HK"),
  key = "SECUMARKET"
)


# to_jydb_innercode <- function(symbol) {
#   
#   secumain <- GCAMCPUB::readDtFeather("D:/JYDB/secumain.feather")
#   secumain <- secumain[SECUMARKET %in% c(83, 90) & SECUCATEGORY == 1] %>% setkey(SECUCODE)
#   stopifnot(is.character(symbol))
#   suffix <- gildata_param_mkt[J(secumain$SECUMARKET), SUFFIX]
#   secumain[, SYMBOL := stringr::str_c(SECUCODE, ".", suffix)]
#   setkey(secumain, SYMBOL)
#   stopifnot(is_pk_dt(secumain))
#   secumain[J(symbol), INNERCODE]
# }

to_jydb_symbol <- function(inner_code) {
  
  secumain <- GCAMCPUB::readDtRds("base_data/secumain.rds") %>% setkey(INNERCODE)
  suffix <- gildata_param_mkt[J(secumain$SECUMARKET), SUFFIX]
  secumain[, SYMBOL := stringr::str_c(SECUCODE, ".", suffix)]
  symbol <- secumain[J(inner_code), SYMBOL]
}
#############################

store_factor <- function(var, from_to, dates, factor_value, factor_folder) {
  
  from_to <- as_from_to(from_to)
  #  trade_dates <- GCAMCQT::factor_dates(from_to, freq = "daily")
  trade_dates <- dates
  storage <- factor_storage(var, trade_dates, factor_folder)
  exists_factor <- reading_fst_only_exists(storage$FILE_PATH)
  if (!is.null(exists_factor)) {
    exists_factor <- exists_factor[DATE < from_to$from | DATE > from_to$to]
    factor_value <- rbind(exists_factor, factor_value, fill = TRUE)
  }
  for (i in seq_len(nrow(storage))) {
    path <- storage[i, FILE_PATH]
    from <- storage[i, DATE_FROM]
    to <- storage[i, DATE_TO]
    stored_value <- factor_value[DATE >= from & DATE <= to]
    create_parent_folder(path)
    fst::write_fst(stored_value, path, compress = 50)
  }
}


factor_storage <- function(var, trade_dates, factor_folder) {
  
  date_seq <- trade_dates
  res <- list()
  
  for (i in 1L:(length(date_seq))) {
    from <- date_seq[i]
    to <- date_seq[i]
    file_name <- fst_file_name(var, from, to)
    path <- file.path(factor_folder, var, file_name)
    res[[i]] <-
      data.table(
        FILE_PATH = path,
        DATE_FROM = from,
        DATE_TO = to
      )
  }
  res <- data.table::rbindlist(res)
}


fst_file_name <- function(var, from, to) {
  
  stopifnot(is.string(var), is.date(from), is.date(to))
  paste0(paste(var, format(from, "%Y%m%d"), format(to, "%Y%m%d"), sep = "_"), ".fst")
}


reading_fst_only_exists <- function(paths, cols = NULL) {
  
  stopifnot(is.character(paths))
  exists <- file.exists(paths)
  valid_paths <- paths[exists]
  if (length(valid_paths) == 0L) {
    return(invisible())
  } else {
    dt <-
      purrr::map(valid_paths, ~GCAMCPUB::readDtFst(., columns = cols)) %>%
      data.table::rbindlist(.)
    return(dt)
  }
}


create_parent_folder <- function(path) {
  
  folder <- dirname(path)
  if (!dir.exists(folder)) dir.create(folder, recursive = TRUE)
  if (!dir.exists(folder)) {
    stop(folder, " can't be created!", call. = FALSE)
  }
}



normalize_uqer_data <- function(from_to, factor_names, uqer_folder, l1_folder) {
  
  dates <- as.Date(get_trade_calendar(from_to$from, from_to$to))
  #factor_names <- names(uqer_factor)
  univ <- new_factor_universe(from_to = from_to, freq = "daily")
  univ[, SYMBOL := to_jydb_symbol(INNER_CODE)]
  univ[, SYMBOL := substr(SYMBOL, 1, 6)]
  
  for (var in factor_names) {
    dt <- read_raw_data(dates, var, uqer_folder)
    dt[is.na(RAW), RAW := NA_real_]
    dt[, DATE := as.Date(DATE)]
    setkey(dt, SYMBOL, DATE)
    
    res <- copy(univ)
    res[, RAW := dt[J(res$SYMBOL, res$DATE)]$RAW]
    
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
    store_factor(var, from_to, dates, res, l1_folder)
  }
}


normalize_uqer_special_data <- function(from_to, factor_names, uqer_folder, l1_folder) {
  
  dates <- as.Date(get_trade_calendar(from_to$from, from_to$to))
  #factor_names <- names(uqer_factor)
  univ <- new_factor_universe(from_to = from_to, freq = "daily")
  univ[, SYMBOL := to_jydb_symbol(INNER_CODE)]
  univ[, SYMBOL := substr(SYMBOL, 1, 6)]
  
  for (var in factor_names) {
    dt <- read_raw_data(dates, var, uqer_folder)
    dt[is.na(RAW), RAW := NA_real_]
    dt[, DATE := as.Date(DATE)]
    setkey(dt, SYMBOL, DATE)
    
    res <- copy(univ)
    res[, RAW := dt[J(res$SYMBOL, res$DATE)]$RAW]
    
    res[, "WINSORIZED" := RAW]
    res[(!is.na(WINSORIZED) & (WINSORIZED %!=% 0)), WINSORIZED := GCAMCQT::winsorizing_median(WINSORIZED), by = DATE]
    
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
    store_factor(var, from_to, dates, res, l1_folder)
  }
}


run_uqer_goldstock <- function(from_to, gold_stock_folder) {
  
  enddate <- from_to$to + 400
  fromdate <- from_to$from - 400
  
  goldstock_info <- get_uqer_goldstockinfo(fromdate, enddate)
  goldstock_info <- as.data.table(goldstock_info)
  
  goldstock_fore <- get_uqer_goldstockfore(fromdate, enddate)
  goldstock_fore <- as.data.table(goldstock_fore)
  
  GCAMCPUB::writeDtRds(goldstock_info, file.path(gold_stock_folder, "goldstock_info.rds"))
  GCAMCPUB::writeDtRds(goldstock_fore, file.path(gold_stock_folder, "goldstock_fore.rds"))
}


