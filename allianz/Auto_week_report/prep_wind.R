

prep_wind <- new.env()
prep_wind$launch <- function() {
  if (!requireNamespace("WindR")) {
    stop("must install WindR package")
  }
  message("Will attach WindR package")
  library(WindR)
  if (!isTRUE(w.isconnected())) {
    w.start()
  }
  if (!isTRUE(w.isconnected())) {
    stop("Can't start WindR")
  }
}
#industry valuation indicator
prep_wind$industry_pe <- function(ref_date) {
  prep_wind$launch()
  
  tbl <- as.data.table(w.wsd(w.wset('sectorconstituent',
                                    'sectorid=a39901012e000000')$Data$wind_code,
                             "pe_ttm",ref_date,ref_date)$Data)
  tbl[,TRADINGDAY:=as.Date(ref_date)]
  tbl
}
prep_wind$industry_pb <- function(ref_date) {
  prep_wind$launch()
  
  tbl <- as.data.table(w.wsd(w.wset('sectorconstituent',
                                    'sectorid=a39901012e000000')$Data$wind_code,
                             "pb_lf",ref_date,ref_date)$Data)
  tbl[,TRADINGDAY:=as.Date(ref_date)]
  tbl
}
prep_wind$industry_peg <- function(ref_date) {
  prep_wind$launch()
  
  tbl <- as.data.table(w.wsd(w.wset('sectorconstituent',
                                    'sectorid=a39901012e000000')$Data$wind_code,
                             "peg",ref_date,ref_date)$Data)
  tbl[,TRADINGDAY:=as.Date(ref_date)]
  tbl
}
#does not change with time
prep_wind$industry_name <- function(ref_date) {
  prep_wind$launch()
  
  tbl <- as.data.table(w.wset('sectorconstituent',
                              'sectorid=a39901012e000000',
                              "gRateType=1")$Data[,c('wind_code','sec_name')])
  tbl
}
#ETF data
prep_wind$etf_stock <- function(ref_date) {
  prep_wind$launch()
  
  tbl <- as.data.table(w.wsd(w.wset('sectorconstituent',
                                    paste0('date=',ref_date,';sectorid=1000009165000000'))$Data$wind_code,
                             "mf_netinflow",ref_date,ref_date,"unit=1")$Data)
  tbl[,`:=`(TRADINGDAY=as.Date(ref_date),TYPE='stock')]
  
  tbl
}
prep_wind$etf_bond <- function(ref_date) {
  prep_wind$launch()
  
  tbl <- as.data.table(w.wsd(w.wset('sectorconstituent',
                                    paste0('date=',ref_date,';sectorid=1000009166000000'))$Data$wind_code,
                             "mf_netinflow",ref_date,ref_date,"unit=1")$Data)
  tbl[,`:=`(TRADINGDAY=as.Date(ref_date),TYPE='bond')]
  
  tbl
}
prep_wind$etf_commodity <- function(ref_date) {
  prep_wind$launch()
  
  tbl <- as.data.table(w.wsd(w.wset('sectorconstituent',
                                    paste0('date=',ref_date,';sectorid=1000010087000000'))$Data$wind_code,
                             "mf_netinflow",ref_date,ref_date,"unit=1")$Data)
  tbl[,`:=`(TRADINGDAY=as.Date(ref_date),TYPE='commodity')]
  
  tbl
}
prep_wind$etf_monetary <- function(ref_date) {
  prep_wind$launch()
  
  tbl <- as.data.table(w.wsd(w.wset('sectorconstituent',
                                    paste0('date=',ref_date,';sectorid=1000032560000000'))$Data$wind_code,
                             "mf_netinflow",ref_date,ref_date,"unit=1")$Data)
  tbl[,`:=`(TRADINGDAY=as.Date(ref_date),TYPE='monetary')]
  
  tbl
}
prep_wind$etf_crossborder <- function(ref_date) {
  prep_wind$launch()
  
  tbl <- as.data.table(w.wsd(w.wset('sectorconstituent',
                                    paste0('date=',ref_date,';sectorid=1000009717000000'))$Data$wind_code,
                             "mf_netinflow",ref_date,ref_date,"unit=1")$Data)
  tbl[,`:=`(TRADINGDAY=as.Date(ref_date),TYPE='crossborder')]
  
  tbl
}
#bond spread data
prep_wind$bond_spread <- function(ref_date) {
  prep_wind$launch()
  
  tbl <- as.data.table(w.edb('S0059744,S0059749,S0059771,S0059776,
                             M1005088,M1005096,S0059843,S0059848,
                             S0059761,S0059766,M1005118,M1005126,
                             S0059890,S0059895,M1000485,M1000493,
                             M1004833,M1004841',ref_date,
                             ref_date,'Fill=Previous')$Data)
  tbl[,TRADINGDAY:=as.Date(ref_date)]
  
  tbl
}
#credittrading data
prep_wind$credittrading <- function(ref_date) {
  prep_wind$launch()
  
  tbl <- as.data.table(w.edb('M0075989',ref_date,ref_date,'Fill=Previous')$Data)
  
  tbl
}
#A return
prep_wind$areturn <- function(ref_date) {
  prep_wind$launch()
  
  tbl <- as.data.table(w.wsd("881001.WI","close,pre_close",ref_date,ref_date)$Data)
  
  tbl
}

prep_wind$read <- function(var, ref_date) {
  rds_file <- file.path("db_data", "wind_data", var, ref_date)
  GCAMCPUB::readDtRds(rds_file)
}

prep_wind$read_all <- function(var) {
  content <- dir(file.path("db_data","wind_data",var))
  dfs <- data.table()
  for (i in seq_along(content)){
    df <- prep_wind$read(var,content[i])
    dfs <- rbind(dfs, df)
  }
  dfs
}

prep_wind$rds_file <- function(var, ref_date) {
  if(var=='industry_name'){
    file.path("db_data", "wind_data", var, sprintf("%s.rds", var))
  }else{
    file.path("db_data", "wind_data", var,  sprintf("%s.rds", format(as.Date(ref_date),'%Y%m%d')))
  }
}

prep_wind$update <- function(ref_date) {
  vars <- setdiff(names(prep_wind), c("launch","update","read","rds_file","read_all"))
  for (var in vars) {
    rds_file <- prep_wind$rds_file(var, ref_date)
    if(file.exists(rds_file)){
      GCAMCPUB::log_info("already updated wind data ", rds_file)
    }else{
      GCAMCPUB::log_info("updating wind data ", var, ref_date)
      tbl <- prep_wind[[var]](ref_date)
      GCAMCPUB::saveDtRds(tbl, rds_file)
    }
  }
}

prep_wind$launch()
tdays <- w.tdays("2021-02-24",update_date)$Data$DATETIME
for(i in seq_along(tdays)) {
  prep_wind$update(tdays[i])
}


#futures----------------------------------------------------------------------

#update futures data
update_futures <- function(from_to, .folder){
  from_to <- GCAMCQT::as_from_to(from_to)
  
  sec_list <- c('IF','IH','IC')#沪深300,中证500,上证50
  type_list <- c('00','01','02','03')
  name_list <- c()
  for(i in 1:length(sec_list)){
    name_list <- c(name_list,
                   paste0(sec_list[i],type_list,'.CFE'))
  }#get the name of futures types
  name_list <- c(name_list,c('000300.SH','000905.SH','000016.SH'))
  futures <- w.wsd(name_list,"close",as.character(from_to$from),as.character(from_to$to),"Period=W","PriceAdj=F")$Data
  
  path <- file.path(.folder,futures_name)
  if (file.exists(path)) {
    origin_futures <- readr::read_csv(path) %>% as.data.table()
    origin_futures$Date <- as.Date(origin_futures$Date)
    setnames(futures, names(origin_futures))
    origin_futures <- origin_futures[Date < min(futures$Date)]
    futures <- rbind(origin_futures, futures) %>% unique(.)
  }
  readr::write_csv(futures, path)
}

#update spread
update_spread <- function(from_to, .folder){
  
  from_to <- GCAMCQT::as_from_to(from_to)
  t_days <- w.tdays(from_to$from, from_to$to)
  dates <- as.Date(t_days$Data$DATETIME)
  
  datas <- list()
  for (i in seq_along(dates)) {
    date <- dates[i]
    IH_data <- as.data.table(w.wset('futurecc',paste0('startdate=', date, ';enddate=', date, ';wind_code=IH.CFE;field=wind_code,last_trade_date'))$Data)
    IH_data[, index_code := "000016.SH"]
    IF_data <- as.data.table(w.wset('futurecc',paste0('startdate=', date, ';enddate=', date, ';wind_code=IF.CFE;field=wind_code,last_trade_date'))$Data)
    IF_data[, index_code := "000300.SH"]
    IC_data <- as.data.table(w.wset('futurecc',paste0('startdate=', date, ';enddate=', date, ';wind_code=IC.CFE;field=wind_code,last_trade_date'))$Data)
    IC_data[, index_code := "000905.SH"]
    data <- rbind(IH_data, IF_data, IC_data)
    data[, DATE := date]
    data[, FUTURE_PRICE := w.wsd(data$wind_code,"close",date,date)$Data$CLOSE]
    index_price <- data.table(INDEX = unique(data$index_code),
                              CLOSE = w.wsd(unique(data$index_code),"close",date,date)$Data$CLOSE) %>% setkey(INDEX)
    data[, INDEX_PRICE := index_price[J(data$index_code)]$CLOSE]
    datas[[i]] <- copy(data)
  }
  datas <- rbindlist(datas)
  datas[, last_trade_date := as.Date(last_trade_date, origin = "1899-12-30")]
  datas[, REMAIN_DAYS := as.numeric(last_trade_date - DATE)]
  datas[, SPREAD := (FUTURE_PRICE - INDEX_PRICE) / FUTURE_PRICE / REMAIN_DAYS * 365]
  
  path <- file.path(.folder,spread_name)
  if (file.exists(path)) {
    origin_datas <- readr::read_csv(path) %>% as.data.table()
    origin_datas <- origin_datas[DATE < min(datas$DATE)]
    datas <- rbind(origin_datas, datas)
  }
  readr::write_csv(datas, path)
}

#update PCR data
update_pcr <- function(from_to, .folder){
  from_to <- GCAMCQT::as_from_to(from_to)
  
  option_list <- c('510050.SH','000300.SH')
  pcr <- data.table()
  for (i in 1:length(option_list)){
    sql <- paste0('startdate=',as.character(from_to$from),
                  ';enddate=', as.character(from_to$to),
                  ';exchange=sse;windcode=',
                  option_list[i],
                  ';field=date,daily_call_position,daily_put_position')
    sql <- as.character(sql)
    wind_pcr <- w.wset('optiondailytransactionstastics', sql)
    wind_pcr <- wind_pcr$Data %>% as.data.table()
    wind_pcr[, `:=`(DATE=as.Date(date, origin = "1899-12-30"),
                    PCR = daily_put_position / daily_call_position)]
    wind_pcr <- wind_pcr[, .(DATE, daily_put_position, daily_call_position, PCR)]
    wind_pcr[, INDEX := option_list[i]]
    pcr <- rbind(pcr, wind_pcr)
  }
  
  
  path <- file.path(.folder,pcr_name)
  if (file.exists(path)) {
    origin_pcr <- readr::read_csv(path) %>% as.data.table()
    origin_pcr$DATE <- as.Date(origin_pcr$DATE)
    colnames(pcr) <- colnames(origin_pcr)
    origin_pcr <- origin_pcr[DATE < min(pcr$DATE)]
    pcr <- rbind(origin_pcr, pcr) %>% unique(.)
  }
  readr::write_csv(pcr, path)
}

futures_name <- "close.csv"
spread_name <- "spread.csv"
pcr_name <- "pcr.csv"

#更新金融期货数据 futures data
if(file.exists(file.path(wind_path,futures_name))){
  updated_date <- max(as.Date(readr::read_csv(file.path(wind_path,futures_name))$Date))
} else {
  updated_date <- update_date - 365
}
if (update_date > updated_date) {
  GCAMCPUB::log_info("Updating from ", as.character(updated_date), " to ", as.character(update_date))
  update_futures(from_to = c(updated_date, update_date), .folder = wind_path)
} else {
  GCAMCPUB::log_info("Updated Already")
}

# 更新期货合约数据
if(file.exists(file.path(wind_path,spread_name))){
  updated_date <- max(readr::read_csv(file.path(wind_path,spread_name))$DATE)
} else {
  updated_date <- update_date - 365
}
if (update_date > updated_date) {
  GCAMCPUB::log_info("Updating from ", as.character(updated_date), " to ", as.character(update_date))
  update_spread(from_to = c(updated_date, update_date), .folder = wind_path)
} else {
  GCAMCPUB::log_info("Updated Already")
}

#更新PCR数据
if(file.exists(file.path(wind_path,pcr_name))){
  updated_date <- max(as.Date(readr::read_csv(file.path(wind_path,pcr_name))$DATE))
} else {
  updated_date <- update_date - 365 * 3
}
if (update_date > updated_date) {
  GCAMCPUB::log_info("Updating from ", as.character(updated_date), " to ", as.character(update_date))
  update_pcr(from_to = c(updated_date, update_date), .folder = wind_path)
} else {
  GCAMCPUB::log_info("Updated Already")
}

#商品主力连续合约
gjs <- c("AU.SHF,AG.SHF")

ny <- c("ZC.CZC,PF.CZC,FU.SHF,
                  PG.DCE,J.DCE,JM.DCE,RU.SHF,
                  BU.SHF,SP.SHF,L.DCE,TA.CZC,EG.DCE,
                  MA.CZC,PP.DCE,EB.DCE,UR.CZC,SA.CZC,
                  V.DCE,FG.CZC")
#剔除线材WR
hs <- c("RB.SHF,I.DCE,SS.SHF,
        HC.SHF,SF.CZC,SM.CZC")
#基本金属
jbjs <- c("CU.SHF,AL.SHF,PB.SHF,
          ZN.SHF,NI.SHF,SN.SHF")

#农产品剔除RS,BB,RI,WH,CY,FB,JR,PM,RR,RS,B,LR
ncp <- c("LH.DCE,PK.CZC,A.DCE,C.DCE,
         M.DCE,Y.DCE,OI.CZC,P.DCE,
         CF.CZC,SR.CZC,JD.DCE,
         RM.CZC,CS.DCE,AP.CZC,CJ.CZC")

#贵金属
if(!file.exists(file.path(wind_path,'gjs.rds'))){
  updated_date <- update_date - 365
  gjs_main <- w.wsd(gjs,"close",as.character(updated_date),
                    as.character(update_date))$Data %>% as.data.table()
  saveRDS(gjs_main,file.path(wind_path,'gjs.rds'))
}
gjs_main <- readRDS(file.path(wind_path,'gjs.rds'))
gjs_main1<-w.wsd(gjs,
                 "close",update_date-ddays(20),update_date)$Data %>% as.data.table()
gjs_main2<-unique(rbind(gjs_main,gjs_main1,fill=TRUE))
saveRDS(gjs_main2,file.path(wind_path,'gjs.rds'))

#能源化工
if(!file.exists(file.path(wind_path,'ny.rds'))){
  updated_date <- update_date - 365
  ny_main <- w.wsd(ny,"close",as.character(updated_date),
                   as.character(update_date))$Data %>% as.data.table()
  saveRDS(ny_main,file.path(wind_path,'ny.rds'))
}
ny_main <- readRDS(file.path(wind_path,'ny.rds'))
ny_main1<-w.wsd(ny,
                "close",update_date-ddays(20),update_date)$Data %>% as.data.table()
ny_main2<-unique(rbind(ny_main,ny_main1,fill=TRUE))
saveRDS(ny_main2,file.path(wind_path,'ny.rds'))

#黑色系
if(!file.exists(file.path(wind_path,'hs.rds'))){
  updated_date <- update_date - 365
  hs_main <- w.wsd(hs,"close",as.character(updated_date),
                   as.character(update_date))$Data %>% as.data.table()
  saveRDS(hs_main,file.path(wind_path,'hs.rds'))
}
hs_main <- readRDS(file.path(wind_path,'hs.rds'))
hs_main1<-w.wsd(hs,
                "close",update_date-ddays(20),update_date)$Data %>% as.data.table()
hs_main2<-unique(rbind(hs_main,hs_main1,fill=TRUE))
saveRDS(hs_main2,file.path(wind_path,'hs.rds'))

#基本金属
if(!file.exists(file.path(wind_path,'jbjs.rds'))){
  updated_date <- update_date - 365
  jbjs_main <- w.wsd(jbjs,"close",as.character(updated_date),
                     as.character(update_date))$Data %>% as.data.table()
  saveRDS(jbjs_main,file.path(wind_path,'jbjs.rds'))
}
jbjs_main <- readRDS(file.path(wind_path,'jbjs.rds'))
jbjs_main1<-w.wsd(jbjs,
                  "close",update_date-ddays(20),update_date)$Data %>% as.data.table()
jbjs_main2<-unique(rbind(jbjs_main,jbjs_main1,fill=TRUE))
saveRDS(jbjs_main2,file.path(wind_path,'jbjs.rds'))

#农产品
if(!file.exists(file.path(wind_path,'npc.rds'))){
  updated_date <- update_date - 365
  ncp_main <- w.wsd(ncp,"close",as.character(updated_date),
                    as.character(update_date))$Data %>% as.data.table()
  saveRDS(ncp_main,file.path(wind_path,'ncp.rds'))
}
ncp_main <- readRDS(file.path(wind_path,'ncp.rds'))
ncp_main1<-w.wsd(ncp,
                 "close",update_date-ddays(20),update_date)$Data %>% as.data.table()
ncp_main2<-unique(rbind(ncp_main,ncp_main1,fill=TRUE))
saveRDS(ncp_main2,file.path(wind_path,'ncp.rds'))



