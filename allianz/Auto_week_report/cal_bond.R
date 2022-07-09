
# source("prep_wind_data.R")
#1. spread----------------------------------------------------------------------

diff <- prep_wind$read_all('bond_spread')

diff <- dcast(diff,TRADINGDAY~CODE,value.var='CLOSE')

diff[,`:=`(AAA=S0059776-S0059771,`AA+`=S0059848-S0059843,`A+`=M1000493-M1000485)]

spread_t <-diff[,.(TRADINGDAY, AAA,`AA+`,`A+`)]
setnames(spread_t,'TRADINGDAY','DATE')
GCAMCPUB::saveDtRds(spread_t,file.path(output_path,'spread.rds'))


#2. convertible bond------------------------------------------------------------
convert1 <- GCAMCPUB::readDtRds(file.path(jydb_path,'cbond_quote1.rds'))
convert2 <- GCAMCPUB::readDtRds(file.path(jydb_path,'cbond_quote2.rds'))
converta<-convert1[, .(sumturnover = sum(TURNOVERVALUE),num = .N), by = TRADINGDAY]
convertb <- convert2[, ret:=CLOSEPRICE/PREVCLOSEPRICE-1]
convert_all <- converta[convertb, on = .(TRADINGDAY = TRADINGDAY)]
convert_final <- convert_all[,.(TRADINGDAY,sumturnover,num,ret)]
convert_final

daily_convert <- convert1[,.(daily_mean=mean(TURNOVERVALUE)),by=TRADINGDAY]
winda <- prep_wind$read_all('areturn')
winda[,a_ret:=CLOSE/PRE_CLOSE-1]
winda_con <- winda[convertb,on=.(DATETIME=TRADINGDAY)][,.(DATETIME,ret,a_ret)]
winda_con
winda_con[1,2]=0
winda_con[,ret1:=ret+1]
winda_con[,a_ret1:=a_ret+1]
winda_con[,ret1_cum:=cumprod(ret1)]
winda_con[,a_ret1_cum:=cumprod(a_ret1)]
winda_co<-winda_con[,.(DATETIME,ret1_cum,a_ret1_cum)]

GCAMCPUB::saveDtRds(winda_co,file.path(output_path,"convert_b0.rds"))

convert_low <- convert1[CONVERTPREMIUMRATE<0][,.(low_n=.N),by=TRADINGDAY]
convpre <- convert_low[converta,on=.(TRADINGDAY=TRADINGDAY)][,.(TRADINGDAY,num,low_n)]
convpre[is.na(low_n),low_n:=0]
convpre[,pct:=low_n/num]
convert_pct <- convpre[TRADINGDAY>=prev_oneyear_date & TRADINGDAY<=update_date]
convert_pct

convert_b1 <- convert_final[convert_pct,on=.(TRADINGDAY=TRADINGDAY)][,.(TRADINGDAY,num,low_n,pct,sumturnover)]
convert_b2 <- winda_con[convert_b1,on=.(DATETIME=TRADINGDAY)][,.(DATETIME,num,low_n,pct,sumturnover,ret,a_ret)]
convert_b1[,sumturnover:=sumturnover/100000000]
convert_b1

GCAMCPUB::saveDtRds(convert_b1,file.path(output_path,"convert_b1.rds"))
