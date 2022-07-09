library(data.table)
library(WindR)
w.start()

#esg数据读取
df <- as.data.table(read.csv("port.csv",stringsAsFactors=FALSE))
esg <- as.data.table(read.csv('harvest_esg_ratings.csv',stringsAsFactors=F))
esg[,DT:=as.character(DT)]
esg[,DT:=as.Date(DT,"%Y%m%d")]

# fund_pool <- df[grepl("fund",df$type),]
# unique(fund_pool[type=='stock_fund',code])
# unique(fund_pool[type=='bond_fund',code])

#分类提取基金的持仓
bond <- data.frame()
stock <- data.frame()
for(i in c(1:5)){
  bond_tmp <- w.wss(unique(df[type=='bond_fund',code]),'prt_topbondwindcode,prt_heavilyheldbondtonav',
                    'rptDate=20210630',paste0('order=',i))$Data
  bond <- rbind(bond,bond_tmp)
}
for(i in c(1:10)){
  stock_tmp <- w.wss(unique(df[type=='stock_fund',code]),'prt_topstockwindcode,prt_heavilyheldstocktonav',
                     'rptDate=20210630',paste0('order=',i))$Data
  stock <- rbind(stock,stock_tmp)
}
bond <- as.data.table(bond)
stock <- as.data.table(stock)
bond[,PRT_HEAVILYHELDBONDTONAV:=PRT_HEAVILYHELDBONDTONAV/sum(PRT_HEAVILYHELDBONDTONAV),by=.(CODE)]
stock[,PRT_HEAVILYHELDSTOCKTONAV:=PRT_HEAVILYHELDSTOCKTONAV/sum(PRT_HEAVILYHELDSTOCKTONAV),by=.(CODE)]
bond[,type:='bond_fund']
stock[,type:='stock_fund']
colnames(bond) <- c("code",'sub_code','rate','type')
colnames(stock) <- c("code",'sub_code','rate','type')
fund_df <- rbind(bond,stock)

#匹配债券对应主体
match <- as.data.table(read.csv('match.csv',stringsAsFactors=FALSE))
setkey(match,code)
write.csv(fund_df,'fund_df.csv')

#保存行业
codelist <- unique(esg[,SEC_ID])
industrylist <- w.wss(codelist,'industry_sw','industryType=1')$Data
write.csv(industrylist,'industry_match.csv')


w.stop()
