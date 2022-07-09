library(data.table)
#原始数据
df <- as.data.table(read.csv("port.csv",stringsAsFactors=FALSE))
esg <- as.data.table(read.csv('zhiding_esg.csv',stringsAsFactors=FALSE))
chiname <- colnames(esg)
colnames(esg) <- c('SEC_ID','NAME','DT','Total','Environment','Social','Governance',chiname[8:21])
esg[,DT:=as.character(DT)]
esg[,DT:=as.Date(DT,"%Y/%m/%d")]


#基金持仓 债券匹配
fund_df <- as.data.table(read.csv('fund_df.csv',stringsAsFactors=FALSE))
fund_df[,X:=NULL]
match <- as.data.table(read.csv('match.csv',stringsAsFactors=FALSE))
setkey(match,code)
#行业对应
industrylist <- as.data.table(read.csv('industry_match.csv',stringsAsFactors=FALSE))
industrylist[,X:=NULL]
setkey(industrylist,CODE)
esg[,INDUSTRY:=industrylist[J(SEC_ID),]$INDUSTRY_SW]

#三级ESG打分
esg_three <- esg[,.(DT,SEC_ID,INDUSTRY,`环境管理评价得分`,
                    `节能政策评价得分`,`环境排放评价得分`,`气候变化评价得分`,
                    `人力资本评价得分`,`健康与安全评价得分`,`产品责任评价得分`,
                    `业务创新评价得分`,`社会资本评价得分`,`治理结构评价得分`,
                    `股东评价得分`,`合规评价得分`,`审计评价得分`,`信息披露评价得分`)]

colnames(esg_three) <- c('DT','SEC_ID','INDUSTRY',
                         'Eovironment_management','Energysaving_policy',
                         'Pollution','Climate_change','Human_resource',
                         'Health&Safety','Product_responsibility',
                         'Innovative_service','Social_capital','Governance_structure',
                         'Shareholder','Legality','Audit','Disclousre')

esg <- esg[,.(DT,SEC_ID,INDUSTRY,Total,Environment,Social,Governance)]
esg_indus <- esg[,.(Total_IND=mean(Total),Environment_IND=mean(Environment),
                    Social_IND=mean(Social),Governance_IND=mean(Governance)),
                 by=.(DT,INDUSTRY)]
setkey(esg_indus,DT,INDUSTRY)
esg[,`:=`(Total_rank=paste0(frank(-Total),"/",.N),
          Environment_rank=paste0(frank(-Environment),"/",.N),
          Social_rank=paste0(frank(-Social),"/",.N),
          Governance_rank=paste0(frank(-Governance),"/",.N)
          ),by=.(DT,INDUSTRY)]
esg <- esg_indus[esg,on=.(DT=DT,INDUSTRY=INDUSTRY)]

esg <- esg[,.(DT,SEC_ID,INDUSTRY,Total,Total_IND,Total_rank,
              Environment,Environment_IND,Environment_rank,
              Social,Social_IND,Social_rank,
              Governance,Governance_IND,Governance_rank)]



#组合ESG
bond_list <- df[type=='bond',code]
df_tmp <- copy(df)
df_tmp[type=='bond',code:=match[J(bond_list)]$match]
df_tmp<-df_tmp[code!='no',]

base <- df_tmp[date=='2021/10/21',]
setkey(base,code)

fund_df_tmp <- fund_df
fund_df_tmp[,w:=base[J(fund_df_tmp$code),]$weight]
fund_df_tmp[,rate:=rate*w]
fund_df_tmp[,w:=NULL]
fund_df_tmp <- fund_df_tmp[,.(sub_code,rate,type)]
colnames(fund_df_tmp) <- c('code','weight','type')
port_tmp <- rbind(base[type%in%c('stock','bond'),.(code,weight,type)],
                  fund_df_tmp)
port_tmp[,ref:=match[J(port_tmp$code),]$match]
port_tmp <- port_tmp[is.na(ref)|ref!='no',]
port_tmp[!is.na(ref),code:=ref]
port_tmp[,`:=`(weight=weight/sum(weight),ref=NULL)]
port_tmp <- port_tmp[,.(weight=sum(weight),type=first(type)),by=.(code)]
setkey(port_tmp,code)

#组合的ESG
port_esg <- esg[SEC_ID%in%port_tmp$code,]
port_esg[,weight:=port_tmp[J(port_esg$SEC_ID),]$weight]
port_esg <- port_esg[,.(Total=sum(Total*weight),Environment=sum(Environment*weight),
                        Social=sum(Social*weight),Governance=sum(Governance*weight)),by=.(DT)]
