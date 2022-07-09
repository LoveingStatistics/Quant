

plots <- new.env()

plots$read <- function(var) {
  rds_file <- file.path("output", sprintf("%s.rds",var))
  GCAMCPUB::readDtRds(rds_file)
}

plots$tbl <- function(tbl) {
  knitr::kable(tbl, format="html",digits=3)
}

plots$valuation <- function(tbl, stroke) {
  ggplot(tbl)+
    geom_point(aes(y=TYPE,x=RANK),stroke=stroke,shape=4,color='#293D8E')+
    theme_bw()+
    labs(x = NULL, y = NULL)+
    scale_x_continuous(labels = scales::percent_format(), limits = c(0, 1)) +
    scale_y_discrete() +
    theme(panel.grid.major.y = element_line(linetype = 2, color = "black"),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          axis.text = element_text("Microsoft YaHei")) +
    ggrepel::geom_label_repel(aes(x = RANK, y = TYPE, label = round(VALUE, 2)))
  
}

plots$north <- function(tbl) {
  
  ggplot(tbl, aes(x = TRADINGDAY)) +
    geom_tile(
      aes(y = INDUSTRY, fill = RATIO),
      position = "identity",
      stat = "identity",
      color = "white"
    ) +
    labs(x = NULL, y = NULL, fill = NULL) +
    geom_text(
      aes(y = INDUSTRY, label = GCAMCPUB::f_fmt_pct(RATIO, 2)),
      stat = "identity",
      position = "identity",
      parse = FALSE,
      size = 3,
      check_overlap = TRUE
    ) +
    scale_fill_gradientn(
      colours = purrr::flatten_chr(GCAMCPUB::excel_colors[c("green", "yellow", "red")]),
      values = c(0, 0.5, 1),
      na.value = "lightgrey",
      guide = "none"
    )+
    theme(legend.position = "None",
          axis.text = element_text(size=8),
          panel.background=element_rect(fill='white'),
          axis.text.x = element_text(size = 8))
  
}
#一 权益------------------------------------------------------------------------
#1.收益率------------------------------------------------------------------------
#指数收益率
plots$index_ret <- function() {
  melt_index_ret <- plots$read("index_ret")
  
  ggplot(data=melt_index_ret,aes(x=SECUNAME,y=value,fill=variable))+
    geom_bar(position='dodge',stat='identity')+
    scale_y_continuous(labels=scales::percent_format(),
                       breaks=scales::pretty_breaks(7))+
    scale_fill_manual(values = c("#293D8E", "#CCCCCC"),
                      labels = c("本周收益率", "本年收益率"))+
    labs(x = NULL, y = NULL, fill = NULL,color=NULL)+
    theme(axis.title.x = element_text(size = 10),
          axis.title.y = element_text(size = 10),legend.position='top',
          panel.grid.major.y=element_line(linetype='dashed',color='gray'),
          panel.background=element_rect(fill='white'),
          axis.line.x.bottom=element_line(color='black'),
          legend.text=element_text(size=15),
          axis.text=element_text(size=15)) +
    ggrepel::geom_label_repel(aes(x = SECUNAME, y = value,
                                  label = round(value * 100, 2),color=variable),
                              label.padding=0.75,show.legend = FALSE,size=5) +
    scale_colour_manual(values=c("white", "black"))
  
}

#行业收益率
plots$industry_ret <- function() {
  melt_indus_ret <- plots$read('industry_ret')
  
  ggplot(data=melt_indus_ret,aes(x=NAME,y=value,fill=variable))+
    geom_bar(position='dodge',stat='identity')+
    coord_flip()+
    scale_y_continuous(labels=scales::percent_format(),
                       breaks=scales::pretty_breaks(7))+
    scale_fill_manual(values = c("#293D8E", "#CCCCCC"),
                      labels = c("本周收益率", "本年收益率"))+
    labs(x = NULL, y = NULL, fill = NULL,color=NULL)+
    theme(legend.position='top',
          panel.grid.major.y=element_line(linetype='dashed',color='gray'),
          panel.background=element_rect(fill='white'),
          axis.line.x.bottom=element_line(color='black'),
          legend.text=element_text(size=8),
          axis.text=element_text(size=8)) +
    ggrepel::geom_label_repel(aes(x = NAME, y = value,
                                  label = round(value * 100, 2),color=variable),
                              show.legend = FALSE,size=2) +
    scale_colour_manual(values=c("white", "black"))
  
}

#2.估值------------------------------------------------------------------------
#指数
#pb
plots$index_pb <- function() {
  tbl <- plots$read('index_pb')
  names(tbl) <- c('TYPE','VALUE','RANK')
  plots$valuation(tbl=tbl, stroke=3)
}
#pe
plots$index_pe <- function() {
  tbl <- plots$read('index_pe')
  names(tbl) <- c('TYPE','VALUE','RANK')
  plots$valuation(tbl=tbl, stroke=3)
}
#peg
plots$index_peg <- function() {
  tbl <- plots$read('index_peg')
  names(tbl) <- c('TYPE','VALUE','RANK')
  plots$valuation(tbl=tbl, stroke=3)  
}

#行业
#pb
plots$industry_pb <- function() {
  tbl <- plots$read('industry_pb')
  names(tbl) <- c('TYPE','VALUE','RANK')
  plots$valuation(tbl=tbl,stroke=2)
}
#pe
plots$industry_pe <- function() {
  tbl <- plots$read('industry_pe')
  names(tbl) <- c('TYPE','VALUE','RANK')
  plots$valuation(tbl=tbl,stroke=2)
}
#peg
plots$industry_peg <- function() {
  tbl <- plots$read('industry_peg')
  names(tbl) <- c('TYPE','VALUE','RANK')
  plots$valuation(tbl=tbl,stroke=2)
}

#3.市场交易额-----------------------------------------------------------------------
plots$turnover <- function() {
  tbl <- plots$read('turnover')
  p <- ggplot2::ggplot() +
    geom_bar(data = tbl,
             mapping = aes(x = ENDDATE, y = STOCKTURNOVER), position = "dodge", stat = "identity",
             fill = "#293D8E", color =  "#293D8E", width = 0.5, group = 1) +
    labs(y = "A股成交额(亿元)", x = NULL) +
    theme(axis.title.x = element_text(size = 10), axis.title.y = element_text(size = 15),
          panel.grid.major.y=element_line(linetype='dashed',color='gray'),
          panel.background=element_rect(fill='white'),
          axis.line.x.bottom=element_line(color='black'),
          axis.text=element_text(size=15))
  print(p)
  
}

plots$tbl_turnover <- function() {
  tbl <- plots$read('turnover_tbl')
  data.table::setnames(tbl, c("本周日均成交额(亿)", "成交额周度变化率", "本周成交额分位值"))
  plots$tbl(tbl)
}

#4.ETF--------------------------------------------------------------------------
plots$tbl_etf <- function() {
  tbl <- plots$read('etf_stat')
  data.table::setnames(tbl, c("ETF类型", "近一周平均净流入", 
                              "近一个月平均净流入","近三个月平均净流入",
                              "近半年平均净流入","近一年平均净流入"))
  plots$tbl(tbl)
}

#5.北向资金------------------------------------------------------------------------
#资金净流入 累计净流入
#累计净流入
plots$north_cum <- function(update_date) {
  tbl <- plots$read('north_flow')
  ggplot() +
    geom_line(data = tbl,
              mapping = aes(x = DATE, y = CUM_VALUE / 1e8),
              color =  "#293D8E", size = 0.8, group = 1) +
    labs(y = "陆股通累计资金净流入(亿元)", x = NULL) +
    theme(axis.title.x = element_text(size = 10),
          axis.title.y = element_text(size = 10),
          panel.grid.major.y=element_line(linetype='dashed',color='gray'),
          panel.background=element_rect(fill='white'),
          axis.line.x.bottom=element_line(color='black'),
          axis.text.x=element_text(angle=30,hjust=1)) +
    scale_x_date(expand = c(0, 0), date_breaks = '3 months',
                 date_labels='%Y-%m',limits=c(as.Date('2017-01-01'),update_date))
  
}
#净流入柱状图
plots$north_single <- function(update_date) {
  tbl <- plots$read('north_flow')
  ggplot() +
    geom_bar(data = tbl,
             mapping = aes(x = DATE, y = NETVALUE / 1e8),
             position = "dodge", stat = "identity",
             fill = "#293D8E", color =  "#293D8E", width = 1, group = 1) +
    labs(y = "陆股通每周资金净流入(亿元)", x = NULL) +
    theme(axis.title.x = element_text(size = 10),
          axis.title.y = element_text(size = 10),
          panel.grid.major.y=element_line(linetype='dashed',color='gray'),
          panel.background=element_rect(fill='white'),
          axis.line.x.bottom=element_line(color='black'),
          axis.text.x=element_text(angle=30,hjust=1)) +
    scale_x_date(expand = c(0, 0), date_breaks = '3 months',
                 date_labels='%Y-%m',limits=c(as.Date('2017-01-01'),update_date))
  
}

#北向资金行业持股比例图
north_industry_hold_plot <- plots$read('north_industry_hold')

plots$north_hold <- function() {
  tbl <- plots$read('north_industry_hold')
  plots$north(tbl)
}

#北向资金行业持股变动图
plots$north_chg <- function() {
  tbl <- plots$read('north_net_cash_flow')
  data.table::setnames(tbl,"CHG_RATIO","RATIO")
  plots$north(tbl)
}

#6.两融成交情况-----------------------------------------------------------------
plots$cred_trade <- function(update_date) {
  tbl <- plots$read('credit_trading')
  ggplot(data=tbl,aes(x=ENDDATE,y=CLOSE))+geom_bar(stat='identity',fill="#293D8E")+
    geom_line(aes(x=ENDDATE,y=scales::rescale(pct,c(0,max(CLOSE)))),color="black",alpha=0.5)+
    scale_y_continuous(expand=c(0,0),limits=c(0,max(tbl$CLOSE)+1000),
                       breaks=seq(0,max(tbl$CLOSE)+1000,500),
                       sec.axis=sec_axis(~scales::rescale(.,c(0,max(tbl$pct))),
                                         breaks=round(seq(0,max(tbl$pct),length.out=6),2),
                                         name='两融交易占全A成交额占比',
                                         labels=paste0(100*round(seq(0,max(tbl$pct),length.out=6),2),'%')))+
    labs(x=NULL,y='两融成交额(亿元)')+
    theme(panel.grid.major.y=element_line(linetype='dashed',color='gray'),
          panel.background=element_rect(fill='white'),
          axis.line.x.bottom=element_line(color='black'),
          axis.text.x=element_text(angle=30,hjust=1))+
    scale_x_date(expand = c(0, 0), date_breaks = '6 months',
                 date_labels='%Y-%m',limits=c(as.Date('2013-01-04'),update_date))
  
}

#7.分化程度---------------------------------------------------------------------
plots$deviation <- function() {
  tbl <- plots$read('stock_deviation')
  p0 <- ggplot2::ggplot() +
    geom_bar(data = tbl,
             mapping = aes(x = TRADINGDAY, y = sd), position = "dodge", stat = "identity",
             fill = "#293D8E", color =  "#293D8E", width = 0.5, group = 1) +
    labs(y = "SD", x = NULL) +
    theme(axis.title.x = element_text(size = 10), axis.title.y = element_text(size = 15),
          panel.grid.major.y=element_line(linetype='dashed',color='gray'),
          panel.background=element_rect(fill='white'),
          axis.line.x.bottom=element_line(color='black'),
          axis.text=element_text(size=15))
  print(p0)
  
}

plots$tbl_deviation <- function() {
  tbl <- tail(plots$read('stock_deviation'),1)
  data.table::setnames(tbl, c("当前时点","当前时点市场波动率",
                              "当前时点市场分位值"))
  plots$tbl(tbl)
}


#二 债券------------------------------------------------------------------------

#1.信用利差---------------------------------------------------------------------


plots$cred_spread <- function() {
  tbl <- plots$read('spread')
  tbl <- tbl[, c("DATE", "AAA", "AA+", "A+"), with = FALSE]
  
  tbl <- data.table::melt(tbl, id.vars = "DATE")
  year_dates <- lubridate::ymd(data.table::year(sort(tbl$DATE)) * 10000 + 101)
  breaks = tbl$DATE[c(6,106,206)]
  gpjj_p <- ggplot2::ggplot(tbl, aes(x = DATE, y = value, color = variable)) +
    geom_line(na.rm = TRUE, size = 0.8) +
    labs(y = NULL, x = NULL, color = NULL) +
    scale_color_manual(values = c("#293D8E", "#B8B8B8", "#FF7F00"),
                       labels = c("AAA", "AA+", "A+")) +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    theme(axis.title.x = element_text(size = 10), axis.title.y = element_text(size = 10),
          panel.grid.major.y=element_line(linetype='dashed',color='gray'),
          panel.background=element_rect(fill='white'),
          axis.line.x.bottom=element_line(color='black'),
          axis.text=element_text(size=15))
  
  print(gpjj_p)
  
}

#2.可转债-----------------------------------------------------------------------
#2.1转债与全A-------------------------------------------------------------------
plots$convert_bond <- function() {
  tbl <- plots$read("convert_b0")

  tbl <- tbl[, c("DATETIME", "ret1_cum", "a_ret1_cum"), with = FALSE]
  
  tbl <- data.table::melt(tbl, id.vars = "DATETIME")
  year_dates <- lubridate::ymd(data.table::year(sort(tbl$DATETIME)) * 10000 + 101)
  breaks = tbl$DATETIME[c(6,106,206)]
  gpjj_p2 <- ggplot2::ggplot(tbl, aes(x = DATETIME, y = value, color = variable)) +
    geom_line(na.rm = TRUE, size = 0.8) +
    labs(y = NULL, x = NULL, color = NULL) +
    scale_color_manual(values = c("#293D8E", "#B8B8B8"),
                       labels = c("可转债收益率", "A股收益率")) +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    theme(axis.title.x = element_text(size = 10), axis.title.y = element_text(size = 10),legend.position='top',
          panel.grid.major.y=element_line(linetype='dashed',color='gray'),
          panel.background=element_rect(fill='white'),
          axis.line.x.bottom=element_line(color='black'),
          axis.text=element_text(size=15))
  
  print(gpjj_p2)
  
}

#2.2日均交易额------------------------------------------------------------------
plots$convert_turnover <- function() {
  tbl <- plots$read("convert_b1")
  
  p2 <- ggplot2::ggplot() +
    geom_bar(data = tbl,
             mapping = aes(x = TRADINGDAY, y = sumturnover), position = "dodge", stat = "identity",
             fill = "#293D8E", color =  "#293D8E", width = 0.5, group = 1) +
    labs(y = "日均成交额(亿元)", x = NULL) +
    theme(axis.title.x = element_text(size = 10), axis.title.y = element_text(size = 15),
          panel.grid.major.y=element_line(linetype='dashed',color='gray'),
          panel.background=element_rect(fill='white'),
          axis.line.x.bottom=element_line(color='black'),
          axis.text=element_text(size=15))
  print(p2)
}


#2.3转股溢价率小于0可转债占比---------------------------------------------------
plots$convert_pct <- function() {
  tbl <- plots$read("convert_b1")
  gpjj_p3 <- ggplot2::ggplot() +
    geom_line(tbl, mapping = aes(x = TRADINGDAY, y = pct), color = "#293D8E", na.rm = TRUE, size = 0.8)+
    labs(y = "转股溢价率小于0可转债占比(%)", x = NULL) +
    ggplot2::scale_y_continuous(labels = scales::percent_format()) +
    theme(axis.title.x = element_text(size = 15), axis.title.y = element_text(size = 15),
          panel.grid.major.y=element_line(linetype='dashed',color='gray'),
          panel.background=element_rect(fill='white'),
          axis.line.x.bottom=element_line(color='black'),
          axis.text=element_text(size=15))
  
  print(gpjj_p3)
}

#三 衍生品------------------------------------------------------------------------
## 1.金融期货
### 1.三大股指期货收益率统计
plots$tbl_futures <- function() {
  tbl <- plots$read("futures_ret")
  plots$tbl(tbl)
}

### 2.三大股指基差走势
plots$futures_spread <- function(update_date) {
  tbl <- plots$read("futures_spread")
  ggplot(data=tbl)+
    geom_line(aes(x=DATE,y=value,color=variable),size=0.8)+
    scale_color_manual(values = c("#CCCCCC", "#FBC78D","#3B5284"))+
    labs(x=NULL,y=NULL,color=NULL)+
    scale_y_continuous(labels=scales::percent_format(),
                       breaks=scales::pretty_breaks(7))+
    theme(axis.title.x = element_text(size = 10), 
          axis.title.y = element_text(size = 10),legend.position='bottom',
          panel.grid.major.y=element_line(linetype='dashed',color='gray'),
          panel.background=element_rect(fill='white'),
          axis.line.x.bottom=element_line(color='black'),
          axis.text.x=element_text(angle=30,hjust=1))+
    scale_x_date(expand=c(0,0),breaks=scales::pretty_breaks(10)(c(data.table::first(tbl$DATE),update_date)),
                 date_labels='%Y-%m',limits=c(data.table::first(tbl$DATE),update_date))
  
}

### 3.股指期权PCR

plots$pcr <- function(update_date) {
  tbl <- plots$read('pcr')
  ggplot(data=tbl)+
    geom_line(aes(x=DATE,y=value,color=variable),size=0.8)+
    scale_color_manual(values = c("#FBC78D","#3B5284"))+
    labs(x=NULL,y=NULL,color=NULL)+
    scale_y_continuous(breaks=scales::pretty_breaks(7))+
    theme(axis.title.x = element_text(size = 10), 
          axis.title.y = element_text(size = 10),legend.position='bottom',
          panel.grid.major.y=element_line(linetype='dashed',color='gray'),
          panel.background=element_rect(fill='white'),
          axis.line.x.bottom=element_line(color='black'),
          axis.text.x=element_text(angle=30,hjust=1))+
    scale_x_date(expand=c(0,0),breaks=scales::pretty_breaks(10)(c(data.table::first(tbl$DATE),update_date)),
                 date_labels='%Y-%m',limits=c(data.table::first(tbl$DATE),update_date))
  
}

## 2.商品期货
### 1.贵金属类
plots$tbl_gjs <- function() {
  tbl <- plots$read("gjs_stat")
  plots$tbl(tbl)
}
### 2.黑色系
plots$tbl_hs <- function() {
  tbl <- plots$read("hs_stat")
  plots$tbl(tbl)
}
### 3.有色金属类
plots$tbl_jbjs <- function() {
  tbl <- plots$read("jbjs_stat")
  plots$tbl(tbl)
}
### 4.能源化工类
plots$tbl_ny <- function() {
  tbl <- plots$read("ny_stat")
  plots$tbl(tbl)
}
### 5.农产品类
plots$tbl_ncp <- function() {
  tbl <- plots$read("ncp_stat")
  plots$tbl(tbl)
}

#四 资管------------------------------------------------------------------------

#1.公募基金今年来分类箱线图-----------------------------------------------------
plots$pub_fund <- function() {
  tbl <- plots$read('public_fund_ret')
  ggplot(data=tbl,aes(x=TYPE,y=PERIOD_ABS,fill=TYPE))+
    geom_boxplot(outlier.size=0.5)+
    scale_y_continuous(labels = scales::percent_format(),breaks=seq(-0.2,0.6,0.1),limits=c(-0.2,0.3)) +
    labs(x = NULL, y = NULL, fill = NULL, title = "本年收益率", size = 3) +
    theme(plot.title = element_text(hjust = 0.5),
          panel.grid.major.y = element_line(linetype = 2, color = "black"),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.background=element_rect(fill='white'),
          axis.line.x.bottom=element_line(color='black')) +
    guides(fill = FALSE)+
    scale_fill_manual(values = c("#7F8BBB", "#E0E0E0","#FFE0A3",
                                 "#C2E0FF","#C2C2C2","#FFFFE0",
                                 "#7F8BBB","#E0E0E0"))
}

plots$tbl_pub_fund <- function() {
  tbl <- plots$read("public_fund_ret")
  tbl <- tbl[,.(MEDIAN=median(PERIOD_ABS),
                                           AVG=mean(PERIOD_ABS),
                                           LOWQ=quantile(PERIOD_ABS)[2],
                                           HIGHQ=quantile(PERIOD_ABS)[4]),by=.(TYPE)]
  tbl <- tbl[order(-MEDIAN),]
  tbl[,c('MEDIAN','AVG','LOWQ','HIGHQ'):=lapply(.SD,function(x){f_fmt_pct(x,2)}),
                  .SDcols=c('MEDIAN','AVG','LOWQ','HIGHQ')]
  names(tbl) <- c('日期','中位数','平均数','25%分位数','75%分位数')
  
  plots$tbl()
}

#2.公募量化基金表现---------------------------------------------------------------

plots$tbl_hs500 <- function() {
  tbl <- plots$read('public_fund_500_stat')
  tbl <- tbl[order(-WEEK_EXCESS),]
  data.table::setnames(tbl, c('管理人','本周绝对收益','本周超额收益',
                              '前三个月绝对收益','前三个月超额收益',
                              '今年来绝对收益','今年来超额收益','今年来最大回撤'))
  plots$tbl(tbl)
}

plots$tbl_hs300 <- function() {
  tbl <- plots$read('public_fund_300_stat')
  tbl <- tbl[order(-WEEK_EXCESS),]
  data.table::setnames(tbl, c('管理人','本周绝对收益','本周超额收益',
                              '前三个月绝对收益','前三个月超额收益',
                              '今年来绝对收益','今年来超额收益','今年来最大回撤'))
  plots$tbl(tbl)
}

plots$tbl_alpha <- function() {
  tbl <- plots$read('public_fund_alpha_stat')
  tbl <- tbl[order(-WEEK_ABS),][!is.na(WEEK_ABS),]
  data.table::setnames(tbl, c('管理人','本周绝对收益',
                              '前三个月绝对收益','今年来绝对收益',
                              '今年来最大回撤'))
  plots$tbl(tbl)
}

#3.公募量化基金本周超额表现较好的8家----------------------------------------------
#中证500
plots$pub_ret <- function(tbl,prev_year_date,update_date) {
  ggplot(data=tbl)+
    geom_line(aes(x=TRADINGDAY,y=NAV-1,color=MANAGER),size=0.8)+
    scale_color_manual(values = c("#A7D676", "#FBC78D","#F9E2AE",
                                  "#A8DEE0","#85CBCC","#628CBF",
                                  "#3B5284","#CCCCCC"))+
    labs(x=NULL,y=NULL,color=NULL)+
    scale_y_continuous(labels=scales::percent_format(),
                       breaks=scales::pretty_breaks(7))+
    theme(axis.title.x = element_text(size = 10),
          axis.title.y = element_text(size = 10),legend.position='bottom',
          panel.grid.major.y=element_line(linetype='dashed',color='gray'),
          panel.background=element_rect(fill='white'),
          axis.line.x.bottom=element_line(color='black'))+
    scale_x_date(expand=c(0,0),breaks=scales::pretty_breaks()(c(prev_year_date,update_date)),
                 date_labels='%Y-%m',limits=c(prev_year_date,update_date))
  
}

plots$hs500 <- function(prev_year_date,update_date) {
  tbl <- plots$read('public_fund_500')
  plots$pub_ret(tbl,prev_year_date,update_date)
  
}

#沪深300
plots$hs300 <- function(prev_year_date,update_date) {
  tbl <- plots$read('public_fund_300')
  plots$pub_ret(tbl,prev_year_date,update_date)
  
}

#alpha
plots$alpha <- function(prev_year_date,update_date) {
  tbl <- plots$read('public_fund_alpha')
  plots$pub_ret(tbl,prev_year_date,update_date)
  
}

#4.私募明星产品-------------------------------------------------------------------
#4.1指增---------------------------------------------------------------------------

plots$enhance <- function(prev_year_date,update_date) {
  tbl <- plots$read("private_fund_index")
  ggplot(data=tbl)+
    geom_line(aes(x=DATE,y=NAV-1,color=variable),size=0.8)+
    scale_color_manual(values = c("#A7D676", "#FBC78D","#F9E2AE",
                                  "#A8DEE0","#85CBCC","#628CBF",
                                  "#3B5284","#CCCCCC"))+
    labs(x=NULL,y=NULL,color=NULL)+
    scale_y_continuous(labels=scales::percent_format(),
                       breaks=scales::pretty_breaks(7))+
    theme(axis.title.x = element_text(size = 10),
          axis.title.y = element_text(size = 10),legend.position='bottom',
          panel.grid.major.y=element_line(linetype='dashed',color='gray'),
          panel.background=element_rect(fill='white'),
          axis.line.x.bottom=element_line(color='black'))+
    scale_x_date(expand=c(0,0),breaks=scales::pretty_breaks()(c(prev_year_date,update_date)),
                 date_labels='%Y-%m',limits=c(prev_year_date,update_date))
  
}
plots$tbl_enhance <- function() {
  tbl <- plots$read("private_fund_detail_index")
  plots$tbl(tbl)
}

#4.2对冲---------------------------------------------------------------------------
plots$private_alpha <- function(prev_year_date,update_date) {
  tbl <- plots$read('private_fund_hedge')
  ggplot(data=tbl)+
    geom_line(aes(x=DATE,y=NAV,color=variable),size=0.8)+
    scale_color_manual(values = c("#A7D676", "#FBC78D","#F9E2AE",
                                  "#A8DEE0","#85CBCC","#628CBF",
                                  "#3B5284"))+
    labs(x=NULL,y=NULL,color=NULL)+
    scale_y_continuous(breaks=scales::pretty_breaks(7))+
    theme(axis.title.x = element_text(size = 10),
          axis.title.y = element_text(size = 10),legend.position='bottom',
          panel.grid.major.y=element_line(linetype='dashed',color='gray'),
          panel.background=element_rect(fill='white'),
          axis.line.x.bottom=element_line(color='black'))+
    scale_x_date(expand=c(0,0),breaks=scales::pretty_breaks()(c(prev_year_date,update_date)),
                 date_labels='%Y-%m',limits=c(prev_year_date,update_date))
  
}
plots$tbl_pri_alpha <- function() {
  tbl <- plots$read("private_fund_detail_hedge")
  plots$tbl(tbl)
}

#4.3CTA----------------------------------------------------------------------------
plots$cta <- function(prev_year_date,update_date) {
  tbl <- plots$read('private_fund_cta')
  ggplot(data=tbl)+
    geom_line(aes(x=DATE,y=NAV,color=variable),size=0.8)+
    scale_color_manual(values = c("#CCCCCC", "#FBC78D","#3B5284"))+
    labs(x=NULL,y=NULL,color=NULL)+
    scale_y_continuous(breaks=scales::pretty_breaks(7))+
    theme(axis.title.x = element_text(size = 10),
          axis.title.y = element_text(size = 10),legend.position='bottom',
          panel.grid.major.y=element_line(linetype='dashed',color='gray'),
          panel.background=element_rect(fill='white'),
          axis.line.x.bottom=element_line(color='black'))+
    scale_x_date(expand=c(0,0),breaks=scales::pretty_breaks()(c(prev_year_date,update_date)),
                 date_labels='%Y-%m',limits=c(prev_year_date,update_date))
  
}
plots$tbl_cta <- function() {
  tbl <- plots$read("private_fund_detail_cta")
  plots$tbl(tbl)
}
#5.新发基金统计-------------------------------------------------------------------
plots$new_prod <- function() {
  tbl <- plots$read("public_fund_new_stat")
  ggplot(data=tbl,aes(x=wk,y=TOTAL))+geom_bar(stat='identity',fill="#95DCFB")+
    geom_line(aes(x=as.numeric(wk),y=scales::rescale(SIZE,c(0,max(TOTAL)))),color="#FF7F00",size=0.8)+
    scale_y_continuous(expand=c(0,0),limits=c(0,max(tbl$TOTAL)+5),
                       breaks=seq(0,max(tbl$TOTAL)+5,5),
                       sec.axis=sec_axis(~scales::rescale(.,c(0,max(tbl$SIZE))),
                                         breaks=round(seq(0,max(tbl$SIZE),length.out=6),1),
                                         name='新发基金规模(亿元)',
                                         labels=round(seq(0,max(tbl$SIZE),length.out=6),2)))+
    labs(x=NULL,y='新发基金数量')+
    theme(panel.grid.major.y=element_line(linetype='dashed',color='gray'),
          panel.background=element_rect(fill='white'),
          axis.line.x.bottom=element_line(color='black'))
}

plots$new_prod_type <- function() {
  tbl <- plots$read("public_fund_type_stat")
  ggplot(data=tbl)+
    geom_line(aes(x=wk,y=SIZE,color=NEW_TYPE))+
    scale_y_continuous(expand=c(0,0),limits=c(0,max(tbl$SIZE)+5),
                       breaks=seq(0,max(tbl$SIZE)+5,100))+
    labs(x=NULL,y='新发基金规模(亿元)',color=NULL)+
    scale_x_continuous(expand=c(0,0),breaks=tbl$wk)+
    scale_color_manual(values = c("#A7D676", "#FBC78D","#F9E2AE",
                                  "#A8DEE0","#85CBCC","#3B5284"
    ))+
    theme(panel.grid.major.y=element_line(linetype='dashed',color='gray'),
          panel.background=element_rect(fill='white'),
          axis.line.x.bottom=element_line(color='black'),
          legend.position='bottom')
}

#6.券商资管产品月度统计-----------------------------------------------------------

plots$tbl_security <- function() {
  tbl <- plots$read("security_asset_management_product")
  plots$tbl(tbl)
}

