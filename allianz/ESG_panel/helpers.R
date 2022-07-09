
library(shiny)
library(shinyBS)
library(shinydashboard)
library(DT)
library(data.table)
library(ggplot2)
library(ggthemes)
source("read_data.R",encoding='UTF-8')


ui <- dashboardPage(
  dashboardHeader(title = "Allianz ESG Panel"),#总标题
  dashboardSidebar(
    sidebarMenu(
      menuItem("ESG Portfolio", tabName = "dashboard", icon = icon("home")),
        menuItem("ESG Score", tabName = "widgets", icon = icon("th"))
    )
  ),#导航栏
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "dashboard",
              h1("Portfolio Info"),
              
              uiOutput("modals"),
              fluidRow(column(3,dateRangeInput("mdates", label = h3("Date Range"),
                                               min='2017-01-31',
                                               max='2021-12-31',
                                               start='2017-01-31',
                                               end='2021-12-31'))),
              dataTableOutput("main_table"),
              tags$small(
                "If you want to know the detail of ESG score of individual stock"),
              tags$small(
                "Please double click the button on the right"
              )
      ),
      
      # Second tab content
      tabItem(tabName = "widgets",
              h2("Portfolio ESG score"),
              fluidRow(column(3,dateRangeInput("ddates", label = h3("Date Range"),
                                               min='2017-01-31',
                                               max='2021-12-31',
                                               start='2017-01-31',
                                               end='2021-12-31')),
                       column(3,radioButtons("t",'Type',
                                                       c('plot','table')))
                       ),
              conditionalPanel(condition="input.t=='plot'",
                               fluidRow(column(3,wellPanel(selectInput('plotradio','Class to show',
                                                                        choices=c('Whole','stock','bond','equity_fund','bond_fund'))))),
                               plotOutput('port_plot')
                               ),
              conditionalPanel(condition="input.t=='table'",
                               dataTableOutput('port_table')),
              
      )
    )
  )
)

server <- function(input, output, session) {
  #main_table selection--------------------------------------------------------------------------
  df_tmp <- df
  
  #click on buttons setting----------------------------------------------------------------------------------------------------------
  shinyInput <- function(FUN, len, id, ...) {
    inputs <- character(len)
    for (i in seq_len(len)) {
      inputs[i] <- as.character(FUN(paste0(id, i), ...))
    }
    inputs
  }
  Action <- shinyInput(actionButton, nrow(df_tmp), 
                       'button_', label = "ESG detail", 
                       onclick = 'Shiny.onInputChange(\"select_button\",  this.id)' )
  df_tmp[,Action:=Action]
  df_r <- reactiveValues(data=df_tmp)

  #results---------------------------------------------------------------------------------------------------------------------------
  
  #主表-持仓情况----------------------------------------------------------------
  output$main_table <- renderDT({
    datatable(df_r$data,escape = FALSE, selection = 'none',
              options=list(lengthMenu = seq(5,30,5), pageLength = 5,searching=F))%>%
      formatPercentage('weight',2)%>% formatRound('price', 2)
  }, server = FALSE,
    escape = FALSE, selection = 'none')
  
  #组合ESG表--------------------------------------------------------------------
  output$port_table <- renderDataTable({
    start_days <- 1
    end_days <- 1
    if(month(input$ddates[1])==12){
      start_year <- year(input$ddates[1])+1
      start_month <- 1
    }else{
      start_year <- year(input$ddates[1])
      start_month <- month(input$ddates[1])+1
    }
    
    if(month(input$ddates[2])==12){
      end_year <- year(input$ddates[2])+1
      end_month <- 1
    }else{
      end_year <- year(input$ddates[2])
      end_month <- month(input$ddates[2])+1
    }
    start_date <- as.Date(paste0(c(start_year,start_month,start_days),collapse = "-"))-lubridate::ddays(1)
    end_date <- as.Date(paste0(c(end_year,end_month,end_days),collapse = "-"))-lubridate::ddays(1)
    
    tb <- port_esg[DT>=start_date&DT<=end_date,][order(DT),]
    
    datatable(
      tb,
      extensions="FixedColumns",
      options = list(lengthMenu = seq(5,30,5), pageLength = 5,
                     searching=F,scrollX=T,fixedColumns = TRUE)
    )%>%formatRound(colnames(tb[,2:ncol(tb)]),2)
  })
  
  #组合选择画图-----------------------------------------------------------------
  output$port_plot <- renderPlot({
    if (is.null(input$plotradio))
      return()
    
    start_days <- 1
    end_days <- 1
    if(month(input$ddates[1])==12){
      start_year <- year(input$ddates[1])+1
      start_month <- 1
    }else{
      start_year <- year(input$ddates[1])
      start_month <- month(input$ddates[1])+1
    }
    
    if(month(input$ddates[2])==12){
      end_year <- year(input$ddates[2])+1
      end_month <- 1
    }else{
      end_year <- year(input$ddates[2])
      end_month <- month(input$ddates[2])+1
    }
    start_date <- as.Date(paste0(c(start_year,start_month,start_days),collapse = "-"))-lubridate::ddays(1)
    end_date <- as.Date(paste0(c(end_year,end_month,end_days),collapse = "-"))-lubridate::ddays(1)
    
    if(input$plotradio=='Whole'){
      esg_temp <- port_esg
    }else{
      esg_temp <- esg[SEC_ID%in%port_tmp[type==input$plotradio,code],]
      tmp <- copy(port_tmp[type==input$plotradio,])
      tmp[,weight:=weight/sum(weight)]
      setkey(tmp,code)
      esg_temp[,weight:=tmp[J(esg_temp$SEC_ID),]$weight]
      esg_temp <- esg_temp[,.(Total=sum(Total*weight),Environment=sum(Environment*weight),
                              Social=sum(Social*weight),Governance=sum(Governance*weight)),by=.(DT)]
      
    }
    
    temp <- esg_temp[DT>=start_date&DT<=end_date,]
    temp <- melt(temp,id.vars=c('DT'),
                 measure.vars=colnames(temp)[2:ncol(temp)],
                 variable.name='index',value.name = 'value')
    
    ggplot(data=temp,aes(x=DT,y=value,color=index))+
      geom_line(na.rm=T,size=1.5)+
      labs(y=NULL,x=NULL,color=NULL)+
      theme(panel.border=element_blank(),panel.background=element_blank(),
            panel.grid = element_blank(),axis.line=element_line(colour='black'))+
      scale_colour_excel_new()
    
  })
  
  #弹出框-----------------------------------------------------------------------
  output$modals <- renderUI({
    #updateTabItems(session, "sidebar", "widgets")
    bsModal('modal','Individual ESG detail',input$select_button,size='large',
            fluidRow(column(3,wellPanel(radioButtons("ttt",'Type',
                                                    c('ESG Plot','ESG Momentum','ESG table'))))),
            fluidRow(#column(3,selectInput('stock',label=h3('股票名称'),
                                          # c(unique(df_r$data$name)))),
                    column(4,uiOutput('detail_name')),
                     column(3,dateRangeInput("dates", label = h3("Date Range"),
                                             min='2017-01-31',
                                             max='2020-12-31',
                                             start='2017-01-31',
                                             end='2020-12-31'))),
            conditionalPanel(condition="input.ttt=='ESG Plot'",
                             plotOutput("plot")),
            conditionalPanel(condition="input.ttt=='ESG Momentum'",
                             plotOutput('mom')),
            conditionalPanel(condition="input.ttt=='ESG table'",
                             fluidRow(column(3,radioButtons('radio','Choose Level',
                                                            choices=c('1st&2nd','3rd')))),
                             conditionalPanel(condition="input.radio=='1st&2nd'",dataTableOutput("detail_table")),
                             conditionalPanel(condition="input.radio=='3rd'",dataTableOutput("detail2_table"))
                             )
      )
  })#ESG个股详情
  output$detail_name <- renderUI({
    strong(paste0('Chi_Name:',df_r$data[as.numeric(strsplit(input$select_button, "_")[[1]][2]),name]))
  })
  
  
  #一级指标---------------------------------------------------------------------
  output$detail_table <- renderDataTable({
    selectedRow <- as.numeric(strsplit(input$select_button, "_")[[1]][2])
    token <- df_r$data[selectedRow,code]

    #日期筛选
    start_days <- 1
    end_days <- 1
    if(month(input$dates[1])==12){
      start_year <- year(input$dates[1])+1
      start_month <- 1
    }else{
      start_year <- year(input$dates[1])
      start_month <- month(input$dates[1])+1
    }

    if(month(input$dates[2])==12){
      end_year <- year(input$dates[2])+1
      end_month <- 1
    }else{
      end_year <- year(input$dates[2])
      end_month <- month(input$dates[2])+1
    }
    start_date <- as.Date(paste0(c(start_year,start_month,start_days),collapse = "-"))-lubridate::ddays(1)
    end_date <- as.Date(paste0(c(end_year,end_month,end_days),collapse = "-"))-lubridate::ddays(1)
    #品种判断---------------------------------------------------------------
    type <- df_r$data[selectedRow,type]
    if(type=='bond'){#个债
      if(match[code==token,match]=='no'){
        datatable(data.table())
      }else{
        token <- match[code==token,match]
        tb <- esg[SEC_ID==token
                  &DT>=start_date
                  &DT<=end_date,][order(DT),]
        tot_color <- scales::gradient_n_pal(c("red","green", "yellow" ))(scales::rescale(c(tb$Total)))
        env_color <- scales::gradient_n_pal(c("red", "green", "yellow"))(scales::rescale(c(tb$Environment)))
        soc_color <- scales::gradient_n_pal(c( "red","green", "yellow"))(scales::rescale(c(tb$Social)))
        gov_color <- scales::gradient_n_pal(c("red", "green", "yellow"))(scales::rescale(c(tb$Governance)))
        
        datatable(
          tb,
          extensions="FixedColumns",
          options = list(lengthMenu = seq(5,30,5), pageLength = 5,
                         searching=F,scrollX=T,fixedColumns = TRUE)
        )%>%formatRound(colnames(esg)[c(4,5,7,8,10,11,13,14)],2)%>%
          formatStyle(columns='Total', backgroundColor = styleEqual(tb$Total,tot_color))%>%
          formatStyle(columns='Environment', backgroundColor = styleEqual(tb$Environment, env_color))%>%
          formatStyle(columns='Social', backgroundColor = styleEqual(tb$Social, soc_color))%>%
          formatStyle(columns='Governance', backgroundColor = styleEqual(tb$Governance, gov_color))
      }
    }else if(type=='stock'){#个股
      tb <- esg[SEC_ID==token
                &DT>=start_date
                &DT<=end_date,][order(DT),]
      tot_color <- scales::gradient_n_pal(c("red","green", "yellow" ))(scales::rescale(c(tb$Total)))
      env_color <- scales::gradient_n_pal(c("red", "green", "yellow"))(scales::rescale(c(tb$Environment)))
      soc_color <- scales::gradient_n_pal(c( "red","green", "yellow"))(scales::rescale(c(tb$Social)))
      gov_color <- scales::gradient_n_pal(c("red", "green", "yellow"))(scales::rescale(c(tb$Governance)))
      
      datatable(
        tb,
        extensions="FixedColumns",
        options = list(lengthMenu = seq(5,30,5), pageLength = 5,
                       searching=F,scrollX=T,fixedColumns = TRUE)
      )%>%formatRound(colnames(esg)[c(4,5,7,8,10,11,13,14)],2)%>%
        formatStyle(columns='Total', backgroundColor = styleEqual(tb$Total,tot_color))%>%
        formatStyle(columns='Environment', backgroundColor = styleEqual(tb$Environment, env_color))%>%
        formatStyle(columns='Social', backgroundColor = styleEqual(tb$Social, soc_color))%>%
        formatStyle(columns='Governance', backgroundColor = styleEqual(tb$Governance, gov_color))
      
    }else if(type=='equity_fund'){#股基
      tbl <- fund_df[code==token,]
      setkey(tbl,sub_code)
      pool <- tbl[,sub_code]
      esg_tmp <- esg[SEC_ID%in%pool
                     &DT>=start_date
                     &DT<=end_date,]
      esg_tmp[,port:=tbl[J(esg_tmp$SEC_ID),]$rate]#匹配持仓权重
      esg_tmp[,port:=port/sum(port),by=.(DT)]#重新归一化
      esg_tmp[,`:=`(Total=Total*port,Environment=Environment*port,
                    Social=Social*port,Governance=Governance*port)]
      esg_tmp <- esg_tmp[,.(SEC_ID=token,Total=sum(Total),Environment=sum(Environment),
                            Social=sum(Social),Governance=sum(Governance)),by=.(DT)]
      
      tb <- esg_tmp[SEC_ID==token
                    &DT>=start_date
                    &DT<=end_date,][order(DT),]
      tot_color <- scales::gradient_n_pal(c("red","green", "yellow" ))(scales::rescale(c(tb$Total)))
      env_color <- scales::gradient_n_pal(c("red", "green", "yellow"))(scales::rescale(c(tb$Environment)))
      soc_color <- scales::gradient_n_pal(c( "red","green", "yellow"))(scales::rescale(c(tb$Social)))
      gov_color <- scales::gradient_n_pal(c("red", "green", "yellow"))(scales::rescale(c(tb$Governance)))
      
      datatable(
        tb,
        extensions="FixedColumns",
        options = list(lengthMenu = seq(5,30,5), pageLength = 5,
                       searching=F,scrollX=T,fixedColumns = TRUE)
      )%>%formatRound(colnames(tb)[c(3,4,5,6)],2)%>%
        formatStyle(columns='Total', backgroundColor = styleEqual(tb$Total,tot_color))%>%
        formatStyle(columns='Environment', backgroundColor = styleEqual(tb$Environment, env_color))%>%
        formatStyle(columns='Social', backgroundColor = styleEqual(tb$Social, soc_color))%>%
        formatStyle(columns='Governance', backgroundColor = styleEqual(tb$Governance, gov_color))
      
    }else{#债基
      tbl <- fund_df[code==token,]
      tbl[,match:=match[J(tbl$sub_code),]$match]
      if(nrow(tbl[match=='no',])==5){
        datatable(data.table())
      }else{
        setkey(tbl,match)
        pool <- tbl[,match]
        esg_tmp <- esg[SEC_ID%in%pool
                       &DT>=start_date
                       &DT<=end_date,]
        esg_tmp[,port:=tbl[J(esg_tmp$SEC_ID),]$rate]#匹配持仓权重
        esg_tmp[,port:=port/sum(port),by=.(DT)]#重新归一化
        esg_tmp[,`:=`(Total=Total*port,Environment=Environment*port,
                      Social=Social*port,Governance=Governance*port)]
        esg_tmp <- esg_tmp[,.(SEC_ID=token,Total=sum(Total),Environment=sum(Environment),
                              Social=sum(Social),Governance=sum(Governance)),by=.(DT)]
        
        tb <- esg_tmp[SEC_ID==token
                  &DT>=start_date
                  &DT<=end_date,][order(DT),]
        tot_color <- scales::gradient_n_pal(c("red","green", "yellow" ))(scales::rescale(c(tb$Total)))
        env_color <- scales::gradient_n_pal(c("red", "green", "yellow"))(scales::rescale(c(tb$Environment)))
        soc_color <- scales::gradient_n_pal(c( "red","green", "yellow"))(scales::rescale(c(tb$Social)))
        gov_color <- scales::gradient_n_pal(c("red", "green", "yellow"))(scales::rescale(c(tb$Governance)))
        
        datatable(
          tb,
          extensions="FixedColumns",
          options = list(lengthMenu = seq(5,30,5), pageLength = 5,
                         searching=F,scrollX=T,fixedColumns = TRUE)
        )%>%formatRound(colnames(tb)[c(3,4,5,6)],2)%>%
          formatStyle(columns='Total', backgroundColor = styleEqual(tb$Total,tot_color))%>%
          formatStyle(columns='Environment', backgroundColor = styleEqual(tb$Environment, env_color))%>%
          formatStyle(columns='Social', backgroundColor = styleEqual(tb$Social, soc_color))%>%
          formatStyle(columns='Governance', backgroundColor = styleEqual(tb$Governance, gov_color))
        
      }
    }

  })
  
  #2 3 级指标表---------------------------------------------------
  output$detail2_table <- renderDataTable({
    selectedRow <- as.numeric(strsplit(input$select_button, "_")[[1]][2])
    token <- df_r$data[selectedRow,code]
    #日期筛选
    start_days <- 1
    end_days <- 1
    if(month(input$dates[1])==12){
      start_year <- year(input$dates[1])+1
      start_month <- 1
    }else{
      start_year <- year(input$dates[1])
      start_month <- month(input$dates[1])+1
    }
    
    if(month(input$dates[2])==12){
      end_year <- year(input$dates[2])+1
      end_month <- 1
    }else{
      end_year <- year(input$dates[2])
      end_month <- month(input$dates[2])+1
    }
    start_date <- as.Date(paste0(c(start_year,start_month,start_days),collapse = "-"))-lubridate::ddays(1)
    end_date <- as.Date(paste0(c(end_year,end_month,end_days),collapse = "-"))-lubridate::ddays(1)
    #品种判断---------------------------------------------------------------
    type <- df_r$data[selectedRow,type]
    if(type=='bond'){#个债
      if(match[code==token,match]=='no'){
        datatable(data.table())
      }else{
        token <- match[code==token,match]
        datatable(
          esg_three[SEC_ID==token
                    &DT>=start_date
                    &DT<=end_date,][order(DT),],
          caption = htmltools::tags$caption(
                      style = 'caption-side: bottom; text-align: center;',
                      'Note: ', htmltools::em('Gray blongs to E;Blue blongs to S;Yellow belongs to G')
                    ),
          extensions="FixedColumns",
          options = list(lengthMenu = seq(5,30,5), pageLength = 5,
                         searching=F,scrollX=T,fixedColumns = TRUE)
        )%>%formatRound(colnames(esg_three[,5:ncol(esg_three)]),2)%>%
          formatStyle(c('Eovironment_management','Energysaving_policy',
                        'Pollution','Climate_change'),
                      background='#CCCCCC')%>%
          formatStyle(c('Human_resource','Health&Safety',
                        'Product_responsibility','Innovative_service','Social_capital'),
                      background='#CCFFFF')%>%
          formatStyle(c('Governance_structure','Shareholder',
                        'Legality','Audit','Disclousre'),
                      background='#FFFFCC')
        
      }
    }else if(type=='stock'){#个股
      datatable(
        esg_three[SEC_ID==token
                  &DT>=start_date
                  &DT<=end_date,][order(DT),],
        caption = htmltools::tags$caption(
          style = 'caption-side: bottom; text-align: center;',
          'Note: ', htmltools::em('Gray blongs to E;Blue blongs to S;Yellow belongs to G')
        ),
        extensions="FixedColumns",
        options = list(lengthMenu = seq(5,30,5), pageLength = 5,
                       searching=F,scrollX=T,fixedColumns = TRUE)
      )%>%formatRound(colnames(esg_three[,5:ncol(esg_three)]),2)%>%
        formatStyle(c('Eovironment_management','Energysaving_policy',
                      'Pollution','Climate_change'),
                    background='#CCCCCC')%>%
        formatStyle(c('Human_resource','Health&Safety',
                      'Product_responsibility','Innovative_service','Social_capital'),
                    background='#CCFFFF')%>%
        formatStyle(c('Governance_structure','Shareholder',
                      'Legality','Audit','Disclousre'),
                    background='#FFFFCC')
    }else if(type=='equity_fund'){#股基
      tbl <- fund_df[code==token,]
      setkey(tbl,sub_code)
      pool <- tbl[,sub_code]
      esg_tmp <- esg_three[SEC_ID%in%pool
                           &DT>=start_date
                           &DT<=end_date,]
      esg_tmp[,port:=tbl[J(esg_tmp$SEC_ID),]$rate]#匹配持仓权重
      esg_tmp[,port:=port/sum(port),by=.(DT)]#重新归一化
      
      chg_col <- colnames(esg_tmp)[4:(ncol(esg_tmp)-1)]
      esg_tmp[,paste0(chg_col):=lapply(.SD,function(x){x*esg_tmp$port}),
              .SDcols=chg_col]
      esg_tmp <- esg_tmp[,lapply(.SD,sum),.SDcols=chg_col,by=.(DT)]
      esg_tmp[,SEC_ID:=token]
      esg_tmp <- cbind(esg_tmp[,c('DT','SEC_ID')],esg_tmp[,c(2:15)])
      
      tb <- esg_tmp[SEC_ID==token,][order(DT),]
      datatable(
        tb,
        caption = htmltools::tags$caption(
          style = 'caption-side: bottom; text-align: center;',
          'Note: ', htmltools::em('Gray blongs to E;Blue blongs to S;Yellow belongs to G')
        ),
        extensions="FixedColumns",
        options = list(lengthMenu = seq(5,30,5), pageLength = 5,
                       searching=F,scrollX=T,fixedColumns = TRUE)
      )%>%formatRound(colnames(tb[,3:ncol(tb)]),2)%>%
        formatStyle(c('Eovironment_management','Energysaving_policy',
                      'Pollution','Climate_change'),
                    background='#CCCCCC')%>%
        formatStyle(colnames(tb)[7:11],
                    background='#CCFFFF')%>%
        formatStyle(c('Governance_structure','Shareholder',
                      'Legality','Audit','Disclousre'),
                    background='#FFFFCC')
    }else{#债基
      tbl <- fund_df[code==token,]
      tbl[,match:=match[J(tbl$sub_code),]$match]
      if(nrow(tbl[match=='no',])==5){
        datatable(data.table())
      }else{
        setkey(tbl,match)
        pool <- tbl[,match]
        esg_tmp <- esg_three[SEC_ID%in%pool
                             &DT>=start_date
                             &DT<=end_date,]
        esg_tmp[,port:=tbl[J(esg_tmp$SEC_ID),]$rate]#匹配持仓权重
        esg_tmp[,port:=port/sum(port),by=.(DT)]#重新归一化
        
        chg_col <- colnames(esg_tmp)[4:(ncol(esg_tmp)-1)]
        esg_tmp[,paste0(chg_col):=lapply(.SD,function(x){x*esg_tmp$port}),
                  .SDcols=chg_col]
        esg_tmp <- esg_tmp[,lapply(.SD,sum),.SDcols=chg_col,by=.(DT)]
        esg_tmp[,SEC_ID:=token]
        esg_tmp <- cbind(esg_tmp[,c('DT','SEC_ID')],esg_tmp[,c(2:15)])
        
        tb <- esg_tmp[SEC_ID==token,][order(DT),]
        datatable(
          tb,
          caption = htmltools::tags$caption(
            style = 'caption-side: bottom; text-align: center;',
            'Note: ', htmltools::em('Gray blongs to E;Blue blongs to S;Yellow belongs to G')
          ),
          extensions="FixedColumns",
          options = list(lengthMenu = seq(5,30,5), pageLength = 5,
                         searching=F,scrollX=T,fixedColumns = TRUE)
        )%>%formatRound(colnames(tb[,3:ncol(tb)]),2)%>%
          formatStyle(c('Eovironment_management','Energysaving_policy',
                        'Pollution','Climate_change'),
                      background='#CCCCCC')%>%
          formatStyle(colnames(tb)[7:11],
                      background='#CCFFFF')%>%
          formatStyle(c('Governance_structure','Shareholder',
                        'Legality','Audit','Disclousre'),
                      background='#FFFFCC')
      }
    }
    
  })
  
  #ESG走势图-----------------------------------------------------
  output$plot <- renderPlot({
    selectedRow <- as.numeric(strsplit(input$select_button, "_")[[1]][2])
    token <- df_r$data[selectedRow,code]
    start_days <- 1
    end_days <- 1
    if(month(input$dates[1])==12){
      start_year <- year(input$dates[1])+1
      start_month <- 1
    }else{
      start_year <- year(input$dates[1])
      start_month <- month(input$dates[1])+1
    }

    if(month(input$dates[2])==12){
      end_year <- year(input$dates[2])+1
      end_month <- 1
    }else{
      end_year <- year(input$dates[2])
      end_month <- month(input$dates[2])+1
    }
    start_date <- as.Date(paste0(c(start_year,start_month,start_days),collapse = "-"))-lubridate::ddays(1)
    end_date <- as.Date(paste0(c(end_year,end_month,end_days),collapse = "-"))-lubridate::ddays(1)
    #品种判断---------------------------------------------------------------
    type <- df_r$data[selectedRow,type]
    if(type=='bond'){#个债
      if(match[code==token,match]=='no'){
        print('选择的品种暂时没有ESG评分')
      }else{
        token <- match[code==token,match]
        
        temp <- esg[SEC_ID==token&DT>=start_date&DT<=end_date,
                    .(DT,SEC_ID,Total,Environment,Social,Governance)]
        temp <- melt(temp,id.vars=c('DT','SEC_ID'),
                     measure.vars=colnames(temp)[3:ncol(temp)],
                     variable.name='index',value.name = 'value')
        ggplot(data=temp,aes(x=DT,y=value,color=index))+
          geom_line(na.rm=T,size=1.5)+
          labs(y=NULL,x=NULL,color=NULL)+
          theme(panel.border=element_blank(),panel.background=element_blank(),
                panel.grid = element_blank(),axis.line=element_line(colour='black'))+
          scale_colour_excel_new()
      }
    }else if(type=='stock'){#个股
      temp <- esg[SEC_ID==token&DT>=start_date&DT<=end_date,
                  .(DT,SEC_ID,Total,Environment,Social,Governance)]
      temp <- melt(temp,id.vars=c('DT','SEC_ID'),
                   measure.vars=colnames(temp)[3:ncol(temp)],
                   variable.name='index',value.name = 'value')
      ggplot(data=temp,aes(x=DT,y=value,color=index))+
        geom_line(na.rm=T,size=1.5)+
        labs(y=NULL,x=NULL,color=NULL)+
        theme(panel.border=element_blank(),panel.background=element_blank(),
              panel.grid = element_blank(),axis.line=element_line(colour='black'))+
        scale_colour_excel_new()
    }else if(type=='equity_fund'){#股基
      tbl <- fund_df[code==token,]
      setkey(tbl,sub_code)
      pool <- tbl[,sub_code]
      esg_tmp <- esg[SEC_ID%in%pool
                     &DT>=start_date
                     &DT<=end_date,]
      esg_tmp[,port:=tbl[J(esg_tmp$SEC_ID),]$rate]#匹配持仓权重
      esg_tmp[,port:=port/sum(port),by=.(DT)]#重新归一化
      esg_tmp[,`:=`(Total=Total*port,Environment=Environment*port,
                    Social=Social*port,Governance=Governance*port)]
      
      esg_tmp <- esg_tmp[,.(SEC_ID=token,Total=sum(Total),Environment=sum(Environment),
                 Social=sum(Social),Governance=sum(Governance)),by=.(DT)]
      
      temp <- esg_tmp[SEC_ID==token&DT>=start_date&DT<=end_date,]
      temp <- melt(temp,id.vars=c('DT','SEC_ID'),
                   measure.vars=colnames(temp)[3:ncol(temp)],
                   variable.name='index',value.name = 'value')
      ggplot(data=temp,aes(x=DT,y=value,color=index))+
        geom_line(na.rm=T,size=1.5)+
        labs(y=NULL,x=NULL,color=NULL)+
        theme(panel.border=element_blank(),panel.background=element_blank(),
              panel.grid = element_blank(),axis.line=element_line(colour='black'))+
        scale_colour_excel_new()
    }else{#债基
      tbl <- fund_df[code==token,]
      tbl[,match:=match[J(tbl$sub_code),]$match]
      if(nrow(tbl[match=='no',])==5){
        print('选择的品种暂时没有ESG评分')
      }else{
        setkey(tbl,match)
        pool <- tbl[,match]
        esg_tmp <- esg[SEC_ID%in%pool
                       &DT>=start_date
                       &DT<=end_date,]
        esg_tmp[,port:=tbl[J(esg_tmp$SEC_ID),]$rate]#匹配持仓权重
        esg_tmp[,port:=port/sum(port),by=.(DT)]#重新归一化
        esg_tmp[,`:=`(Total=Total*port,Environment=Environment*port,
                      Social=Social*port,Governance=Governance*port)]
        esg_tmp <- esg_tmp[,.(SEC_ID=token,Total=sum(Total),Environment=sum(Environment),
                   Social=sum(Social),Governance=sum(Governance)),by=.(DT)]
        
        temp <- esg_tmp[SEC_ID==token&DT>=start_date&DT<=end_date,]
        temp <- melt(temp,id.vars=c('DT','SEC_ID'),
                     measure.vars=colnames(temp)[3:ncol(temp)],
                     variable.name='index',value.name = 'value')
        ggplot(data=temp,aes(x=DT,y=value,color=index))+
          geom_line(na.rm=T,size=1.5)+
          labs(y=NULL,x=NULL,color=NULL)+
          theme(panel.border=element_blank(),panel.background=element_blank(),
                panel.grid = element_blank(),axis.line=element_line(colour='black'))+
          scale_colour_excel_new()

      }
    }
  })
  
  #动量图----------------------------------------------------------------
  output$mom <- renderPlot({
    selectedRow <- as.numeric(strsplit(input$select_button, "_")[[1]][2])
    token <- df_r$data[selectedRow,code]
    start_days <- 1
    end_days <- 1
    if(month(input$dates[1])==12){
      start_year <- year(input$dates[1])+1
      start_month <- 1
    }else{
      start_year <- year(input$dates[1])
      start_month <- month(input$dates[1])+1
    }
    
    if(month(input$dates[2])==12){
      end_year <- year(input$dates[2])+1
      end_month <- 1
    }else{
      end_year <- year(input$dates[2])
      end_month <- month(input$dates[2])+1
    }
    start_date <- as.Date(paste0(c(start_year,start_month,start_days),collapse = "-"))-lubridate::ddays(1)
    end_date <- as.Date(paste0(c(end_year,end_month,end_days),collapse = "-"))-lubridate::ddays(1)
    #品种判断---------------------------------------------------------------
    type <- df_r$data[selectedRow,type]
    if(type=='bond'){#个债
      if(match[code==token,match]=='no'){
        print('选择的品种暂时没有ESG评分')
      }else{
        token <- match[code==token,match]
        
        temp <- esg[SEC_ID==token,
                    .(DT,SEC_ID,Total,Environment,Social,Governance)][order(DT),]
        temp[,`:=`(Total=Total/shift(Total,6)-1,Environment=Environment/shift(Environment,6)-1,
                   Social=Social/shift(Social,6)-1,Governance=Governance/shift(Governance,6)-1)]
        temp <- melt(temp,id.vars=c('DT','SEC_ID'),
                     measure.vars=colnames(temp)[3:ncol(temp)],
                     variable.name='index',value.name = 'value')
        ggplot(data=temp,aes(x=DT,y=value,color=index))+
          geom_line(na.rm=T,size=1.5)+
          labs(y=NULL,x=NULL,color=NULL)+
          theme(panel.border=element_blank(),panel.background=element_blank(),
                panel.grid = element_blank(),axis.line=element_line(colour='black'))+
          scale_colour_excel_new()
      }
    }else if(type=='stock'){#个股
      temp <- esg[SEC_ID==token,
                  .(DT,SEC_ID,Total,Environment,Social,Governance)][order(DT),]
      temp[,`:=`(Total=Total/shift(Total,6)-1,Environment=Environment/shift(Environment,6)-1,
                 Social=Social/shift(Social,6)-1,Governance=Governance/shift(Governance,6)-1)]
      temp <- melt(temp,id.vars=c('DT','SEC_ID'),
                   measure.vars=colnames(temp)[3:ncol(temp)],
                   variable.name='index',value.name = 'value')
      ggplot(data=temp,aes(x=DT,y=value,color=index))+
        geom_line(na.rm=T,size=1.5)+
        labs(y=NULL,x=NULL,color=NULL)+
        theme(panel.border=element_blank(),panel.background=element_blank(),
              panel.grid = element_blank(),axis.line=element_line(colour='black'))+
        scale_colour_excel_new()
    }else if(type=='equity_fund'){#股基
      tbl <- fund_df[code==token,]
      setkey(tbl,sub_code)
      pool <- tbl[,sub_code]
      esg_tmp <- esg[SEC_ID%in%pool
                     &DT>=start_date
                     &DT<=end_date,]
      esg_tmp[,port:=tbl[J(esg_tmp$SEC_ID),]$rate]#匹配持仓权重
      esg_tmp[,port:=port/sum(port),by=.(DT)]#重新归一化
      esg_tmp[,`:=`(Total=Total*port,Environment=Environment*port,
                    Social=Social*port,Governance=Governance*port)]
      esg_tmp <- esg_tmp[,.(SEC_ID=token,Total=sum(Total),Environment=sum(Environment),
                            Social=sum(Social),Governance=sum(Governance)),by=.(DT)]
      
      temp <- esg_tmp[SEC_ID==token,
                  .(DT,SEC_ID,Total,Environment,Social,Governance)][order(DT),]
      temp[,`:=`(Total=Total/shift(Total,6)-1,Environment=Environment/shift(Environment,6)-1,
                 Social=Social/shift(Social,6)-1,Governance=Governance/shift(Governance,6)-1)]
      temp <- melt(temp,id.vars=c('DT','SEC_ID'),
                   measure.vars=colnames(temp)[3:ncol(temp)],
                   variable.name='index',value.name = 'value')
      ggplot(data=temp,aes(x=DT,y=value,color=index))+
        geom_line(na.rm=T,size=1.5)+
        labs(y=NULL,x=NULL,color=NULL)+
        theme(panel.border=element_blank(),panel.background=element_blank(),
              panel.grid = element_blank(),axis.line=element_line(colour='black'))+
        scale_colour_excel_new()
    }else{#债基
      tbl <- fund_df[code==token,]
      tbl[,match:=match[J(tbl$sub_code),]$match]
      if(nrow(tbl[match=='no',])==5){
        print('选择的品种暂时没有ESG评分')
      }else{
        setkey(tbl,match)
        pool <- tbl[,match]
        esg_tmp <- esg[SEC_ID%in%pool
                       &DT>=start_date
                       &DT<=end_date,]
        esg_tmp[,port:=tbl[J(esg_tmp$SEC_ID),]$rate]#匹配持仓权重
        esg_tmp[,port:=port/sum(port),by=.(DT)]#重新归一化
        esg_tmp[,`:=`(Total=Total*port,Environment=Environment*port,
                      Social=Social*port,Governance=Governance*port)]
        esg_tmp <- esg_tmp[,.(SEC_ID=token,Total=sum(Total),Environment=sum(Environment),
                              Social=sum(Social),Governance=sum(Governance)),by=.(DT)]
        
        temp <- esg_tmp[SEC_ID==token,
                    .(DT,SEC_ID,Total,Environment,Social,Governance)][order(DT),]
        temp[,`:=`(Total=Total/shift(Total,6)-1,Environment=Environment/shift(Environment,6)-1,
                   Social=Social/shift(Social,6)-1,Governance=Governance/shift(Governance,6)-1)]
        temp <- melt(temp,id.vars=c('DT','SEC_ID'),
                     measure.vars=colnames(temp)[3:ncol(temp)],
                     variable.name='index',value.name = 'value')
        ggplot(data=temp,aes(x=DT,y=value,color=index))+
          geom_line(na.rm=T,size=1.5)+
          labs(y=NULL,x=NULL,color=NULL)+
          theme(panel.border=element_blank(),panel.background=element_blank(),
                panel.grid = element_blank(),axis.line=element_line(colour='black'))+
          scale_colour_excel_new()
        
      }
    }
  })
  
}

shinyApp(ui, server)
