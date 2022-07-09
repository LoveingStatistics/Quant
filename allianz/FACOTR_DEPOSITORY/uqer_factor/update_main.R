library(reticulate)
library(data.table)
library(dplyr)
library(GCAMCPUB)
library(assertthat)


source('D:/Code/r_codes/FACOTR_DEPOSITORY/uqer_factor/uqer_data.R', encoding = 'UTF-8')

# config
from_to <- as_from_to(c(20211227, 20211227))
uqer_folder <- "E:/alpha_factor/uqer_factor"
l1_folder <- "E:/alpha_factor/l1_factor"
gold_stock_folder <- "E:/alpha_factor/gold_stock"

factor_config <- readxl::read_xlsx("E:/alpha_factor/factor_config.xlsx") %>% as.data.table()
vars <- factor_config$VAR

# step1: 更新优矿数据
run_uqer_data(from_to, uqer_folder, vars)
#run_uqer_data2(read_folder = "F:/BaiduNetdiskDownload/data_rar/data_rar", uqer_folder, vars)

# step2: 归一化数据
vars1 <- vars[!vars %in% "GREV"]
vars2 <- "GREV"

normalize_uqer_data(from_to, vars1, uqer_folder, l1_folder)
normalize_uqer_special_data(from_to, vars2, uqer_folder, l1_folder)

# step3: 更新优矿金股数据(按月度更新)
run_uqer_goldstock(as_from_to(c(as.Date("2017-01-01"), from_to$to)), gold_stock_folder)
