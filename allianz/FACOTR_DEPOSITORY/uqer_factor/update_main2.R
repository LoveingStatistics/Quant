
# 衍生因子框架
# 基于优矿基本面因子进行衍生

library(data.table)
library(dplyr)
source('D:/CODE/r_codes/FACOTR_DEPOSITORY/uqer_factor/uqer_data.R', encoding = 'UTF-8')


# config
from_to <- as_from_to(c(20210701, 20211221))
adj_from_to <- c(from_to$from - 5*365.5, from_to$to)
uqer_folder <- "E:/alpha_factor/uqer_factor"
save_folder <- "E:/alpha_factor/uqer_dev_factor"

# data
factor_config <- readr::read_csv("E:/alpha_factor/dev_factors.csv") %>% as.data.table()

# dev_factors
vars <- factor_config$NAME
dates <- GCAMCQT::factor_dates(from_to = from_to, freq = "daily")
adj_dates <- GCAMCQT::factor_dates(from_to = adj_from_to, freq = "daily")


for (var in vars) {
  def <- factor_config[NAME == var]$FUNCTION
  expr1 <- parse(text = paste0(def, "(var = var, dates = dates, adj_dates = adj_dates, save_folder = save_folder, uqer_folder = uqer_folder)"))
  eval(expr1, envir = list(var = var, dates = dates, adj_dates = adj_dates, save_folder = save_folder, uqer_folder = uqer_folder))
}
