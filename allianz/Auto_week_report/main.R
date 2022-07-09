
# main
# 周报内容
# 权益 债券 衍生品 资管产品

library(data.table)
library(magrittr)

source("./cal_utils.R")

# config
file_path <- function(x) file.path(".", x)
run_script <- function(x) {
GCAMCPUB::log_info("running ", x, "...")
x <- file_path(x)
stopifnot(file.exists(x))
source(x, encoding = "UTF-8")
}

update_date <- as.Date("2021-12-17")
prev_week_date <- as.Date("2021-12-10")
prev_prev_week_date <- as.Date("2021-12-03")
prev_year_date <- as.Date("2020-12-31")
prev_oneyear_date <- as.Date("2020-01-01")

wind_path <- "./db_data/wind_data"
jydb_path <- "./db_data/jydb_data"
download_path <- "./db_data/download_wind_data"
output_path <- "./output"

# 第一步 更新wind数据
# 默认更新好聚源数据
run_script("prep_wind.R")

# 第二步 计算权益所需指标
run_script("cal_equity.R")

# 第三步 计算债券所需指标
run_script("cal_bond.R")

# 第四步 计算衍生品所需指标
run_script("cal_derivarite.R")

# 第五步 计算公募和私募基金所需指标
run_script("cal_am_public.R")
run_script("cal_am_private.R")






