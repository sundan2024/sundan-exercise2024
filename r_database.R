#HEADER------------------------------------
#
#Author: sundan
#Copyright  Copyright 2024-sundan
#Email: sundan@mail.ustc.edu.cn
#
#Date:2024-04-10 
#
#Script Name:r_database.R
#
#Script Description:Rstudio与SQLite、PostgreSQL数据库的交互
#
#
#SETUP ----------------------------------------------


##01-基础配置
# 安装reticulate包，用于在R中与Python交互
install.packages('reticulate')

# 安装Python环境
reticulate::install_miniconda(force = TRUE)

# 安装Python中的retriever包
reticulate::py_install('retriever')

# 安装rdataretriever包，用于在R中运行retriever
install.packages('rdataretriever')

# 检查并更新可用数据集
rdataretriever::check_for_updates()

# 检查rdataretriever包是否已成功安装
"rdataretriever" %in% installed.packages()

# 创建Python虚拟环境
reticulate::virtualenv_create("r-reticulate")

# 安装所需的Python包
reticulate::py_install(c("keras", "tensorflow"))

# 安装R中需要其他的包
install.packages("ade4")
install.packages("RPostgreSQL")
install.packages("RSQLite")
install.packages("DBI")
install.packages("RODBC")
install.packages("odbc")
install.packages("dplyr")
install.packages("dbplyr")
install.packages("RPostgres")

# 加载所需的包
library(reticulate)
library(rdataretriever)
library(DBI)
library(ade4)
library(dplyr)
library(RODBC)
library(odbc)
library(dbplyr)
library(RPostgres)
library(RPostgreSQL)

# 设置Python虚拟环境
use_virtualenv("~/.virtualenvs/r-reticulate")
py_config()



##02-加载数据
# 使用rdataretriever包下载Doubs数据集 但是发现rdataretriever包里没有，更新后仍然找不到
help(package = "rdataretriever")
dataset_names()
rdataretriever::fetch("Doubs", quiet = TRUE, data_names = NULL)
rdataretriever::datasets("doubs")

rdataretriever::get_updates() 
rdataretriever::fetch("Doubs", quiet = TRUE, data_names = NULL)

#后来成功使用ade4加载了doubs
library(ade4)
data("doubs")
str(doubs)
head(doubs)



##03-数据传输到SQLite数据库
# 下载并查看"portal.sqlite"数据库
download.file("https://ndownloader.figshare.com/files/11188550", "portal.sqlite", mode = "wb")
getwd()

#  查看数据集中列表类容，该数据集共有四个列表："env"、"fish"、"xy"、"species"。
?doubs 

# 创建与SQLite数据库的连接
con_1 <- dbConnect(RSQLite::SQLite(), "portal.sqlite")

# 查看数据库中的表
dbListTables(con_1)

# 转换数据集并写入数据库
combined_df <- purrr::map_dfr(doubs, as.data.frame)
dbWriteTable(con_1, "Doubs", combined_df)

# 再次查看数据库中的表
dbListTables(con_1)

# 关闭数据库连接
dbDisconnect(con_1)



##04-数据传输到PostgreSQL数据库
#现在开始尝试连接postgreSQL数据库，已经提前在电脑里下载了该程序并新建了一个Employees数据库
con_2 <- dbCanConnect(RPostgres::Postgres(),
                      dbname ="Employees",
                      port =5432,
                      user ="postgres",
                      password="123456")
con_2

#上述结果返回TRUE，证明可以有效连接
con_3 <-dbConnect(RPostgres::Postgres(),
                  dbname ="Employees",
                  port =5432,
                  user = "postgres",
                  password="123456")

con_3

# 查看PostgreSQL数据库中的表
dbListTables(con_2)

# 将数据传输到PostgreSQL数据库
dbWriteTable(con_2, "Doubs", combined_df)

# 再次查看PostgreSQL数据库中的表，已成功传输
dbListTables(con_2)

# 关闭数据库连接
dbDisconnect(con_2)


