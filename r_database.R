#HEADER------------------------------------
#
#Author: sundan
#Copyright  Copyright 2024-sundan
#Email: sundan@mail.ustc.edu.cn
#
#Date:2024-04-10 
#
#Script Name:
#
#Script Description:
#
#
#SETUP ----------------------------------------------

#安装一些需要的安装包，且在电脑本地下载了SQLite3及DB
reticulate::virtualenv_create("r-reticulate")
reticulate::py_install("keras")
reticulate::py_install("tensorflow")
install.packages("ade4")
install.packages("RPostgreSQL")
install.packages("RSQLite")
install.packages("DBI")

#加载所需的安装包
library(reticulate)
library(rdataretriever)
library(DBI)
library(ade4)
library(dplyr)
library(keras)

#加载python环境
use_virtualenv("~/.virtualenvs/r-reticulate")
py_config()

# 使用rdataretriever包下载Doubs数据集 但是发现rdataretriever包里没有，更新后仍然找不到
help(package = "rdataretriever")
dataset_names()
rdataretriever::fetch("Doubs", quiet = TRUE, data_names = NULL)
rdataretriever::datasets("doubs")
rdataretriever::get_updates() 
rdataretriever::fetch("Doubs", quiet = TRUE, data_names = NULL)

# 使用ade4成功加载了doubs
library(ade4)
data("doubs")
str(doubs)

#下载"portal.sqlite",已使用DB打开查看
download.file("https://ndownloader.figshare.com/files/11188550",
              "portal.sqlite",
              mode = "wb")
getwd()

#  查看数据集中列表类容，该数据集共有四个列表："env"、"fish"、"xy"、"species"。
?doubs                           

# 创建连接到 SQLite 数据库
con <- dbConnect(RSQLite::SQLite(), "C:/Users/Administrator/Documents/portal.sqlite")

#查看现有数据库中的已存在数据
dbListTables(con)

#将doubs转换类型
combined_df <- purrr::map_dfr(doubs, as.data.frame)

#将转换后的doubs写入数据库，名为Doubs
dbWriteTable(con, "Doubs", combined_df)

#再次查看现有数据库中的已存在数据，已成功导入
dbListTables(con)

# 关闭数据库连接
dbDisconnect(con)



