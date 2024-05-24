#HEADER------------------------------------
#
#Author: sundan
#Copyright   Copyright 2024-sundan
#Email:sundan@mail.ustc.edu.cn
#
#Date:2024-05-20
#
#Script Name:ts_fs_ml
#
#Script Description:This script analyzes the fish population data and builds a predictive model 
#for biomass stability.
#
#
#SETUP ----------------------------------------------

#加载必要的包
install.packages("tsibble")
install.packages("tidymodels")
install.packages("tsfeatures")
install.packages("fable")
install.packages("stats")
library(tidyverse)
library(tsibble)
library(ggplot2)
library(tidymodels)
library(readr) 
library(magrittr)  
library(dplyr)
library(tsfeatures)
library(fable)
library(stats)
library(forecast)

#01-创建并可视化VERCah站点VIA物种密度的时间序列对象
#导入数据集
fish <- read.table("F:/1/shuju/sundan-exercise2024/Prunier et al._RawBiomassData.txt", header = TRUE)

#检查数据结构
head(fish)
str(fish)

#查看每列缺失值的数量
sum(is.na(fish)) 

#删除缺失值所在的行
fish <- na.omit(fish)  
colnames(fish)

#创建子集
VERCah_VAI <- dplyr::filter(fish, STATION == "VERCah" & SP == "VAI")
head(VERCah_VAI)

#确保日期列被正确识别并转换为日期时间格式
VERCah_VAI$DATE <- as.Date(VERCah_VAI$DATE)

#按日期排序
VERCah_VAI <- arrange(VERCah_VAI, DATE)

#检查重复行
dup_rows <- duplicates(VERCah_VAI)

#打印重复行
print(dup_rows)

#根据DATE列删除重复行
VERCah_VAI_unique <- VERCah_VAI[!duplicated(VERCah_VAI$DATE), ]

#再次检查是否已经删除完全
duplicates(VERCah_VAI_unique)
head(VERCah_VAI_unique)

#将数据转换为时间序列对象
fish_ts <- as_tsibble(VERCah_VAI_unique, key = SP, index = DATE)

#可视化时间序列对象
ggplot(fish_ts, aes(x = DATE, y = DENSITY, color = SP, group = SP)) +
  geom_line(size = 1.0) +  
  scale_color_manual(values = c("VAI" = "blue")) + 
  theme_minimal() +  
  theme(axis.title = element_text(size = 14, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1), 
        axis.text.y = element_text(size = 12),
        legend.position = "bottom", 
        legend.title.align = 0.5) +  
  labs(title = "Density of VIA Species at VERCah Site",
       subtitle = "Trend over the years",
       x = "Year", y = "Density",
       color = "Species") +
  scale_x_date(date_labels = "%Y",  
               limits = c(as.Date("2000-01-01"), as.Date("2020-12-31"))) + 
  geom_vline(xintercept = as.Date(c("2010-01-01", "2020-01-01")), color = "grey50", linetype = "dotted")  


#02-提取时间序列特征并构建预测模型
#提取时间序列特征
#将fish_ts转换为一个时间序列对象的列表
fish_ts_list <- list(density = fish_ts$DENSITY)

#计算特征
features_ts <- tsfeatures(fish_ts_list)

#查看特征矩阵
head(features_ts)

#将特征矩阵转换为数据框
features_df <- as.data.frame(features_ts)

#features_df包含了提取的特征，将其与原始数据合并或用于模型训练
#使用cbind来合并特征和原始数据
final_data <- cbind(VERCah_VAI_unique, features_df)

#查看最终数据集的前几行
head(final_data)

fish_tsibble <- as_tsibble(VERCah_VAI_unique, key = "SP", index = "DATE")
head(fish_tsibble)

#使用fable包中的model()函数来创建模型规范
str(fish_tsibble)
time_series_data <- fish_tsibble %>% 
  as_tsibble(key = STATION, index = DATE)

#使用fable包中的arima()函数来创建ARIMA模型
arima_model <- arima(time_series_data$DENSITY, order = c(1, 0, 1))

class(arima_model)
str(arima_model)

#拟合模型
fit_model <- Arima(time_series_data$DENSITY, order=c(1, 0, 1))

#查看模型摘要
summary(fit_model)

# 进行预测，假设预测未来12个月
forecast_results <- forecast(fit_model, h = 12)

#获取原始时间序列的最后一个观测时间点
last_obs_date <- max(VERCah_VAI$DATE)

#生成预测的日期序列
forecast_dates <- seq(last_obs_date + 1, by = "month", length.out = 12)

#将forecast_results转换为数据框
forecast_results_df <- data.frame(
  Date = forecast_dates,
  PointForecast = forecast_results$mean,
  Lo80 = forecast_results$lower[,1],
  Hi80 = forecast_results$upper[,1],
  Lo95 = forecast_results$lower[,2],
  Hi95 = forecast_results$upper[,2]
)

#绘制点预测值的变化趋势
p <- ggplot(data = forecast_results_df, aes(x = Date, y = PointForecast)) +
  geom_line(color = "blue") +  
  geom_ribbon(aes(ymin = Lo80, ymax = Hi80), fill = "grey80", alpha = 0.2) +
  geom_ribbon(aes(ymin = Lo95, ymax = Hi95), fill = "red", alpha = 0.2) +
  labs(title = "Forecast Results", x = "Date", y = "Density Forecast") +
  theme_minimal()

#显示绘图
print(p)

