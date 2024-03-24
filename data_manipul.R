#HEADER------------------------------------
#
#Author: sundan
#Copyright   Copyright 2024-sundan
#Email:sundan@mail.ustc.edu.cn
#
#Date:2024-03-22
#
#Script Name:data_manipul.R
#
#Script Description:homework_03,主要关于如何获取数据，检查数据，数据可视化等
#
#
#SETUP ----------------------------------------------

#加载相关安装包,在之前已经对这两个包进行了安装
library(tidyverse)
library(ggplot2)

#获取数据存储到电脑的F盘这个文件夹下
download.file("tinyurl.com/dcmac2017dec/data/surveys_wide.csv",
              dest="F:/1/shuju/sundan-exercise2024/surveys_wide.csv")

#导入数据
data_wide <- read.csv("F:/1/shuju/sundan-exercise2024/surveys_wide.csv") 
head(data_wide)

#检查数据结构
str(data_wide)  

#检查数据表是否有缺失值
any(is.na(data_wide))

#检查特定的行/列是否有缺失值并输出对应名称
que <- any(is.na(data_wide$weight))
print(que)

#选择需要的数据储存在select_wide
library(dplyr)
select_wide <- select(data_wide, plot_id) 
select_wide

#过滤掉除年份等于1995的其他数据保存在filter_wide
filter_wide <- filter(data_wide, year == 1995)
filter_wide

# 进行多重筛选,年份小于1995同时只保留plot_id、DM数据保存在select2
select2 <- data_wide %>% 
filter(year < 1995) %>%
  select(plot_id, DM)
select2

# 筛选后添加新列sum,为原来DM+ZL值
data_wide %>%
  filter(!is.na(DM)) %>%
  mutate(SUM = DM+ZL) %>%
  head()

#将宽格式数据转换为了长格式数据,并查看数据结构/前几行/后几行
data_gather <-data_wide %>% 
  tidyr::gather(key = species_abbrev, value = count, -c(month, day, year, plot_id)) 
str(data_gather)
head(data_gather)
tail(data_gather)

#将长格式文件转回宽格式，并查看数据结构/前几行/后几行
data_spread <- spread(data_gather, key = species_abbrev,value = count)
data_spread
str(data_spread)
head(data_spread)
tail(data_spread)

#利用获取的数据绘制散点图, 设置颜色透明度标题等
plot1 <- ggplot(data_wide, 
       aes(x = plot_id, y = DM)) +
  geom_point(alpha = 0.1, 
             color = "red") +
  labs(title = "Scatter plot of plot_id vs DM colored by year") 
plot1

plot2 <- ggplot(data_wide, 
       aes(x = plot_id, y = PP)) +
  geom_point(alpha = 0.1,
             color = "blue") +
  labs(title = "Scatter plot of plot_id vs PP colored by year") 
plot2

#利用获取的数据根据year进行分类，绘制多个子图
plot3 <- ggplot(data_wide, aes(x = plot_id, y = DM, color = "red")) +
  geom_line() +
  facet_wrap(~ year) +
  labs(title = "Observed DM in time",
       x = "Year of observation",
       y = "Number of plot_id") +
  theme_bw() +
  theme(text=element_text(size = 8))
plot3

# 合并plot1与plot2，保存plot3
install.packages("gridExtra")
library(gridExtra)
gridExtra::grid.arrange(plot1, 
             plot2, 
             ncol = 2, widths = c(5, 5))
ggsave("visualization.png",plot3, width = 6, height = 4, dpi = 300)


