#HEADER------------------------------------
#
#Author: sundan
#Copyright   Copyright 2024-sundan
#Email:sundan@mail.ustc.edu.cn
#
#Date:2024-04-12
#
#Script Name: data exploration.R
#
#Script Description:①删除 Doubs 数据集中缺失数据的站点并检测环境因素是否存在共线性。
#②在网站上学习PCAF分析及聚类分析，应用于doubs数据进行学习探索
#③分析鱼类与环境因素之间的关系，并可视化。
#
#
#SETUP ----------------------------------------------

##01-数据加载与查看
#加载所需的包ade4与caret用于数据分析与处理，并查看ade4的帮助文档，学习如何使用该包
help(package=ade4)
library(ade4)
library(caret)
help(package=ade4)

#加载Doubs数据集并查看基础信息，主要是数据结构，前几行数据与数据类型
data(doubs)
str(doubs)
head(doubs)
class(doubs)


##02-数据处理
#检测数据是否有缺失值，如果有删除缺失数据位点，运行后发现没有
summary(doubs) 
which(is.na (doubs), arr.ind = T)
image (t(as.matrix(is.na(doubs))))
Doubs_clean <- na.omit(doubs)

#检查环境因素之间的共线性，输出结果为检测到共线变量，获得"nit" "dfs" "alt" "pho" "slo" "amm" "oxy"
correlation_matrix <- cor(Doubs_clean$env)
collinear <- findCorrelation(correlation_matrix, cutoff = 0.7)
collinear_vars <- colnames(correlation_matrix)[collinear]
if (length(collinear_vars) > 0) {
  print("检测到共线变量:")
  print(collinear_vars)
} else {
  print("未检测到共线变量.")
}
correlation_matrix

#可视化环境因素之间的相关性，获得热图
heatmap(correlation_matrix, 
        col = colorRampPalette(c("blue", "white", "red"))(20), 
        main = "Correlation Heatmap of Environmental Factors")

#检查高度相关的变量
highly_correlated <- findCorrelation(correlation_matrix, cutoff = 0.8)

#打印高度相关的变量,获得"nit" "dfs" "alt" "pho" "amm" "oxy"
print("Highly correlated environmental factors:")
print(names(Doubs_clean$env)[highly_correlated])


##03-使用PCA及聚类分析对doubs进行数据探索
#PCA分析及可视化
# 加载所需的包
library(vegan)

#提取数据中的环境变量env
env_data <- Doubs_clean$env

#进行env的PCA分析
env_pca <- rda(env_data)
summary(env_pca)

#绘制PCA的双变量图，并在图上添加相关因素，主要是标题、图例等
dev.new()
biplot(env_pca, display = "sites", type = "points", col = "blue")
text(env_pca, display = "species", col = "red", cex = 0.8, adj = c(0, -0.5), srt = 45) 
title(main = "PCA of Environmental Factors", xlab = "PCA1", ylab = "PCA2")
legend("bottomright", legend = rownames(env_data), col = "red", pch = 1, cex = 0.8, bty = "n", pt.cex = 0.5)

# 保存图形为图片名称为pca_plot.png，已成功保存到本地
dev.copy(png, "pca_plot.png")
dev.off()
#关闭绘图窗口

#聚类分析
# 载入聚类分析所需的包cluster与ggplot2
library(cluster)
library(ggplot2)

# 合并环境因素和鱼类物种数据env与fish
data <- cbind(Doubs_clean$env, Doubs_clean$fish)

# 对数据进行标准化处理
normalized_data <- scale(data)

# 使用k均值聚类，假设聚成3类
kmeans_result <- kmeans(data, centers = 3)

# 将聚类结果添加到原始数据中
clustered_data <- cbind(data, cluster = kmeans_result$cluster)

# 聚类中心
centers <- kmeans_result$centers

# 绘制环境因素散点图，并按照聚类结果进行着色
dev.set(dev.interactive())
ggplot(clustered_data, 
       aes(x = dfs, 
           y = alt, 
           color = factor(cluster))) +
  geom_point() +
  geom_point(data = as.data.frame(centers), 
             aes(x = dfs, 
                 y = alt, 
                 color = "Cluster Center"), 
             size = 3, 
             shape = 17) +
  labs(x = "DFS", 
       y = "Altitude", 
       color = "Cluster") +
  theme_minimal()

# 绘制鱼类物种散点图，并按照聚类结果进行着色
ggplot(clustered_data, aes(x = Cogo, 
                           y = Satr, 
                           color = factor(cluster))) +
  geom_point() +
  geom_point(data = as.data.frame(centers), 
             aes(x = Cogo, 
                 y = Satr, 
                 color = "Cluster Center"), 
             size = 3, 
             shape = 17) +
  labs(x = "Cogo", 
       y = "Satr", 
       color = "Cluster") +
  theme_minimal()


##04-可视化分析鱼类和环境之间的关系
#加载R包
library(reshape2)

#确定环境因素和鱼类之间的相关性
fish_env_correlation <- cor(Doubs_clean$fish, Doubs_clean$env)

#可视化环境因素和鱼类之间的相关性
ggplot(data = melt(fish_env_correlation), 
       aes(x = Var2, 
           y = Var1, 
           fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", 
                      high = "blue") +
  labs(x = "Environmental Factors", 
       y = "Fish Species", 
       title = "Correlation between Fish and Environmental Factors") +
  theme(axis.text.x = element_text(angle = 90, 
                                   vjust = 0.5, hjust=1))

# 结合热图结果，以dfs为例检查该环境因素与鱼类Anan的关系
ggplot(data = Doubs_clean$env, 
       aes(x = dfs, 
           y = Doubs_clean$fish$Anan)) +
  geom_point() +
  geom_smooth(method = "lm", 
              se = FALSE) +
  labs(x = "Depth", 
       y= "Anan", 
       title = "Relationship between Depth and Anan")
colnames(Doubs_clean$fish)

#可以仿照最后的绘图代码结合实际，分析自己或者科学研究时更感兴趣的因素，以便得到一定的相关性结果





