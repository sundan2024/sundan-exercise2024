#HEADER------------------------------------
#
#Author: sundan
#Copyright   Copyright 2024-sundan
#Email:sundan@mail.ustc.edu.cn
#
#Date:2024-04-21
#
#Script Name:homework_08
#
#Script Description: 
#01.沿着Doubs河设置2公里的缓冲区，并从地图中剪辑;
#02.使用qgis流程包提取每个点的集水面积和坡度的栅格值;
#03.将提取的数据与来自Doubs数据集中的其他环境因子合并;
#
#
#SETUP ----------------------------------------------

#下载并加载本次任务所需R包
install.packages("gdistance")
install.packages("terra")
install.packages("sf")
library(terra)
library(sf)
library(raster)
library(gdistance)
library(dplyr)
library(ggplot2)
library(ade4)

#载入所需数据
#读取DEM数据:通过网址：https://download.gebco.net/寻找到doubs对应地域获取tif文件
doubs_dem <- terra::rast("F:/1/shuju/gebco_doubs.tif")

#可视化DEM数据，设置颜色、标题、图例
plot(doubs_dem,
     col = terrain.colors(100),  # 设置颜色
     main = "Digital Elevation Model (DEM) Visualization",  # 设置标题
     legend = TRUE,  # 显示图例
     legend.shrink = 0.8)  # 调整图例大小

#读取河流线数据，通过quickOSM搜索获得，导出另存为shp文件
doubs_line <- sf::st_read("F:/1/shuju/line_doubs.shp")

#读取点数据，
doubs_points <- sf::st_read("F:/1/shuju/doubs_utm.shp")


#01-沿着Doubs河设置2公里的缓冲区，并从地图中剪辑
#转换投影坐标系
#将河流线数据转换到UTM坐标系
doubs_line_utm <- st_transform(doubs_line, 32631)

#建立缓冲区
#创建河流线的2km缓冲区
doubs_line_buffer <- st_buffer(doubs_line_utm, dist = 2000)
plot(st_geometry(doubs_line_buffer), axes = TRUE)

#可视化缓冲区
ggplot() + geom_sf(data = doubs_line_buffer) 

#裁剪所需高程数据

#获取DEM数据的地理坐标系
terra::crs(doubs_dem) 

#设置地理坐标系CRS为UTM坐标系,根据doubs河流位置，选择EPSG:32631
utm_crs <- "EPSG:32631"

#转换DEM数据到UTM坐标系
doubs_dem_utm <- terra::project(doubs_dem, utm_crs)
terra::crs(doubs_dem_utm)

#裁剪DEM数据到河流线缓冲区范围内
doubs_dem_utm_cut <- crop(doubs_dem_utm, doubs_line_buffer)

#使用河流线缓冲区掩蔽裁剪后的DEM数据
doubs_dem_utm_hide <- mask(doubs_dem_utm_cut, doubs_line_buffer)

#可视化裁剪后的高程数据
plot(doubs_dem_utm_hide, axes = TRUE)

#计算地形参数并绘图
#获取地形数据中的坡度信息，转换为数据框并绘制直方图
slope <- terrain(doubs_dem_utm_hide, "slope", unit = "degrees", neighbors = 8)

slope_df <- as.data.frame(slope, xy = TRUE)

ggplot(slope_df, aes(x = slope)) +
  geom_histogram(bins = 30, fill = "green", color = "black") +
  labs(x = "Slope", y = "Frequency") +
  theme_minimal()

#提取坡向数据，转换为数据框，绘制密度图
aspect <- terrain(doubs_dem_utm_hide, "aspect")
str(aspect)

aspect_values <- values(aspect)
aspect_df <- as.data.frame(aspect_values, stringsAsFactors = FALSE)

ggplot(aspect_df, aes(x = aspect)) +
  geom_density(fill = "yellow", color = "black") +
  labs(x = "Aspect", y = "Density") +
  theme_minimal()

#提取TPI数据，转换为数据框，绘制TPI直方图
TPI<- terrain(doubs_dem_utm_hide,"TPI")
TPI_df <- as.data.frame(TPI,stringsAsFactors = FALSE)

ggplot(TPI_df, aes(x = TPI)) +
  geom_histogram(binwidth = 10, fill = "skyblue", color = "black") +
  labs(title = "TPI值分布直方图", x = "TPI值", y = "频数")

#提取TRI数据，转换为数据框，绘制TRI直方图
TRI<- terrain(doubs_dem_utm_hide,"TRI")
TRI_df <- as.data.frame(TRI,stringsAsFactors = FALSE)

ggplot(TRI_df, aes(x = TRI)) +
  geom_histogram(fill = "red", color = "black", bins = 30) +
  labs(x = "TRI", y = "Frequency") +
  theme_minimal()

#提取地形粗糙度数据，转换为数据框，绘制粗糙度密度图
roughness <- terrain(doubs_dem_utm_hide, "roughness")
roughness_df <- as.data.frame(roughness,stringsAsFactors = FALSE)

ggplot(roughness_df, aes(x = roughness)) +
  geom_density(fill = "orange", color = "black") +
  labs(x = "Roughness", y = "Density") +
  theme_minimal()

#提取流向数据，转换为数据框
flowdir <- terrain(doubs_dem_utm_hide, "flowdir")
flowdir_df <- as.data.frame(flowdir,stringsAsFactors = FALSE)

#统计不同流向的频率
flowdir_freq <- as.data.frame(table(flowdir_df$flowdir))

#重命名频率列
colnames(flowdir_freq)[2] <- "Frequency"

#绘制 flowdir 的条形图
ggplot(flowdir_freq, aes(x = factor(flowdir_freq$Var1), y = Frequency)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(x = "Flow Direction", y = "Frequency") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#02使用qgisprocess包提取每个点的集水面积和坡度的栅格值
#安装qgisprocess
install.packages("qgisprocess")
options(qgisprocess.path = "F:/1/shuju/QGIS/bin/qgis_process-qgis-ltr.bat")
library(qgisprocess)

#配置qgisprocess
qgis_configure()

#搜寻所有可获取算法，验证qgisprocess已经安装
#因为后续涉及到sagang:sagawetnessindex算法,在qgis插件中安装Processing 
#Saga NextGen Provider及相关软件SAGA 9.1，成功搜索到约1300条算法
qgis_algorithms()

#寻找湿度指数算法，并显示前2个
qgis_search_algorithms("wetness") |>
  dplyr::select(provider_title,algorithm) |>
  head(2)

#利用找到的湿度指数算法sagang:sagawetnessindex进行计算
topo_total <- qgisprocess::qgis_run_algorithm(
  alg = "sagang:sagawetnessindex",
  DEM = doubs_dem_utm_hide,
  SLOPE_TYPE = 1,
  SLOPE = tempfile(fileext = ".sdat"),
  AREA = tempfile(fileext = ".sdat"),
  .quiet = TRUE)

#选择并重命名湿度指数和坡度数据
topo_select <- topo_total[c("AREA","SLOPE")] |>
  unlist() |>
  rast()

names(topo_select) = c("carea","cslope")
print(topo_select)

#将选择的湿度指数和坡度数据的原点设为数字高程模型的原点
origin(topo_select) = origin(doubs_dem_utm_hide)

#将数字高程模型与选择的湿度指数和坡度数据组合,并可视化
topo_char = c(doubs_dem_utm_hide,topo_select)
plot(topo_char)

#03.将提取的数据与来自Doubs数据集中的其他环境因子合并
#将点数据转换为UTM坐标系（EPSG:32631）
doubs_points_utm <- sf::st_transform(doubs_points ,32631)
dim(doubs_points_utm)

#从组合数据中提取环境变量值
topo_env <- terra::extract(topo_char,doubs_points_utm,ID = FALSE)

#从doubs数据框中提取环境变量
doubs_env <- doubs$env

#合并点数据、提取的环境变量和doubs数据框中的环境变量
doubs_env_combined <- cbind(doubs_points_utm, topo_env, doubs_env)
str(doubs_env_combined)

#将数据保存为CSV文件
write.csv(doubs_env_combined, file = "F:/1/shuju/doubs_environment_data.csv", row.names = FALSE)

#将sf对象保存为Shapefile文件
st_write(doubs_env_combined, "F:/1/shuju/doubs_environment_data.shp")
