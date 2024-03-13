#HEADER------------------------------------
#
#Author: sundan
#Copyright   Copyright 2024-sundan
#Email:sundan@mail.ustc.edu.cn
#
#Date:2024-03-13
#
#Script Name:
#
#Script Description:
#
#
#SETUP ----------------------------------------------


#安装tidyverse包
install.packages("tidyverse")

#加载tidyverse包
library(tidyverse)

#查看tidyverse包的帮助文档
help(package="tidyverse")

#打开tidyverse包的vignette文档
vignette("tidyverse")

#浏览tidyverse包的vignette文档
browseVignettes(package="tidyverse")

#运行tidyverse包中的演示
demo(package="tidyverse")

#搜索包含"tidyverse"关键词的函数、变量
apropos("^tidyverse")

#列出tidyverse包中的所有函数和变量
ls("package:tidyverse")

#搜索关键词中包含"tidyverse"的帮助文档
help.search("^tidyverse")

#查看tidyverse包详细信息
packageDescription("tidyverse")

