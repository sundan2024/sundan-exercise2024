#HEADER------------------------------------
#
#Author: sundan
#Copyright   Copyright 2024-sundan
#Email:sundan@mail.ustc.edu.cn
#
#Date:2024-03-30
#
#Script Name:sundan_homework_04
#
#Script Description:homework_04,主要关于如何使用caret进行模型构建等
#
#
#SETUP ----------------------------------------------


#加载caret，之前已经进行了该包的安装
library(ggplot2)
library(caret)
library(randomForest)
library(skimr)

#根据作业要求，加载R语言中内置的mtcars数据集
data(mtcars)

#查看数据集结构及内容
str(mtcars)
head(mtcars[, 1:10])
summary(mtcars)
pairs(mtcars)

#检查数据集是否有缺失值，返回值0
sum(is.na(mtcars))

#进行数据缩放
df <-mtcars
df <- as.data.frame(scale(df))
df$mpg <- NULL
df$mpg <- mtcars$mpg
head(df)

#创建训练集和测试集，确定mpg为目标变量
trainingRowIndex <- sample(1:nrow(df), 0.8*nrow(df)) 
trainData <- df[trainingRowIndex, ]  
testData  <- df[-trainingRowIndex, ]   

#查看训练集
head(trainData)
ncol(trainData)

#统计信息
library(skimr)
skimmed <- skim(trainData)
skimmed[, c(1:5, 9:11)]

#训练随机森林模型
rf_model <- randomForest(mpg ~ ., data = trainData,  proximity=TRUE, ntree=1000)

#评估变量重要性
(varimp.rf_model <- varImp(rf_model))
varImpPlot(rf_model)

#确定重要变量
important_features <- importance(rf_model, type = 2)
str(important_features)
selected_features <- rownames(important_features)[order(important_features[, "IncNodePurity"], decreasing = TRUE)]

#根据上述结果，重新训练模型
rf_model_selected <- randomForest(mpg ~ ., data = trainData[, c("mpg", selected_features)])

#使用交叉验证进行模型调优
tune_grid <- expand.grid(mtry = c(1, 2, 3, 4, 5))
rf_tuned <- train(
  mpg ~ ., 
  data = trainData[, c("mpg", selected_features)],
  method = "rf",
  trControl = trainControl(method = "cv", number = 5),
  tuneGrid = tune_grid
)

#获得最佳模型
best_model <- rf_tuned$bestTune

#评估模型性能
predictions <- predict(rf_tuned, newdata = testData[, c("mpg", selected_features)])
perf <- postResample(pred = predictions, obs = testData$mpg)

#打印性能指标，获得的结果表明预测结果良好
print(perf)

#绘制残差图
residuals <- testData$mpg - predictions
ggplot(data.frame(residuals = residuals, predictions = predictions), aes(x = predictions, y = residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(x = "Predicted mpg", y = "Residuals") +
  theme_minimal()

#可视化预测结果
ggplot(data.frame(predictions = predictions, actual = testData$mpg), aes(x = actual, y = predictions)) +
  geom_point() +
  geom_abline(color = "red") +
  labs(x = "Actual mpg", y = "Predicted mpg") +
  theme_minimal()

#根据选定的特征，确定了预测mpg与实际mpg之间的线性关系并进一步获取了拟合效果
total_variance <- sum((testData$mpg - mean(testData$mpg))^2)
residual_variance <- sum((testData$mpg - predictions)^2)
rsquared <- 1 - (residual_variance / total_variance)
print(rsquared)

