## 应变量误差
## 载入数据
sim.dat<-read.csv("https://raw.githubusercontent.com/happyrabbit/DataScientistR/master/Data/SegData.csv")
ymad<-mad(na.omit(sim.dat$income))
## 计算Z分值
zs<-(sim.dat$income-mean(na.omit(sim.dat$income)))/ymad
## which(na.omit(zs>3.5)) 找到利群点
## which(is.na(zs)) 找到缺失值
idex<-c(which(na.omit(zs>3.5)),which(is.na(zs)))
## 删除含有离群点和缺失值的行
sim.dat<-sim.dat[-idex,]
fit<-lm(income~store_exp+online_exp+store_trans+online_trans,data=sim.dat)

## 在应变量年收入（income）上添加不同程度的噪音（均方根误差的0到3倍）
noise<-matrix(rep(NA,7*nrow(sim.dat)),nrow=nrow(sim.dat),ncol=7)
for (i in 1:nrow(sim.dat)){
  noise[i,]<-rnorm(7,rep(0,7),summary(fit)$sigma*seq(0,3,by=0.5))
}

## 拟合一般线性回归模型
rsq_linear<-rep(0,ncol(noise))
for (i in 1:7){
  withnoise<-sim.dat$income+noise[,i]
  fit0<-lm(withnoise~store_exp+online_exp+store_trans+online_trans,data=sim.dat)
  rsq_linear[i]<-summary(fit0)$adj.r.squared
}


## pls: 进行偏最小二乘回归和主成分回归
library(pls)
rsq_pls<-rep(0,ncol(noise))
## 拟合PLS模型
for (i in 1:7){
  withnoise<-sim.dat$income+noise[,i]
  fit0<-plsr(withnoise~store_exp+online_exp+store_trans+online_trans,data=sim.dat)
  ## plsr函数结果是mvr对象，需要用特定函数提取模型解释的应变量方差
  rsq_pls[i]<-max(drop(R2(fit0, estimate = "train",intercept = FALSE)$val))
}

## earth: 拟合多元自适应回归样条
library(earth)
rsq_mars<-rep(0,ncol(noise))
## 拟合多元自适应回归样条
for (i in 1:7){
  withnoise<-sim.dat$income+noise[,i]
  fit0<-earth(withnoise~store_exp+online_exp+store_trans+online_trans,data=sim.dat)
  ## 提取模型解释的应变量方差
  rsq_mars[i]<-fit0$rsq
}

## caret: 用于建立预测模型的包，可以拟合多种模型
library(caret)
rsq_svm<-rep(0,ncol(noise))
## 拟合支持向量机
## 注意：运行需要一些时间
for (i in 1:7){
  idex<-which(is.na(sim.dat$income))
  withnoise<-sim.dat$income+noise[,i]
  trainX<-sim.dat[,c("store_exp","online_exp","store_trans","online_trans")]
  trainY<-withnoise
  fit0<-train(trainX,trainY,method="svmRadial",
              tuneLength=15,
              trControl=trainControl(method="cv"))
  ## 提取模型解释的应变量方差
  rsq_svm[i]<-max(fit0$results$Rsquared)
}

## randomForest: 拟合随机森林模型
library(randomForest)
rsq_rf<-rep(0,ncol(noise))
## 拟合随机森林模型
## ntree=500 用500棵树
## na.action = na.omit 忽略缺失值
for (i in 1:7){
  withnoise<-sim.dat$income+noise[,i]
  fit0<-randomForest(withnoise~store_exp+online_exp+store_trans+online_trans,data=sim.dat,ntree=500,na.action = na.omit)
  ## 提取模型解释的应变量方差
  rsq_rf[i]<-tail(fit0$rsq,1)
}
## reshape2在之前介绍过，用于数据整形
library(reshape2)
rsq<-data.frame(cbind(Noise=c(0.0, 0.5, 1.0, 1.5, 2.0, 2.5, 3.0),rsq_linear,rsq_pls,rsq_mars,rsq_svm,rsq_rf))
## 将数据转化成长型
rsq<-melt(rsq,id.vars="Noise",measure.vars=c("rsq_linear","rsq_pls","rsq_mars","rsq_svm","rsq_rf"))

## 功能强大的绘图包
library(ggplot2)
## 用ggplot2包进行可视化
ggplot(data=rsq, aes(x=Noise, y=value, group=variable, colour=variable)) +
  geom_line() +
  geom_point()+
  ylab("R2") 
##-----------------------------------
## 自变量误差

## 生成噪音 
noise<-matrix(rep(NA,7*nrow(sim.dat)),nrow=nrow(sim.dat),ncol=7)
for (i in 1:nrow(sim.dat)){
  noise[i,]<-rnorm(7,rep(0,7),sd(sim.dat$online_exp)*seq(0,3,by=0.5))
}

## 拟合的模型复杂度从低到高依次为：一般线性回归，偏最小二乘回归，
## 多元自适应回归样条，支持向量机（核函数是径向基函数），随机森林。
## 代码和之前类似

## 拟合一般线性回归模型
rsq_linear<-rep(0,ncol(noise))
for (i in 1:7){
  withnoise_online<-sim.dat$online_exp+noise[,i]
  fit0<-lm(income ~ store_exp + withnoise_online + store_trans + online_trans,data=sim.dat)
  ##fit0<-lm(income ~ store_exp + store_trans + online_trans,data=sim.dat)
  
  rsq_linear[i]<-summary(fit0)$adj.r.squared
}

## pls: 进行偏最小二乘回归和主成分回归
library(pls)
rsq_pls<-rep(0,ncol(noise))
## 拟合PLS模型
for (i in 1:7){
  withnoise_online<-sim.dat$online_exp+noise[,i]
  fit0<-plsr(income~store_exp+withnoise_online+store_trans+online_trans,data=sim.dat)
  ##fit0<-plsr(income~store_exp+store_trans+online_trans,data=sim.dat)
  ## plsr函数结果是mvr对象，需要用特定函数提取模型解释的应变量方差
  rsq_pls[i]<-max(drop(R2(fit0, estimate = "train",intercept = FALSE)$val))
}

## earth: 拟合多元自适应回归样条
library(earth)
rsq_mars<-rep(0,ncol(noise))
## 拟合多元自适应回归样条
for (i in 1:7){
  withnoise_online<-sim.dat$online_exp+noise[,i]
  fit0<-earth(income~store_exp+withnoise_online+store_trans+online_trans,data=sim.dat)
  ##fit0<-earth(income~store_exp+store_trans+online_trans,data=sim.dat)
  ## 提取模型解释的应变量方差
  rsq_mars[i]<-fit0$rsq
}

## caret: 用于建立预测模型的包，可以拟合多种模型
library(caret)
rsq_svm<-rep(0,ncol(noise))
## 拟合支持向量机
## 注意：运行需要一些时间
for (i in 1:7){
  idex<-which(is.na(sim.dat$income))
  withnoise_online<-sim.dat$online_exp+noise[,i]
  trainX<-cbind(sim.dat[,c("store_exp","store_trans","online_trans")],withnoise_online)
  trainY<-sim.dat$income
  fit0<-train(trainX,trainY,method="svmRadial",
              tuneLength=15,
              trControl=trainControl(method="cv"))
  ## 提取模型解释的应变量方差
  rsq_svm[i]<-max(fit0$results$Rsquared)
}

## randomForest: 拟合随机森林模型
library(randomForest)
rsq_rf<-rep(0,ncol(noise))
## 拟合随机森林模型
## ntree=500 用500棵树
## na.action = na.omit 忽略缺失值
for (i in 1:7){
  withnoise_online<-sim.dat$online_exp+noise[,i]
  fit0<-randomForest(income~store_exp+withnoise_online+store_trans+online_trans,data=sim.dat,ntree=500,na.action = na.omit)
  ##fit0<-randomForest(income~store_exp+store_trans+online_trans,data=sim.dat,ntree=500,na.action = na.omit)
  ## 提取模型解释的应变量方差
  rsq_rf[i]<-tail(fit0$rsq,1)
}
## reshape2在之前介绍过，用于数据整形
library(reshape2)
rsq<-data.frame(cbind(Noise=c(0.0, 0.5, 1.0, 1.5, 2.0, 2.5, 3.0),rsq_linear,rsq_pls,rsq_mars,rsq_svm,rsq_rf))
## 将数据转化成长型
rsq<-melt(rsq,id.vars="Noise",measure.vars=c("rsq_linear","rsq_pls","rsq_mars","rsq_svm","rsq_rf"))
# 用ggplot2包进行可视化
ggplot(data=rsq, aes(x=Noise, y=value, group=variable, colour=variable)) +
  geom_line() +
  geom_point()+
  ylab("R2") 
