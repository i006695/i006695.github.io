library(dplyr)
setwd("~/Desktop/TU/M2_計量経済")
D<-read.table("Mondai1_5.csv", header=TRUE, sep=",",fileEncoding="CP932")

### 1.のモデル（制約なし）
kaiki1 <- lm(data=D,LOGSAL ~ EDU + LOGSALBEGIN + GENDER + MINORITY)
summary(kaiki1)
RSS.UR<-sum(kaiki1$residuals^2)
RSS.UR

### 2.のモデル（制約:B_4=B_5=0)
kaiki2 <- lm(data=D,LOGSAL ~ EDU + LOGSALBEGIN)
summary(kaiki2)
RSS.R2<-sum(kaiki2$residuals^2)
RSS.R2

### 2.のF検定
F<- ( (RSS.R2-RSS.UR)/2 ) / (RSS.UR/(nrow(D) - 5))
F
alpha <-0.01
qf(1-alpha, 2, nrow(D)-5)


### 3.のモデル（制約:B_4 + B_5=0)
D<-D %>% mutate(GEN_MIN = GENDER - MINORITY)
kaiki3 <- lm(data=D,LOGSAL ~ EDU + LOGSALBEGIN + GEN_MIN)
summary(kaiki3)
RSS.R3<-sum(kaiki3$residuals^2)
RSS.R3

### ３.のF検定
F<- ( (RSS.R3-RSS.UR)/1 ) / (RSS.UR/(nrow(D) - 5))
F
alpha <-0.01
qf(1-alpha, 1, nrow(D)-5)

### ４．
plot(x=D$LOGSAL,y=kaiki1$residuals^2)

### 検算
head((D[,1]-predict(kaiki1,D[,-1]))^2)
head(kaiki1$residuals^2)

### 回帰診断
plot(kaiki1)


