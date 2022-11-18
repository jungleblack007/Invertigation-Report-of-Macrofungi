# 大型真菌多样西调查物种分析流程
# 制作：王小黑
# 日期：20221111

# 物种调查完以后有一系列的分析，要出一些结果，手动操作很烦，于是用R语言手敲一个流程，以备日后使用。

# 日常清内存
rm(list=ls())

# 读取程辑包
library(xlsx)
library(reshape2)
library(dplyr)
library(tidyverse)
library(raster)
library(ggplot2)

# 设定分析文件夹
setwd("D:/2022 总结/20221009 浙江衢州大型真菌调查报告")
# 读取鉴定表
table <- read.csv(file = "标本号物种表.csv", sep = ",", header = T)

# 纵表转横表
# 在一些特殊的条件下需要使用
table1 <- dcast(table,Species ~ SpecimensNum)
write.xlsx(table1, "标本号物种表1.xlsx")

# 1. 添加中文名
chnnamelist <- read.xlsx("数据库 中文名.xlsx", sheetIndex = "Sheet1", header=TRUE)
## 使用left_join实现vlookup相同的功能
table <- left_join(table, chnnamelist, by = "Species")
rm(chnnamelist)

# 2. 添加物种的系统位置
## 读取mycobank制作的数据库，需要经常更新
mycobank <- read.csv("数据库 Classification 20210608.csv", header = T, sep = ",")
table <- left_join(table, mycobank, by = "Species")
rm(mycobank)
table <- separate(data = table, col = Classification, into = c("Kin.", "SubK.", "Phy.", "SubP.", "Cla.", "SubC.", "Ord.", "Fam.", "Gen."), sep = ",")
table <- trim(table)

## 由于有些物种会有亚纲，而有些没有所以对不齐。
## 于是写了一个简单的函数，首先判断科的位置对不对，
## 如果不对，就往后移一格
for ( i in 1:nrow(table)){
  if (is.na(table[i, 4]) == FALSE){
    if(str_sub(table[i,10], -4)=="ceae"){
      table[i,12] = table[i,11]
      table[i,11] = table[i,10]
      table[i,10] = table[i,9]
      table[i,9] = NA
    }
  }
}
rm(i)
## 因为有很多东西Mycobank里边可能找不到，
## 所以这里需要手动筛选一下一些没有定到系统位置的类群
# write.xlsx(table, "table_positions.xlsx", "Sheet1")
# table <- read.xlsx("table_positions.xlsx", "Sheet1", header = TRUE)

## 删掉Kin.为NA的行，没有Kin.就肯定是没有定出系统位置的
table1 <- table %>% drop_na(Kin.)
rm(table)

## 对界门纲目科属种的组成进行分析
n_kin <- length(unique(table1$Kin.))
n_phy <- length(unique(table1$Phy.))
n_cla <- length(unique(table1$Cla.))
n_ord <- length(unique(table1$Ord.))
n_fam <- length(unique(table1$Fam.))
n_gen <- length(unique(table1$Gen.))
n_spe <- length(unique(table1$Species))

text <- paste(n_kin,"界",n_phy,"门",n_cla,"纲",n_ord,"目",n_fam,"科",n_gen,"属",n_spe,"种")

## 优势科分析
## 优势属或其他分类单元类似
fam <- as.data.frame(table(table1$Fam.))
fam$frequence<-prop.table(fam$Freq)
fam$percent <- round(fam$frequence*100, 2)
fam_dmnt <- fam[which(fam$percent>5),]
fam_dmnt <- fam_dmnt[order(fam_dmnt$percent, decreasing = TRUE),]

text <- paste(text,"\n","大型真菌的优势科（占比5%以上）分别为：")
for (i in 1:nrow(fam_dmnt)){
  text <- paste(text,fam_dmnt[i,1],fam_dmnt[i,4],"%，")
}

p <- group_by(mpg, class) %>%
  summarise(percent = n() / nrow(mpg)) %>%
  ggplot(aes(x = factor(1), y = percent, fill = class)) +
  geom_col(colour = "white")

rm(fam,fam_dmnt)
# 3. 添加红色名录信息
redlist <- read.xlsx("数据库 红色名录 大型真菌卷.xlsx", "担子&子囊", header = TRUE)
redlist <- redlist[,-c(1:4)]
redlist <- redlist[,-3]
table1 <- left_join(table1,redlist,by=c("Species" = "Scientific.Names"))

## 濒危等级统计
status <- as.data.frame(table(table1$Status))
text <- paste(text, '\n', "物种濒危等级","\n :")
for (i in 1:nrow(status)){
  text <- paste(text,status[i,1],status[i,2],"种,")
}

endemic <- as.data.frame(table(table1$Endemic))
text <- paste(text, "中国特有种", endemic[1,2], "种")

rm(redlist,status,endemic)

## 4. 添加食用、药用、有毒信息
emp <- read.xlsx("数据库 食用菌、药用菌、毒菌名录.xlsx","Sheet1", header=TRUE)
table1 <- left_join(table1,emp,by=c("Species"="物种名"))
table1 <- table1[,-18]
edible <- as.data.frame(table(table1$食用))
text <- paste(text, "，食用菌", edible[2,2], "种")
medicine <- as.data.frame(table(table1$药用))
n_med <- sum(medicine$Freq)-medicine[1,2]
text <- paste(text, "，药用菌", n_med, "种")
poisonous <- as.data.frame(table(table1$有毒))
text <- paste(text, "，毒菌", poisonous[2,2], "种")
rm(emp, edible, medicine, poisonous)

## 写入一个excel
write.xlsx(table1, "table1.xlsx", sheetName = "Sheet1")