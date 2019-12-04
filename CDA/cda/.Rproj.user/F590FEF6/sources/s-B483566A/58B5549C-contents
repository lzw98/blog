library(cdabookdb)
library(cdabookfunc)
library(MASS)
library(dplyr)

# get the lines of file
lines<-readLines('GDS5037/data_table.csv')
len <- length(lines)

data_COL=read.csv('GDS5037/data_table.csv',encoding='UTF-8',header=TRUE,nrows = 1)
COL <- colnames(data_COL)
description <- read.csv('GDS5037/description.csv')
denote <- data.frame(ID = c('MMA','control','SA'),Y = c(1,0,1))
data <- merge(description,denote,by = 'ID')
des <- data[,-c(1,2)]


f <- function(nrows,line){
  data <- read.csv('GDS5037/data_table.csv',encoding='UTF-8',header=TRUE,nrows = nrows,skip = (line-1)*nrows)
  # bug happened this skip parameter doesn't work
  # data <- read.csv('GDS5037/data_table.csv',encoding='UTF-8',header=TRUE,nrows = nrows,skip = 1*nrows)
  coln <- as.vector(as.matrix((data[,1])))
  colnames(data) <- COL
  data2 = data
  ID_REF <- colnames(data2)
  ID_REF <- ID_REF[-c(1,2)]
  data3 <- data2[,-c(1,2)]
  data3 <- apply(data3,1,as.numeric)
  data4 <- data.frame(ID_REF,data3)
  colnames(data4) <- c('ID_REF',coln)
  data <- merge(des,data4,by = 'ID_REF');
  return(data)
}


ptm <- proc.time()

nrows <- 1000
c2 <- c(rep(10000000,10))
id <- c(rep(0,10))
lines <- len%/%nrows
# so the bug is (len%/%) need a parenthesis

for (line in 1:lines){
  if(line != lines){
    d <- f(nrows,line)
  }else{
    nrows = (len%%nrows)
    d = f(nrows,line)
  }
  
  coln <- colnames(d)[-c(1,2)]
  for(i in 1:nrows){
    m <- glm(Y~d[,i+2],family = binomial(link = 'logit'),data = d);
    if(m$aic < max(c2)){
      id[which.max(c2)] <- coln[i]# the sentence must be ahead the next one, or which.max(c2) would change
      c2[which.max(c2)] <- m$aic
    }
  }
  remove(d)
}
proc.time() - ptm

result <- list(c2,id)

library(readr)

ptm <- proc.time() #用于计算读取耗时


filter <- function (x,pos){
  subset(x, ID_REF %in% result[[2]])
}  #定义筛选条件
df <- read_csv_chunked(file = "GDS5037/data_table.csv", DataFrameCallback$new(filter), chunk_size = 1000,col_names = TRUE, progress = FALSE)
t <- proc.time() - ptm


df
pre <- function(df){
  coln <- as.vector(as.matrix((df[,1])))
  colnames(df) <- COL
  data2 = df
  ID_REF <- colnames(data2)
  ID_REF <- ID_REF[-c(1,2)]
  data3 <- data2[,-c(1,2)]
  data3 <- apply(data3,1,as.numeric)
  data4 <- data.frame(ID_REF,data3)
  colnames(data4) <- c('ID_REF',coln)
  data <- merge(des,data4,by = 'ID_REF');
  return(data)
}
data <- pre(df)

library(Hmisc)#加载包
res2 <- rcorr(as.matrix(data[,-c(1,2)]))
res2





##----------------
data<-t(read.csv("GDS5037/data_table.csv",header = FALSE))
a<-read.csv("new.csv")
denote <- data.frame(ID =c('MMA','control','SA'),Y = c(1,0,1))
b<- merge(a,denote,by = 'ID')
c<-matrix(0,nrow = 2,ncol = 41000)

for (i in 1:41000){
  c[1,i]<-i
  c[2,i]<-glm(b$Y~data[,i],family = binomial(link = 'logit'))$aic
}
d<-t(c)
e<-d[order(d[,2]),]
e[1:10,1:2]

var <- data[,34445][-c(1,2)]
var <- as.numeric(as.vector(var))
var



glm((data$Y)~(var),family = binomial(link = 'logit'))$aic












# coln <- as.vector(as.matrix((df[,1])))
# colnames(df) <- COL
# data2 = df
# ID_REF <- colnames(data2)
# ID_REF <- ID_REF[-c(1,2)]
# data3 <- data2[,-c(1,2)]
# data3 <- apply(data3,1,as.character)
# data3 <- apply(data3,1,as.numeric)
# 
# data4 <- as.data.frame(data3)
# data4 <- data.frame(ID_REF,t(data4))
# colnames(data4) <- c('ID_REF',coln)
# data <- merge(des,data4,by = 'ID_REF');





