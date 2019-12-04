data <- read.csv('GDS5037/data_table.csv',encoding='UTF-8',header=TRUE,nrows = 10,skip = line-1)
coln <- as.character(data[,1])
colnames(data) <- COL
data2 = data
ID_REF <- colnames(data2)
ID_REF <- ID_REF[-c(1,2)]
data3 <- data2[,-c(1,2)]
data3 <- apply(data3,1,as.numeric)
data4 <- data.frame(ID_REF,data3)
colnames(data4) <- c('ID_REF',coln)
data <- merge(des,data4,by = 'ID_REF');


c2 <- c(rep(10000000,10))
id <- c(rep(0,10))
for(i in 1:10){m <- glm(Y~data[,i+2],family = binomial(link = 'logit'),data = data);
if(m$aic < max(c2)){
  c2[which.max(c2)] <- m$aic
  id[which.max(c2)] <- coln[i]
  id
}}


id
c2
