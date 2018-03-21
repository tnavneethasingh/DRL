library(xlsx)
library(caret)
library(data.table)
library(dplyr)
library(corrplot)
options(width = 130)

input_data <- read.csv("F:\\RstudioWorkspace\\KDD\\Sample.csv",header=FALSE,stringsAsFactors = FALSE)
head(input_data,1)

col_names <- sapply(input_data[1:2,],FUN=function(x) paste(x,collapse="-"))
col_names[1:2] <- c("CRUDE.TYPE","X2")
input_data <- input_data[-c(1:2),]
names(input_data) <- col_names

idataa <- apply(input_data[3:ncol(input_data)],2,FUN = function(x) as.integer(x))
input_data <- cbind(input_data[,1:2],idataa)

#Replacing NaN and empty strings with NA
input_data1 <- as.data.frame(lapply(input_data,FUN = function(x) { x <- ifelse(x=="" | is.nan(x),NA,x) }))
input_data1$NA_cols <- apply(input_data1,1,FUN = function(x) { sum(is.na(x)) })
input_data1 <- input_data1[!input_data1$NA_cols == 256,]

# Removing Description rows
desc_rows <- which(input_data1$`CRUDE.TYPE` == 'Description')
input_data1 <- input_data1[-desc_rows,]	

#Removing Total rows
total_rows <- which(input_data1$X2 == "Total")
input_data1 <- input_data1[-total_rows,]

#write.csv(input_data1,"C:\\Users\\P.Aditya\\Desktop\\input_data1_full.csv")

input_data2 <- input_data1[187:384,]
row.names(input_data2) <- NULL

input_data2$CRUDE.TYPE <- as.character(input_data2$CRUDE.TYPE)
input_data2$X2 <- as.character(input_data2$X2)

rm_rows <- which(grepl("[CV]-",input_data2$CRUDE.TYPE) & nchar(input_data2$CRUDE.TYPE) == 5)
input_data2 <- input_data2[-rm_rows,]
rm_rows <- which(input_data2$NA_cols == 254 | input_data2$NA_cols == 255)
input_data2 <- input_data2[-rm_rows,]
dim(input_data2)

input_data2$X2 <- ifelse(is.na(input_data2$X2) & !is.na(input_data2$CRUDE.TYPE),input_data2$CRUDE.TYPE,input_data2$X2)

input_data2$CRUDE.TYPE <- NULL
colnames(input_data2)[which(names(input_data2) == 'X2')] <- "CRUDE.TYPE"
input_data2$NA_cols <- NULL

input_data2$id <- 1:nrow(input_data2)
input_data2$X..1 <- NULL

melt_idata2 <- melt(input_data2,id=c("id","CRUDE.TYPE"))
cast_idata2 <- dcast(melt_idata2,formula = variable~CRUDE.TYPE,value.var="value",fun.aggregate=sum,na.rm=TRUE)


idata <- cast_idata2
idata$HK <- c(1704.7,1742.9,1759.8,1800,1800,1820.2,1816.7,1892.2,1764.7,1283,1498.1,1714.6,1628.4,1619.5,1485.2,1377.8,1495.8,1351.2,
              1581.1,1750.1,1757.9,1858.1,2022.9,1901.5,1912.8,1871.3,1873.3,1851.6,1926.4,1872.1,1868.9,1950.3,1833.1,1604.3,1717.2,1848.8,1854.2,
              1836.6,1927.6,2006.4,2006.7,1944.8,1940.1,1940.2,1956.9,1949.9,1949.9,2009.1,1975.9,2011.6,1713.3,1800.1,1906.9,1343.4,1582.2,1819.5,
              1840.7,2180.3,2185.2,2123.62,1998.88,1720,1947,2113,2164,2174,2114,1849,1889,1549,1503,1784,1529,1686,1962,2000,1620,1368,1340,2101,
              2037,1741,1551,1451,1246,1987,2641,2169,1900,1900,1900,1900,2029,2056,1996,1987,2157,2183,2182,2336,2191,2148,2096,2056,2241,2093,2000,
              2000,2000,2000,NA,2011,2241,2260,2030,2056,2090,2105,2054,2099,2147,2150,2156,2138,2000,NA,1255,1722,1713,1915,2002,2095,2068,2038,1926,1950,
              1798,1829,1900,1900,1900,1885,1900,1900,1895,1810,1700,1702,1809,1967,2122,2115,1815,1473,1696,1981,1980,1954,1954,2078,1514,1701,1528,
              1480,1506,1649,1666,1857,1639,1625,1350,1379,1388,1336,1260,1234,1146,1202,1323,1375,1520,1661,2014,2054,2049,2051,2051,1569,1534,1592,
              1278,1350,1272,1622,1762,1441,1300,1236,1820,1757,1870,1900,1882,2010,2120,2114,2234,2155,2207,1972,2003,1777,1679,1809,1708,1468,1943,
              1690,1597,1233,1200,1223,1193,1163,1284,1742,1837,1436,1658,1876,1860,1874,2089,1722,1897,1887,1949,1830,1703,1721,1723,1774,1846,1854,
              1900,1900,1900,1900,2035,1960,1948,1901,1762)

rm_rows <- which(is.na(idata$HK))
idata <- idata[-rm_rows,]


idata1 <- idata[,2:(ncol(idata)-1)]

zv <- apply(idata1,2, function(x) length(unique(x)) == 1)
idata1 <- idata1[,!zv]

idata2 <- cbind(HK = idata$HK,idata1)

####### linear model
full=lm(HK~., data=idata2)
steplmobj<-step(full, data=idata2, direction="backward")
summary(steplmobj) 


write.csv(idata2,"idata2.csv",row.names=FALSE)


########################################################
library(FactoMineR)

milkcontents<-milk
res = RegBest(y=milk[,6],x=milk[,-6])
res$best

   ### using step function
    ####### linear model
    full=lm(yield~., data=milkcontents)
    milkyieldObj<-step(full, data=milkcontents, direction="backward")
    summary(milkyieldObj) 

a<-milk
uniqueDensity<-sort(unique(milk$density))
vect<-c(1.2,1.3,1.4,1.5,1.6,1.7,1.8)
for(i in 1:nrow(a))
{
  a$newDensity[i]<-vect[which(uniqueDensity==a$density[i])]
}

a<-a[,-1]
names(a)[6]<-"density"

a<-a[,c("density","fat","protein","casein","dry","yield")]

write.csv(a,"milkcontents.csv",row.names = FALSE)


milkcontents<-a
full=lm(yield~., data=milkcontents)

summary(full) 