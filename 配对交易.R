library(data.table)
library(lubridate)
library(tseries)
library(urca)
library(FinTS)
library(ggplot2)
library(xts)
library(rugarch)
library(psych)

#################################
spot_1 <- fread("D:/厦门大学研究生/量化投资（赵华）/配对交易/spot_2010_2014.csv")[,2:6]
spot_2 <- fread("D:/厦门大学研究生/量化投资（赵华）/配对交易/spot_2015_2017.csv")[,2:6]
spot <- rbind(spot_1,spot_2)
names(spot) <- c("TDate","open_1","high_1","low_1","close_1")
spot[,TDate:=ymd(TDate)]

futures <- fread("D:/厦门大学研究生/量化投资（赵华）/配对交易/futures.csv")[,1:5]
names(futures) <- c("TDate","open_2","high_2","low_2","close_2")
futures[,TDate:=ymd(TDate)]

da_all <- merge(spot,futures,by="TDate")
da_all[,spread:=close_1-close_2]
ArchTest(da_all[,spread], lags=12, demean = FALSE) # ARCH LM test

for (k in 2010:2017) {
  assign(paste0("da_",k),da_all[TDate>=firstof(k) & TDate<=lastof(k),])
}
#da_2010 <- da_all[TDate>=ymd("2010-1-1") & TDate<=ymd("2010-12-31"),]
#da_2011 <- da_all[TDate>=ymd("2011-1-1") & TDate<=ymd("2011-12-31"),]
#da_2012 <- da_all[TDate>=ymd("2012-1-1") & TDate<=ymd("2012-12-31"),]
#da_2013 <- da_all[TDate>=ymd("2013-1-1") & TDate<=ymd("2013-12-31"),]
#da_2014 <- da_all[TDate>=ymd("2014-1-1") & TDate<=ymd("2014-12-31"),]
#da_2015 <- da_all[TDate>=ymd("2015-1-1") & TDate<=ymd("2015-12-31"),]
#da_2016 <- da_all[TDate>=ymd("2016-1-1") & TDate<=ymd("2016-12-31"),]
#da_2017 <- da_all[TDate>=ymd("2017-1-1") & TDate<=ymd("2017-12-31"),]

#### unit root test
adf.test(da_all[,close_1])
pp.test(da_all[,close_1])
adf.test(da_all[,close_2])
pp.test(da_all[,close_2])
# We can't reject the null hypothesis of unit root

#### test cointegration
po.test(da_all[,.(close_1,close_2)]) # Phillips-Ouliaris Cointegration Test

po_test <- ca.po(da_all[,.(close_1,close_2)],
                 demean="none",lag="long",type="Pz") # Phillips-Ouliaris Cointegration Test
summary(po_test)

jo_test <- ca.jo(da_all[,.(close_1,close_2)],
                 ecdet = "none", type="eigen", K=2, spec="longrun",season=NULL)
summary(jo_test) # Johansen procedure

#### plot
p_all <- ggplot(da_all,aes(x=TDate,y=spread))
p_all+geom_line()

#################################
#### Method 1: standardization of the spread
# We use past three years data to estimate the future mean and sd of spread.
for (k in 2013:2017) {
  assign(paste0("spread_mean_",k-3,"_",k-1),
         mean(rbind(get(paste0("da_",k-3)),get(paste0("da_",k-2)),get(paste0("da_",k-1)))[,spread]))
  assign(paste0("spread_sd_",k-3,"_",k-1),
         sd(rbind(get(paste0("da_",k-3)),get(paste0("da_",k-2)),get(paste0("da_",k-1)))[,spread]))
}

#spread_mean_2010_2012 <- mean(rbind(da_2010,da_2011,da_2012)[,spread])
#spread_mean_2011_2013 <- mean(rbind(da_2011,da_2012,da_2013)[,spread])
#spread_mean_2012_2014 <- mean(rbind(da_2012,da_2013,da_2014)[,spread])
#spread_mean_2013_2015 <- mean(rbind(da_2013,da_2014,da_2015)[,spread])
#spread_mean_2014_2016 <- mean(rbind(da_2014,da_2015,da_2016)[,spread])

#spread_sd_2010_2012 <- sd(rbind(da_2010,da_2011,da_2012)[,spread])
#spread_sd_2011_2013 <- sd(rbind(da_2011,da_2012,da_2013)[,spread])
#spread_sd_2012_2014 <- sd(rbind(da_2012,da_2013,da_2014)[,spread])
#spread_sd_2013_2015 <- sd(rbind(da_2013,da_2014,da_2015)[,spread])
#spread_sd_2014_2016 <- sd(rbind(da_2014,da_2015,da_2016)[,spread])

da_2013[,zscore_2013:=(spread-spread_mean_2010_2012)/spread_sd_2010_2012]
da_2014[,zscore_2014:=(spread-spread_mean_2011_2013)/spread_sd_2011_2013]
da_2015[,zscore_2015:=(spread-spread_mean_2012_2014)/spread_sd_2012_2014]
da_2016[,zscore_2016:=(spread-spread_mean_2013_2015)/spread_sd_2013_2015]
da_2017[,zscore_2017:=(spread-spread_mean_2014_2016)/spread_sd_2014_2016]

#################################
#### Method 2: standardization of the spread
# We use all past years data to estimate the future mean and sd of spread.
for (k in 2013:2017) {
  assign(paste0("spread_mean_",2010,"_",k-1),
         mean(da_all[TDate<firstof(k),spread]))
  assign(paste0("spread_sd_",2010,"_",k-1),
         sd(da_all[TDate<firstof(k),spread]))
}

da_2013[,zscore_2013:=(spread-spread_mean_2010_2012)/spread_sd_2010_2012]
da_2014[,zscore_2014:=(spread-spread_mean_2010_2013)/spread_sd_2010_2013]
da_2015[,zscore_2015:=(spread-spread_mean_2010_2014)/spread_sd_2010_2014]
da_2016[,zscore_2016:=(spread-spread_mean_2010_2015)/spread_sd_2010_2015]
da_2017[,zscore_2017:=(spread-spread_mean_2010_2016)/spread_sd_2010_2016]

#################################
#### trading strategy (for method 1 and method 2)
threshold_open <- seq(0.5,1,by=0.1)
threshold_close <- seq(0,0.5,by=0.1)
first_year <- 2013
last_year <- 2017
trade_array <- array(NA,dim=c(length(threshold_open),length(threshold_close),
                            length(first_year:last_year))) 
ret_array <- array(NA,dim=c(length(threshold_open),length(threshold_close),
                            length(first_year:last_year))) 
ret_array_cost <- array(NA,dim=c(length(threshold_open),length(threshold_close),
                            length(first_year:last_year)))
sd_array <- array(NA,dim=c(length(threshold_open),length(threshold_close),
                  length(first_year:last_year)))
for (i in 1:length(threshold_open)) {
  for (j in 1:length(threshold_close)) {
    assign(paste0("ret_",i,"_",j),vector())
  }
}
# Note: Why different ret_i_j has different length?
# We use na.omit() to delete lines with NA.
ret_all_array <- array(NA,dim=c(length(threshold_open),length(threshold_close),
                                nrow(da_all)))

for (k in first_year:last_year) {
  for (i in 1:length(threshold_open)) {
    for (j in 1:length(threshold_close)) {
      da_pair <- get(paste0("da_",k))
      da_pair[,zscore:=get(paste0("zscore_",k))]
      da_pair[,`:=`(longs=(zscore<=-threshold_open[i]),shorts=(zscore>=threshold_open[i]))]
      da_pair[longs==T,`:=`(postion_1=1,postion_2=-1)]
      da_pair[shorts==T,`:=`(postion_1=-1,postion_2=1)]

      position_xts_1 <- xts((da_pair[,.(postion_1,postion_2)]),
                          order.by=(da_pair[,TDate]))
      position_xts_1 <- na.locf(position_xts_1)
      postion_dt <- data.table(position_xts_1)
      postion_dt[,TDate:=index(position_xts_1)]
      da_pair[,`:=`(postion_1=NULL,postion_2=NULL)]
      da_pair <- merge(da_pair,postion_dt,by="TDate")
      da_pair[,exits:=(postion_1==1 & zscore>=-threshold_close[j]) |
                (postion_1==-1 & zscore<=threshold_close[j])]
      # Note: postion_1==1 corresponds to long value of zscore.
      da_pair[,`:=`(postion_1=NULL,postion_2=NULL)]
      da_pair[longs==T,`:=`(postion_1=1,postion_2=-1)]
      da_pair[shorts==T,`:=`(postion_1=-1,postion_2=1)]
      da_pair[exits==T,`:=`(postion_1=0,postion_2=0)]

      position_xts_2 <- xts((da_pair[,.(postion_1,postion_2)]),
                          order.by=(da_pair[,TDate]))
      position_xts_2 <- na.locf(position_xts_2)
      position_xts_3 <- lag(position_xts_2)
      trade_array[i,j,k-2012] <- sum(sign(as.numeric(position_xts_2$postion_1))
                                     !=sign(as.numeric(position_xts_3$postion_1)),
                                     na.rm=T)
      
      postion_dt <- data.table(position_xts_3)
      postion_dt[,TDate:=index(position_xts_3)]
      #postion_dt[,class(TDate)]
      
      da_pair[,`:=`(postion_1=NULL,postion_2=NULL)]
      da_pair <- merge(da_pair,postion_dt,by="TDate")
      
      da_pair[,`:=`(ret_1=c(NA,diff(log(close_1))),ret_2=c(NA,diff(log(close_2))))]
      da_pair <- na.omit(da_pair)
      
      da_pair[,ret_12:=postion_1*ret_1+postion_2*ret_2]
      ret_array[i,j,k-2012] <- da_pair[,sum(ret_12,na.rm=T)]
      ret_array_cost[i,j,k-2012] <- da_pair[,sum(ret_12,na.rm=T)]-trade_array[i,j,k-2012]*5/10000
      sd_array[i,j,k-2012] <- da_pair[,sd(ret_12,na.rm=T)]
      assign(paste0("ret_",i,"_",j),c(get(paste0("ret_",i,"_",j)),da_pair[,ret_12]))
    }
  }
}

trade_matrix <- matrix(NA,nrow=length(threshold_open),ncol=length(threshold_close))
ret_matrix <- matrix(NA,nrow=length(threshold_open),ncol=length(threshold_close))
ret_matrix_cost <- matrix(NA,nrow=length(threshold_open),ncol=length(threshold_close))
sharperatio_matrix <- matrix(NA,nrow=length(threshold_open),ncol=length(threshold_close))

for (i in 1:length(threshold_open)) {
  for (j in 1:length(threshold_close)) {
    trade_matrix[i,j] <- mean(trade_array[i,j,])
    ret_matrix[i,j] <- mean(ret_array[i,j,])
    ret_matrix_cost[i,j] <- mean(ret_array_cost[i,j,])
    sharperatio_matrix[i,j] <-  (mean(get(paste0("ret_",i,"_",j)),na.rm=T)-0.03/250)/
      sd(get(paste0("ret_",i,"_",j)),na.rm=T)*sqrt(250)
  }
}

rownames(trade_matrix) <- as.character(threshold_open)
colnames(trade_matrix) <- as.character(threshold_close)
rownames(ret_matrix) <- as.character(threshold_open)
colnames(ret_matrix) <- as.character(threshold_close)
rownames(ret_matrix_cost) <- as.character(threshold_open)
colnames(ret_matrix_cost) <- as.character(threshold_close)
rownames(sharperatio_matrix) <- as.character(threshold_open)
colnames(sharperatio_matrix) <- as.character(threshold_close)

trade_matrix 
ret_matrix
ret_matrix_cost
sharperatio_matrix

#### drawdown
drawdown_matrix <- matrix(NA,nrow=length(threshold_open),ncol=length(threshold_close)) 
drawdown_date_matrix <- matrix(NA,nrow=length(threshold_open),ncol=length(threshold_close))
drawdownduration_matrix <- matrix(NA,nrow=length(threshold_open),ncol=length(threshold_close)) 
drawdownduration_date_matrix <- matrix(NA,nrow=length(threshold_open),ncol=length(threshold_close))

FUN_DRAWDOWN <- function(dataset) {
  dataset <- as.data.frame(dataset)
  names(dataset) <- "ret"
  dataset$cumret <- cumsum(dataset$ret)
  dataset$highwatermark <- c(dataset$ret[1],rep(NA,nrow(dataset)-1))
  dataset$drawdown <- c(0,rep(NA,nrow(dataset)-1))
  dataset$drawdownduration <- c(0,rep(NA,nrow(dataset)-1))
  for(i in 2:nrow(dataset)) {
    dataset$highwatermark[i] <- max(dataset$highwatermark[i-1],dataset$cumret[i])
    dataset$drawdown[i] <- dataset$cumret[i]-dataset$highwatermark[i]
    dataset$drawdownduration[i] <- ifelse(dataset$drawdown[i]==0,0,dataset$drawdownduration[i-1]+1)
  }
  max_drawdown <<- min(dataset$drawdown)
  max_drawdown_date <<- which.min(dataset$drawdown)
  max_drawdownduration <<- max(dataset$drawdownduration)
  max_drawdownduration_date <<- which.max(dataset$drawdownduration)
}

for (i in 1:length(threshold_open)) {
  for (j in 1:length(threshold_open)) {
    FUN_DRAWDOWN(get(paste0("ret_",i,"_",j)))
    drawdown_matrix[i,j] <- max_drawdown
    drawdown_date_matrix[i,j] <- max_drawdown_date
    drawdownduration_matrix[i,j] <- max_drawdownduration
    drawdownduration_date_matrix[i,j] <- max_drawdownduration_date
  }
}
drawdown_matrix
drawdown_date_matrix
drawdownduration_matrix
drawdownduration_date_matrix

#ret_df <- ret_1_1
#ret_df <- as.data.frame(ret_df)
#names(ret_df) <- "ret"
#ret_df$cumret <- cumsum(ret_df$ret)
#ret_df$highwatermark <- c(ret_df$ret[1],rep(NA,nrow(ret_df)-1))
#ret_df$drawdown <- c(0,rep(NA,nrow(ret_df)-1))
#ret_df$drawdownduration <- c(0,rep(NA,nrow(ret_df)-1))

#for(i in 2:nrow(ret_df)) {
#  ret_df$highwatermark[i] <- max(ret_df$highwatermark[i-1],ret_df$cumret[i])
#  ret_df$drawdown[i] <- ret_df$cumret[i]-ret_df$highwatermark[i]
#  ret_df$drawdownduration[i] <- ifelse(ret_df$drawdown[i]==0,0,ret_df$drawdownduration[i-1]+1)
#}
#max_drawdown <- min(ret_df$drawdown)
#max_drawdownduration <- max(ret_df$drawdownduration)

#### sharperatio in each year
sharperatio_array <- array(NA,dim=c(length(threshold_open),length(threshold_close),
                                    length(first_year:last_year)))
sharperatio_array_cost <- array(NA,dim=c(length(threshold_open),length(threshold_close),
                                         length(first_year:last_year)))

for(k in 1:length(first_year:last_year)) { # In some cases in 2017, no trade happens.
  for (i in 1:length(threshold_open)) {
    for (j in 1:length(threshold_close)) {
      # We assume yearly risk-free rate is 0.03.
      sharperatio_array[i,j,k] <- (ret_array[i,j,k]-0.03)/
        nrow(get(paste0("da_",as.vector(first_year:last_year)[k])))/
        sd_array[i,j,k]*sqrt(250) 
      sharperatio_array_cost[i,j,k] <- (ret_array_cost[i,j,k]-0.03)/
        nrow(get(paste0("da_",as.vector(first_year:last_year)[k])))/
        sd_array[i,j,k]*sqrt(250)
    }
  }
}

#################################
#### Method 3: Use ARMA(1,1)-GARCH(1,1) model to deal with spread
first_day <- da_all[,which(TDate==as.Date("2013-1-4"),arr.ind=T)]
last_day <- da_all[,.N]
my_spec <- ugarchspec(variance.model=list(model="sGARCH", garchOrder=c(1, 1)),
                      mean.model = list(armaOrder=c(1, 1), include.mean=FALSE, 
                                        archm=FALSE, arfima=FALSE),
                      distribution.model="norm")

for (k in first_day:last_day) {
  grach_fit <- ugarchfit(da_all[1:k,spread], spec=my_spec)
  da_all[k,zscore:=grach_fit@fit$z[length(grach_fit@fit$z)]]
}
da_all <- na.omit(da_all)
#save(da_all,file="C:/Users/Ding/Desktop/da_all.RData")

####
load("D:/厦门大学研究生/量化投资（赵华）/配对交易/da_all.RData")
ArchTest(da_all[,zscore], lags=12, demean = FALSE)
describe(da_all[,zscore])

#### plot
p_zscore <- ggplot(da_all,aes(x=TDate,y=zscore))
p_zscore+geom_line()

#### trading strategy (for method 3)
threshold_open <- seq(0.5,1,by=0.1)
threshold_close <- seq(0,0.5,by=0.1)

trade_matrix <- matrix(NA,nrow=length(threshold_open),ncol=length(threshold_close)) 
ret_matrix <- matrix(NA,nrow=length(threshold_open),ncol=length(threshold_close))
ret_matrix_cost <- matrix(NA,nrow=length(threshold_open),ncol=length(threshold_close))
for (i in 1:length(threshold_open)) {
  for (j in 1:length(threshold_close)) {
    assign(paste0("ret_",i,"_",j),vector())
  }
}
# Note: Why different ret_i_j has different length?
# We use na.omit() to delete lines with NA.
sharperatio_matrix <- matrix(NA,nrow=length(threshold_open),ncol=length(threshold_close)) 

for (i in 1:length(threshold_open)) {
  for (j in 1:length(threshold_close)) {
    da_pair <- da_all
    da_pair[,`:=`(longs=(zscore<=-threshold_open[i]),shorts=(zscore>=threshold_open[i]))]
    da_pair[longs==T,`:=`(postion_1=1,postion_2=-1)]
    da_pair[shorts==T,`:=`(postion_1=-1,postion_2=1)]
    
    position_xts_1 <- xts((da_pair[,.(postion_1,postion_2)]),
                          order.by=(da_pair[,TDate]))
    position_xts_1 <- na.locf(position_xts_1)
    postion_dt <- data.table(position_xts_1)
    postion_dt[,TDate:=index(position_xts_1)]
    da_pair[,`:=`(postion_1=NULL,postion_2=NULL)]
    da_pair <- merge(da_pair,postion_dt,by="TDate")
    da_pair[,exits:=(postion_1==1 & zscore>=-threshold_close[j]) |
              (postion_1==-1 & zscore<=threshold_close[j])]
    # Note: postion_1==1 corresponds to long value of zscore.
    da_pair[,`:=`(postion_1=NULL,postion_2=NULL)]
    da_pair[longs==T,`:=`(postion_1=1,postion_2=-1)]
    da_pair[shorts==T,`:=`(postion_1=-1,postion_2=1)]
    da_pair[exits==T,`:=`(postion_1=0,postion_2=0)]
    
    position_xts_2 <- xts((da_pair[,.(postion_1,postion_2)]),
                          order.by=(da_pair[,TDate]))
    position_xts_2 <- na.locf(position_xts_2)
    position_xts_3 <- lag(position_xts_2)
    trade_matrix[i,j] <- sum(sign(as.numeric(position_xts_2$postion_1))
                                   !=sign(as.numeric(position_xts_3$postion_1)),
                                   na.rm=T)
    
    postion_dt <- data.table(position_xts_3)
    postion_dt[,TDate:=index(position_xts_3)]
    #postion_dt[,class(TDate)]
    
    da_pair[,`:=`(postion_1=NULL,postion_2=NULL)]
    da_pair <- merge(da_pair,postion_dt,by="TDate")
    
    da_pair[,`:=`(ret_1=c(NA,diff(log(close_1))),ret_2=c(NA,diff(log(close_2))))]
    da_pair <- na.omit(da_pair)
    
    da_pair[,ret_12:=postion_1*ret_1+postion_2*ret_2]
    ret_matrix[i,j] <- da_pair[,sum(ret_12,na.rm=T)]
    ret_matrix_cost[i,j] <- da_pair[,sum(ret_12,na.rm=T)]-trade_matrix[i,j]*5/10000
    assign(paste0("ret_",i,"_",j),c(get(paste0("ret_",i,"_",j)),da_pair[,ret_12]))
  }
}

for (i in 1:length(threshold_open)) {
  for (j in 1:length(threshold_close)) {
    sharperatio_matrix[i,j] <-  (mean(get(paste0("ret_",i,"_",j)),na.rm=T)-0.03/250)/
      sd(get(paste0("ret_",i,"_",j)),na.rm=T)*sqrt(250)
  }
}

rownames(trade_matrix) <- as.character(threshold_open)
colnames(trade_matrix) <- as.character(threshold_close)
rownames(ret_matrix) <- as.character(threshold_open)
colnames(ret_matrix) <- as.character(threshold_close)
rownames(ret_matrix_cost) <- as.character(threshold_open)
colnames(ret_matrix_cost) <- as.character(threshold_close)
rownames(sharperatio_matrix) <- as.character(threshold_open)
colnames(sharperatio_matrix) <- as.character(threshold_close)

trade_matrix 
ret_matrix/5
ret_matrix_cost/5
sharperatio_matrix

#### drawdown
drawdown_matrix <- matrix(NA,nrow=length(threshold_open),ncol=length(threshold_close)) 
drawdown_date_matrix <- matrix(NA,nrow=length(threshold_open),ncol=length(threshold_close))
drawdownduration_matrix <- matrix(NA,nrow=length(threshold_open),ncol=length(threshold_close)) 
drawdownduration_date_matrix <- matrix(NA,nrow=length(threshold_open),ncol=length(threshold_close))

FUN_DRAWDOWN <- function(dataset) {
  dataset <- as.data.frame(dataset)
  names(dataset) <- "ret"
  dataset$cumret <- cumsum(dataset$ret)
  dataset$highwatermark <- c(dataset$ret[1],rep(NA,nrow(dataset)-1))
  dataset$drawdown <- c(0,rep(NA,nrow(dataset)-1))
  dataset$drawdownduration <- c(0,rep(NA,nrow(dataset)-1))
  for(i in 2:nrow(dataset)) {
    dataset$highwatermark[i] <- max(dataset$highwatermark[i-1],dataset$cumret[i])
    dataset$drawdown[i] <- dataset$cumret[i]-dataset$highwatermark[i]
    dataset$drawdownduration[i] <- ifelse(dataset$drawdown[i]==0,0,dataset$drawdownduration[i-1]+1)
  }
  max_drawdown <<- min(dataset$drawdown)
  max_drawdown_date <<- which.min(dataset$drawdown)
  max_drawdownduration <<- max(dataset$drawdownduration)
  max_drawdownduration_date <<- which.max(dataset$drawdownduration)
}

for (i in 1:length(threshold_open)) {
  for (j in 1:length(threshold_open)) {
    FUN_DRAWDOWN(get(paste0("ret_",i,"_",j)))
    drawdown_matrix[i,j] <- max_drawdown
    drawdown_date_matrix[i,j] <- max_drawdown_date
    drawdownduration_matrix[i,j] <- max_drawdownduration
    drawdownduration_date_matrix[i,j] <- max_drawdownduration_date
  }
}
drawdown_matrix
drawdown_date_matrix
drawdownduration_matrix
drawdownduration_date_matrix









