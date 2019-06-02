library(knitr)
library(magrittr)
library(edgarWebR)
library(dplyr)
library(tidyverse)
library(xml2)
library(rvest)
library(tidytext)
library(wordcloud)
library(data.table)
library(stringr)
library(textclean)

setwd("/Users/jiaminghuang/Downloads/")

Sentimental_Data = as.data.table(read.csv("part_item_sentimental3.csv"))

CRSP_Data = as.data.table(read.csv("All_Sentimental_Data.csv"))
CRSP_Data$PRC = abs(CRSP_Data$PRC)
CRSP_Data$SHROUT = abs(CRSP_Data$SHROUT)
CRSP_Data[,Mkt:=PRC*SHROUT]
CRSP_Data[,lag_Mkt := shift(Mkt),by = c("TICKER")]
CRSP_Data[(!is.na(RET))&(!is.na(DLRET)),RETURN := ((1+RET)*(1+DLRET)-1)]
CRSP_Data[(is.na(RET))&(!is.na(DLRET)),RETURN := DLRET]
CRSP_Data[(!is.na(RET))&(is.na(DLRET)),RETURN := RET]
CRSP_Data[,Month:=month(date)]

parts = c("I","II","III","IV")
items = c("ITEM 1. ","ITEM 1A. ","ITEM 1B. ","ITEM 2. ","ITEM 3. ", "ITEM 4. ","ITEM 5. "
          , "ITEM 6. ","ITEM 7. ","ITEM 7A. ", "ITEM 8. " , "ITEM 9. ", "ITEM 9A. " , "ITEM 9B. ")


# correlation part


##Annual_Return = CRSP_Data[,.(Annual_Return=sum(RETURN)),by = c("TICKER","Year")]

betas = data.table(
  items = items
)

t_stat = data.table(
  items = items
)

for (it in items) {
  part_data_temp = Sentimental_Data[grepl(it,item,ignore.case = TRUE)]
  part_data = part_data_temp[,.(sentiment = mean(sentiment.ratio)),by = c("TICKER","filing_date")]
  part_data$filing_date = str_sub(part_data$filing_date,1,10)
  stock_data = CRSP_Data[,list(TICKER,Year,Month,RETURN,PRC,SHROUT,Mkt,lag_Mkt)]
  part_data[,filing_year:=year(filing_date)]
  part_data[,filing_month:=month(filing_date)]
  part_data[,Year := year(filing_date)]
  part_data[,pre_sentiment:=shift(sentiment),by=c("TICKER")]
  setkey(part_data,TICKER,Year)
  setkey(stock_data,TICKER,Year)
  ALL_data = merge(part_data,stock_data)
  ALL_data[Month<=filing_month, sentiment_ratio := pre_sentiment]
  ALL_data[Month>filing_month, sentiment_ratio := sentiment]
  ALL_data = ALL_data[!is.na(sentiment_ratio)]
  stock_data_cor = ALL_data[,list(TICKER,Year,filing_year,filing_month,Month,RETURN,sentiment_ratio)]
  stock_data_cor[Month>filing_month, Forecast_year:=Year]
  stock_data_cor[Month<=filing_month,Forecast_year:=(Year-1)]
  cor_dt = stock_data_cor[,.(Annual_Return = sum(RETURN),sentiment_ratio = mean(sentiment_ratio)),by=c("TICKER","Forecast_year")]
  l = c()
  r = c()
  count = 1
  for (y in unique(cor_dt$Forecast_year)) {
    cor_dt_temp = na.omit(cor_dt[Forecast_year == y])
    linear = lm(Annual_Return ~ sentiment_ratio,data = cor_dt_temp)
    l[count] = linear$coefficients[2]
    r[count] = summary(linear)$r.squared
    count = count + 1
  }
  l = na.omit(l)
  betas[items == it,beta:=mean(l,na.rm = TRUE)]
  t_stat[items == it,t_stat:=(mean(l,na.rm = TRUE)*sqrt(length(l)))/sd(l)]
}

setkey(betas,items)
setkey(t_stat,items)
merge(betas,t_stat)

RESULT = NULL

for (it in items) {
  part_data_temp = Sentimental_Data[grepl(it,item,ignore.case = TRUE)]
  part_data = part_data_temp[,.(sentiment = mean(sentiment.ratio)),by = c("TICKER","filing_date")]
  part_data$filing_date = str_sub(part_data$filing_date,1,10)
  stock_data = CRSP_Data[,list(TICKER,Year,Month,RETURN,PRC,SHROUT,Mkt,lag_Mkt)]
  part_data[,filing_year:=year(filing_date)]
  part_data[,filing_month:=month(filing_date)]
  part_data[,Year := year(filing_date)]
  part_data[,pre_sentiment:=shift(sentiment),by=c("TICKER")]
  setkey(part_data,TICKER,Year)
  setkey(stock_data,TICKER,Year)
  ALL_data = merge(part_data,stock_data)
  ALL_data[Month<=filing_month, sentiment_ratio := pre_sentiment]
  ALL_data[Month>filing_month, sentiment_ratio := sentiment]
  ALL_data = ALL_data[!is.na(sentiment_ratio)]
  for (y in unique(ALL_data$Year)) {
    for (m in unique(ALL_data$Month)) {
      temp = ALL_data[Year == y & Month == m]
      cut = quantile(temp$sentiment_ratio,seq(0,1,0.2))
      ALL_data[Year == y & Month == m & sentiment_ratio>=cut[1],decile:=1]
      ALL_data[Year == y & Month == m & sentiment_ratio>=cut[2],decile:=2]
      ALL_data[Year == y & Month == m & sentiment_ratio>=cut[3],decile:=3]
      ALL_data[Year == y & Month == m & sentiment_ratio>=cut[4],decile:=4]
      ALL_data[Year == y & Month == m & sentiment_ratio>=cut[5],decile:=5]
    }
  }
  ALL_data = ALL_data[!is.na(lag_Mkt)]
  item_ret = ALL_data[,.(return = weighted.mean(RETURN,lag_Mkt)),by = c("Year","Month","decile")]
  item_ret[,ITEM := it]
  setkey(item_ret,Year,Month)
  RESULT = rbind(RESULT,item_ret)
}

CRSP_Data = CRSP_Data[!is.na(lag_Mkt)]

Benchmark = CRSP_Data[,.(return = weighted.mean(RETURN,lag_Mkt)),by = c("Year","Month")]

winner = RESULT[decile == 5& ITEM == "ITEM 1. "]
loser = RESULT[decile == 1& ITEM == "ITEM 1. "]

winner$decile = NULL
winner$ITEM = NULL
loser$ITEM = NULL
loser$decile = NULL
colnames(winner) = c("Year","Month",paste0("Winner+",)
colnames(loser) = c("Year","Month","Loser")


setkey(Benchmark,Year,Month)
setkey(loser)


ALL = merge(RESULT,Benchmark,all.x = TRUE)

