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

webpage <- read_html("https://en.wikipedia.org/wiki/List_of_S%26P_500_companies")

tbls <- html_nodes(webpage, "table")

constitude = tbls[1]

Symbols = html_table(constitude)

Symbols = Symbols[[1]]

Symbols = Symbols$Symbol


count = 0

unable_tickers = c("BRK.B","BF.B")

for (symbol in Symbols[86:length(Symbols)]) {
  if(TRUE)
  {
    company_details(symbol,type = "10-K",count = 15)
    count = count + 1
    print(count)
  }
}


ten_K_Sentimental_Analysis <- function(ticker){
  #knitr::spin_child("/Users/jiaminghuang/Downloads/10-K\ Sentimental\ Analysis.R")

  years <- 15
  company.details <- company_details(ticker, type = "10-K", count = years)
  
  
  kable(company.details$information %>% select(name, cik, fiscal_year_end),col.names=c('Company Name', 'CIK', 'Fiscal Year End'))
  
  
  filing_doc <- function(href) {
    sapply(href, function(x) {
      filing_documents(x) %>%
        filter( type == "10-K" ) %>% 
        filter(str_detect(href,".htm")) %>%
        select(href) }) %>%
      unlist(recursive = TRUE, use.names = FALSE)
  }
  
  if (nrow(company.details$filings %>%
            filter(type == "10-K") %>%
            slice(1:years)) <1){
    return(NULL)
  }
  
  temp = company.details$filings %>%
    filter(type == "10-K") %>%
    slice(1:years)
  
  href = temp$href
  
  if(length(filing_doc(href))<1){
    return(NULL)
  }
  
  if(length(filing_doc(href)) != length(href)){
    return(NULL)
  }
  
  company.reports <-  temp%>% 
    mutate(doc.href = filing_doc(href),
           mdlink = paste0("[Filing Link](", href, ")"),
           reportLink = paste0("[10-K Link](", doc.href, ")")) %>%
    select(filing_date, accession_number, mdlink, reportLink, href, doc.href)
  
  knitr::kable(company.reports %>% select(-href, -doc.href),
               col.names=c('Filing Date', 'Accession #', 'Filing Link', '10-K Link'))
  
  
  #knitr::spin_child("../R/parse10k.R")
  parse10k <- function(uri) {
    # 10-K HTML files are very flat with a long list of nodes. This pulls all
    # the relevant nodes.
    
    nodes <- read_html(uri) %>% html_node('text') %>% html_children() %>% html_children()
    
    #nodes <- nodes[xml_name(nodes) != "hr"]
    # Unfortunately there isn't much of a workaround to this loop - we need
    # to track position in the file so it has to be a bit sequential...
    doc.parts <- tibble(nid = seq(length(nodes)),
                        node = nodes,
                        text =xml_text(nodes) ) %>% 
      filter(text != "") # way to get columns defined properly
    
    doc.parts$text = replace_non_ascii(doc.parts$text)
    
    parts <- doc.parts %>%
      filter(grepl("^(PART|Table of Contents PART) ",text, ignore.case=TRUE)) %>%
      select(nid,text)
    
    
    #  mutate(next.nid = c(nid[-1],length(nodes)+1)) %>%
    if (parts$nid[1] > 1) {
      parts <- bind_rows(tibble(nid = 0, text= "PART 0"), parts)
    }
    parts <- bind_rows(parts,
                       tibble(nid = doc.parts$nid[length(doc.parts$nid)] + 1,
                              text = "NA"))
    
    if(nrow(parts)<10){
      #browser()
    }
    
    
    #browser()
    if(nrow(parts)<1){
      browser()
    }
    
    items <- doc.parts %>%
      filter(grepl("^(Item|Table of Contents Item) ",text, ignore.case=TRUE)) %>%
      select(nid,text) %>%
      mutate(next.nid = c(nid[-1],length(nodes)+1),
             part.next = parts$nid[findInterval(nid,parts$nid) + 1],
             next.nid = ifelse(part.next < next.nid, part.next, next.nid),
             prev.end = c(0,next.nid[-length(nid)]))
    
    if(nrow(parts)<1){
      browser()
    }
    
    if(nrow(items) < 1){
      browser()
    }
    
    # Fill in item gaps w/ N/A
    n <- 0
    for(i in seq(length(items$nid))) {
      j <- i + n
      if(items$prev.end[j] != items$nid[j]) {
        items <- items %>% 
          add_row(nid = items$prev.end[j], text = NA, .before = j)
        n <- n + 1
      }
    }
    
    doc.parts <- doc.parts %>% 
      mutate( part = parts$text[findInterval(nid, parts$nid)],
              item = items$text[findInterval(nid, items$nid)]) %>%
      select(nid,part,item,text)
    #browser()
    
    print(parts)
    print(items)
    
    return(doc.parts)
  }
  
  data <- company.reports %>% rowwise() %>%
    mutate(nodes = map(doc.href, parse10k)) %>%
    select(-accession_number, -href, -mdlink, -doc.href, -reportLink) %>%
    ungroup() %>%
    group_by(filing_date)
  
  words <- data %>%
    unnest(nodes) %>%
    select(-nid) %>%
    unnest_tokens(word, text)
  
  #words %>%
  #  ungroup() %>%
  #  anti_join(stop_words) %>%
  #  ungroup() %>%
  #  count(word) %>%
  #  with(wordcloud(word,n,max.words = 75, use.r.layout=FALSE, rot.per=0.35))
  
  word.counts <- words %>% 
    group_by(filing_date,part,item) %>% 
    summarize(words = n())
  
  bing <- words %>%
    inner_join(get_sentiments("bing"), by=c("word")) %>% 
    count(filing_date, part,item,sentiment) %>%
    spread(sentiment,n,fill=0) %>%
    left_join(word.counts, by=c("filing_date","part","item")) %>%
    mutate(sentiment = positive-negative,
           sentiment.ratio = sentiment/words, 
           positive.ratio = positive/words, 
           negative.ratio = negative/words ) %>%
    filter(TRUE) # Noop so we can comment things in the middle for testing
  #ggplot(bing, aes(x=year(filing_date), y=positive.ratio)) +
  #  geom_col() +
  #  labs(x='Filing Date', y='Pos. Ratio', title='10-K Sentiment')
  
  #knitr::kable(bing,
  #            col.names=c('Filing Date', "Neg.", "Pos.", 'Total Words', 'Sentiment',
  #                         'Sentiment Ratio', 'Pos. Ratio', 'Neg. Ratio'))
  
  bing = as.data.table(bing)
  bing[,Year:=year(filing_date)]
  bing[,TICKER:=ticker]
  
  return(bing)
  
}


setwd("/Users/jiaminghuang/Downloads/")
SPY_Stock_Data <- as.data.table(read.csv("SPY_CRSP_Stock.csv"))
SPY_Stock_Data$date = as.Date(as.character(SPY_Stock_Data$date),format = "%Y%m%d")
SPY_Stock_Data[,Month:=month(SPY_Stock_Data$date)]
SPY_Stock_Data[,Year:=year(SPY_Stock_Data$date)]
SPY_Stock_Data$date = NULL

SPY_Stock_Data[TICKER == "WMI"]$TICKER = "WM"
SPY_Stock_Data[TICKER == "Q"]$COMNAM = "QUINTILES TRANSNATIONAL HOLDINGS INC"
SPY_Stock_Data[TICKER == "MHP"]$COMNAM = "MCGRAW HILL INC"
SPY_Stock_Data[TICKER == "FPL"]$COMNAM = "FPL GROUP INC"
SPY_Stock_Data[TICKER == "CEFT"]$COMNAM = "CONCORD EFS INC"
SPY_Stock_Data[TICKER == "SNS"]$COMNAM = "STEAK N SHAKE OPERATIONS INC"
SPY_Stock_Data[TICKER == "CUM"]$COMNAM = "CUMMINS ENGINE CO INC"
SPY_Stock_Data[TICKER == "TYC"]$COMNAM = "TYCO INTERNATIONAL LTD"
SPY_Stock_Data[TICKER == "SBC"]$COMNAM = "SBC COMMUNICATIONS INC"
SPY_Stock_Data[TICKER == "MNS"]$COMNAM = "MSC SOFTWARE CORP"
SPY_Stock_Data[TICKER == "IDPH"]$COMNAM = "IDEC PHARMACEUTICALS CORP"
SPY_Stock_Data[TICKER == "FD"]$COMNAM = "FEDERATED DEPARTMENT STORES INC"
SPY_Stock_Data[TICKER == "WMIH"]$COMNAM = "WMIH CORP"
SPY_Stock_Data[TICKER == "PLI"]$COMNAM = "PROLIANCE INTERNATIONAL"
SPY_Stock_Data[TICKER == "TMPW"]$COMNAM = "TMP WORLDWIDE INC"
SPY_Stock_Data[TICKER == "AMB"]$COMNAM = "AMB PROPERTY CORP"
SPY_Stock_Data[TICKER == "WSH"]$COMNAM = "WILLIS GROUP HOLDINGS LTD"
SPY_Stock_Data[TICKER == "CNTE"]$COMNAM = "CENTENE CORP"
SPY_Stock_Data[TICKER == "CBG"]$COMNAM = "CB RICHARD ELLIS GROUP INC"
SPY_Stock_Data[TICKER == "UARM"]$COMNAM = "UNDER ARM"
SPY_Stock_Data[TICKER == "MXB"]$COMNAM =  "MSCI INC"
SPY_Stock_Data[TICKER == "MSCI"]$COMNAM =  "MSCI INC"

SPY_Stock_Data$TICKER = as.character(SPY_Stock_Data$TICKER)
Symbols = unique(SPY_Stock_Data$TICKER)
SPY_Stock_Data$COMNAM = as.character(SPY_Stock_Data$COMNAM)

Results = NULL

count = 0

for (symbol in Symbols[238:length(Symbols)]) {
  if(symbol !="BRK.B" & symbol != "BF.B"){
    count = count + 1
    result = ten_K_Sentimental_Analysis(symbol)
    Results = rbind(Results,result)
    print(count)
    print(symbol)
    print(Results)
  }
}


SymbSentimental_Data <- as.data.table(read.csv("SP500_Sentimental_Data.csv"))
setkey(Sentimental_Data,TICKER,Year)
SPY_Stock_Data <- as.data.table(read.csv("SPY_CRSP_Stock.csv"))
SPY_Stock_Data$date = as.Date(as.character(SPY_Stock_Data$date),format = "%Y%m%d")
SPY_Stock_Data[,Month:=month(SPY_Stock_Data$date)]
SPY_Stock_Data[,Year:=year(SPY_Stock_Data$date)]
SPY_Stock_Data$date = NULL

SPY_Stock_Data[TICKER == "WMI"]$TICKER = "WM"
SPY_Stock_Data[TICKER == "Q"]$COMNAM = "QUINTILES TRANSNATIONAL HOLDINGS INC"
SPY_Stock_Data[TICKER == "MHP"]$COMNAM = "MCGRAW HILL INC"
SPY_Stock_Data[TICKER == "FPL"]$COMNAM = "FPL GROUP INC"
SPY_Stock_Data[TICKER == "CEFT"]$COMNAM = "CONCORD EFS INC"
SPY_Stock_Data[TICKER == "SNS"]$COMNAM = "STEAK N SHAKE OPERATIONS INC"
SPY_Stock_Data[TICKER == "CUM"]$COMNAM = "CUMMINS ENGINE CO INC"
SPY_Stock_Data[TICKER == "TYC"]$COMNAM = "TYCO INTERNATIONAL LTD"
SPY_Stock_Data[TICKER == "SBC"]$COMNAM = "SBC COMMUNICATIONS INC"
SPY_Stock_Data[TICKER == "MNS"]$COMNAM = "MSC SOFTWARE CORP"
SPY_Stock_Data[TICKER == "IDPH"]$COMNAM = "IDEC PHARMACEUTICALS CORP"
SPY_Stock_Data[TICKER == "FD"]$COMNAM = "FEDERATED DEPARTMENT STORES INC"
SPY_Stock_Data[TICKER == "WMIH"]$COMNAM = "WMIH CORP"
SPY_Stock_Data[TICKER == "PLI"]$COMNAM = "PROLIANCE INTERNATIONAL"
SPY_Stock_Data[TICKER == "TMPW"]$COMNAM = "TMP WORLDWIDE INC"
SPY_Stock_Data[TICKER == "AMB"]$COMNAM = "AMB PROPERTY CORP"
SPY_Stock_Data[TICKER == "WSH"]$COMNAM = "WILLIS GROUP HOLDINGS LTD"
SPY_Stock_Data[TICKER == "CNTE"]$COMNAM = "CENTENE CORP"
SPY_Stock_Data[TICKER == "CBG"]$COMNAM = "CB RICHARD ELLIS GROUP INC"
SPY_Stock_Data[TICKER == "UARM"]$COMNAM = "UNDER ARM"
SPY_Stock_Data[TICKER == "MXB"]$COMNAM =  "MSCI INC"
SPY_Stock_Data[TICKER == "MSCI"]$COMNAM =  "MSCI INC"

SPY_Stock_Data$TICKER = as.character(SPY_Stock_Data$TICKER)
SPY_Stock_Data$COMNAM = as.character(SPY_Stock_Data$COMNAM)

setkey(SPY_Stock_Data,TICKER,Year)

SPY_Stock_ALL_Data = merge(SPY_Stock_Data,Sentimental_Data,all.x = TRUE)

fwrite(SPY_Stock_ALL_Data,"SPY_Stock_ALL_Data.csv")
