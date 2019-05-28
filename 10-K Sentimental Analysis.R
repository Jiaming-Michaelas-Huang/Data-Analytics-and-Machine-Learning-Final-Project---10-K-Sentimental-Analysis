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



ten_K_Sentimental_Analysis <- function(ticker,name){
  #knitr::spin_child("/Users/jiaminghuang/Downloads/10-K\ Sentimental\ Analysis.R")
  
  name = str_remove(name," NEW")
  name = str_remove(name," COS")
  #name = str_remove(name," INC")
  
  
  cik = cik_search(name)
  cik = cik[cik$company_name == name,]
  
  if(length(cik)>0){
    if(nrow(cik)>0){
      cik = cik$cik[nrow(cik)]
      years <- 15
      company.details <- company_details(cik, type = "10-K", count = years)
    }
  }
  
  else{
    years <- 15
    company.details <- company_details(ticker, type = "10-K", count = years)
  }
  
  
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
    
    if(length(read_html(uri) %>% html_nodes('text'))>0){
      nodes <- read_html(uri) %>% html_nodes('text') %>% xml_children()
    }
    else{
      nodes <- read_html(uri) %>% html_nodes('body') %>% xml_children()
    }
    
    if(length(nodes)<length(nodes %>% xml_children())){
      nodes <- nodes %>% xml_children()
    }
    
    
    nodes <- nodes[xml_name(nodes) != "hr"]
    
    #browser()
    # Unfortunately there isn't much of a workaround to this loop - we need
    # to track position in the file so it has to be a bit sequential...
    doc.parts <- tibble(nid = seq(length(nodes)),
                        node = nodes,
                        text = xml_text(nodes) ) %>% 
      filter(text != "") # way to get columns defined properly
    
    parts <- doc.parts %>%
      filter(grepl("(PART|Part)",text, ignore.case=FALSE)) %>%
      select(nid,text)
    
    if(nrow(parts) < 1){
      browser()
    }
    
    #  mutate(next.nid = c(nid[-1],length(nodes)+1)) %>%
    if (parts$nid[1] > 1) {
      parts <- bind_rows(tibble(nid = 0, text= "PART 0"), parts)
    }
    parts <- bind_rows(parts,
                       tibble(nid = doc.parts$nid[length(doc.parts$nid)] + 1,
                              text = "NA"))
    
    if(nrow(parts)<1){
      browser()
    }
    
    items <- doc.parts %>%
      filter(grepl("(Item|ITEM)",text, ignore.case=FALSE)) %>%
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
    group_by(filing_date) %>% 
    summarize(words = n())
  
  bing <- words %>%
    inner_join(get_sentiments("bing"), by=c("word")) %>% 
    count(filing_date, sentiment) %>%
    spread(sentiment,n,fill=0) %>%
    left_join(word.counts, by=("filing_date")) %>%
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


for (symbol in Symbols[622:length(Symbols)]) {
  if(TRUE){
    result = ten_K_Sentimental_Analysis(symbol,SPY_Stock_Data[TICKER == symbol]$COMNAM[1])
    Results = rbind(Results,result)
    print(Results)
  }
}

