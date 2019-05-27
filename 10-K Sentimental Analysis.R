library(knitr)
library(magrittr)
library(edgarWebR)
library(dplyr)
library(tidyverse)
library(xml2)
library(rvest)
library(tidytext)
library(wordcloud)

ticker <- 'DGSE'
years <- 5
company.details <- company_details(ticker, type = "10-K", count = years)

kable(company.details$information %>% select(name, cik, fiscal_year_end),col.names=c('Company Name', 'CIK', 'Fiscal Year End'))


filing_doc <- function(href) {
  sapply(href, function(x) {
    filing_documents(x) %>%
      filter( type == "10-K" ) %>% select(href) }) %>%
    unlist(recursive = TRUE, use.names = FALSE)
}
company.reports <- company.details$filings %>%
  filter(type == "10-K") %>%
  slice(1:years) %>% 
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
  nodes <- read_html(uri) %>% html_nodes('text') %>% xml_children()
  nodes <- nodes[xml_name(nodes) != "hr"]
  
  # Unfortunately there isn't much of a workaround to this loop - we need
  # to track position in the file so it has to be a bit sequential...
  doc.parts <- tibble(nid = seq(length(nodes)),
                      node = nodes,
                      text = xml_text(nodes) ) %>% 
    filter(text != "") # way to get columns defined properly
  
  parts <- doc.parts %>%
    filter(grepl("^part",text, ignore.case=TRUE)) %>%
    select(nid,text)
  #  mutate(next.nid = c(nid[-1],length(nodes)+1)) %>%
  if (parts$nid[1] > 1) {
    parts <- bind_rows(tibble(nid = 0, text= "PART 0"), parts)
  }
  parts <- bind_rows(parts,
                     tibble(nid = doc.parts$nid[length(doc.parts$nid)] + 1,
                            text = "NA"))
  
  items <- doc.parts %>%
    filter(grepl("^item",text, ignore.case=TRUE)) %>%
    select(nid,text) %>%
    mutate(next.nid = c(nid[-1],length(nodes)+1),
           part.next = parts$nid[findInterval(nid,parts$nid) + 1],
           next.nid = ifelse(part.next < next.nid, part.next, next.nid),
           prev.end = c(0,next.nid[-length(nid)]))
  
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

words %>%
  ungroup() %>%
  anti_join(stop_words) %>%
  ungroup() %>%
  count(word) %>%
  with(wordcloud(word,n,max.words = 75, use.r.layout=FALSE, rot.per=0.35))

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
ggplot(bing, aes(x=filing_date, y=sentiment)) +
  geom_col() +
  labs(x='Filing Date', y='Sentiment', title='10-K Sentiment')

knitr::kable(bing,
             col.names=c('Filing Date', "Neg.", "Pos.", 'Total Words', 'Sentiment',
                         'Sentiment Ratio', 'Pos. Ratio', 'Neg. Ratio'))
