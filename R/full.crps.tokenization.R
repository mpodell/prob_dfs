full.crps.tokenization <-
function(text, ng_min, ng_max) {
  
  library(tm)
  library(RWeka)
  library(data.table)
    
  # Break into 100x 1% samples
  sample.idx <- sample(length(text))
  sample.idx <- split(sample.idx, ceiling(seq_along(sample.idx)/(length(text)/100)))
  
  ngramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = ng_min, max = ng_max, delimiters = " \\r\\n\\t.,;:\"()?!"))
  ngrams <- data.table()
  for(i in 1:length(sample.idx)) {
    crps <- text[sample.idx[[i]]]
    ngr <- ngramTokenizer(crps)
    ngr.tbl <- data.table(phrase = ngr, freq = rep(1,length(ngr)))
    ngr.tbl <- ngr.tbl[, lapply(.SD, sum), by = c("phrase")] # aggregate
    ##  http://stackoverflow.com/questions/13673793/how-can-i-sum-values-in-a-column-grouped-by-names-in-rows-in-r-without-listing
    ngr.tbl <- ngr.tbl[ngr.tbl$freq>1] # prune
    ngrams  <- rbind(ngrams, ngr.tbl)  # append 
    ngrams <- ngrams[, lapply(.SD, sum), by = c("phrase")] # aggregate
    rm(crps, ngr, ngr.tbl)
  }
  
  # split into separate ngrams
  space.count <- nchar(gsub("[^ ]", "", ngrams$phrase)) # remove any leading spaces (presumably no 
  # trailing spaces exist.)
  
  ngram1 <- ngrams[space.count==0,]   # ng1s have only 1 word and thus no spaces
  ngram1 <- ngram1[order(-ngram1$freq),]
  
  ngram2 <- ngrams[space.count==1,]
  ngram2 <- ngram2[order(-ngram2$freq),]
  
  ngram3 <- ngrams[space.count==2,]
  ngram3 <- ngram3[order(-ngram3$freq),]
  
  ngram4 <- ngrams[space.count==3,]
  ngram4 <- ngram4[order(-ngram4$freq),]
  
  return(list(ngram1, ngram2, ngram3, ngram4))
}
