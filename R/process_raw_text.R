process_raw_text <-
function (sentence) {
  ## create the patterns and replacements
  
  # change all " symbols to the " U+0022
  # " U+0022  quotation mark
  # “ U+201C  left double quotation mark
  # ” U+201D  right double quotation mark
  # « U+00AB  left quotation mark (French)
  # » U+00BB  right quotation mark (French)
  # „ U+201E  double low-9 quotation mark
  # ‟ U+201F   double high-reversed-9 quotation mark 
  
  uquote <- "\U0000201C|\U0000201D|\U000000AB|\U000000BB"
  quote <- "\U00000022"
  
  # change all apostrophes and single qotations to ' U+0027
  # ' U+0027  apostrophe
  # ` U+0060  grave accent
  # ´ U+00B4  acute accent
  # ‘ U+2018  left single quotation mark
  # ’ U+2019  right single quotation mark
  # ‚ U+201A  single low-9 quotation mark
  # ‛ U+201B  single high-reversed-9 quotation mark
  
  
  
  tics <- "\U00000060|\U000000B4|\U00002018|\U00002019"
  apostrophe <- "\U00000027"
  
  # can't to cannot
  cant <- "\\scan't(\\s|$|,|:|;|\\.|!|\\?)"
  cannot <- " cannot "
  
  # a.m. to am
  a_m <- "\\sa\\.m\\."
  am <- " am "
  
  # p.m. to pm
  p_m <- "\\sp\\.m\\."
  pm <- " pm "
  
  # won't to will not
  wont <- "\\swon't(\\s|$|,|:|;|\\.|!|\\?)"
  willnot <- " will not "
  
  # re to are
  re <- "'re(\\s|$|,|:|;|\\.|!|\\?)"
  are <- " are "
  
  # 'll to will
  ll <- "'ll(\\s|$|,|:|;|\\.|!|\\?)"
  will <- " will "
  
  # 've to have 
  ve <- "'ve(\\s|$|,|:|;|\\.|!|\\?)"
  have <- " have "
  
  # 'd to have 
  d <- "'d(\\s|$|,|:|;|\\.|!|\\?)"
  would <- " would "
  
  # t's to t is
  ts <- "t's(\\s|$|,|:|;|\\.|!|\\?)"
  tis <- "t is "
  
  # I'm to I am
  Im <- "I'm(\\s|$|,|:|;|\\.|!|\\?)"
  Iam <- "I am "
  
  # n't to not
  nt <- "n't(\\s|$|,|:|;|\\.|!|\\?)"
  not <- " not "
  
  # non-ascii to <UNK>
  nonASCII <- "[\U00000080-\U0001FFFF]"
  UNK <- " <UNK> "
  nothing <- ""
  
  # remove profanity
  profanity <- "[fF][uU][cC][kK]|[sS][hH][iI][tT]|[dD][aA][mM][nN]|[bB][iI][tT][cC][hH]|[aA][sS][sS][hH][oO][lL][eE]"
  nada <- ""
  
  # remove words with any given character repeated more than 3 times consecutively
  repeated <- "(.)\\1{2,}"
  replace <- "\\1\\1"
  
  ## maybe don't need this if instead use openNPL and sentDetect before process_raw_text
  # split sentences  ## this does not work once text is in a corpus. Split sentences before reading in.
  # punctuation at end of lines
  atEnd <- "[\\.]+$|!+$|[\\?]+$"
  inLine <- "[\\.]+|!+|[\\?]+"
  # newLine <- "\U0000000D\U0000000A"
  newLine <- "\\r\\n"
  
  sentence <- gsub( uquote, quote, sentence)
  sentence <- gsub( tics, apostrophe, sentence)
  sentence <- gsub( repeated, replace, sentence)
  sentence <- gsub( cant, cannot, sentence)
  sentence <- gsub( wont, willnot, sentence)
  sentence <- gsub( a_m, am, sentence)
  sentence <- gsub( p_m, pm, sentence)
  sentence <- gsub( re, are, sentence)
  sentence <- gsub( ll, will, sentence)
  sentence <- gsub( ve, have, sentence)
  sentence <- gsub( d, would, sentence)
  sentence <- gsub( ts, tis, sentence)
  sentence <- gsub( Im, Iam, sentence)
  sentence <- gsub( nt, not, sentence)
  sentence <- gsub( nonASCII, UNK, sentence)
#   sentence <- gsub( nonASCII, nothing, sentence) # may want to assign UNK to low frequency ASCII words instead
  sentence <- gsub( profanity, nada, sentence)
  #   sentence <- gsub( atEnd, nada, sentence)    # no need for this if pre- sentence tokenizing
  #   sentence <- gsub( inLine, newLine, sentence) # no need for this if pre- sentence tokenizing
  
  sentence <- tolower(sentence)
  sentence <- removePunctuation(sentence)
  sentence <- removeNumbers(sentence)
  sentence <- stripWhitespace(sentence)
  #   sentence <- removeWords(sentence, stopwords("english"))
  #   sentence <- stemDocument(sentence)
  #   sentence <- unlist(strsplit(sentence, split = " "))
  
  # get rid of any 'blank' ("") sentences
  trim <- grep("^$", sentence)
  if( length(trim) >0 ) sentence <- sentence[-trim]
  
  return(sentence)
}
