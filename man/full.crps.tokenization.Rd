\name{full.crps.tokenization}
\alias{full.crps.tokenization}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
full.crps.tokenization(text, ng_min, ng_max)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{text}{
%%     ~~Describe \code{text} here~~
}
  \item{ng_min}{
%%     ~~Describe \code{ng_min} here~~
}
  \item{ng_max}{
%%     ~~Describe \code{ng_max} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (text, ng_min, ng_max) 
{
    library(tm)
    library(RWeka)
    library(data.table)
    sample.idx <- sample(length(text))
    sample.idx <- split(sample.idx, ceiling(seq_along(sample.idx)/(length(text)/100)))
    ngramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = ng_min, 
        max = ng_max, delimiters = " \\r\\n\\t.,;:\"()?!"))
    ngrams <- data.table()
    for (i in 1:length(sample.idx)) {
        crps <- text[sample.idx[[i]]]
        ngr <- ngramTokenizer(crps)
        ngr.tbl <- data.table(phrase = ngr, freq = rep(1, length(ngr)))
        ngr.tbl <- ngr.tbl[, lapply(.SD, sum), by = c("phrase")]
        ngr.tbl <- ngr.tbl[ngr.tbl$freq > 1]
        ngrams <- rbind(ngrams, ngr.tbl)
        ngrams <- ngrams[, lapply(.SD, sum), by = c("phrase")]
        rm(crps, ngr, ngr.tbl)
    }
    space.count <- nchar(gsub("[^ ]", "", ngrams$phrase))
    ngram1 <- ngrams[space.count == 0, ]
    ngram1 <- ngram1[order(-ngram1$freq), ]
    ngram2 <- ngrams[space.count == 1, ]
    ngram2 <- ngram2[order(-ngram2$freq), ]
    ngram3 <- ngrams[space.count == 2, ]
    ngram3 <- ngram3[order(-ngram3$freq), ]
    ngram4 <- ngrams[space.count == 3, ]
    ngram4 <- ngram4[order(-ngram4$freq), ]
    return(list(ngram1, ngram2, ngram3, ngram4))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
