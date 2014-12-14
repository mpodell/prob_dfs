\name{process_raw_text}
\alias{process_raw_text}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
process_raw_text(sentence)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{sentence}{
%%     ~~Describe \code{sentence} here~~
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
function (sentence) 
{
    uquote <- "<U+201C>|<U+201D>|<U+00AB>|<U+00BB>"
    quote <- "\""
    tics <- "`|<U+00B4>|<U+2018>|<U+2019>"
    apostrophe <- "'"
    cant <- "\\scan't(\\s|$|,|:|;|\\.|!|\\?)"
    cannot <- " cannot "
    a_m <- "\\sa\\.m\\."
    am <- " am "
    p_m <- "\\sp\\.m\\."
    pm <- " pm "
    wont <- "\\swon't(\\s|$|,|:|;|\\.|!|\\?)"
    willnot <- " will not "
    re <- "'re(\\s|$|,|:|;|\\.|!|\\?)"
    are <- " are "
    ll <- "'ll(\\s|$|,|:|;|\\.|!|\\?)"
    will <- " will "
    ve <- "'ve(\\s|$|,|:|;|\\.|!|\\?)"
    have <- " have "
    d <- "'d(\\s|$|,|:|;|\\.|!|\\?)"
    would <- " would "
    ts <- "t's(\\s|$|,|:|;|\\.|!|\\?)"
    tis <- "t is "
    Im <- "I'm(\\s|$|,|:|;|\\.|!|\\?)"
    Iam <- "I am "
    nt <- "n't(\\s|$|,|:|;|\\.|!|\\?)"
    not <- " not "
    nonASCII <- "[<U+0080>-<U+0001FFFF>]"
    UNK <- " <UNK> "
    nothing <- ""
    profanity <- "[fF][uU][cC][kK]|[sS][hH][iI][tT]|[dD][aA][mM][nN]|[bB][iI][tT][cC][hH]|[aA][sS][sS][hH][oO][lL][eE]"
    nada <- ""
    repeated <- "(.)\\1{2,}"
    replace <- "\\1\\1"
    atEnd <- "[\\.]+$|!+$|[\\?]+$"
    inLine <- "[\\.]+|!+|[\\?]+"
    newLine <- "\\r\\n"
    sentence <- gsub(uquote, quote, sentence)
    sentence <- gsub(tics, apostrophe, sentence)
    sentence <- gsub(repeated, replace, sentence)
    sentence <- gsub(cant, cannot, sentence)
    sentence <- gsub(wont, willnot, sentence)
    sentence <- gsub(a_m, am, sentence)
    sentence <- gsub(p_m, pm, sentence)
    sentence <- gsub(re, are, sentence)
    sentence <- gsub(ll, will, sentence)
    sentence <- gsub(ve, have, sentence)
    sentence <- gsub(d, would, sentence)
    sentence <- gsub(ts, tis, sentence)
    sentence <- gsub(Im, Iam, sentence)
    sentence <- gsub(nt, not, sentence)
    sentence <- gsub(nonASCII, UNK, sentence)
    sentence <- gsub(profanity, nada, sentence)
    sentence <- tolower(sentence)
    sentence <- removePunctuation(sentence)
    sentence <- removeNumbers(sentence)
    sentence <- stripWhitespace(sentence)
    trim <- grep("^$", sentence)
    if (length(trim) > 0) 
        sentence <- sentence[-trim]
    return(sentence)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
