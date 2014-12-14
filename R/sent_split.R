sent_split <-
function (c, annotator) {
  c <- as.String(c)
  a <- NLP::annotate(c, annotator)
  c[a]
}
