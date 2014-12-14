vector_split <-
function (v) {
  sapply(v$phase, function (x) {strsplit(as.character(x), "\\s" )})
}
