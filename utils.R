# =========================================================================
# === salix-d; 2021.03.12
# === AdventOfCode  utils functions
# =========================================================================

read.input2arrBits <- function(inputFile){
  lines <- strsplit(readLines(inputFile), "")
  mat <- matrix(strtoi(unlist(lines)), ncol=length(lines[[1]]), byrow = T)
  return(mat)
}

read.mat <- function(inputfile, nCol = 2){
  input <- scan(inputfile, what = character())
  mat <- vapply(nCol:1-1, function(i) input[seq(input)%%nCol==i], character(length(input)/nCol))
}
