# =========================================================================
# === salix-d; 2021.03.12
# === AdventOfCode  utils functions
# =========================================================================

read.input <- function(filePath){
  input <- readLines("input-day3.txt")
  mat <- t(vapply(input,
                  function(x) strtoi(charToRaw(x))-30L, 
                  integer(nchar(input[[1]]))))
  return(mat)
}