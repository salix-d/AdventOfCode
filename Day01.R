# =========================================================================
# === salix-d; 2021.03.12
# === AdventOfCode  DAY 1
# =========================================================================

# --- PREP -----------------------------------------------------------------
input.mat <- as.integer(readLines("input-day1.txt"))

# --- UTILS ----------------------------------------------------------------
notEqualPrev <- function(arr){
  out <- vapply(seq(arr)[-1], function(i){
    arr[i]>arr[i-1]
  }, logical(1))
  return(out)
}

seqBy3 <- function(arr){
  len <- length(arr)
  x <- matrix(0, nrow = 3, ncol = len)
  i <- 1
  while(max(x)!=len){
    x[,i] <- 0:2+i
    i <- i + 1
  }
  x <- x[,x[1,]!=0]
  out <- vapply(seq(ncol(x)), function(i) arr[x[,i]], numeric(3))
  return(out)
}
# --- PUZZLE 1 -------------------------------------------------------------
sum(notEqualPrev(input.mat))
# 1298
# --- PUZZLE 2 -------------------------------------------------------------
sum(notEqualPrev(colSums(seqBy3(input.mat))))
# 1248