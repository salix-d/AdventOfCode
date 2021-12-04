# =========================================================================
# === salix-d; 2021.03.12
# === AdventOfCode  DAY 3
# =========================================================================

# --- PREP -----------------------------------------------------------------
input.mat <- read.input("input-day3.txt")

# --- UTILS ----------------------------------------------------------------
arrBit2Int <- function(arr){
  return(strtoi(paste(arr, collapse=""), base=2))
}

get_mostCommon <- function(mat){
  if(!is.null(dim(mat))){
    n <- round(colMeans(mat+1))
  } else {
    n <- round(mean(mat+1))
  }
  return((0:1)[n])
}

# --- PUZZLE 1 -------------------------------------------------------------
get_powerConsumption <- function(input.mat){
  g <- get_mostCommon(input.mat)
  return(arrBit2Int(g) * arrBit2Int(abs(g-1)))
}
get_powerConsumption(input.mat)
#3277364

# --- PUZZLE 2 -------------------------------------------------------------
# --- V1 ----
# get_lifeSupportRating <- function(input.mat){
#   out <- vapply(c(`>=`, `<`), function(fun){
#     for(n in seq(ncol(input.mat))){
#       bit <- (0:1)[sum(fun(sum(input.mat[,n])*2, nrow(input.mat)), 1)]
#       test <- input.mat[,n] == bit
#       input.mat <- input.mat[test,]
#       if(is.null(dim(input.mat))) break
#     }
#     return(strtoi(paste(input.mat, collapse=""), base=2))
#   }, numeric(1))
#   return(prod(out))
# }
# --- V2 ----
get_lifeSupportRating <- function(input.mat){
  out <- vapply(0:1, function(i){
    for(n in seq(ncol(input.mat))){
      bit <- abs(i-get_mostCommon(input.mat[,n]))
      input.mat <- input.mat[input.mat[,n] == bit,]
      if(is.null(dim(input.mat))) break
    }
    return(arrBit2Int(input.mat))
  }, numeric(1))
  return(prod(out))
}
get_lifeSupportRating(input.mat)
#5736383