# =========================================================================
# === salix-d; 2021.12.03
# === AdventOfCode  DAY 3
# =========================================================================

# --- PREP DATA ------------------------------------------------------------
input <- scan("input-day3.txt", character())
make_bimat <- function(input){
  lines <- strsplit(input, "")
  mat <- t(vapply(lines, strtoi, integer(12)))
  return(mat)
}
input.mat <- make_bimat(input)
rm(input)
# --- UTILS ----------------------------------------------------------------
biArr2Int <- function(biArr){
  biChar <- paste(biArr, collapse="")
  bi <- strtoi(biChar, base=2)
  return(bi)
}

get_mostCommon <- function(arr, i){ UseMethod("get_mostCommon") } 
get_mostCommon.matrix <- function(arr){
  return(round(colMeans(arr)+1)-1)
} 
get_mostCommon.integer <- function(arr, i){
  return(round(mean(arr)+1)-1)
}

# --- PUZZLE 1 -------------------------------------------------------------
get_powerConsumption <- function(input.mat){
  g <- get_mostCommon(input.mat)
  e <- as.integer(!g)
  return(biArr2Int(g) * biArr2Int(e))
}
get_powerConsumption(input.mat)
#3277364

# --- PUZZLE 2 -------------------------------------------------------------
get_lifeSupportRating <- function(input.mat){
  out <- vapply(1:2, function(i){
    test <- c(`!=`, `==`)
    for(n in seq(ncol(input.mat))){
      bit <- get_mostCommon(input.mat[,n])
      rows <- test[[i]](input.mat[,n], bit)
      input.mat <- input.mat[rows,]
      if(is.null(dim(input.mat))) break
    }
    return(biArr2Int(input.mat))
  }, numeric(1))
  return(prod(out))
}
get_lifeSupportRating(input.mat)
#5736383