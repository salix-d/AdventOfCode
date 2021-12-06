# ==========================================================================
# === salix-d; 2021.12.05
# === AdventOfCode  DAY 5
# ==========================================================================

# --- PREP DATA ------------------------------------------------------------
input <- scan("input-day04.txt", character(), sep = ".")
input.mat <- do.call(rbind, strsplit(input , ",| -> "))
input.mat <- apply(input.mat, 2, as.integer)+1
rm(input)

# --- PUZZLE 1 -------------------------------------------------------------
get_sumOverlapHV <- function(mat){
  mat <- mat[mat[,1] == mat[,3] | mat[,2] == mat[,4],]
  n <- max(mat)
  output <- matrix(0, ncol = n+1, nrow = n+1)
  for(row in seq(nrow(mat))){
    row <- mat[row,]
    x <- row[1]:row[3] + 1
    y <- row[2]:row[4] + 1
    output[y,x] <- output[y,x]+1
  }
  return(sum(output>1))
}
get_sumOverlapHV(input.mat)
# 5632

# --- PUZZLE 2 -------------------------------------------------------------
get_sumOverlap <- function(mat, hvOnly = FALSE){
  if(hvOnly) mat <- mat[mat[,1] == mat[,3] | mat[,2] == mat[,4],]
  n <- max(mat)
  output <- matrix(0, ncol = n, nrow = n)
  for(r in seq(nrow(mat))){
    row <- mat[r,]
    x <- row[1]:row[3]
    y <- row[2]:row[4]
    if(hvOnly){
      output[y,x] <- output[y,x] + 1
    }
    else {
      coord <- cbind(x, y)
      output[coord] <-output[coord] + 1
    }
  }
  return(sum(output>1))
}

get_sumOverlap(input.mat)
# 22213
get_sumOverlap(input.mat, hvOnly = TRUE)
# 5632








