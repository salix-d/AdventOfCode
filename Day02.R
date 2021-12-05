# =========================================================================
# === salix-d; 2021.12.04
# === AdventOfCode  DAY 2
# =========================================================================

# --- PREP -----------------------------------------------------------------
source("utils.R")
# input.mat <- "forward 5\ndown 5\nforward 8\nup 3\ndown 8\nforward 2"
# cat(input.mat, file = "input-day2.test.txt")
# input.mat <- read.mat("input-day2.test.txt")
input.mat <- read.mat("input-day2.txt")

# --- PUZZLE 1 -------------------------------------------------------------
calc_position <- function(mat, action, fun = `-`){
  val <- as.integer(mat[,2])
  if(length(action)==1){
    return(sum(val[mat[,1]==action]))
  } else {
    Reduce(fun, vapply(action, function(x) sum(val[mat[,1]==x]), integer(1)))
  }
  
}
get_positions <- function(input.mat){
  res <- setNames(integer(2),c("finalDepth","finalHorizontal"))
  res["finalHorizontal"] <- calc_position(input.mat,"forward")
  res["finalDepth"] <- calc_position(input.mat,c("down", "up"))
  return(res)
}
prod(get_positions(input.mat))
# 2073315

# --- PUZZLE 2 -------------------------------------------------------------
cal_position2 < function(input.mat){
  horizontal <- 0
  aim <- 0
  depth <- 0
  for(r in seq(nrow(input.mat))){
    vapply(1:2, function(i) assign(value = input.mat[r,i], x = c("action", "val")[i], envir = .GlobalEnv), NA_character_)
    val <- as.integer(val)
    if(action=="forward"){
      horizontal <- horizontal + val
      depth <- depth + val * aim
    } else if(action=="down"){
      aim <- aim + val
    } else if(action=="up"){
      aim <- aim - val
    } else {
      message("error in the matrix")
    }
  }
  return(horizontal*depth)
}
cal_position2(input.mat)
# 1840311528