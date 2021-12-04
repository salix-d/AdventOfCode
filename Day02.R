# =========================================================================
# === salix-d; 2021.03.12
# === AdventOfCode  DAY 2
# =========================================================================

# --- PREP -----------------------------------------------------------------
# source("utils.R")
# input.mat <- "forward 5\ndown 5\nforward 8\nup 3\ndown 8\nforward 2"
# cat(input.mat, file = "input-day2.test.txt")
# input <- scan("input-day2.test.txt", what = character())
# input.mat <- vapply(1:0, function(i) input[seq(input)%%2==i], character(length(input)/2))
input.mat <- read.mat("input-day2.txt")
# --- UTILS ----------------------------------------------------------------
# calc_finalDepth <- function(input.df){
#   fun <- setNames(c(`-`, `+`), c("up","down"))
#   depth <- 0
#   for(i in which(input.df$action %in% names(fun))){
#     depth <- fun[[input.df$action[i]]](depth, input.df$val[i])
#   }
#   return(depth)
# }
# finalDepth <- function(x) sum(x$val[x$action=="down"]) - sum(x$val[x$action=="up"])
# finalHorizontal <- function(x) sum(x$val[x$action=="forward"])
# finalDepth <- function(x){
#   val <- x[,"val"]
#   action <- x[,"action"]
#   forward <- sum(df$val[df$action == "forward"])
#   down <- sum(df$val[df$action == "down"])
#   up <- sum(df$val[df$action == "up"])
#   finalDepth = down-up
#   finalHorizontal = forward
# }
# finalHorizontal <- function(x) sum(x$val[x$action=="forward"])
# calc_position.mat <- function(input.mat){
#   res <- setNames(integer(2),c("finalDepth","finalHorizontal"))
#   res["finalHorizontal"] <- sum(input.mat[input.mat[,"action"]==1,"val"])
#   res["finalDepth"] <- Reduce(`-`, vapply(2:3, function(i) sum(input.mat[input.mat[,"action"]==i,"val"]), integer(1)))
#   return(res)
# }
# calc_position.mat <- function(input.mat){
#   res <- setNames(integer(2),c("finalDepth","finalHorizontal"))
#   res["finalHorizontal"] <- sum(val[input.mat[,1]=="forward"])
#   res["finalDepth"] <- Reduce(`-`, vapply(c("down", "up"), function(i) sum(val[input.mat[,1]==i]), integer(1)))
#   return(res)
# }
calc_position <- function(mat, action, fun = `-`){
  val <- as.integer(mat[,2])
  if(length(action)==1){
    return(sum(val[mat[,1]==action]))
  } else {
    Reduce(fun, vapply(action, function(x) sum(val[mat[,1]==x]), integer(1)))
  }
  
}
calc_position.mat <- function(input.mat){
  res <- setNames(integer(2),c("finalDepth","finalHorizontal"))
  res["finalHorizontal"] <- calc_position(input.mat,"forward")
  res["finalDepth"] <- calc_position(input.mat,c("down", "up"))
  return(res)
}
# --- PUZZLE 1 -------------------------------------------------------------
prod(calc_position.mat(input.mat))

# --- PUZZLE 2 -------------------------------------------------------------

