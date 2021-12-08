# ==========================================================================
# === salix-d; 2021.12.08
# === AdventOfCode  DAY 8
# === challenge from : https://adventofcode.com/2021/day/8
# ==========================================================================

# --- PREP DATA ------------------------------------------------------------
segPerDgt <- matrix(ncol = 2, nrow = 10)
colnames(segPerDgt) <- c("dgt", "len")
segPerDgt[,"dgt"] <- 0:9
segPerDgt[,"len"] <- c(6,2,5,5,4,5,6,3,7,6)
uniques <- segPerDgt[!segPerDgt[,2] %in% segPerDgt[,2][duplicated(segPerDgt[,2])],]
rm(segPerDgt)

# data <- matrix(scan("Day08_input_test.txt", character()), ncol = 15, byrow = TRUE)
data <- matrix(scan("Day08_input.txt", character()), ncol = 15, byrow = TRUE)

# --- PUZZLE 1 -------------------------------------------------------------
sum(vapply(data[,12:15], nchar, integer(1)) %in% uniques[,"len"])
# 255
# --- PUZZLE 2 -------------------------------------------------------------
# --- PREP -----------------------------------------------------------------
wireInDgt <- matrix(0, ncol = 7, nrow=10, dimnames = list(0:9,letters[1:7]))
wireInDgt["0",-4] <-
  wireInDgt["1",c(3,6)] <-
  wireInDgt["2",-c(2,6)] <-
  wireInDgt["3",-c(2,5)] <-
  wireInDgt["4",-c(1,5,7)] <-
  wireInDgt["5",-c(3,5)] <-
  wireInDgt["6",-3] <-
  wireInDgt["7",c(1,3,6)] <-
  wireInDgt["8",] <-
  wireInDgt["9",-5] <- 1
# check that I transcribed properly
# sapply(1:10, function(i) colnames(wireInDgt)[as.logical(wireInDgt[i,])])

# wireInDgt[c("1","4","7","8"),]
# translation lists : sum of 1 the uniques matrix for the distinct letters in the matrices of digits of 5 and 6 segments 
# 6 seg digits
# 0 d
# 6 c
# 9 e
# 5 seg digits
# 2 : b
#     f
# 3 : b
#     e
# 5 : c
#     e
trans <- list(
  `6` = list(
    "0" = 2,
    "6" = 4,
    "9" = 1
  ), 
  `5` = list(
    "2" = c(2,4),
    "3" = c(2,1),
    "5" = c(4,1)
  )
)
rm(wireInDgt)
# --------------------------------------------------------------------------
read_input <- function(dat, uniques = NULL){
  if(!is.null(dim(dat))){
    out <- vapply(seq(nrow(dat)), function(r) read_input(dat = dat[r,], uniques = uniques), array(data = double(), dim= c(10,9)))
  }
  else {
    if(is.null(uniques)){
      uniques <- get("uniques", envir  = .GlobalEnv)
      if(is.null(uniques)) stop("uniques is missing")
    }
    rnames <- dat[1:10]
    dat <- strsplit(rnames,"")
    out <- t(vapply(dat, function(x){
      as.integer(letters[1:7] %in% x)
    }, array(integer(7), dim = 7, dimnames = list(letters[1:7]))))
    out <- cbind(out, rs = rowSums(out), n = NA)
    for(i in 1:10){
      x <- out[i,"rs"]
      if(x %in% uniques[,2]){
        out[i,"n"] <- uniques[uniques[,2] == x,1]
      } else {
        out[i,"n"] <- out[i,"n"]
      }
    }
  }
  return(out)
}
add_missingDgt <- function(dat, trans = NULL, nSeg = c(6,5)){
  if(length(dim(dat))==3 && dim(dat)[3]>1){
    d <- dim(dat)
    out <- vapply(seq(d[3]), function(i){add_missingDgt(dat = dat[,,i], trans = trans, nSeg = nSeg)}, array(data = double(), dim= c(d[1],d[2])))
    return(out)
  } else {
    if(is.null(trans)){
      trans <- get("trans", envir  = .GlobalEnv)
      if(is.null(trans)) stop("Please provide a translation list")
    }
    if(length(nSeg) == 2){
      return(add_missingDgt(add_missingDgt(dat = dat, trans = trans, nSeg = 6), trans = trans, nSeg = 5))
    } else {
      tr <- trans[[as.character(nSeg)]]
      rownames(dat) <- seq(nrow(dat))
      x <- dat[dat[,"rs"] == nSeg, 1:7]
      l <- vapply(seq(nrow(x)), function(i){
        r <- rownames(x)[i]
        c <- colnames(dat)[1:7][!x[i,]]
        return(c(r,c))
      }, character((2:3)[which(6:5==nSeg)]))
      if(nSeg == 6) {
        cs <- colSums(dat[dat[,"n"] %in% c("1","4","7","8"), unlist(l[2,])])
        res <- sapply(cs, function(x) names(tr[which(tr == x)]))
      } else {
        cs <- lapply(seq(ncol(l)), function(i) colSums(dat[dat[,"n"] %in% c("1","4","7","8"), l[2:3,i]]))
        res <- sapply(cs, function(x) names(tr[which(sapply(tr, function(y) all(y %in% x)))]))
      }
      res <- setNames(res, l[1,])
      dat[names(res),"n"] <- as.integer(res)
      return(dat)
    }
  }
}

sort_signal <- function(x){
  if(length(x)>1) return(sapply(x, sort_signal))
  else return(paste(sort(strsplit(x,"")[[1]]), collapse = ""))
}

read_output <- function(transDat, output){
  checDat <- length(dim(transDat))==3
  checkIn <- !is.null(dim(output))
  if(checDat != checkIn)  stop("Must have as many decoded inputs as outputs")
  if(checDat && checkIn){
    x <- if(typeof(transDat) == "list") length(transDat) else dim(transDat)[3]
    y <- nrow(output)
    if(x != y) stop("Must have as many decoded inputs as outputs")
    return(vapply(seq(x), 
                  function(i) read_output(transDat[,,i], output[i,]), 
                  integer(1)))
  } else {
    rownames(transDat) <- sapply(1:10, function(r) paste(letters[1:7][as.logical(transDat[r, 1:7])], collapse = ""))
    output <- unname(sort_signal(output))
    if(!all(output %in% rownames(transDat))) return(double(length(output)))
    out <- paste(unname(transDat[output, "n"]), collapse = "")
    return(as.integer(out))
  }
}
parseData <- function(dat, uniques = NULL, trans = NULL, nSeg = c(6,5)){
  input <- dat[,1:10]
  output <- dat[,12:15]
  rm(dat)
  inputs <- read_input(dat = input, uniques = uniques)
  inputsDecoded <-  add_missingDgt(dat = inputs, trans = trans, nSeg = nSeg)
  rm(inputs) #free memory
  outputs <- read_output(inputsDecoded, output = output)
  return(sum(outputs))
}
parseData(data)
# 982158
