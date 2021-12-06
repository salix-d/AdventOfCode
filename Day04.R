# =========================================================================
# === salix-d; 2021.12.04
# === AdventOfCode  DAY 4
# =========================================================================

# --- PREP DATA ------------------------------------------------------------
filePath <- "input-day05.txt"
draws <-scan(filePath, sep = ',', nlines = 1, quiet = TRUE)
read.boards <- function(file){
  f <- readChar(file, file.info(file)$size)
  f <- strsplit(f, "\n\n")[[1]][-1]
  f <- unlist(strsplit(f, "\n+| +"))
  out <- matrix(strtoi(f[f!=""]), ncol=5, byrow = T)
  rownames(out) <- seq(nrow(out))
  return(out)
}
boards <- read.boards(filePath)
# --- PUZZLE 1 -------------------------------------------------------------
calc_winnerScore <- function(winnerBoard, last.n) sum(winnerBoard[winnerBoard>0])*last.n
play_bingo <- function(mat, draws, bSize = 5){
  for(n in draws){
    mat[mat==n] <- -1
    boards <- seq(1,nrow(mat), by=bSize)
    bingo <- vapply(boards, function(i){
      mat <- mat[i:(i+bSize-1),]
      any(c(colSums(mat),rowSums(mat)) == -bSize)
    }, logical(1))
    if(any(bingo)){
      i <- boards[which(bingo)]
      winner <- mat[i:(i+bSize-1),]
      break
    }
  }
  return(calc_winnerScore(winnerBoard=winner, last.n=n))
}
play_bingo(boards, draws)
# 63424
# --- PUZZLE 2 -------------------------------------------------------------
calc_winnerScore <- function(winnerBoard, lastDraw) return(sum(winnerBoard, na.rm = TRUE)*lastDraw)
check_bingo <- function(mat, boards, bSize, winners){
  bingo <- NULL
  b <- vapply(boards, function(i){
    mat <- is.na(mat[as.character(1:bSize+i-1),])
    any(c(colSums(mat),rowSums(mat)) == bSize)
  }, logical(1))
  if(any(b)){ 
    b <- boards[which(b)]
    bingo <- b[!b %in% winners]
  }
  return(bingo)
}
add_winners <- function(winners, bingo){
  i <- which.min(winners)
  if(length(bingo)>1){
    i <- i:(i+length(bingo)-1)
    winners[i] <- bingo
  } else {
    winners[[i]] <- bingo
  }
  return(winners)
}
print_bingo <- function(winners){
  if(length(winners)>1){
    winners <- gsub(",( [0-9]*)$", " and\\1", paste(winners, collapse = ", "))
  }
  message("Bingo! for the board(s) ", winners, "!")
  return(NULL)
}
play_bingo <- function(mat, draws, stopFirstWin = FALSE){
  bSize = ncol(mat)
  bAll <- b <- seq(from = 1, to = nrow(mat), by=bSize)
  nWin <- if(stopFirstWin) 1 else length(b)
  winners <- integer(nWin)
  for(n in draws){
    mat[mat==n] <- NA
    bingo <- check_bingo(mat=mat, boards=b, bSize=bSize, winners=winners)
    if(is.null(bingo) || length(bingo)==0) next
    print_bingo(which(bAll %in% bingo))
    winners <- add_winners(winners = winners, bingo = bingo)
    if(winners[[nWin]] != 0) break
    b <- b[!b %in% bingo]
    rows <- as.vector(vapply(0:4, function(i) as.character(bingo+i), character(length(bingo))))
    mat <- mat[!rownames(mat) %in% rows,]
  }
  i <- winners[[nWin]]
  winner <- mat[as.character(1:bSize+i-1),]
  return(calc_winnerScore(winnerBoard = winner, lastDraw = n))
}
play_bingo(boards, draws)
# 23541
# made it so it work with puzzle 1 too
play_bingo(boards, draws, stopFirstWin = TRUE)
# 63424
