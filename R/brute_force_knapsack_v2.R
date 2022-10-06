#' Version two of the brute force knapsack after suggestion at the seminar.
#' This version of the function, initializes the parallelization only once.
#' @param x a data frame
#' @param W a positive number
#' @param parallel initialized to FALSE, if TRUE uses parallel computing
#'
#' @return a list
#' @export
#'
#' @examples
#' x <- data.frame(v = c(3, 4, 10), w = c(10, 12, 7))
#' brute_force_knapsack_v2(x, 10)
brute_force_knapsack_v2 <-
  function(x, W, parallel = FALSE){
    #checks
    #x is a data frame
    stopifnot(is.data.frame(x))
    #W is a number
    stopifnot(is.numeric(W))
    #W is positive
    stopifnot(W >= 0)
    #x has cols v and w
    stopifnot(all(sort(names(x)) == c("v", "w")))
    #v and w are positive
    stopifnot(all(x$v > 0))
    stopifnot(all(x$w > 0))


    #get the number of objects and the vectors v and w
    n <- length(x$w)
    v <- x$v
    w <- x$w
    #if n > 32 intToBits does not generate all the combinations, we should use
    #expand.grid instead
    stopifnot(n <= 32) #stop if n is too big

    #get the number of bit combinations
    m <- 2^n #number of combinations

    #get the number of cores
    ncores <- parallel::detectCores()

    par <- function(x){
      comb <- intToBits(x)[1:n]
      ws <- sum(w[which(comb == 1)])
      vs <- sum(v[which(comb == 1)])
      return(list(comb = comb, ws = ws, vs = vs))
    }

    if (parallel == FALSE){
      l <- mapply(par, 1:m)
    }else{
      l <- parallel::mcmapply(par, 1:m, mc.cores = ncores)
    }

    vs <- as.numeric(l["vs", ])
    ws <- as.numeric(l["ws", ])
    comb <- l["comb", ]

    #pick combination with best value given constraint
    max_v <- max(vs[which(ws <= W)]) #max value given constraint

    #get index of combination corresponding to max value given constraint
    vs[which(ws > W)] <- 0
    i_max_v <- which(vs == max_v)[1] #in case of more than one comb yielding max value

    #get objects corresponding to combination
    res <- x[comb[[i_max_v]] == 1, ]

    return(list(value = max_v, elements = as.numeric(row.names(res))))
  }
