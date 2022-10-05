#' Brute force knapsack
#' Implements the knapsack problem using brute force. Also, given parallel = TRUE it implements the brute
#' force knapsack using parallel computing.
#' @param x a data frame
#' @param W a positive number
#' @param parallel initialized to FALSE, if TRUE uses parallel computing
#'
#' @return a list
#' @export
#'
#' @examples
#' x <- data.frame(v = c(3, 4, 10), w = c(10, 12, 7))
#' brute_force_knapsack(x, 10)
brute_force_knapsack <-
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

    if(parallel == FALSE){
      #find all the combinations
      comb <- lapply(1:m, function(x){intToBits(x)[1:n]})
      #find total weight and value for each combination
      ws <- vapply(1:m, function(i){sum(w[which(comb[[i]] == 1)])}, numeric(1))
      vs <- vapply(1:m, function(i){sum(v[which(comb[[i]] == 1)])}, numeric(1))
    }else{
      #get the number of cores
      ncores <- parallel::detectCores()
      #find all the combinations in parallel
      comb <- parallel::mclapply(1:m, function(x){intToBits(x)[1:n]}, mc.cores = ncores)
      #find total weight and value for each combination in parallel
      ws <- as.numeric(parallel::mclapply(1:m, function(i){sum(w[which(comb[[i]] == 1)])}, mc.cores = ncores))
      vs <- as.numeric(parallel::mclapply(1:m, function(i){sum(v[which(comb[[i]] == 1)])}, mc.cores = ncores))
    }


    #pick combination with best value given constraint
    max_v <- max(vs[which(ws <= W)]) #max value given constraint

    #get index of combination corresponding to max value given constraint
    vs[which(ws > W)] <- 0
    i_max_v <- which(vs == max_v)[1] #in case of more than one comb yielding max value

    #get objects corresponding to combination
    res <- x[comb[[i_max_v]] == 1, ]

    return(list(value = max_v, elements = as.numeric(row.names(res))))
  }


