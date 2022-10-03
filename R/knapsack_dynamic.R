#' Dynamic programming knapsack
#' Implements the dynamic programming version of the knapsack problem.
#' @param x a data frame
#' @param W a positive number
#'
#' @return a list
#' @export
#'
#' @examples
#' x <- data.frame(v = c(3, 4, 10), w = c(10, 12, 7))
#' knapsack_dynamic(x, 10)
knapsack_dynamic <-
  function(x, W){
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

    w <- x$w
    v <- x$v
    #initialization
    n <- length(w)
    m <- matrix(0, nrow = W + 1, ncol = n + 1)
    e <- matrix(0, nrow = W + 1, ncol = n + 1)

    for (i in n:1){
      #filling cols
      for (j in 1:(W+1)){
        #filling rows
        arg1 <- m[j, i + 1] #not picking the next item
        if ((j - 1) >= w[i]){
          arg2 <- v[i] + m[j - w[i], i + 1] #picking the next item
        } else {
          arg2 <- - Inf
        }
        m[j, i] <- max(arg1, arg2)
        if (arg2 > arg1){
          e[j, (i+1):(n+1)] <- e[j - w[i], (i+1):(n+1)]
          e[j, i] <- 1
        }
      }
    }
    elements = 1:n
    res <- list(value = m[W + 1, 1], elements = elements[which(e[W + 1, ] == 1)])
    res
  }


