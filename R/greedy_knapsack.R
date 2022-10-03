#' Greedy heuristic knapsack
#' Implement the greedy heuristic version of the knapsack problem
#' @param x data frame
#' @param W a positive number
#'
#' @return a list
#' @export
#'
#' @examples
#' x <- data.frame(v = c(3, 4, 10), w = c(10, 12, 7))
#' greedy_knapsack(x, 10)
greedy_knapsack <-
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

    #extract values
    v <- x$v
    #extract weights
    w <- x$w
    #get number of items
    n <- length(x$v)

    #step 2 --- get items with biggest ratio
    #compute v/w ratio  for each element
    r <- v / w

    #sort items by ratio in decreasing order
    s_r <- sort(r, decreasing = TRUE)

    #initialize vector of zeros for the selected elements
    #1 corresponds to picked elements
    e<- rep(0, n)
    #initialize current capacity
    c <- W

    #loop through the ratios
    for(i in s_r){
      #get index of the item corresponding to current ratio
      item <- which(r == i)[1] #in case of items with same ratio
      if (w[item] <= c){
        #add element
        e[item] <- 1
        #reduce capacity
        c <- c - w[item]
      }
    }

    elements <- which(e == 1)
    value <- sum(v[elements])



    #step 3 --- get items with biggest values
    if (length(elements) != n){
      #sort items in terms of value only
      s_v <- sort(v, decreasing = TRUE)
      #initialize vector of zeros for the selected items
      e <- rep(0, n)
      #initialize current capacity
      c <- W

      #loop through the values
      for(i in s_v){
        #get index of item corresponding to current value
        item <- which(v == i)[1] #in case of items with same value
        if (w[item] <= c){
          #add item
          e[item] <- 1
          #reduce capacity
          c <- c - w[item]
        }
      }
      elements2 <- which(e == 1)
      value2 <- sum(v[elements2])

      #compare solutions in step2 and 3
      if (value2 > value){
        value <- value2
        elements <- elements2
      }
    }

    res <- list(value = value, elements = elements)
    res
  }

