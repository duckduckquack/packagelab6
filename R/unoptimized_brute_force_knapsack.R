unoptimized_brute_force_knapsack <-
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


    #get the number of objects
    n <- length(x$w)

    #get all bit combinations
    m <- 2^n #number of combinations
    comb <- lapply(1:m, function(x){intToBits(x)[1:n]})

    #find total weight and value for each combination
    #total weights for all the combinations
    ws <- sapply(1:m, function(i){sum(x[which(comb[[i]] == 1), ]$w)})
    #total values for all the combinations
    vs <- sapply(1:m, function(i){sum(x[which(comb[[i]] == 1), ]$v)})

    #pick combination with best value given constraint
    max_v <- max(vs[which(ws <= W)]) #max value given constraint

    #get index of combination corresponding to max value given constraint
    vs[which(ws > W)] <- 0
    i_max_v <- which(vs == max_v)[1] #in case of more than one comb yielding max value

    #get objects corresponding to combination
    res <- x[comb[[i_max_v]] == 1, ]

    return(list(value = max_v, elements = as.numeric(row.names(res))))
  }


