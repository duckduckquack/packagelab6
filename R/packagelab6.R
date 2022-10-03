#' packagelab6: A package for solving the knapsack problem.
#'
#'The packagelab6 provides three different functions to solve the knapsack problem:
#'brute_force_knapsack, knapsack_dynamic and greedy_knapsack.
#'
#'@section Brute force:
#'The brute_force_knapsack implements the brute force algorithm to solve the knapsack problem.
#'This implementation is really slow. The time complexity is O(2^n).
#'@section Dynamic:
#'The knapsack_dynamic implements the dynamic programming version of the knapsack problem.
#'The time complexity of this implementation is O(n*W).
#'@section Greedy:
#'The greedy_knapsack uses a greedy approach to solve the knapsack problem. As a result the optimal
#'solution is not always found. However, it can be shown that the solution is always at most twice as worse as
#'the optimal.The time complexity is O(n).
#'@docType package
#'@name packagelab6
NULL
