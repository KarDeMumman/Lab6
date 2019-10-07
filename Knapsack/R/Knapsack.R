#Generate the data
#From the way we generate the data, we conculde that we are supposed to solve the following problem:
# The bounded 0-1 knapsack problem: removes the restriction that there is only one of each item but restricts the number of copies of each item.

#' Solve the 0-1 knapsack problem (brute force approach)
#' @param x A dataframe with weights (w) and values (v).
#' @param W the knapsack size.
#' @return a list with the maximum value and the indexes of the elements that compose this optimal solution 
brute_force_knapsack<-function(x,W)
{
  stopifnot(is.data.frame(x))
  stopifnot(length(x) == 2)
  stopifnot(names(x) == c("w", "v"))
  stopifnot(W>=0)
  
  w<-x[,1]
  v<-x[,2]
  
  stopifnot(all(w > 0))
  stopifnot(all(v > 0))
  
  n<-nrow(x)
  
  best_value<-0
  best_elements<-c()
  for (i in 1:2^n)
  {
    elements<-as.integer(intToBits(i))[1:n]
    weight<-elements %*% w
    value<-elements %*% v
    if (weight <= W && value > best_value) 
    {
      best_value<-value
      best_elements<-elements
    }
  }

  m<-list(value=as.numeric(best_value),elements=which(best_elements == 1))
  
  return(m)
}
