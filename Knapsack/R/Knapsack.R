
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


#' Solve the 0-1 knapsack problem (dynamic programming approach)
#' @param x A dataframe with weights (w) and values (v).
#' @param W the knapsack size.
#' @return a list with the maximum value and the indexes of the elements that compose this optimal solution 

dynamic_knapsack<-function(x,W)
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
  m<-matrix(nrow = n+1, ncol = W+1)
  keep<-matrix(rep(FALSE, (n+1)*(W+1)), nrow = n+1, ncol = W+1)
  
  for (j in 0:W)
  {
    m[1, j+1]<-0 
  }
      
  for (i in 1:n)
  {
    for (j in 0:W) 
    {
      if (w[i] > j)
      {
        m[i+1, j+1]<- m[i-1+1, j+1]
        keep[i+1, j+1]<-FALSE
      }
      else
      {
        #m[i+1, j+1] <-max(m[i-1+1, j+1], m[i-1+1, j-w[i]+1] + v[i])
        
        if (m[i-1+1, j+1] < m[i-1+1, j-w[i]+1] + v[i])
        {
          m[i+1, j+1] <- m[i-1+1, j-w[i]+1] + v[i]
          keep[i+1, j+1]<-TRUE
        }
        else
        {
          m[i+1, j+1] <- m[i-1+1, j+1]
          keep[i+1, j+1]<-FALSE
        }
      }
    }
  }
  
  elements <- c()
  K <- W
  for(i in n:0)
  {
    if(keep[i+1, K+1])
    {
      elements <- c(elements, i)
      K <- K - w[i]
    }
  }
  
  sol<-list(value=as.numeric(m[n+1, W+1]),elements=elements)
  
  return(sol)
}


#' Solve the 0-1 knapsack problem (greedy approach)
#' @param x A dataframe with weights (w) and values (v).
#' @param W the knapsack size.
#' @return a list with the maximum value and the indexes of the elements that compose this optimal solution 

greedy_knapsack<-function(x,W)
{
  stopifnot(is.data.frame(x))
  stopifnot(length(x) == 2)
  stopifnot(names(x) == c("w", "v"))
  stopifnot(W>=0)
  
  w<-x[,1]
  v<-x[,2]
  
  stopifnot(all(w > 0))
  stopifnot(all(v > 0))
  
  best_elements<-c()
  
  rate <- v/w
  
  ranked_rate <- order(rate,decreasing = TRUE)
  
  best_weight <- 0
  best_value <- 0
  
  for (i in ranked_rate){
    if(w[i] + best_weight <= W){
      best_weight <- best_weight + w[i]
      best_value <- best_value + v[i]
      best_elements <- c(best_elements, i)
    }
    else if(w[i] + best_weight > W){
      break
    }
    
  }
  return(list(value=best_value, elements=best_elements))
}

