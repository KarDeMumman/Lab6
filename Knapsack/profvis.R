library(profvis)
suppressWarnings(RNGversion("3.5.9"))
set.seed(42)
n <- 2000
knapsack_objects <- data.frame(
  w=sample(1:4000, size = n, replace = TRUE),
  v=runif(n = n, 0, 10000)
)
# greedy_knapsack
profvis({
  pause(0.01)
  x = knapsack_objects[1:1200,]
  W = 3500
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
  list(value=round(best_value), elements=best_elements)

  pause(0.01)
  
})

# brute_force_knapsack
profvis({
  pause(0.01)
  
  x = knapsack_objects[1:8,]
  W = 3500
  stopifnot(is.data.frame(x))
  stopifnot(length(x) == 2)
  stopifnot(names(x) == c("w", "v"))
  stopifnot(W>=0)
  
  w<-x[,1]
  v<-x[,2]
  
  stopifnot(all(w > 0))
  stopifnot(all(v > 0))
  
  n<-nrow(x)
  cases <- 2^n
  
  best_value<-0
  best_elements<-c()
    for (i in 1:cases)
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
    
    m
  pause(0.01)
  
})

#dynamic_knapsack
profvis({
  pause(0.01)
  
  x = knapsack_objects[1:12,]
  W = 2000
  stopifnot(is.data.frame(x))
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
  
  sol
  pause(0.01)
  
}
)