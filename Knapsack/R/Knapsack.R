#Generate the data

set.seed(42)
n<-20 
knapsack_object<-data.frame(w=sample(1:40,size = n,replace = TRUE), v=runif(n=n,0,10))

#knapsack_brute_force<-function(x,W)
#{
  
  
#}