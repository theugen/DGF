# Setting the value of the piezometric height to the east and to
# the west of a confined aquifer
# x is the dataset, a is the value of piezometric height (h) at east,
# b is the value of h at west.

set_dirichlet <- function(x, a, b){
  x[,1] <- a # Setting first column as value a
  x[, ncol(x)] <- b # Setting last column as value b
  x
}

# Computing the value of h, x is the matrix representing
# the aquifer, threshold is the difference between the values
# of h in two subsequent steps of the cycle

get_h <- function(x, threshold = 1e-6){
  diff <- x[1,1]
  while(diff > threshold){
    for(i in 1: length(x[,1])){  # Cycling over rows
      for(j in 2:(length(x[1,])-1)){ # Cycling over columns
        if(i != 1 && i!=length(x[,1])){
          x[i,j] <- (x[i+1,j] + x[i-1,j] + x[i, j+1] + x[i, j-1])/4
        }
        else if(i==1){
          x[i,j] <- (x[i+1,j] + x[i, j+1] + x[i, j-1])/3
        }
        else if(i==length(x[,1])){
          x[i,j] <- (x[i-1,j] + x[i, j+1] + x[i, j-1])/3
        }
      }
    #if(i != 1 && i!=length(x[,1])){ 
    #}
    }
    diff <- diff - x[2,2]
  }
  x
}