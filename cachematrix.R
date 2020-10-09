## Hi! The following two function and a test below is a Coursera programming assigment.
## THe first function (makeCacheMatrix) is used to create a matrix, and store an inversion (in case already calculated), pretty simular to the example.
## The second function (cacheSolve) is used to solve the matrix and cache the value for the future use. GINV() function is used to solve the matrix (MASS library required)

library (MASS)

makeCacheMatrix <- function(x = matrix()) {

inverse_value = NULL

  set = function (y)
  {
    x <<- y
    inverse_value <<- NULL
  }
  
  get = function () x
  
  set_inverse = function (inverse) inverse_value <<- inverse
  
  get_inverse = function () inverse_value

  list (set=set, get=get, set_inverse = set_inverse, get_inverse = get_inverse)

}


## A matrix inversion value is required from the newly created matrix. In case the value is found then no further calculation is required and the function return inversion value
## Otherwise matrix inverse is calculated using GINV function of the MASS library, which return a Moore-Penrose generalized inverse of a matrix, which is faster than solve(), 
## the calculated value is passed to the matrix (set_inverse) and returned.

cacheSolve <- function(x, ...) {

inverse_value = x$get_inverse()

  if(!is.null(inverse_value))
  {
    message("getting cashed data")
    return (inverse_value)
  }
  
  inverse_value = ginv(mtx$get())
  
  x$set_inverse(inverse_value)
  
  inverse_value
}       

#creating a random matrix to test the solution
mtx = makeCacheMatrix(matrix(rnorm(9,5,1),nrow=3,ncol=3))

mtx$get()
#Random matrix
##          [,1]     [,2]     [,3]
## [1,] 3.948115 3.569542 6.822109
## [2,] 5.266728 4.059193 4.952060
## [3,] 5.139120 3.460574 5.880460

cacheSolve(mtx)
##Result of calculations
##           [,1]       [,2]       [,3]
## [1,] -0.6064805 -0.2358078  0.9021760
## [2,]  0.4973623  1.0667665 -1.4753525
## [3,]  0.2373311 -0.4216983  0.2498404

cacheSolve(mtx)
##Result of second call - no callculations - getting stored data
##getting cashed data
##           [,1]       [,2]       [,3]
## [1,] -0.6064805 -0.2358078  0.9021760
## [2,]  0.4973623  1.0667665 -1.4753525
## [3,]  0.2373311 -0.4216983  0.2498404

mtx$get_inverse()
##checking data stored in the matrix after the calculations
##           [,1]       [,2]       [,3]
## [1,] -0.6064805 -0.2358078  0.9021760
## [2,]  0.4973623  1.0667665 -1.4753525
## [3,]  0.2373311 -0.4216983  0.2498404