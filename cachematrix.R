# Assignment: Caching the Inverse of a Matrix 


# The purpouse of this program is to write a copule of R functions
# with the aim of Caching the inverse of a matrix rather than compute it repeatedly.

# ***************************************************************************************************************


## Put comments here that give an overall description of what your
## functions do
# ***************************************************************************************************************
# Descriptions of Two functions:

# 1. makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

# 2. cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
# If the inverse has already been calculated (and the matrix has not changed),
# then the cachesolve should retrieve the inverse from the cache.


# Assumption: The matrix supplied is always invertible.
# ***************************************************************************************************************

## Write a short comment describing this function

# ***************************************************************************************************************

# makeCacheMatrix functions do the follwing jobs

# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of the inverse
# 4. get the value of the inverse

# and finally return a list of the above objects

# ***************************************************************************************************************

makeCacheMatrix <- function(matrix = matrix()) {
        
        # The object, Matrix, to be inversed is of matrix class
        
        InverseTag <- NULL
        # Indicator whether Inverse has been claculated
        
        SetMatrix <- function(GiveMatrix) {
                
                matrix <<- GiveMatrix
                # Matrix now becomes Global object
                
                InverseTag <<- NULL
                # InverseTag now becomes Global object
        }
        
        GetMatrix <- function() matrix
        # For returning Matrix to be inversed
        
        
        SetInverse <- function(Inverse) InverseTag <<- Inverse
        # For returing Calculated Inverse
        
        GetInverse <- function() InverseTag
        # For retrieval of inverse matrix
        
        list(SetMatrix = SetMatrix, GetMatrix = GetMatrix, SetInverse = SetInverse, GetInverse = GetInverse)
        # list returned by makeCacheMatrix() function
}


## Write a short comment describing this function

# ***************************************************************************************************************

# cacheSolve() first check whether the inverse has already been in the Cache, it it is found, it directly shows
# the inverse of the matric from the Cache
# However, if that is not found in Cache, it computes the inverse by using solve() function and returns the inverse

# ***************************************************************************************************************

cacheSolve <- function(matrix, ...) {
        
        
        # Get the inverse matrix either calculated or NULL
        InverseTag <- matrix$GetInverse()
        
        # Check whether the inversion already in cache
        if(!is.null(InverseTag)) {
                
                message("Getting Cached Inverse Matrix.")
                
                return(InverseTag)
        }
        
        
        #Since inverse is not found in Cache, the following code runs
        
        message("Inverse is calculated now, not in cache.") 
        
        matrixdata <- matrix$GetMatrix()
        
        InverseTag <- solve(matrixdata)
        
        matrix$SetInverse(InverseTag)
        
        InverseTag
}


# Below is the Testing of the above functions that I run 


# > matrix=matrix(rnorm(9),ncol=3)
# > templist=makeCacheMatrix(matrix)

# > cacheSolve(templist)
# Inverse is calculated now, not in cache.
# [,1]        [,2]      [,3]
# [1,] -0.1467330  2.23734575 0.5977198
# [2,]  0.4621377 -0.50840186 0.4111126
# [3,] -0.6566900  0.09540911 0.1079592

# > cacheSolve(templist)
# Getting Cached Inverse Matrix.
# [,1]        [,2]      [,3]
# [1,] -0.1467330  2.23734575 0.5977198
# [2,]  0.4621377 -0.50840186 0.4111126
# [3,] -0.6566900  0.09540911 0.1079592

# > cacheSolve(templist)
# Getting Cached Inverse Matrix.
# [,1]        [,2]      [,3]
# [1,] -0.1467330  2.23734575 0.5977198
# [2,]  0.4621377 -0.50840186 0.4111126
# [3,] -0.6566900  0.09540911 0.1079592
