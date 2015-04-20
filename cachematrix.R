# The purpose of these functions is to cache an invertable matrix.
# The first function (makeCacheMatrix) is used for setting the the standard and invertable matrix,
# it also is used to return the details of the stored standard and invertable matrix.
# The first function is also used by the second function (cacheSolve) for caching an 
# invertable matrix, if there is no defined inverted matrix then one is evaluated and stored, 
# once the inverted matrix is cached this function is called the cached inverted matrix is returned.
#

# makeCacheMatrix
# ---------------
# These are the functions of the MakeCacheMatrix function
# $set            - Sets new Matrix, clears inverted cache if already cached
# $get            - Returns the cached Matrix
# $setinv         - Sets a new Inverted Matrix 
# $getinv         - Returns the cached inverted matrix  
#
# There is no checking in this funtion to detemine if the matrix is invertable.
#
makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL           # Removes invert matrix from cache
      set <- function(y) {  # Seems to behave like a setter method in OOP
            x <<- y         # Passes the matrix up the env, setter
            inv <<- NULL    # Removes invert matrix from cache
      }
      get <- function() x   # Seems to behave like a getter method in OOP
      setinv <- function(solve) inv <<- solve  # Sets invert matrix
      getinv <- function() inv  # Gets inver matrix
      list( set = set, 
            get = get,
            setinv = setinv,
            getinv = getinv)  # Creates a list of functions that can be called 
}

# cacheSolve
# ----------
#
# This function will cache an inverterted matrix using the makeCacheMatrix function.
# If the inverted matrix is already in cache then it will just return the cached one.
#
cacheSolve <- function(x, ...) {
      inv <- x$getinv()                   # Gets invert matrix into variable
      if(!is.null(inv)) {
            message("getting cached data") 
            return(inv)                   # Returns the matrix to tha caller
      }                                   # This if checks if matrix cached, exits if it is 
      data <- x$get()                     # Gets the non inverted matrix into a var
      inv <- solve(data, ...)             # Inverts the matrix
      x$setinv(inv)                       # Sets the new inverted to cache
      return(inv)                         # Returns the matrix to tha caller
}

# Use Case for these functions
# ----------------------------
#
# newmatrix <- matrix(1:4,2,2)      # Creates a new 2x2 matrix 
# cachedmatrix <- makeCacheMatrix() # Creates a new blank object
# cachedmatrix$set(newmatrix)       # Sets 
# cachedmatrix$get()                # Confirms new matrix has been set       
# cacheSolve(cachedmatrix)          # Caches inverted matrix and returns results
# cacheSolve(cachedmatrix)          # Returns inverted matrix from cache
# solve(newmatrix)                  # Confirms that cacheSolve does work correct
#
# newmatrix <- matrix(4:1,2,2)      # Create a new 2x2 matrix different from the first
# cachedmatrix$set(newmatrix)       # Set the new matrix to the object
# cacheSolve(cachedmatrix)          # Return new inverted matrix and confirm not cached
# cacheSolve(cachedmatrix)          # Confirm matrix is cached
# 
# This use case just tests functionality, it does not take into account 
# doing a test on a large matrix.