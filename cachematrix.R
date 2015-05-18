# makeCacheMatrix -- creates a "matrix" object which will then have its
# matrix inverse computed using the solve() R function
# The matrix inverse will be cached unless the matrix changed or
# the set function is called, which will return the matrix inverse to
# NULL
#
# The routine accepts a matrix as input and returns a list of functions 
# within the routine which can be called
#
# This is the companion routine to cacheSolve
#
# Tony Smaldone
# May 18, 2015
#

makeCacheMatrix <- function(x = matrix()) {
   # initialize the matrix inverse to NULL
   #
   mInverse <- NULL
   # 
   # the set function can be called to reset the matrix inverse to NULL
   # which basically resets the cached inverse (and will use perhaps a
   # different matrix should the caller provide a different matrix)
   #
   set <- function(y) {
      message("Resetting the matrix inverse to NULL!")
      # the use of the superassignment operator makes sure that the
      # values of A and mInverse get raised up
      A <<- y
      mInverse <<- NULL
   }
   #
   # the get function returns the matrix
   get <- function() {A}
   #
   # the setSolve function actually stores away the passed in
   # matrix inverse. The use of the superassignment operator makes sure 
   # that the value of mInverse get raised up

   setSolve <- function(solvedInverse) {mInverse <<- solvedInverse}
   #
   # the getSolve function returns the stored matrix inverse which will
   # be either NULL if not previously computed OR will return the "cached"
   # value 
   getSolve <- function() {mInverse}
   # 
   # return the list of functions
   list(set = set, get = get, setSolve = setSolve, getSolve = getSolve)

}


# cachedSolve -- first determines whether the need exists to compute
# the matrix inverse via getting the value of the matrix inverse from
# the makeCacheMatrix routine. If it is NULL, the matrix inverse will be
# computed; if it is not NULL this implies that the matrix inverse was already
# computed and is "cached"
#
# The routine accepts the list of functions available to be called within
# makeCacheMatrix
#
# This is the companion routine to makeCacheMatrix
#
# Tony Smaldone
# May 18, 2015
#

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
   # get the value of the matrix inverse as computed in makeCacheMatrix
   # note: The scope of mInverse is within this routine and is NOT the
   # same as that in makeCacheMatrix
   mInverse <- x$getSolve()
   # 
   # if the matrix inverse is not NULL, that means it was already
   # computed so there is no need to recompute it
   if (!is.null(mInverse)) {
      message("getting cached matrix inverse")
      # return to the user the cached matrix inverse and exit the routine
      return(mInverse)
   }
   #
   # the matrix inverse was NULL, so it must be computed
   message("will compute the matrix inverse")
   #
   # get the matrix for which the inverse must be computed
   A <- x$get()
   # 
   # actually compute the matrix inverse using the R solve() routine
   mInverse <- solve(A)
   #
   # pass the newly computed matrix inverse so that it can be cached for
   # future use
   x$setSolve(mInverse)
   #
   # return to the user the newly computed matrix inverse
   mInverse

}
