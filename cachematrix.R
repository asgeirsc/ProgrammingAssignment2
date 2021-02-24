##
## This R script is a submission for the Week 3 assignment in the Coursera course
## R Programming. It contains two functions:
## 
## makeCacheMatrix: Creates a list of functions to handling caching of a matrix and its inverse
## cacheSolve: Solves, i.e. creates an inverse, of a matrix - first checking if the inverse is available in cache
##
## The purpose of these two functions is to create a function that benefits from the lexical scope property
## of the R language, assigning cached values in the global environment.
##
## Author: Asgeir Brenne, asgeir.brenne@gmail.com
## 
## Thanks to mentor Alan E. Berger for his tips in the discussion forum.
##


## The function makeCacheMatrix makes a special matrix object (list of four functions) that can cache
## both the matrix and its inverse
## 
## 'x' is a matrix argument
##
## NOTE. The function does not perform any checking of the matrix to confirm it is invertible,
## so 'x' argument is assumed to be an invertible matrix, with number of rows equal to number of columns,
## and determinant different from zero.
makeCacheMatrix <- function(x = matrix()) {

        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolved <- function(solve) m <<- solve(x)
        getsolved <- function() m
        list(set = set, get = get,
             setsolved = setsolved,
             getsolved = getsolved)
}


## The function cacheSolve solves the matrix contained in an object created by use of the makeCacheMatrix
## function. It first checks whether the solved matrix is available in cache, and if so returns it without
## using computing power to solve it. If not, it solves the matrix and caches it by means of the setsolved()
## function within 'x'.
## 
## 'x' is a list created by the makeCacheMatrix function
##
## NOTE. The function does not perform any checking of the matrix to confirm it is invertible,
## so 'x' argument is assumed to contain an invertible matrix, with number of rows equal to number of columns,
## and determinant different from zero.
cacheSolve <- function(x, ...) {
        m.local <- x$getsolved()
        if(!is.null(m.local)) {
                message("getting cached data")
                return(m.local)
        }
        data <- x$get()
        m.local.calculated <- solve(data, ...)
        x$setsolved(m.local.calculated)
        m.local.calculated # return the mean value

}
