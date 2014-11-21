## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
## assign NULL to inver variable
	inver <<- NULL	
	## n is the matrix dimension	
	## n is a random that gos from 2 to 1000			
	n <- runif(1,2,1000)
	## n could be an non-integer number, because this was used the trunc funtion
	n <- trunc(n)
	## a is the number of elements of the matrix (n x n)
	a <- n*n
	## all elements of the matrix is generated randomly
	w <- runif(a,0,1000)
	## x is a matrix (n x n) that use the elements of the vector w
	x <<- matrix(w,n)
	## y is the matrix determinant
	y <- det (x)
	## this loop verify if the matrix is inversible or not and generates another matrix while the matris determinate is zero
	## the generated matrix is x
	if (y == 0){
		while (y == 0){
			w <- runif(a,0,1000)
			x <<- matrix(w,n)
			y <- det (x)
		}
	}
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
## it is verified if the matrix was reversed
	## if yes the program just retorn the inverse matrix
	## else the program calcultes the inverse matrix
	if (!is.null(inver)){
		message("getting cached data")
		return(inver)
	}
	else {
		inver <<- solve(x)
		inver
	}
}
