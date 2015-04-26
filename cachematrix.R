#Following functions compute matrix inverse, store result in cache
#and obtain result of the inverse operation from cache
#if the inverse has already been calculated and matrix has not changed

#makeCacheMatrix: 
#This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x_mt = matrix()){
    m_mt <- NULL    #sets matrix evaluation result to NULL
    set <- function(y_mt){
        x_mt <<- y_mt
        m_mt <<- NULL
    }
    get <- function(){ x_mt } #gets input data
    setinv <- function(solve){ m_mt <<- solve }
    getinv <- function(){ m_mt }
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}

#cacheSolve: 
#This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#If the inverse has already been calculated (and the matrix has not changed), 
#then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x_mt, ...){
    m_mt <- x_mt$getinv()
    if(!is.null(m_mt)) {
        message("getting cached data")
        return(m_mt) #returns data from cache if it contains inversion result
    }
    data <- x_mt$get()
    m_mt <- solve(data, ...)
    x_mt$setinv(m_mt)
    m_mt        #returns result           
}