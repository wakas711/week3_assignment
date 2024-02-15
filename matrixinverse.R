makeCacheMatrix <- function( m = matrix() ) 
{
  init <- NULL
  set_matrix <- function( matrix ) 
  {
    m <<- matrix
    init <<- NULL
  }
  get_matrix <- function() 
  {
    m
  }
  set_Inverse_matrix <- function(inverse) 
  {
    init <<- inverse
  }
  get_Inverse_matrix <- function() 
  {
    init
  }
  list(set = set_matrix, get = get_matrix , setInverse = set_Inverse_matrix,getInverse = get_Inverse_matrix )
}


cacheSolve <- function(x, ...) 
{
  m <- x$getInverse()
  
  if( !is.null(m) ) 
  {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data) %*% data
  x$setInverse(m)
  m
}