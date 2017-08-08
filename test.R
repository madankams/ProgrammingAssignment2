makeVector <- function(x = numeric()) {
  #xx <<- NULL
  #m <- NULL
  
  if (!exists("xx")) {
    xx <<- NULL
    m <<- NULL
  }
  
  set <- function(y) {
    xx <<- y
    m <<- NULL
  }
  get <- function() xx
  setmean <- function(mean) m <<- mean
  getmean <- function() m
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}

cachemean <- function(x, ...) {
  tmp<- makeVector()
  m <- tmp$getmean()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- tmp$get()
  if (length(data) > 0){
    m <- mean(data, ...)  
  }
  else {
    m <- mean(x)
  }
  
  tmp$setmean(m)
  m
}