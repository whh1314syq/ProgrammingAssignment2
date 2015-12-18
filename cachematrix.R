## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
# 函数内部定义的变量
  m <- NULL
  
  # 用来赋值的函数。"<<-" 是全局赋值符号。
  # 一般的赋值（"＝" 或者 "<-"）只会在当前函数内部（局部）对符号赋值；
  # 如果符号不存在，则创建新的符号。
  # 而 "<<-" 会搜索全局的符号进行赋值，
  # 所以此函数会把 y 的值赋给 makeVector 的输入变量 x，
  # 而不是在 set 函数内部创建一个新的符号 x，将 m 设为 NULL。
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  # 常值函数，返回 x，即需要处理的数据。
  get <- function() x
  
  # 将 mean 的值赋给 m，即缓存处理之后的结果。
  setmean <- function(mean) m <<- mean
  
  # 常值函数，返回 m，即处理结果。
  getmean <- function() m
  
  # 函数 makeVector 返回的是一个 list，其中共有四个元素，
  # 每个元素都是之前定义过的一个函数。
  list(set = set,
       get = get,
       setmean = setmean,
       getmean = getmean) 

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
         # x 返回一个 makeVector，
  # x$getmean 就是调用 makeVector 中的 getmean 函数，
  # 也就是先查询一下之前有没有计算过 mean 值。
  m <- x$getmean()
  if(!is.null(m)) {
    # m 不是 NULL，说明之前已经计算过 mean 值了，
    # 直接返回结果。
    message("getting cached data")
    return(m)
  }
  
  # 函数如果执行到这一步，说明之前没有计算过 mean。
  # 调用 x 内部的 get 函数，从 x 中调出需要处理的数据。
  data <- x$get()
  # 计算 mean 值。
  m <- mean(data, ...)
  # 调用 x 内部的 setmean 函数，将得到的结果还存到 x 里面去；
  # 下次调用 cachemean 的时候，x 已经缓存了结果，
  # cachemean 函数会在之前的 if 查询中直接返回，不需要再计算了。
  x$setmean(m)
  # 返回结果。
  m
}
