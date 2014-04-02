##' Load multiple packages with a single call
##' 
##' Instead of library(foo); library(bar) we can now write
##' libraries(foo, bar). A little easier.
##' @param ... packages to load
##' @return nothing, invoked for side effects: loading packages
##' @export 
libraries <- function(...) {
  call <- match.call()
  n <- length(call)
  for (i in 2:n) {
    eval(
      substitute(
        library(pkg),
        list(pkg = call[[i]])))
  }
}
