##' Create list element(s) from supplied object(s) and give them the
##' names of the objects as element names
##' 
##' Instead of mylist$mysomething <- mysomething, we can now write
##' mylist %$% mysomething. A little easier. And instead of
##' mylist$mysomething <- mysomething; mylist$somethingelse <-
##' somethingelse, we can now write mylist %$% c(mysomething,
##' somethingelse). Even a little more easier.
##' @param structure a list object to append to
##' @param x an object, or a list holding the objects to be added to
##' the list
##' @return nothing, invoked for side effects: the original list is
##' modified in place
##' @export 
`%$%` <- function(structure, x) {

  do_assign <- function(str_name, x_quoted, x_name) {
    eval(
      substitute(
        STR[[QUOTED]] <<- NAME,
        list(STR = str_name, QUOTED = x_quoted, NAME = x_name)))
  }
  
  call <- match.call()
  str_name <- as.name(deparse(substitute(structure)))
  
  if (is.call(call[[3]])) {
    members_list <- call[[3]]
    n_members <- length(members_list)
    for (i in 2:n_members) {
      x_name <- members_list[[i]]
      x_quoted <- as.character(x_name)
      do_assign(str_name, x_quoted, x_name)
    }
  } else {
    x_quoted <- deparse(substitute(x))
    x_name <- as.name(x_quoted)
    do_assign(str_name, x_quoted, x_name)
  }
}
