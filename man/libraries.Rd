\name{libraries}
\alias{libraries}
\title{Load multiple packages with a single call}
\usage{
libraries(...)
}
\arguments{
  \item{...}{packages to load}
}
\value{
nothing, invoked for side effects: loading packages
}
\description{
Instead of library(foo); library(bar) we can now write
libraries(foo, bar). A little easier.
}

