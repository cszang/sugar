\name{\%$\%}
\alias{\%$\%}
\title{Create list element(s) from supplied object(s) and give them the
names of the objects as element names}
\usage{
structure \%$\% x
}
\arguments{
  \item{structure}{a list object to append to}

  \item{x}{an object, or a list holding the objects to be
  added to the list}
}
\value{
nothing, invoked for side effects: the original list is
modified in place
}
\description{
Instead of mylist$mysomething <- mysomething, we can now
write mylist %$% mysomething. A little easier. And instead
of mylist$mysomething <- mysomething; mylist$somethingelse
<- somethingelse, we can now write mylist %$%
c(mysomething, somethingelse). Even a little more easier.
}

