\name{neighbor}
\alias{neighbor}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Finding Neighborhood Locations
}
\description{
Starting with the intersection of ef and fe alignment one by one and finding the square neighbors including the union and intersection, recursively.
}
\usage{
neighbor(fe, ef, n.row)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{fe}{
an integer vector.
}
  \item{ef}{
an integer vector.
}
  \item{n.row}{
an integer. Number of rows of an initial matrix. 
}
}
\value{
An integer vector.
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
Koehn P. (2010), "Statistical Machine Translation.",
Cambridge University, New York.
}
\author{
Neda Daneshgar and Majid Sarmad.
}
\examples{
fe = c(1,4,2,4,2)
ef = c(3,2,1,5)
n.row = 4
neighbor (fe, ef, n.row)
}
