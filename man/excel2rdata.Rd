\name{excel2rdata}
\alias{excel2rdata}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Converting Excel Files Into Required R Format 
}
\description{
This function converts the excel files into required RData format.
}
\usage{
excel2rdata(file.align = 'alignment.xlsx', null.tokens = TRUE, len = len)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file.align}{
the excel file name which we want to convert it into required RData format. 
}
  \item{null.tokens}{
logical. If \samp{TRUE}, 'null' is added at the first of each source sentence of the test set.
}
 \item{len}{
the  number of sheets of the excel file to be converted into RData format. It must be assigned by the user.
}
}

\value{
an RData object as 'file.align.RData'.
}

\author{
Neda Daneshgar and Majid Sarmad.
}
\note{
Note that in order to use the created excel file for the function \code{\link{evaluation}}, don't forget to use \code{\link{excel2rdata}} function to convert the excel file into required \code{RData} format.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{cross.table}}, \code{\link{evaluation}}
}
